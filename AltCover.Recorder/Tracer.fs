namespace AltCover.Recorder

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.IO
open System.IO.Compression

[<ExcludeFromCodeCoverage>]
type internal Close =
  | DomainUnload
  | ProcessExit
  | Pause
  | Resume

[<NoComparison; AutoSerializable(false)>]
type internal Tracer =
  { Tracer: string
    Runner: bool
    Definitive: bool
    Stream: System.IO.Stream
    Formatter: System.IO.BinaryWriter }

  static member internal Create(name: string) =
    { Tracer = name
      Runner = false
      Definitive = false
      Stream = null
      Formatter = null }

  member this.IsConnected
    with internal get () =
      match this.Stream with
      | null -> false
      | _ -> this.Runner

  [<SuppressMessage("Gendarme.Rules.Correctness",
                    "EnsureLocalDisposalRule",
                    Justification = "Record return confusing Gendarme -- TODO")>]
  [<SuppressMessage("Microsoft.Reliability",
                    "CA2000:DisposeObjectsBeforeLosingScope",
                    Justification = "'fs' is subsumed")>]
  member private this.MakeConnection f =
    let fs = File.OpenWrite f

    let s =
      new DeflateStream(fs, CompressionMode.Compress)

    { this with
        Stream = s
        Formatter = new BinaryWriter(s)
        Runner = true }

  member internal this.Connect() =
    if File.Exists this.Tracer then
      Seq.initInfinite (fun i -> Path.ChangeExtension(this.Tracer, sprintf ".%d.acv" i))
      |> Seq.filter (File.Exists >> not)
      |> Seq.map this.MakeConnection
      |> Seq.head
    else
      this

  member internal this.Close() =
    try
      this.Stream.Flush()
      this.Formatter.Close()
    with
    | :? ObjectDisposedException
    | :? NullReferenceException -> ()

  member private this.PushContext (table: Map<string, int>) context =
    match context with
    | Null -> this.Formatter.Write(Tag.Null |> byte)
    | Time t ->
      this.Formatter.Write(Tag.Time |> byte)
      this.Formatter.Write(t)
    | Call t ->
      this.Formatter.Write(Tag.Call |> byte)
      this.Formatter.Write(t)
    | Both b ->
      this.Formatter.Write(Tag.Both |> byte)
      this.Formatter.Write(b.Time)
      this.Formatter.Write(b.Call)
    | Table t ->
      this.Formatter.Write(Tag.Table |> byte)

      t.Keys
      |> Seq.filter (fun k -> t.[k].Count > 0)
      |> Seq.iter
           (fun m ->
             this.Formatter.Write(Map.find m table)
             this.Formatter.Write t.[m].Count

             t.[m].Keys
             |> Seq.iter
                  (fun p ->
                    this.Formatter.Write p
                    let v = t.[m].[p]
                    this.Formatter.Write v.Count
                    v.Tracks |> Seq.iter (this.PushContext table)
                    this.PushContext table Null))

      this.Formatter.Write(Map.find String.Empty table)

  member internal this.Push
    (table: Map<string, int>)
    (moduleId: string)
    (hitPointId: int)
    context
    =
    this.Formatter.Write(Map.find moduleId table)
    this.Formatter.Write hitPointId
    this.PushContext table context

  member internal this.CatchUp
    (table: Map<string, int>)
    (visits: Dictionary<string, Dictionary<int, PointVisit>>)
    =
    if visits.Values |> Seq.sumBy (fun x -> x.Count) > 0 then
      visits
      |> Table
      |> (this.Push table String.Empty 0)

  member internal this.OnStart() =
    let running =
      if this.Tracer <> "Coverage.Default.xml.acv" then
        this.Connect()
      else
        this

    { running with Definitive = true }

  member internal this.OnConnected f g = if this.IsConnected then f () else g ()

  member internal this.OnFinish table visits =
    this.CatchUp table visits
    this.Close()

  member internal this.OnVisit table visits moduleId hitPointId context =
    this.CatchUp table visits
    this.Push table moduleId hitPointId context
    this.Formatter.Flush()
    this.Stream.Flush()