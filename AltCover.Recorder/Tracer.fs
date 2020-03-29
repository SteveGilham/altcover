namespace AltCover.Recorder

open System
open System.Collections.Generic
open System.IO
open System.IO.Compression
open System.Threading

#if NET2
[<System.Runtime.InteropServices.ProgIdAttribute("ExcludeFromCodeCoverage hack for OpenCover issue 615")>]
#else
[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
#endif
type internal Close =
  | DomainUnload
  | ProcessExit
  | Pause
  | Resume

[<NoComparison; AutoSerializable(false)>]
type Tracer =
  { Tracer : string
    Runner : bool
    Definitive : bool
    Stream : System.IO.Stream
    Formatter : System.IO.BinaryWriter }

  static member internal Create(name : string) =
    { Tracer = name
      Runner = false
      Definitive = false
      Stream = null
      Formatter = null }

  member internal this.IsConnected
    with get() =
      match this.Stream with
      | null -> false
      | _ -> this.Runner

  [<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Correctness",
         "EnsureLocalDisposalRule",
         Justification="Record return confusing Gendarme -- TODO")>]
  [<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Reliability",
                                                    "CA2000:DisposeObjectsBeforeLosingScope",
                                                    Justification = "'fs' is subsumed")>]
  member private this.MakeConnection f =
    let fs = File.OpenWrite f
    let s = new DeflateStream(fs, CompressionMode.Compress)
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
    with :? ObjectDisposedException -> ()

  member private this.PushContext context =
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
        |> Seq.iter (fun m ->
             this.Formatter.Write m
             this.Formatter.Write t.[m].Keys.Count
             t.[m].Keys
             |> Seq.iter (fun p ->
                  this.Formatter.Write p
                  let v = t.[m].[p]
                  this.Formatter.Write v.Count
                  v.Tracks |> Seq.iter this.PushContext
                  this.PushContext Null))
        this.Formatter.Write String.Empty

  member internal this.Push (moduleId : string) (hitPointId : int) context =
    this.Formatter.Write moduleId
    this.Formatter.Write hitPointId
    this.PushContext context

  member internal this.CatchUp(visits : Dictionary<string, Dictionary<int, PointVisit>>) =
    if visits.Count > 0 then
      visits
      |> Table
      |> this.Push String.Empty 0

  member internal this.OnStart() =
    let running =
      if this.Tracer <> "Coverage.Default.xml.acv"
      then this.Connect()
      else this
    { running with Definitive = true }

  member internal  this.OnConnected f g =
    if this.IsConnected then f() else g()

  member internal this.OnFinish visits =
    this.CatchUp visits
    this.Close()

  member internal this.OnVisit visits moduleId hitPointId context =
    this.CatchUp visits
    this.Push moduleId hitPointId context
    this.Formatter.Flush()
    this.Stream.Flush()