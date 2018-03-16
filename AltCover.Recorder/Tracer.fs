namespace AltCover.Recorder

open System.Collections.Generic
open System.IO
open System.IO.Compression

type Tracer = {
                Tracer : string
                Stream : System.IO.Stream
                Formatter : System.Runtime.Serialization.Formatters.Binary.BinaryFormatter
              }
  with
#if NETSTANDARD2_0
    static member Core () =
             typeof<Microsoft.FSharp.Core.CompilationMappingAttribute>.Assembly.Location
#endif

    static member Create (name:string) =
      {
       Tracer = name
       Stream = null
       Formatter = System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()
      }

    member this.IsConnected () =
      match this.Stream with
      | null -> false
      | _ -> File.Exists this.Tracer

    member this.Connect () =
      if File.Exists this.Tracer then
        Seq.initInfinite (fun i -> Path.ChangeExtension(this.Tracer,
                                                        sprintf ".%d.acv" i))
        |> Seq.filter (File.Exists >> not)
        |> Seq.map (fun f -> let fs = File.OpenWrite f
                             { this with Stream = new DeflateStream(fs, CompressionMode.Compress) })
        |> Seq.head
      else
        this

    member this.Close() =
      this.Stream.Dispose()

    member this.Push (moduleId:string) hitPointId context =
      let stream = this.Stream
      this.Formatter.Serialize(stream, (moduleId, hitPointId, context))

    member this.CatchUp (visits:Dictionary<string, Dictionary<int, int * (int64 option * int option) list>>) =
      visits.Keys
      |> Seq.iter (fun moduleId ->
        visits.[moduleId].Keys
        |> Seq.iter (fun hitPointId -> for i = 1 to fst visits.[moduleId].[hitPointId] do
                                         this.Push moduleId hitPointId (None, None)))
      visits.Clear()

    member this.OnStart () =
      if this.Tracer <> "Coverage.Default.xml.acv" then
        this.Connect ()
      else this

    member this.OnConnected f g =
      if this.IsConnected() then f()
      else g ()

    member this.OnFinish visits =
      this.CatchUp visits
      this.Push null -1 (None, None)
      this.Stream.Flush()
      this.Close()

    member this.OnVisit visits moduleId hitPointId context =
      this.CatchUp visits
      this.Push moduleId hitPointId context