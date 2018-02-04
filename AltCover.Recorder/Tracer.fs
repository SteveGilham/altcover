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
                                                        sprintf ".%d.bin" i))
        |> Seq.filter (File.Exists >> not)
        |> Seq.map (fun f -> let fs = File.OpenWrite f 
                             { this with Stream = new DeflateStream(fs, CompressionMode.Compress) })
        |> Seq.head
      else
        this

    member this.Close() =
      match this.Stream with
      | null -> ()
      | _ -> this.Stream.Dispose()

    member this.Push (moduleId:string) hitPointId =
      let stream = this.Stream
      this.Formatter.Serialize(stream, (moduleId, hitPointId))
      stream.Flush()

    member this.CatchUp (visits:Dictionary<string, Dictionary<int, int>>) =
      visits.Keys
      |> Seq.iter (fun moduleId ->
        visits.[moduleId].Keys
        |> Seq.iter (fun hitPointId -> for i = 1 to visits.[moduleId].[hitPointId] do
                                         this.Push moduleId hitPointId))
      visits.Clear()

    member this.OnStart () =
      if this.Tracer <> "Coverage.Default.xml.bin" then
        this.Connect ()
      else this

    member this.OnConnected f g =
      if this.IsConnected() then f()
      else g ()

    member this.OnFinish visits finish =
      this.CatchUp visits
      if finish then
        this.Push null -1
        this.Close()

    member this.OnVisit visits moduleId hitPointId =
      this.CatchUp visits
      this.Push moduleId hitPointId