namespace AltCover.Recorder

open System
open System.Collections.Generic
open System.IO

type Tracer = {
                Tracer : string
                Stream : System.IO.FileStream
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
        { this with Stream = File.OpenWrite(this.Tracer) }
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

    member this.OnConnected f l g =
      if this.IsConnected() then f()
      else  l g

    member this.OnFinish finish =
      if finish then
        this.Push null -1
        this.Close()

    member this.OnVisit visits moduleId hitPointId =
      this.CatchUp visits
      this.Push moduleId hitPointId