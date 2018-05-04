namespace AltCover.Recorder

open System.Collections.Generic
open System.IO
open System.IO.Compression

type Tracer = {
                Tracer : string
                Runner : bool
                Definitive : bool
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
       Runner = false
       Definitive = false
       Stream = null
       Formatter = System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()
      }

    member this.IsConnected () =
      match this.Stream with
      | null -> false
      | _ -> this.Runner

    member this.Connect () =
      if File.Exists this.Tracer then
        Seq.initInfinite (fun i -> Path.ChangeExtension(this.Tracer,
                                                        sprintf ".%d.acv" i))
        |> Seq.filter (File.Exists >> not)
        |> Seq.map (fun f -> let fs = File.OpenWrite f
                             { this with Stream = new BufferedStream(new DeflateStream(fs, CompressionMode.Compress))
                                         Runner = true })
        |> Seq.head
      else
        this

    member this.Close() =
      this.Stream.Dispose()

    member internal this.Push (moduleId:string) hitPointId context =
      let stream = this.Stream
      this.Formatter.Serialize(stream, match context with
                                       | Null -> (moduleId, hitPointId) :> obj
                                       | _ -> (moduleId, hitPointId, context) :> obj)

    member internal this.CatchUp (visits:Dictionary<string, Dictionary<int, int * Track list>>) =
      let empty = Null
      visits.Keys
      |> Seq.iter (fun moduleId ->
        visits.[moduleId].Keys
        |> Seq.iter (fun hitPointId -> let n, l = visits.[moduleId].[hitPointId]
                                       let push = this.Push moduleId hitPointId
                                       [seq {1 .. n} |> Seq.map (fun _ -> empty )
                                        l |> List.toSeq]
                                       |> Seq.concat |> Seq.iter push
                                       ))
      visits.Clear()

    member this.OnStart () =
      let running = if this.Tracer <> "Coverage.Default.xml.acv" then
                       this.Connect () else this
      {running with Definitive = true}

    member this.OnConnected f g =
      if this.IsConnected() then f()
      else g ()

    member internal this.OnFinish visits =
      this.CatchUp visits
      this.Push null -1 Null
      this.Stream.Flush()
      this.Close()

    member internal this.OnVisit visits moduleId hitPointId context =
      this.CatchUp visits
      this.Push moduleId hitPointId context