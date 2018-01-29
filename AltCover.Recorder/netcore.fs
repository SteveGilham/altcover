namespace AltCover.Recorder

open System
open System.Collections.Generic
open System.IO
open System.Threading.Tasks

module Communications =
  let internal ResilientAgainstDisposedObject (f: unit -> unit) (tidy : unit -> unit)=
    try
      f()
    with
    | :? ObjectDisposedException -> tidy()

  let SignalOnReceive (s:Stream) (h:System.Threading.EventWaitHandle) =
    let b = s.ReadByte()
    if (b >= 0) then
        h.Set() |> ignore

type Tracer = {
                Tracer : string
                Pipe : System.IO.Pipes.NamedPipeClientStream
                Activated : System.Threading.ManualResetEvent
                Formatter : System.Runtime.Serialization.Formatters.Binary.BinaryFormatter
              }
  with
    static member Core () =
             typeof<Microsoft.FSharp.Core.CompilationMappingAttribute>.Assembly.Location

    static member Create (name:string) =
      {
       Tracer = name;
       Pipe = new System.IO.Pipes.NamedPipeClientStream(name);
       Activated = new System.Threading.ManualResetEvent false
       Formatter = System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()
      }

    member this.IsConnected ()=
      this.Pipe.IsConnected &&
        this.Pipe.CanWrite

    member this.IsActivated ()=
      this.IsConnected() &&
        this.Activated.WaitOne(0)

    member this.Connect () =
      this.Pipe.Connect()  // this could and should be able to throw ObjectDisposed = Pipe broken
      Communications.ResilientAgainstDisposedObject(fun () ->
          Communications.SignalOnReceive this.Pipe this.Activated) ignore

    member this.Close() =
      this.Pipe.Dispose()
      this.Activated.Dispose()

    member this.Push (moduleId:string) hitPointId =
      Communications.ResilientAgainstDisposedObject (fun () ->
        this.Formatter.Serialize(this.Pipe, (moduleId, hitPointId))
        this.Pipe.Flush()) ignore

    member this.CatchUp (visits:Dictionary<string, Dictionary<int, int>>) =
      visits.Keys
      |> Seq.iter (fun moduleId ->
        visits.[moduleId].Keys
        |> Seq.iter (fun hitPointId -> for i = 1 to visits.[moduleId].[hitPointId] do
                                         this.Push moduleId hitPointId))
      visits.Clear()

    member this.OnStart () =
      if this.Tracer <> "AltCover" then
        try
          this.Connect ()
        with
        | :? TimeoutException
        | :? ObjectDisposedException
        | :? IOException ->
            ()

    member this.OnConnected f l g =
      if this.IsActivated() then f()
      else  l g

    member this.OnFinish finish =
      if finish then
        try
          while this.Pipe.CanWrite do
            this.Push null -1
        with
        | :? IOException -> ()

    member this.OnVisit visits moduleId hitPointId =
      this.CatchUp visits
      this.Push moduleId hitPointId