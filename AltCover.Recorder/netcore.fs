namespace AltCover.Recorder

open System
open System.IO

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
              }
  with
    static member Core () =
             typeof<Microsoft.FSharp.Core.CompilationMappingAttribute>.Assembly.Location

    static member CreatePipe (name:string) =
      printfn "**Creating NamedPipeClientStream %s" name
      {Tracer = name;
       Pipe = new System.IO.Pipes.NamedPipeClientStream(name);
       Activated = new System.Threading.ManualResetEvent false }

    member this.IsConnected ()=
      this.Pipe.IsConnected &&
        this.Pipe.CanWrite

    member this.IsActivated ()=
      this.IsConnected() &&
        this.Activated.WaitOne(0)

    member this.Connect ms =
      try
        this.Pipe.Connect(ms)
        async {
          Communications.ResilientAgainstDisposedObject(fun () ->
            Communications.SignalOnReceive this.Pipe this.Activated) ignore
        } |> Async.Start
      with
      | :? TimeoutException ->
        reraise ()

    member this.Close() =
      this.Pipe.Dispose()
      this.Activated.Dispose()
