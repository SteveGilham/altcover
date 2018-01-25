namespace AltCover.Recorder

[<System.Runtime.InteropServices.ProgId("ExcludeFromCodeCoverage")>] // HACK HACK HACK
type Tracer = {
                Tracer : string
              }
  with
    static member Create (name:string) =
      {Tracer = name}
    member this.Close () = ()
    member this.OnStart () = ()
    member this.OnConnected f l g =
      f |> ignore
      l g
    member this.OnFinish (finish:bool) =
      ignore finish
    member this.OnVisit _ _ _ = ()