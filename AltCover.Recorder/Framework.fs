namespace AltCover.Recorder

[<System.Runtime.InteropServices.ProgId("ExcludeFromCodeCoverage")>] // HACK HACK HACK
type Tracer = {
                Tracer : string
              }
  with
    static member Create (name:string) =
      {Tracer = name}
    member this.Close () = ()
    member this.OnStart _ = ()
    member this.OnConnected f l g =
#if DEBUG
      f()
#else
      f |> ignore
#endif
      l g

    // possible values of "f" above
    member this.OnFinish (finish:bool) =
      ignore finish
    member this.OnVisit _ _ _ = ()
    member this.CatchUp _ = () 