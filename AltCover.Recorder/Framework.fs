#if RUNNER
namespace AltCover.Base
#else
namespace AltCover.Recorder
#endif

[<System.Runtime.InteropServices.ProgId("ExcludeFromCodeCoverage")>] // HACK HACK HACK
type Tracer = {
                Tracer : string
              }
  with
    static member Create (name:string) =
      {Tracer = name}
    member this.Close() = ()
