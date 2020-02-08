#if NETCOREAPP2_0
namespace Tests.Recorder.Core
#else
#if NET4
namespace Tests.Recorder.Clr4
#else
#if NET2
namespace Tests.Recorder.Clr2
#else
namespace Tests.Recorder.Unknown
#endif
#endif
#endif

module Program =
  [<EntryPoint>]
  let main _ = 0