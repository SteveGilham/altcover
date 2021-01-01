#if !NET472
#if NET20
namespace Tests.Recorder.Clr2
#else
namespace Tests.Recorder.Core
#endif
#else
namespace Tests.Recorder.Clr4
#endif

module UnitTestStub =
  [<EntryPoint>]
  let main _ = 0