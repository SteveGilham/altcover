#if !NET472
#if NET20
namespace Tests.Recorder.Clr2
#else

namespace Tests.Recorder.Core
#endif
#else
namespace Tests.Recorder.Clr4
#endif
{
  internal static class UnitTestStub
  {
    private static int Main()
    {
      return 0;
    }
  }
}