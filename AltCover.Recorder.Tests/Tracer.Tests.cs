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
  using System;
  using System.Collections.Generic;
  using System.IO;
  using System.IO.Compression;
  using System.Reflection;
  using AltCover.Recorder;
  using NUnit.Framework;
  using NUnit.Framework.Constraints;
  using static NUnit.Framework.Constraints.Tolerance;

  //[<TestFixture>]
  public static class AltCoverCoreTests
  {
    private static void maybeIOException(Action f)
    {
      try { f(); }
      catch (IOException)
      { }
    }

    private static void maybeDeleteFile(string f)
    {
      if (File.Exists(f))
      {
        File.Delete(f);
      }
    }

    private static void maybeReraise(Action f, Action g)
    {
      try { f(); }
      catch (Exception)
      {
        g();
        throw;
      }
    }

    private static void ignore()
    { }

    [Test]
    public static void ExcerciseItAll()
    {
      var where =
        Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);

      var unique =
        Path.Combine(Path.Combine(where, Guid.NewGuid().ToString()), "nonesuch.txt");

      maybeDeleteFile(unique);
      maybeIOException(() => { maybeReraise(() => { File.Delete(unique); }, ignore); });
      maybeIOException(() => { maybeReraise(() => { throw new IOException(); }, ignore); });
    }
  }
}