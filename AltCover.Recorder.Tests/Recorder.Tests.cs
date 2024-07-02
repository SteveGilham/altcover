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
  using System.Runtime.CompilerServices;
  using System.Threading;
  using System.Xml;

  using AltCover.Recorder;
  using NUnit.Framework;

  public static class AltCoverTests
  {
    [Test]
    public static void ShouldCoverTrivalClass()
    {
      var mark = new InstrumentationAttribute(); // Constructor has all the instrumented code
      Assert.That(mark.Assembly, Is.EqualTo("AltCover.Recorder.g!"));
      Assert.That(mark.Configuration, Is.EqualTo("Uninstrumented!!"));
      mark.Assembly = String.Empty;
      mark.Configuration = String.Empty;
      Assert.True(String.IsNullOrEmpty(mark.Assembly));
      Assert.True(String.IsNullOrEmpty(mark.Configuration));
    }
  }
}