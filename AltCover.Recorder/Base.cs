#if RUNNER

namespace AltCover
#else
namespace AltCover.Recorder
#endif
{
  using System;
  using System.Collections.Generic;
  using System.Diagnostics.CodeAnalysis;
  using System.Globalization;
  using System.IO;
  using System.Xml;

#if !RUNNER
  using ICSharpCode.SharpZipLib.Zip;

  [AttributeUsage(AttributeTargets.All, Inherited = false, AllowMultiple = false)]
  [SuppressMessage("Gendarme.Rules.Performance",
                    "AvoidUninstantiatedInternalClassesRule",
                    Justification = "Looks like a bug, not detecting its use")]
  internal sealed class ExcludeFromCodeCoverageAttribute : Attribute
  {}
#endif

  [Flags]
  [SuppressMessage("Gendarme.Rules.Design",
                    "FlagsShouldNotDefineAZeroValueRule",
                    Justification = "Zero is meaningful")]
  [SuppressMessage("Gendarme.Rules.Naming",
                    "UsePluralNameInEnumFlagsRule",
                    Justification = "Not meaningful to do so")]
  internal enum ReportFormat
  {
    NCover = 0,
    OpenCover = 1,
    NativeJson = 2,
    TrackMask = 63,
    WithTracking = 64,
    ZipMask = 127,
    Zipped = 128,
  };

  internal enum Sampling
  {
    All = 0,
    Single = 1,
  };

  // TODO isolate where
  [SuppressMessage("Gendarme.Rules.Performance",
                    "AvoidUninstantiatedInternalClassesRule",
                    Justification = "Used as pattern match and compiled away")]
  internal enum Tag
  {
    Null = 0,
    Time = 1,
    Call = 2,
    Both = 3,
    Table = 4,
  }

  [ExcludeFromCodeCoverage]
  internal struct Pair
  {
    public long Time;
    public int Call;

    public static Pair Create(long time, int call)
    {
      return new Pair { Time = time, Call = call };
    }
  }
}