namespace AltCover.Recorder
{
  using System;
  using System.Collections.Generic;
  using System.Diagnostics;
  using System.Diagnostics.CodeAnalysis;
  using System.IO;
  using System.Reflection;

  using System.Resources;
  using System.Runtime.CompilerServices;

#if !NET20
  using System.Threading;
#endif

  //using AltCover.Shared;

  public static class Instance
  {
    // Public "fields"

    /// <summary>
    /// Gets the location of coverage xml file
    /// This property's IL code is modified to store the actual file location
    /// </summary>
    public static string ReportFile
    {
      [MethodImpl(MethodImplOptions.NoInlining)]
      get { return "Coverage.Default.xml"; }
    }

#if DEBUG

    internal static class I
#else
     private static class I
#endif
    {
    }
  }
}