using System.Collections.Generic;
using System;
using System.Runtime.InteropServices;
using System.Threading;
using System.Runtime.Versioning;
using System.Diagnostics.CodeAnalysis;

[assembly: CLSCompliant(true)]
[assembly: ComVisible(false)]

namespace AltCover.Recorder;

public static class Instance
{
  public static class I
  {
    internal static class CallTrack
    {
      [SuppressMessage("Microsoft.Performance",
                       "CA1823:AvoidUnusedPrivateFields",
                       Justification = "Template code only")]
      private static readonly TargetFrameworkAttribute attr =
        new TargetFrameworkAttribute(".NETFramework,Version=v4.6");

      private static readonly AsyncLocal<Stack<int>> value = new AsyncLocal<Stack<int>>();

      // no race conditions here
      [SuppressMessage("Gendarme.Rules.Performance",
                       "AvoidUncalledPrivateCodeRule",
                       Justification = "Template code only")]
      [SuppressMessage("Microsoft.Performance",
                       "CA1811:AvoidUncalledPrivateCode",
                       Justification = "Template code only")]
      [SuppressMessage("Gendarme.Rules.Performance",
                        "AvoidRepetitiveCallsToPropertiesRule",
                        Justification = "Initialization")]
      private static Stack<int> Instance()
      {
        if (value.Value == null)
          value.Value = new Stack<int>();

        return value.Value;
      }
    }
  }
}