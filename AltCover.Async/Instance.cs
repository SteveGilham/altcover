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

      private static readonly AsyncLocal<Stack<int>> __value = new AsyncLocal<Stack<int>>();

      private static AsyncLocal<Stack<int>> Value
      {
        [SuppressMessage("Microsoft.Performance",
          "CA1811:AvoidUncalledPrivateCode",
        Justification = "Template code only")]
        get { return __value; }
      }

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
        if (Value.Value == null)
          Value.Value = new Stack<int>();

        return Value.Value;
      }
    }
  }
}