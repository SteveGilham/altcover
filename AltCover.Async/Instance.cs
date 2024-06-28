using System.Collections.Generic;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System;
using System.Runtime.InteropServices;
using System.Runtime.Versioning;
using System.Threading;

[assembly: CLSCompliant(true)]
[assembly: ComVisible(false)]
[assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUncalledPrivateCodeRule",
                            Scope = "member", // MethodDefinition
                            Target = "AltCover.Recorder.Instance/I/CallTrack::get_attr()",
                            Justification = "stub assembly")]
[assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUncalledPrivateCodeRule",
                            Scope = "member", // MethodDefinition
                            Target = "AltCover.Recorder.Instance/I/CallTrack::instance()",
                            Justification = "stub assembly")]
[assembly: SuppressMessage("Gendarme.Rules.Naming",
                            "UseCorrectCasingRule",
                            Scope = "member", // MethodDefinition
                            Target = "AltCover.Recorder.Instance/I/CallTrack::get_attr()",
                            Justification = "stub assembly")]
[assembly: SuppressMessage("Gendarme.Rules.Naming",
                            "UseCorrectCasingRule",
                            Scope = "member", // MethodDefinition
                            Target = "AltCover.Recorder.Instance/I/CallTrack::get_value()",
                            Justification = "stub assembly")]
[assembly: SuppressMessage("Gendarme.Rules.Naming",
                            "UseCorrectCasingRule",
                            Scope = "member", // MethodDefinition
                            Target = "AltCover.Recorder.Instance/I/CallTrack::instance()",
                            Justification = "stub assembly")]
[assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidRepetitiveCallsToPropertiesRule",
                            Scope = "member", // MethodDefinition
                            Target = "AltCover.Recorder.Instance/I/CallTrack::instance()",
                            Justification = "stub assembly")]
[assembly: SuppressMessage("Microsoft.Performance",
  "CA1811:AvoidUncalledPrivateCode",
  Scope = "member",
  Target = "AltCover.Recorder.Instance+I+CallTrack.#instance()",
  Justification = "stub assembly")]
[assembly: SuppressMessage("Microsoft.Performance",
  "CA1810:InitializeReferenceTypeStaticFieldsInline",
  Scope = "member",
  Target = "AltCover.Recorder.LibraryHelper.#.cctor()",
  Justification = "stub assembly")]

namespace AltCover.Recorder;

internal static class LibraryHelper
{
  [DebuggerBrowsable(DebuggerBrowsableState.Never)]
  internal static readonly TargetFrameworkAttribute attr_004011;

  [DebuggerBrowsable(DebuggerBrowsableState.Never)]
  internal static readonly AsyncLocal<Stack<int>> value_004014;

  static LibraryHelper()
  {
    TargetFrameworkAttribute attr = (attr_004011 = new TargetFrameworkAttribute(".NETFramework,Version=v4.6"));
    AsyncLocal<Stack<int>> asyncLocal = (value_004014 = new AsyncLocal<Stack<int>>());
  }
}

public static class Instance
{
  public static class I
  {
    internal static class CallTrack
    {
      [SuppressMessage("Microsoft.Performance", "CA1811:AvoidUncalledPrivateCode", Justification = "stub assembly")]
      internal static TargetFrameworkAttribute attr => LibraryHelper.attr_004011;

      [SuppressMessage("Microsoft.Performance", "CA1811:AvoidUncalledPrivateCode", Justification = "stub assembly")]
      internal static AsyncLocal<Stack<int>> value => LibraryHelper.value_004014;

      internal static Stack<int> instance()
      {
        if (value.Value == null)
        {
          value.Value = new Stack<int>();
        }
        return value.Value;
      }
    }
  }
}