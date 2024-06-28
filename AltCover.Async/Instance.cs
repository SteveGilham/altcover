using System.Collections.Generic;
using System.Diagnostics;
using System.Runtime.CompilerServices;
using System.Runtime.Versioning;
using System.Threading;

namespace AltCover.Recorder;

internal static class _0024Library
{
  [DebuggerBrowsable(DebuggerBrowsableState.Never)]
  internal static readonly TargetFrameworkAttribute attr_004011;

  [DebuggerBrowsable(DebuggerBrowsableState.Never)]
  internal static readonly AsyncLocal<Stack<int>> value_004014;

  static _0024Library()
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
      internal static TargetFrameworkAttribute attr => _0024Library.attr_004011;

      internal static AsyncLocal<Stack<int>> value => _0024Library.value_004014;

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