#if RUNNER

using AltCover;
using System.Drawing;
using System;

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

    public override string ToString()
    {
      return "AltCover.Pair(Time=" + Time.ToString(CultureInfo.InvariantCulture)
        + ", Call=" + Call.ToString(CultureInfo.InvariantCulture) + ")";
    }

    public override bool Equals(object obj)
    {
      if (obj is Pair b)
      {
        return Call == b.Call
          && Time == b.Time;
      }

      return false;
    }

    public override int GetHashCode()
    {
      return Call.GetHashCode() ^ Time.GetHashCode();
    }
  }

  internal abstract class Track
  {
    internal static readonly string Entry = "\u2611"; // BALLOT BOX WITH CHECK
    internal static readonly string Exit = "\u2612"; // BALLOT BOX WITH X

    public override bool Equals(object obj)
    {
      return (obj is Track);
    }

    public override int GetHashCode()
    {
      return 0;
    }
  }

  internal class Null : Track
  {
    public override string ToString()
    {
      return "AltCover.Null";
    }
  }

  internal class Time : Track
  {
    public readonly long Value;

    public Time(long time)
    {
      Value = time;
    }

    public override bool Equals(object obj)
    {
      if (obj is Time)
      {
        return Value == ((Time)obj).Value;
      }

      return false;
    }

    public override int GetHashCode()
    {
      return Value.GetHashCode();
    }

    public override string ToString()
    {
      return "AltCover.Time : " + Value.ToString(CultureInfo.InvariantCulture);
    }
  }

  internal class Call : Track
  {
    public readonly int Value;

    public Call(int call)
    {
      Value = call;
    }

    public override bool Equals(object obj)
    {
      if (obj is Call)
      {
        return Value == ((Call)obj).Value;
      }

      return false;
    }

    public override int GetHashCode()
    {
      return Value.GetHashCode();
    }

    public override string ToString()
    {
      return "AltCover.Call : " + Value.ToString(CultureInfo.InvariantCulture);
    }
  }

  internal class Both : Track
  {
    public readonly Pair Value;

    public Both(Pair both)
    {
      Value = both;
    }

    public override bool Equals(object obj)
    {
      if (obj is Both b)
      {
        var ok = Value.Equals(b.Value);
        return ok;
      }

      return false;
    }

    public override int GetHashCode()
    {
      return Value.GetHashCode();
    }

    public override string ToString()
    {
      return "AltCover.Both : " + Value.ToString();
    }
  }

  internal class Table : Track
  {
    public readonly Dictionary<string, Dictionary<int, PointVisit>> Value;

    public Table(Dictionary<string, Dictionary<int, PointVisit>> table)
    {
      Value = table;
    }
  }

  internal class PointVisit
  {
    public long Count;
    public readonly List<Track> Tracks;

    public override bool Equals(object obj)
    {
      if (obj is PointVisit b)
      {
        var ok = Count == b.Count && Tracks.Count == b.Tracks.Count;
        for (var i = 0; ok && i < Tracks.Count; i++)
        {
          ok = ok && Tracks[i].Equals(b.Tracks[i]);
        }

        return ok;
      }

      return false;
    }

    public override int GetHashCode()
    {
      return Count.GetHashCode();
    }

    public override string ToString()
    {
      var tracks = new string[Tracks.Count];
      for (int i = 0; i < Tracks.Count; ++i)
        tracks[i] = Tracks[i].ToString();

      return "AltCover.PointVisit : Count = " + Count.ToString(CultureInfo.InvariantCulture) +
        "Tracks = " + String.Join("; ", tracks);
    }

    private PointVisit(long count)
    {
      Count = count;
      Tracks = new List<Track>();
    }

    internal static PointVisit Create()
    {
      return new PointVisit(0);
    }

    internal void Step()
    {
      System.Threading.Interlocked.Increment(ref this.Count);
    }

    internal void Track(Track something)
    {
      lock (this.Tracks)
      {
        this.Tracks.Add(something);
      }
    }

    internal long Total
    {
      get { return this.Count + this.Tracks.Count; }
    }
  }

  internal static class Counter
  {
    // // <summary>
    // // The time at which coverage run began
    // // </summary>
    internal static DateTime startTime = DateTime.UtcNow;

    // // <summary>
    // // The finishing time taken of the coverage run
    // // </summary>
    internal static DateTime measureTime = DateTime.UtcNow;

    // // <summary>
    // // The offset flag for branch counts
    // // </summary>
    internal const int branchFlag = unchecked((int)0x80000000);

    internal const int branchMask = unchecked((int)0x7FFFFFFF);

    internal static long totalVisits = 0;

    internal static long branchVisits = 0;

#if DEBUG

    internal static class I
#else
    static private class I
#endif
    {
    }
  }
}