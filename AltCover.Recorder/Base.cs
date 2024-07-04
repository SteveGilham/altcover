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
  using System.ComponentModel;
  using System.Diagnostics.CodeAnalysis;
  using System.Globalization;
  using System.IO;
  using System.Text.RegularExpressions;
  using System.Xml;
  using static System.Net.Mime.MediaTypeNames;

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
      //  let internal openCoverXml =
      //  ("//Module",
      //   "hash",
      //   "Classes/Class/Methods/Method",
      //   [ ("SequencePoints/SequencePoint", 0)
      //     ("BranchPoints/BranchPoint", branchFlag) ],
      //   "vc")

      //let internal IEnumerable<string> nCoverXml =
      //  ("//module", "moduleId", "method", [ ("seqpnt", 0)], "visitcount")
      internal static readonly object openCoverXml = "//Module";//TODO

      internal static readonly object nCoverXml = "//module";//TODO

      internal static object xmlByFormat(ReportFormat format)
      {
        switch (format & ReportFormat.TrackMask)
        {
          case ReportFormat.OpenCover: return openCoverXml;
          case ReportFormat.NCover: return nCoverXml;
          default: throw new NotSupportedException(format.ToString());
        }
      }

      internal static DateTime minTime(DateTime t1, DateTime t2)
      {
        return t1 < t2 ? t1 : t2;
      }

      internal static DateTime maxTime(DateTime t1, DateTime t2)
      {
        return t1 > t2 ? t1 : t2;
      }

      internal static int findIndexFromUspid(int flag, string uspid)
      {
        var f = Int32.TryParse(
              uspid,
              NumberStyles.Integer,
              CultureInfo.InvariantCulture,
              out int c);
        return f ? (c | flag) : -1;
      }

      internal static void ensurePoint(Dictionary<int, PointVisit> next, int hitPointId)
      { }

      internal static long addTable(
              Dictionary<string, Dictionary<int, PointVisit>> counts,
              Dictionary<string, Dictionary<int, PointVisit>> t
        )
      {
        long hitcount = 0;
        //let internal addTable
        //  (counts: Dictionary<string, Dictionary<int, PointVisit>>)
        //  (t: Dictionary<string, Dictionary<int, PointVisit>>)
        //  =
        //  let mutable hitcount = 0L

        //  t.Keys
        //  |> Seq.iter (fun m ->
        //    if counts.ContainsKey m |> not then
        //      counts.Add(m, Dictionary<int, PointVisit>())

        //    let next = counts.[m]
        //    let here = t.[m]

        //    here.Keys
        //    |> Seq.iter (fun p ->
        //      ensurePoint next p
        //      let mutable v = next.[p]
        //      let add = here.[p]
        //      hitcount <- hitcount + add.Total

        //      lock v (fun () ->
        //        v.Count <- v.Count + add.Count
        //        v.Tracks.AddRange(add.Tracks))))

        return hitcount;
      }

      public static DateTime updateReport(
      Action<XmlDocument> postProcess,
      /*Action<XmlElement, IEnumerable<Track>>*/ object pointProcess,
      bool own,
      Dictionary<string, Dictionary<int, PointVisit>> counts,
      ReportFormat format,
      Stream coverageFile,
      Stream outputFile
      )
      {
        var flushStart = DateTime.UtcNow;
        //  let flushStart =
        //    updateReport postProcess pointProcess own counts format coverageFile outputFile

        //  TimeSpan(DateTime.UtcNow.Ticks - flushStart.Ticks)
        return flushStart;
      }

      public static TimeSpan doFlush(
      Action<XmlDocument> postProcess,
      /*Action<XmlElement, IEnumerable<Track>>*/ object pointProcess,
      bool own,
      Dictionary<string, Dictionary<int, PointVisit>> counts,
      ReportFormat format,
      Stream coverageFile,
      Stream outputFile
      )
      {
        var flushStart =
          updateReport(postProcess, pointProcess, own, counts, format, coverageFile, outputFile);

        return new TimeSpan(DateTime.UtcNow.Ticks - flushStart.Ticks);
      }
    }

    // "Public" API
    internal static void addSingleVisit(
      Dictionary<string, Dictionary<int, PointVisit>> counts,
      string moduleId,
      int hitPointId,
      Track context
      )
    {
      if (counts.ContainsKey(moduleId))
      {
        var next = counts[moduleId];
        I.ensurePoint(next, hitPointId);
        var v = next[hitPointId];

        if (context is Null n)
        { v.Step(); }
        else { v.Track(context); }
      }
    }

#if RUNNER

    internal static long addVisit(
      Dictionary<string, Dictionary<int, PointVisit>> counts,
      string moduleId,
      int hitPointId,
      Track context
      )
    {
      if (context is Table t)
      {
        return I.addTable(counts, t.Value);
      }

      addSingleVisit(counts, moduleId, hitPointId, context);
      return 1;
    }

    [SuppressMessage("Gendarme.Rules.Smells",
                      "AvoidLongParameterListsRule",
                      Justification = "Most of this gets curried away")]
    public static TimeSpan doFlushStream(
      Action<XmlDocument> postProcess,
      Action<XmlElement, IEnumerable<Track>> pointProcess,
      bool own,
      Dictionary<string, Dictionary<int, PointVisit>> counts,
      ReportFormat format,
      Stream coverageFile,
      Stream outputFile
      )
    {
      return I.doFlush(postProcess, pointProcess, own, counts, format, coverageFile, outputFile);
    }

#else
  //[<SuppressMessage("Gendarme.Rules.Smells",
  //                  "AvoidLongParameterListsRule",
  //                  Justification = "Most of this gets curried away")>]
  //[<SuppressMessage("Microsoft.Reliability",
  //                  "CA2000:DisposeObjectsBeforeLosingScope",
  //                  Justification = "'target' is disposed")>]
  //let internal doFlushStream
  //  postProcess
  //  pointProcess
  //  own
  //  counts
  //  format
  //  coverageFile
  //  output
  //  =
  //  use target =
  //    match output with
  //    | None -> new MemoryStream() :> Stream
  //    | Some f ->
  //      new FileStream(
  //        f,
  //        FileMode.OpenOrCreate,
  //        FileAccess.Write,
  //        FileShare.None,
  //        4096,
  //        FileOptions.SequentialScan
  //      )
  //      :> Stream

  //  let outputFile =
  //    if Option.isSome output then
  //      target
  //    else
  //      coverageFile :> Stream

  //  I.doFlush postProcess pointProcess own counts format coverageFile outputFile

  //[<SuppressMessage("Gendarme.Rules.Smells",
  //                  "AvoidLongParameterListsRule",
  //                  Justification = "Most of this gets curried away")>]
  //[<SuppressMessage("Gendarme.Rules.Correctness",
  //                  "EnsureLocalDisposalRule",
  //                  Justification = "'zip' owns 'container' and is 'Close()'d")>]
  //[<SuppressMessage("Microsoft.Reliability",
  //                  "CA2000:DisposeObjectsBeforeLosingScope",
  //                  Justification = "ald also 'target' is disposed")>]
  //let internal doFlushFile postProcess pointProcess own counts format report output =
  //  let zipped =
  //    int (format &&& ReportFormat.Zipped) <> 0

  //  if not zipped then
  //    use coverageFile =
  //      new FileStream(
  //        report,
  //        FileMode.Open,
  //        FileAccess.ReadWrite,
  //        FileShare.None,
  //        4096,
  //        FileOptions.SequentialScan
  //      )

  //    doFlushStream postProcess pointProcess own counts format coverageFile output
  //  else
  //    let container =
  //      new FileStream(
  //        report + ".zip",
  //        FileMode.Open,
  //        FileAccess.ReadWrite,
  //        FileShare.None,
  //        4096,
  //        FileOptions.SequentialScan
  //      )

  //    use target =
  //      match output with
  //      | None -> new MemoryStream() :> Stream
  //      | Some f ->
  //        new FileStream(
  //          f,
  //          FileMode.OpenOrCreate,
  //          FileAccess.Write,
  //          FileShare.None,
  //          4096,
  //          FileOptions.SequentialScan
  //        )
  //        :> Stream

  //    try
  //      ZipConstants.DefaultCodePage <- 65001 //UTF-8 as System.IO.Compression.ZipFile uses internally
  //      let zip = new ZipFile(container)

  //      try
  //        let entryName = report |> Path.GetFileName
  //        let entry = zip.GetEntry(entryName)

  //        let result =
  //          use reader = zip.GetInputStream(entry)
  //          I.doFlush postProcess pointProcess own counts format reader target

  //        if output.IsNone then
  //          zip.BeginUpdate()
  //          zip.Delete entry
  //          target.Seek(0L, SeekOrigin.Begin) |> ignore

  //          let source =
  //            { new IStaticDataSource with
  //                member self.GetSource() = target }

  //          zip.Add(source, entryName)
  //          zip.CommitUpdate()

  //        result
  //      finally
  //        zip.Close()
  //    with :? ZipException ->
  //      use reader = new MemoryStream()
  //      I.doFlush postProcess pointProcess own counts format reader target

#endif // !RUNNER
  }
}