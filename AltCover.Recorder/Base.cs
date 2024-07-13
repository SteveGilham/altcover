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
  { }

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

    public override bool Equals(object obj)
    {
      return obj is Null n;
    }

    public override int GetHashCode()
    {
      return string.Empty.GetHashCode();
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
      internal struct coverXml
      {
        internal string m;
        internal string i;
        internal string m2;
        internal Dictionary<string, int> s;
        internal string v;
      }

      internal static readonly coverXml openCoverXml =
        new coverXml()
        {
          m = "//Module",
          i = "hash",
          m2 = "Classes/Class/Methods/Method",
          s = new Dictionary<string, int> { { "SequencePoints/SequencePoint", 0 },
            { "BranchPoints/BranchPoint", branchFlag }
          },
          v = "vc"
        };

      internal static readonly coverXml nCoverXml =
        new coverXml()
        {
          m = "//module",
          i = "moduleId",
          m2 = "method",
          s = new Dictionary<string, int> { { "seqpnt", 0 } },
          v = "visitcount"
        };

      internal static coverXml xmlByFormat(ReportFormat format)
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

      internal static void ensurePoint(Dictionary<int, PointVisit> counts, int hitPointId)
      {
        if (!counts.ContainsKey(hitPointId))
        {
          lock (counts)
          {
            if (!counts.ContainsKey(hitPointId))
            {
              System.Threading.Interlocked.Increment(ref Counter.totalVisits);

              if (hitPointId < 0)
              { System.Threading.Interlocked.Increment(ref Counter.branchVisits); }

              counts.Add(hitPointId, PointVisit.Create());
            }
          }
        }
      }

      internal static long addTable(
              Dictionary<string, Dictionary<int, PointVisit>> counts,
              Dictionary<string, Dictionary<int, PointVisit>> t
        )
      {
        long hitcount = 0;

        foreach (var key in t.Keys)
        {
          if (!counts.ContainsKey(key))
          {
            counts.Add(key, new Dictionary<int, PointVisit>());
            var next = counts[key];
            var here = t[key];

            foreach (var p in here.Keys)
            {
              ensurePoint(next, p);
              var v = next[p];
              var add = here[p];
              hitcount += add.Total;
              lock (v)
              {
                v.Count += add.Count;
                v.Tracks.AddRange(add.Tracks);
              }
            }
          }
        }
        return hitcount;
      }

      // TODO inline in release if possible
      internal static IEnumerable<XmlElement> selectNodes(XmlNode node, string name)
      {
        var result = new List<XmlElement>();
        foreach (var x in node.SelectNodes(name))
          result.Add(x as XmlElement);
        return result;
      }

      // // <summary>
      // // Load the XDocument
      // // </summary>
      // // <param name="path">The XML file to load</param>
      // // <remarks>Idiom to work with CA2202; we still double dispose the stream, but elude the rule.
      // // If this is ever a problem, we will need mutability and two streams, with explicit
      // // stream disposal if and only if the reader or writer doesn't take ownership
      // // Approved way is ugly -- https://docs.microsoft.com/en-us/visualstudio/code-quality/ca2202?view=vs-2019
      // // Also, this rule is deprecated
      // // </remarks>
      private static XmlDocument readXDocument(Stream stream)
      {
        var doc = new XmlDocument();

        try
        { doc.Load(stream); }
        catch (XmlException)
        { doc.LoadXml("<null/>"); }

        return doc;
      }

      // // <summary>
      // // Write the XDocument
      // // </summary>
      // // <param name="coverageDocument">The XML document to write</param>
      // // <param name="path">The XML file to write to</param>
      // // <remarks>Idiom to work with CA2202 as above</remarks>
      private static void writeXDocument(XmlDocument coverageDocument, Stream stream)
      { coverageDocument.Save(stream); }

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
        var xmlformat = xmlByFormat(format);// throw early on unsupported
        //let (m, i, m', s, v)

        var coverageDocument = readXDocument(coverageFile);

        var root = coverageDocument.DocumentElement;

        var startTimeNode = root.GetAttributeNode("startTime");

        if ((format == ReportFormat.NCover) && !Object.ReferenceEquals(startTimeNode, null))
        {
          //then
          //  let startTimeAttr = startTimeNode.Value

          //  let measureTimeAttr =
          //    root.GetAttribute("measureTime")

          //  let oldStartTime =
          //    DateTime.ParseExact(startTimeAttr, "o", null)

          //  let oldMeasureTime =
          //    DateTime.ParseExact(measureTimeAttr, "o", null)

          //  let st = minTime startTime oldStartTime
          //  startTime <- st.ToUniversalTime() // Min
          //  let mt = maxTime measureTime oldMeasureTime
          //  measureTime <- mt.ToUniversalTime() // Max

          //  root.SetAttribute(
          //    "startTime",
          //    startTime.ToString("o", System.Globalization.CultureInfo.InvariantCulture)
          //  )

          //  root.SetAttribute(
          //    "measureTime",
          //    measureTime.ToString("o", System.Globalization.CultureInfo.InvariantCulture)
          //  )

          //  root.SetAttribute(
          //    "driverVersion",
          //    "AltCover.Recorder "
          //    + System.Diagnostics.FileVersionInfo
          //      .GetVersionInfo(
          //        System.Reflection.Assembly
          //          .GetExecutingAssembly()
          //          .Location
          //      )
          //      .FileVersion
          //  )
        }

        var moduleNodes = selectNodes(coverageDocument, xmlformat.m);

        //moduleNodes
        //|> Seq.cast<XmlElement>
        //|> Seq.map (fun el -> el.GetAttribute(i), el)
        //|> Seq.filter (fun (k, _) -> counts.ContainsKey k)
        //|> Seq.iter (fun (k, affectedModule) ->
        //  let moduleHits = counts.[k]
        //  // Don't do this in one leap like --
        //  // affectedModule.Descendants(XName.Get("seqpnt"))
        //  // Get the methods, then flip their
        //  // contents before concatenating
        //  let nn = selectNodes affectedModule m'

        //  nn
        //  |> Seq.cast<XmlElement>
        //  |> Seq.collect (fun (method: XmlElement) ->
        //    s
        //    |> Seq.collect (fun (name, flag) ->
        //      let nodes = selectNodes method name

        //      nodes
        //      |> Seq.cast<XmlElement>
        //      |> Seq.map (fun x -> (x, flag))
        //      |> Seq.toList
        //      |> List.rev))
        //  |> Seq.mapi (fun counter (pt, flag) ->
        //    ((match format &&& ReportFormat.TrackMask with
        //      | ReportFormat.OpenCover ->
        //        "uspid"
        //        |> pt.GetAttribute
        //        |> (findIndexFromUspid flag)
        //      | _ -> counter),
        //     pt))
        //  |> Seq.filter (fst >> moduleHits.ContainsKey)
        //  |> Seq.iter (fun x ->
        //    let pt = snd x
        //    let counter = fst x

        //    let vc =
        //      Int64.TryParse(
        //        pt.GetAttribute(v),
        //        System.Globalization.NumberStyles.Integer,
        //        System.Globalization.CultureInfo.InvariantCulture
        //      )
        //      |> snd
        //    // Treat -ve visit counts (an exemption added in analysis) as zero
        //    let count = moduleHits.[counter]
        //    let visits = (max 0L vc) + count.Total
        //    pt.SetAttribute(v, visits.ToString(CultureInfo.InvariantCulture))
        //    pointProcess pt count.Tracks))

        postProcess(coverageDocument);

        // Save modified xml to a file
        outputFile.Seek(0L, SeekOrigin.Begin);
        outputFile.SetLength(0);

        if (own)
        { writeXDocument(coverageDocument, outputFile); }

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

    internal static void doFlushFile(
        Action<XmlDocument> postProcess,
      /*Action<XmlElement, IEnumerable<Track>>*/ object pointProcess,
        bool own,
        Dictionary<string, Dictionary<int, PointVisit>> counts,
        ReportFormat format,
        Stream report,
        object output
      )
    {
    }

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