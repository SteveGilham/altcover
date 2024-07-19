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
  [Serializable]
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

  [SuppressMessage("Gendarme.Rules.Performance",
                   "AvoidUncalledPrivateCodeRule",
                   Justification = "Internals Visible To")]
  [SuppressMessage("Gendarme.Rules.Performance",
                   "AvoidUninstantiatedInternalClassesRule",
                   Justification = "Internals Visible To")]
  [Serializable]
  internal enum Sampling
  {
    All = 0,
    Single = 1,
  };

  [SuppressMessage("Gendarme.Rules.Performance",
                   "AvoidUncalledPrivateCodeRule",
                   Justification = "Internals Visible To")]
  [SuppressMessage("Gendarme.Rules.Performance",
                   "AvoidUninstantiatedInternalClassesRule",
                   Justification = "Internals Visible To")]
  [Serializable]
  internal enum Tag
  {
    Null = 0,
    Time = 1,
    Call = 2,
    Both = 3,
    Table = 4,
  }

  [ExcludeFromCodeCoverage]
  [SuppressMessage("Gendarme.Rules.Performance",
                   "ImplementEqualsTypeRule",
                   Justification = "No use case")]
  [SuppressMessage("Gendarme.Rules.Design",
                   "OperatorEqualsShouldBeOverloadedRule",
                   Justification = "No use case")]
  [SuppressMessage("Gendarme.Rules.Performance",
                    "OverrideValueTypeDefaultsRule",
                    Justification = "You what, mate?")]
  internal struct Pair
  {
    public long Time;
    public int Call;

    [SuppressMessage("Gendarme.Rules.Performance",
                     "AvoidUncalledPrivateCodeRule",
                     Justification = "Internals Visible To")]
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

  [SuppressMessage("Gendarme.Rules.Performance",
                   "ImplementEqualsTypeRule",
                   Justification = "abstract type, No use case")]
  [SuppressMessage("Gendarme.Rules.Design",
                   "ConsiderUsingStaticTypeRule",
                   Justification = "Base class of union")]
  internal abstract class Track
  {
    internal const string Entry = "\u2611"; // BALLOT BOX WITH CHECK
    internal const string Exit = "\u2612"; // BALLOT BOX WITH X
  }

  [SuppressMessage("Gendarme.Rules.Performance",
                   "ImplementEqualsTypeRule",
                   Justification = "No use case")]
  internal sealed class Null : Track
  {
    public override string ToString()
    {
      return "AltCover.Null";
    }

    public override bool Equals(object obj)
    {
      return obj is Null;
    }

    public override int GetHashCode()
    {
      return string.Empty.GetHashCode();
    }
  }

  [SuppressMessage("Gendarme.Rules.Performance",
                   "ImplementEqualsTypeRule",
                   Justification = "No use case")]
  internal sealed class Time : Track
  {
    public readonly long Value;

    public Time(long time)
    {
      Value = time;
    }

    public override bool Equals(object obj)
    {
      if (obj is Time t)
      {
        return Value == t.Value;
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

  [SuppressMessage("Gendarme.Rules.Performance",
                   "ImplementEqualsTypeRule",
                   Justification = "No use case")]
  internal sealed class Call : Track
  {
    public readonly int Value;

    public Call(int call)
    {
      Value = call;
    }

    public override bool Equals(object obj)
    {
      if (obj is Call c)
      {
        return Value == c.Value;
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

  [SuppressMessage("Gendarme.Rules.Performance",
                   "ImplementEqualsTypeRule",
                   Justification = "No use case")]
  internal sealed class Both : Track
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

  internal sealed class Table : Track
  {
    public readonly Dictionary<string, Dictionary<int, PointVisit>> Value;

    public Table(Dictionary<string, Dictionary<int, PointVisit>> table)
    {
      Value = table;
    }
  }

  [SuppressMessage("Gendarme.Rules.Performance",
                   "ImplementEqualsTypeRule",
                   Justification = "No use case")]
  internal sealed class PointVisit
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
        " Tracks = '" + String.Join("; ", tracks) + "'";
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

  [SuppressMessage("Gendarme.Rules.Smells",
                   "AvoidLongParameterListsRule",
                   Justification = "Stable code")]
  internal static class Counter
  {
    [SuppressMessage("Gendarme.Rules.Design.Generic",
                     "AvoidDeclaringCustomDelegatesRule",
                     Justification = "Net Framework 2.0")]
    public delegate void PointProcessor(XmlElement doc, IEnumerable<Track> tracking);

    // <summary>
    // The time at which coverage run began
    // </summary>
    internal static DateTime startTime = DateTime.UtcNow;

    // <summary>
    // The finishing time taken of the coverage run
    // </summary>
    internal static DateTime measureTime = DateTime.UtcNow;

    // <summary>
    // The offset flag for branch counts
    // </summary>
    internal const int branchFlag = unchecked((int)0x80000000);

    internal const int branchMask = unchecked((int)0x7FFFFFFF);

    internal static long totalVisits = 0;

    internal static long branchVisits = 0;

#if DEBUG

    internal static class I
#else

    private static class I
#endif
    {
      [SuppressMessage("Gendarme.Rules.Performance",
                       "AvoidLargeStructureRule",
                       Justification = "No premature work")]
      [SuppressMessage("Gendarme.Rules.Performance",
                    "OverrideValueTypeDefaultsRule",
                    Justification = "You what, mate?")]
      internal struct CoverXml
      {
        internal string m;
        internal string i;
        internal string m2;
        internal Dictionary<string, int> s;
        internal string v;
      }

      internal static readonly CoverXml openCoverXml =
        new CoverXml()
        {
          m = "//Module",
          i = "hash",
          m2 = "Classes/Class/Methods/Method",
          s = new Dictionary<string, int> { { "SequencePoints/SequencePoint", 0 },
            { "BranchPoints/BranchPoint", branchFlag }
          },
          v = "vc"
        };

      internal static readonly CoverXml nCoverXml =
        new CoverXml()
        {
          m = "//module",
          i = "moduleId",
          m2 = "method",
          s = new Dictionary<string, int> { { "seqpnt", 0 } },
          v = "visitcount"
        };

      internal static CoverXml XmlByFormat(ReportFormat format)
      {
        switch (format & ReportFormat.TrackMask)
        {
          case ReportFormat.OpenCover: return openCoverXml;
          case ReportFormat.NCover: return nCoverXml;
          default: throw new NotSupportedException(format.ToString());
        }
      }

      internal static DateTime MinTime(DateTime t1, DateTime t2)
      {
        return t1 < t2 ? t1 : t2;
      }

      internal static DateTime MaxTime(DateTime t1, DateTime t2)
      {
        return t1 > t2 ? t1 : t2;
      }

      internal static int FindIndexFromUspid(int flag, string uspid)
      {
        var f = Int32.TryParse(
              uspid,
              NumberStyles.Integer,
              CultureInfo.InvariantCulture,
              out int c);
        return f ? (c | flag) : -1;
      }

      internal static void EnsurePoint(Dictionary<int, PointVisit> counts, int hitPointId)
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

#if RUNNER
      [SuppressMessage("Gendarme.Rules.Performance",
                       "AvoidUncalledPrivateCodeRule",
                       Justification = "Internals Visible To")]
      internal static long AddTable(
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
              EnsurePoint(next, p);
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
#endif

      // TODO inline in release if possible
      internal static IEnumerable<XmlElement> SelectNodes(XmlNode node, string name)
      {
        var result = new List<XmlElement>();
        foreach (var x in node.SelectNodes(name))
          result.Add(x as XmlElement);
        return result;
      }

      // <summary>
      // Load the XDocument
      // </summary>
      // <param name="path">The XML file to load</param>
      // <remarks>Idiom to work with CA2202; we still double dispose the stream, but elude the rule.
      // If this is ever a problem, we will need mutability and two streams, with explicit
      // stream disposal if and only if the reader or writer doesn't take ownership
      // Approved way is ugly -- https://docs.microsoft.com/en-us/visualstudio/code-quality/ca2202?view=vs-2019
      // Also, this rule is deprecated
      // </remarks>
      private static XmlDocument ReadXDocument(Stream stream)
      {
        var doc = new XmlDocument();

        try
        { doc.Load(stream); }
        catch (XmlException)
        { doc.LoadXml("<null/>"); }

        return doc;
      }

      // <summary>
      // Write the XDocument
      // </summary>
      // <param name="coverageDocument">The XML document to write</param>
      // <param name="path">The XML file to write to</param>
      // <remarks>Idiom to work with CA2202 as above</remarks>
      private static void WriteXDocument(XmlDocument coverageDocument, Stream stream)
      { coverageDocument.Save(stream); }

      [SuppressMessage("Gendarme.Rules.Smells",
                       "AvoidLongMethodsRule",
                       Justification = "Well tested code")]
      [SuppressMessage("Gendarme.Rules.Smells",
                       "AvoidLongParameterListsRule",
                       Justification = "Stable code")]
      [SuppressMessage("Microsoft.Usage",
                       "CA1806:DoNotIgnoreMethodResults",
                       Justification = "TryParse fails safe")]
      public static DateTime UpdateReport(
      Action<XmlDocument> postProcess,
      PointProcessor pointProcess,
      bool own,
      Dictionary<string, Dictionary<int, PointVisit>> counts,
      ReportFormat format,
      Stream coverageFile,
      Stream outputFile
      )
      {
        var flushStart = DateTime.UtcNow;
        var xmlformat = XmlByFormat(format);// throw early on unsupported

        var coverageDocument = ReadXDocument(coverageFile);

        var root = coverageDocument.DocumentElement;

        var startTimeNode = root.GetAttributeNode("startTime");

        if ((format == ReportFormat.NCover) && startTimeNode is object)
        {
          var startTimeAttr = startTimeNode.Value;
          var measureTimeAttr = root.GetAttribute("measureTime");
          var oldStartTime = DateTime.ParseExact(startTimeAttr, "o", null);
          var oldMeasureTime = DateTime.ParseExact(measureTimeAttr, "o", null);

          var st = MinTime(startTime, oldStartTime);
          startTime = st.ToUniversalTime(); // Min
          var mt = MaxTime(measureTime, oldMeasureTime);
          measureTime = mt.ToUniversalTime(); // Max

          root.SetAttribute(
            "startTime",
            startTime.ToString("o", System.Globalization.CultureInfo.InvariantCulture)
          );

          root.SetAttribute(
            "measureTime",
            measureTime.ToString("o", System.Globalization.CultureInfo.InvariantCulture)
          );

          root.SetAttribute(
            "driverVersion",
            "AltCover.Recorder "
            + System.Diagnostics.FileVersionInfo
              .GetVersionInfo(
                System.Reflection.Assembly
                  .GetExecutingAssembly()
                  .Location
              )
              .FileVersion
          );
        }

        //let moduleNodes =
        //  selectNodes coverageDocument m
        var moduleNodes = SelectNodes(coverageDocument, xmlformat.m);

        //moduleNodes
        //|> Seq.cast < XmlElement >
        foreach (var el in moduleNodes)
        {
          //|> Seq.map(fun el->el.GetAttribute(i), el)
          var k = el.GetAttribute(xmlformat.i);

          // |> Seq.filter(fun(k, _)->counts.ContainsKey k)
          if (!counts.ContainsKey(k)) continue;

          // let moduleHits = counts.[k]
          var moduleHits = counts[k];

          // Don't do this in one leap like --
          // affectedModule.Descendants(XName.Get("seqpnt"))
          // Get the methods, then flip their
          // contents before concatenating
          // let nn = selectNodes affectedModule m'
          var nn = SelectNodes(el, xmlformat.m2);

          //nn
          //|> Seq.cast < XmlElement >
          //|> Seq.collect(fun(method: XmlElement)->
          var nodes = new List<KeyValuePair<XmlElement, int>>();
          foreach (var method in nn)
          {
            //  s
            //|> Seq.collect(fun(name, flag)->
            foreach (var nameflag in xmlformat.s)
            {
              //  let nodes = selectNodes method name

              //  nodes
              //  |> Seq.cast<XmlElement>
              var nodes1 = new List<KeyValuePair<XmlElement, int>>();
              foreach (var node in SelectNodes(method, nameflag.Key))
              {
                //  |> Seq.map(fun x-> (x, flag))
                nodes1.Insert(0, new KeyValuePair<XmlElement, int>(node, nameflag.Value));
              }

              nodes1.Reverse();
              //  |> Seq.toList
              //  |> List.rev))

              nodes.AddRange(nodes);
            }
          }

          int counter = -1;
          foreach (var node in nodes)
          {
            ++counter;
            var index = counter;
            if ((format & ReportFormat.TrackMask) ==
              ReportFormat.OpenCover)
            {
              index = FindIndexFromUspid(node.Value, node.Key.GetAttribute("uspid"));
            }

            if (moduleHits.TryGetValue(index, out PointVisit value))
            {
              var pt = node.Key;

              Int64.TryParse(
                pt.GetAttribute(xmlformat.v),
                NumberStyles.Integer,
                CultureInfo.InvariantCulture,
                out var vc
              );

              // Treat -ve visit counts (an exemption added in analysis) as zero
              var count = value;
              var visits = Math.Max(vc, 0) + count.Total;
              pt.SetAttribute(xmlformat.v, visits.ToString(CultureInfo.InvariantCulture));
              pointProcess(pt, count.Tracks);
            }
          }
        }

        postProcess(coverageDocument);

        // Save modified xml to a file
        outputFile.Seek(0L, SeekOrigin.Begin);
        outputFile.SetLength(0);

        if (own)

        {
          WriteXDocument(coverageDocument, outputFile);
        }

        return flushStart;
      }

      [SuppressMessage("Gendarme.Rules.Smells",
                       "AvoidLongParameterListsRule",
                       Justification = "Stable code")]
      public static TimeSpan DoFlush(
        Action<XmlDocument> postProcess,
        PointProcessor pointProcess,
        bool own,
        Dictionary<string, Dictionary<int, PointVisit>> counts,
        ReportFormat format,
        Stream coverageFile,
        Stream outputFile
      )
      {
        var flushStart =
          UpdateReport(postProcess, pointProcess, own, counts, format, coverageFile, outputFile);

        return new TimeSpan(DateTime.UtcNow.Ticks - flushStart.Ticks);
      }
    }

    // "Public" API
    [SuppressMessage("Gendarme.Rules.Maintainability",
                     "AvoidUnnecessarySpecializationRule",
                     Justification = "No speculative generalization")]
    internal static void AddSingleVisit(
      Dictionary<string, Dictionary<int, PointVisit>> counts,
      string moduleId,
      int hitPointId,
      Track context
      )
    {
      if (counts.TryGetValue(moduleId, out Dictionary<int, PointVisit> value))
      {
        var next = value;
        I.EnsurePoint(next, hitPointId);
        var v = next[hitPointId];
        if (context is Null)
        { v.Step(); }
        else { v.Track(context); }
      }
    }

#if RUNNER

    [SuppressMessage("Gendarme.Rules.Performance",
                     "AvoidUncalledPrivateCodeRule",
                     Justification = "Internals Visible To")]
    internal static long AddVisit(
      Dictionary<string, Dictionary<int, PointVisit>> counts,
      string moduleId,
      int hitPointId,
      Track context
      )
    {
      if (context is Table t)
      {
        return I.AddTable(counts, t.Value);
      }

      AddSingleVisit(counts, moduleId, hitPointId, context);
      return 1;
    }

    [SuppressMessage("Gendarme.Rules.Performance",
                     "AvoidUncalledPrivateCodeRule",
                     Justification = "Internals Visible To")]
    [SuppressMessage("Gendarme.Rules.Smells",
                       "AvoidLongParameterListsRule",
                       Justification = "Stable code")]
    public static TimeSpan DoFlushStream(
      Action<XmlDocument> postProcess,
      PointProcessor pointProcess,
      bool own,
      Dictionary<string, Dictionary<int, PointVisit>> counts,
      ReportFormat format,
      Stream coverageFile,
      Stream outputFile
      )
    {
      return I.DoFlush(postProcess, pointProcess, own, counts, format, coverageFile, outputFile);
    }

#else

    [SuppressMessage("Microsoft.Reliability",
                     "CA2000:Dispose objects before losing scope",
                     Justification = "MemoryStream is 'use'd if created")]
    internal static TimeSpan DoFlushStream(
        Action<XmlDocument> postProcess,
        PointProcessor pointProcess,
        bool own,
        Dictionary<string, Dictionary<int, PointVisit>> counts,
        ReportFormat format,
        Stream coverageFile,
        string output) // option
    {
      using (var target = string.IsNullOrEmpty(output) ?
                         new MemoryStream() as Stream :
            new FileStream(
              output,
              FileMode.OpenOrCreate,
              FileAccess.Write,
              FileShare.None,
              4096,
              FileOptions.SequentialScan
            ))
      {
        var outputFile = string.IsNullOrEmpty(output) ?
          coverageFile : target;
        return I.DoFlush(postProcess, pointProcess, own, counts, format, coverageFile, outputFile);
      }
    }

    [SuppressMessage("Gendarme.Rules.Correctness",
                     "EnsureLocalDisposalRule",
                     Justification = "'zip' owns 'container' and is 'Close()'d")]
    [SuppressMessage("Microsoft.Reliability",
                     "CA2000:Dispose objects before losing scope",
                     Justification = "'zip' owns 'container' and is 'Close()'d")]
    internal static TimeSpan DoFlushFile(
        Action<XmlDocument> postProcess,
        PointProcessor pointProcess,
        bool own,
        Dictionary<string, Dictionary<int, PointVisit>> counts,
        ReportFormat format,
        string report,
        string output // option
      )
    {
      var zipped = (format & ReportFormat.Zipped) != 0;
      if (!zipped)
      {
        using (var coverageFile = new FileStream(
            report,
            FileMode.Open,
            FileAccess.ReadWrite,
            FileShare.None,
            4096,
            FileOptions.SequentialScan
          ))
          return DoFlushStream(postProcess, pointProcess, own, counts, format, coverageFile, output);
      }
      else
      {
        var container =
          new FileStream(
            report + ".zip",
            FileMode.Open,
            FileAccess.ReadWrite,
            FileShare.None,
            4096,
            FileOptions.SequentialScan
          );
        using (var target = string.IsNullOrEmpty(output) ?
          new MemoryStream() as Stream :
                               new FileStream(
                                 output,
                                 FileMode.OpenOrCreate,
                                 FileAccess.Write,
                                 FileShare.None,
                                 4096,
                                 FileOptions.SequentialScan
                               ))
        {
          try
          {
            ZipConstants.DefaultCodePage = 65001; //UTF-8 as System.IO.Compression.ZipFile uses internally
            var zip = new ZipFile(container);

            try
            {
              var entryName = Path.GetFileName(report);
              var entry = zip.GetEntry(entryName);

              var result = new TimeSpan(0);
              using (var reader = zip.GetInputStream(entry))
              {
                result = I.DoFlush(postProcess, pointProcess, own, counts, format, reader, target);
              }

              if (string.IsNullOrEmpty(output))
              {
                zip.BeginUpdate();
                zip.Delete(entry);
                target.Seek(0L, SeekOrigin.Begin); //|> ignore

                zip.Add(new Source(target), entryName);
                zip.CommitUpdate();
              }
              return result;
            }
            finally { zip.Close(); }
          }
          catch (ZipException)
          {
            using (var reader = new MemoryStream())
            { return I.DoFlush(postProcess, pointProcess, own, counts, format, reader, target); }
          }
        }
      }
    }

#if !RUNNER

    internal sealed class Source : IStaticDataSource
    {
      private readonly Stream _target;

      public Source(Stream t)
      {
        _target = t;
      }

      [SuppressMessage("Gendarme.Rules.Design",
                       "ConsiderConvertingMethodToPropertyRule",
                       Justification = "Third party interface")]
      public Stream GetSource()
      {
        return _target;
      }
    }

#endif

#endif // !RUNNER
  }
}