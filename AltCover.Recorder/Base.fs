#if RUNNER
namespace AltCover.Base
#else
namespace AltCover.Recorder
#endif

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Xml

// These conditionally internal for Gendarme
type internal ReportFormat =
  | NCover = 0
  | OpenCover = 1
  | OpenCoverWithTracking = 2

type internal Sampling =
  | All = 0
  | Single = 1

// TODO isolate where
#if RUNNER
[<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Performance",
  "AvoidUninstantiatedInternalClassesRule",
  Justification="Used as pattern match and compiled away")>]
#endif
type internal Tag =
  | Null = 0
  | Time = 1
  | Call = 2
  | Both = 3
  | Table = 4

#if NET2
[<System.Runtime.InteropServices.ProgIdAttribute("ExcludeFromCodeCoverage hack for OpenCover issue 615")>]
#else
[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
#endif
[<NoComparison>]
type internal Pair =
  { Time : int64; Call : int }

#if NET2
[<System.Runtime.InteropServices.ProgIdAttribute("ExcludeFromCodeCoverage hack for OpenCover issue 615")>]
#else
[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
#endif
[<NoComparison>]
type internal Track =
  | Null
  | Time of int64
  | Call of int
  | Both of Pair
  | Table of Dictionary<string, Dictionary<int, PointVisit>>
and [<NoComparison>]
#if NET2
[<System.Runtime.InteropServices.ProgIdAttribute("ExcludeFromCodeCoverage hack for OpenCover issue 615")>]
#else
[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
#endif
    internal PointVisit =
    {
      mutable Count : int64
      Tracks : List<Track>
    }
    with
    static member internal Create () = { Count = 0L; Tracks = List<Track>() }
    member internal self.Step() = System.Threading.Interlocked.Increment(&self.Count) |> ignore
    member internal self.Track something = lock self.Tracks (fun () -> self.Tracks.Add something)
    member internal self.Total() = self.Count + int64 self.Tracks.Count

[<RequireQualifiedAccess>]
module internal Counter =
  // "Public" "fields"

  /// <summary>
  /// The time at which coverage run began
  /// </summary>
  let mutable internal startTime = DateTime.UtcNow

  /// <summary>
  /// The finishing time taken of the coverage run
  /// </summary>
  let mutable internal measureTime = DateTime.UtcNow

  /// <summary>
  /// The offset flag for branch counts
  /// </summary>
  let internal branchFlag = 0x80000000

  let internal branchMask = 0x7FFFFFFF

  // Implementation details
#if DEBUG
  module internal I =
#else
  module private I =
#endif

    /// <summary>
    /// Load the XDocument
    /// </summary>
    /// <param name="path">The XML file to load</param>
    /// <remarks>Idiom to work with CA2202; we still double dispose the stream, but elude the rule.
    /// If this is ever a problem, we will need mutability and two streams, with explicit
    /// stream disposal if and only if the reader or writer doesn't take ownership
    /// Approved way is ugly -- https://docs.microsoft.com/en-us/visualstudio/code-quality/ca2202?view=vs-2019
    /// Also, this rule is deprecated
    /// </remarks>
    let private readXDocument(stream : Stream) =
      let doc = XmlDocument()
      doc.Load(stream)
      doc

    /// <summary>
    /// Write the XDocument
    /// </summary>
    /// <param name="coverageDocument">The XML document to write</param>
    /// <param name="path">The XML file to write to</param>
    /// <remarks>Idiom to work with CA2202 as above</remarks>
    let private writeXDocument (coverageDocument : XmlDocument) (stream : Stream) =
      coverageDocument.Save(stream)

    let internal findIndexFromUspid flag uspid =
      let f, c =
        Int32.TryParse
          (uspid, System.Globalization.NumberStyles.Integer,
           System.Globalization.CultureInfo.InvariantCulture)
      if f then (c ||| flag)
      else -1

    let internal openCoverXml =
      ("//Module", "hash", "Classes/Class/Methods/Method",
       [ ("SequencePoints/SequencePoint", 0)
         ("BranchPoints/BranchPoint", branchFlag) ], "vc")

    let internal nCoverXml =
      ("//module", "moduleId", "method", [ ("seqpnt", 0) ], "visitcount")

    let internal xmlByFormat format =
      match format with
      | ReportFormat.OpenCoverWithTracking
      | ReportFormat.OpenCover -> openCoverXml
      | _ -> nCoverXml

    let internal minTime (t1:DateTime) (t2:DateTime) =
      if t1 < t2
      then t1
      else t2

    let internal maxTime (t1:DateTime) (t2:DateTime) =
      if t1 > t2
      then t1
      else t2

    /// <summary>
    /// Save sequence point hit counts to xml report file
    /// </summary>
    /// <param name="hitCounts">The coverage results to incorporate</param>
    /// <param name="coverageFile">The coverage file to update as a stream</param>
    [<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Smells",
     "AvoidLongParameterListsRule",
     Justification="Most of this gets curried away")>]
    let internal updateReport (postProcess : XmlDocument -> unit)
        (pointProcess : XmlElement -> List<Track> -> unit) own
        (counts : Dictionary<string, Dictionary<int, PointVisit>>) format coverageFile
        (outputFile : Stream) =
      let flushStart = DateTime.UtcNow
      let coverageDocument = readXDocument coverageFile
      let root = coverageDocument.DocumentElement

      if format = ReportFormat.NCover then
        let startTimeAttr = root.GetAttribute("startTime")
        let measureTimeAttr = root.GetAttribute("measureTime")
        let oldStartTime = DateTime.ParseExact(startTimeAttr, "o", null)
        let oldMeasureTime = DateTime.ParseExact(measureTimeAttr, "o", null)
        let st = minTime startTime oldStartTime
        startTime <- st.ToUniversalTime() // Min
        let mt = maxTime measureTime  oldMeasureTime
        measureTime <- mt.ToUniversalTime() // Max
        root.SetAttribute
          ("startTime",
           startTime.ToString("o", System.Globalization.CultureInfo.InvariantCulture))
        root.SetAttribute
          ("measureTime",
           measureTime.ToString("o", System.Globalization.CultureInfo.InvariantCulture))
        root.SetAttribute
          ("driverVersion",
           "AltCover.Recorder "
           + System.Diagnostics.FileVersionInfo.GetVersionInfo(
               System.Reflection.Assembly.GetExecutingAssembly().Location).FileVersion)

      let (m, i, m', s, v) = xmlByFormat format

  #if NET2
      let
  #else
      use
  #endif
         moduleNodes = coverageDocument.SelectNodes(m)
      moduleNodes
      |> Seq.cast<XmlElement>
      |> Seq.map (fun el -> el.GetAttribute(i), el)
      |> Seq.filter (fun (k, _) -> counts.ContainsKey k)
      |> Seq.iter (fun (k, affectedModule) ->
           let moduleHits = counts.[k]
           // Don't do this in one leap like --
           // affectedModule.Descendants(XName.Get("seqpnt"))
           // Get the methods, then flip their
           // contents before concatenating
  #if NET2
           let
  #else
           use
  #endif
               nn = affectedModule.SelectNodes(m')
           nn
           |> Seq.cast<XmlElement>
           |> Seq.collect (fun (method : XmlElement) ->
                s
                |> Seq.collect (fun (name, flag) ->
  #if NET2
                     let
  #else
                     use
  #endif
                         nodes = method.SelectNodes(name)
                     nodes
                     |> Seq.cast<XmlElement>
                     |> Seq.map (fun x -> (x, flag))
                     |> Seq.toList
                     |> List.rev))
           |> Seq.mapi (fun counter (pt, flag) ->
                ((match format with
                  | ReportFormat.OpenCoverWithTracking | ReportFormat.OpenCover ->
                    "uspid"
                    |> pt.GetAttribute
                    |> (findIndexFromUspid flag)
                  | _ -> counter), pt))
           |> Seq.filter (fst >> moduleHits.ContainsKey)
           |> Seq.iter (fun x ->
                let pt = snd x
                let counter = fst x
                let vc =
                  Int64.TryParse
                    (pt.GetAttribute(v), System.Globalization.NumberStyles.Integer,
                     System.Globalization.CultureInfo.InvariantCulture) |> snd
                // Treat -ve visit counts (an exemption added in analysis) as zero
                let count = moduleHits.[counter]
                let visits = (max 0L vc) + count.Total()
                pt.SetAttribute(v, visits.ToString(CultureInfo.InvariantCulture))
                pointProcess pt count.Tracks))
      postProcess coverageDocument

      // Save modified xml to a file
      outputFile.Seek(0L, SeekOrigin.Begin) |> ignore
      outputFile.SetLength 0L
      if own then writeXDocument coverageDocument outputFile
      flushStart

    let internal ensureModule (counts : Dictionary<string, Dictionary<int, PointVisit>>) moduleId =
      if not (counts.ContainsKey moduleId) then
        lock counts (fun () ->
          if not (counts.ContainsKey moduleId)
          then counts.Add(moduleId, Dictionary<int, PointVisit>())
        )

    let internal ensurePoint (counts : Dictionary<int, PointVisit>) hitPointId =
      if not (counts.ContainsKey hitPointId) then
        lock counts (fun () ->
        if not (counts.ContainsKey hitPointId)
        then counts.Add(hitPointId, PointVisit.Create()))

#if RUNNER
    let internal addTable (counts : Dictionary<string, Dictionary<int, PointVisit>>)
                          (t : Dictionary<string, Dictionary<int, PointVisit>>) =
      let mutable hitcount = 0L
      t.Keys
      |> Seq.iter (fun m -> ensureModule counts m
                            let next = counts.[m]
                            let here = t.[m]
                            here.Keys |>
                            Seq.iter (fun p -> ensurePoint next p
                                               let v = next.[p]
                                               let add = here.[p]
                                               hitcount <- hitcount + add.Total()
                                               lock v (fun () -> v.Count <- v.Count + add.Count
                                                                 v.Tracks.AddRange(add.Tracks)
                            )))
      hitcount
#endif
  // "Public" API
  let internal addSingleVisit  (counts : Dictionary<string, Dictionary<int, PointVisit>>)
      moduleId hitPointId context =
    I.ensureModule counts moduleId
    let next = counts.[moduleId]
    I.ensurePoint next hitPointId

    let v = next.[hitPointId]
    match context with
    | Null -> v.Step()
    | something -> v.Track something

#if RUNNER
  let internal addVisit (counts : Dictionary<string, Dictionary<int, PointVisit>>)
      moduleId hitPointId context =
    match context with
    | Table t -> I.addTable counts t
    | _ -> addSingleVisit counts moduleId hitPointId context
           1L
#endif

  [<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Reliability",
                                                    "CA2000:DisposeObjectsBeforeLosingScope",
                                                    Justification = "'Target' is disposed")>]
  [<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Smells",
   "AvoidLongParameterListsRule",
   Justification="Most of this gets curried away")>]
  let internal doFlush postProcess pointProcess own counts format report output =
    use coverageFile =
      new FileStream(report, FileMode.Open, FileAccess.ReadWrite, FileShare.None, 4096,
                      FileOptions.SequentialScan)

    use target =
      match output with
      | None -> new MemoryStream() :> Stream
      | Some f ->
        new FileStream(f, FileMode.OpenOrCreate, FileAccess.Write, FileShare.None, 4096,
                        FileOptions.SequentialScan) :> Stream

    let outputFile =
      if Option.isSome output then target
      else coverageFile :> Stream

    let flushStart =
      I.updateReport postProcess pointProcess own counts format coverageFile outputFile
    TimeSpan(DateTime.UtcNow.Ticks - flushStart.Ticks)