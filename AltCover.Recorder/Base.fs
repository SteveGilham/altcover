#if RUNNER
namespace AltCover
#else
namespace AltCover.Recorder
#endif

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.Globalization
open System.IO
open System.Xml

type internal ReportFormat =
  | NCover = 0
  | OpenCover = 1
  | OpenCoverWithTracking = 2
  | NativeJson = 3
  | NativeJsonWithTracking = 4

#if !RUNNER
open ICSharpCode.SharpZipLib.Zip

[<AttributeUsage(AttributeTargets.Class, Inherited = false, AllowMultiple = false)>]
[<Sealed>]
[<SuppressMessage("Gendarme.Rules.Performance",
                  "AvoidUninstantiatedInternalClassesRule",
                  Justification = "Looks like a bug, not detecting its use")>]
type internal ExcludeFromCodeCoverageAttribute() =
  inherit Attribute()
#endif

type internal Sampling =
  | All = 0
  | Single = 1

// TODO isolate where
#if RUNNER
[<SuppressMessage("Gendarme.Rules.Performance",
                  "AvoidUninstantiatedInternalClassesRule",
                  Justification = "Used as pattern match and compiled away")>]
#endif
type internal Tag =
  | Null = 0
  | Time = 1
  | Call = 2
  | Both = 3
  | Table = 4

[<ExcludeFromCodeCoverage>]
[<NoComparison>]
type internal Pair = { Time: int64; Call: int }

[<ExcludeFromCodeCoverage>]
[<NoComparison>]
type internal Track =
  | Null
  | Time of int64
  | Call of int
  | Both of Pair
  | Table of Dictionary<string, Dictionary<int, PointVisit>>
  static member internal Entry = "\u2611" // BALLOT BOX WITH CHECK
  static member internal Exit = "\u2612" // BALLOT BOX WITH X

and [<NoComparison; ExcludeFromCodeCoverage>] internal PointVisit =
  { mutable Count: int64
    Tracks: List<Track> }
  static member internal Create() = { Count = 0L; Tracks = List<Track>() }

  member internal self.Step() =
    System.Threading.Interlocked.Increment(&self.Count)
    |> ignore

  member internal self.Track something =
    lock self.Tracks (fun () -> self.Tracks.Add something)

  member internal self.Total() = self.Count + int64 self.Tracks.Count

[<RequireQualifiedAccess>]
module internal Counter =
  // "Public" "fields"

  // // <summary>
  // // The time at which coverage run began
  // // </summary>
  let mutable internal startTime = DateTime.UtcNow

  // // <summary>
  // // The finishing time taken of the coverage run
  // // </summary>
  let mutable internal measureTime = DateTime.UtcNow

  // // <summary>
  // // The offset flag for branch counts
  // // </summary>
  let internal branchFlag = 0x80000000

  let internal branchMask = 0x7FFFFFFF

  [<SuppressMessage("Gendarme.Rules.Performance",
                    "AvoidUncalledPrivateCodeRule",
                    Justification = "Access by reflection in the monitor")>]
  let mutable internal totalVisits = 0L

  [<SuppressMessage("Gendarme.Rules.Performance",
                    "AvoidUncalledPrivateCodeRule",
                    Justification = "Access by reflection in the monitor")>]
  let mutable internal branchVisits = 0L

  // Implementation details
#if DEBUG
  module internal I =
#else
  module private I =
#endif

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
    let private readXDocument (stream: Stream) =
      let doc = XmlDocument()

      try
        doc.Load(stream)
      with :? XmlException -> doc.LoadXml "<null/>"

      doc

    // // <summary>
    // // Write the XDocument
    // // </summary>
    // // <param name="coverageDocument">The XML document to write</param>
    // // <param name="path">The XML file to write to</param>
    // // <remarks>Idiom to work with CA2202 as above</remarks>
    let private writeXDocument (coverageDocument: XmlDocument) (stream: Stream) =
      coverageDocument.Save(stream)

    let internal findIndexFromUspid flag uspid =
      let f, c =
        Int32.TryParse(
          uspid,
          System.Globalization.NumberStyles.Integer,
          System.Globalization.CultureInfo.InvariantCulture
        )

      if f then (c ||| flag) else -1

    let internal openCoverXml =
      ("//Module",
       "hash",
       "Classes/Class/Methods/Method",
       [ ("SequencePoints/SequencePoint", 0)
         ("BranchPoints/BranchPoint", branchFlag) ],
       "vc")

    let internal nCoverXml =
      ("//module", "moduleId", "method", [ ("seqpnt", 0) ], "visitcount")

    let internal xmlByFormat format =
      match format with
      | ReportFormat.OpenCoverWithTracking
      | ReportFormat.OpenCover -> openCoverXml
      | ReportFormat.NCover -> nCoverXml
      | _ ->
          raise (
            format
            |> (sprintf "%A")
            |> NotSupportedException)

    let internal minTime (t1: DateTime) (t2: DateTime) = if t1 < t2 then t1 else t2

    let internal maxTime (t1: DateTime) (t2: DateTime) = if t1 > t2 then t1 else t2

    [<SuppressMessage("Gendarme.Rules.Correctness",
                      "EnsureLocalDisposalRule",
                      Justification = "Not an IDisposeable at net2.0")>]
    let
#if !DEBUG
         inline
#endif
                  internal selectNodes (node:XmlNode) name =
      node.SelectNodes(name) |> Seq.cast<XmlNode>

    // // <summary>
    // // Save sequence point hit counts to xml report file
    // // </summary>
    // // <param name="hitCounts">The coverage results to incorporate</param>
    // // <param name="coverageFile">The coverage file to update as a stream</param>
    [<SuppressMessage("Gendarme.Rules.Smells",
                      "AvoidLongParameterListsRule",
                      Justification = "Most of this gets curried away")>]
    let internal updateReport
      (postProcess: XmlDocument -> unit)
      (pointProcess: XmlElement -> List<Track> -> unit)
      own
      (counts: Dictionary<string, Dictionary<int, PointVisit>>)
      format
      coverageFile
      (outputFile: Stream)
      =
      let flushStart = DateTime.UtcNow
      let (m, i, m', s, v) = xmlByFormat format // throw early on unsupported
      let coverageDocument = readXDocument coverageFile
      let root = coverageDocument.DocumentElement
      let startTimeNode = root.GetAttributeNode("startTime")

      if format = ReportFormat.NCover
         && Object.ReferenceEquals(startTimeNode, null) |> not then
        let startTimeAttr = startTimeNode.Value
        let measureTimeAttr = root.GetAttribute("measureTime")

        let oldStartTime =
          DateTime.ParseExact(startTimeAttr, "o", null)

        let oldMeasureTime =
          DateTime.ParseExact(measureTimeAttr, "o", null)

        let st = minTime startTime oldStartTime
        startTime <- st.ToUniversalTime() // Min
        let mt = maxTime measureTime oldMeasureTime
        measureTime <- mt.ToUniversalTime() // Max

        root.SetAttribute(
          "startTime",
          startTime.ToString("o", System.Globalization.CultureInfo.InvariantCulture)
        )

        root.SetAttribute(
          "measureTime",
          measureTime.ToString("o", System.Globalization.CultureInfo.InvariantCulture)
        )

        root.SetAttribute(
          "driverVersion",
          "AltCover.Recorder "
          + System
            .Diagnostics
            .FileVersionInfo
            .GetVersionInfo(
              System
                .Reflection
                .Assembly
                .GetExecutingAssembly()
                .Location
            )
            .FileVersion
        )

      let moduleNodes = selectNodes coverageDocument m

      moduleNodes
      |> Seq.cast<XmlElement>
      |> Seq.map (fun el -> el.GetAttribute(i), el)
      |> Seq.filter (fun (k, _) -> counts.ContainsKey k)
      |> Seq.iter
           (fun (k, affectedModule) ->
             let moduleHits = counts.[k]
             // Don't do this in one leap like --
             // affectedModule.Descendants(XName.Get("seqpnt"))
             // Get the methods, then flip their
             // contents before concatenating
             let nn = selectNodes affectedModule m'

             nn
             |> Seq.cast<XmlElement>
             |> Seq.collect
                  (fun (method: XmlElement) ->
                    s
                    |> Seq.collect
                         (fun (name, flag) ->
                           let nodes = selectNodes method name

                           nodes
                           |> Seq.cast<XmlElement>
                           |> Seq.map (fun x -> (x, flag))
                           |> Seq.toList
                           |> List.rev))
             |> Seq.mapi
                  (fun counter (pt, flag) ->
                    ((match format with
                      | ReportFormat.OpenCoverWithTracking
                      | ReportFormat.OpenCover ->
                          "uspid"
                          |> pt.GetAttribute
                          |> (findIndexFromUspid flag)
                      | _ -> counter),
                     pt))
             |> Seq.filter (fst >> moduleHits.ContainsKey)
             |> Seq.iter
                  (fun x ->
                    let pt = snd x
                    let counter = fst x

                    let vc =
                      Int64.TryParse(
                        pt.GetAttribute(v),
                        System.Globalization.NumberStyles.Integer,
                        System.Globalization.CultureInfo.InvariantCulture
                      )
                      |> snd
                    // Treat -ve visit counts (an exemption added in analysis) as zero
                    let count = moduleHits.[counter]
                    let visits = (max 0L vc) + count.Total()
                    pt.SetAttribute(v, visits.ToString(CultureInfo.InvariantCulture))
                    pointProcess pt count.Tracks))

      postProcess coverageDocument

      // Save modified xml to a file
      outputFile.Seek(0L, SeekOrigin.Begin) |> ignore
      outputFile.SetLength 0L

      if own then
        writeXDocument coverageDocument outputFile

      flushStart

    let internal ensureModule
      (counts: Dictionary<string, Dictionary<int, PointVisit>>)
      moduleId
      =
      if not (counts.ContainsKey moduleId) then
        lock
          counts
          (fun () ->
            if not (counts.ContainsKey moduleId) then
              counts.Add(moduleId, Dictionary<int, PointVisit>()))

    let internal ensurePoint (counts: Dictionary<int, PointVisit>) hitPointId =
      if not (counts.ContainsKey hitPointId) then
        lock
          counts
          (fun () ->
            if not (counts.ContainsKey hitPointId) then
              System.Threading.Interlocked.Increment(&totalVisits)
              |> ignore

              if hitPointId < 0 then
                System.Threading.Interlocked.Increment(&branchVisits)
                |> ignore

              counts.Add(hitPointId, PointVisit.Create()))

#if RUNNER
    let internal addTable
      (counts: Dictionary<string, Dictionary<int, PointVisit>>)
      (t: Dictionary<string, Dictionary<int, PointVisit>>)
      =
      let mutable hitcount = 0L

      t.Keys
      |> Seq.iter
           (fun m ->
             ensureModule counts m
             let next = counts.[m]
             let here = t.[m]

             here.Keys
             |> Seq.iter
                  (fun p ->
                    ensurePoint next p
                    let v = next.[p]
                    let add = here.[p]
                    hitcount <- hitcount + add.Total()

                    lock
                      v
                      (fun () ->
                        v.Count <- v.Count + add.Count
                        v.Tracks.AddRange(add.Tracks))))

      hitcount
#endif
    [<SuppressMessage("Gendarme.Rules.Smells",
                      "AvoidLongParameterListsRule",
                      Justification = "Most of this gets curried away")>]
    let doFlush postProcess pointProcess own counts format coverageFile outputFile =
      let flushStart =
        updateReport postProcess pointProcess own counts format coverageFile outputFile

      TimeSpan(DateTime.UtcNow.Ticks - flushStart.Ticks)

  // "Public" API
  let internal addSingleVisit
    (counts: Dictionary<string, Dictionary<int, PointVisit>>)
    moduleId
    hitPointId
    context
    =
    I.ensureModule counts moduleId
    let next = counts.[moduleId]
    I.ensurePoint next hitPointId

    let v = next.[hitPointId]

    match context with
    | Null -> v.Step()
    | something -> v.Track something

#if RUNNER
  let internal addVisit
    (counts: Dictionary<string, Dictionary<int, PointVisit>>)
    moduleId
    hitPointId
    context
    =
    match context with
    | Table t -> I.addTable counts t
    | _ ->
        addSingleVisit counts moduleId hitPointId context
        1L
#endif

  [<SuppressMessage("Gendarme.Rules.Smells",
                    "AvoidLongParameterListsRule",
                    Justification = "Most of this gets curried away")>]
  [<SuppressMessage("Microsoft.Reliability",
                    "CA2000:DisposeObjectsBeforeLosingScope",
                    Justification = "'target' is disposed")>]
  let internal doFlushStream
    postProcess
    pointProcess
    own
    counts
    format
    coverageFile
    output
    =
    use target =
      match output with
      | None -> new MemoryStream() :> Stream
      | Some f ->
          new FileStream(
            f,
            FileMode.OpenOrCreate,
            FileAccess.Write,
            FileShare.None,
            4096,
            FileOptions.SequentialScan
          )
          :> Stream

    let outputFile =
      if Option.isSome output then
        target
      else
        coverageFile :> Stream

    I.doFlush postProcess pointProcess own counts format coverageFile outputFile

#if !RUNNER
  [<SuppressMessage("Gendarme.Rules.Smells",
                    "AvoidLongParameterListsRule",
                    Justification = "Most of this gets curried away")>]
  [<SuppressMessage("Gendarme.Rules.Correctness",
                    "EnsureLocalDisposalRule",
                    Justification = "'zip' owns 'container' and is 'Close()'d")>]
  [<SuppressMessage("Microsoft.Reliability",
                    "CA2000:DisposeObjectsBeforeLosingScope",
                    Justification = "ald also 'target' is disposed")>]
  let internal doFlushFile postProcess pointProcess own counts format report output =
    if File.Exists report then
      use coverageFile =
        new FileStream(
          report,
          FileMode.Open,
          FileAccess.ReadWrite,
          FileShare.None,
          4096,
          FileOptions.SequentialScan
        )

      doFlushStream postProcess pointProcess own counts format coverageFile output
    else
      let container =
        new FileStream(
          report + ".zip",
          FileMode.Open,
          FileAccess.ReadWrite,
          FileShare.None,
          4096,
          FileOptions.SequentialScan
        )

      use target =
        match output with
        | None -> new MemoryStream() :> Stream
        | Some f ->
            new FileStream(
              f,
              FileMode.OpenOrCreate,
              FileAccess.Write,
              FileShare.None,
              4096,
              FileOptions.SequentialScan
            )
            :> Stream

      try
        ZipConstants.DefaultCodePage <- 65001 //UTF-8 as System.IO.Compression.ZipFile uses internally
        let zip = new ZipFile(container)

        try
          let entryName = report |> Path.GetFileName
          let entry = zip.GetEntry(entryName)

          let result =
            use reader = zip.GetInputStream(entry)
            I.doFlush postProcess pointProcess own counts format reader target

          if output.IsNone then
            zip.BeginUpdate()
            zip.Delete entry
            target.Seek(0L, SeekOrigin.Begin) |> ignore

            let source =
              { new IStaticDataSource with
                  member self.GetSource() = target }

            zip.Add(source, entryName)
            zip.CommitUpdate()

          result
        finally
          zip.Close()
      with :? ZipException ->
        use reader = new MemoryStream()
        I.doFlush postProcess pointProcess own counts format reader target

#endif // !RUNNER