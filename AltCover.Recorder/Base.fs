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

type ReportFormat = NCover = 0 | OpenCover = 1

module Counter =
   /// <summary>
   /// The time at which coverage run began
   /// </summary>
  let mutable internal startTime = DateTime.UtcNow

  /// <summary>
  /// The finishing time taken of the coverage run
  /// </summary>
  let mutable internal measureTime = DateTime.UtcNow

  /// <summary>
  /// Load the XDocument
  /// </summary>
  /// <param name="path">The XML file to load</param>
  /// <remarks>Idiom to work with CA2202; we still double dispose the stream, but elude the rule.
  /// If this is ever a problem, we will need mutability and two streams, with explicit
  /// stream disposal if and only if the reader or writer doesn't take ownership
  /// </remarks>
  let private ReadXDocument (stream:Stream)  =
    let doc = XmlDocument()
    doc.Load(stream)
    doc

  /// <summary>
  /// Write the XDocument
  /// </summary>
  /// <param name="coverageDocument">The XML document to write</param>
  /// <param name="path">The XML file to write to</param>
  /// <remarks>Idiom to work with CA2202 as above</remarks>
  let private WriteXDocument (coverageDocument:XmlDocument) (stream:Stream) =
    coverageDocument.Save(stream)

  let internal FindIndexFromUspid uspid =
    let f, c = Int32.TryParse( uspid ,
                    System.Globalization.NumberStyles.Integer,
                    System.Globalization.CultureInfo.InvariantCulture)
    if f then c else -1

  /// <summary>
  /// Save sequence point hit counts to xml report file
  /// </summary>
  /// <param name="hitCounts">The coverage results to incorporate</param>
  /// <param name="coverageFile">The coverage file to update as a stream</param>
  let internal UpdateReport (postProcess:XmlDocument -> unit) own (counts:Dictionary<string, Dictionary<int, int * (int64 option * int option) list>>) format coverageFile =
    let flushStart = DateTime.UtcNow
    let coverageDocument = ReadXDocument coverageFile
    let root = coverageDocument.DocumentElement

    if format = ReportFormat.NCover then
        let startTimeAttr = root.GetAttribute("startTime")
        let measureTimeAttr = root.GetAttribute("measureTime")
        let oldStartTime = DateTime.ParseExact(startTimeAttr, "o", null)
        let oldMeasureTime = DateTime.ParseExact(measureTimeAttr, "o", null)

        startTime <- (if startTime < oldStartTime then startTime else oldStartTime).ToUniversalTime() // Min
        measureTime <- (if measureTime > oldMeasureTime then measureTime else oldMeasureTime).ToUniversalTime() // Max

        root.SetAttribute("startTime",
                            startTime.ToString("o", System.Globalization.CultureInfo.InvariantCulture))
        root.SetAttribute("measureTime",
                        measureTime.ToString("o", System.Globalization.CultureInfo.InvariantCulture))
        root.SetAttribute("driverVersion", "AltCover.Recorder " +
                                     System.Diagnostics.FileVersionInfo.GetVersionInfo(System.Reflection.Assembly.GetExecutingAssembly().Location).FileVersion)

    let (m, i, m', s, v) = match format with
                           | ReportFormat.OpenCover -> ("//Module", "hash", "Classes/Class/Methods/Method", "SequencePoints/SequencePoint", "vc")
                           | _ -> ("//module", "moduleId", "method", "seqpnt", "visitcount")
    coverageDocument.SelectNodes(m)
    |> Seq.cast<XmlElement>
    |> Seq.map (fun el -> el.GetAttribute(i), el)
    |> Seq.filter (fun (k,e) -> counts.ContainsKey k)
    |> Seq.iter (fun(k,affectedModule) ->
        let moduleHits = counts.[k]

        // Don't do this in one leap like --
        // affectedModule.Descendants(XName.Get("seqpnt"))
        // Get the methods, then flip their
        // contents before concatenating
        let nn = affectedModule.SelectNodes(m')
        nn
        |> Seq.cast<XmlElement>
        |> Seq.collect (fun (``method``:XmlElement) -> ``method``.SelectNodes(s)
                                                        |> Seq.cast<XmlElement>
                                                        |> Seq.toList |> List.rev)
        |> Seq.mapi (fun counter pt -> ((match format with
                                        | ReportFormat.OpenCover -> "uspid" |> pt.GetAttribute |> FindIndexFromUspid
                                        | _ -> counter),
                                        pt))
        |> Seq.filter (fst >> moduleHits.ContainsKey)
        |> Seq.iter (fun x ->
            let pt = snd x
            let counter = fst x
            let vc = Int32.TryParse(pt.GetAttribute(v),
                                    System.Globalization.NumberStyles.Integer,
                                    System.Globalization.CultureInfo.InvariantCulture) |> snd
            // Treat -ve visit counts (an exemption added in analysis) as zero
            let visits = (fst moduleHits.[counter]) + (max 0 vc)
            pt.SetAttribute(v, visits.ToString(CultureInfo.InvariantCulture))))

    postProcess coverageDocument

    // Save modified xml to a file
    coverageFile.Seek(0L, SeekOrigin.Begin) |> ignore
    coverageFile.SetLength 0L
    if own then WriteXDocument coverageDocument coverageFile
    flushStart

  let DoFlush postProcess own counts format report =
    use coverageFile = new FileStream(report, FileMode.Open, FileAccess.ReadWrite, FileShare.None, 4096, FileOptions.SequentialScan)
    let flushStart = UpdateReport postProcess own counts format coverageFile
    TimeSpan(DateTime.UtcNow.Ticks - flushStart.Ticks)

  let AddVisit (counts:Dictionary<string, Dictionary<int, int * (int64 option * int option) list>>) moduleId hitPointId context =
    if not (counts.ContainsKey moduleId) then counts.[moduleId] <- Dictionary<int, int * (int64 option * int option) list>()
    if not (counts.[moduleId].ContainsKey hitPointId) then
        counts.[moduleId].Add(hitPointId, (0,[]))
    counts.[moduleId].[hitPointId] <- match context with
                                      | (None, None) -> (1 + fst counts.[moduleId].[hitPointId], snd counts.[moduleId].[hitPointId])
                                      | something -> (1 + fst counts.[moduleId].[hitPointId], something :: snd counts.[moduleId].[hitPointId])