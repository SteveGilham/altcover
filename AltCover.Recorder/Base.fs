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

  /// <summary>
  /// Save sequence point hit counts to xml report file
  /// </summary>
  /// <param name="hitCounts">The coverage results to incorporate</param>
  /// <param name="coverageFile">The coverage file to update as a stream</param>
  let internal UpdateReport own (counts:Dictionary<string, Dictionary<int, int>>) coverageFile =
    let flushStart = DateTime.UtcNow
    let coverageDocument = ReadXDocument coverageFile
    let root = coverageDocument.DocumentElement

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

    coverageDocument.SelectNodes("//module")
    |> Seq.cast<XmlElement>
    |> Seq.map (fun el -> el.GetAttribute("moduleId"), el)
    |> Seq.filter (fun (k,e) -> counts.ContainsKey k)
    |> Seq.iter (fun(k,affectedModule) ->
        let moduleHits = counts.[k]

        // Don't do this in one leap like --
        // affectedModule.Descendants(XName.Get("seqpnt"))
        // Get the methods, then flip their
        // contents before concatenating
        affectedModule.SelectNodes("method")
        |> Seq.cast<XmlElement>
        |> Seq.collect (fun (``method``:XmlElement) -> ``method``.SelectNodes("seqpnt")
                                                        |> Seq.cast<XmlElement>
                                                        |> Seq.toList |> List.rev)
        |> Seq.mapi (fun counter pt -> (counter, pt))
        |> Seq.filter (fst >> moduleHits.ContainsKey)
        |> Seq.iter (fun x ->
            let pt = snd x
            let counter = fst x
            let attribute = pt.GetAttribute("visitcount")
            let value = if String.IsNullOrEmpty attribute then "0" else attribute
            let vc = Int32.TryParse(value,
                                    System.Globalization.NumberStyles.Integer,
                                    System.Globalization.CultureInfo.InvariantCulture)
            // Treat -ve visit counts (an exemption added in analysis) as zero
            let visits = moduleHits.[counter] + (max 0 (if fst vc then snd vc else 0))
            pt.SetAttribute("visitcount", visits.ToString(CultureInfo.InvariantCulture))))

    // Save modified xml to a file
    coverageFile.Seek(0L, SeekOrigin.Begin) |> ignore
    coverageFile.SetLength 0L
    if own then WriteXDocument coverageDocument coverageFile
    flushStart

  let DoFlush own counts report =
    use coverageFile = new FileStream(report, FileMode.Open, FileAccess.ReadWrite, FileShare.None, 4096, FileOptions.SequentialScan)
    let flushStart = UpdateReport own counts coverageFile
    TimeSpan(DateTime.UtcNow.Ticks - flushStart.Ticks)

  let AddVisit (counts:Dictionary<string, Dictionary<int, int>>) moduleId hitPointId =
    if not (counts.ContainsKey moduleId) then counts.[moduleId] <- Dictionary<int, int>()
    if not (counts.[moduleId].ContainsKey hitPointId) then
        counts.[moduleId].Add(hitPointId, 1)
    else
        counts.[moduleId].[hitPointId] <- 1 + counts.[moduleId].[hitPointId]