// Based upon C# code by Sergiy Sakharov (sakharov@gmail.com)
// http://code.google.com/p/dot-net-coverage/source/browse/trunk/Coverage.Counter/Coverage.Counter.csproj

namespace AltCover.Recorder

open System
open System.Collections.Generic
open System.IO
open System.Runtime.CompilerServices
open System.Xml
open System.Xml.Linq

type Tracer = { Tracer : string }

module Instance =

   /// <summary>
   /// The time at which coverage run began
   /// </summary>
  let mutable private startTime = DateTime.Now

  /// <summary>
  /// Thime taken to perform coverage run
  /// </summary>
  let mutable private measureTime = DateTime.Now

  /// <summary>
  /// Gets the location of coverage xml file
  /// This property's IL code is modified to store actual file location
  /// </summary>
  [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
  let ReportFile = "Coverage.Default.xml"

  /// <summary>
  /// Accumulation of visit records
  /// </summary>
  let private Visits = new Dictionary<string, Dictionary<int, int>>();

  /// <summary>
  /// Interlock for report instances
  /// </summary>
  let private mutex = new System.Threading.Mutex(false, "AltCover.Recorder.Instance.mutex");
  
  /// <summary>
  /// Load the XDocument
  /// </summary>
  /// <param name="path">The XML file to load</param>
  /// <remarks>Idiom to work with CA2202; we still double dispose the stream, but elude the rule.
  /// If this is ever a problem, we will need mutability and two streams, with explicit 
  /// stream disposal if and only if the reader or writer doesn't take ownership
  /// </remarks>
  let private ReadXDocument (stream:FileStream)  =
     use reader = XmlReader.Create stream
     XDocument.Load reader

  /// <summary>
  /// Write the XDocument
  /// </summary>
  /// <param name="coverageDocument">The XML document to write</param>
  /// <param name="path">The XML file to write to</param>
  /// <remarks>Idiom to work with CA2202 as above</remarks>
  let private WriteXDocument (coverageDocument:XDocument) (stream:FileStream) =
     use writer = XmlWriter.Create stream
     coverageDocument.WriteTo writer

  /// <summary>
  /// Save sequence point hit counts to xml report file
  /// </summary>
  /// <param name="hitCounts">The coverage results to incorporate</param>
  let private UpdateReport counts =
    mutex.WaitOne(10000) |> ignore
    let flushStart = DateTime.Now;
    try
      // Edit xml report to store new hits
      use coverageFile = new FileStream(ReportFile, FileMode.Open, FileAccess.ReadWrite, FileShare.None, 4096, FileOptions.SequentialScan)
      let coverageDocument = ReadXDocument coverageFile

      let startTimeAttr = coverageDocument.Root.Attribute(XName.Get("startTime"))
      let measureTimeAttr = coverageDocument.Root.Attribute(XName.Get("measureTime"))
      let oldStartTime = DateTime.ParseExact(startTimeAttr.Value, "o", null)
      let oldMeasureTime = DateTime.ParseExact(measureTimeAttr.Value, "o", null)

      startTime <- if startTime < oldStartTime then startTime else oldStartTime // Min
      measureTime <- if measureTime > oldMeasureTime then measureTime else oldMeasureTime // Max

      startTimeAttr.SetValue(startTime.ToString("o", System.Globalization.CultureInfo.InvariantCulture));
      measureTimeAttr.SetValue(measureTime.ToString("o", System.Globalization.CultureInfo.InvariantCulture));

      counts
      |> Seq.iter (fun (pair : KeyValuePair<string, Dictionary<int,int>>) ->
          let moduleId = pair.Key;
          let moduleHits = pair.Value;
          let affectedModules =
              coverageDocument.Descendants(XName.Get("module"))
              |> Seq.filter (fun el -> el.Attribute(XName.Get("moduleId")).Value.Equals(moduleId))
              |> Seq.truncate 1 // at most 1

          affectedModules
          |> Seq.iter (fun affectedModule ->
          // Don't do this in one leap like --
          // affectedModule.Descendants(XName.Get("seqpnt"))
          // Get the methods, then flip their
          // contents before concatenating
          affectedModule.Descendants(XName.Get("method"))
          |> Seq.collect (fun (``method``:XElement) -> ``method``.Descendants(XName.Get("seqpnt"))
                                                       |> Seq.toList |> List.rev)
          |> Seq.mapi (fun counter pt -> (counter, pt))
          |> Seq.filter (fst >> moduleHits.ContainsKey)
          |> Seq.iter (fun x ->
              let pt = snd x
              let counter = fst x
              // Treat -ve visit counts (an exemption added in analysis) as zero
              let vc = Int32.TryParse(pt.Attribute(XName.Get("visitcount")).Value,
                                      System.Globalization.NumberStyles.Integer,
                                      System.Globalization.CultureInfo.InvariantCulture)
              let visits = max 0 (if fst vc then snd vc else 0)

              pt.SetAttributeValue(XName.Get("visitcount"), visits + moduleHits.[counter]))))

      // Save modified xml to a file
      coverageFile.Seek(0L, SeekOrigin.Begin) |> ignore
      WriteXDocument coverageDocument coverageFile
    finally
        let delta = TimeSpan(DateTime.Now.Ticks - flushStart.Ticks)
        mutex.ReleaseMutex()
        Console.Out.WriteLine("Coverage statistics flushing took {0:N} seconds", delta.TotalSeconds)

  /// <summary>
  /// This method flushes hit count buffers.
  /// </summary>
  let private FlushCounter _ =
    lock Visits (fun () ->
      match Visits.Count with
      | 0 -> ()
      | _ -> let counts = Visits |> Seq.toArray
             Visits.Clear()
             measureTime <- DateTime.Now
             UpdateReport(counts))

  /// <summary>
  /// This method is executed from instrumented assemblies.
  /// </summary>
  /// <param name="moduleId">Assembly being visited</param>
  /// <param name="hitPointId">Sequence Point identifier</param>
  let Visit moduleId hitPointId =
   if not <| String.IsNullOrEmpty(moduleId) then
    lock Visits (fun () ->
      if not (Visits.ContainsKey(moduleId))
        then Visits.[moduleId] <- new Dictionary<int, int>()
      if not (Visits.[moduleId].ContainsKey(hitPointId)) then
        Visits.[moduleId].Add(hitPointId, 1)
      else
        Visits.[moduleId].[hitPointId] <- 1 + Visits.[moduleId].[hitPointId])
    AppDomain.CurrentDomain.DomainUnload.Add(fun x -> Console.Out.WriteLine("unloaded"))
  // Register event handling
  do
    AppDomain.CurrentDomain.DomainUnload.Add(FlushCounter)
    AppDomain.CurrentDomain.ProcessExit.Add(FlushCounter)