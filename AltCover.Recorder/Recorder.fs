// Based upon C# code by Sergiy Sakharov (sakharov@gmail.com)
// http://code.google.com/p/dot-net-coverage/source/browse/trunk/Coverage.Counter/Coverage.Counter.csproj

namespace AltCover.Recorder

open System
open System.Collections.Generic
open System.IO
open System.Xml
open System.Xml.Linq

type Tracer =
  | Active
  | Inactive

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
  let private ReportFile = "C:\Windows\Temp\Coverage.Default.xml"
  
  /// <summary>
  /// Accumulation of visit records
  /// </summary>  
  let private Visits = new Dictionary<int, Dictionary<int, int>>();
  
  /// <summary>
  /// Interlock for report instances
  /// </summary>
  let private mutex = new System.Threading.Mutex(false, "AltCover.Recorder.Instance.mutex");
  
  /// <summary>
  /// Save sequence point hit counts to xml report file
  /// </summary>
  /// <param name="hitCounts">The coverage results to incorporate</param>
  let private UpdateReport counts =
    mutex.WaitOne(10000) |> ignore
    let flushStart = DateTime.Now;
    try
      use coverageFile = new FileStream(ReportFile, FileMode.Open, FileAccess.ReadWrite, FileShare.None, 4096, FileOptions.SequentialScan)
      // Edit xml report to store new hits
      let coverageDocument = XDocument.Load(new XmlTextReader(coverageFile))

      let startTimeAttr = coverageDocument.Root.Attribute(XName.Get("startTime"))
      let measureTimeAttr = coverageDocument.Root.Attribute(XName.Get("measureTime"))
      let oldStartTime = DateTime.ParseExact(startTimeAttr.Value, "o", null)
      let oldMeasureTime = DateTime.ParseExact(measureTimeAttr.Value, "o", null)
      
      startTime <- if startTime < oldStartTime then startTime else oldStartTime // Min
      measureTime <- if measureTime > oldMeasureTime then measureTime else oldMeasureTime // Max

      startTimeAttr.SetValue(startTime.ToString("o", System.Globalization.CultureInfo.InvariantCulture));
      measureTimeAttr.SetValue(measureTime.ToString("o", System.Globalization.CultureInfo.InvariantCulture));

      counts
      |> Seq.iter (fun (pair : KeyValuePair<int, Dictionary<int,int>>) ->
          let moduleId = pair.Key;
          let moduleHits = pair.Value;
          let affectedModule = 
              coverageDocument.Descendants(XName.Get("module"))
              |> Seq.filter (fun el -> el.Attribute(XName.Get("moduleId")).Value.Equals(moduleId.ToString(System.Globalization.CultureInfo.InvariantCulture)))
              |> Seq.head

          affectedModule.Descendants(XName.Get("seqpnt"))
          |> Seq.mapi (fun counter pt -> (counter, pt))
          |> Seq.filter (fun x -> moduleHits.ContainsKey(fst x))
          |> Seq.iter (fun x ->
              let pt = snd x
              let counter = fst x
              let visits = Int32.Parse(pt.Attribute(XName.Get("visitcount")).Value,
                                     System.Globalization.CultureInfo.InvariantCulture);
              pt.SetAttributeValue(XName.Get("visitcount"), visits + moduleHits.[counter])))
                    
      // Save modified xml to a file
      coverageFile.Seek(0L, SeekOrigin.Begin) |> ignore
      let writer = XmlWriter.Create(coverageFile)
      coverageDocument.WriteTo(writer)
      writer.Flush()
    finally
        let delta = new TimeSpan(DateTime.Now.Ticks - flushStart.Ticks)
        mutex.ReleaseMutex()
        Console.WriteLine("Coverage statistics flushing took {0:N} seconds", delta.TotalSeconds)
    

  /// <summary>
  /// This method flushes hit count buffers.
  /// </summary>
  let private FlushCounter _ =
    lock Visits (fun () ->
      let counts = Visits |> Seq.toArray
      Visits.Clear()
      measureTime <- DateTime.Now
      match counts.Length with
      | 0 -> ()
      | _ -> UpdateReport(counts)
    )
    
    
  /// <summary>
  /// This method is executed from instrumented assemblies.
  /// </summary>
  /// <param name="moduleId">Assembly being visited</param>
  /// <param name="hitPointId">Sequence Point identifier</param>
  let Visit moduleId hitPointId =
    lock Visits (fun () ->
      if not (Visits.ContainsKey(moduleId))
        then Visits.[moduleId] <- new Dictionary<int, int>()
      if not (Visits.[moduleId].ContainsKey(hitPointId)) then
        Visits.[moduleId].Add(hitPointId, 1)
      else
        Visits.[moduleId].[hitPointId] <- 1 + Visits.[moduleId].[hitPointId])
   
  // Register event handling 
  do 
    AppDomain.CurrentDomain.DomainUnload.Add(FlushCounter)
    AppDomain.CurrentDomain.ProcessExit.Add(FlushCounter)
