// Based upon C# code by Sergiy Sakharov (sakharov@gmail.com)
// http://code.google.com/p/dot-net-coverage/source/browse/trunk/Coverage.Counter/Coverage.Counter.csproj

namespace AltCover.Recorder

open System
open System.Collections.Generic
open System.IO
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Xml

#if NETSTANDARD2_0
module Communications =
  let internal ResilientAgainstDisposedObject (f: unit -> unit) (tidy : unit -> unit)=
    try
      f()
    with
    | :? ObjectDisposedException -> tidy()

  let SignalOnReceive (s:Stream) (h:System.Threading.EventWaitHandle) =
    let b = s.ReadByte()
    if (b >= 0) then
        h.Set() |> ignore

#endif

[<ProgId("ExcludeFromCodeCoverage")>] // HACK HACK HACK
type Tracer = {
                Tracer : string
#if NETSTANDARD2_0
                Pipe : System.IO.Pipes.NamedPipeClientStream
                Activated : System.Threading.ManualResetEvent
              }
  with
    static member Core () =
             typeof<Microsoft.FSharp.Core.CompilationMappingAttribute>.Assembly.Location

    static member CreatePipe (name:string) =
      printfn "**Creating NamedPipeClientStream %s" name
      {Tracer = name;
       Pipe = new System.IO.Pipes.NamedPipeClientStream(name);
       Activated = new System.Threading.ManualResetEvent false }

    member this.IsConnected ()=
      this.Pipe.IsConnected &&
        this.Pipe.CanWrite

    member this.IsActivated ()=
      this.IsConnected() &&
        this.Activated.WaitOne(0)

    member this.Connect ms =
      try
        this.Pipe.Connect(ms)
        async {
          Communications.ResilientAgainstDisposedObject(fun () ->
            Communications.SignalOnReceive this.Pipe this.Activated) ignore
        } |> Async.Start
      with
      | :? TimeoutException ->
        reraise ()

    member this.Close() =
      this.Pipe.Dispose()
      this.Activated.Dispose()
#else
              }
#endif

// Abstract out compact bits of F# that expand into
// enough under-the-covers code to make Gendarme spot duplication
// with a generic try/finally block.  *sigh*

module Locking =
  /// <summary>
  /// Synchronize an action on an object
  /// </summary>
  /// <param name="f">The action to perform</param>
  let internal WithLockerLocked (locker:'a) (f: unit -> unit) =
    lock locker f

module Instance =
  open System.Globalization

   /// <summary>
   /// The time at which coverage run began
   /// </summary>
  let mutable internal startTime = DateTime.UtcNow

  /// <summary>
  /// Thime taken to perform coverage run
  /// </summary>
  let mutable internal measureTime = DateTime.UtcNow

  /// <summary>
  /// Gets the location of coverage xml file
  /// This property's IL code is modified to store actual file location
  /// </summary>
  [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
  let ReportFile = "Coverage.Default.xml"

  /// <summary>
  /// Accumulation of visit records
  /// </summary>
  let internal Visits = new Dictionary<string, Dictionary<int, int>>();

  /// <summary>
  /// Gets the unique token for this instance
  /// This property's IL code is modified to store a GUID-based token
  /// </summary>
  [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
  let Token = "AltCover"

  /// <summary>
  /// Interlock for report instances
  /// </summary>
  let private mutex = new System.Threading.Mutex(false, Token + ".mutex");

#if NETSTANDARD2_0
  /// <summary>
  /// Reporting back to the mother-ship; only on the .net core build
  /// because this API isn't available in .net 2.0 (framework back-version support)
  /// </summary>
  let mutable internal pipe = Tracer.CreatePipe Token

  let private formatter = System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()
#endif

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
  let internal UpdateReport (counts:Dictionary<string, Dictionary<int, int>>) coverageFile =
    let own = mutex.WaitOne(10000)
    let flushStart = DateTime.UtcNow;
    try
      // Edit xml report to store new hits
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
    finally
        if own then mutex.ReleaseMutex()

  /// <summary>
  /// Synchronize an action on the visits table
  /// </summary>
  let private WithVisitsLocked =
    Locking.WithLockerLocked Visits

#if NETSTANDARD2_0
  let private push (moduleId:string) hitPointId =
    Communications.ResilientAgainstDisposedObject (fun () ->
      formatter.Serialize(pipe.Pipe, (moduleId, hitPointId))
      pipe.Pipe.Flush()) ignore
#endif

#if NETSTANDARD2_0
  let internal OnConnected f g =
     if pipe.IsActivated() then f()
     else WithVisitsLocked g
#endif

  /// <summary>
  /// This method flushes hit count buffers.
  /// </summary>
  let internal FlushCounter finish _ =
#if NETSTANDARD2_0
    OnConnected (fun () ->
      printfn "**pushing flush %A" finish
      if finish then
        push null -1)
#else
    ignore finish
    WithVisitsLocked
#endif
      (fun () ->
#if NETSTANDARD2_0
      if finish then
        pipe.Close()
#endif
      match Visits.Count with
      | 0 -> ()
      | _ -> let counts = Dictionary<string, Dictionary<int, int>> Visits
             Visits.Clear()
             measureTime <- DateTime.UtcNow
             use coverageFile = new FileStream(ReportFile, FileMode.Open, FileAccess.ReadWrite, FileShare.None, 4096, FileOptions.SequentialScan)
             let flushStart = UpdateReport counts coverageFile
             let delta = TimeSpan(DateTime.UtcNow.Ticks - flushStart.Ticks)
             Console.Out.WriteLine("Coverage statistics flushing took {0:N} seconds", delta.TotalSeconds))

  /// <summary>
  /// This method is executed from instrumented assemblies.
  /// </summary>
  /// <param name="moduleId">Assembly being visited</param>
  /// <param name="hitPointId">Sequence Point identifier</param>
  let Visit moduleId hitPointId =
    if not <| String.IsNullOrEmpty(moduleId) then
#if NETSTANDARD2_0
      OnConnected (fun () ->
        printfn "**pushing Visit"
        push moduleId hitPointId
                 )
#else
      WithVisitsLocked
#endif
                 (fun () -> if not (Visits.ContainsKey moduleId)
                                      then Visits.[moduleId] <- new Dictionary<int, int>()
                            if not (Visits.[moduleId].ContainsKey hitPointId) then
                               Visits.[moduleId].Add(hitPointId, 1)
                            else
                               Visits.[moduleId].[hitPointId] <- 1 + Visits.[moduleId].[hitPointId])

#if NETSTANDARD2_0
  let internal Connect (name:string) (p:Tracer) =
    if name <> "AltCover" then
      try
        printfn "**Connecting pipe %s ..." pipe.Tracer
        p.Connect 2000 // 2 seconds
        printfn "**Connected."
      with
      | :? TimeoutException
      | :? IOException ->
          printfn "**timed out"
          ()
#endif

  // Register event handling
  do
    AppDomain.CurrentDomain.DomainUnload.Add(FlushCounter false)
    AppDomain.CurrentDomain.ProcessExit.Add(FlushCounter true)
#if NETSTANDARD2_0
    Connect Token pipe
#endif