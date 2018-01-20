// Based upon C# code by Sergiy Sakharov (sakharov@gmail.com)
// http://code.google.com/p/dot-net-coverage/source/browse/trunk/Coverage.Counter/Coverage.Counter.csproj

namespace AltCover.Recorder

open System
open System.Collections.Generic
open System.IO
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Xml

#if NET2
module private NativeMethods =
   [<System.Runtime.InteropServices.DllImport("kernel32.dll",
                                               EntryPoint = "CreateFile", 
                                               SetLastError = true,
                                               CharSet = CharSet.Unicode)>]
    extern Microsoft.Win32.SafeHandles.SafeFileHandle CreateFileW(String lpFileName,
        UInt32 dwDesiredAccess, UInt32 dwShareMode,
        IntPtr lpSecurityAttributes, UInt32 dwCreationDisposition,
        UInt32 dwFlagsAndAttributes,
        IntPtr hTemplateFile)
#endif

[<ProgId("ExcludeFromCodeCoverage")>] // HACK HACK HACK
type Tracer = {
                Tracer : string
                Pipe : Stream
              }
  with
#if NETSTANDARD2_0
    static member Core () =
             typeof<Microsoft.FSharp.Core.CompilationMappingAttribute>.Assembly.Location
#endif
    static member CreatePipe (name:string) =
      printfn "Creating pipe %s" name
#if NET2
      // conceal path name from Gendarme
      let pipeHeader = String.Join(@"\", [|String.Empty; String.Empty; "."; "pipe"; String.Empty|])
      let handle = NativeMethods.CreateFileW(pipeHeader + name,
                                             0x40000000u, // GENERIC_WRITE,
                                             0u,
                                             IntPtr.Zero,
                                             3u, // OPEN_EXISTING,
                                             0u,
                                             IntPtr.Zero)

      // Test against INVALID_HANDLE_VALUE
      if handle.IsInvalid then // System.ComponentModel.Win32Exception(Marshal.GetLastWin32Error()) |> raise 
        {Tracer = name; Pipe = new MemoryStream() :> Stream}
      else
        {Tracer = name; Pipe = new FileStream(handle, FileAccess.Write) :> Stream}
#else
      {Tracer = name; Pipe = new System.IO.Pipes.NamedPipeClientStream(name) :> Stream}
#endif
    member this.IsConnected ()=
#if NET2
      match this.Pipe with
      | :? FileStream -> this.Pipe.CanWrite
      | _ -> false
#else
      (this.Pipe :?> System.IO.Pipes.NamedPipeClientStream).IsConnected
#endif

    member this.Connect ms =
#if NET2
      ignore ms
#else
      (this.Pipe :?> System.IO.Pipes.NamedPipeClientStream).Connect(ms)
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

  /// <summary>
  /// Reporting back to the mother-ship; only on the .net core build
  /// because this API isn't available in .net 2.0 (framework back-version support)
  /// </summary>
  let mutable internal pipe = Tracer.CreatePipe Token

  let private formatter = System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()

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

  let private push (moduleId:string) hitPointId =
     formatter.Serialize(pipe.Pipe, (moduleId, hitPointId))

  let internal OnConnected f g =
     if pipe.IsConnected() then f() 
     else WithVisitsLocked g

  /// <summary>
  /// This method flushes hit count buffers.
  /// </summary>
  let internal FlushCounter finish _ =
    OnConnected (fun () -> 
      printfn "pushing flush %A" finish
      if finish then
        push null -1
        use local = pipe.Pipe
        local.Flush())
      (fun () ->
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
      OnConnected (fun () -> 
        printfn "pushing Visit"
        push moduleId hitPointId
                 )
                 (fun () -> if not (Visits.ContainsKey moduleId)
                                      then Visits.[moduleId] <- new Dictionary<int, int>()
                            if not (Visits.[moduleId].ContainsKey hitPointId) then
                               Visits.[moduleId].Add(hitPointId, 1)
                            else
                               Visits.[moduleId].[hitPointId] <- 1 + Visits.[moduleId].[hitPointId])

  // Register event handling
  do
    AppDomain.CurrentDomain.DomainUnload.Add(FlushCounter false)
    AppDomain.CurrentDomain.ProcessExit.Add(FlushCounter true)
    try
      printfn "Connecting pipe %s ..." pipe.Tracer
      pipe.Connect 2000 // 2 seconds
      printfn "Connected."
      push null -1
    with
    | :? TimeoutException ->
        printfn "timed out"
        ()