// Based upon C# code by Sergiy Sakharov (sakharov@gmail.com)
// http://code.google.com/p/dot-net-coverage/source/browse/trunk/Coverage.Counter/Coverage.Counter.csproj

namespace AltCover.Recorder

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Resources
open System.Runtime.CompilerServices
open System.Threading

#if NETSTANDARD2_0
[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
#else
[<System.Runtime.InteropServices.ProgIdAttribute("ExcludeFromCodeCoverage hack for OpenCover issue 615")>]
#endif
[<NoComparison>]
type internal Carrier = SequencePoint of String * int * Track

module Instance =
  let internal resources =
    ResourceManager("AltCover.Recorder.Strings", Assembly.GetExecutingAssembly())

  let GetResource s =
    [ System.Globalization.CultureInfo.CurrentUICulture.Name;
      System.Globalization.CultureInfo.CurrentUICulture.Parent.Name; "en" ]
    |> Seq.map (fun l -> resources.GetString(s + "." + l))
    |> Seq.tryFind (String.IsNullOrEmpty >> not)

  /// <summary>
  /// Gets the location of coverage xml file
  /// This property's IL code is modified to store actual file location
  /// </summary>
  [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
  let ReportFile = "Coverage.Default.xml"

  /// <summary>
  /// Gets whether to defer output until process exit
  /// This property's IL code is modified to store actual file location
  /// </summary>
  [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
  let Defer = false

  let mutable Supervision =
    AppDomain.CurrentDomain.GetAssemblies()
    |> Seq.map (fun a -> a.GetName())
    |> Seq.exists (fun n -> n.Name = "AltCover.DataCollector" &&
                            n.FullName.EndsWith("PublicKeyToken=c02b1a9f5b7cade8",
                                                StringComparison.Ordinal))

  /// <summary>
  /// Accumulation of visit records
  /// </summary>
  let internal Visits = [|
                            new Dictionary<string, Dictionary<int, PointVisit>>()
                            new Dictionary<string, Dictionary<int, PointVisit>>()
  |]
  let internal FileVisits = Visits.[ReportIndex.File |> int]
  let mutable internal ActiveVisits = Visits.[ReportIndex.Memory |> int]

  let internal Samples = [|
    new Dictionary<string, Dictionary<int, bool>>()
    new Dictionary<string, Dictionary<int, bool>>()
  |]
  let mutable internal ActiveSamples = Samples.[ReportIndex.Memory |> int]

  let mutable internal VisitIndex = ReportIndex.Memory

  let internal synchronize = Object()

  /// <summary>
  /// Gets the unique token for this instance
  /// This property's IL code is modified to store a GUID-based token
  /// </summary>
  [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
  let Token = "AltCover"

  /// <summary>
  /// Gets the style of the associated report
  /// This property's IL code is modified to store the user chosen override if applicable
  /// </summary>
  [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
  let mutable internal CoverageFormat = ReportFormat.NCover

  /// <summary>
  /// Gets the frequency of time sampling
  /// This property's IL code is modified to store the user chosen override if applicable
  /// </summary>
  [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
  let Timer = 0L

  /// <summary>
  /// Gets the sampling strategy
  /// This property's IL code is modified to store the user chosen override if applicable
  /// </summary>
  [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
  let internal Sample = Sampling.All

  /// <summary>
  /// Gets or sets the current test method
  /// </summary>
  type private CallStack =
    [<ThreadStatic; DefaultValue>]
    static val mutable private instance : Option<CallStack>
    val mutable private caller : int list
    private new(x : int) = { caller = [ x ] }

    static member Instance =
      match CallStack.instance with
      | None -> CallStack.instance <- Some(CallStack(0))
      | _ -> ()
      CallStack.instance.Value

    member self.Push x = self.caller <- x :: self.caller

    //let s = sprintf "push %d -> %A" x self.caller
    //System.Diagnostics.Debug.WriteLine(s)
    member self.Pop() =
      self.caller <- match self.caller with
                     | [] | [ 0 ] -> [ 0 ]
                     | _ :: xs -> xs

    //let s = sprintf "pop -> %A"self.caller
    //System.Diagnostics.Debug.WriteLine(s)
    member self.CallerId() = Seq.head self.caller

  (*let x = Seq.head self.caller
                              let s = sprintf "peek %d" x
                              System.Diagnostics.Debug.WriteLine(s)
                              x*)

  let Push x = CallStack.Instance.Push x
  let Pop() = CallStack.Instance.Pop()
  let CallerId() = CallStack.Instance.CallerId()

  /// <summary>
  /// Serialize access to the report file across AppDomains for the classic mode
  /// </summary>
  let internal mutex = new System.Threading.Mutex(false, Token + ".mutex")

  let SignalFile() = ReportFile + ".acv"

  /// <summary>
  /// Reporting back to the mother-ship
  /// </summary>
  let mutable internal trace = Tracer.Create(SignalFile())

  let internal WithMutex(f : bool -> 'a) =
    let own = mutex.WaitOne(1000)
    try
      f (own)
    finally
      if own then mutex.ReleaseMutex()

  let InitialiseTrace() =
    WithMutex(fun _ ->
      let t = Tracer.Create(SignalFile())
      let tt, ii = t.OnStart()
      ActiveVisits <- Visits.[ii |> int]
      ActiveSamples <- Samples.[ii |> int]
      VisitIndex <- ii
      trace <- tt)

  let internal Watcher = new FileSystemWatcher()
  let mutable internal Recording = true

  /// <summary>
  /// This method flushes hit count buffers.
  /// </summary>
  let internal FlushAll finish =
    trace.OnConnected (fun () -> trace.OnFinish finish FileVisits)
      (fun () ->
      match Visits.[ReportIndex.Memory |> int].Count with
      | 0 -> ()
      | _ ->
        WithMutex
          (fun own ->
          let delta =
            Counter.DoFlush ignore (fun _ _ -> ()) own Visits.[ReportIndex.Memory |> int] CoverageFormat ReportFile
              None
          GetResource "Coverage statistics flushing took {0:N} seconds"
          |> Option.iter (fun s -> Console.Out.WriteLine(s, delta.TotalSeconds))))

  let FlushPause() =
    ("PauseHandler")
    |> GetResource
    |> Option.iter Console.Out.WriteLine
    Recording <- false

  let FlushResume() =
    ("ResumeHandler")
    |> GetResource
    |> Option.iter Console.Out.WriteLine
    InitialiseTrace()
    ActiveVisits <- FileVisits
    ActiveSamples <- Samples.[ReportIndex.File |> int]
    VisitIndex <- ReportIndex.File // belt and braces
    Recording <- true

  let FlushFinish () =
    FlushAll ProcessExit

  let internal TraceVisit moduleId hitPointId context =
    lock FileVisits (fun () ->
    trace.OnVisit FileVisits moduleId hitPointId context)

  let internal AddVisit moduleId hitPointId context =
    Counter.AddSingleVisit ActiveVisits moduleId hitPointId context

  let internal TakeSample strategy moduleId hitPointId =
    match strategy with
    | Sampling.All -> true
    | _ ->
      let i = VisitIndex |> int
      let samples = Samples.[i]
      let mutable hasModuleKey = samples.ContainsKey(moduleId)
      if hasModuleKey |> not then
        lock Samples (fun () ->
          hasModuleKey <- samples.ContainsKey(moduleId)
          if hasModuleKey |> not
          then samples.Add(moduleId, Dictionary<int, bool>())
        )

      let next = samples.[moduleId]
      let mutable hasPointKey = next.ContainsKey(hitPointId)
      if hasPointKey |> not then
          lock next (fun () ->
            hasPointKey <- next.ContainsKey(hitPointId)
            if hasPointKey |> not
            then next.Add(hitPointId, true))
      (hasPointKey && hasModuleKey) |> not

  /// <summary>
  /// This method is executed from instrumented assemblies.
  /// </summary>
  /// <param name="moduleId">Assembly being visited</param>
  /// <param name="hitPointId">Sequence Point identifier</param>
  let internal VisitImpl moduleId hitPointId context =
    if (Sample = Sampling.All || TakeSample Sample moduleId hitPointId) then
      let adder =
        if trace.IsConnected() && ((Defer || Supervision) |> not) then TraceVisit
        else AddVisit
      adder moduleId hitPointId context

  let private IsOpenCoverRunner() =
    (CoverageFormat = ReportFormat.OpenCoverWithTracking)
    && (VisitIndex = ReportIndex.File)
  let internal Granularity() = Timer
  let internal Clock() = DateTime.UtcNow.Ticks

  let internal PayloadSelection clock frequency wantPayload =
    if wantPayload() then
      match (frequency(), CallerId()) with
      | (0L, 0) -> Null
      | (t, 0) -> Time(t * (clock() / t))
      | (0L, n) -> Call n
      | (t, n) -> Both(t * (clock() / t), n)
    else Null

  let internal PayloadControl = PayloadSelection Clock
  let internal PayloadSelector enable = PayloadControl Granularity enable

  let internal VisitSelection track moduleId hitPointId =
    VisitImpl moduleId hitPointId track

  let Visit moduleId hitPointId =
    if Recording then
      VisitSelection (if CoverageFormat = ReportFormat.OpenCoverWithTracking
                      then PayloadSelector IsOpenCoverRunner
                      else Null) moduleId hitPointId

  let internal FlushCounter (finish : Close) _ =
      match finish with
      | Resume -> FlushResume()
      | Pause -> FlushPause()
      | _ ->
        Recording <- false
        if Supervision |> not then FlushAll finish

  // Register event handling
  let DoPause = FlushCounter Pause
  let DoResume = FlushCounter Resume

  let internal StartWatcher() =
    Watcher.Path <- Path.GetDirectoryName <| SignalFile()
    Watcher.Filter <- Path.GetFileName <| SignalFile()
    Watcher.Created.Add DoResume
    Watcher.Deleted.Add DoPause
    Watcher.EnableRaisingEvents <- Watcher.Path
                                   |> String.IsNullOrEmpty
                                   |> not

  do AppDomain.CurrentDomain.DomainUnload.Add(FlushCounter DomainUnload)
     AppDomain.CurrentDomain.ProcessExit.Add(FlushCounter ProcessExit)
     StartWatcher()
     InitialiseTrace()