// Based upon C# code by Sergiy Sakharov (sakharov@gmail.com)
// http://code.google.com/p/dot-net-coverage/source/browse/trunk/Coverage.Counter/Coverage.Counter.csproj

namespace AltCover.Recorder

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Reflection

open System.Resources
open System.Runtime.CompilerServices

module Instance =
  let internal resources =
    ResourceManager("AltCover.Recorder.Strings", Assembly.GetExecutingAssembly())

  let GetResource s =
    [ System.Globalization.CultureInfo.CurrentUICulture.Name
      System.Globalization.CultureInfo.CurrentUICulture.Parent.Name
      "en" ]
    |> Seq.map (fun l -> resources.GetString(s + "." + l))
    |> Seq.tryFind (String.IsNullOrEmpty >> not)

  /// <summary>
  /// Gets the location of coverage xml file
  /// This property's IL code is modified to store the actual file location
  /// </summary>
  [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
  let ReportFile = "Coverage.Default.xml"

  /// <summary>
  /// Gets whether to defer output until process exit
  /// This property's IL code is modified to store the actual value
  /// </summary>
  [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
  let Defer = false

  let mutable Supervision =
    AppDomain.CurrentDomain.GetAssemblies()
    |> Seq.map (fun a -> a.GetName())
    |> Seq.exists (fun n ->
         n.Name = "AltCover.DataCollector"
         && n.FullName.EndsWith
              ("PublicKeyToken=c02b1a9f5b7cade8", StringComparison.Ordinal))

  /// <summary>
  /// Accumulation of visit records
  /// </summary>
  let mutable internal Visits = new Dictionary<string, Dictionary<int, PointVisit>>()

  let mutable internal Samples = new Dictionary<string, Dictionary<int, bool>>()
  let mutable internal IsRunner = false

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
      self.caller <-
        match self.caller with
        | []
        | [ 0 ] -> [ 0 ]
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

  let InitialiseTrace(t : Tracer) =
    WithMutex(fun _ ->
      trace <- t.OnStart()
      IsRunner <- IsRunner || trace.IsConnected())

  let internal Watcher = new FileSystemWatcher()
  let mutable internal Recording = true

  let Clear() = Visits <- Dictionary<string, Dictionary<int, PointVisit>>()

  /// <summary>
  /// This method flushes hit count buffers.
  /// </summary>
  let internal FlushAll _ =
    let counts = Visits
    Clear()
    trace.OnConnected (fun () -> trace.OnFinish counts) (fun () ->
      match counts.Count with
      | 0 -> ()
      | _ ->
          WithMutex(fun own ->
            let delta =
              Counter.DoFlush ignore (fun _ _ -> ()) own counts CoverageFormat ReportFile
                None
            GetResource "Coverage statistics flushing took {0:N} seconds"
            |> Option.iter (fun s -> Console.Out.WriteLine(s, delta.TotalSeconds))))

  let FlushPause() =
    ("PauseHandler")
    |> GetResource
    |> Option.iter Console.Out.WriteLine
    Recording <- false
    FlushAll Pause
    trace <- SignalFile() |> Tracer.Create

  let FlushResume() =
    ("ResumeHandler")
    |> GetResource
    |> Option.iter Console.Out.WriteLine
    let wasConnected = IsRunner
    InitialiseTrace trace
    if (wasConnected <> IsRunner) then
      Samples <- Dictionary<string, Dictionary<int, bool>>()
      Clear()
    Recording <- true

  let FlushFinish() = FlushAll ProcessExit

  let internal TraceVisit moduleId hitPointId context =
    lock synchronize (fun () ->
      let counts = Visits
      if counts.Count > 0 then Clear()
      trace.OnVisit counts moduleId hitPointId context)

  [<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage",
                                                    "CA2202:DisposeObjectsBeforeLosingScope",
                                                    Justification =
                                                      "Damned if you do, damned if you don't Dispose()")>]
  let LogException moduleId hitPointId context x =
    let text =
      [| sprintf "ModuleId = %A" moduleId
         sprintf "hitPointId = %A" hitPointId
         sprintf "context = %A" context
         sprintf "exception = %s" (x.ToString())
         StackTrace().ToString() |]

    let stamp = sprintf "%A" DateTime.UtcNow.Ticks
    let filename = ReportFile + "." + stamp + ".exn"
    use file = File.Open(filename, FileMode.OpenOrCreate, FileAccess.Write)
    use writer = new StreamWriter(file)
    text |> Seq.iter (fun line -> writer.WriteLine("{0}", line))

  let
#if DEBUG
#else
      inline
#endif
             internal Issue71Wrapper visits moduleId hitPointId context handler add =
    try
      add visits moduleId hitPointId context
    with x ->
      match x with
      | :? KeyNotFoundException
      | :? NullReferenceException
      | :? ArgumentNullException -> handler moduleId hitPointId context x
      | _ -> reraise()

  let internal AddVisit moduleId hitPointId context =
    Issue71Wrapper Visits moduleId hitPointId context LogException Counter.AddSingleVisit

  let internal TakeSample strategy moduleId hitPointId =
    match strategy with
    | Sampling.All -> true
    | _ ->
        let mutable hasModuleKey = Samples.ContainsKey(moduleId)
        if hasModuleKey |> not then
          lock Samples (fun () ->
            hasModuleKey <- Samples.ContainsKey(moduleId)
            if hasModuleKey |> not then Samples.Add(moduleId, Dictionary<int, bool>()))

        let next = Samples.[moduleId]
        let mutable hasPointKey = next.ContainsKey(hitPointId)
        if hasPointKey |> not then
          lock next (fun () ->
            hasPointKey <- next.ContainsKey(hitPointId)
            if hasPointKey |> not then next.Add(hitPointId, true))
        (hasPointKey && hasModuleKey) |> not

  /// <summary>
  /// This method is executed from instrumented assemblies.
  /// </summary>
  /// <param name="moduleId">Assembly being visited</param>
  /// <param name="hitPointId">Sequence Point identifier</param>
  let internal VisitImpl moduleId hitPointId context =
    if (Sample = Sampling.All || TakeSample Sample moduleId hitPointId) then
      let adder =
        if Defer || Supervision || (trace.IsConnected() |> not)
        then AddVisit
        else TraceVisit
      adder moduleId hitPointId context

  let private IsOpenCoverRunner() =
    (CoverageFormat = ReportFormat.OpenCoverWithTracking) && IsRunner
  let internal Granularity() = Timer
  let internal Clock() = DateTime.UtcNow.Ticks

  let internal PayloadSelection clock frequency wantPayload =
    if wantPayload() then
      match (frequency(), CallerId()) with
      | (0L, 0) -> Null
      | (t, 0) -> Time(t * (clock() / t))
      | (0L, n) -> Call n
      | (t, n) ->
          Both
            { Time = t * (clock() / t)
              Call = n }
    else
      Null

  let internal PayloadControl = PayloadSelection Clock
  let internal PayloadSelector enable = PayloadControl Granularity enable

  let internal VisitSelection track moduleId hitPointId =
    VisitImpl moduleId hitPointId track

  let Visit moduleId hitPointId =
    if Recording then
      VisitSelection
        (if CoverageFormat = ReportFormat.OpenCoverWithTracking
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
  let DoPause = FileSystemEventHandler (fun _ a -> FlushCounter Pause a)
  let DoResume = FileSystemEventHandler (fun _ a -> FlushCounter Resume a)
  let DoUnload = EventHandler (fun _ a -> FlushCounter DomainUnload a)
  let DoExit = EventHandler (fun _ a -> FlushCounter ProcessExit a)

  let internal StartWatcher() =
    Watcher.Path <- Path.GetDirectoryName <| SignalFile()
    Watcher.Filter <- Path.GetFileName <| SignalFile()
    Watcher.add_Created DoResume
    Watcher.add_Deleted DoPause
    Watcher.EnableRaisingEvents <-
      Watcher.Path
      |> String.IsNullOrEmpty
      |> not

  do AppDomain.CurrentDomain.add_DomainUnload DoUnload
     AppDomain.CurrentDomain.add_ProcessExit DoExit
     StartWatcher()
     SignalFile()
     |> Tracer.Create
     |> InitialiseTrace