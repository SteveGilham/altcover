// Based upon C# code by Sergiy Sakharov (sakharov@gmail.com)
// http://code.google.com/p/dot-net-coverage/source/browse/trunk/Coverage.Counter/Coverage.Counter.csproj

namespace AltCover.Recorder

open System
open System.Collections.Generic
open System.Diagnostics
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Reflection

open System.Resources
open System.Runtime.CompilerServices

module Instance =
  // Public "fields"

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

  /// <summary>
  /// Gets the style of the associated report
  /// This property's IL code is modified to store the user chosen override if applicable
  /// </summary>
  [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
  [<System.Diagnostics.CodeAnalysis.SuppressMessage(
      "Gendarme.Rules.Performance", "AvoidUncalledPrivateCodeRule",
      Justification = "Unit test accessor")>]
  [<System.Diagnostics.CodeAnalysis.SuppressMessage(
      "Gendarme.Rules.Naming", "UseCorrectCasingRule",
      Justification = "Code rewritten")>]
  let
#if DEBUG
      mutable
#endif
              internal CoverageFormat = ReportFormat.NCover // fsharplint:disable-line NonPublicValuesNames

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
  let internal Sample = Sampling.All // fsharplint:disable-line NonPublicValuesNames

  /// <summary>
  /// Gets the unique token for this instance
  /// This property's IL code is modified to store a GUID-based token
  /// </summary>
  [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
  let Token = "AltCover"

  [<System.Diagnostics.CodeAnalysis.SuppressMessage(
      "Gendarme.Rules.Performance", "AvoidUncalledPrivateCodeRule",
      Justification = "Access by reflection in the data collector")>]
  let mutable internal supervision =
    AppDomain.CurrentDomain.GetAssemblies()
    |> Seq.map (fun a -> a.GetName())
    |> Seq.exists (fun n ->
          n.Name = "AltCover.DataCollector"
          && n.FullName.EndsWith
              ("PublicKeyToken=c02b1a9f5b7cade8", StringComparison.Ordinal))

  // Implementation details
#if DEBUG
  module internal I =
#else
  module private I =
#endif

    let internal resources =
      ResourceManager("AltCover.Recorder.Strings", Assembly.GetExecutingAssembly())

    let internal getResource s =
      let cc = System.Globalization.CultureInfo.CurrentUICulture
      [ cc.Name
        cc.Parent.Name
        "en" ]
      |> Seq.map (fun l -> resources.GetString(s + "." + l))
      |> Seq.tryFind (String.IsNullOrEmpty >> not)

    /// <summary>
    /// Accumulation of visit records
    /// </summary>
    let mutable internal visits = new Dictionary<string, Dictionary<int, PointVisit>>()

    let mutable internal samples = new Dictionary<string, Dictionary<int, bool>>()
    let mutable internal isRunner = false

    let internal synchronize = Object()

    /// <summary>
    /// Gets or sets the current test method
    /// </summary>
    [<SuppressMessage("Gendarme.Rules.Naming",
      "UseCorrectSuffixRule", Justification="It's the program call stack");
      Sealed; AutoSerializable(false)>]
    type private CallStack =
      [<ThreadStatic; DefaultValue>]
      static val mutable private instance : Option<CallStack>
      val mutable private caller : int list
      private new(x : int) = { caller = [ x ] }

      [<System.Diagnostics.CodeAnalysis.SuppressMessage(
          "Gendarme.Rules.Performance", "AvoidUncalledPrivateCodeRule",
          Justification = "TODO -- fix this Gendarme bug")>]
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

    let internal callerId() = CallStack.Instance.CallerId()
    let internal push x = CallStack.Instance.Push x
    let internal pop() = CallStack.Instance.Pop()

    /// <summary>
    /// Serialize access to the report file across AppDomains for the classic mode
    /// </summary>
    let internal mutex = new System.Threading.Mutex(false, Token + ".mutex")

    let internal signalFile() = ReportFile + ".acv"

    /// <summary>
    /// Reporting back to the mother-ship
    /// </summary>
    let mutable internal trace = Tracer.Create(signalFile())

    let internal withMutex(f : bool -> 'a) =
      let own = mutex.WaitOne(1000)
      try
        f (own)
      finally
        if own then mutex.ReleaseMutex()

    let internal initialiseTrace(t : Tracer) =
      withMutex(fun _ ->
        trace <- t.OnStart()
        isRunner <- isRunner || trace.IsConnected)

    let internal watcher = new FileSystemWatcher()
    let mutable internal recording = true

    let internal clear() = visits <- Dictionary<string, Dictionary<int, PointVisit>>()

    /// <summary>
    /// This method flushes hit count buffers.
    /// </summary>
    let internal flushAll _ =
      let counts = visits
      clear()
      trace.OnConnected (fun () -> trace.OnFinish counts) (fun () ->
        match counts.Count with
        | 0 -> ()
        | _ ->
            withMutex(fun own ->
              let delta =
                Counter.doFlush ignore (fun _ _ -> ()) own counts CoverageFormat ReportFile
                  None
              getResource "Coverage statistics flushing took {0:N} seconds"
              |> Option.iter (fun s -> Console.Out.WriteLine(s, delta.TotalSeconds))))

    let internal flushPause() =
      ("PauseHandler")
      |> getResource
      |> Option.iter Console.Out.WriteLine
      recording <- false
      flushAll Pause
      trace <- signalFile() |> Tracer.Create

    let internal flushResume() =
      ("ResumeHandler")
      |> getResource
      |> Option.iter Console.Out.WriteLine
      let wasConnected = isRunner
      initialiseTrace trace
      if (wasConnected <> isRunner) then
        samples <- Dictionary<string, Dictionary<int, bool>>()
        clear()
      recording <- true

    let internal traceVisit moduleId hitPointId context =
      lock synchronize (fun () ->
        let counts = visits
        if counts.Count > 0 then clear()
        trace.OnVisit counts moduleId hitPointId context)

    [<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage",
                                                      "CA2202:DisposeObjectsBeforeLosingScope",
                                                      Justification =
                                                        "Damned if you do, damned if you don't Dispose()")>]
    let internal logException moduleId hitPointId context x =
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

    [<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Smells",
     "AvoidLongParameterListsRule",
      Justification="Self-contained internal decorator")>]
    let
#if DEBUG
#else
        inline
#endif
               internal issue71Wrapper visits moduleId hitPointId context handler add =
      try
        add visits moduleId hitPointId context
      with x ->
        match x with
        | :? KeyNotFoundException
        | :? NullReferenceException
        | :? ArgumentNullException -> handler moduleId hitPointId context x
        | _ -> reraise()

    let internal addVisit moduleId hitPointId context =
      issue71Wrapper visits moduleId hitPointId context logException Counter.addSingleVisit

    let internal takeSample strategy moduleId hitPointId =
      match strategy with
      | Sampling.All -> true
      | _ ->
          let mutable hasModuleKey = samples.ContainsKey(moduleId)
          if hasModuleKey |> not then
            lock samples (fun () ->
              hasModuleKey <- samples.ContainsKey(moduleId)
              if hasModuleKey |> not then samples.Add(moduleId, Dictionary<int, bool>()))

          let next = samples.[moduleId]
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
    let internal visitImpl moduleId hitPointId context =
      if (Sample = Sampling.All || takeSample Sample moduleId hitPointId) then
        let adder =
          if Defer || supervision || (trace.IsConnected |> not)
          then addVisit
          else traceVisit
        adder moduleId hitPointId context

    let internal isOpenCoverRunner() =
      (CoverageFormat = ReportFormat.OpenCoverWithTracking) && isRunner
    let internal granularity() = Timer
    let internal clock() = DateTime.UtcNow.Ticks

    let internal payloadSelection clock frequency wantPayload =
      if wantPayload() then
        match (frequency(), callerId()) with
        | (0L, 0) -> Null
        | (t, 0) -> Time(t * (clock() / t))
        | (0L, n) -> Call n
        | (t, n) ->
            Both
              { Time = t * (clock() / t)
                Call = n }
      else
        Null

    let internal payloadControl = payloadSelection clock
    let internal payloadSelector enable = payloadControl granularity enable

    let internal visitSelection track moduleId hitPointId =
      visitImpl moduleId hitPointId track

    let internal flushCounter (finish : Close) _ =
      match finish with
      | Resume -> flushResume()
      | Pause -> flushPause()
      | _ ->
          recording <- false
          if supervision |> not then flushAll finish

    // Register event handling
    let internal doPause = FileSystemEventHandler (fun _ a -> flushCounter Pause a)
    let internal doResume = FileSystemEventHandler (fun _ a -> flushCounter Resume a)
    let internal doUnload = EventHandler (fun _ a -> flushCounter DomainUnload a)
    let internal doExit = EventHandler (fun _ a -> flushCounter ProcessExit a)

    let internal startWatcher() =
      watcher.Path <- Path.GetDirectoryName <| signalFile()
      watcher.Filter <- Path.GetFileName <| signalFile()
      watcher.add_Created doResume
      watcher.add_Deleted doPause
      watcher.EnableRaisingEvents <-
        watcher.Path
        |> String.IsNullOrEmpty
        |> not

    do AppDomain.CurrentDomain.add_DomainUnload doUnload
       AppDomain.CurrentDomain.add_ProcessExit doExit
       startWatcher()
       signalFile()
       |> Tracer.Create
       |> initialiseTrace

  // Public API
  let Visit moduleId hitPointId =
    if I.recording then
      I.visitSelection
        (if CoverageFormat = ReportFormat.OpenCoverWithTracking
          then I.payloadSelector I.isOpenCoverRunner
          else Null) moduleId hitPointId

  let Push caller = I.push caller
  let Pop() = I.pop()
  // Used by the datacollector
  let FlushFinish() = I.flushAll ProcessExit

#if SHADOW
[<assembly: SuppressMessage("Microsoft.Performance",
  "CA1810:InitializeReferenceTypeStaticFieldsInline", Scope="member",
  Target="<StartupCode$AltCover-Shadow>.$Base.#.cctor()",
  Justification="Compiler generated")>]
[<assembly: SuppressMessage("Microsoft.Performance",
  "CA1810:InitializeReferenceTypeStaticFieldsInline", Scope="member",
  Target="<StartupCode$AltCover-Shadow>.$Recorder.#.cctor()",
  Justification="Compiler generated")>]
()
#endif