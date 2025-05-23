﻿// Based upon C# code by Sergiy Sakharov (sakharov@gmail.com)
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
#if !NET20
open System.Threading
#endif

open AltCover.Shared

module Instance =
  // Public "fields"

  /// <summary>
  /// Gets the location of coverage xml file
  /// This property's IL code is modified to store the actual file location
  /// </summary>
  [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
  let ReportFile = "Coverage.Default.xml"

  let ReportFilePath =
    Path.Combine(
      Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location),
      ReportFile
    )
    |> AltCover.Canonical.canonicalPath

  /// <summary>
  /// Gets whether to defer output until process exit
  /// This property's IL code is modified to store the actual value
  /// </summary>
  [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
  let Eager = false

  /// <summary>
  /// Gets the style of the associated report
  /// This property's IL code is modified to store the user chosen override if applicable
  /// </summary>
  [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
  [<SuppressMessage("Gendarme.Rules.Performance",
                    "AvoidUncalledPrivateCodeRule",
                    Justification = "Unit test accessor")>]
  [<SuppressMessage("Gendarme.Rules.Naming",
                    "UseCorrectCasingRule",
                    Justification = "Code rewritten")>]
  let
#if DEBUG
      mutable
#endif
      internal CoverageFormat = // fsharplint:disable-line NonPublicValuesNames
    ReportFormat.NCover

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

  /// <summary>
  /// Gets the indexed module tokens
  /// This property's IL code is modified to store instrumentation results
  /// </summary>
  [<SuppressMessage("Gendarme.Rules.Performance",
                    "AvoidUncalledPrivateCodeRule",
                    Justification = "Unit test accessor")>]
  [<SuppressMessage("Gendarme.Rules.Performance",
                    "AvoidReturningArraysOnPropertiesRule",
                    Justification = "Code more easily rewritten thus")>]
  [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
  let mutable internal modules =
    [| String.Empty |]

  [<SuppressMessage("Gendarme.Rules.Performance",
                    "AvoidUncalledPrivateCodeRule",
                    Justification = "Access by reflection in the data collector")>]
  let mutable internal supervision =
    //Assembly.GetExecutingAssembly().GetName().Name = "AltCover.Recorder.g" &&
    AppDomain.CurrentDomain.GetAssemblies()
    |> Seq.map (fun a -> a.GetName())
    |> Seq.exists (fun n ->
      n.Name == "AltCover.DataCollector"
      && n.FullName.EndsWith("PublicKeyToken=c02b1a9f5b7cade8", StringComparison.Ordinal))
    && Token <> "AltCover"

  type internal Sampled =
    | Visit of int
    | CallVisit of (int * int)
    | TimeVisit of (int * int64)

  // Implementation details
#if DEBUG
  module internal I =
#else
  module private I =
#endif

    let internal resources =
      ResourceManager("AltCover.Recorder.Strings", Assembly.GetExecutingAssembly())

    let internal getResource s =
      let cc =
        System.Globalization.CultureInfo.CurrentUICulture

      [ cc.Name; cc.Parent.Name; "en" ]
      |> Seq.map (fun l -> resources.GetString(s + "." + l))
      |> Seq.tryFind (String.IsNullOrEmpty >> not)

    let private makeVisits () =
      [ modules
        [| Track.Entry; Track.Exit |] ]
      |> Seq.concat
      |> Seq.fold
        (fun (d: Dictionary<string, Dictionary<int, PointVisit>>) k ->
          d.Add(k, Dictionary<int, PointVisit>())
          d)
        (Dictionary<string, Dictionary<int, PointVisit>>())

    /// <summary>
    /// Accumulation of visit records
    /// </summary>
    let mutable internal visits = makeVisits ()

    let internal makeSamples () =
      modules
      |> Seq.fold
        (fun (d: Dictionary<string, Dictionary<Sampled, bool>>) k ->
          d.Add(k, Dictionary<Sampled, bool>())
          d)
        (Dictionary<string, Dictionary<Sampled, bool>>())

    let mutable internal samples =
      makeSamples ()

    let mutable internal isRunner = false

    let internal synchronize = Object()

#if NET20
    // class needed for "[ThreadStatic] static val mutable"
    [<Sealed>]
    type private AsyncLocal<'a>() =
      [<ThreadStatic; DefaultValue>]
      static val mutable private item: 'a

      [<SuppressMessage("Gendarme.Rules.Correctness",
                        "MethodCanBeMadeStaticRule",
                        Justification = "It's a compatibility hack")>]
      member this.Value
        with get () = AsyncLocal<'a>.item
        and set (value) = AsyncLocal<'a>.item <- value
#endif

    /// <summary>
    /// Gets or sets the current test method
    /// </summary>
    module private CallTrack =
      let value = AsyncLocal<Stack<int>>()

      // no race conditions here
      let instance () =
        match value.Value with
        | null -> value.Value <- Stack<int>()
        | _ -> ()

        value.Value

      let private look op =
        let i = instance ()

        match i.Count with
        | 0 -> None
        | _ -> Some(op i)

      let peek () = look (fun i -> i.Peek())

      let push x = instance().Push x

      let pop () = look (fun i -> i.Pop())

    let internal callerId () = CallTrack.peek ()
    let internal push x = CallTrack.push x
    let internal pop () = CallTrack.pop ()

    /// <summary>
    /// Serialize access to the report file across AppDomains for the classic mode
    /// </summary>
    let internal mutex =
      new System.Threading.Mutex(false, Token + ".mutex")

    let internal signalFile () = ReportFilePath + ".acv"

    /// <summary>
    /// Reporting back to the mother-ship
    /// </summary>
    let mutable internal trace =
      Tracer.Create(signalFile ())

    let internal withMutex (f: bool -> 'a) =
      let own = mutex.WaitOne(1000)

      try
        f (own)
      finally
        if own then
          mutex.ReleaseMutex()

    let internal initialiseTrace (t: Tracer) =
      withMutex (fun _ ->
        trace <- t.OnStart()
        isRunner <- isRunner || trace.IsConnected)

    let internal watcher =
      new FileSystemWatcher()

    let mutable internal recording = true

    let internal clear () =
      visits <- makeVisits ()
      Counter.branchVisits <- 0L
      Counter.totalVisits <- 0L

    /// <summary>
    /// This method flushes hit count buffers.
    /// </summary>
    let internal flushAll _ =
      let counts = visits
      clear ()

      trace.OnConnected (fun () -> trace.OnFinish counts) (fun () ->
        match counts.Values |> Seq.sumBy (fun x -> x.Count) with
        | 0 -> ()
        | _ ->
          withMutex (fun own ->
            let delta =
              Counter.doFlushFile
                ignore
                (fun _ _ -> ())
                own
                counts
                CoverageFormat
                ReportFilePath
                None

            getResource "Coverage statistics flushing took {0:N} seconds"
            |> Option.iter (fun s -> Console.Out.WriteLine(s, delta.TotalSeconds))))

    let internal flushPause () =
      ("PauseHandler")
      |> getResource
      |> Option.iter Console.Out.WriteLine

      recording <- false
      flushAll Pause
      trace <- signalFile () |> Tracer.Create

    let internal flushResume () =
      ("ResumeHandler")
      |> getResource
      |> Option.iter Console.Out.WriteLine

      let wasConnected = isRunner
      initialiseTrace trace

      if (wasConnected <> isRunner) then
        samples <- makeSamples ()
        clear ()

      recording <- true

    let internal traceVisit moduleId hitPointId context =
      lock synchronize (fun () ->
        let counts = visits

        if counts.Values |> Seq.sumBy (fun x -> x.Count) > 0 then
          clear ()

        trace.OnVisit counts moduleId hitPointId context)

    [<SuppressMessage("Microsoft.Usage",
                      "CA2202:DisposeObjectsBeforeLosingScope",
                      Justification = "Damned if you do, damned if you don't Dispose()")>]
    let internal logException moduleId hitPointId context x =
      let text =
        [| sprintf "ModuleId = %A" moduleId
           sprintf "hitPointId = %A" hitPointId
           sprintf "context = %A" context
           sprintf "exception = %s" (x.ToString())
           StackTrace().ToString() |]

      let stamp =
        sprintf "%A" DateTime.UtcNow.Ticks

      let filename =
        ReportFilePath + "." + stamp + ".exn"

      use file =
        File.Open(filename, FileMode.OpenOrCreate, FileAccess.Write)

      use writer = new StreamWriter(file)

      text
      |> Seq.iter (fun line -> writer.WriteLine("{0}", line))

    [<SuppressMessage("Gendarme.Rules.Smells",
                      "AvoidLongParameterListsRule",
                      Justification = "Self-contained internal decorator")>]

    let
#if !DEBUG
      inline
#endif
      internal issue71Wrapper
        visits
        moduleId
        hitPointId
        context
        handler
        add
        =
      try
        add visits moduleId hitPointId context
      with x ->
        match x with
        | :? KeyNotFoundException
        | :? NullReferenceException
        | :? ArgumentNullException -> handler moduleId hitPointId context x
        | _ -> reraise ()

    let
#if !DEBUG
      inline
#endif
      internal curriedIssue71Wrapper
        visits
        moduleId
        hitPointId
        context
        add
        =
      issue71Wrapper visits moduleId hitPointId context logException add

    let internal addVisit moduleId hitPointId context =
      curriedIssue71Wrapper visits moduleId hitPointId context Counter.addSingleVisit

    type InvalidDataException with
      [<SuppressMessage("Gendarme.Rules.Design.Generic",
                        "AvoidMethodWithUnusedGenericTypeRule",
                        Justification = "Matches clause type")>]
      static member Throw<'T>(message: obj) : 'T =
        message.ToString()
        |> InvalidDataException
        |> raise

    let internal takeSample strategy moduleId hitPointId (context: Track) =
      match strategy with
      | Sampling.All -> true
      | _ ->
        (match context with
         | Null -> [ Visit hitPointId ]
         | Time t ->
           [ Visit hitPointId
             TimeVisit(hitPointId, t) ]
         | Call c ->
           [ Visit hitPointId
             CallVisit(hitPointId, c) ]
         | Both b ->
           [ Visit hitPointId
             TimeVisit(hitPointId, b.Time)
             CallVisit(hitPointId, b.Call) ]
         | _ -> context |> InvalidDataException.Throw)
        |> Seq.map (fun hit ->
          if samples.ContainsKey(moduleId) then
            let next = samples.[moduleId]

            let mutable hasPointKey =
              next.ContainsKey(hit)

            if hasPointKey |> not then
              lock next (fun () ->
                hasPointKey <- next.ContainsKey(hit)

                if hasPointKey |> not then
                  next.Add(hit, true))

            not hasPointKey
          else
            false)
        |> Seq.fold (||) false // true if any are novel -- all must be evaluated

    /// <summary>
    /// This method is executed from instrumented assemblies.
    /// </summary>
    /// <param name="moduleId">Assembly being visited</param>
    /// <param name="hitPointId">Sequence Point identifier</param>
    let internal visitImpl moduleId hitPointId context =
      if
        (Sample = Sampling.All
         || takeSample Sample moduleId hitPointId context)
      then
        let adder =
          if !Eager || supervision || (trace.IsConnected |> not) then
            addVisit
          else
            traceVisit

        adder moduleId hitPointId context

    let internal isTracking () =
      (int (CoverageFormat &&& ReportFormat.WithTracking)
       <> 0)

    let internal isTrackingRunner () = isTracking () && isRunner

    let internal granularity () = Timer
    let internal clock () = DateTime.UtcNow.Ticks

    let internal payloadSelection clock frequency wantPayload =
      if wantPayload () then
        match (frequency (), callerId ()) with
        | (0L, None) -> Null
        | (t, None) -> Time(t * (clock () / t))
        | (0L, n) -> Call n.Value
        | (t, n) ->
          Both
            { Time = t * (clock () / t)
              Call = n.Value }
      else
        Null

    let internal payloadControl =
      payloadSelection clock

    let internal payloadSelector enable = payloadControl granularity enable

    let internal visitSelection track moduleId hitPointId =
      visitImpl moduleId hitPointId track

    let internal flushCounter (finish: Close) _ =
      match finish with
      | Resume -> flushResume ()
      | Pause -> flushPause ()
      | _ ->
        recording <- false

        if supervision |> not then
          flushAll finish

    // Register event handling
    let internal doPause =
      FileSystemEventHandler(fun _ a -> flushCounter Pause a)

    let internal doResume =
      FileSystemEventHandler(fun _ a -> flushCounter Resume a)

    let internal doUnload =
      EventHandler(fun _ a -> flushCounter DomainUnload a)

    let internal doExit =
      EventHandler(fun _ a -> flushCounter ProcessExit a)

    let internal startWatcher () =
      watcher.Path <- Path.GetDirectoryName <| signalFile ()
      watcher.Filter <- Path.GetFileName <| signalFile ()
      watcher.add_Created doResume
      watcher.add_Deleted doPause
      watcher.EnableRaisingEvents <- watcher.Path |> String.IsNullOrEmpty |> not

    do
      AppDomain.CurrentDomain.add_DomainUnload doUnload
      AppDomain.CurrentDomain.add_ProcessExit doExit
      startWatcher ()
      signalFile () |> Tracer.Create |> initialiseTrace

  // Public API
  let Visit moduleId hitPointId =
    if I.recording then
      I.visitSelection
        (if I.isTracking () then
           I.payloadSelector I.isTrackingRunner
         else
           Null)
        moduleId
        hitPointId

  // The moduleId strings are not the hash or guids normally found there
  let Push caller =
    I.push caller

    if I.isTrackingRunner () then
      I.visitSelection (Time DateTime.UtcNow.Ticks) Track.Entry caller

  let Pop () =
    let caller = I.pop ()

    if I.isTrackingRunner () && caller.IsSome then
      I.visitSelection (Time DateTime.UtcNow.Ticks) Track.Exit caller.Value

  // Used by the datacollector
  let FlushFinish () = I.flushAll ProcessExit

[<assembly: SuppressMessage("Microsoft.Performance",
                            "CA1810:InitializeReferenceTypeStaticFieldsInline",
                            Scope = "member",
                            Target = "<StartupCode$AltCover-Recorder>.$Base.#.cctor()",
                            Justification = "Compiler generated")>]
[<assembly: SuppressMessage("Microsoft.Performance",
                            "CA1810:InitializeReferenceTypeStaticFieldsInline",
                            Scope = "member",
                            Target = "<StartupCode$AltCover-Recorder>.$Recorder.#.cctor()",
                            Justification = "Compiler generated")>]
[<assembly: SuppressMessage("Gendarme.Rules.Correctness",
                            "DeclareEventsExplicitlyRule",
                            Scope = "type", // TypeDefinition
                            Target = "<StartupCode$AltCover-Recorder>.$Recorder",
                            Justification =
                              "Compiler generated doExit@453 and :doUnload@450")>]
()