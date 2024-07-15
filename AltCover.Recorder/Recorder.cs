namespace AltCover.Recorder
{
  using System;
  using System.Collections.Generic;
  using System.ComponentModel;
  using System.Diagnostics;
  using System.Diagnostics.CodeAnalysis;
  using System.Drawing;
  using System.IO;
  using System.Reflection;

  using System.Resources;
  using System.Runtime.CompilerServices;
  using System.Runtime.InteropServices;
  using System.Runtime.Remoting.Contexts;
  using System.Security.Policy;
  using System.Text.RegularExpressions;
  using System.Threading;

  public static class Instance
  {
    // Public "fields"

    /// <summary>
    /// Gets the location of coverage xml file
    /// This property's IL code is modified to store the actual file location
    /// </summary>
    internal static string ReportFile
    {
      [MethodImpl(MethodImplOptions.NoInlining)]
      get { return "Coverage.Default.xml"; }
    }

    internal static string ReportFilePath
    {
      get
      {
        return canonicalPath(Path.Combine(
            Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location),
            ReportFile));
      }
    }

    internal static string canonicalPath(string path)
    // Mono+Linux barfs at a path of "/_" without the "file://" prefix
    {
      var u = new Uri("file://" + Path.GetFullPath(path), UriKind.Absolute);
      return u.LocalPath;
    }

    /// <summary>
    /// Gets whether to defer output until process exit
    /// This property's IL code is modified to store the actual value
    /// </summary>
    internal static bool Defer
    {
      [MethodImpl(MethodImplOptions.NoInlining)]
      get { return false; }
    }

    /// <summary>
    /// Gets the style of the associated report
    /// This property's IL code is modified to store the user chosen override if applicable
    /// </summary>
    private static ReportFormat __coverageFormat = ReportFormat.NCover;

    internal static ReportFormat CoverageFormat
    {
#if DEBUG
      set { __coverageFormat = value; }
#endif
      [MethodImpl(MethodImplOptions.NoInlining)]
      get { return __coverageFormat; }
    }

    /// <summary>
    /// Gets the frequency of time sampling
    /// This property's IL code is modified to store the user chosen override if applicable
    /// </summary>
    public static long Timer
    {
      [MethodImpl(MethodImplOptions.NoInlining)]
      get { return 0; }
    }

    /// <summary>
    /// Gets the sampling strategy
    /// This property's IL code is modified to store the user chosen override if applicable
    /// </summary>
    internal static Sampling Sample
    {
      [MethodImpl(MethodImplOptions.NoInlining)]
      get { return Sampling.All; }
    }

    /// <summary>
    /// Gets the unique token for this instance
    /// This property's IL code is modified to store a GUID-based token
    /// </summary>
    internal static string Token
    {
      [MethodImpl(MethodImplOptions.NoInlining)]
      get { return "AltCover"; }
    }

    //  /// <summary>
    //  /// Gets the indexed module tokens
    //  /// This property's IL code is modified to store instrumentation results
    //  /// </summary>
    private static IEnumerable<string> __modules = new string[] { string.Empty };

    internal static IEnumerable<string> modules
    {
#if DEBUG
      set { __modules = value; }
#endif
      [MethodImpl(MethodImplOptions.NoInlining)]
      get { return __modules; }
    }

    //    [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
    //    let mutable internal modules =
    //    [| String.Empty |]

    //    [<SuppressMessage("Gendarme.Rules.Performance",
    //                    "AvoidUncalledPrivateCodeRule",
    //                    Justification = "Access by reflection in the data collector")>]

    internal static bool supervision;
    //    let mutable internal supervision =
    //    //Assembly.GetExecutingAssembly().GetName().Name = "AltCover.Recorder.g" &&
    //    AppDomain.CurrentDomain.GetAssemblies()
    //    |> Seq.map(fun a -> a.GetName())
    //    |> Seq.exists(fun n ->
    //      n.Name == "AltCover.DataCollector"
    //      && n.FullName.EndsWith("PublicKeyToken=c02b1a9f5b7cade8", StringComparison.Ordinal))
    //    && Token<> "AltCover"

    internal abstract class Sampled
    { }

    //  type internal Sampled =
    //    | Visit of int
    //    | CallVisit of(int* int)
    //    | TimeVisit of(int* int64)

#if DEBUG

    internal static class I
#else
     private static class I
#endif
    {
      //      let internal resources =
      //      ResourceManager("AltCover.Recorder.Strings", Assembly.GetExecutingAssembly())

      //    let internal getResource s =
      //      let cc =
      //        System.Globalization.CultureInfo.CurrentUICulture

      //      [cc.Name; cc.Parent.Name; "en" ]
      //      |> Seq.map(fun l -> resources.GetString(s + "." + l))
      //      |> Seq.tryFind(String.IsNullOrEmpty >> not)

      internal static Dictionary<string, Dictionary<int, PointVisit>> makeVisits()
      {
        return default;
      }

      //    let private makeVisits() =
      //      [modules
      //        [| Track.Entry; Track.Exit |] ]
      //      |> Seq.concat
      //      |> Seq.fold
      //        (fun (d: Dictionary<string, Dictionary<int, PointVisit>>) k ->
      //          d.Add(k, Dictionary<int, PointVisit>())
      //          d)
      //        (Dictionary<string, Dictionary<int, PointVisit>>())

      /// <summary>
      /// Accumulation of visit records
      /// </summary>
      internal static Dictionary<string, Dictionary<int, PointVisit>> visits = makeVisits();

      internal static Dictionary<string, Dictionary<Sampled, bool>> makeSamples()
      {
        return default;
      }

      //    let internal makeSamples() =
      //      modules
      //      |> Seq.fold
      //        (fun (d: Dictionary<string, Dictionary<Sampled, bool>>) k ->
      //          d.Add(k, Dictionary<Sampled, bool>())
      //          d)
      //        (Dictionary<string, Dictionary<Sampled, bool>>())

      internal static Dictionary<string, Dictionary<Sampled, bool>> samples = makeSamples();

      internal static bool isRunner = false;

      internal static readonly object synchronize = new Object();

      //#if NET20
      //    // class needed for "[ThreadStatic] static val mutable"
      //    [<Sealed>]
      //    type private AsyncLocal<'a>() =
      //      [<ThreadStatic; DefaultValue>]
      //      static val mutable private item: 'a

      //      [<SuppressMessage("Gendarme.Rules.Correctness",
      //                        "MethodCanBeMadeStaticRule",
      //                        Justification = "It's a compatibility hack")>]
      //      member this.Value
      //        with get () = AsyncLocal<'a>.item
      //        and set (value) = AsyncLocal<'a>.item <- value
      //#endif

      //      /// <summary>
      //      /// Gets or sets the current test method
      //      /// </summary>
      //    module private CallTrack =
      //      let value = AsyncLocal<Stack<int>>()

      //      // no race conditions here
      //      let instance() =
      //        match value.Value with
      //        | null -> value.Value<- Stack<int>()
      //        | _ -> ()

      //        value.Value

      //      let private look op =
      //        let i = instance()

      //        match i.Count with
      //        | 0 -> None
      //        | _ -> Some(op i)

      //      let peek () = look(fun i->i.Peek())

      //      let push x = instance().Push x

      //      let pop () = look(fun i->i.Pop())

      internal static Nullable<int> callerId
      {
        get { return default; } //CallTrack.peek()
      }

      internal static void push(int i)
      { }

      //    let internal push x = CallTrack.push x

      internal static int pop()
      { return default; }

      //    let internal pop() = CallTrack.pop()

      /// <summary>
      /// Serialize access to the report file across AppDomains for the classic mode
      /// </summary>
      internal static readonly Mutex mutex = new Mutex(false, Token + ".mutex");

      internal static string signalFile
      {
        get { return ReportFilePath + ".acv"; }
      }

      /// <summary>
      /// Reporting back to the mother-ship
      /// </summary>
      //    let mutable internal trace =
      //      Tracer.Create(signalFile ())

      private static Tracer __trace = Tracer.Create(signalFile);

      internal static Tracer trace
      {
#if DEBUG
        set { __trace = value; }
#endif
        [MethodImpl(MethodImplOptions.NoInlining)]
        get { return __trace; }
      }

      //    let internal withMutex(f: bool -> 'a) =
      //      let own = mutex.WaitOne(1000)

      //      try
      //        f (own)
      //      finally
      //        if own then
      //          mutex.ReleaseMutex()

      //    let internal initialiseTrace(t: Tracer) =
      //      withMutex(fun _ ->
      //        trace<- t.OnStart()
      //        isRunner <- isRunner || trace.IsConnected)

      //    let internal watcher =
      //      new FileSystemWatcher()

      private static bool __recording = true;

      internal static bool recording
      {
#if DEBUG
        set { __recording = value; }
#endif
        [MethodImpl(MethodImplOptions.NoInlining)]
        get { return __recording; }
      }

      internal static void clear()
      { }

      //    let internal clear() =
      //      visits<- makeVisits ()
      //      Counter.branchVisits<- 0L
      //      Counter.totalVisits<- 0L

      /// <summary>
      /// This method flushes hit count buffers.
      /// </summary>
      internal static void flushAll(Close _)
      {
        //    let internal flushAll _ =
        //      let counts = visits
        //      clear()

        //      trace.OnConnected(fun () -> trace.OnFinish counts) (fun () ->
        //        match counts.Values |> Seq.sumBy (fun x -> x.Count) with
        //        | 0 -> ()
        //        | _ ->
        //          withMutex(fun own ->
        //            let delta =
        //              Counter.doFlushFile
        //                ignore
        //                (fun _ _ -> ())
        //                own
        //                counts
        //                CoverageFormat
        //                ReportFilePath
        //                None

        //            getResource "Coverage statistics flushing took {0:N} seconds"
        //            |> Option.iter(fun s -> Console.Out.WriteLine(s, delta.TotalSeconds))))
      }

      //    let internal flushPause() =
      //      ("PauseHandler")
      //      |> getResource
      //      |> Option.iter Console.Out.WriteLine

      //      recording<- false
      //      flushAll Pause
      //      trace<- signalFile () |> Tracer.Create

      //    let internal flushResume() =
      //      ("ResumeHandler")
      //      |> getResource
      //      |> Option.iter Console.Out.WriteLine

      //      let wasConnected = isRunner
      //      initialiseTrace trace

      //      if (wasConnected<> isRunner) then
      //        samples<- makeSamples ()
      //        clear ()

      //      recording <- true

      //    let internal traceVisit moduleId hitPointId context =
      //      lock synchronize(fun () ->
      //        let counts = visits

      //        if counts.Values |> Seq.sumBy (fun x -> x.Count) > 0 then
      //          clear()

      //        trace.OnVisit counts moduleId hitPointId context)

      //    [<SuppressMessage("Microsoft.Usage",
      //                      "CA2202:DisposeObjectsBeforeLosingScope",
      //                      Justification = "Damned if you do, damned if you don't Dispose()")>]
      internal static void logException<T1, T2, T3, T4>(T1 moduleId, T2 hitPointId, T3 context, T4 x)
      { }

      //      let internal logException moduleId hitPointId context x =
      //      let text =
      //        [| sprintf "ModuleId = %A" moduleId
      //           sprintf "hitPointId = %A" hitPointId
      //           sprintf "context = %A" context
      //           sprintf "exception = %s" (x.ToString())
      //           StackTrace().ToString() |]

      //      let stamp =
      //        sprintf "%A" DateTime.UtcNow.Ticks

      //      let filename =
      //        ReportFilePath + "." + stamp + ".exn"

      //      use file =
      //        File.Open(filename, FileMode.OpenOrCreate, FileAccess.Write)

      //      use writer = new StreamWriter(file)

      //      text
      //      |> Seq.iter(fun line -> writer.WriteLine("{0}", line))

      //    [<SuppressMessage("Gendarme.Rules.Smells",
      //                      "AvoidLongParameterListsRule",
      //                      Justification = "Self-contained internal decorator")>]

      internal delegate T HandlerFunction<T1, T2, T3, T>(T1 a, T2 b, T3 c, Exception x);

      internal delegate T Adder<T1, T2, T3, T4, T>(T1 a, T2 b, T3 c, T4 d);

      internal static T issue71Wrapper<T1, T2, T3, T4, T>(T1 visits, T2 moduleId, T3 hitPointId,
        T4 context, HandlerFunction<T1, T2, T3, T> handler, Adder<T1, T2, T3, T4, T> add)
      {
        return default;
      }

      //      let
      //#if !DEBUG
      //      inline
      //#endif
      //      internal issue71Wrapper
      //        visits
      //        moduleId
      //        hitPointId
      //        context
      //        handler
      //        add
      //        =
      //      try
      //        add visits moduleId hitPointId context
      //      with x ->
      //        match x with
      //        | :? KeyNotFoundException
      //        | :? NullReferenceException
      //        | :? ArgumentNullException -> handler moduleId hitPointId context x
      //        | _ -> reraise()

      internal static T curriedIssue71Wrapper<T1, T2, T3, T4, T>(T1 visits, T2 moduleId,
        T3 hitPointId,
        T4 context, Adder<T1, T2, T3, T4, T> add)
      {
        return default;
      }

      //    let
      //#if !DEBUG
      //      inline
      //#endif
      //      internal curriedIssue71Wrapper
      //        visits
      //        moduleId
      //        hitPointId
      //        context
      //        add
      //        =
      //      issue71Wrapper visits moduleId hitPointId context logException add

      //    let internal addVisit moduleId hitPointId context =
      //      curriedIssue71Wrapper visits moduleId hitPointId context Counter.addSingleVisit

      //    type InvalidDataException with
      //      [< SuppressMessage("Gendarme.Rules.Design.Generic",
      //                        "AvoidMethodWithUnusedGenericTypeRule",
      //                        Justification = "Matches clause type") >]
      //      static member Throw<'T>(message: obj) : 'T =
      //        message.ToString()
      //        |> InvalidDataException
      //        |> raise

      internal static bool takeSample(Sampling strategy, string moduleId, int hitPointId, Track context)
      {
        return false;
      }

      //    let internal takeSample strategy moduleId hitPointId(context: Track) =
      //      match strategy with
      //      | Sampling.All -> true
      //      | _ ->
      //        (match context with
      //         | Null -> [Visit hitPointId]
      //         | Time t ->
      //           [Visit hitPointId
      //             TimeVisit(hitPointId, t)]
      //         | Call c ->
      //           [Visit hitPointId
      //             CallVisit(hitPointId, c)]
      //         | Both b ->
      //           [Visit hitPointId
      //             TimeVisit(hitPointId, b.Time)
      //             CallVisit(hitPointId, b.Call)]
      //         | _ -> context |> InvalidDataException.Throw)
      //        |> Seq.map(fun hit ->
      //          if samples.ContainsKey(moduleId) then
      //            let next = samples.[moduleId]

      //            let mutable hasPointKey =
      //              next.ContainsKey(hit)

      //            if hasPointKey |> not then
      //              lock next(fun () ->
      //                hasPointKey<- next.ContainsKey(hit)

      //                if hasPointKey |> not then
      //                  next.Add(hit, true))

      //            not hasPointKey
      //          else
      //            false)
      //        |> Seq.fold(||) false // true if any are novel -- all must be evaluated

      //    /// <summary>
      //    /// This method is executed from instrumented assemblies.
      //    /// </summary>
      //    /// <param name="moduleId">Assembly being visited</param>
      //    /// <param name="hitPointId">Sequence Point identifier</param>
      internal static void visitImpl(string moduleId, int hitPointId, Track context)
      {
      }

      //    let internal visitImpl moduleId hitPointId context =
      //      if
      //        (Sample = Sampling.All
      //         || takeSample Sample moduleId hitPointId context)
      //      then
      //        let adder =
      //          if Defer || supervision || (trace.IsConnected |> not) then
      //            addVisit
      //          else
      //            traceVisit

      //        adder moduleId hitPointId context

      //    let internal isTracking() =
      //      (int (CoverageFormat &&& ReportFormat.WithTracking)
      //       <> 0)

      //    let internal isTrackingRunner() = isTracking() && isRunner

      //    let internal granularity() = Timer
      internal static long granularity
      {
        get { return Timer; }
      }

      internal static long clock()
      { return DateTime.UtcNow.Ticks; }

      internal delegate long ClockProvider();

      internal delegate long FrequencyProvider();

      internal delegate bool PayloadProvider();

      internal static Track payloadSelection(ClockProvider clock,
        FrequencyProvider frequency, PayloadProvider wantPayload)
      {
        if (wantPayload())
        {
          //        match(frequency (), callerId ()) with
          //        | (0L, None) -> Null
          //        | (t, None) -> Time(t* (clock () / t))
          //        | (0L, n) -> Call n.Value
          //        | (t, n) ->
          //          Both
          //            {
          //        Time = t * (clock() / t)
          //              Call = n.Value }
        }
        return new Null();
      }

      internal static Track payloadControl(FrequencyProvider frequency, PayloadProvider wantPayload)
      {
        return payloadSelection(clock, frequency, wantPayload);
      }

      internal delegate T PayloadEnabler<T>();

      internal static Track payloadSelector<T>(PayloadEnabler<T> enable)
      { return default; }

      //    let internal payloadSelector enable = payloadControl granularity enable

      internal static void visitSelection<T>(T track, string moduleId, int hitPointId)
      { }

      //    let internal visitSelection track moduleId hitPointId =
      //      visitImpl moduleId hitPointId track

      //    let internal flushCounter(finish: Close) _ =
      //      match finish with
      //      | Resume -> flushResume()
      //      | Pause -> flushPause()
      //      | _ ->
      //        recording<- false

      //        if supervision |> not then
      //          flushAll finish

      // Register event handling
      internal static FileSystemEventHandler doPause = default;

      //(FileSystemEventHandlerfun _ a -> flushCounter Pause a)

      internal static FileSystemEventHandler doResume = default;
      //FileSystemEventHandler(fun _ a -> flushCounter Resume a)

      internal static EventHandler doUnload = default;
      //EventHandler(fun _ a -> flushCounter DomainUnload a)

      internal static EventHandler doExit = default;
      //EventHandler(fun _ a -> flushCounter ProcessExit a)

      //  let internal startWatcher() =
      //  watcher.Path<- Path.GetDirectoryName<| signalFile ()
      //  watcher.Filter<- Path.GetFileName<| signalFile ()
      //  watcher.add_Created doResume
      //  watcher.add_Deleted doPause
      //  watcher.EnableRaisingEvents<- watcher.Path |> String.IsNullOrEmpty |> not

      //do
      //  AppDomain.CurrentDomain.add_DomainUnload doUnload
      //  AppDomain.CurrentDomain.add_ProcessExit doExit
      //  startWatcher ()
      //  signalFile () |> Tracer.Create |> initialiseTrace
    }

    // Public API
    public static void Visit(string moduleId, int hitPointId)
    {
      //  let Visit moduleId hitPointId =
      //    if I.recording then
      //    I.visitSelection
      //      (if I.isTracking() then
      //         I.payloadSelector I.isTrackingRunner
      //       else
      //         Null)
      //      moduleId
      //      hitPointId
    }

    //// The moduleId strings are not the hash or guids normally found there
    public static void Push(int caller)
    {
      //let Push caller =
      //  I.push caller

      //  if I.isTrackingRunner() then
      //    I.visitSelection(Time DateTime.UtcNow.Ticks) Track.Entry caller
    }

    public static void Pop()
    {
      //let Pop() =
      //  let caller = I.pop()

      //  if I.isTrackingRunner() && caller.IsSome then
      //    I.visitSelection(Time DateTime.UtcNow.Ticks) Track.Exit caller.Value
    }

    //// Used by the datacollector
    internal static void FlushFinish()
    {
      I.flushAll(Close.ProcessExit);
    }
  }
}