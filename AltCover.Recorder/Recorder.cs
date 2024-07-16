using System.Diagnostics.CodeAnalysis;

[assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUnneededFieldInitializationRule",
                            Scope = "member", // MethodDefinition
                            Target = "AltCover.Recorder.Instance::.cctor()",
                            Justification = "Compiler generated")]

namespace AltCover.Recorder
{
  using System;
  using System.Collections.Generic;
  using System.Diagnostics;
  using System.IO;
  using System.Reflection;

  using System.Resources;
  using System.Runtime.CompilerServices;
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
        return CanonicalPath(Path.Combine(
            Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location),
            ReportFile));
      }
    }

    internal static string CanonicalPath(string path)
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
      [SuppressMessage("Gendarme.Rules.Performance",
                       "AvoidUncalledPrivateCodeRule",
                       Justification = "Test interface")]
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

    /// <summary>
    /// Gets the indexed module tokens
    /// This property's IL code is modified to store instrumentation results
    /// </summary>
    private static IEnumerable<string> __modules = new string[] { string.Empty };

    internal static IEnumerable<string> Modules
    {
#if DEBUG
      [SuppressMessage("Gendarme.Rules.Performance",
                       "AvoidUncalledPrivateCodeRule",
                       Justification = "Test interface")]
      set { __modules = value; }
#endif
      [MethodImpl(MethodImplOptions.NoInlining)]
      get { return __modules; }
    }

    [SuppressMessage("Gendarme.Rules.Design",
                     "ConsiderConvertingMethodToPropertyRule",
                     Justification = "A bit big for that")]
    private static bool IsSupervised()
    {
      bool maybe = false;
      foreach (var a in AppDomain.CurrentDomain.GetAssemblies())
      {
        var n = a.GetName();
        maybe = maybe ||
         n.Name == "AltCover.DataCollector"
        && n.FullName.EndsWith("PublicKeyToken=c02b1a9f5b7cade8", StringComparison.Ordinal);
      }

      return maybe && !Token.Equals("AltCover", StringComparison.Ordinal);
    }

    //Assembly.GetExecutingAssembly().GetName().Name = "AltCover.Recorder.g" &&
    internal static bool supervision = IsSupervised();

    [SuppressMessage("Gendarme.Rules.Smells",
                     "AvoidCodeDuplicatedInSiblingClassesRule",
                     Justification = "Too trivial")]
    internal abstract class Sampled
    {
      [SuppressMessage("Gendarme.Rules.Maintainability",
                       "VariableNamesShouldNotMatchFieldNamesRule",
                       Justification = "too trivial")]
      protected Sampled(int visit)
      {
        this.visit = visit;
      }

      public readonly int visit;
    }

    [SuppressMessage("Gendarme.Rules.Performance",
                     "ImplementEqualsTypeRule",
                     Justification = "No use case")]
    internal sealed class SimpleVisit : Sampled
    {
      public SimpleVisit(int visit) : base(visit)
      {
      }

      public override bool Equals(object obj)
      {
        if (obj is SimpleVisit visit1)
        {
          return visit == visit1.visit;
        }
        return false;
      }

      public override int GetHashCode()
      {
        return visit.GetHashCode();
      }
    }

    [SuppressMessage("Gendarme.Rules.Performance",
                     "ImplementEqualsTypeRule",
                     Justification = "No use case")]
    internal sealed class CallVisit : Sampled
    {
      public readonly int call;

      [SuppressMessage("Gendarme.Rules.Maintainability",
                       "VariableNamesShouldNotMatchFieldNamesRule",
                       Justification = "too trivial")]
      public CallVisit(int visit, int call) : base(visit)
      {
        this.call = call;
      }

      public override bool Equals(object obj)
      {
        if (obj is CallVisit visit1)
        {
          return (visit == visit1.visit) && (call == visit1.call);
        }
        return false;
      }

      public override int GetHashCode()
      {
        return visit.GetHashCode() ^ call.GetHashCode();
      }
    }

    [SuppressMessage("Gendarme.Rules.Performance",
                     "ImplementEqualsTypeRule",
                     Justification = "No use case")]
    internal sealed class TimeVisit : Sampled
    {
      public readonly long time;

      [SuppressMessage("Gendarme.Rules.Maintainability",
                       "VariableNamesShouldNotMatchFieldNamesRule",
                       Justification = "too trivial")]
      public TimeVisit(int visit, long time) : base(visit)
      {
        this.time = time;
      }

      public override bool Equals(object obj)
      {
        if (obj is TimeVisit visit1)
        {
          return (visit == visit1.visit) && (time == visit1.time);
        }
        return false;
      }

      public override int GetHashCode()
      {
        return visit.GetHashCode() ^ time.GetHashCode();
      }
    }

#if DEBUG

    [SuppressMessage("Gendarme.Rules.Smells",
                     "AvoidLargeClassesRule",
                     Justification = "No. Go away.")]
    [SuppressMessage("Gendarme.Rules.Smells",
                     "AvoidLongParameterListsRule",
                     Justification = "Stable code")]
    internal static class I
#else

    private static class I
#endif
    {
      internal static readonly ResourceManager resources =
            new ResourceManager("AltCover.Recorder.Strings", Assembly.GetExecutingAssembly());

      internal static string GetResource(string s)
      {
        var cc = System.Globalization.CultureInfo.CurrentUICulture;
        var names = new string[] { cc.Name, cc.Parent.Name, "en" };
        foreach (var name in names)
        {
          var result = resources.GetString(s + "." + name);
          if (!string.IsNullOrEmpty(result))
            return result;
        }

        return null;
      }

      private static Dictionary<string, Dictionary<int, PointVisit>> MakeVisits()
      {
        var d = new Dictionary<string, Dictionary<int, PointVisit>>
        {
          { Track.Entry, new Dictionary<int, PointVisit>() },
          { Track.Exit, new Dictionary<int, PointVisit>() }
        };
        foreach (var item in Modules)
        {
          d.Add(item, new Dictionary<int, PointVisit>());
        }
        return d;
      }

      /// <summary>
      /// Accumulation of visit records
      /// </summary>
      internal static Dictionary<string, Dictionary<int, PointVisit>> visits = MakeVisits();

      internal static Dictionary<string, Dictionary<Sampled, bool>> MakeSamples()
      {
        var d = new Dictionary<string, Dictionary<Sampled, bool>>();
        foreach (var item in Modules)
        {
          d.Add(item, new Dictionary<Sampled, bool>());
        }
        return d;
      }

      internal static Dictionary<string, Dictionary<Sampled, bool>> samples = MakeSamples();

      internal static bool isRunner = false;

      internal static readonly object synchronize = new Object();

#if NET20

      // class needed for "[ThreadStatic] static val mutable"
      private sealed class AsyncLocal<T>
      {
        private static T item;

        public T Value
        {
          [SuppressMessage("Gendarme.Rules.Correctness",
                           "MethodCanBeMadeStaticRule",
                           Justification = "Whole point of the exercise")]
          get { return item; }
          [SuppressMessage("Gendarme.Rules.Correctness",
                           "MethodCanBeMadeStaticRule",
                           Justification = "Whole point of the exercise")]
          [SuppressMessage("Gendarme.Rules.Concurrency",
                           "WriteStaticFieldFromInstanceMethodRule",
                           Justification = "Whole point of the exercise")]
          set { item = value; }
        }
      }

#endif

      /// <summary>
      /// Gets or sets the current test method
      /// </summary>
      private static class CallTrack
      {
        private static AsyncLocal<Stack<int>> value = new AsyncLocal<Stack<int>>();

        // no race conditions here
        private static Stack<int> instance()
        {
          if (value.Value == null)
            value.Value = new Stack<int>();

          return value.Value;
        }

        public static Nullable<int> Peek()
        {
          var i = instance();
          return (i.Count > 0) ? (Nullable<int>)i.Peek() : null;
        }

        public static void Push(int x)
        {
          instance().Push(x);
        }

        public static Nullable<int> Pop()
        {
          var i = instance();
          return (i.Count > 0) ? (Nullable<int>)i.Pop() : null;
        }
      }

      internal static Nullable<int> CallerId
      {
        get { return CallTrack.Peek(); }
      }

      internal static void Push(int i)
      {
        CallTrack.Push(i);
      }

      internal static Nullable<int> Pop()
      { return CallTrack.Pop(); }

      /// <summary>
      /// Serialize access to the report file across AppDomains for the classic mode
      /// </summary>
      internal static readonly Mutex mutex = new Mutex(false, Token + ".mutex");

      internal static string SignalFile
      {
        get { return ReportFilePath + ".acv"; }
      }

      /// <summary>
      /// Reporting back to the mother-ship
      /// </summary>
      private static Tracer __trace = Tracer.Create(SignalFile);

      internal static Tracer Trace
      {
        set { __trace = value; }
        [MethodImpl(MethodImplOptions.NoInlining)]
        get { return __trace; }
      }

      [SuppressMessage("Gendarme.Rules.Design.Generic",
                       "AvoidDeclaringCustomDelegatesRule",
                       Justification = "Net Framework 2.0")]
      internal delegate void MutexHandler(bool own);

      internal static void WithMutex(MutexHandler f)
      {
        var own = mutex.WaitOne(1000);

        try
        {
          f(own);
        }
        finally
        {
          if (own) mutex.ReleaseMutex();
        }
      }

      internal static void InitialiseTrace(Tracer t)
      {
        WithMutex(
          x =>
          {
            Trace = t.OnStart();
            isRunner = isRunner || Trace.IsConnected;
          }
          );
      }

      internal static FileSystemWatcher watcher = new FileSystemWatcher();

      private static bool __recording = true;

      internal static bool Recording
      {
        set { __recording = value; }
        [MethodImpl(MethodImplOptions.NoInlining)]
        get { return __recording; }
      }

      internal static void Clear()
      {
        visits = MakeVisits();
        Counter.branchVisits = 0;
        Counter.totalVisits = 0;
      }

      /// <summary>
      /// This method flushes hit count buffers.
      /// </summary>
      [SuppressMessage("Gendarme.Rules.Performance",
                       "AvoidUnusedParametersRule",
                       Justification = "Tidying later perhaps")]
      internal static void FlushAll(Close _)
      {
        var counts = visits;
        Clear();

        Trace.OnConnected(
          () => Trace.OnFinish(counts),
          () =>
          {
            var any = false;
            foreach (var n in counts.Values)
            {
              if (n.Count > 0)
              {
                any = true;
                break;
              }
            }

            if (!any) return;

            WithMutex(own =>
              {
                var delta =
                Counter.DoFlushFile(
                  x => { return; },
                  (x, y) => { return; },
                  own,
                  counts,
                  CoverageFormat,
                  ReportFilePath,
                  null
                  );

                var message = GetResource("Coverage statistics flushing took {0:N} seconds");
                if (!string.IsNullOrEmpty(message))
                  Console.Out.WriteLine(message, delta.TotalSeconds);
              });
          }
          );
      }

      internal static void FlushPause()
      {
        var message = GetResource("PauseHandler");
        if (!string.IsNullOrEmpty(message))
          Console.Out.WriteLine(message);

        Recording = false;
        FlushAll(Close.Pause);
        Trace = Tracer.Create(SignalFile);
      }

      internal static void FlushResume()
      {
        var message = GetResource("ResumeHandler");
        if (!string.IsNullOrEmpty(message))
          Console.Out.WriteLine(message);

        var wasConnected = isRunner;
        InitialiseTrace(Trace);

        if (wasConnected != isRunner)
        {
          samples = MakeSamples();
          Clear();
        }

        Recording = true;
      }

      internal static void TraceVisit(string moduleId, int hitPointId, Track context)
      {
        lock (synchronize)
        {
          var counts = visits;
          //if counts.Values |> Seq.sumBy (fun x -> x.Count) > 0 then
          foreach (var item in counts.Values)
          {
            if (item.Count > 0)
            {
              Clear();
              break;
            }
          }

          Trace.OnVisit(counts, moduleId, hitPointId, context);
        }
      }

      [SuppressMessage("Gendarme.Rules.Globalization",
                       "PreferIFormatProviderOverrideRule",
                       Justification = "later, perhaps")]
      [SuppressMessage("Microsoft.Globalization",
                       "CA1305:SpecifyIFormatProvider",
                       Justification = "later, perhaps")]
      internal static void LogException<T1, T2, T3, T4>(T1 moduleId, T2 hitPointId, T3 context, T4 x)
      {
        var text = new string[] {
          String.Format("ModuleId = {0}", moduleId),
          String.Format("hitPointId = {0}", hitPointId),
          String.Format("context = {0}", context),
          String.Format("exception = {0}", x.ToString()),
          (new StackTrace()).ToString()
        };

        var stamp = DateTime.UtcNow.Ticks.ToString();
        var filename = ReportFilePath + "." + stamp + ".exn";
        using (var file = File.Open(filename, FileMode.OpenOrCreate, FileAccess.Write))
        using (var writer = new StreamWriter(file))
        {
          foreach (var line in text)
            writer.WriteLine(line);
        }
      }

      [SuppressMessage("Gendarme.Rules.Design.Generic",
                       "AvoidDeclaringCustomDelegatesRule",
                       Justification = "Net Framework 2.0")]
      internal delegate void HandlerFunction<T1, T2, T3>(T1 a, T2 b, T3 c, Exception x);

      [SuppressMessage("Gendarme.Rules.Design.Generic",
                       "AvoidDeclaringCustomDelegatesRule",
                       Justification = "Net Framework 2.0")]
      internal delegate void Adder<T1, T2, T3, T4>(T1 a, T2 b, T3 c, T4 d);

      internal static void Issue71Wrapper<T1, T2, T3, T4>(T1 visitsIn, T2 moduleId, T3 hitPointId,
        T4 context, HandlerFunction<T2, T3, T4> handler, Adder<T1, T2, T3, T4> add)
      {
        try
        {
          add(visitsIn, moduleId, hitPointId, context);
        }
        catch (Exception x)
        {
          if (x is KeyNotFoundException || x is NullReferenceException || x is ArgumentNullException)
          {
            handler(moduleId, hitPointId, context, x);
          }
          else throw;
        }
      }

      internal static void CurriedIssue71Wrapper<T1, T2, T3, T4>(T1 visitsIn, T2 moduleId,
        T3 hitPointId,
        T4 context, Adder<T1, T2, T3, T4> add)
      {
        Issue71Wrapper(visitsIn, moduleId, hitPointId, context, LogException, add);
      }

      internal static void AddVisit(string moduleId, int hitPointId, Track context)
      {
        CurriedIssue71Wrapper(visits, moduleId, hitPointId, context, Counter.AddSingleVisit);
      }

      internal static bool TakeSample(Sampling strategy, string moduleId, int hitPointId, Track context)
      {
        if (strategy == Sampling.All)
          return true;

        Sampled[] sampleds = null;
        if (context is Null)
          sampleds = new Sampled[] { new SimpleVisit(hitPointId) };
        else if (context is Time t)
          sampleds = new Sampled[] { new SimpleVisit(hitPointId),
                                     new TimeVisit(hitPointId, t.Value)};
        else if (context is Call c)
          sampleds = new Sampled[] { new SimpleVisit(hitPointId),
                                     new CallVisit(hitPointId, c.Value)};
        else if (context is Both b)
        {
          sampleds = new Sampled[] { new SimpleVisit(hitPointId),
                                     new TimeVisit(hitPointId, b.Value.Time),
                                     new CallVisit(hitPointId, b.Value.Call)};
        }
        else throw new InvalidDataException(context.ToString());

        var wanted = false;
        foreach (var sample in sampleds)
        {
          if (!samples.ContainsKey(moduleId))
            continue;
          var next = samples[moduleId];
          var hasPointKey = next.ContainsKey(sample);
          if (!hasPointKey)
          {
            lock (next)
            {
              hasPointKey = next.ContainsKey(sample);
              if (!hasPointKey)
              {
                next.Add(sample, true);
              }
            }

            wanted = wanted || !hasPointKey;
          }
        }

        return wanted;
      }

      /// <summary>
      /// This method is executed from instrumented assemblies.
      /// </summary>
      /// <param name="moduleId">Assembly being visited</param>
      /// <param name="hitPointId">Sequence Point identifier</param>
      /// <param name="context">What sort of visit</param>
      internal static void VisitImpl(string moduleId, int hitPointId, Track context)
      {
        if
          (Sample == Sampling.All
           || TakeSample(Sample, moduleId, hitPointId, context))
        {
          if (Defer || supervision || !Trace.IsConnected)
          {
            AddVisit(moduleId, hitPointId, context);
          }
          else
          {
            TraceVisit(moduleId, hitPointId, context);
          }
        }
      }

      internal static bool IsTracking
      {
        get { return (CoverageFormat & ReportFormat.WithTracking) != 0; }
      }

      [SuppressMessage("Gendarme.Rules.Design",
                       "ConsiderConvertingMethodToPropertyRule",
                       Justification = "No use case")]
      internal static bool IsTrackingRunner()
      {
        return IsTracking && isRunner;
      }

      internal static long Granularity()
      {
        return Timer;
      }

      internal static long Clock()
      {
        return DateTime.UtcNow.Ticks;
      }

      [SuppressMessage("Gendarme.Rules.Design.Generic",
                       "AvoidDeclaringCustomDelegatesRule",
                       Justification = "Net Framework 2.0")]
      internal delegate long ClockProvider();

      [SuppressMessage("Gendarme.Rules.Design.Generic",
                       "AvoidDeclaringCustomDelegatesRule",
                       Justification = "Net Framework 2.0")]
      internal delegate long FrequencyProvider();

      [SuppressMessage("Gendarme.Rules.Design.Generic",
                       "AvoidDeclaringCustomDelegatesRule",
                       Justification = "Net Framework 2.0")]
      internal delegate bool PayloadProvider();

      [SuppressMessage("Gendarme.Rules.Performance",
                       "AvoidRepetitiveCallsToPropertiesRule",
                       Justification = "Separate branches")]
      internal static Track PayloadSelection(ClockProvider clock,
        FrequencyProvider frequency, PayloadProvider wantPayload)
      {
        if (wantPayload())
        {
          var f = frequency();
          var id = CallerId;
          if (f == 0)
          {
            return id.HasValue ?
              (Track)new Call(id.Value) :
                     new Null();
          }
          else
          {
            var t = f * (clock() / f);
            return id.HasValue ?
                (Track)new Both(Pair.Create(t, id.Value)) :
                       new Time(t);
          }
        }
        return new Null();
      }

      internal static Track PayloadControl(FrequencyProvider frequency, PayloadProvider wantPayload)
      {
        return PayloadSelection(Clock, frequency, wantPayload);
      }

      internal static Track PayloadSelector(PayloadProvider enable)
      {
        return PayloadControl(Granularity, enable);
      }

      internal static void VisitSelection(Track track, string moduleId, int hitPointId)
      {
        VisitImpl(moduleId, hitPointId, track);
      }

      internal static void FlushCounter(Close finish, EventArgs _)
      {
        switch (finish)
        {
          case Close.Resume:
            FlushResume(); break;
          case Close.Pause:
            FlushPause(); break;
          default:
            Recording = false;
            if (!supervision)
              FlushAll(finish);
            break;
        }
      }

      // Register event handling
      internal static FileSystemEventHandler doPause = PauseHandler;

      internal static void PauseHandler(object sender, FileSystemEventArgs e)
      {
        FlushCounter(Close.Pause, e);
      }

      internal static FileSystemEventHandler doResume = ResumeHandler;

      internal static void ResumeHandler(object sender, FileSystemEventArgs e)
      {
        FlushCounter(Close.Resume, e);
      }

      [SuppressMessage("Gendarme.Rules.Correctness",
                       "DeclareEventsExplicitlyRule",
                       Justification = "Wrong use case")]
      internal static EventHandler doUnload = UnloadHandler;

      internal static void UnloadHandler(object sender, EventArgs e)
      {
        FlushCounter(Close.DomainUnload, e);
      }

      [SuppressMessage("Gendarme.Rules.Correctness",
                       "DeclareEventsExplicitlyRule",
                       Justification = "Wrong use case")]
      internal static EventHandler doExit = ExitHandler;

      internal static void ExitHandler(object sender, EventArgs e)
      {
        FlushCounter(Close.ProcessExit, e);
      }

      internal static void StartWatcher()
      {
        var s = SignalFile;
        watcher.Path = Path.GetDirectoryName(s);
        watcher.Filter = Path.GetFileName(s);
        watcher.Created += doResume;
        watcher.Deleted += doPause;
        watcher.EnableRaisingEvents = !String.IsNullOrEmpty(watcher.Path);
      }

      [SuppressMessage("Gendarme.Rules.Performance",
                       "AvoidUnneededFieldInitializationRule",
                       Justification = "Simpler this way")]
      [SuppressMessage("Gendarme.Rules.Performance",
                       "AvoidRepetitiveCallsToPropertiesRule",
                       Justification = "You what, mate?")]
      [SuppressMessage("Microsoft.Performance",
                       "CA1810:InitializeReferenceTypeStaticFieldsInline",
                       Justification = "Simpler this way")]
      static I()
      {
        AppDomain.CurrentDomain.DomainUnload += doUnload;
        AppDomain.CurrentDomain.ProcessExit += doExit;
        StartWatcher();
        InitialiseTrace(
          Tracer.Create(SignalFile)
           );
      }
    }

    // Public API
    public static void Visit(string moduleId, int hitPointId)
    {
      if (I.Recording)
      {
        var track = I.IsTracking ?
                           I.PayloadSelector(I.IsTrackingRunner) :
                           new Null();
        I.VisitSelection(track, moduleId, hitPointId);
      }
    }

    //// The moduleId strings are not the hash or guids normally found there
    public static void Push(int caller)
    {
      I.Push(caller);

      if (I.IsTrackingRunner())
        I.VisitSelection(new Time(DateTime.UtcNow.Ticks), Track.Entry, caller);
    }

    public static void Pop()
    {
      var caller = I.Pop();

      if (I.IsTrackingRunner() && caller.HasValue)
        I.VisitSelection(new Time(DateTime.UtcNow.Ticks), Track.Exit, caller.Value);
    }

    [SuppressMessage("Gendarme.Rules.Performance",
                     "AvoidUncalledPrivateCodeRule",
                     Justification = "Internals Visible To")]
    internal static void FlushFinish()
    {
      I.FlushAll(Close.ProcessExit);
    }
  }
}