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

    /// <summary>
    /// Gets the indexed module tokens
    /// This property's IL code is modified to store instrumentation results
    /// </summary>
    private static IEnumerable<string> __modules = new string[] { string.Empty };

    internal static IEnumerable<string> modules
    {
#if DEBUG
      set { __modules = value; }
#endif
      [MethodImpl(MethodImplOptions.NoInlining)]
      get { return __modules; }
    }

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

    internal abstract class Sampled
    {
      protected Sampled(int visit)
      {
        this.visit = visit;
      }

      public readonly int visit;
    }

    internal class SimpleVisit : Sampled
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

    internal class CallVisit : Sampled
    {
      public readonly int call;

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

    internal class TimeVisit : Sampled
    {
      public readonly long time;

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

    internal static class I
#else

    private static class I
#endif
    {
      internal static readonly ResourceManager resources =
            new ResourceManager("AltCover.Recorder.Strings", Assembly.GetExecutingAssembly());

      internal static string getResource(string s)
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

      private static Dictionary<string, Dictionary<int, PointVisit>> makeVisits()
      {
        var d = new Dictionary<string, Dictionary<int, PointVisit>>
        {
          { Track.Entry, new Dictionary<int, PointVisit>() },
          { Track.Exit, new Dictionary<int, PointVisit>() }
        };
        foreach (var item in modules)
        {
          d.Add(item, new Dictionary<int, PointVisit>());
        }
        return d;
      }

      /// <summary>
      /// Accumulation of visit records
      /// </summary>
      internal static Dictionary<string, Dictionary<int, PointVisit>> visits = makeVisits();

      internal static Dictionary<string, Dictionary<Sampled, bool>> makeSamples()
      {
        var d = new Dictionary<string, Dictionary<Sampled, bool>>();
        foreach (var item in modules)
        {
          d.Add(item, new Dictionary<Sampled, bool>());
        }
        return d;
      }

      internal static Dictionary<string, Dictionary<Sampled, bool>> samples = makeSamples();

      internal static bool isRunner = false;

      internal static readonly object synchronize = new Object();

#if NET20

      // class needed for "[ThreadStatic] static val mutable"
      private sealed class AsyncLocal<T>
      {
        private static T item;

        public T Value
        {
          get { return item; }
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

        public static Nullable<int> peek()
        {
          var i = instance();
          return (i.Count > 0) ? (Nullable<int>)i.Peek() : null;
        }

        public static void push(int x)
        {
          instance().Push(x);
        }

        public static Nullable<int> pop()
        {
          var i = instance();
          return (i.Count > 0) ? (Nullable<int>)i.Pop() : null;
        }
      }

      internal static Nullable<int> callerId
      {
        get { return CallTrack.peek(); }
      }

      internal static void push(int i)
      {
        CallTrack.push(i);
      }

      internal static Nullable<int> pop()
      { return CallTrack.pop(); }

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
      private static Tracer __trace = Tracer.Create(signalFile);

      internal static Tracer trace
      {
        set { __trace = value; }
        [MethodImpl(MethodImplOptions.NoInlining)]
        get { return __trace; }
      }

      internal delegate void MutexHandler(bool own);

      internal static void withMutex(MutexHandler f)
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

      internal static void initialiseTrace(Tracer t)
      {
        withMutex(
          x =>
          {
            trace = t.OnStart();
            isRunner = isRunner || trace.IsConnected;
          }
          );
      }

      internal static FileSystemWatcher watcher = new FileSystemWatcher();

      private static bool __recording = true;

      internal static bool recording
      {
        set { __recording = value; }
        [MethodImpl(MethodImplOptions.NoInlining)]
        get { return __recording; }
      }

      internal static void clear()
      {
        visits = makeVisits();
        Counter.branchVisits = 0;
        Counter.totalVisits = 0;
      }

      /// <summary>
      /// This method flushes hit count buffers.
      /// </summary>
      internal static void flushAll(Close _)
      {
        var counts = visits;
        clear();

        trace.OnConnected(
          () => trace.OnFinish(counts),
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

            withMutex(own =>
              {
                var delta =
                Counter.doFlushFile(
                  x => { return; },
                  (x, y) => { return; },
                  own,
                  counts,
                  CoverageFormat,
                  ReportFilePath,
                  null
                  );

                var message = getResource("Coverage statistics flushing took {0:N} seconds");
                if (!string.IsNullOrEmpty(message))
                  Console.Out.WriteLine(message, delta.TotalSeconds);
              });
          }
          );
      }

      internal static void flushPause()
      {
        var message = getResource("PauseHandler");
        if (!string.IsNullOrEmpty(message))
          Console.Out.WriteLine(message);

        recording = false;
        flushAll(Close.Pause);
        trace = Tracer.Create(signalFile);
      }

      internal static void flushResume()
      {
        var message = getResource("ResumeHandler");
        if (!string.IsNullOrEmpty(message))
          Console.Out.WriteLine(message);

        var wasConnected = isRunner;
        initialiseTrace(trace);

        if (wasConnected != isRunner)
        {
          samples = makeSamples();
          clear();
        }

        recording = true;
      }

      internal static void traceVisit(string moduleId, int hitPointId, Track context)
      {
        lock (synchronize)
        {
          var counts = visits;
          //if counts.Values |> Seq.sumBy (fun x -> x.Count) > 0 then
          foreach (var item in counts.Values)
          {
            if (item.Count > 0)
            {
              clear();
              break;
            }
          }

          trace.OnVisit(counts, moduleId, hitPointId, context);
        }
      }

      internal static void logException<T1, T2, T3, T4>(T1 moduleId, T2 hitPointId, T3 context, T4 x)
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

      internal delegate void HandlerFunction<T1, T2, T3>(T1 a, T2 b, T3 c, Exception x);

      internal delegate void Adder<T1, T2, T3, T4>(T1 a, T2 b, T3 c, T4 d);

      internal static void issue71Wrapper<T1, T2, T3, T4>(T1 visits, T2 moduleId, T3 hitPointId,
        T4 context, HandlerFunction<T2, T3, T4> handler, Adder<T1, T2, T3, T4> add)
      {
        try
        {
          add(visits, moduleId, hitPointId, context);
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

      internal static void curriedIssue71Wrapper<T1, T2, T3, T4>(T1 visits, T2 moduleId,
        T3 hitPointId,
        T4 context, Adder<T1, T2, T3, T4> add)
      {
        issue71Wrapper(visits, moduleId, hitPointId, context, logException, add);
      }

      internal static void addVisit(string moduleId, int hitPointId, Track context)
      {
        curriedIssue71Wrapper(visits, moduleId, hitPointId, context, Counter.addSingleVisit);
      }

      internal static bool takeSample(Sampling strategy, string moduleId, int hitPointId, Track context)
      {
        if (strategy == Sampling.All)
          return true;

        Sampled[] sampleds = null;
        if (context is Null)
          sampleds = new Sampled[] { new SimpleVisit(hitPointId) };
        else if (context is Time)
          sampleds = new Sampled[] { new SimpleVisit(hitPointId),
                                     new TimeVisit(hitPointId, ((Time)context).Value) };
        else if (context is Call)
          sampleds = new Sampled[] { new SimpleVisit(hitPointId),
                                     new CallVisit(hitPointId, ((Call)context).Value) };
        else if (context is Both)
        {
          var b = (Both)context;
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
      internal static void visitImpl(string moduleId, int hitPointId, Track context)
      {
        if
          (Sample == Sampling.All
           || takeSample(Sample, moduleId, hitPointId, context))
        {
          if (Defer || supervision || !trace.IsConnected)
          {
            addVisit(moduleId, hitPointId, context);
          }
          else
          {
            traceVisit(moduleId, hitPointId, context);
          }
        }
      }

      internal static bool isTracking
      {
        get { return (CoverageFormat & ReportFormat.WithTracking) != 0; }
      }

      internal static bool isTrackingRunner()
      {
        return isTracking && isRunner;
      }

      internal static long granularity()
      {
        return Timer;
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
          var f = frequency();
          var id = callerId;
          if (f == 0)
          {
            return id.HasValue ?
              (Track)new Call(id.Value) :
                     new Null();
          }

          var t = f * (clock() / f);
          return id.HasValue ?
              (Track)new Both(Pair.Create(t, id.Value)) :
                     new Time(t);
        }
        return new Null();
      }

      internal static Track payloadControl(FrequencyProvider frequency, PayloadProvider wantPayload)
      {
        return payloadSelection(clock, frequency, wantPayload);
      }

      internal static Track payloadSelector(PayloadProvider enable)
      {
        return payloadControl(granularity, enable);
      }

      internal static void visitSelection(Track track, string moduleId, int hitPointId)
      {
        visitImpl(moduleId, hitPointId, track);
      }

      internal static void flushCounter(Close finish, EventArgs _)
      {
        switch (finish)
        {
          case Close.Resume:
            flushResume(); break;
          case Close.Pause:
            flushPause(); break;
          default:
            recording = false;
            if (!supervision)
              flushAll(finish);
            break;
        }
      }

      // Register event handling
      internal static FileSystemEventHandler doPause = PauseHandler;

      internal static void PauseHandler(object sender, FileSystemEventArgs e)
      {
        flushCounter(Close.Pause, e);
      }

      internal static FileSystemEventHandler doResume = ResumeHandler;

      internal static void ResumeHandler(object sender, FileSystemEventArgs e)
      {
        flushCounter(Close.Resume, e);
      }

      internal static EventHandler doUnload = UnloadHandler;

      internal static void UnloadHandler(object sender, EventArgs e)
      {
        flushCounter(Close.DomainUnload, e);
      }

      internal static EventHandler doExit = ExitHandler;

      internal static void ExitHandler(object sender, EventArgs e)
      {
        flushCounter(Close.ProcessExit, e);
      }

      internal static void startWatcher()
      {
        watcher.Path = Path.GetDirectoryName(signalFile);
        watcher.Filter = Path.GetFileName(signalFile);
        watcher.Created += doResume;
        watcher.Deleted += doPause;
        watcher.EnableRaisingEvents = !String.IsNullOrEmpty(watcher.Path);
      }

      static I()
      {
        AppDomain.CurrentDomain.DomainUnload += doUnload;
        AppDomain.CurrentDomain.ProcessExit += doExit;
        startWatcher();
        initialiseTrace(
          Tracer.Create(signalFile)
           );
      }
    }

    // Public API
    public static void Visit(string moduleId, int hitPointId)
    {
      if (I.recording)
      {
        var track = I.isTracking ?
                           I.payloadSelector(I.isTrackingRunner) :
                           new Null();
        I.visitSelection(track, moduleId, hitPointId);
      }
    }

    //// The moduleId strings are not the hash or guids normally found there
    public static void Push(int caller)
    {
      I.push(caller);

      if (I.isTrackingRunner())
        I.visitSelection(new Time(DateTime.UtcNow.Ticks), Track.Entry, caller);
    }

    public static void Pop()
    {
      var caller = I.pop();

      if (I.isTrackingRunner() && caller.HasValue)
        I.visitSelection(new Time(DateTime.UtcNow.Ticks), Track.Exit, caller.Value);
    }

    //// Used by the datacollector
    internal static void FlushFinish()
    {
      I.flushAll(Close.ProcessExit);
    }
  }
}