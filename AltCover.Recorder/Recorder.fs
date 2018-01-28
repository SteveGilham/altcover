// Based upon C# code by Sergiy Sakharov (sakharov@gmail.com)
// http://code.google.com/p/dot-net-coverage/source/browse/trunk/Coverage.Counter/Coverage.Counter.csproj

namespace AltCover.Recorder

open System
open System.Collections.Generic
open System.Runtime.CompilerServices

module Instance =

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
  let mutable internal trace = Tracer.Create Token

  let internal WithMutex (f : bool -> 'a) =
    let own = mutex.WaitOne(10000)
    try
      f(own)
    finally
      if own then mutex.ReleaseMutex()

  /// <summary>
  /// Save sequence point hit counts to xml report file
  /// </summary>
  /// <param name="hitCounts">The coverage results to incorporate</param>
  /// <param name="coverageFile">The coverage file to update as a stream</param>
  let internal UpdateReport (counts:Dictionary<string, Dictionary<int, int>>) coverageFile =
    WithMutex (fun own -> Counter.UpdateReport own counts coverageFile)

  /// <summary>
  /// Synchronize an action on the visits table
  /// </summary>
  let private WithVisitsLocked =
    Locking.WithLockerLocked Visits

  /// <summary>
  /// This method flushes hit count buffers.
  /// </summary>
  let internal FlushCounter (finish:bool) _ =
    trace.OnConnected (fun () -> WithVisitsLocked (fun () -> trace.CatchUp Visits)
                                 trace.OnFinish finish)
     WithVisitsLocked
      (fun () ->
      if finish then
        trace.Close()
      match Visits.Count with
      | 0 -> ()
      | _ -> let counts = Dictionary<string, Dictionary<int, int>> Visits
             Visits.Clear()
             WithMutex (fun own ->
                Counter.DoFlush own counts ReportFile
             ))

  let internal TraceVisit moduleId hitPointId =
     WithVisitsLocked (fun () -> trace.OnVisit Visits moduleId hitPointId)

  /// <summary>
  /// This method is executed from instrumented assemblies.
  /// </summary>
  /// <param name="moduleId">Assembly being visited</param>
  /// <param name="hitPointId">Sequence Point identifier</param>
  let Visit moduleId hitPointId =
    if not <| String.IsNullOrEmpty(moduleId) then
      trace.OnConnected (fun () -> TraceVisit moduleId hitPointId)
        WithVisitsLocked (fun () -> Counter.AddVisit Visits moduleId hitPointId)

  // Register event handling
  do
    AppDomain.CurrentDomain.DomainUnload.Add(FlushCounter false)
    AppDomain.CurrentDomain.ProcessExit.Add(FlushCounter true)
    trace.OnStart 2000