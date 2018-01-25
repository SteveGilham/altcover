// Based upon C# code by Sergiy Sakharov (sakharov@gmail.com)
// http://code.google.com/p/dot-net-coverage/source/browse/trunk/Coverage.Counter/Coverage.Counter.csproj

#if RUNNER
namespace AltCover.Base
#else
namespace AltCover.Recorder
#endif

open System
#if NETSTANDARD2_0
open System.IO
#endif
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

  /// <summary>
  /// Save sequence point hit counts to xml report file
  /// </summary>
  /// <param name="hitCounts">The coverage results to incorporate</param>
  /// <param name="coverageFile">The coverage file to update as a stream</param>
  let internal UpdateReport (counts:Dictionary<string, Dictionary<int, int>>) coverageFile =
    Locking.WithMutex mutex (fun own -> Counter.UpdateReport own counts coverageFile)

  /// <summary>
  /// Synchronize an action on the visits table
  /// </summary>
  let private WithVisitsLocked =
    Locking.WithLockerLocked Visits

#if NETSTANDARD2_0
  let internal OnConnected f g =
     if trace.IsActivated() then f()
     else WithVisitsLocked g
#endif


#if RUNNER
#else
  /// <summary>
  /// This method flushes hit count buffers.
  /// </summary>
  let internal FlushCounter finish _ =
#if NETSTANDARD2_0
    OnConnected (fun () ->
      printfn "**pushing flush %A" finish
      if finish then
        trace.Push null -1)
#else
    ignore finish
    WithVisitsLocked
#endif
      (fun () ->
      if finish then
        trace.Close()
      match Visits.Count with
      | 0 -> ()
      | _ -> let counts = Dictionary<string, Dictionary<int, int>> Visits
             Visits.Clear()
             Locking.WithMutex mutex (fun own -> 
                Counter.DoFlush own counts ReportFile
             ))

  /// <summary>
  /// This method is executed from instrumented assemblies.
  /// </summary>
  /// <param name="moduleId">Assembly being visited</param>
  /// <param name="hitPointId">Sequence Point identifier</param>
  let Visit moduleId hitPointId =
    if not <| String.IsNullOrEmpty(moduleId) then
#if NETSTANDARD2_0
      OnConnected (fun () ->
        WithVisitsLocked (fun () -> trace.CatchUp Visits)
        printfn "**pushing Visit"
        trace.Push moduleId hitPointId
                 )
#else
      WithVisitsLocked
#endif
                 (fun () -> Counter.AddVisit Visits moduleId hitPointId)

#if NETSTANDARD2_0
  let internal Connect (name:string) (p:Tracer) =
    if name <> "AltCover" then
      try
        printfn "**Connecting pipe %s ..." trace.Tracer
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
    Connect Token trace
#endif
#endif