// Based upon C# code by Sergiy Sakharov (sakharov@gmail.com)
// http://code.google.com/p/dot-net-coverage/source/browse/trunk/Coverage.Counter/Coverage.Counter.csproj

namespace AltCover.Recorder

open System
open System.Collections.Generic
open System.Reflection
open System.Resources
open System.Runtime.CompilerServices

[<System.Runtime.InteropServices.ProgIdAttribute("ExcludeFromCodeCoverage hack for OpenCover issue 615")>]
type internal Close =
    | DomainUnload
    | ProcessExit

[<System.Runtime.InteropServices.ProgIdAttribute("ExcludeFromCodeCoverage hack for OpenCover issue 615")>]
type internal Carrier =
    | SequencePoint of String*int*Track

[<System.Runtime.InteropServices.ProgIdAttribute("ExcludeFromCodeCoverage hack for OpenCover issue 615")>]
type internal Message =
    | AsyncItem of Carrier
    | Item of Carrier*AsyncReplyChannel<unit>
    | Finish of Close * AsyncReplyChannel<unit>

module Instance =

  // Can't hard-code what with .net-core and .net-core tests as well as classic .net
  // all giving this a different namespace
  let private resource = Assembly.GetExecutingAssembly().GetManifestResourceNames()
                         |> Seq.map (fun s -> s.Substring(0, s.Length - 10)) // trim ".resources"
                         |> Seq.find (fun n -> n.EndsWith("Strings", StringComparison.Ordinal))
  let internal resources = ResourceManager(resource , Assembly.GetExecutingAssembly())

  let GetResource s =
    [
      System.Globalization.CultureInfo.CurrentUICulture.Name
      System.Globalization.CultureInfo.CurrentUICulture.Parent.Name
      "en"
    ]
    |> Seq.map (fun l -> resources.GetString(s + "." + l))
    |> Seq.tryFind (String.IsNullOrEmpty >> not)

  /// <summary>
  /// Gets the location of coverage xml file
  /// This property's IL code is modified to store actual file location
  /// </summary>
  [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
  let ReportFile = "Coverage.Default.xml"

  /// <summary>
  /// Accumulation of visit records
  /// </summary>
  let internal Visits = new Dictionary<string, Dictionary<int, int * Track list>>();

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
  let CoverageFormat = ReportFormat.NCover

  /// <summary>
  /// Serialize access to the report file across AppDomains for the classic mode
  /// </summary>
  let internal mutex = new System.Threading.Mutex(false, Token + ".mutex");

  /// <summary>
  /// Reporting back to the mother-ship
  /// </summary>
  let mutable internal trace = Tracer.Create (ReportFile + ".acv")

  let internal WithMutex (f : bool -> 'a) =
    let own = mutex.WaitOne(1000)
    try
      f(own)
    finally
      if own then mutex.ReleaseMutex()

  /// <summary>
  /// This method flushes hit count buffers.
  /// </summary>
  let internal FlushCounterImpl _ =
    trace.OnConnected (fun () -> trace.OnFinish Visits)
      (fun () ->
      match Visits.Count with
      | 0 -> ()
      | _ -> let counts = Dictionary<string, Dictionary<int, int * Track list>> Visits
             Visits.Clear()
             WithMutex (fun own ->
                let delta = Counter.DoFlush ignore own counts CoverageFormat ReportFile
                GetResource "Coverage statistics flushing took {0:N} seconds"
                |> Option.iter (fun s -> Console.Out.WriteLine(s, delta.TotalSeconds))
             ))

  let internal TraceVisit moduleId hitPointId context =
     trace.OnVisit Visits moduleId hitPointId context

  /// <summary>
  /// This method is executed from instrumented assemblies.
  /// </summary>
  /// <param name="moduleId">Assembly being visited</param>
  /// <param name="hitPointId">Sequence Point identifier</param>
  let internal VisitImpl moduleId hitPointId context =
    if not <| String.IsNullOrEmpty(moduleId) then
      trace.OnConnected (fun () -> TraceVisit moduleId hitPointId context)
                        (fun () -> Counter.AddVisit Visits moduleId hitPointId context)

  let rec private loop (inbox:MailboxProcessor<Message>) =
          async {
             // release the wait every half second
             let! opt = inbox.TryReceive(500)
             match opt with
             | None -> return! loop inbox
             | Some msg ->
                 match msg with
                 | AsyncItem (SequencePoint (moduleId, hitPointId, context)) ->
                     VisitImpl moduleId hitPointId context
                     return! loop inbox
                 | Item (SequencePoint (moduleId, hitPointId, context), channel)->
                     VisitImpl moduleId hitPointId context
                     channel.Reply ()
                     return! loop inbox
                 | Finish (mode, channel) ->
                     FlushCounterImpl mode
                     channel.Reply ()
          }

  let internal MakeMailbox () =
    new MailboxProcessor<Message>(loop)

  let mutable internal mailbox = MakeMailbox ()

  let internal Backlog () =
    mailbox.CurrentQueueLength

  let internal VisitSelection (f: unit -> bool) (g: unit -> Track) moduleId hitPointId =
    // When writing to file for the runner to process,
    // make this semi-synchronous to avoid choking the mailbox
    // Backlogs of over 90,000 items were observed in self-test
    // which failed to drain during the ProcessExit grace period
    // when sending only async messages.
    let message = SequencePoint (moduleId, hitPointId, g())
    if f() then
       mailbox.TryPostAndReply ((fun c -> Item (message, c)), 10) |> ignore
    else message |> AsyncItem |> mailbox.Post

  let Visit moduleId hitPointId =
     VisitSelection (fun () -> trace.IsConnected() || Backlog() > 10)
                    (fun () -> Null) // update iff runner file exists and opencover and option set
                     moduleId hitPointId

  let internal FlushCounter (finish:Close) _ =
    mailbox.PostAndReply (fun c -> Finish (finish, c))

  // unit test helpers -- avoid issues with cross CLR version calls
  let internal RunMailbox () =
    mailbox <- MakeMailbox ()
    mailbox.Start()

  // Register event handling
  do
    AppDomain.CurrentDomain.DomainUnload.Add(FlushCounter DomainUnload)
    AppDomain.CurrentDomain.ProcessExit.Add(FlushCounter ProcessExit)
    WithMutex (fun _ -> trace <- trace.OnStart ())
    mailbox.Start()