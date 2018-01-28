namespace AltCover

open System
open System.Collections.Generic
open System.IO
open System.Threading
open System.Threading.Tasks

open Mono.Cecil
open Mono.Options
open Augment

[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
type Tracer = { Tracer : string }

module Runner =

  let mutable internal recordingDirectory : Option<string> = None
  let mutable internal workingDirectory : Option<string> = None
  let mutable internal executable : Option<string> ref = ref None

  let internal DeclareOptions () =
    [ ("r|recorderDirectory=",
       (fun x -> if not (String.IsNullOrWhiteSpace(x)) && Directory.Exists(x) then
                    if Option.isSome recordingDirectory then
                      CommandLine.error <- true
                    else
                      recordingDirectory <- Some (Path.GetFullPath x)
                 else CommandLine.error <- true))
      ("w|workingDirectory=",
       (fun x -> if not (String.IsNullOrWhiteSpace(x)) && Directory.Exists(x) then
                    if Option.isSome workingDirectory then
                      CommandLine.error <- true
                    else
                      workingDirectory <- Some (Path.GetFullPath x)
                 else CommandLine.error <- true))
      ("x|executable=",
       (fun x -> if not (String.IsNullOrWhiteSpace(x)) then
                    if Option.isSome !executable then
                      CommandLine.error <- true
                    else
                      executable := Some x
                 else CommandLine.error <- true))
      ("?|help|h", (fun x -> CommandLine.help <- not (isNull x)))
      ("<>", (fun x -> CommandLine.error <- true))         ]// default end stop
      |> List.fold (fun (o:OptionSet) (p, a) -> o.Add(p, CommandLine.resources.GetString(p), new System.Action<string>(a))) (OptionSet())

  let HandleBadArguments arguments intro options =
        String.Join (" ", arguments |> Seq.map (sprintf "%A"))
        |> CommandLine.WriteErr
        CommandLine.Usage intro options

  let internal RequireExe (parse:(Either<string*OptionSet, string list*OptionSet>)) =
    match parse with
    | Right (l, options) -> match !executable with
                            | None -> Left ("UsageError", options)
                            | Some exe -> Right (exe::l, options)
    | fail -> fail

  let internal RequireRecorder (parse:(Either<string*OptionSet, string list*OptionSet>)) =
    match parse with
    | Right (_, options) -> match recordingDirectory with
                            | None -> Left ("UsageError", options)
                            | Some path -> let dll = Path.Combine (path, "AltCover.Recorder.g.dll")
                                           if File.Exists dll then parse
                                           else Left ("UsageError", options)
    | fail -> fail

  let internal RequireWorker (parse:(Either<string*OptionSet, string list*OptionSet>)) =
    match parse with
    | Right _ -> match workingDirectory with
                 | None -> workingDirectory <- Directory.GetCurrentDirectory() |> Some
                 | _ -> ()
                 parse
    | fail -> fail

  // mocking point
  let mutable internal RecorderName = "AltCover.Recorder.g.dll"

  let RecorderInstance () =
    let recorderPath = Path.Combine (Option.get recordingDirectory, RecorderName)
    let definition = AssemblyDefinition.ReadAssembly recorderPath
    definition.MainModule.GetType("AltCover.Recorder.Instance")

  let GetMethod (t:TypeDefinition) (name:string) =
    t.Methods
    |> Seq.filter (fun m -> m.Name = name)
    |> Seq.head

  let GetFirstOperandAsString (m:MethodDefinition) =
     m.Body.Instructions
     |> Seq.filter (fun i -> i.OpCode = Cil.OpCodes.Ldstr)
     |> Seq.map (fun i -> i.Operand :?> string)
     |> Seq.head

  let PayloadBase (rest:string list) (latch:EventWaitHandle) =
    async {
            latch.WaitOne(10000) |> ignore
            CommandLine.doPathOperation (fun () ->
                CommandLine.ProcessTrailingArguments rest (DirectoryInfo(Option.get workingDirectory)))
          }

  let MonitorBase (hits:ICollection<(string*int)>) token  (latch:EventWaitHandle) =
    async {
      use server = new System.IO.Pipes.NamedPipeServerStream(token)
      let formatter = System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()
      latch.Set() |> ignore
      server.WaitForConnection()
      server.WriteByte(0uy)
      let rec sink () =
          // On Mono/Linux, we get no clue from the pipe that the other end has gone away
          // Not even a serialization error from a partial message
          // Async didn't seem to play well with the blocking read within Deserialize
          // So do it this way, where we do seem to get a timeout
          let task = Task.Run(fun () -> try
                                          formatter.Deserialize(server) :?> (string*int)
                                        with
                                        | :? System.Runtime.Serialization.SerializationException as x ->
                                            (String.Empty, -1) )
          if task.Wait(10000) then
            let result = task.Result
            if result|> fst |> String.IsNullOrWhiteSpace  |> not then
               result |> hits.Add
               sink()
      sink()
           }

  let WriteReportBase (hits:ICollection<(string*int)>) report =
    let counts = Dictionary<string, Dictionary<int, int>>()
    hits |> Seq.iter(fun (moduleId, hitPointId) ->
                        AltCover.Base.Counter.AddVisit counts moduleId hitPointId)
    AltCover.Base.Counter.DoFlush true counts report

  // mocking points
  let mutable internal GetPayload = PayloadBase
  let mutable internal GetMonitor = MonitorBase
  let mutable internal DoReport = WriteReportBase

  let DoCoverage arguments =
    let check1 = DeclareOptions ()
                 |> CommandLine.ParseCommandLine arguments
                 |> CommandLine.ProcessHelpOption
                 |> RequireExe
                 |> RequireRecorder
                 |> RequireWorker
    match check1 with
    | Left (intro, options) -> HandleBadArguments arguments intro options
    | Right (rest, _) ->
          let instance = RecorderInstance()
          let token = (GetMethod instance "get_Token") |> GetFirstOperandAsString
          let report = (GetMethod instance "get_ReportFile") |> GetFirstOperandAsString
          use latch = new ManualResetEvent false

          let hits = List<(string*int)>()

          let payload = GetPayload rest latch
          let monitor = GetMonitor hits token latch

          [monitor; payload]
          |> Async.Parallel
          |> Async.RunSynchronously
          |> ignore

          DoReport hits report

  [<EntryPoint>]
  let private Main arguments =
    DoCoverage arguments
    0