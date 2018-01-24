namespace AltCover

open System
open System.Collections.Generic
open System.IO
open System.Reflection

open Mono.Options
open Augment

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
          use latch = new System.Threading.ManualResetEvent false
          use latch' = new System.Threading.ManualResetEvent false

          let recorderPath = Path.Combine (Option.get recordingDirectory, "AltCover.Recorder.g.dll")
          let recorder = Assembly.LoadFrom recorderPath
          let instanceType = recorder.GetType("AltCover.Recorder.Instance")
          let token = instanceType.GetProperty("Token", BindingFlags.Public ||| BindingFlags.Static).GetValue(null) :?> string
          let report = instanceType.GetProperty("ReportFile", BindingFlags.Public ||| BindingFlags.Static).GetValue(null) :?> string
          use server = new System.IO.Pipes.NamedPipeServerStream(token)
          let formatter = System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()

          let hits = List<(string*int)>()

          let payload = async {
            CommandLine.doPathOperation (fun () ->
              CommandLine.ProcessTrailingArguments rest (DirectoryInfo(Option.get workingDirectory)))
            latch.Set() |> ignore
          }

          let monitor = async {
            printfn "Begun monitoring"
            server.WaitForConnection()
            printfn "connected"
            server.WriteByte(0uy)
            let rec sink () =
              let result = formatter.Deserialize(server) :?> (string*int)
              printfn "%A" result
              if result |> fst |> String.IsNullOrWhiteSpace  |> not then
                hits.Add result
                sink()
            sink()
            latch.WaitOne() |> ignore
            printfn "Done monitoring"
            latch'.Set() |> ignore
           }

          [monitor; payload]
          |> Async.Parallel
          |> Async.RunSynchronously
          |> ignore

          latch'.WaitOne() |> ignore
          let visits = instanceType.GetProperty("Visits", BindingFlags.NonPublic ||| BindingFlags.Static).GetValue(null)
                          :?> Dictionary<string, Dictionary<int, int>>
          let counts = Dictionary<string, Dictionary<int, int>>()

          let visit = instanceType.GetMethod("AddVisit", BindingFlags.Public ||| BindingFlags.Static)
          hits |> Seq.iter(fun (moduleId, hitPointId) ->
                                visit.Invoke(null, [|counts; moduleId; hitPointId|]) |> ignore)
          visits.Clear()

          let update = instanceType.GetMethod("UpdateReport", BindingFlags.NonPublic ||| BindingFlags.Static)
          let measureTime = instanceType.GetProperty("measureTime", BindingFlags.NonPublic ||| BindingFlags.Static)
          measureTime.SetValue(null, DateTime.UtcNow)
          use coverageFile = new FileStream(report, FileMode.Open,
                                 FileAccess.ReadWrite, FileShare.None,
                                 4096, FileOptions.SequentialScan)
          let flushStart = update.Invoke(null, [|counts; coverageFile|]) :?> DateTime
          let delta = TimeSpan(DateTime.UtcNow.Ticks - flushStart.Ticks)
          Console.Out.WriteLine("Coverage statistics flushing took {0:N} seconds", delta.TotalSeconds)

  [<EntryPoint>]
  let private Main arguments =
    DoCoverage arguments
    0