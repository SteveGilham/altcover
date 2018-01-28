namespace Tests.Runner

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Text
open System.Threading
open System.Threading.Tasks
open System.Xml

open AltCover
open AltCover.Augment
open AltCover.Base
open NUnit.Framework

[<TestFixture>]
type AltCoverTests() = class

  // Augment.fs

  [<Test>]
  member self.AugmentNullableDetectNulls() =
    let input = [ "string"; null; "another string" ]
    let nulls = input |> Seq.map (Option.nullable >> Option.isNone)
    Assert.That(nulls, Is.EquivalentTo([false; true; false]))

  [<Test>]
  member self.AugmentGetOrElseFillsInNone() =
    let input = [ "string"; null; "another string" ]
    let strings = input |> Seq.map (Option.nullable >> (Option.getOrElse "fallback"))
    Assert.That(strings, Is.EquivalentTo([ "string"; "fallback"; "another string" ]))

  // CommandLine.fs

  [<Test>]
  member self.NoThrowNoErrorLeavesAllOK () =
    try
      CommandLine.error <- false
      CommandLine.doPathOperation ignore
      Assert.That(CommandLine.error, Is.False)
    finally
      CommandLine.error <- false

  [<Test>]
  member self.NoThrowWithErrorIsSignalled () =
    try
      CommandLine.error <- false
      CommandLine.doPathOperation (fun () -> CommandLine.error <- true)
      Assert.That(CommandLine.error, Is.True)
    finally
      CommandLine.error <- false

  [<Test>]
  member self.ArgumentExceptionWrites () =
    let saved = (Console.Out, Console.Error)
    try
      use stdout = new StringWriter()
      use stderr = new StringWriter()
      Console.SetOut stdout
      Console.SetError stderr
      let unique = "ArgumentException " + Guid.NewGuid().ToString()

      CommandLine.error <- false
      CommandLine.doPathOperation (fun () -> ArgumentException(unique) |> raise)
      Assert.That(CommandLine.error, Is.True)
      Assert.That(stdout.ToString(), Is.Empty)
      let result = stderr.ToString()
      Assert.That(result, Is.EqualTo (unique + Environment.NewLine))
    finally
      CommandLine.error <- false
      Console.SetOut (fst saved)
      Console.SetError (snd saved)

  [<Test>]
  member self.IOExceptionWrites () =
    let saved = (Console.Out, Console.Error)
    try
      use stdout = new StringWriter()
      use stderr = new StringWriter()
      Console.SetOut stdout
      Console.SetError stderr
      let unique = "IOException " + Guid.NewGuid().ToString()

      CommandLine.error <- false
      CommandLine.doPathOperation (fun () -> IOException(unique) |> raise)
      Assert.That(CommandLine.error, Is.True)
      Assert.That(stdout.ToString(), Is.Empty)
      let result = stderr.ToString()
      Assert.That(result, Is.EqualTo (unique + Environment.NewLine))
    finally
      CommandLine.error <- false
      Console.SetOut (fst saved)
      Console.SetError (snd saved)

  [<Test>]
  member self.NotSupportedExceptionWrites () =
    let saved = (Console.Out, Console.Error)
    try
      use stdout = new StringWriter()
      use stderr = new StringWriter()
      Console.SetOut stdout
      Console.SetError stderr
      let unique = "NotSupportedException " + Guid.NewGuid().ToString()

      CommandLine.error <- false
      CommandLine.doPathOperation (fun () -> NotSupportedException(unique) |> raise)
      Assert.That(CommandLine.error, Is.True)
      Assert.That(stdout.ToString(), Is.Empty)
      let result = stderr.ToString()
      Assert.That(result, Is.EqualTo (unique + Environment.NewLine))
    finally
      CommandLine.error <- false
      Console.SetOut (fst saved)
      Console.SetError (snd saved)

  [<Test>]
  member self.SecurityExceptionWrites () =
    let saved = (Console.Out, Console.Error)
    try
      use stdout = new StringWriter()
      use stderr = new StringWriter()
      Console.SetOut stdout
      Console.SetError stderr
      let unique = "SecurityException " + Guid.NewGuid().ToString()

      CommandLine.error <- false
      CommandLine.doPathOperation (fun () -> System.Security.SecurityException(unique) |> raise)
      Assert.That(CommandLine.error, Is.True)
      Assert.That(stdout.ToString(), Is.Empty)
      let result = stderr.ToString()
      Assert.That(result, Is.EqualTo (unique + Environment.NewLine))
    finally
      CommandLine.error <- false
      Console.SetOut (fst saved)
      Console.SetError (snd saved)

  // Base.fs

  [<Test>]
  member self.ShouldBeExecutingTheCorrectCopyOfThisCode() =
    let mutable where = ""
    Locking.WithLockerLocked self (fun () -> where <- Assembly.GetCallingAssembly().GetName().Name)
    Assert.That(where, Is.EqualTo "AltCover.Runner")

  [<Test>]
  member self.RealIdShouldIncrementCount() =
    let visits = new Dictionary<string, Dictionary<int, int>>()
    let key = " "
    Counter.AddVisit visits key  23
    Assert.That (visits.Count, Is.EqualTo 1)
    Assert.That (visits.[key].Count, Is.EqualTo 1)
    Assert.That (visits.[key].[23], Is.EqualTo 1)

  [<Test>]
  member self.DistinctIdShouldBeDistinct() =
    let visits = new Dictionary<string, Dictionary<int, int>>()
    let key = " "
    Counter.AddVisit visits key 23
    Counter.AddVisit visits "key" 42
    Assert.That (visits.Count, Is.EqualTo 2)

  [<Test>]
  member self.DistinctLineShouldBeDistinct() =
    let visits = new Dictionary<string, Dictionary<int, int>>()
    let key = " "
    Counter.AddVisit visits key 23
    Counter.AddVisit visits key 42
    Assert.That (visits.Count, Is.EqualTo 1)
    Assert.That (visits.[key].Count, Is.EqualTo 2)

  [<Test>]
  member self.RepeatVisitsShouldIncrementCount() =
    let visits = new Dictionary<string, Dictionary<int, int>>()
    let key = " "
    Counter.AddVisit visits key 23
    Counter.AddVisit visits key 23
    Assert.That (visits.[key].[23], Is.EqualTo 2)

  member self.resource = Assembly.GetExecutingAssembly().GetManifestResourceNames()
                         |> Seq.find (fun n -> n.EndsWith("SimpleCoverage.xml", StringComparison.Ordinal))

  [<Test>]
  member self.FlushLeavesExpectedTraces() =
    let saved = Console.Out
    let here = Directory.GetCurrentDirectory()
    let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
    let unique = Path.Combine(where, Guid.NewGuid().ToString())
    let reportFile = Path.Combine(unique, "FlushLeavesExpectedTraces.xml")
    try
      let visits = new Dictionary<string, Dictionary<int, int>>()
      use stdout = new StringWriter()
      Console.SetOut stdout
      Directory.CreateDirectory(unique) |> ignore
      Directory.SetCurrentDirectory(unique)

      Counter.measureTime <- DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)
      use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(self.resource)
      let size = int stream.Length
      let buffer = Array.create size 0uy
      Assert.That (stream.Read(buffer, 0, size), Is.EqualTo size)
      do
        use worker = new FileStream(reportFile, FileMode.CreateNew)
        worker.Write(buffer, 0, size)
        ()

      let payload = Dictionary<int,int>()
      [0..9 ]
      |> Seq.iter(fun i -> payload.[i] <- (i+1))
      visits.["f6e3edb3-fb20-44b3-817d-f69d1a22fc2f"] <- payload

      Counter.DoFlush true visits reportFile

      let head = "Coverage statistics flushing took "
      let tail = " seconds\n"
      let recorded = stdout.ToString().Replace("\r\n","\n")
      Assert.That (recorded.StartsWith(head, StringComparison.Ordinal))
      Assert.That (recorded.EndsWith(tail, StringComparison.Ordinal))
      use worker' = new FileStream(reportFile, FileMode.Open)
      let after = XmlDocument()
      after.Load worker'
      Assert.That( after.SelectNodes("//seqpnt")
                   |> Seq.cast<XmlElement>
                   |> Seq.map (fun x -> x.GetAttribute("visitcount")),
                   Is.EquivalentTo [ "11"; "10"; "9"; "8"; "7"; "6"; "4"; "3"; "2"; "1"])
    finally
      if File.Exists reportFile then File.Delete reportFile
      Console.SetOut saved
      Directory.SetCurrentDirectory(here)
      try
        Directory.Delete(unique)
      with
      | :? IOException -> ()

  // Runner.fs and CommandLine.fs

  [<Test>]
  member self.UsageIsAsExpected() =
    let options = Runner.DeclareOptions ()
    let saved = Console.Error

    try
      use stderr = new StringWriter()
      Console.SetError stderr
      CommandLine.Usage "UsageError" options
      let result = stderr.ToString().Replace("\r\n", "\n")
      let expected = """Error - usage is:
  -r, --recorderDirectory=VALUE
                             The folder containing the instrumented code to
                               monitor (including the AltCover.Recorder.g.dll
                               generated by previous a use of the .net core
                               AltCover).
  -w, --workingDirectory=VALUE
                             Optional: The working directory for the
                               application launch
  -x, --executable=VALUE     The executable to run e.g. dotnet
  -?, --help, -h             Prints out the options.
"""

      Assert.That (result, Is.EqualTo (expected.Replace("\r\n", "\n")), "*" + result + "*")

    finally Console.SetError saved

  [<Test>]
  member self.ShouldLaunchWithExpectedOutput() =
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), "_Mono/Sample1")
#if NETCOREAPP2_0
    let path' = if Directory.Exists path then path
                else Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), "../_Mono/Sample1")
#else
    let path' = path
#endif
    let files = Directory.GetFiles(path')
    let program = files
                  |> Seq.filter (fun x -> x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
                  |> Seq.head

    let saved = (Console.Out, Console.Error)
    try
      use stdout = new StringWriter()
      use stderr = new StringWriter()
      Console.SetOut stdout
      Console.SetError stderr

      CommandLine.Launch program (String.Empty) (Path.GetDirectoryName (Assembly.GetExecutingAssembly().Location))

      Assert.That(stderr.ToString(), Is.Empty)
      let result = stdout.ToString()
      // hack for Mono
      let computed = if result.Length = 14 then
                       result |> Encoding.Unicode.GetBytes |> Array.takeWhile (fun c -> c <> 0uy)|> Encoding.UTF8.GetString
                     else result

      if "TRAVIS_JOB_NUMBER" |> Environment.GetEnvironmentVariable |> String.IsNullOrWhiteSpace || result.Length > 0 then
        Assert.That(computed.Trim(), Is.EqualTo("Where is my rocket pack?"))
    finally
      Console.SetOut (fst saved)
      Console.SetError (snd saved)

  [<Test>]
  member self.ShouldHaveExpectedOptions() =
    let options = Runner.DeclareOptions ()
    Assert.That (options.Count, Is.EqualTo 5)
    Assert.That(options |> Seq.filter (fun x -> x.Prototype <> "<>")
                        |> Seq.forall (fun x -> (String.IsNullOrWhiteSpace >> not) x.Description))
    Assert.That (options |> Seq.filter (fun x -> x.Prototype = "<>") |> Seq.length, Is.EqualTo 1)

  [<Test>]
  member self.ParsingJunkIsAnError() =
    let options = Runner.DeclareOptions ()
    let parse = CommandLine.ParseCommandLine [| "/@thisIsNotAnOption" |] options
    match parse with
    | Right _ -> Assert.Fail()
    | Left (x, y) -> Assert.That (x, Is.EqualTo "UsageError")
                     Assert.That (y, Is.SameAs options)

  [<Test>]
  member self.ParsingJunkAfterSeparatorIsExpected() =
    let options = Runner.DeclareOptions ()
    let input = [| "--";  "/@thisIsNotAnOption"; "this should be OK" |]
    let parse = CommandLine.ParseCommandLine input options
    match parse with
    | Left _ -> Assert.Fail()
    | Right (x, y) -> Assert.That (x, Is.EquivalentTo (input |> Seq.skip 1))
                      Assert.That (y, Is.SameAs options)

  [<Test>]
  member self.ParsingHelpGivesHelp() =
    let options = Runner.DeclareOptions ()
    let input = [| "--?" |]
    let parse = CommandLine.ParseCommandLine input options
    match parse with
    | Left _ -> Assert.Fail()
    | Right (x, y) -> Assert.That (y, Is.SameAs options)

    match CommandLine.ProcessHelpOption parse with
    | Right _ -> Assert.Fail()
    | Left (x, y) -> Assert.That (x, Is.EqualTo "HelpText")
                     Assert.That (y, Is.SameAs options)

    // a "not sticky" test
    lock Runner.executable (fun () ->
      Runner.executable := None
      match CommandLine.ParseCommandLine [| "/x"; "x" |] options
            |> CommandLine.ProcessHelpOption with
      | Left _ -> Assert.Fail()
      | Right (x, y) -> Assert.That (y, Is.SameAs options)
                        Assert.That (x, Is.Empty))

  [<Test>]
  member self.ParsingErrorHelpGivesHelp() =
    let options = Runner.DeclareOptions ()
    let input = [| "--o"; Path.GetInvalidPathChars() |> String |]
    let parse = CommandLine.ParseCommandLine input options
    match parse with
    | Right _ -> Assert.Fail()
    | Left (x, y) -> Assert.That (x, Is.EqualTo "UsageError")
                     Assert.That (y, Is.SameAs options)

    match CommandLine.ProcessHelpOption parse with
    | Right _ -> Assert.Fail()
    | Left (x, y) -> Assert.That (x, Is.EqualTo "UsageError")
                     Assert.That (y, Is.SameAs options)

    // a "not sticky" test
    lock Runner.executable (fun () ->
      Runner.executable := None
      match CommandLine.ParseCommandLine [| "/x"; "x" |] options
            |> CommandLine.ProcessHelpOption with
      | Left _ -> Assert.Fail()
      | Right (x, y) -> Assert.That (y, Is.SameAs options)
                        Assert.That (x, Is.Empty))

  [<Test>]
  member self.ParsingExeGivesExe() =
    lock Runner.executable (fun () ->
    try
      Runner.executable := None
      let options = Runner.DeclareOptions ()
      let unique = "some exe"
      let input = [| "-x"; unique |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Left _ -> Assert.Fail()
      | Right (x, y) -> Assert.That (y, Is.SameAs options)
                        Assert.That (x, Is.Empty)

      match !Runner.executable with
      | None -> Assert.Fail()
      | Some x -> Assert.That(Path.GetFileName x, Is.EqualTo unique)
    finally
      Runner.executable := None)

  [<Test>]
  member self.ParsingMultipleExeGivesFailure() =
    lock Runner.executable (fun () ->
    try
      Runner.executable := None
      let options = Runner.DeclareOptions ()
      let unique = Guid.NewGuid().ToString()
      let input = [| "-x"; unique; "/x"; unique.Replace("-", "+") |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Runner.executable := None)

  [<Test>]
  member self.ParsingNoExeGivesFailure() =
    lock Runner.executable (fun () ->
    try
      Runner.executable := None
      let options = Runner.DeclareOptions ()
      let blank = " "
      let input = [| "-x"; blank; |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Runner.executable := None)

  [<Test>]
  member self.ParsingWorkerGivesWorker() =
    try
      Runner.workingDirectory <- None
      let options = Runner.DeclareOptions ()
      let unique = Path.GetFullPath(".")
      let input = [| "-w"; unique |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Left _ -> Assert.Fail()
      | Right (x, y) -> Assert.That (y, Is.SameAs options)
                        Assert.That (x, Is.Empty)

      match Runner.workingDirectory with
      | None -> Assert.Fail()
      | Some x -> Assert.That(x, Is.EqualTo unique)
    finally
      Runner.workingDirectory <- None

  [<Test>]
  member self.ParsingMultipleWorkerGivesFailure() =
    try
      Runner.workingDirectory <- None
      let options = Runner.DeclareOptions ()
      let input = [| "-w"; Path.GetFullPath("."); "/w"; Path.GetFullPath("..") |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Runner.workingDirectory <- None

  [<Test>]
  member self.ParsingBadWorkerGivesFailure() =
    try
      Runner.workingDirectory <- None
      let options = Runner.DeclareOptions ()
      let unique = Guid.NewGuid().ToString().Replace("-", "*")
      let input = [| "-w"; unique |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Runner.workingDirectory <- None

  [<Test>]
  member self.ParsingNoWorkerGivesFailure() =
    try
      Runner.workingDirectory <- None
      let options = Runner.DeclareOptions ()
      let input = [| "-w" |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Runner.workingDirectory <- None

  [<Test>]
  member self.ParsingRecorderGivesRecorder() =
    try
      Runner.recordingDirectory <- None
      let options = Runner.DeclareOptions ()
      let unique = Path.GetFullPath(".")
      let input = [| "-r"; unique |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Left _ -> Assert.Fail()
      | Right (x, y) -> Assert.That (y, Is.SameAs options)
                        Assert.That (x, Is.Empty)

      match Runner.recordingDirectory with
      | None -> Assert.Fail()
      | Some x -> Assert.That(x, Is.EqualTo unique)
    finally
      Runner.recordingDirectory <- None

  [<Test>]
  member self.ParsingMultipleRecorderGivesFailure() =
    try
      Runner.recordingDirectory <- None
      let options = Runner.DeclareOptions ()
      let input = [| "-r"; Path.GetFullPath("."); "/r"; Path.GetFullPath("..") |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Runner.recordingDirectory <- None

  [<Test>]
  member self.ParsingBadRecorderGivesFailure() =
    try
      Runner.recordingDirectory <- None
      let options = Runner.DeclareOptions ()
      let unique = Guid.NewGuid().ToString().Replace("-", "*")
      let input = [| "-r"; unique |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Runner.recordingDirectory <- None

  [<Test>]
  member self.ParsingNoRecorderGivesFailure() =
    try
      Runner.recordingDirectory <- None
      let options = Runner.DeclareOptions ()
      let input = [| "-r" |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Runner.recordingDirectory <- None

  [<Test>]
  member self.ShouldRequireExe() =
    lock Runner.executable (fun () ->
    try
      Runner.executable := None
      let options = Runner.DeclareOptions ()
      let parse = Runner.RequireExe (Right ([], options))
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Runner.executable := None)

  [<Test>]
  member self.ShouldAcceptExe() =
    lock Runner.executable (fun () ->
    try
      Runner.executable := Some "xxx"
      let options = Runner.DeclareOptions ()
      let parse = Runner.RequireExe (Right (["b"], options))
      match parse with
      | Right (x::y, z) -> Assert.That (z, Is.SameAs options)
                           Assert.That (x, Is.EqualTo "xxx")
                           Assert.That (y, Is.EquivalentTo ["b"])
      | _ -> Assert.Fail()
    finally
      Runner.executable := None)

  [<Test>]
  member self.ShouldRequireWorker() =
    try
      Runner.workingDirectory <- None
      let options = Runner.DeclareOptions ()
      let input = (Right ([], options))
      let parse = Runner.RequireWorker input
      match parse with
      | Right _ -> Assert.That(parse, Is.SameAs input)
                   Assert.That(Option.isSome Runner.workingDirectory)
      | _-> Assert.Fail()
    finally
      Runner.workingDirectory <- None

  [<Test>]
  member self.ShouldAcceptWorker() =
    try
      Runner.workingDirectory <- Some "ShouldAcceptWorker"
      let options = Runner.DeclareOptions ()
      let input = (Right ([], options))
      let parse = Runner.RequireWorker input
      match parse with
      | Right _ -> Assert.That(parse, Is.SameAs input)
                   Assert.That(Runner.workingDirectory,
                               Is.EqualTo (Some "ShouldAcceptWorker"))
      | _-> Assert.Fail()
    finally
      Runner.workingDirectory <- None

  [<Test>]
  member self.ShouldRequireRecorder() =
    try
      Runner.recordingDirectory <- None
      let options = Runner.DeclareOptions ()
      let input = (Right ([], options))
      let parse = Runner.RequireRecorder input
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Runner.recordingDirectory <- None

  [<Test>]
  member self.ShouldRequireRecorderDll() =
    try
      let where = Assembly.GetExecutingAssembly().Location
      let path = Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), "_Mono/Sample1")
      let path' = if Directory.Exists path then path
                  else Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), "../_Mono/Sample1")
      Runner.recordingDirectory <- Some path'
      let options = Runner.DeclareOptions ()
      let input = (Right ([], options))
      let parse = Runner.RequireRecorder input
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Runner.recordingDirectory <- None
  [<Test>]
  member self.ShouldAcceptRecorder() =
    try
      let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
      Runner.recordingDirectory <- Some where
      let create = Path.Combine(where, "AltCover.Recorder.g.dll")
      if create |> File.Exists |> not then do
        let from = Path.Combine(where, "AltCover.Recorder.dll")
        use frombytes = new FileStream(from, FileMode.Open, FileAccess.Read)
        use libstream = new FileStream(create, FileMode.Create)
        frombytes.CopyTo libstream

      let options = Runner.DeclareOptions ()
      let input = (Right ([], options))
      let parse = Runner.RequireRecorder input
      match parse with
      | Right _ -> Assert.That(parse, Is.SameAs input)
      | _-> Assert.Fail()
    finally
      Runner.recordingDirectory <- None

  [<Test>]
  member self.ShouldProcessTrailingArguments() =
    // Hack for running while instrumented
    let where = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    let path = Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), "_Mono/Sample1")
#if NETCOREAPP2_0
    let path' = if Directory.Exists path then path
                else Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), "../_Mono/Sample1")
#else
    let path' = path
#endif
    let files = Directory.GetFiles(path')
    let program = files
                  |> Seq.filter (fun x -> x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
                  |> Seq.head

    let saved = (Console.Out, Console.Error)
    try
      use stdout = new StringWriter()
      use stderr = new StringWriter()
      Console.SetOut stdout
      Console.SetError stderr

      let u1 = Guid.NewGuid().ToString()
      let u2 = Guid.NewGuid().ToString()

      CommandLine.ProcessTrailingArguments [program; u1; u2]
                                     (DirectoryInfo(where))

      Assert.That(stderr.ToString(), Is.Empty)
      stdout.Flush()
      let result = stdout.ToString()

      // hack for Mono
      let computed = if result.Length = 50 then
                       result |> Encoding.Unicode.GetBytes |> Array.takeWhile (fun c -> c <> 0uy)|> Encoding.UTF8.GetString
                     else result
      if "TRAVIS_JOB_NUMBER" |> Environment.GetEnvironmentVariable |> String.IsNullOrWhiteSpace || result.Length > 0 then
        Assert.That(computed.Trim(), Is.EqualTo("Where is my rocket pack? " +
                                                  u1 + "*" + u2))
    finally
      Console.SetOut (fst saved)
      Console.SetError (snd saved)

  [<Test>]
  member self.ShouldNoOp() =
    let where = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    CommandLine.ProcessTrailingArguments [] (DirectoryInfo(where))
    Assert.Pass()

  [<Test>]
  member self.ErrorResponseIsAsExpected() =
    let saved = Console.Error
    try
      use stderr = new StringWriter()
      Console.SetError stderr
      let unique = Guid.NewGuid().ToString()
      let main = typeof<Tracer>.Assembly.GetType("AltCover.Runner").GetMethod("Main", BindingFlags.NonPublic ||| BindingFlags.Static)
      let returnCode = main.Invoke(null, [| [| "-r"; unique |] |])
      Assert.That(returnCode, Is.EqualTo 0)
      let result = stderr.ToString().Replace("\r\n", "\n")
      let expected = "\"-r\" \"" + unique + "\"\n" +
                       """Error - usage is:
  -r, --recorderDirectory=VALUE
                             The folder containing the instrumented code to
                               monitor (including the AltCover.Recorder.g.dll
                               generated by previous a use of the .net core
                               AltCover).
  -w, --workingDirectory=VALUE
                             Optional: The working directory for the
                               application launch
  -x, --executable=VALUE     The executable to run e.g. dotnet
  -?, --help, -h             Prints out the options.
"""

      Assert.That (result, Is.EqualTo (expected.Replace("\r\n", "\n")))

    finally Console.SetError saved

  [<Test>]
  member self.ShouldGetStringConstants() =
    let where = Assembly.GetExecutingAssembly().Location
                |> Path.GetDirectoryName
    let save = Runner.RecorderName
    lock self (fun () ->
    try
      Runner.recordingDirectory <- Some where
      Runner.RecorderName <- "AltCover.Recorder.dll"
      let instance = Runner.RecorderInstance()
      Assert.That(instance.FullName, Is.EqualTo "AltCover.Recorder.Instance", "should be the instance")
      let token = (Runner.GetMethod instance "get_Token") |> Runner.GetFirstOperandAsString
      Assert.That(token, Is.EqualTo "AltCover", "should be plain token")
      let report = (Runner.GetMethod instance "get_ReportFile") |> Runner.GetFirstOperandAsString
      Assert.That(report, Is.EqualTo "Coverage.Default.xml", "should be default coverage file")

    finally
      Runner.recordingDirectory <- None
      Runner.RecorderName <- save)

  [<Test>]
  member self.ShouldProcessPayload() =
    // Hack for running while instrumented
    let where = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    let path = Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), "_Mono/Sample1")
#if NETCOREAPP2_0
    let path' = if Directory.Exists path then path
                else Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), "../_Mono/Sample1")
#else
    let path' = path
#endif
    let files = Directory.GetFiles(path')
    let program = files
                  |> Seq.filter (fun x -> x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
                  |> Seq.head

    let saved = (Console.Out, Console.Error)
    Runner.workingDirectory <- Some where
    try
      use stdout = new StringWriter()
      use stderr = new StringWriter()
      Console.SetOut stdout
      Console.SetError stderr

      let u1 = Guid.NewGuid().ToString()
      let u2 = Guid.NewGuid().ToString()
      use latch = new ManualResetEvent true

      let payload = Runner.GetPayload [program; u1; u2] latch
      payload |> Async.RunSynchronously

      Assert.That(stderr.ToString(), Is.Empty)
      stdout.Flush()
      let result = stdout.ToString()

      // hack for Mono
      let computed = if result.Length = 50 then
                       result |> Encoding.Unicode.GetBytes |> Array.takeWhile (fun c -> c <> 0uy)|> Encoding.UTF8.GetString
                     else result
      if "TRAVIS_JOB_NUMBER" |> Environment.GetEnvironmentVariable |> String.IsNullOrWhiteSpace || result.Length > 0 then
        Assert.That(computed.Trim(), Is.EqualTo("Where is my rocket pack? " +
                                                  u1 + "*" + u2))
    finally
      Console.SetOut (fst saved)
      Console.SetError (snd saved)
      Runner.workingDirectory <- None

  [<Test>]
  member self.ShouldDoCoverage() =
    let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
    let create = Path.Combine(where, "AltCover.Recorder.g.dll")
    if create |> File.Exists |> not then do
        let from = Path.Combine(where, "AltCover.Recorder.dll")
        use frombytes = new FileStream(from, FileMode.Open, FileAccess.Read)
        use libstream = new FileStream(create, FileMode.Create)
        frombytes.CopyTo libstream

    let save = Runner.RecorderName
    let save1 = Runner.GetPayload
    let save2 = Runner.GetMonitor
    let save3 = Runner.DoReport
    try
      Runner.RecorderName <- "AltCover.Recorder.dll"
      let payload (rest:string list) _ =
        Assert.That(rest, Is.EquivalentTo [|"test"; "1"|])
        async { () }

      let monitor (hits:ICollection<(string*int)>) (token:string) _ =
        Assert.That(token, Is.EqualTo "AltCover", "should be plain token")
        Assert.That(hits, Is.Empty)
        async { () }

      let write (hits:ICollection<(string*int)>) (report:string) =
        Assert.That(report, Is.EqualTo "Coverage.Default.xml", "should be default coverage file")
        Assert.That(hits, Is.Empty)

      Runner.GetPayload <- payload
      Runner.GetMonitor <- monitor
      Runner.DoReport <- write

      Runner.DoCoverage [|"-x"; "test"; "-r"; where; "--"; "1"|]

    finally
      Runner.GetPayload <- save1
      Runner.GetMonitor <- save2
      Runner.DoReport <- save3
      Runner.RecorderName <- save

  [<Test>]
  member self.WriteLeavesExpectedTraces() =
    let saved = Console.Out
    let here = Directory.GetCurrentDirectory()
    let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
    let unique = Path.Combine(where, Guid.NewGuid().ToString())
    let reportFile = Path.Combine(unique, "FlushLeavesExpectedTraces.xml")
    try
      let visits = new Dictionary<string, Dictionary<int, int>>()
      use stdout = new StringWriter()
      Console.SetOut stdout
      Directory.CreateDirectory(unique) |> ignore
      Directory.SetCurrentDirectory(unique)

      Counter.measureTime <- DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)
      use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(self.resource)
      let size = int stream.Length
      let buffer = Array.create size 0uy
      Assert.That (stream.Read(buffer, 0, size), Is.EqualTo size)
      do
        use worker = new FileStream(reportFile, FileMode.CreateNew)
        worker.Write(buffer, 0, size)
        ()

      let hits = List<(string*int)>()
      [0..9 ]
      |> Seq.iter(fun i ->
        for j = 1 to i+1 do
          hits.Add("f6e3edb3-fb20-44b3-817d-f69d1a22fc2f", i)
          ignore j
      )

      let payload = Dictionary<int,int>()
      [0..9 ]
      |> Seq.iter(fun i -> payload.[i] <- (i+1))
      visits.["f6e3edb3-fb20-44b3-817d-f69d1a22fc2f"] <- payload

      Runner.DoReport hits reportFile

      let head = "Coverage statistics flushing took "
      let tail = " seconds\n"
      let recorded = stdout.ToString().Replace("\r\n","\n")
      Assert.That (recorded.StartsWith(head, StringComparison.Ordinal))
      Assert.That (recorded.EndsWith(tail, StringComparison.Ordinal))
      use worker' = new FileStream(reportFile, FileMode.Open)
      let after = XmlDocument()
      after.Load worker'
      Assert.That( after.SelectNodes("//seqpnt")
                   |> Seq.cast<XmlElement>
                   |> Seq.map (fun x -> x.GetAttribute("visitcount")),
                   Is.EquivalentTo [ "11"; "10"; "9"; "8"; "7"; "6"; "4"; "3"; "2"; "1"])
    finally
      if File.Exists reportFile then File.Delete reportFile
      Console.SetOut saved
      Directory.SetCurrentDirectory(here)
      try
        Directory.Delete(unique)
      with
      | :? IOException -> ()

  [<Test>]
  member self.PipeMonitorShouldReceiveSignal() =
    let token = Guid.NewGuid().ToString() + "PipeMonitorShouldReceiveSignal"
    let formatter = System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()
    let hits = List<(string*int)>()
    use signal = new AutoResetEvent false
    use latch = new ManualResetEvent false
    let os = Environment.OSVersion.ToString()

    let task = Task.Run(fun () ->
          use client = new System.IO.Pipes.NamedPipeClientStream(token)
          if os.StartsWith("Microsoft Windows", StringComparison.Ordinal) |> not then
            client.Connect()

          async {
                do! Runner.GetMonitor hits token latch
                printfn "cr: monitor exit"
                do! async { signal.Set() |> ignore }
            } |> Async.Start

          if os.StartsWith("Microsoft Windows", StringComparison.Ordinal) then
            printfn "cr: about to connect"
            client.Connect()
            while client.IsConnected |> not do
              printfn "."
              Thread.Sleep 100

          printfn "cr: connected"
          let x = client.ReadByte()
          Assert.That(x, Is.GreaterThanOrEqualTo 0)
          printfn "cr: active"
          formatter.Serialize(client, ("name", 23))
          client.Flush()
          printfn "cr: tuple sent"
          formatter.Serialize(client, ("name2", 42))
          client.Flush()
          printfn "cr: tuple sent"
          let nulled = { Tracer = null }
          formatter.Serialize(client, (nulled.Tracer, -1))
          client.Flush()
          printfn "cr: termination sent")

    let bigWait = 12000 // 2s more than the timeout in monitorbase
    if task.Wait(bigWait) then
        Assert.That(signal.WaitOne(bigWait), "Went on too long")
        Assert.That(latch.WaitOne(), Is.True, "didn't finish monitoring")
        Assert.That(hits, Is.EquivalentTo [("name", 23); ("name2", 42)])
    else Assert.Fail("Task timeout")

  [<Test>]
  member self.PipeMonitorShouldHandleException() =
    let token = Guid.NewGuid().ToString() + "PipeMonitorShouldHandleException"
    let formatter = System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()
    let hits = List<(string*int)>()
    use signal = new AutoResetEvent false
    use latch = new ManualResetEvent false
    let os = Environment.OSVersion.ToString()

    let task = Task.Run(fun () ->
      use client = new System.IO.Pipes.NamedPipeClientStream(token)
      if os.StartsWith("Microsoft Windows", StringComparison.Ordinal) |> not then
        client.Connect()

      async {
        do! Runner.GetMonitor hits token latch
        printfn "ch: monitor exit"
        do! async { signal.Set() |> ignore }
      } |> Async.Start

      if os.StartsWith("Microsoft Windows", StringComparison.Ordinal) then
        printfn "ch: about to connect"
        client.Connect()
        while client.IsConnected |> not do
          printf "."
          Thread.Sleep 100

      printfn "ch: connected"
      let x = client.ReadByte()
      Assert.That(x, Is.GreaterThanOrEqualTo 0)
      printfn "ch: active"
      formatter.Serialize(client, ("name", 23))
      client.Flush()
      printfn "ch: tuple sent"
      let broken = System.Text.Encoding.UTF8.GetBytes "just junk"
      client.Write(broken, 0, broken.Length)
      client.Flush()
      printfn "ch: junk sent")

    let bigWait = 12000 // 2s more than the timeout in monitorbase
    if task.Wait(bigWait) then
        Assert.That(signal.WaitOne(bigWait), "Went on too long")
        Assert.That(latch.WaitOne(), Is.True, "didn't finish monitoring")
        Assert.That(hits, Is.EquivalentTo [("name", 23)])
    else Assert.Fail("Task timeout")

end