namespace Tests
// fsharplint:disable  MemberNames NonPublicValuesNames RedundantNewKeyword

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.IO
open System.IO.Compression
open System.Reflection
open System.Text.RegularExpressions
open System.Xml
open System.Xml.Linq
open System.Xml.Schema

open AltCover
open AltCover.Augment
open AltCover.Base
open Mono.Options
open Swensen.Unquote

type Assert = NUnit.Framework.Assert
type Does = NUnit.Framework.Does
type Is = NUnit.Framework.Is

#if NETCOREAPP2_1
[<AttributeUsage(AttributeTargets.Method)>]
type TestAttribute() = class
    inherit Attribute()
end
#else
type TestAttribute = NUnit.Framework.TestAttribute
#endif

module AltCoverRunnerTests =
    // Base.fs

    [<Test>]
    let MaxTimeFirst () =
      let now = DateTime.Now
      let ago = now - TimeSpan(1,0,0,0)
      test <@ (Base.Counter.I.maxTime now ago) = now @>

    [<Test>]
    let MaxTimeLast () =
      let now = DateTime.Now
      let ago = now - TimeSpan(1,0,0,0)
      test <@ (Base.Counter.I.maxTime ago now) = now @>

    [<Test>]
    let MinTimeFirst () =
      let now = DateTime.Now
      let ago = now - TimeSpan(1,0,0,0)
      test <@ (Base.Counter.I.minTime ago now) = ago @>

    [<Test>]
    let MinTimeLast () =
      let now = DateTime.Now
      let ago = now - TimeSpan(1,0,0,0)
      test <@ (Base.Counter.I.minTime now ago) = ago @>

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let JunkUspidGivesNegativeIndex() =
      Runner.init()
      let key = " "
      let index = Counter.I.findIndexFromUspid 0 key
      test <@ index < 0 @>

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let RealIdShouldIncrementCount() =
      Runner.init()
      let visits = new Dictionary<string, Dictionary<int, PointVisit>>()
      let key = " "
      let v1 = Counter.addVisit visits key 23 Null
      Assert.That(v1, Is.EqualTo 1)
      Assert.That(visits.Count, Is.EqualTo 1)
      Assert.That(visits.[key].Count, Is.EqualTo 1)
      let x = visits.[key].[23]
      Assert.That(x.Count, Is.EqualTo 1)
      Assert.That(x.Tracks, Is.Empty)

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let RealIdShouldIncrementList() =
      Runner.init()
      let visits = new Dictionary<string, Dictionary<int, PointVisit>>()
      let key = " "
      let payload = Time DateTime.UtcNow.Ticks
      let v2 = Counter.addVisit visits key 23 payload
      Assert.That(v2, Is.EqualTo 1)
      Assert.That(visits.Count, Is.EqualTo 1)
      Assert.That(visits.[key].Count, Is.EqualTo 1)
      let x = visits.[key].[23]
      Assert.That(x.Count, Is.EqualTo 0)
      Assert.That(x.Tracks, Is.EquivalentTo [ payload ])

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let DistinctIdShouldBeDistinct() =
      Runner.init()
      let visits = new Dictionary<string, Dictionary<int, PointVisit>>()
      let key = " "
      let v3 = Counter.addVisit visits key 23 Null
      Assert.That(v3, Is.EqualTo 1)
      let v4 = Counter.addVisit visits "key" 42 Null
      Assert.That(visits.Count, Is.EqualTo 2)
      Assert.That(v4, Is.EqualTo 1)

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let DistinctLineShouldBeDistinct() =
      Runner.init()
      let visits = new Dictionary<string, Dictionary<int, PointVisit>>()
      let key = " "
      let v5 = Counter.addVisit visits key 23 Null
      Assert.That(v5, Is.EqualTo 1)
      let v6 = Counter.addVisit visits key 42 Null
      Assert.That(v6, Is.EqualTo 1)
      Assert.That(visits.Count, Is.EqualTo 1)
      Assert.That(visits.[key].Count, Is.EqualTo 2)

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let RepeatVisitsShouldIncrementCount() =
      Runner.init()
      let visits = new Dictionary<string, Dictionary<int, PointVisit>>()
      let key = " "
      let v7 = Counter.addVisit visits key 23 Null
      Assert.That(v7, Is.EqualTo 1)
      let v8 = Counter.addVisit visits key 23 Null
      Assert.That(v8, Is.EqualTo 1)
      let x = visits.[key].[23]
      Assert.That(x.Count, Is.EqualTo 2)
      Assert.That(x.Tracks, Is.Empty)

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let RepeatVisitsShouldIncrementTotal() =
      Runner.init()
      let visits = new Dictionary<string, Dictionary<int, PointVisit>>()
      let key = " "
      let payload = Time DateTime.UtcNow.Ticks
      let v9 = Counter.addVisit visits key 23 Null
      Assert.That(v9, Is.EqualTo 1)
      let v10 = Counter.addVisit visits key 23 payload
      Assert.That(v10, Is.EqualTo 1)
      let x = visits.[key].[23]
      Assert.That(x.Count, Is.EqualTo 1)
      Assert.That(x.Tracks, Is.EquivalentTo [ payload ])

    let resource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find (fun n -> n.EndsWith("SimpleCoverage.xml", StringComparison.Ordinal))
    let resource2 =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find
           (fun n -> n.EndsWith("Sample1WithOpenCover.xml", StringComparison.Ordinal))

    let internal Init n l = let tmp = { PointVisit.Create() with Count = n }
                            tmp.Tracks.AddRange l
                            tmp

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let KnownModuleWithPayloadMakesExpectedChangeInOpenCover() =
      Runner.init()
      Counter.measureTime <- DateTime.ParseExact
                               ("2017-12-29T16:33:40.9564026+00:00", "o", null)
      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)
      let size = int stream.Length
      let buffer = Array.create size 0uy
      Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)
      use worker = new MemoryStream()
      use worker2 = new MemoryStream()
      worker.Write(buffer, 0, size)
      worker.Position <- 0L
      let payload = Dictionary<int, PointVisit>()
      [ 0..9 ] |> Seq.iter (fun i -> payload.[10 - i] <- Init (int64(i + 1)) [])
      [ 11..12 ] |> Seq.iter (fun i -> payload.[i ||| Counter.branchFlag] <- Init (int64(i - 10)) [])
      let item = Dictionary<string, Dictionary<int, PointVisit>>()
      item.Add("7C-CD-66-29-A3-6C-6D-5F-A7-65-71-0E-22-7D-B2-61-B5-1F-65-9A", payload)
      Counter.I.updateReport ignore (fun _ _ -> ()) true item ReportFormat.OpenCover worker
        worker2 |> ignore
      worker2.Position <- 0L
      let after = XmlDocument()
      after.Load worker2
      Assert.That
        (after.SelectNodes("//SequencePoint")
         |> Seq.cast<XmlElement>
         |> Seq.map (fun x -> x.GetAttribute("vc")),
         Is.EquivalentTo [ "11"; "10"; "9"; "8"; "7"; "6"; "4"; "3"; "2"; "1" ])
      Assert.That
        (after.SelectNodes("//BranchPoint")
         |> Seq.cast<XmlElement>
         |> Seq.map (fun x -> x.GetAttribute("vc")), Is.EquivalentTo [ "2"; "2" ])

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let FlushLeavesExpectedTraces() =
      Runner.init()
      let saved = Console.Out
      let here = Directory.GetCurrentDirectory()
      let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
      let unique = Path.Combine(where, Guid.NewGuid().ToString())
      let reportFile = Path.Combine(unique, "FlushLeavesExpectedTraces.xml")
      try
        let visits = new Dictionary<string, Dictionary<int, PointVisit>>()
        use stdout = new StringWriter()
        Console.SetOut stdout
        Directory.CreateDirectory(unique) |> ignore
        Directory.SetCurrentDirectory(unique)
        Counter.measureTime <- DateTime.ParseExact
                                 ("2017-12-29T16:33:40.9564026+00:00", "o", null)
        use stream =
          Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
        let size = int stream.Length
        let buffer = Array.create size 0uy
        Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)
        do use worker = new FileStream(reportFile, FileMode.CreateNew)
           worker.Write(buffer, 0, size)
           ()
        let payload = Dictionary<int, PointVisit>()
        [ 0..9 ] |> Seq.iter (fun i -> payload.[i] <- Init (int64(i + 1)) [])
        visits.["f6e3edb3-fb20-44b3-817d-f69d1a22fc2f"] <- payload
        Counter.doFlush ignore (fun _ _ -> ()) true visits
          AltCover.Base.ReportFormat.NCover reportFile None |> ignore
        use worker' = new FileStream(reportFile, FileMode.Open)
        let after = XmlDocument()
        after.Load worker'
        Assert.That
          (after.SelectNodes("//seqpnt")
           |> Seq.cast<XmlElement>
           |> Seq.map (fun x -> x.GetAttribute("visitcount")),
           Is.EquivalentTo [ "11"; "10"; "9"; "8"; "7"; "6"; "4"; "3"; "2"; "1" ])
      finally
        if File.Exists reportFile then File.Delete reportFile
        Console.SetOut saved
        Directory.SetCurrentDirectory(here)
        try
          Directory.Delete(unique)
        with :? IOException -> ()

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let FlushLeavesExpectedTracesWhenDiverted() =
      Runner.init()
      let saved = Console.Out
      let here = Directory.GetCurrentDirectory()
      let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
      let unique = Path.Combine(where, Guid.NewGuid().ToString())
      let reportFile = Path.Combine(unique, "FlushLeavesExpectedTraces.xml")
      let outputFile = Path.Combine(unique, "FlushLeavesExpectedTracesWhenDiverted.xml")
      try
        let visits = new Dictionary<string, Dictionary<int, PointVisit>>()
        use stdout = new StringWriter()
        Console.SetOut stdout
        Directory.CreateDirectory(unique) |> ignore
        Directory.SetCurrentDirectory(unique)
        Counter.measureTime <- DateTime.ParseExact
                                 ("2017-12-29T16:33:40.9564026+00:00", "o", null)
        use stream =
          Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
        let size = int stream.Length
        let buffer = Array.create size 0uy
        Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)
        do use worker = new FileStream(reportFile, FileMode.CreateNew)
           worker.Write(buffer, 0, size)
           ()
        let payload = Dictionary<int, PointVisit>()
        [ 0..9 ] |> Seq.iter (fun i -> payload.[i] <- Init (int64(i + 1)) [])
        visits.["f6e3edb3-fb20-44b3-817d-f69d1a22fc2f"] <- payload
        Counter.doFlush ignore (fun _ _ -> ()) true visits
          AltCover.Base.ReportFormat.NCover reportFile (Some outputFile) |> ignore
        use worker' = new FileStream(outputFile, FileMode.Open)
        let after = XmlDocument()
        after.Load worker'
        Assert.That
          (after.SelectNodes("//seqpnt")
           |> Seq.cast<XmlElement>
           |> Seq.map (fun x -> x.GetAttribute("visitcount")),
           Is.EquivalentTo [ "11"; "10"; "9"; "8"; "7"; "6"; "4"; "3"; "2"; "1" ])
      finally
        if File.Exists reportFile then File.Delete reportFile
        Console.SetOut saved
        Directory.SetCurrentDirectory(here)
        try
          Directory.Delete(unique)
        with :? IOException -> ()

    // Runner.fs and CommandLine.fs
#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let UsageIsAsExpected() =
      Runner.init()
      let options = Runner.declareOptions()
      let saved = Console.Error
      try
        use stderr = new StringWriter()
        Console.SetError stderr
        let empty = OptionSet()
        CommandLine.usageBase { Intro = "UsageError"; Options = empty; Options2 = options}
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
      --collect              Optional: Process previously saved raw coverage
                               data, rather than launching a process.
  -l, --lcovReport=VALUE     Optional: File for lcov format version of the
                               collected data
  -t, --threshold=VALUE      Optional: minimum acceptable coverage percentage (
                               integer, 0 to 100).  If the coverage result is
                               below threshold, the return code of the process
                               is (threshold - actual) rounded up to the
                               nearest integer.
  -c, --cobertura=VALUE      Optional: File for Cobertura format version of the
                               collected data
  -o, --outputFile=VALUE     Optional: write the recorded coverage to this file
                               rather than overwriting the original report file.
      --dropReturnCode       Optional: Do not report any non-zero return code
                               from a launched process.
      --teamcity[=VALUE]     Optional: Show summary in TeamCity format as well
                               as/instead of the OpenCover summary
  -?, --help, -h             Prints out the options.
"""
        Assert.That
          (result, Is.EqualTo(expected.Replace("\r\n", "\n")), "*" + result + "*")
      finally
        Console.SetError saved

    [<Test>]
    let ShouldLaunchWithExpectedOutput() =
      Runner.init()
      let path = Path.Combine(SolutionRoot.location, "_Mono/Sample1")
      let files = Directory.GetFiles(path)

      let program =
        files
        |> Seq.filter (fun x -> x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
        |> Seq.head
      AltCover.toConsole()
      let saved = (Console.Out, Console.Error)
      let e0 = Console.Out.Encoding
      let e1 = Console.Error.Encoding
      try
        use stdout =
          { new StringWriter() with
              member self.Encoding = e0 }

        use stderr =
          { new StringWriter() with
              member self.Encoding = e1 }

        Console.SetOut stdout
        Console.SetError stderr
        let nonWindows = System.Environment.GetEnvironmentVariable("OS") <> "Windows_NT"

        let exe, args =
          if nonWindows then ("mono", "\"" + program + "\"")
          else (program, String.Empty)

        let r =
          CommandLine.I.launch exe args
            (Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location))
        Assert.That(r, Is.EqualTo 0)
        Assert.That(stderr.ToString(), Is.Empty)
        let result = stdout.ToString()

        let quote =
          if System.Environment.GetEnvironmentVariable("OS") = "Windows_NT" then "\""
          else String.Empty

        let expected =
          "Command line : '" + quote + exe + quote + " " + args + "\'"
          + Environment.NewLine + "Where is my rocket pack? " + Environment.NewLine
        Assert.That(result, Is.EqualTo(expected))
      finally
        Console.SetOut(fst saved)
        Console.SetError(snd saved)

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ShouldHaveExpectedOptions() =
      Runner.init()
      let options = Runner.declareOptions()
      Assert.That(options.Count, Is.EqualTo 12)
      Assert.That
        (options
         |> Seq.filter (fun x -> x.Prototype <> "<>")
         |> Seq.forall (fun x -> (String.IsNullOrWhiteSpace >> not) x.Description))
      Assert.That(options
                  |> Seq.filter (fun x -> x.Prototype = "<>")
                  |> Seq.length, Is.EqualTo 1)

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ParsingJunkIsAnError() =
      Runner.init()
      let options = Runner.declareOptions()
      let parse = CommandLine.parseCommandLine [| "/@thisIsNotAnOption" |] options
      match parse with
      | Right _ -> Assert.Fail()
      | Left(x, y) ->
        Assert.That(x, Is.EqualTo "UsageError")
        Assert.That(y, Is.SameAs options)

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ParsingJunkAfterSeparatorIsExpected() =
      Runner.init()
      let options = Runner.declareOptions()
      let input = [| "--"; "/@thisIsNotAnOption"; "this should be OK" |]
      let parse = CommandLine.parseCommandLine input options
      match parse with
      | Left _ -> Assert.Fail()
      | Right(x, y) ->
        Assert.That(x, Is.EquivalentTo(input |> Seq.skip 1))
        Assert.That(y, Is.SameAs options)

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ParsingHelpGivesHelp() =
      Runner.init()
      let options = Runner.declareOptions()
      let input = [| "--?" |]
      let parse = CommandLine.parseCommandLine input options
      match parse with
      | Left _ -> Assert.Fail()
      | Right(x, y) -> Assert.That(y, Is.SameAs options)
      match CommandLine.processHelpOption parse with
      | Right _ -> Assert.Fail()
      | Left(x, y) ->
        Assert.That(x, Is.EqualTo "HelpText")
        Assert.That(y, Is.SameAs options)
      // a "not sticky" test
      lock Runner.executable (fun () ->
        Runner.executable := None
        match CommandLine.parseCommandLine [| "/x"; "x" |] options
              |> CommandLine.processHelpOption with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty))

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ParsingErrorHelpGivesHelp() =
      Runner.init()
      let options = Runner.declareOptions()

      let input =
        [| "--zzz"
           Path.GetInvalidPathChars() |> String |]

      let parse = CommandLine.parseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left(x, y) ->
        Assert.That(x, Is.EqualTo "UsageError")
        Assert.That(y, Is.SameAs options)
      match CommandLine.processHelpOption parse with
      | Right _ -> Assert.Fail()
      | Left(x, y) ->
        Assert.That(x, Is.EqualTo "UsageError")
        Assert.That(y, Is.SameAs options)
      // a "not sticky" test
      lock Runner.executable (fun () ->
        Runner.executable := None
        match CommandLine.parseCommandLine [| "/x"; "x" |] options
              |> CommandLine.processHelpOption with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty))

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ParsingExeGivesExe() =
      Runner.init()
      lock Runner.executable (fun () ->
        try
          Runner.executable := None
          let options = Runner.declareOptions()
          let unique = "some exe"
          let input = [| "-x"; unique |]
          let parse = CommandLine.parseCommandLine input options
          match parse with
          | Left _ -> Assert.Fail()
          | Right(x, y) ->
            Assert.That(y, Is.SameAs options)
            Assert.That(x, Is.Empty)
          match !Runner.executable with
          | None -> Assert.Fail()
          | Some x -> Assert.That(Path.GetFileName x, Is.EqualTo unique)
        finally
          Runner.executable := None)

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ParsingMultipleExeGivesFailure() =
      Runner.init()
      lock Runner.executable (fun () ->
        try
          Runner.executable := None
          let options = Runner.declareOptions()
          let unique = Guid.NewGuid().ToString()

          let input =
            [| "-x"
               unique
               "/x"
               unique.Replace("-", "+") |]

          let parse = CommandLine.parseCommandLine input options
          match parse with
          | Right _ -> Assert.Fail()
          | Left(x, y) ->
            Assert.That(y, Is.SameAs options)
            Assert.That(x, Is.EqualTo "UsageError")
            Assert.That(CommandLine.error |> Seq.head, Is.EqualTo "--executable : specify this only once")
        finally
          Runner.executable := None)

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ParsingNoExeGivesFailure() =
      Runner.init()
      lock Runner.executable (fun () ->
        try
          Runner.executable := None
          let options = Runner.declareOptions()
          let blank = " "
          let input = [| "-x"; blank |]
          let parse = CommandLine.parseCommandLine input options
          match parse with
          | Right _ -> Assert.Fail()
          | Left(x, y) ->
            Assert.That(y, Is.SameAs options)
            Assert.That(x, Is.EqualTo "UsageError")
        finally
          Runner.executable := None)

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ParsingWorkerGivesWorker() =
      Runner.init()
      try
        Runner.workingDirectory <- None
        let options = Runner.declareOptions()
        let unique = Path.GetFullPath(".")
        let input = [| "-w"; unique |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        match Runner.workingDirectory with
        | None -> Assert.Fail()
        | Some x -> Assert.That(x, Is.EqualTo unique)
      finally
        Runner.workingDirectory <- None

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ParsingMultipleWorkerGivesFailure() =
      Runner.init()
      try
        Runner.workingDirectory <- None
        let options = Runner.declareOptions()

        let input =
          [| "-w"
             Path.GetFullPath(".")
             "/w"
             Path.GetFullPath("..") |]

        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
          Assert.That(CommandLine.error |> Seq.head, Is.EqualTo "--workingDirectory : specify this only once")
      finally
        Runner.workingDirectory <- None

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ParsingBadWorkerGivesFailure() =
      Runner.init()
      try
        Runner.workingDirectory <- None
        let options = Runner.declareOptions()
        let unique = Guid.NewGuid().ToString().Replace("-", "*")
        let input = [| "-w"; unique |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.workingDirectory <- None

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ParsingNoWorkerGivesFailure() =
      Runner.init()
      try
        Runner.workingDirectory <- None
        let options = Runner.declareOptions()
        let input = [| "-w" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.workingDirectory <- None

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ParsingRecorderGivesRecorder() =
      Runner.init()
      try
        Runner.recordingDirectory <- None
        let options = Runner.declareOptions()
        let unique = Path.GetFullPath(".")
        let input = [| "-r"; unique |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        match Runner.recordingDirectory with
        | None -> Assert.Fail()
        | Some x -> Assert.That(x, Is.EqualTo unique)
      finally
        Runner.recordingDirectory <- None

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ParsingMultipleRecorderGivesFailure() =
      Runner.init()
      try
        Runner.recordingDirectory <- None
        let options = Runner.declareOptions()

        let input =
          [| "-r"
             Path.GetFullPath(".")
             "/r"
             Path.GetFullPath("..") |]

        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
          Assert.That(CommandLine.error |> Seq.head, Is.EqualTo "--recorderDirectory : specify this only once")
      finally
        Runner.recordingDirectory <- None

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ParsingBadRecorderGivesFailure() =
      Runner.init()
      try
        Runner.recordingDirectory <- None
        let options = Runner.declareOptions()
        let unique = Guid.NewGuid().ToString().Replace("-", "*")
        let input = [| "-r"; unique |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.recordingDirectory <- None

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ParsingNoRecorderGivesFailure() =
      Runner.init()
      try
        Runner.recordingDirectory <- None
        let options = Runner.declareOptions()
        let input = [| "-r" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.recordingDirectory <- None

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ParsingCollectGivesCollect() =
      Runner.init()
      try
        Runner.collect := false
        let options = Runner.declareOptions()
        let input = [| "--collect" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        Assert.That(!Runner.collect, Is.True)
      finally
        Runner.collect := false

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ParsingMultipleCollectGivesFailure() =
      Runner.init()
      try
        Runner.collect := false
        let options = Runner.declareOptions()
        let input = [| "--collect"; "--collect" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
          Assert.That(CommandLine.error |> Seq.head, Is.EqualTo "--collect : specify this only once")
      finally
        Runner.collect := false

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ParsingLcovGivesLcov() =
      Runner.init()
      lock LCov.path (fun () ->
        try
          LCov.path := None
          Runner.I.summaries <- [ Runner.I.standardSummary ]
          let options = Runner.declareOptions()
          let unique = "some exe"
          let input = [| "-l"; unique |]
          let parse = CommandLine.parseCommandLine input options
          match parse with
          | Left _ -> Assert.Fail()
          | Right(x, y) ->
            Assert.That(y, Is.SameAs options)
            Assert.That(x, Is.Empty)
          match !LCov.path with
          | None -> Assert.Fail()
          | Some x -> Assert.That(Path.GetFileName x, Is.EqualTo unique)
          Assert.That(Runner.I.summaries.Length, Is.EqualTo 2)
        finally
          Runner.I.summaries <- [ Runner.I.standardSummary ]
          LCov.path := None)

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ParsingMultipleLcovGivesFailure() =
      Runner.init()
      lock LCov.path (fun () ->
        try
          LCov.path := None
          Runner.I.summaries <- [ Runner.I.standardSummary ]
          let options = Runner.declareOptions()
          let unique = Guid.NewGuid().ToString()

          let input =
            [| "-l"
               unique
               "/l"
               unique.Replace("-", "+") |]

          let parse = CommandLine.parseCommandLine input options
          match parse with
          | Right _ -> Assert.Fail()
          | Left(x, y) ->
            Assert.That(y, Is.SameAs options)
            Assert.That(x, Is.EqualTo "UsageError")
            Assert.That(CommandLine.error |> Seq.head, Is.EqualTo "--lcovReport : specify this only once")
        finally
          Runner.I.summaries <- [ Runner.I.standardSummary ]
          LCov.path := None)

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ParsingNoLcovGivesFailure() =
      Runner.init()
      lock LCov.path (fun () ->
        try
          LCov.path := None
          Runner.I.summaries <- [ Runner.I.standardSummary ]
          let options = Runner.declareOptions()
          let blank = " "
          let input = [| "-l"; blank |]
          let parse = CommandLine.parseCommandLine input options
          match parse with
          | Right _ -> Assert.Fail()
          | Left(x, y) ->
            Assert.That(y, Is.SameAs options)
            Assert.That(x, Is.EqualTo "UsageError")
        finally
          Runner.I.summaries <- [ Runner.I.standardSummary ]
          LCov.path := None)

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ParsingThresholdGivesThreshold() =
      Runner.init()
      try
        Runner.threshold <- None
        let options = Runner.declareOptions()
        let input = [| "-t"; "57" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        match Runner.threshold with
        | None -> Assert.Fail()
        | Some x -> Assert.That(x, Is.EqualTo 57)
      finally
        Runner.threshold <- None

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ParsingMultipleThresholdGivesFailure() =
      Runner.init()
      try
        Runner.threshold <- None
        let options = Runner.declareOptions()
        let input = [| "-t"; "23"; "/t"; "42" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
          Assert.That(CommandLine.error |> Seq.head, Is.EqualTo "--threshold : specify this only once")
      finally
        Runner.threshold <- None

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ParsingBadThresholdGivesFailure() =
      Runner.init()
      try
        Runner.threshold <- None
        let options = Runner.declareOptions()
        let input = [| "-t"; "-111" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.threshold <- None

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ParsingEmptyThresholdGivesFailure() =
      Runner.init()
      try
        Runner.threshold <- None
        let options = Runner.declareOptions()
        let input = [| "-t"; "  " |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.threshold <- None

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ParsingNoThresholdGivesFailure() =
      Runner.init()
      try
        Runner.threshold <- None
        let options = Runner.declareOptions()
        let input = [| "-t" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.threshold <- None

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ParsingCoberturaGivesCobertura() =
      Runner.init()
      lock Cobertura.path (fun () ->
        try
          Cobertura.path := None
          Runner.I.summaries <- [ Runner.I.standardSummary ]
          let options = Runner.declareOptions()
          let unique = "some exe"
          let input = [| "-c"; unique |]
          let parse = CommandLine.parseCommandLine input options
          match parse with
          | Left _ -> Assert.Fail()
          | Right(x, y) ->
            Assert.That(y, Is.SameAs options)
            Assert.That(x, Is.Empty)
          match !Cobertura.path with
          | None -> Assert.Fail()
          | Some x -> Assert.That(Path.GetFileName x, Is.EqualTo unique)
          Assert.That(Runner.I.summaries.Length, Is.EqualTo 2)
        finally
          Runner.I.summaries <- [ Runner.I.standardSummary ]
          Cobertura.path := None)

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ParsingMultipleCoberturaGivesFailure() =
      Runner.init()
      lock Cobertura.path (fun () ->
        try
          Cobertura.path := None
          Runner.I.summaries <- [ Runner.I.standardSummary ]
          let options = Runner.declareOptions()
          let unique = Guid.NewGuid().ToString()

          let input =
            [| "-c"
               unique
               "/c"
               unique.Replace("-", "+") |]

          let parse = CommandLine.parseCommandLine input options
          match parse with
          | Right _ -> Assert.Fail()
          | Left(x, y) ->
            Assert.That(y, Is.SameAs options)
            Assert.That(x, Is.EqualTo "UsageError")
            Assert.That(CommandLine.error |> Seq.head, Is.EqualTo "--cobertura : specify this only once")
        finally
          Runner.I.summaries <- [ Runner.I.standardSummary ]
          Cobertura.path := None)

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ParsingNoCoberturaGivesFailure() =
      Runner.init()
      lock Cobertura.path (fun () ->
        try
          Cobertura.path := None
          Runner.I.summaries <- [ Runner.I.standardSummary ]
          let options = Runner.declareOptions()
          let blank = " "
          let input = [| "-c"; blank |]
          let parse = CommandLine.parseCommandLine input options
          match parse with
          | Right _ -> Assert.Fail()
          | Left(x, y) ->
            Assert.That(y, Is.SameAs options)
            Assert.That(x, Is.EqualTo "UsageError")
        finally
          Runner.I.summaries <- [ Runner.I.standardSummary ]
          Cobertura.path := None)

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ParsingOutputGivesOutput() =
      Runner.init()
      try
        Runner.output <- None
        let options = Runner.declareOptions()
        let unique = Guid.NewGuid().ToString()
        let input = [| "-o"; unique |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        match Runner.output with
        | None -> Assert.Fail()
        | Some x -> Assert.That(Path.GetFileName x, Is.EqualTo unique)
      finally
        Runner.output <- None

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ParsingMultipleOutputGivesFailure() =
      Runner.init()
      try
        Runner.output <- None
        Runner.collect := false
        let options = Runner.declareOptions()
        let unique = Guid.NewGuid().ToString()

        let input =
          [| "-o"
             unique
             "/o"
             unique.Replace("-", "+") |]

        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
          Assert.That(CommandLine.error |> Seq.head, Is.EqualTo "--outputFile : specify this only once")
      finally
        Runner.output <- None

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ParsingNoOutputGivesFailure() =
      Runner.init()
      try
        Runner.output <- None
        let options = Runner.declareOptions()
        let blank = " "
        let input = [| "-o"; blank |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.output <- None

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ParsingDropGivesDrop() =
      Runner.init()
      try
        CommandLine.dropReturnCode := false
        let options = Runner.declareOptions()
        let input = [| "--dropReturnCode" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        Assert.That(!CommandLine.dropReturnCode, Is.True)
      finally
        CommandLine.dropReturnCode := false

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ParsingMultipleDropGivesFailure() =
      Runner.init()
      try
        CommandLine.dropReturnCode := false
        let options = Runner.declareOptions()
        let input = [| "--dropReturnCode"; "--dropReturnCode" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
          Assert.That(CommandLine.error |> Seq.head, Is.EqualTo "--dropReturnCode : specify this only once")
      finally
        CommandLine.dropReturnCode := false

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ParsingTCString() =
      Runner.init()
      [
        (String.Empty, Default)
        ("+", BPlus)
        ("+b", BPlus)
        ("+B", BPlus)
        ("b+", BPlus)
        ("B+", BPlus)
        ("b",B)
        ("B", B)
        ("+r", RPlus)
        ("+R", RPlus)
        ("r+", RPlus)
        ("R+", RPlus)
        ("r", R)
        ("R", R)
        ("true", Default)
       ]
       |> List.iter (fun (x,y) -> Assert.That (TeamCityFormat.Factory x,
                                               Is.EqualTo y,
                                               x))

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ParsingTCGivesTC() =
      Runner.init()
      [
        (String.Empty, B)
        (":+", BPlus)
        (":+b", BPlus)
        (":+B", BPlus)
        (":b+", BPlus)
        (":B+", BPlus)
        (":b",B)
        (":B", B)
        (":+r", RPlus)
        (":+R", RPlus)
        (":r+", RPlus)
        (":R+", RPlus)
        (":r", R)
        (":R", R) ]
      |> List.iter (fun (a, v) ->
        lock Runner.summaryFormat (fun () ->
          Runner.summaryFormat <- Default
          let options = Runner.declareOptions()
          let input = [| "--teamcity" + a |]
          let parse = CommandLine.parseCommandLine input options
          match parse with
          | Left _ -> Assert.Fail()
          | Right(x, y) ->
            Assert.That(y, Is.SameAs options)
            Assert.That(x, Is.Empty)
          match Runner.summaryFormat with
          | x when v = x ->
#if NETCOREAPP2_0
            ()
#else
            Assert.Pass()
#endif
          | _ -> Assert.Fail(sprintf "%A %A => %A" a v Runner.summaryFormat) ))

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ParsingMultipleTCGivesFailure() =
      Runner.init()
      lock Runner.summaryFormat (fun () ->
        Runner.summaryFormat <- Default
        let options = Runner.declareOptions()
        let input = [| "--teamcity"; "--teamcity" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
          Assert.That(CommandLine.error |> Seq.head, Is.EqualTo "--teamcity : specify this only once"))

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ParsingBadTCGivesFailure() =
      Runner.init()
      lock Runner.summaryFormat (fun () ->
        Runner.summaryFormat <- Default
        let options = Runner.declareOptions()
        let blank = " "
        let input = [| "--teamcity:junk" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError"))

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ShouldRequireExe() =
      Runner.init()
      lock Runner.executable (fun () ->
        try
          Runner.executable := None
          let options = Runner.declareOptions()
          let parse = Runner.J.requireExe(Right([], options))
          match parse with
          | Right _ -> Assert.Fail()
          | Left(x, y) ->
            Assert.That(y, Is.SameAs options)
            Assert.That(x, Is.EqualTo "UsageError")
        finally
          Runner.executable := None)

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ShouldAcceptExe() =
      Runner.init()
      lock Runner.executable (fun () ->
        try
          Runner.executable := Some "xxx"
          let options = Runner.declareOptions()
          let parse = Runner.J.requireExe(Right([ "b" ], options))
          match parse with
          | Right(x :: y, z) ->
            Assert.That(z, Is.SameAs options)
            Assert.That(x, Is.EqualTo "xxx")
            Assert.That(y, Is.EquivalentTo [ "b" ])
          | _ -> Assert.Fail()
        finally
          Runner.executable := None)

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ShouldRequireCollectIfNotExe() =
      Runner.init()
      lock Runner.executable (fun () ->
        try
          Runner.executable := None
          Runner.collect := true
          let options = Runner.declareOptions()
          let parse = Runner.J.requireExe(Right([ "a"; "b" ], options))
          match parse with
          | Right([], z) -> Assert.That(z, Is.SameAs options)
          | _ -> Assert.Fail()
        finally
          Runner.collect := false
          Runner.executable := None)

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ShouldRejectExeIfCollect() =
      Runner.init()
      lock Runner.executable (fun () ->
        try
          Runner.executable := Some "xxx"
          Runner.collect := true
          let options = Runner.declareOptions()
          let parse = Runner.J.requireExe(Right([ "b" ], options))
          match parse with
          | Right _ -> Assert.Fail()
          | Left(x, y) ->
            Assert.That(y, Is.SameAs options)
            Assert.That(x, Is.EqualTo "UsageError")
        finally
          Runner.collect := false
          Runner.executable := None)

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ShouldRequireWorker() =
      Runner.init()
      try
        Runner.workingDirectory <- None
        let options = Runner.declareOptions()
        let input = (Right([], options))
        let parse = Runner.J.requireWorker input
        match parse with
        | Right _ ->
          Assert.That(parse, Is.SameAs input)
          Assert.That(Option.isSome Runner.workingDirectory)
        | _ -> Assert.Fail()
      finally
        Runner.workingDirectory <- None

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ShouldAcceptWorker() =
      Runner.init()
      try
        Runner.workingDirectory <- Some "ShouldAcceptWorker"
        let options = Runner.declareOptions()
        let input = (Right([], options))
        let parse = Runner.J.requireWorker input
        match parse with
        | Right _ ->
          Assert.That(parse, Is.SameAs input)
          Assert.That(Runner.workingDirectory, Is.EqualTo(Some "ShouldAcceptWorker"))
        | _ -> Assert.Fail()
      finally
        Runner.workingDirectory <- None

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ShouldRequireRecorder() =
      Runner.init()
      try
        Runner.recordingDirectory <- None
        let options = Runner.declareOptions()
        let input = (Right([], options))
        let parse = Runner.J.requireRecorder input
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.recordingDirectory <- None

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ShouldRequireRecorderDll() =
      Runner.init()
      try
        let where = Assembly.GetExecutingAssembly().Location
        let path =
          Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), "_Mono/Sample1")

        let path' =
          if Directory.Exists path then path
          else
            Path.Combine
              (where.Substring(0, where.IndexOf("_Binaries")), "../_Mono/Sample1")
        Runner.recordingDirectory <- Some path'
        let options = Runner.declareOptions()
        let input = (Right([], options))
        let parse = Runner.J.requireRecorder input
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.recordingDirectory <- None

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ShouldAcceptRecorder() =
      Runner.init()
      try
        let here = (Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName)
        let where = Path.Combine(here, Guid.NewGuid().ToString())
        Directory.CreateDirectory(where) |> ignore
        Runner.recordingDirectory <- Some where
        let create = Path.Combine(where, "AltCover.Recorder.g.dll")
        if create
           |> File.Exists
           |> not
        then
          do let from = Path.Combine(here, "AltCover.Recorder.dll")
             use frombytes = new FileStream(from, FileMode.Open, FileAccess.Read)
             use libstream = new FileStream(create, FileMode.Create)
             frombytes.CopyTo libstream
        let options = Runner.declareOptions()
        let input = (Right([], options))
        let parse = Runner.J.requireRecorder input
        match parse with
        | Right _ -> Assert.That(parse, Is.SameAs input)
        | _ -> Assert.Fail()
      finally
        Runner.recordingDirectory <- None

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ShouldHandleReturnCodes() =
      Runner.init()
      // Hack for running while instrumented
      let where = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
#if NETCOREAPP2_0
      let path = Path.Combine(SolutionRoot.location, "_Binaries/Sample12/Debug+AnyCPU/netcoreapp2.0/Sample12.dll")
#else
      let path = Path.Combine(where, "Sample12.exe")
#endif

      let nonWindows = System.Environment.GetEnvironmentVariable("OS") <> "Windows_NT"

      let args =
#if NETCOREAPP2_0
          [ "dotnet"; path ]
#else
          if nonWindows then [ "mono"; path ]
          else [ path ]
#endif

      let r = CommandLine.processTrailingArguments args <| DirectoryInfo(where)
      Assert.That(r, Is.EqualTo 0)

      let r2 = CommandLine.processTrailingArguments (args @ [ "1"; "2" ]) <| DirectoryInfo(where)
      Assert.That(r2, Is.EqualTo 2)

      try
        CommandLine.dropReturnCode := true
        let r0 = CommandLine.processTrailingArguments (args @ [ "1"; "2" ]) <| DirectoryInfo(where)
        Assert.That(r0, Is.EqualTo 0)
      finally
        CommandLine.dropReturnCode := false

    [<Test>]
    let ShouldProcessTrailingArguments() =
      Runner.init()
      let path = Path.Combine(SolutionRoot.location, "_Mono/Sample1")
      let files = Directory.GetFiles(path)

      let program =
        files
        |> Seq.filter (fun x -> x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
        |> Seq.head
      AltCover.toConsole()
      let saved = (Console.Out, Console.Error)
      let e0 = Console.Out.Encoding
      let e1 = Console.Error.Encoding
      try
        use stdout =
          { new StringWriter() with
              member self.Encoding = e0 }

        use stderr =
          { new StringWriter() with
              member self.Encoding = e1 }

        Console.SetOut stdout
        Console.SetError stderr
        let u1 = Guid.NewGuid().ToString()
        let u2 = Guid.NewGuid().ToString()
        let baseArgs = [ program; u1; u2 ]
        let nonWindows = System.Environment.GetEnvironmentVariable("OS") <> "Windows_NT"

        let args =
          if nonWindows then "mono" :: baseArgs
          else baseArgs

        let r = CommandLine.processTrailingArguments args <| DirectoryInfo(path)
        Assert.That(r, Is.EqualTo 0)
        Assert.That(stderr.ToString(), Is.Empty)
        stdout.Flush()
        let result = stdout.ToString()

        let quote =
          if System.Environment.GetEnvironmentVariable("OS") = "Windows_NT" then "\""
          else String.Empty

        let expected =
          "Command line : '" + quote + args.Head + quote + " "
          + String.Join(" ", args.Tail) + "'" + Environment.NewLine
          + "Where is my rocket pack? " + u1 + "*" + u2 + Environment.NewLine
        Assert.That(result, Is.EqualTo expected)
      finally
        Console.SetOut(fst saved)
        Console.SetError(snd saved)

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ShouldNoOp() =
      Runner.init()
      let where = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
      let r = CommandLine.processTrailingArguments [] <| DirectoryInfo(where)
      Assert.That(r, Is.EqualTo 0)

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ErrorResponseIsAsExpected() =
      Runner.init()
      let saved = Console.Error
      try
        use stderr = new StringWriter()
        Console.SetError stderr
        let unique = Guid.NewGuid().ToString()
        let main =
          typeof<TeamCityFormat>.Assembly.GetType("AltCover.AltCover")
            .GetMethod("main", BindingFlags.NonPublic ||| BindingFlags.Static)
        let returnCode = main.Invoke(null, [| [| "RuNN"; "-r"; unique |] |])
        Assert.That(returnCode, Is.EqualTo 255)
        let result = stderr.ToString().Replace("\r\n", "\n")
        let expected = "\"RuNN\" \"-r\" \"" + unique + "\"\n"
                       + "--recorderDirectory : Directory " + unique + " not found\n" + """Error - usage is:
  -i, --inputDirectory=VALUE Optional, multiple: A folder containing assemblies
                               to instrument (default: current directory)
  -o, --outputDirectory=VALUE
                             Optional, multiple: A folder to receive the
                               instrumented assemblies and their companions (
                               default: sub-folder '__Instrumented' of the
                               current directory; or '__Saved' if '--inplace'
                               is set).
                               See also '--inplace'
  -y, --symbolDirectory=VALUE
                             Optional, multiple: Additional directory to search
                               for matching symbols for the assemblies in the
                               input directory
  -d, --dependency=VALUE     Optional, multiple: assembly path to resolve
                               missing reference.
  -k, --key=VALUE            Optional, multiple: any other strong-name key to
                               use
      --sn, --strongNameKey=VALUE
                             Optional: The default strong naming key to apply
                               to instrumented assemblies (default: None)
  -x, --xmlReport=VALUE      Optional: The output report template file (default:
                                coverage.xml in the current directory)
  -f, --fileFilter=VALUE     Optional, multiple: source file name to exclude
                               from instrumentation
  -p, --pathFilter=VALUE     Optional, multiple: source file path to exclude
                               from instrumentation
  -s, --assemblyFilter=VALUE Optional, multiple: assembly name to exclude from
                               instrumentation
  -e, --assemblyExcludeFilter=VALUE
                             Optional, multiple: assembly which links other
                               instrumented assemblies but for which internal
                               details may be excluded
  -t, --typeFilter=VALUE     Optional, multiple: type name to exclude from
                               instrumentation
  -m, --methodFilter=VALUE   Optional, multiple: method name to exclude from
                               instrumentation
  -a, --attributeFilter=VALUE
                             Optional, multiple: attribute name to exclude from
                               instrumentation
  -l, --localSource          Don't instrument code for which the source file is
                               not present.
  -c, --callContext=VALUE    Optional, multiple: Tracking either times of
                               visits in ticks or designated method calls
                               leading to the visits.
                                   A single digit 0-7 gives the number of
                               decimal places of seconds to report; everything
                               else is at the mercy of the system clock
                               information available through DateTime.UtcNow
                                   A string in brackets "[]" is interpreted as
                               an attribute type name (the trailing "Attribute"
                               is optional), so [Test] or [TestAttribute] will
                               match; if the name contains one or more ".",
                               then it will be matched against the full name of
                               the attribute type.
                                   Other strings are interpreted as method
                               names (fully qualified if the string contains
                               any "." characters).
                                   Incompatible with --single
      --opencover            Optional: Generate the report in OpenCover format
      --inplace              Optional: Instrument the inputDirectory, rather
                               than the outputDirectory (e.g. for dotnet test)
      --save                 Optional: Write raw coverage data to file for
                               later processing
      --single               Optional: only record the first hit at any
                               location.
                                   Incompatible with --callContext.
      --linecover            Optional: Do not record branch coverage.  Implies,
                               and is compatible with, the --opencover option.
                                   Incompatible with --branchcover.
      --branchcover          Optional: Do not record line coverage.  Implies,
                               and is compatible with, the --opencover option.
                                   Incompatible with --linecover.
      --dropReturnCode       Optional: Do not report any non-zero return code
                               from a launched process.
      --sourcelink           Optional: Display sourcelink URLs rather than file
                               paths if present.
      --defer[=VALUE]        Optional, defers writing runner-mode coverage data
                               until process exit.
  -v, --visibleBranches      Hide complex internal IL branching implementation
                               details in switch/match constructs, and just
                               show what the source level logic implies.
      --showstatic[=VALUE]   Optional: Instrument and show code that is by
                               default skipped as trivial.  --showstatic:- is
                               equivalent to omitting the parameter; --
                               showstatic or --showstatic:+ sets the unvisited
                               count to a negative value interpreted by the
                               visualizer (but treated as zero by
                               ReportGenerator) ; --showstatic:++ sets the
                               unvisited count to zero.
      --showGenerated        Mark generated code with a visit count of -2 (
                               Automatic) for the Visualizer if unvisited
  -?, --help, -h             Prints out the options.
or
  Runner
  -r, --recorderDirectory=VALUE
                             The folder containing the instrumented code to
                               monitor (including the AltCover.Recorder.g.dll
                               generated by previous a use of the .net core
                               AltCover).
  -w, --workingDirectory=VALUE
                             Optional: The working directory for the
                               application launch
  -x, --executable=VALUE     The executable to run e.g. dotnet
      --collect              Optional: Process previously saved raw coverage
                               data, rather than launching a process.
  -l, --lcovReport=VALUE     Optional: File for lcov format version of the
                               collected data
  -t, --threshold=VALUE      Optional: minimum acceptable coverage percentage (
                               integer, 0 to 100).  If the coverage result is
                               below threshold, the return code of the process
                               is (threshold - actual) rounded up to the
                               nearest integer.
  -c, --cobertura=VALUE      Optional: File for Cobertura format version of the
                               collected data
  -o, --outputFile=VALUE     Optional: write the recorded coverage to this file
                               rather than overwriting the original report file.
      --dropReturnCode       Optional: Do not report any non-zero return code
                               from a launched process.
      --teamcity[=VALUE]     Optional: Show summary in TeamCity format as well
                               as/instead of the OpenCover summary
  -?, --help, -h             Prints out the options.
"""
        Assert.That
          (result.Replace("\r\n", "\n"), Is.EqualTo(expected.Replace("\r\n", "\n")))
      finally
        Console.SetError saved

    let synchronized = Object()

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ShouldGetStringConstants() =
      Runner.init()
      let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
      let save = Runner.J.recorderName
      lock synchronized (fun () ->
        try
          Runner.recordingDirectory <- Some where
          Runner.J.recorderName <- "AltCover.Recorder.dll"
          let instance = Runner.J.recorderInstance() |> snd
          Assert.That
            (instance.FullName, Is.EqualTo "AltCover.Recorder.Instance",
             "should be the instance")
          let token =
            (Runner.J.getMethod instance "get_Token") |> Runner.J.getFirstOperandAsString
          Assert.That(token, Is.EqualTo "AltCover", "should be plain token")
          let report =
            (Runner.J.getMethod instance "get_ReportFile") |> Runner.J.getFirstOperandAsString
          Assert.That
            (report, Is.EqualTo "Coverage.Default.xml", "should be default coverage file")
        finally
          Runner.recordingDirectory <- None
          Runner.J.recorderName <- save)

    [<Test>]
    let ShouldProcessPayload() =
      Runner.init()
      let path = Path.Combine(SolutionRoot.location, "_Mono/Sample1")
      let files = Directory.GetFiles(path)

      let program =
        files
        |> Seq.filter (fun x -> x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
        |> Seq.head
      AltCover.toConsole()
      let saved = (Console.Out, Console.Error)
      Runner.workingDirectory <- Some path
      let e0 = Console.Out.Encoding
      let e1 = Console.Error.Encoding
      try
        use stdout =
          { new StringWriter() with
              member self.Encoding = e0 }

        use stderr =
          { new StringWriter() with
              member self.Encoding = e1 }

        Console.SetOut stdout
        Console.SetError stderr
        let u1 = Guid.NewGuid().ToString()
        let u2 = Guid.NewGuid().ToString()
        let baseArgs = [ program; u1; u2 ]
        let nonWindows = System.Environment.GetEnvironmentVariable("OS") <> "Windows_NT"

        let args =
          if nonWindows then "mono" :: baseArgs
          else baseArgs

        let r = Runner.J.getPayload args
        Assert.That(r, Is.EqualTo 0)
        Assert.That(stderr.ToString(), Is.Empty)
        stdout.Flush()
        let result = stdout.ToString()

        let quote =
          if System.Environment.GetEnvironmentVariable("OS") = "Windows_NT" then "\""
          else String.Empty

        let expected =
          "Command line : '" + quote + args.Head + quote + " "
          + String.Join(" ", args.Tail) + "'" + Environment.NewLine
          + "Where is my rocket pack? " + u1 + "*" + u2 + Environment.NewLine
        Assert.That(result, Is.EqualTo expected)
      finally
        Console.SetOut(fst saved)
        Console.SetError(snd saved)
        Runner.workingDirectory <- None

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let WriteLeavesExpectedTraces() =
      Runner.init()
      let saved = Console.Out
      let here = Directory.GetCurrentDirectory()
      let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
      let unique = Path.Combine(where, Guid.NewGuid().ToString())
      let reportFile = Path.Combine(unique, "FlushLeavesExpectedTraces.xml")
      try
        use stdout = new StringWriter()
        Console.SetOut stdout
        Directory.CreateDirectory(unique) |> ignore
        Directory.SetCurrentDirectory(unique)
        Counter.measureTime <- DateTime.ParseExact
                                 ("2017-12-29T16:33:40.9564026+00:00", "o", null)
        use stream =
          Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
        let size = int stream.Length
        let buffer = Array.create size 0uy
        Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)
        do use worker = new FileStream(reportFile, FileMode.CreateNew)
           worker.Write(buffer, 0, size)
           ()
        let hits = List<string * int * Base.Track>()
        [ 0..9 ]
        |> Seq.iter (fun i ->
             for j = 1 to i + 1 do
               hits.Add("f6e3edb3-fb20-44b3-817d-f69d1a22fc2f", i, Base.Null)
               ignore j)

        let counts = Dictionary<string, Dictionary<int, PointVisit>>()
        hits
        |> Seq.iter
             (fun (moduleId, hitPointId, hit) ->
             AltCover.Base.Counter.addVisit counts moduleId hitPointId hit |> ignore)

        Runner.J.doReport counts AltCover.Base.ReportFormat.NCover reportFile None |> ignore
        use worker' = new FileStream(reportFile, FileMode.Open)
        let after = XmlDocument()
        after.Load worker'
        Assert.That
          (after.SelectNodes("//seqpnt")
           |> Seq.cast<XmlElement>
           |> Seq.map (fun x -> x.GetAttribute("visitcount")),
           Is.EquivalentTo [ "11"; "10"; "9"; "8"; "7"; "6"; "4"; "3"; "2"; "1" ])
      finally
        if File.Exists reportFile then File.Delete reportFile
        Console.SetOut saved
        Directory.SetCurrentDirectory(here)
        try
          Directory.Delete(unique)
        with :? IOException -> ()

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let NullPayloadShouldReportNothing() =
      Runner.init()
      let counts = Dictionary<string, Dictionary<int, PointVisit>>()
      let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
      let unique = Path.Combine(where, Guid.NewGuid().ToString())
      do use s = File.Create(unique + ".0.acv")
         s.Close()
      let r = Runner.J.getMonitor counts unique List.length []
      Assert.That(r, Is.EqualTo 0)
      Assert.That(File.Exists(unique + ".acv"))
      Assert.That(counts, Is.Empty)

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ActivePayloadShouldReportAsExpected() =
      Runner.init()
      let counts = Dictionary<string, Dictionary<int, PointVisit>>()
      let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
      let unique = Path.Combine(where, Guid.NewGuid().ToString())

      let r =
        Runner.J.getMonitor counts unique (fun l ->
          use sink =
            new DeflateStream(File.OpenWrite(unique + ".0.acv"), CompressionMode.Compress)
          use formatter = new BinaryWriter(sink)
          l
          |> List.mapi (fun i x ->
               formatter.Write x
               formatter.Write i
               formatter.Write 0uy
               x)
          |> List.length) [ "a"; "b"; String.Empty; "c" ]
      Assert.That(r, Is.EqualTo 4)
      Assert.That(File.Exists(unique + ".acv"))
      let expected = Dictionary<string, Dictionary<int, PointVisit>>()
      let a = Dictionary<int, PointVisit>()
      a.Add(0, Init 1L [])
      let b = Dictionary<int, PointVisit>()
      b.Add(1, Init 1L [])
      let c = Dictionary<int, PointVisit>()
      c.Add(3, Init 1L [])
      expected.Add ("a", a)
      expected.Add ("b", b)
      expected.Add ("c", c)

      Assert.That (counts.Count, Is.EqualTo 3)
      Assert.That (counts.["a"].Count, Is.EqualTo 1)
      Assert.That (counts.["b"].Count, Is.EqualTo 1)
      Assert.That (counts.["c"].Count, Is.EqualTo 1)
      Assert.That (counts.["a"].[0].Count, Is.EqualTo 1)
      Assert.That (counts.["a"].[0].Tracks, Is.Empty)
      Assert.That (counts.["b"].[1].Count, Is.EqualTo 1)
      Assert.That (counts.["b"].[1].Tracks, Is.Empty)
      Assert.That (counts.["c"].[3].Count, Is.EqualTo 1)
      Assert.That (counts.["c"].[3].Tracks, Is.Empty)

      if File.Exists(unique + ".acv") then File.Delete(unique + ".acv")

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let CollectShouldReportAsExpected() =
      Runner.init()
      try
        Runner.collect := true
        let counts = Dictionary<string, Dictionary<int, PointVisit>>()
        let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
        let unique = Path.Combine(where, Guid.NewGuid().ToString())

        let r =
          Runner.J.getMonitor counts unique (fun l ->
            use sink =
              new DeflateStream(File.OpenWrite(unique + ".0.acv"),
                                CompressionMode.Compress)
            use formatter = new BinaryWriter(sink)
            l
            |> List.mapi (fun i x ->
                 formatter.Write x
                 formatter.Write i
                 formatter.Write 0uy
                 x)
            |> List.length) [ "a"; "b"; String.Empty; "c" ]
        Assert.That(r, Is.EqualTo 0)
        Assert.That(File.Exists(unique + ".acv") |> not)
        let doc = Runner.J.loadReport(unique + ".acv")
        Assert.That(doc.Nodes(), Is.Empty)
        Assert.That(counts, Is.Empty)
      finally
        Runner.collect := false

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let JunkPayloadShouldReportAsExpected() =
      Runner.init()
      let counts = Dictionary<string, Dictionary<int, PointVisit>>()
      let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
      let unique = Path.Combine(where, Guid.NewGuid().ToString())
      let formatter = System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()

      let r =
        Runner.J.getMonitor counts unique (fun l ->
          use sink =
            new DeflateStream(File.OpenWrite(unique + ".0.acv"), CompressionMode.Compress)
          l
          |> List.mapi (fun i x ->
               formatter.Serialize(sink, (x, i, Base.Null, DateTime.UtcNow))
               x)
          |> List.length) [ "a"; "b"; String.Empty; "c" ]
      Assert.That(r, Is.EqualTo 4)
      Assert.That(File.Exists(unique + ".acv"))
      Assert.That(counts, Is.EquivalentTo [])

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let TrackingPayloadShouldReportAsExpected() =
      Runner.init()
      let counts = Dictionary<string, Dictionary<int, PointVisit>>()
      let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
      let unique = Path.Combine(where, Guid.NewGuid().ToString())

      let payloads0 =
        [ Base.Null
          Base.Call 17
          Base.Time 23L
          Base.Both { Time = 5L; Call = 42 }
          Base.Time 42L
          Base.Call 5 ]

      let pv = Init 42L (payloads0 |> List.tail)
      let table = Dictionary<string, Dictionary<int, PointVisit>>()
      table.Add("Extra", Dictionary<int, PointVisit>())
      table.["Extra"].Add(3, pv)
      let payloads = (Table table) :: payloads0

      let inputs = [ String.Empty; "a"; "b"; "c"; "d"; String.Empty; "e" ]

      let r =
        Runner.J.getMonitor counts unique (fun l ->
          use sink =
            new DeflateStream(File.OpenWrite(unique + ".0.acv"), CompressionMode.Compress)
          use formatter = new BinaryWriter(sink)
          l
          |> List.zip payloads
          |> List.mapi (fun i (y, x) ->
               formatter.Write x
               formatter.Write i
               match y with
               | Null -> formatter.Write(Base.Tag.Null |> byte)
               | Time t ->
                 formatter.Write(Base.Tag.Time |> byte)
                 formatter.Write(t)
               | Call t ->
                 formatter.Write(Base.Tag.Call |> byte)
                 formatter.Write(t)
               | Both b ->
                 formatter.Write(Base.Tag.Both |> byte)
                 formatter.Write(b.Time)
                 formatter.Write(b.Call)
               | Table t ->
                 formatter.Write(Base.Tag.Table |> byte)
                 t.Keys
                 |> Seq.iter (fun m -> formatter.Write m
                                       formatter.Write t.[m].Keys.Count
                                       t.[m].Keys
                                       |> Seq.iter (fun p -> formatter.Write p
                                                             let v = t.[m].[p]
                                                             formatter.Write v.Count
                                                             v.Tracks
                                                             |> Seq.iter (fun tx -> match tx with
                                                                                    | Time t ->
                                                                                      formatter.Write(Base.Tag.Time |> byte)
                                                                                      formatter.Write(t)
                                                                                    | Call t ->
                                                                                      formatter.Write(Base.Tag.Call |> byte)
                                                                                      formatter.Write(t)
                                                                                    | Both b ->
                                                                                      formatter.Write(Base.Tag.Both |> byte)
                                                                                      formatter.Write(b.Time)
                                                                                      formatter.Write(b.Call)
                                                                                    | _ -> tx |> (sprintf "%A") |> Assert.Fail)
                                                             formatter.Write(Base.Tag.Null |> byte)))
                 formatter.Write String.Empty
               x)
          |> List.length) inputs

      let expected = Dictionary<string, Dictionary<int, int64 * Base.Track list>>()
      let a = Dictionary<int, int64 * Base.Track list>()
      a.Add(1, (1L, []))
      let b = Dictionary<int, int64 * Base.Track list>()
      b.Add(2, (0L, [Call 17]))
      let c = Dictionary<int, int64 * Base.Track list>()
      c.Add(3, (0L, [Time 23L]))
      let d = Dictionary<int, int64 * Base.Track list>()
      d.Add(4, (0L, [Both { Time = 5L; Call = 42 } ]))
      let e = Dictionary<int, int64 * Base.Track list>()
      e.Add(6, (0L, [Call 5]))
      let f = Dictionary<int, int64 * Base.Track list>()
      f.Add(3, (42L, payloads0 |> List.tail))

      expected.Add ("a", a)
      expected.Add ("b", b)
      expected.Add ("c", c)
      expected.Add ("d", d)
      expected.Add ("e", e)
      expected.Add ("Extra", f)

      Assert.That(r, Is.EqualTo 7)
      Assert.That(File.Exists(unique + ".acv"))

      let result = Dictionary<string, Dictionary<int, int64 * Base.Track list>>()
      counts.Keys
      |> Seq.iter (fun k -> let inner = Dictionary<int, int64 * Base.Track list>()
                            result.Add(k, inner)
                            counts.[k].Keys
                            |> Seq.iter (fun k2 ->
                                let v = counts.[k].[k2]
                                inner.Add(k2, (v.Count, v.Tracks |> Seq.toList))
                            ))

      Assert.That(result, Is.EquivalentTo expected)

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let PointProcessShouldCaptureTimes() =
      Runner.init()
      let x = XmlDocument()
      x.LoadXml("<root />")
      let root = x.DocumentElement

      let hits =
        [ Base.Null
          Base.Call 17
          Base.Time 23L
          Base.Both { Time = 5L;  Call = 42 }
          Base.Time 42L
          Base.Time 5L ]
      Runner.J.pointProcess root hits
      Assert.That
        (x.DocumentElement.OuterXml,
         Is.EqualTo
           """<root><Times><Time time="5" vc="2" /><Time time="23" vc="1" /><Time time="42" vc="1" /></Times><TrackedMethodRefs><TrackedMethodRef uid="17" vc="1" /><TrackedMethodRef uid="42" vc="1" /></TrackedMethodRefs></root>""")

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let PostprocessShouldRestoreKnownOpenCoverState() =
      Runner.init()
      Counter.measureTime <- DateTime.ParseExact
                               ("2017-12-29T16:33:40.9564026+00:00", "o", null)
      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)
      let after = XmlDocument()
      after.Load stream
      let before = after.OuterXml
      after.DocumentElement.SelectNodes("//Summary")
      |> Seq.cast<XmlElement>
      |> Seq.iter (fun el ->
           el.SetAttribute("visitedBranchPoints", "0")
           el.SetAttribute("branchCoverage", "0")
           el.SetAttribute("visitedSequencePoints", "0")
           el.SetAttribute("sequenceCoverage", "0")
           el.SetAttribute("visitedClasses", "0")
           el.SetAttribute("visitedMethods", "0"))
      after.DocumentElement.SelectNodes("//Method")
      |> Seq.cast<XmlElement>
      |> Seq.iter (fun el ->
           el.SetAttribute("visited", "false")
           el.SetAttribute("sequenceCoverage", "0")
           el.SetAttribute("branchCoverage", "0"))
      after.DocumentElement.SelectNodes("//SequencePoint")
      |> Seq.cast<XmlElement>
      |> Seq.iter (fun el -> el.SetAttribute("bev", "0"))
      let empty = Dictionary<string, Dictionary<int, PointVisit>>()
      Runner.J.postProcess empty Base.ReportFormat.OpenCover after
      Assert.That
        (after.OuterXml.Replace("uspid=\"100663298", "uspid=\"13"), Is.EqualTo before,
         after.OuterXml)

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let PostprocessShouldRestoreKnownOpenCoverStateFromMono() =
      Counter.measureTime <- DateTime.ParseExact
                               ("2017-12-29T16:33:40.9564026+00:00", "o", null)
      let resource =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find
             (fun n -> n.EndsWith("HandRolledMonoCoverage.xml", StringComparison.Ordinal))
      use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
      let after = XmlDocument()
      after.Load stream
      let before = after.OuterXml
      after.DocumentElement.SelectNodes("//Summary")
      |> Seq.cast<XmlElement>
      |> Seq.iter (fun el ->
           el.SetAttribute("visitedBranchPoints", "0")
           el.SetAttribute("branchCoverage", "0")
           el.SetAttribute("visitedSequencePoints", "0")
           el.SetAttribute("sequenceCoverage", "0")
           el.SetAttribute("visitedClasses", "0")
           el.SetAttribute("visitedMethods", "0"))
      after.DocumentElement.SelectNodes("//Method")
      |> Seq.cast<XmlElement>
      |> Seq.iter (fun el ->
           el.SetAttribute("visited", "false")
           el.SetAttribute("sequenceCoverage", "0")
           el.SetAttribute("branchCoverage", "0"))
      after.DocumentElement.SelectNodes("//SequencePoint")
      |> Seq.cast<XmlElement>
      |> Seq.iter (fun el -> el.SetAttribute("bev", "0"))
      after.DocumentElement.SelectNodes("//MethodPoint")
      |> Seq.cast<XmlElement>
      |> Seq.toList
      |> List.iter (fun el -> el.RemoveAllAttributes())
      let visits = Dictionary<string, Dictionary<int, PointVisit>>()
      let visit = Dictionary<int, PointVisit>()
      visits.Add("6A-33-AA-93-82-ED-22-9D-F8-68-2C-39-5B-93-9F-74-01-76-00-9F", visit)
      visit.Add(100663297, Init 1L []) // should fill in the expected non-zero value
      visit.Add(100663298, Init 23L []) // should be ignored
      Runner.J.postProcess visits Base.ReportFormat.OpenCover after
      Assert.That
        (after.OuterXml.Replace("uspid=\"100663298", "uspid=\"13"), Is.EqualTo before,
         after.OuterXml)

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let PostprocessShouldRestoreDegenerateOpenCoverState() =
      Runner.init()
      Counter.measureTime <- DateTime.ParseExact
                               ("2017-12-29T16:33:40.9564026+00:00", "o", null)
      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)
      let after = XmlDocument()
      after.Load stream
      after.DocumentElement.SelectNodes("//Summary")
      |> Seq.cast<XmlElement>
      |> Seq.iter (fun el ->
           el.SetAttribute("visitedSequencePoints", "0")
           el.SetAttribute("sequenceCoverage", "0")
           if el.GetAttribute("minCrapScore") = "2.11" then
             el.SetAttribute("minCrapScore", "2.15")
             el.SetAttribute("maxCrapScore", "2.15"))
      after.DocumentElement.SelectNodes("//Method")
      |> Seq.cast<XmlElement>
      |> Seq.iter
           (fun el ->
           el.SetAttribute("sequenceCoverage", "0")
           if el.GetAttribute("crapScore") = "2.11" then
             el.SetAttribute("crapScore", "2.15"))
      after.DocumentElement.SelectNodes("//SequencePoint")
      |> Seq.cast<XmlElement>
      |> Seq.toList
      |> List.iter (fun el ->
           el
           |> el.ParentNode.RemoveChild
           |> ignore)
      after.DocumentElement.SelectNodes("//MethodPoint")
      |> Seq.cast<XmlElement>
      |> Seq.toList
      |> List.iter (fun el ->
           el
           |> el.ParentNode.RemoveChild
           |> ignore)
      let before = after.OuterXml.Replace("uspid=\"13", "uspid=\"100663298")
      after.DocumentElement.SelectNodes("//Summary")
      |> Seq.cast<XmlElement>
      |> Seq.iter (fun el ->
           el.SetAttribute("visitedBranchPoints", "0")
           el.SetAttribute("branchCoverage", "0")
           el.SetAttribute("visitedSequencePoints", "0")
           el.SetAttribute("sequenceCoverage", "0")
           el.SetAttribute("visitedClasses", "0")
           el.SetAttribute("visitedMethods", "0")
           if el.GetAttribute "minCrapScore"
              |> String.IsNullOrWhiteSpace
              |> not
           then
             el.SetAttribute("minCrapScore", "0")
             el.SetAttribute("maxCrapScore", "0"))
      after.DocumentElement.SelectNodes("//Method")
      |> Seq.cast<XmlElement>
      |> Seq.iter (fun el ->
           el.SetAttribute("visited", "false")
           el.SetAttribute("sequenceCoverage", "0")
           el.SetAttribute("branchCoverage", "0")
           if el.GetAttribute "crapScore"
              |> String.IsNullOrWhiteSpace
              |> not
           then el.SetAttribute("crapScore", "0"))
      let empty = Dictionary<string, Dictionary<int, PointVisit>>()
      Runner.J.postProcess empty Base.ReportFormat.OpenCover after
      Assert.That(after.OuterXml, Is.EqualTo before, after.OuterXml)

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let PostprocessShouldRestoreBranchOnlyOpenCoverState() =
      Runner.init()
      Counter.measureTime <- DateTime.ParseExact
                               ("2017-12-29T16:33:40.9564026+00:00", "o", null)
      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)
      let after = XmlDocument()
      after.Load stream
      after.DocumentElement.SelectNodes("//Summary")
      |> Seq.cast<XmlElement>
      |> Seq.iter (fun el ->
           el.SetAttribute("visitedSequencePoints", "0")
           el.SetAttribute("sequenceCoverage", "0")
           if el.GetAttribute("minCrapScore") = "2.11" then
             el.SetAttribute("minCrapScore", "2.15")
             el.SetAttribute("maxCrapScore", "2.15"))
      after.DocumentElement.SelectNodes("//Method")
      |> Seq.cast<XmlElement>
      |> Seq.iter
           (fun el ->
           el.SetAttribute("sequenceCoverage", "0")
           if el.GetAttribute("crapScore") = "2.11" then
             el.SetAttribute("crapScore", "2.15"))
      after.DocumentElement.SelectNodes("//SequencePoint")
      |> Seq.cast<XmlElement>
      |> Seq.toList
      |> List.iter (fun el ->
           el
           |> el.ParentNode.RemoveChild
           |> ignore)
      after.DocumentElement.SelectNodes("//MethodPoint")
      |> Seq.cast<XmlElement>
      |> Seq.iter (fun el -> el.SetAttribute("vc", "0"))
      let before =
        after.OuterXml.Replace("uspid=\"13", "uspid=\"100663298")
             .Replace("uspid=\"1\"", "uspid=\"100663297\"")
      after.DocumentElement.SelectNodes("//Summary")
      |> Seq.cast<XmlElement>
      |> Seq.iter (fun el ->
           el.SetAttribute("visitedBranchPoints", "0")
           el.SetAttribute("branchCoverage", "0")
           el.SetAttribute("visitedSequencePoints", "0")
           el.SetAttribute("sequenceCoverage", "0")
           el.SetAttribute("visitedClasses", "0")
           el.SetAttribute("visitedMethods", "0")
           if el.GetAttribute "minCrapScore"
              |> String.IsNullOrWhiteSpace
              |> not
           then
             el.SetAttribute("minCrapScore", "0")
             el.SetAttribute("maxCrapScore", "0"))
      after.DocumentElement.SelectNodes("//Method")
      |> Seq.cast<XmlElement>
      |> Seq.iter (fun el ->
           el.SetAttribute("visited", "false")
           el.SetAttribute("sequenceCoverage", "0")
           el.SetAttribute("branchCoverage", "0")
           if el.GetAttribute "crapScore"
              |> String.IsNullOrWhiteSpace
              |> not
           then el.SetAttribute("crapScore", "0"))
      let empty = Dictionary<string, Dictionary<int, PointVisit>>()
      Runner.J.postProcess empty Base.ReportFormat.OpenCover after
      Assert.That(after.OuterXml, Is.EqualTo before, after.OuterXml)

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let JunkTokenShouldDefaultZero() =
      Runner.init()
      let visits = Dictionary<int, PointVisit>()
      let key = " "
      let result = Runner.J.lookUpVisitsByToken key visits
      match (result.Count, result.Tracks |> Seq.toList) with
      | (0L, []) -> ()
      | _ -> Assert.Fail(sprintf "%A" result)

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let EmptyNCoverGeneratesExpectedSummary() =
      Runner.init()
      let report = XDocument()
      let builder = System.Text.StringBuilder()
      Runner.summary.Clear() |> ignore
      try
        lock Runner.summaryFormat (fun () ->
          Runner.summaryFormat <- Default
          let task = Collect()
          Output.info <- (fun s -> builder.Append(s).Append("|") |> ignore)
          let r = Runner.I.standardSummary report Base.ReportFormat.NCover 0
          Assert.That(r, Is.EqualTo 0)
          let expected = "Visited Classes 0 of 0 (n/a)|Visited Methods 0 of 0 (n/a)|Visited Points 0 of 0 (n/a)|"
          Assert.That
            (builder.ToString(),
             Is.EqualTo expected
               )
          let collected = task.Summary.Replace("\r",String.Empty).Replace("\n", "|")
          Assert.That(collected, Is.EqualTo expected))
      finally
        Output.info <- ignore

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let EmptyNCoverGeneratesExpectedTCSummary() =
      Runner.init()
      let report = XDocument()
      let builder = System.Text.StringBuilder()
      Runner.summary.Clear() |> ignore
      try
        lock Runner.summaryFormat (fun () ->
          Runner.summaryFormat <- B
          let task = Collect()
          Output.info <- (fun s -> builder.Append(s).Append("|") |> ignore)
          let r = Runner.I.standardSummary report Base.ReportFormat.NCover 0
          Assert.That(r, Is.EqualTo 0)
          let expected = "##teamcity[buildStatisticValue key='CodeCoverageAbsCTotal' value='0']|##teamcity[buildStatisticValue key='CodeCoverageAbsCCovered' value='0']|##teamcity[buildStatisticValue key='CodeCoverageAbsMTotal' value='0']|##teamcity[buildStatisticValue key='CodeCoverageAbsMCovered' value='0']|##teamcity[buildStatisticValue key='CodeCoverageAbsSTotal' value='0']|##teamcity[buildStatisticValue key='CodeCoverageAbsSCovered' value='0']|"
          let result = builder.ToString()
          Assert.That
            (result,
             Is.EqualTo expected,
             result
               )
          let collected = task.Summary.Replace("\r",String.Empty).Replace("\n", "|")
          Assert.That(collected, Is.EqualTo expected, collected))
      finally
        Output.info <- ignore

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let EmptyNCoverGeneratesExpectedSummaries() =
      Runner.init()
      let report = XDocument()
      let builder = System.Text.StringBuilder()
      Runner.summary.Clear() |> ignore
      try
        lock Runner.summaryFormat (fun () ->
          Runner.summaryFormat <- BPlus
          let task = Collect()
          Output.info <- (fun s -> builder.Append(s).Append("|") |> ignore)
          let r = Runner.I.standardSummary report Base.ReportFormat.NCover 0
          Assert.That(r, Is.EqualTo 0)
          let expected = "Visited Classes 0 of 0 (n/a)|Visited Methods 0 of 0 (n/a)|Visited Points 0 of 0 (n/a)|##teamcity[buildStatisticValue key='CodeCoverageAbsCTotal' value='0']|##teamcity[buildStatisticValue key='CodeCoverageAbsCCovered' value='0']|##teamcity[buildStatisticValue key='CodeCoverageAbsMTotal' value='0']|##teamcity[buildStatisticValue key='CodeCoverageAbsMCovered' value='0']|##teamcity[buildStatisticValue key='CodeCoverageAbsSTotal' value='0']|##teamcity[buildStatisticValue key='CodeCoverageAbsSCovered' value='0']|"
          let result = builder.ToString()
          Assert.That
            (result,
             Is.EqualTo expected,
             result
               )
          let collected = task.Summary.Replace("\r",String.Empty).Replace("\n", "|")
          Assert.That(collected, Is.EqualTo expected, collected))
      finally
        Output.info <- ignore

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let NCoverShouldGeneratePlausibleSummary() =
      Runner.init()
      let resource =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find (fun n -> n.EndsWith("SimpleCoverage.xml", StringComparison.Ordinal))
      use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
      let baseline = XDocument.Load(stream)
      let builder = System.Text.StringBuilder()
      try
        lock Runner.summaryFormat (fun () ->
          Runner.summaryFormat <- RPlus
          Output.info <- (fun s -> builder.Append(s).Append("|") |> ignore)
          let r =
            try
              Runner.threshold <- Some 25
              Runner.I.standardSummary baseline Base.ReportFormat.NCover 42
            finally
              Runner.threshold <- None
          // 80% coverage > threshold so expect return code coming in
          Assert.That(r, Is.EqualTo 42)
          Assert.That
            (builder.ToString(),
             Is.EqualTo
               ("Visited Classes 1 of 1 (100)|Visited Methods 1 of 1 (100)|Visited Points 8 of 10 (80)|" +
                "##teamcity[buildStatisticValue key='CodeCoverageAbsCTotal' value='1']|##teamcity[buildStatisticValue key='CodeCoverageAbsCCovered' value='1']|##teamcity[buildStatisticValue key='CodeCoverageAbsMTotal' value='1']|##teamcity[buildStatisticValue key='CodeCoverageAbsMCovered' value='1']|##teamcity[buildStatisticValue key='CodeCoverageAbsSTotal' value='10']|##teamcity[buildStatisticValue key='CodeCoverageAbsSCovered' value='8']|")
            ))
      finally
        Output.info <- ignore

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let EmptyOpenCoverGeneratesExpectedSummary() =
      Runner.init()
      let report = XDocument.Load(new System.IO.StringReader("""<CoverageSession>
  <Summary numSequencePoints="0" visitedSequencePoints="0" numBranchPoints="0" visitedBranchPoints="0" sequenceCoverage="0" branchCoverage="0" maxCyclomaticComplexity="0" minCyclomaticComplexity="1" visitedClasses="0" numClasses="0" visitedMethods="0" numMethods="0" />
</CoverageSession>"""))
      let builder = System.Text.StringBuilder()
      try
        lock Runner.summaryFormat (fun () ->
          Runner.summaryFormat <- Default
          Output.info <- (fun s -> builder.Append(s).Append("|") |> ignore)
          let r = Runner.I.standardSummary report Base.ReportFormat.OpenCover 0
          Assert.That(r, Is.EqualTo 0)
          Assert.That
            (builder.ToString(),
             Is.EqualTo
               ("Visited Classes 0 of 0 (n/a)|Visited Methods 0 of 0 (n/a)|"
                + "Visited Points 0 of 0 (0)|Visited Branches 0 of 0 (0)||"
                + "==== Alternative Results (includes all methods including those without corresponding source) ====|"
                + "Alternative Visited Classes 0 of 0 (n/a)|Alternative Visited Methods 0 of 0 (n/a)|")))
      finally
        Output.info <- ignore

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let EmptyOpenCoverGeneratesExpectedTCSummary() =
      Runner.init()
      let report = XDocument.Load(new System.IO.StringReader("""<CoverageSession>
  <Summary numSequencePoints="0" visitedSequencePoints="0" numBranchPoints="0" visitedBranchPoints="0" sequenceCoverage="0" branchCoverage="0" maxCyclomaticComplexity="0" minCyclomaticComplexity="1" visitedClasses="0" numClasses="0" visitedMethods="0" numMethods="0" />
</CoverageSession>"""))
      let builder = System.Text.StringBuilder()
      try
        lock Runner.summaryFormat (fun () ->
          Runner.summaryFormat <- B
          Output.info <- (fun s -> builder.Append(s).Append("|") |> ignore)
          let r = Runner.I.standardSummary report Base.ReportFormat.OpenCover 0
          Assert.That(r, Is.EqualTo 0)
          Assert.That
            (builder.ToString(),
             Is.EqualTo
               ("##teamcity[buildStatisticValue key='CodeCoverageAbsCTotal' value='0']|" +
                "##teamcity[buildStatisticValue key='CodeCoverageAbsCCovered' value='0']|" +
                "##teamcity[buildStatisticValue key='CodeCoverageAbsMTotal' value='0']|" +
                "##teamcity[buildStatisticValue key='CodeCoverageAbsMCovered' value='0']|" +
                "##teamcity[buildStatisticValue key='CodeCoverageAbsSTotal' value='0']|" +
                "##teamcity[buildStatisticValue key='CodeCoverageAbsSCovered' value='0']|" +
                "##teamcity[buildStatisticValue key='CodeCoverageAbsBTotal' value='0']|" +
                "##teamcity[buildStatisticValue key='CodeCoverageAbsBCovered' value='0']|")))
      finally
        Output.info <- ignore

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let EmptyOpenCoverGeneratesExpectedSummaries() =
      Runner.init()
      let report = XDocument.Load(new System.IO.StringReader("""<CoverageSession>
  <Summary numSequencePoints="0" visitedSequencePoints="0" numBranchPoints="0" visitedBranchPoints="0" sequenceCoverage="0" branchCoverage="0" maxCyclomaticComplexity="0" minCyclomaticComplexity="1" visitedClasses="0" numClasses="0" visitedMethods="0" numMethods="0" />
</CoverageSession>"""))
      let builder = System.Text.StringBuilder()
      try
        lock Runner.summaryFormat (fun () ->
          Runner.summaryFormat <- RPlus
          Output.info <- (fun s -> builder.Append(s).Append("|") |> ignore)
          let r = Runner.I.standardSummary report Base.ReportFormat.OpenCover 0
          Assert.That(r, Is.EqualTo 0)
          Assert.That
            (builder.ToString(),
             Is.EqualTo
               ("Visited Classes 0 of 0 (n/a)|Visited Methods 0 of 0 (n/a)|"
                + "Visited Points 0 of 0 (0)|Visited Branches 0 of 0 (0)||"
                + "==== Alternative Results (includes all methods including those without corresponding source) ====|"
                + "Alternative Visited Classes 0 of 0 (n/a)|Alternative Visited Methods 0 of 0 (n/a)|" +
                "##teamcity[buildStatisticValue key='CodeCoverageAbsCTotal' value='0']|" +
                "##teamcity[buildStatisticValue key='CodeCoverageAbsCCovered' value='0']|" +
                "##teamcity[buildStatisticValue key='CodeCoverageAbsMTotal' value='0']|" +
                "##teamcity[buildStatisticValue key='CodeCoverageAbsMCovered' value='0']|" +
                "##teamcity[buildStatisticValue key='CodeCoverageAbsSTotal' value='0']|" +
                "##teamcity[buildStatisticValue key='CodeCoverageAbsSCovered' value='0']|" +
                "##teamcity[buildStatisticValue key='CodeCoverageAbsRTotal' value='0']|" +
                "##teamcity[buildStatisticValue key='CodeCoverageAbsRCovered' value='0']|")))
      finally
        Output.info <- ignore

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let OpenCoverShouldGeneratePlausibleSummary() =
      Runner.init()
      let resource =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find
             (fun n -> n.EndsWith("Sample1WithOpenCover.xml", StringComparison.Ordinal))
      use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
      let baseline = XDocument.Load(stream)
      let builder = System.Text.StringBuilder()
      try
        lock Runner.summaryFormat (fun () ->
          Runner.summaryFormat <- BPlus
          Output.info <- (fun s -> builder.Append(s).Append("|") |> ignore)
          let r =
            try
              Runner.threshold <- Some 75
              Runner.I.standardSummary baseline Base.ReportFormat.OpenCover 23
            finally
              Runner.threshold <- None
          // 70% coverage < threshold so expect shortfall
          Assert.That(r, Is.EqualTo 5)
          Assert.That
            (builder.ToString(),
             Is.EqualTo
               ("Visited Classes 1 of 1 (100)|Visited Methods 1 of 1 (100)|"
                + "Visited Points 7 of 10 (70)|Visited Branches 2 of 3 (66.67)||"
                + "==== Alternative Results (includes all methods including those without corresponding source) ====|"
                + "Alternative Visited Classes 1 of 1 (100)|Alternative Visited Methods 1 of 2 (50)|"
                + "##teamcity[buildStatisticValue key='CodeCoverageAbsCTotal' value='1']|##teamcity[buildStatisticValue key='CodeCoverageAbsCCovered' value='1']|##teamcity[buildStatisticValue key='CodeCoverageAbsMTotal' value='1']|##teamcity[buildStatisticValue key='CodeCoverageAbsMCovered' value='1']|##teamcity[buildStatisticValue key='CodeCoverageAbsSTotal' value='10']|##teamcity[buildStatisticValue key='CodeCoverageAbsSCovered' value='7']|"
                + "##teamcity[buildStatisticValue key='CodeCoverageAbsBTotal' value='3']|##teamcity[buildStatisticValue key='CodeCoverageAbsBCovered' value='2']|")
               ))
      finally
        Output.info <- ignore

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let OpenCoverShouldGeneratePlausibleLcov() =
      Runner.init()
      let resource =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find
             (fun n -> n.EndsWith("Sample1WithOpenCover.xml", StringComparison.Ordinal))
      use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
      let baseline = XDocument.Load(stream)
      let unique =
        Path.Combine
          (Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName,
           Guid.NewGuid().ToString() + "/OpenCover.lcov")
      LCov.path := Some unique
      unique
      |> Path.GetDirectoryName
      |> Directory.CreateDirectory
      |> ignore
      try
        Runner.I.addLCovSummary()
        let summarize = Runner.I.summaries |> Seq.head
        let r = summarize baseline Base.ReportFormat.OpenCover 0
        Assert.That(r, Is.EqualTo 0)
        let result = File.ReadAllText unique
        let resource2 =
          Assembly.GetExecutingAssembly().GetManifestResourceNames()
          |> Seq.find (fun n -> n.EndsWith("OpenCover.lcov", StringComparison.Ordinal))
        use stream2 = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)
        use reader = new StreamReader(stream2)
        let expected = reader.ReadToEnd().Replace("\r", String.Empty).Replace("\\", "/")
        Assert.That
          (result.Replace("\r", String.Empty).Replace("\\", "/"), Is.EqualTo expected)
      finally
        LCov.path := None

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let NCoverShouldGeneratePlausibleLcov() =
      Runner.init()
      let resource =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find (fun n -> n.EndsWith("SimpleCoverage.xml", StringComparison.Ordinal))
      use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
      let baseline = XDocument.Load(stream)
      let unique =
        Path.Combine
          (Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName,
           Guid.NewGuid().ToString() + "/NCover.lcov")
      LCov.path := Some unique
      unique
      |> Path.GetDirectoryName
      |> Directory.CreateDirectory
      |> ignore
      try
        let r = LCov.summary baseline Base.ReportFormat.NCover 0
        Assert.That(r, Is.EqualTo 0)
        let result = File.ReadAllText unique
        let resource2 =
          Assembly.GetExecutingAssembly().GetManifestResourceNames()
          |> Seq.find (fun n -> n.EndsWith("NCover.lcov", StringComparison.Ordinal))
        use stream2 = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)
        use reader = new StreamReader(stream2)
        let expected = reader.ReadToEnd().Replace("\r", String.Empty).Replace("\\", "/")
        Assert.That
          (result.Replace("\r", String.Empty).Replace("\\", "/"), Is.EqualTo expected)
      finally
        LCov.path := None

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let NCoverShouldGeneratePlausibleLcovWithMissingFullName() =
      Runner.init()
      let resource =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find
             (fun n -> n.EndsWith("Sample1WithNCover.xml", StringComparison.Ordinal))
      use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
      let baseline = XDocument.Load(stream)
      let excluded = XName.Get "excluded"
      baseline.Descendants()
      |> Seq.iter (fun x -> if x.Attribute(excluded).IsNotNull then
                               x.SetAttributeValue(excluded, "false"))
      let unique =
        Path.Combine
          (Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName,
           Guid.NewGuid().ToString() + "/NCoverBugFix.lcov")
      LCov.path := Some unique
      unique
      |> Path.GetDirectoryName
      |> Directory.CreateDirectory
      |> ignore
      try
        let r = LCov.summary baseline Base.ReportFormat.NCover 0
        Assert.That(r, Is.EqualTo 0)
        let result = File.ReadAllText unique
        let resource2 =
          Assembly.GetExecutingAssembly().GetManifestResourceNames()
          |> Seq.find (fun n -> n.EndsWith("NCoverBugFix.lcov", StringComparison.Ordinal))
        use stream2 = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)
        use reader = new StreamReader(stream2)
        let expected = reader.ReadToEnd().Replace("\r", String.Empty).Replace("\\", "/")
        Assert.That
          (result.Replace("\r", String.Empty).Replace("\\", "/"), Is.EqualTo expected)
      finally
        LCov.path := None

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let MultiSortDoesItsThing() =
      Runner.init()
      let load f =
        use r = new System.IO.StringReader(f)
        XDocument.Load r

      let input =
        [ ("m", [ 3; 2; 1 ])
          ("a", [ 4; 9; 7 ])
          ("z", [ 3; 5 ]) ]
        |> List.map (fun (x, y) ->
             (x,
              y
              |> List.map (sprintf "<x><seqpnt line=\"%d\" /></x>"
                           >> load
                           >> (fun x -> x.Descendants(XName.Get "x") |> Seq.head))
              |> List.toSeq))
        |> List.toSeq

      let result =
        LCov.I.multiSortByNameAndStartLine input
        |> Seq.map (fun (f, ms) ->
             (f,
              ms
              |> Seq.map
                   (fun m ->
                   m.ToString().Replace("\r", String.Empty).Replace("\n", String.Empty)
                    .Replace("  <", "<"))
              |> Seq.toList))
        |> Seq.toList

      Assert.That
        (result,
         Is.EquivalentTo
           [ ("a",
              [ """<x><seqpnt line="4" /></x>"""
                """<x><seqpnt line="7" /></x>""";
                """<x><seqpnt line="9" /></x>""" ])

             ("m",
              [ """<x><seqpnt line="1" /></x>"""
                """<x><seqpnt line="2" /></x>""";
                """<x><seqpnt line="3" /></x>""" ])
             ("z", [ """<x><seqpnt line="3" /></x>"""
                     """<x><seqpnt line="5" /></x>""" ]) ])

    // Approved way is ugly -- https://docs.microsoft.com/en-us/visualstudio/code-quality/ca2202?view=vs-2019
    // Also, this rule is deprecated
    let private LoadSchema() =
      let schemas = new XmlSchemaSet()

      use stream =
          Assembly.GetExecutingAssembly()
#if LEGACY
                  .GetManifestResourceStream("coverage-04.xsd")
#else
                  .GetManifestResourceStream("altcover.tests.core.coverage-04.xsd")
#endif
      use reader = new StreamReader(stream)
      use xreader = XmlReader.Create(reader)
      schemas.Add(String.Empty, xreader) |> ignore
      schemas

    let private Validate result =
        let schema = LoadSchema ()
        let xmlDocument = XmlDocument()
        xmlDocument.LoadXml(result)
        xmlDocument.Schemas <- schema
        xmlDocument.Validate(null)

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let NCoverShouldGeneratePlausibleCobertura() =
      Runner.init()
      let resource =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find (fun n -> n.EndsWith("SimpleCoverage.xml", StringComparison.Ordinal))
      use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
      let baseline = XDocument.Load(stream)
      let unique =
        Path.Combine
          (Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName,
           Guid.NewGuid().ToString() + "/NCover.cobertura")
      Cobertura.path := Some unique
      unique
      |> Path.GetDirectoryName
      |> Directory.CreateDirectory
      |> ignore
      try
        Runner.I.addCoberturaSummary()
        let summarize = Runner.I.summaries |> Seq.head
        let r = summarize baseline Base.ReportFormat.NCover 0
        Assert.That(r, Is.EqualTo 0)
        let result =
          Regex.Replace(File.ReadAllText unique, """timestamp=\"\d*\">""",
                        """timestamp="xx">""").Replace("\\", "/")
        let resource2 =
          Assembly.GetExecutingAssembly().GetManifestResourceNames()
          |> Seq.find (fun n -> n.EndsWith("NCover.cobertura", StringComparison.Ordinal))
        use stream2 = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)
        use reader = new StreamReader(stream2)
        let expected =
          reader.ReadToEnd().Replace("\r", String.Empty).Replace("\\", "/")
                .Replace("""version="3.0.0.0""",
                         "version=\""
                         + typeof<TeamCityFormat>.Assembly.GetName().Version.ToString())
        Assert.That(result.Replace("\r", String.Empty), Is.EqualTo expected, result)
        Validate result
      finally
        Cobertura.path := None

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let NCoverShouldGeneratePlausibleCoberturaWithMissingFullName() =
      Runner.init()
      let resource =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find
             (fun n -> n.EndsWith("Sample1WithNCover.xml", StringComparison.Ordinal))
      use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
      let baseline = XDocument.Load(stream)
      let excluded = XName.Get "excluded"
      baseline.Descendants()
      |> Seq.iter (fun x -> if x.Attribute(excluded) .IsNotNull then
                               x.SetAttributeValue(excluded, "false"))
      let unique =
        Path.Combine
          (Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName,
           Guid.NewGuid().ToString() + "/NCover.cobertura")
      Cobertura.path := Some unique
      unique
      |> Path.GetDirectoryName
      |> Directory.CreateDirectory
      |> ignore
      try
        let r = Cobertura.summary baseline Base.ReportFormat.NCover 0
        Assert.That(r, Is.EqualTo 0)
        let result =
          Regex.Replace(File.ReadAllText unique, """timestamp=\"\d*\">""",
                        """timestamp="xx">""").Replace("\\", "/")
        let resource2 =
          Assembly.GetExecutingAssembly().GetManifestResourceNames()
          |> Seq.find
               (fun n -> n.EndsWith("NCoverBugFix.cobertura", StringComparison.Ordinal))
        use stream2 = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)
        use reader = new StreamReader(stream2)
        let expected =
          reader.ReadToEnd().Replace("\r", String.Empty).Replace("\\", "/")
                .Replace("""version="3.5.0.0""",
                         "version=\""
                         + typeof<TeamCityFormat>.Assembly.GetName().Version.ToString())
        Assert.That(result.Replace("\r", String.Empty), Is.EqualTo expected, result)
        Validate result
      finally
        Cobertura.path := None

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let OpenCoverShouldGeneratePlausibleCobertura() =
      Runner.init()
      let resource =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find
             (fun n -> n.EndsWith("Sample1WithOpenCover.xml", StringComparison.Ordinal))
      use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
      let baseline = XDocument.Load(stream)
      let unique =
        Path.Combine
          (Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName,
           Guid.NewGuid().ToString() + "/OpenCover.cobertura")
      Cobertura.path := Some unique
      unique
      |> Path.GetDirectoryName
      |> Directory.CreateDirectory
      |> ignore
      try
        let r = Cobertura.summary baseline Base.ReportFormat.OpenCover 0
        Assert.That(r, Is.EqualTo 0)
        let result =
          Regex.Replace
            (File.ReadAllText unique, """timestamp=\"\d*\">""", """timestamp="xx">""")
        let resource2 =
          Assembly.GetExecutingAssembly().GetManifestResourceNames()
          |> Seq.find
               (fun n -> n.EndsWith("OpenCover.cobertura", StringComparison.Ordinal))
        use stream2 = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)
        use reader = new StreamReader(stream2)
        let expected =
          reader.ReadToEnd().Replace("\r", String.Empty).Replace("\\", "/")
                .Replace("""version="3.0.0.0""",
                         "version=\""
                         + typeof<TeamCityFormat>.Assembly.GetName().Version.ToString())
        Assert.That
          (result.Replace("\r", String.Empty).Replace("\\", "/"), Is.EqualTo expected,
           result)
        Validate result
      finally
        Cobertura.path := None

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let ThresholdViolationShouldBeReported() =
      Runner.init()
      let saveErr = Output.error
      let saveSummaries = Runner.I.summaries
      let builder = System.Text.StringBuilder()
      let saved = Runner.threshold
      try
        Runner.I.summaries <- [ (fun _ _ _ -> 23) ]
        Output.error <- (fun s -> builder.Append(s).Append("|") |> ignore)
        Runner.threshold <- Some 42
        let delta = Runner.J.doSummaries (XDocument()) Base.ReportFormat.NCover 0
        Assert.That(delta, Is.EqualTo 23)
        Assert.That
          (builder.ToString(),
           Is.EqualTo "Coverage percentage achieved is 23% below the threshold of 42%.|")
      finally
        Output.error <- saveErr
        Runner.I.summaries <- saveSummaries
        Runner.threshold <- saved

#if NETCOREAPP2_0
#else
    [<Test>]
#endif
    let TryGetValueHandlesNull() =
      Runner.init()
      let dict : Dictionary<int, int> = null
      Assert.That(Runner.J.tryGetValue dict 0 |> fst, Is.False)