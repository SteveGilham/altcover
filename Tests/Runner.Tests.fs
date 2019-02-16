namespace Tests.Runner

open System
open System.Collections.Generic
open System.IO
open System.IO.Compression
open System.Reflection
open System.Text.RegularExpressions
open System.Xml
open System.Xml.Linq

open AltCover
open AltCover.Augment
open AltCover.Base
open Mono.Options
open NUnit.Framework

[<TestFixture>]
type AltCoverTests() =
  class

    // Base.fs
    [<Test>]
    member self.SafeDisposalProtects() =
      let obj1 =
        { new System.IDisposable with
            member x.Dispose() = ObjectDisposedException("Bang!") |> raise }
      Assist.SafeDispose obj1
      Assert.Pass()

    [<Test>]
    member self.JunkUspidGivesNegativeIndex() =
      let key = " "
      let index = Counter.FindIndexFromUspid 0 key
      Assert.That(index, Is.LessThan 0)

    [<Test>]
    member self.RealIdShouldIncrementCount() =
      let visits = new Dictionary<string, Dictionary<int, int * Track list>>()
      let key = " "
      Counter.AddVisit visits key 23 Null
      Assert.That(visits.Count, Is.EqualTo 1)
      Assert.That(visits.[key].Count, Is.EqualTo 1)
      Assert.That(visits.[key].[23], Is.EqualTo(1, []))

    [<Test>]
    member self.RealIdShouldIncrementList() =
      let visits = new Dictionary<string, Dictionary<int, int * Track list>>()
      let key = " "
      let payload = Time DateTime.UtcNow.Ticks
      Counter.AddVisit visits key 23 payload
      Assert.That(visits.Count, Is.EqualTo 1)
      Assert.That(visits.[key].Count, Is.EqualTo 1)
      Assert.That(visits.[key].[23], Is.EqualTo(0, [ payload ]))

    [<Test>]
    member self.DistinctIdShouldBeDistinct() =
      let visits = new Dictionary<string, Dictionary<int, int * Track list>>()
      let key = " "
      Counter.AddVisit visits key 23 Null
      Counter.AddVisit visits "key" 42 Null
      Assert.That(visits.Count, Is.EqualTo 2)

    [<Test>]
    member self.DistinctLineShouldBeDistinct() =
      let visits = new Dictionary<string, Dictionary<int, int * Track list>>()
      let key = " "
      Counter.AddVisit visits key 23 Null
      Counter.AddVisit visits key 42 Null
      Assert.That(visits.Count, Is.EqualTo 1)
      Assert.That(visits.[key].Count, Is.EqualTo 2)

    [<Test>]
    member self.RepeatVisitsShouldIncrementCount() =
      let visits = new Dictionary<string, Dictionary<int, int * Track list>>()
      let key = " "
      Counter.AddVisit visits key 23 Null
      Counter.AddVisit visits key 23 Null
      Assert.That(visits.[key].[23], Is.EqualTo(2, []))

    [<Test>]
    member self.RepeatVisitsShouldIncrementTotal() =
      let visits = new Dictionary<string, Dictionary<int, int * Track list>>()
      let key = " "
      let payload = Time DateTime.UtcNow.Ticks
      Counter.AddVisit visits key 23 Null
      Counter.AddVisit visits key 23 payload
      Assert.That(visits.[key].[23], Is.EqualTo(1, [ payload ]))

    member self.resource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find (fun n -> n.EndsWith("SimpleCoverage.xml", StringComparison.Ordinal))
    member self.resource2 =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find
           (fun n -> n.EndsWith("Sample1WithOpenCover.xml", StringComparison.Ordinal))

    [<Test>]
    member self.KnownModuleWithPayloadMakesExpectedChangeInOpenCover() =
      Counter.measureTime <- DateTime.ParseExact
                               ("2017-12-29T16:33:40.9564026+00:00", "o", null)
      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(self.resource2)
      let size = int stream.Length
      let buffer = Array.create size 0uy
      Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)
      use worker = new MemoryStream()
      use worker2 = new MemoryStream()
      worker.Write(buffer, 0, size)
      worker.Position <- 0L
      let payload = Dictionary<int, int * Track list>()
      [ 0..9 ] |> Seq.iter (fun i -> payload.[10 - i] <- (i + 1, []))
      [ 11..12 ] |> Seq.iter (fun i -> payload.[i ||| Counter.BranchFlag] <- (i - 10, []))
      let item = Dictionary<string, Dictionary<int, int * Track list>>()
      item.Add("7C-CD-66-29-A3-6C-6D-5F-A7-65-71-0E-22-7D-B2-61-B5-1F-65-9A", payload)
      Counter.UpdateReport ignore (fun _ _ -> ()) true item ReportFormat.OpenCover worker
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

    [<Test>]
    member self.FlushLeavesExpectedTraces() =
      let saved = Console.Out
      let here = Directory.GetCurrentDirectory()
      let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
      let unique = Path.Combine(where, Guid.NewGuid().ToString())
      let reportFile = Path.Combine(unique, "FlushLeavesExpectedTraces.xml")
      try
        let visits = new Dictionary<string, Dictionary<int, int * Track list>>()
        use stdout = new StringWriter()
        Console.SetOut stdout
        Directory.CreateDirectory(unique) |> ignore
        Directory.SetCurrentDirectory(unique)
        Counter.measureTime <- DateTime.ParseExact
                                 ("2017-12-29T16:33:40.9564026+00:00", "o", null)
        use stream =
          Assembly.GetExecutingAssembly().GetManifestResourceStream(self.resource)
        let size = int stream.Length
        let buffer = Array.create size 0uy
        Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)
        do use worker = new FileStream(reportFile, FileMode.CreateNew)
           worker.Write(buffer, 0, size)
           ()
        let payload = Dictionary<int, int * Track list>()
        [ 0..9 ] |> Seq.iter (fun i -> payload.[i] <- (i + 1, []))
        visits.["f6e3edb3-fb20-44b3-817d-f69d1a22fc2f"] <- payload
        Counter.DoFlush ignore (fun _ _ -> ()) true visits
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

    [<Test>]
    member self.FlushLeavesExpectedTracesWhenDiverted() =
      let saved = Console.Out
      let here = Directory.GetCurrentDirectory()
      let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
      let unique = Path.Combine(where, Guid.NewGuid().ToString())
      let reportFile = Path.Combine(unique, "FlushLeavesExpectedTraces.xml")
      let outputFile = Path.Combine(unique, "FlushLeavesExpectedTracesWhenDiverted.xml")
      try
        let visits = new Dictionary<string, Dictionary<int, int * Track list>>()
        use stdout = new StringWriter()
        Console.SetOut stdout
        Directory.CreateDirectory(unique) |> ignore
        Directory.SetCurrentDirectory(unique)
        Counter.measureTime <- DateTime.ParseExact
                                 ("2017-12-29T16:33:40.9564026+00:00", "o", null)
        use stream =
          Assembly.GetExecutingAssembly().GetManifestResourceStream(self.resource)
        let size = int stream.Length
        let buffer = Array.create size 0uy
        Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)
        do use worker = new FileStream(reportFile, FileMode.CreateNew)
           worker.Write(buffer, 0, size)
           ()
        let payload = Dictionary<int, int * Track list>()
        [ 0..9 ] |> Seq.iter (fun i -> payload.[i] <- (i + 1, []))
        visits.["f6e3edb3-fb20-44b3-817d-f69d1a22fc2f"] <- payload
        Counter.DoFlush ignore (fun _ _ -> ()) true visits
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
    [<Test>]
    member self.UsageIsAsExpected() =
      let options = Runner.DeclareOptions()
      let saved = Console.Error
      try
        use stderr = new StringWriter()
        Console.SetError stderr
        let empty = OptionSet()
        CommandLine.Usage("UsageError", empty, options)
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
  -?, --help, -h             Prints out the options.
"""
        Assert.That
          (result, Is.EqualTo(expected.Replace("\r\n", "\n")), "*" + result + "*")
      finally
        Console.SetError saved

    [<Test>]
    member self.ShouldLaunchWithExpectedOutput() =
      // Hack for running while instrumented
      let where = Assembly.GetExecutingAssembly().Location
      let path =
        Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), "_Mono/Sample1")
#if NETCOREAPP2_0
      let path' =
        if Directory.Exists path then path
        else
          Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), "../_Mono/Sample1")
#else
      let path' = path
#endif
      let files = Directory.GetFiles(path')

      let program =
        files
        |> Seq.filter (fun x -> x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
        |> Seq.head
      AltCover.ToConsole()
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
          CommandLine.Launch exe args
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

    [<Test>]
    member self.ShouldHaveExpectedOptions() =
      let options = Runner.DeclareOptions()
      Assert.That(options.Count, Is.EqualTo 11)
      Assert.That
        (options
         |> Seq.filter (fun x -> x.Prototype <> "<>")
         |> Seq.forall (fun x -> (String.IsNullOrWhiteSpace >> not) x.Description))
      Assert.That(options
                  |> Seq.filter (fun x -> x.Prototype = "<>")
                  |> Seq.length, Is.EqualTo 1)

    [<Test>]
    member self.ParsingJunkIsAnError() =
      let options = Runner.DeclareOptions()
      let parse = CommandLine.ParseCommandLine [| "/@thisIsNotAnOption" |] options
      match parse with
      | Right _ -> Assert.Fail()
      | Left(x, y) ->
        Assert.That(x, Is.EqualTo "UsageError")
        Assert.That(y, Is.SameAs options)

    [<Test>]
    member self.ParsingJunkAfterSeparatorIsExpected() =
      let options = Runner.DeclareOptions()
      let input = [| "--"; "/@thisIsNotAnOption"; "this should be OK" |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Left _ -> Assert.Fail()
      | Right(x, y) ->
        Assert.That(x, Is.EquivalentTo(input |> Seq.skip 1))
        Assert.That(y, Is.SameAs options)

    [<Test>]
    member self.ParsingHelpGivesHelp() =
      let options = Runner.DeclareOptions()
      let input = [| "--?" |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Left _ -> Assert.Fail()
      | Right(x, y) -> Assert.That(y, Is.SameAs options)
      match CommandLine.ProcessHelpOption parse with
      | Right _ -> Assert.Fail()
      | Left(x, y) ->
        Assert.That(x, Is.EqualTo "HelpText")
        Assert.That(y, Is.SameAs options)
      // a "not sticky" test
      lock Runner.executable (fun () ->
        Runner.executable := None
        match CommandLine.ParseCommandLine [| "/x"; "x" |] options
              |> CommandLine.ProcessHelpOption with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty))

    [<Test>]
    member self.ParsingErrorHelpGivesHelp() =
      let options = Runner.DeclareOptions()

      let input =
        [| "--zzz"
           Path.GetInvalidPathChars() |> String |]

      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left(x, y) ->
        Assert.That(x, Is.EqualTo "UsageError")
        Assert.That(y, Is.SameAs options)
      match CommandLine.ProcessHelpOption parse with
      | Right _ -> Assert.Fail()
      | Left(x, y) ->
        Assert.That(x, Is.EqualTo "UsageError")
        Assert.That(y, Is.SameAs options)
      // a "not sticky" test
      lock Runner.executable (fun () ->
        Runner.executable := None
        match CommandLine.ParseCommandLine [| "/x"; "x" |] options
              |> CommandLine.ProcessHelpOption with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty))

    [<Test>]
    member self.ParsingExeGivesExe() =
      lock Runner.executable (fun () ->
        try
          Runner.executable := None
          let options = Runner.DeclareOptions()
          let unique = "some exe"
          let input = [| "-x"; unique |]
          let parse = CommandLine.ParseCommandLine input options
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

    [<Test>]
    member self.ParsingMultipleExeGivesFailure() =
      lock Runner.executable (fun () ->
        try
          Runner.executable := None
          let options = Runner.DeclareOptions()
          let unique = Guid.NewGuid().ToString()

          let input =
            [| "-x"
               unique
               "/x"
               unique.Replace("-", "+") |]

          let parse = CommandLine.ParseCommandLine input options
          match parse with
          | Right _ -> Assert.Fail()
          | Left(x, y) ->
            Assert.That(y, Is.SameAs options)
            Assert.That(x, Is.EqualTo "UsageError")
        finally
          Runner.executable := None)

    [<Test>]
    member self.ParsingNoExeGivesFailure() =
      lock Runner.executable (fun () ->
        try
          Runner.executable := None
          let options = Runner.DeclareOptions()
          let blank = " "
          let input = [| "-x"; blank |]
          let parse = CommandLine.ParseCommandLine input options
          match parse with
          | Right _ -> Assert.Fail()
          | Left(x, y) ->
            Assert.That(y, Is.SameAs options)
            Assert.That(x, Is.EqualTo "UsageError")
        finally
          Runner.executable := None)

    [<Test>]
    member self.ParsingWorkerGivesWorker() =
      try
        Runner.workingDirectory <- None
        let options = Runner.DeclareOptions()
        let unique = Path.GetFullPath(".")
        let input = [| "-w"; unique |]
        let parse = CommandLine.ParseCommandLine input options
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

    [<Test>]
    member self.ParsingMultipleWorkerGivesFailure() =
      try
        Runner.workingDirectory <- None
        let options = Runner.DeclareOptions()

        let input =
          [| "-w"
             Path.GetFullPath(".")
             "/w"
             Path.GetFullPath("..") |]

        let parse = CommandLine.ParseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.workingDirectory <- None

    [<Test>]
    member self.ParsingBadWorkerGivesFailure() =
      try
        Runner.workingDirectory <- None
        let options = Runner.DeclareOptions()
        let unique = Guid.NewGuid().ToString().Replace("-", "*")
        let input = [| "-w"; unique |]
        let parse = CommandLine.ParseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.workingDirectory <- None

    [<Test>]
    member self.ParsingNoWorkerGivesFailure() =
      try
        Runner.workingDirectory <- None
        let options = Runner.DeclareOptions()
        let input = [| "-w" |]
        let parse = CommandLine.ParseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.workingDirectory <- None

    [<Test>]
    member self.ParsingRecorderGivesRecorder() =
      try
        Runner.recordingDirectory <- None
        let options = Runner.DeclareOptions()
        let unique = Path.GetFullPath(".")
        let input = [| "-r"; unique |]
        let parse = CommandLine.ParseCommandLine input options
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

    [<Test>]
    member self.ParsingMultipleRecorderGivesFailure() =
      try
        Runner.recordingDirectory <- None
        let options = Runner.DeclareOptions()

        let input =
          [| "-r"
             Path.GetFullPath(".")
             "/r"
             Path.GetFullPath("..") |]

        let parse = CommandLine.ParseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.recordingDirectory <- None

    [<Test>]
    member self.ParsingBadRecorderGivesFailure() =
      try
        Runner.recordingDirectory <- None
        let options = Runner.DeclareOptions()
        let unique = Guid.NewGuid().ToString().Replace("-", "*")
        let input = [| "-r"; unique |]
        let parse = CommandLine.ParseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.recordingDirectory <- None

    [<Test>]
    member self.ParsingNoRecorderGivesFailure() =
      try
        Runner.recordingDirectory <- None
        let options = Runner.DeclareOptions()
        let input = [| "-r" |]
        let parse = CommandLine.ParseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.recordingDirectory <- None

    [<Test>]
    member self.ParsingCollectGivesCollect() =
      try
        Runner.collect := false
        let options = Runner.DeclareOptions()
        let input = [| "--collect" |]
        let parse = CommandLine.ParseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        Assert.That(!Runner.collect, Is.True)
      finally
        Runner.collect := false

    [<Test>]
    member self.ParsingMultipleCollectGivesFailure() =
      try
        Runner.collect := false
        let options = Runner.DeclareOptions()
        let input = [| "--collect"; "--collect" |]
        let parse = CommandLine.ParseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.collect := false

    [<Test>]
    member self.ParsingLcovGivesLcov() =
      lock LCov.path (fun () ->
        try
          LCov.path := None
          Runner.Summaries <- [ Runner.StandardSummary ]
          let options = Runner.DeclareOptions()
          let unique = "some exe"
          let input = [| "-l"; unique |]
          let parse = CommandLine.ParseCommandLine input options
          match parse with
          | Left _ -> Assert.Fail()
          | Right(x, y) ->
            Assert.That(y, Is.SameAs options)
            Assert.That(x, Is.Empty)
          match !LCov.path with
          | None -> Assert.Fail()
          | Some x -> Assert.That(Path.GetFileName x, Is.EqualTo unique)
          Assert.That(Runner.Summaries.Length, Is.EqualTo 2)
        finally
          Runner.Summaries <- [ Runner.StandardSummary ]
          LCov.path := None)

    [<Test>]
    member self.ParsingMultipleLcovGivesFailure() =
      lock LCov.path (fun () ->
        try
          LCov.path := None
          Runner.Summaries <- [ Runner.StandardSummary ]
          let options = Runner.DeclareOptions()
          let unique = Guid.NewGuid().ToString()

          let input =
            [| "-l"
               unique
               "/l"
               unique.Replace("-", "+") |]

          let parse = CommandLine.ParseCommandLine input options
          match parse with
          | Right _ -> Assert.Fail()
          | Left(x, y) ->
            Assert.That(y, Is.SameAs options)
            Assert.That(x, Is.EqualTo "UsageError")
        finally
          Runner.Summaries <- [ Runner.StandardSummary ]
          LCov.path := None)

    [<Test>]
    member self.ParsingNoLcovGivesFailure() =
      lock LCov.path (fun () ->
        try
          LCov.path := None
          Runner.Summaries <- [ Runner.StandardSummary ]
          let options = Runner.DeclareOptions()
          let blank = " "
          let input = [| "-l"; blank |]
          let parse = CommandLine.ParseCommandLine input options
          match parse with
          | Right _ -> Assert.Fail()
          | Left(x, y) ->
            Assert.That(y, Is.SameAs options)
            Assert.That(x, Is.EqualTo "UsageError")
        finally
          Runner.Summaries <- [ Runner.StandardSummary ]
          LCov.path := None)

    [<Test>]
    member self.ParsingThresholdGivesThreshold() =
      try
        Runner.threshold <- None
        let options = Runner.DeclareOptions()
        let input = [| "-t"; "57" |]
        let parse = CommandLine.ParseCommandLine input options
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

    [<Test>]
    member self.ParsingMultipleThresholdGivesFailure() =
      try
        Runner.threshold <- None
        let options = Runner.DeclareOptions()
        let input = [| "-t"; "23"; "/t"; "42" |]
        let parse = CommandLine.ParseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.threshold <- None

    [<Test>]
    member self.ParsingBadThresholdGivesFailure() =
      try
        Runner.threshold <- None
        let options = Runner.DeclareOptions()
        let input = [| "-t"; "-111" |]
        let parse = CommandLine.ParseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.threshold <- None

    [<Test>]
    member self.ParsingEmptyThresholdGivesFailure() =
      try
        Runner.threshold <- None
        let options = Runner.DeclareOptions()
        let input = [| "-t"; "  " |]
        let parse = CommandLine.ParseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.threshold <- None

    [<Test>]
    member self.ParsingNoThresholdGivesFailure() =
      try
        Runner.threshold <- None
        let options = Runner.DeclareOptions()
        let input = [| "-t" |]
        let parse = CommandLine.ParseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.threshold <- None

    [<Test>]
    member self.ParsingCoberturaGivesCobertura() =
      lock Cobertura.path (fun () ->
        try
          Cobertura.path := None
          Runner.Summaries <- [ Runner.StandardSummary ]
          let options = Runner.DeclareOptions()
          let unique = "some exe"
          let input = [| "-c"; unique |]
          let parse = CommandLine.ParseCommandLine input options
          match parse with
          | Left _ -> Assert.Fail()
          | Right(x, y) ->
            Assert.That(y, Is.SameAs options)
            Assert.That(x, Is.Empty)
          match !Cobertura.path with
          | None -> Assert.Fail()
          | Some x -> Assert.That(Path.GetFileName x, Is.EqualTo unique)
          Assert.That(Runner.Summaries.Length, Is.EqualTo 2)
        finally
          Runner.Summaries <- [ Runner.StandardSummary ]
          Cobertura.path := None)

    [<Test>]
    member self.ParsingMultipleCoberturaGivesFailure() =
      lock Cobertura.path (fun () ->
        try
          Cobertura.path := None
          Runner.Summaries <- [ Runner.StandardSummary ]
          let options = Runner.DeclareOptions()
          let unique = Guid.NewGuid().ToString()

          let input =
            [| "-c"
               unique
               "/c"
               unique.Replace("-", "+") |]

          let parse = CommandLine.ParseCommandLine input options
          match parse with
          | Right _ -> Assert.Fail()
          | Left(x, y) ->
            Assert.That(y, Is.SameAs options)
            Assert.That(x, Is.EqualTo "UsageError")
        finally
          Runner.Summaries <- [ Runner.StandardSummary ]
          Cobertura.path := None)

    [<Test>]
    member self.ParsingNoCoberturaGivesFailure() =
      lock Cobertura.path (fun () ->
        try
          Cobertura.path := None
          Runner.Summaries <- [ Runner.StandardSummary ]
          let options = Runner.DeclareOptions()
          let blank = " "
          let input = [| "-c"; blank |]
          let parse = CommandLine.ParseCommandLine input options
          match parse with
          | Right _ -> Assert.Fail()
          | Left(x, y) ->
            Assert.That(y, Is.SameAs options)
            Assert.That(x, Is.EqualTo "UsageError")
        finally
          Runner.Summaries <- [ Runner.StandardSummary ]
          Cobertura.path := None)

    [<Test>]
    member self.ParsingOutputGivesOutput() =
      try
        Runner.output <- None
        let options = Runner.DeclareOptions()
        let unique = Guid.NewGuid().ToString()
        let input = [| "-o"; unique |]
        let parse = CommandLine.ParseCommandLine input options
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

    [<Test>]
    member self.ParsingMultipleOutputGivesFailure() =
      try
        Runner.output <- None
        Runner.collect := false
        let options = Runner.DeclareOptions()
        let unique = Guid.NewGuid().ToString()

        let input =
          [| "-o"
             unique
             "/o"
             unique.Replace("-", "+") |]

        let parse = CommandLine.ParseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.output <- None

    [<Test>]
    member self.ParsingNoOutputGivesFailure() =
      try
        Runner.output <- None
        let options = Runner.DeclareOptions()
        let blank = " "
        let input = [| "-o"; blank |]
        let parse = CommandLine.ParseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.output <- None

    [<Test>]
    member self.ParsingDropGivesDrop() =
      try
        CommandLine.dropReturnCode := false
        let options = Main.DeclareOptions()
        let input = [| "--dropReturnCode" |]
        let parse = CommandLine.ParseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        Assert.That(!CommandLine.dropReturnCode, Is.True)
      finally
        CommandLine.dropReturnCode := false

    [<Test>]
    member self.ParsingMultipleDropGivesFailure() =
      try
        CommandLine.dropReturnCode := false
        let options = Main.DeclareOptions()
        let input = [| "--dropReturnCode"; "--dropReturnCode" |]
        let parse = CommandLine.ParseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        CommandLine.dropReturnCode := false

    [<Test>]
    member self.ShouldRequireExe() =
      lock Runner.executable (fun () ->
        try
          Runner.executable := None
          let options = Runner.DeclareOptions()
          let parse = Runner.RequireExe(Right([], options))
          match parse with
          | Right _ -> Assert.Fail()
          | Left(x, y) ->
            Assert.That(y, Is.SameAs options)
            Assert.That(x, Is.EqualTo "UsageError")
        finally
          Runner.executable := None)

    [<Test>]
    member self.ShouldAcceptExe() =
      lock Runner.executable (fun () ->
        try
          Runner.executable := Some "xxx"
          let options = Runner.DeclareOptions()
          let parse = Runner.RequireExe(Right([ "b" ], options))
          match parse with
          | Right(x :: y, z) ->
            Assert.That(z, Is.SameAs options)
            Assert.That(x, Is.EqualTo "xxx")
            Assert.That(y, Is.EquivalentTo [ "b" ])
          | _ -> Assert.Fail()
        finally
          Runner.executable := None)

    [<Test>]
    member self.ShouldRequireCollectIfNotExe() =
      lock Runner.executable (fun () ->
        try
          Runner.executable := None
          Runner.collect := true
          let options = Runner.DeclareOptions()
          let parse = Runner.RequireExe(Right([ "a"; "b" ], options))
          match parse with
          | Right([], z) -> Assert.That(z, Is.SameAs options)
          | _ -> Assert.Fail()
        finally
          Runner.collect := false
          Runner.executable := None)

    [<Test>]
    member self.ShouldRejectExeIfCollect() =
      lock Runner.executable (fun () ->
        try
          Runner.executable := Some "xxx"
          Runner.collect := true
          let options = Runner.DeclareOptions()
          let parse = Runner.RequireExe(Right([ "b" ], options))
          match parse with
          | Right _ -> Assert.Fail()
          | Left(x, y) ->
            Assert.That(y, Is.SameAs options)
            Assert.That(x, Is.EqualTo "UsageError")
        finally
          Runner.collect := false
          Runner.executable := None)

    [<Test>]
    member self.ShouldRequireWorker() =
      try
        Runner.workingDirectory <- None
        let options = Runner.DeclareOptions()
        let input = (Right([], options))
        let parse = Runner.RequireWorker input
        match parse with
        | Right _ ->
          Assert.That(parse, Is.SameAs input)
          Assert.That(Option.isSome Runner.workingDirectory)
        | _ -> Assert.Fail()
      finally
        Runner.workingDirectory <- None

    [<Test>]
    member self.ShouldAcceptWorker() =
      try
        Runner.workingDirectory <- Some "ShouldAcceptWorker"
        let options = Runner.DeclareOptions()
        let input = (Right([], options))
        let parse = Runner.RequireWorker input
        match parse with
        | Right _ ->
          Assert.That(parse, Is.SameAs input)
          Assert.That(Runner.workingDirectory, Is.EqualTo(Some "ShouldAcceptWorker"))
        | _ -> Assert.Fail()
      finally
        Runner.workingDirectory <- None

    [<Test>]
    member self.ShouldRequireRecorder() =
      try
        Runner.recordingDirectory <- None
        let options = Runner.DeclareOptions()
        let input = (Right([], options))
        let parse = Runner.RequireRecorder input
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.recordingDirectory <- None

    [<Test>]
    member self.ShouldRequireRecorderDll() =
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
        let options = Runner.DeclareOptions()
        let input = (Right([], options))
        let parse = Runner.RequireRecorder input
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.recordingDirectory <- None

    [<Test>]
    member self.ShouldAcceptRecorder() =
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
        let options = Runner.DeclareOptions()
        let input = (Right([], options))
        let parse = Runner.RequireRecorder input
        match parse with
        | Right _ -> Assert.That(parse, Is.SameAs input)
        | _ -> Assert.Fail()
      finally
        Runner.recordingDirectory <- None

    [<Test>]
    member self.ShouldHandleReturnCodes() =
      // Hack for running while instrumented
      let where = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
#if NETCOREAPP2_0
      let path = Path.Combine(where, "Sample12.dll")
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

      let r = CommandLine.ProcessTrailingArguments args <| DirectoryInfo(where)
      Assert.That(r, Is.EqualTo 0)

      let r2 = CommandLine.ProcessTrailingArguments (args @ [ "1"; "2" ]) <| DirectoryInfo(where)
      Assert.That(r2, Is.EqualTo 2)

      try
        CommandLine.dropReturnCode := true
        let r0 = CommandLine.ProcessTrailingArguments (args @ [ "1"; "2" ]) <| DirectoryInfo(where)
        Assert.That(r0, Is.EqualTo 0)
      finally
        CommandLine.dropReturnCode := false

    [<Test>]
    member self.ShouldProcessTrailingArguments() =
      // Hack for running while instrumented
      let where = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
      let path =
        Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), "_Mono/Sample1")
#if NETCOREAPP2_0
      let path' =
        if Directory.Exists path then path
        else
          Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), "../_Mono/Sample1")
#else
      let path' = path
#endif
      let files = Directory.GetFiles(path')

      let program =
        files
        |> Seq.filter (fun x -> x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
        |> Seq.head
      AltCover.ToConsole()
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

        let r = CommandLine.ProcessTrailingArguments args <| DirectoryInfo(where)
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

    [<Test>]
    member self.ShouldNoOp() =
      let where = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
      let r = CommandLine.ProcessTrailingArguments [] <| DirectoryInfo(where)
      Assert.That(r, Is.EqualTo 0)

    [<Test>]
    member self.ErrorResponseIsAsExpected() =
      let saved = Console.Error
      try
        use stderr = new StringWriter()
        Console.SetError stderr
        let unique = Guid.NewGuid().ToString()
        let main =
          typeof<Tracer>.Assembly.GetType("AltCover.AltCover")
            .GetMethod("Main", BindingFlags.NonPublic ||| BindingFlags.Static)
        let returnCode = main.Invoke(null, [| [| "RuNN"; "-r"; unique |] |])
        Assert.That(returnCode, Is.EqualTo 255)
        let result = stderr.ToString().Replace("\r\n", "\n")
        let expected = "\"RuNN\" \"-r\" \"" + unique + "\"\n"
                       + "--recorderDirectory : Directory " + unique + " not found\n" + """Error - usage is:
  -i, --inputDirectory=VALUE Optional: The folder containing assemblies to
                               instrument (default: current directory)
  -o, --outputDirectory=VALUE
                             Optional: The folder to receive the instrumented
                               assemblies and their companions (default: sub-
                               folder '__Instrumented' of the current directory;
                                or '__Saved' if 'inplace' is set)
  -y, --symbolDirectory=VALUE
                             Optional, multiple: Additional directory to search
                               for matching symbols for the assemblies in the
                               input directory
"""
#if NETCOREAPP2_0
                     + """  -d, --dependency=VALUE     Optional,multiple: assembly path to resolve
                               missing reference.
"""
#else
                     + """  -k, --key=VALUE            Optional, multiple: any other strong-name key to
                               use
      --sn, --strongNameKey=VALUE
                             Optional: The default strong naming key to apply
                               to instrumented assemblies (default: None)
"""
#endif
                     + """  -x, --xmlReport=VALUE      Optional: The output report template file (default:
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
  -?, --help, -h             Prints out the options.
"""
        Assert.That
          (result.Replace("\r\n", "\n"), Is.EqualTo(expected.Replace("\r\n", "\n")))
      finally
        Console.SetError saved

    [<Test>]
    member self.ShouldGetStringConstants() =
      let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
      let save = Runner.RecorderName
      lock self (fun () ->
        try
          Runner.recordingDirectory <- Some where
          Runner.RecorderName <- "AltCover.Recorder.dll"
          let instance = Runner.RecorderInstance() |> snd
          Assert.That
            (instance.FullName, Is.EqualTo "AltCover.Recorder.Instance",
             "should be the instance")
          let token =
            (Runner.GetMethod instance "get_Token") |> Runner.GetFirstOperandAsString
          Assert.That(token, Is.EqualTo "AltCover", "should be plain token")
          let report =
            (Runner.GetMethod instance "get_ReportFile") |> Runner.GetFirstOperandAsString
          Assert.That
            (report, Is.EqualTo "Coverage.Default.xml", "should be default coverage file")
        finally
          Runner.recordingDirectory <- None
          Runner.RecorderName <- save)

    [<Test>]
    member self.ShouldProcessPayload() =
      // Hack for running while instrumented
      let where = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
      let path =
        Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), "_Mono/Sample1")
#if NETCOREAPP2_0
      let path' =
        if Directory.Exists path then path
        else
          Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), "../_Mono/Sample1")
#else
      let path' = path
#endif
      let files = Directory.GetFiles(path')

      let program =
        files
        |> Seq.filter (fun x -> x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
        |> Seq.head
      AltCover.ToConsole()
      let saved = (Console.Out, Console.Error)
      Runner.workingDirectory <- Some where
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

        let r = Runner.GetPayload args
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

    [<Test>]
    member self.WriteLeavesExpectedTraces() =
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
          Assembly.GetExecutingAssembly().GetManifestResourceStream(self.resource)
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

        let counts = Dictionary<string, Dictionary<int, int * Base.Track list>>()
        hits
        |> Seq.iter
             (fun (moduleId, hitPointId, hit) ->
             AltCover.Base.Counter.AddVisit counts moduleId hitPointId hit)

        Runner.DoReport counts AltCover.Base.ReportFormat.NCover reportFile None |> ignore
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

    [<Test>]
    member self.NullPayloadShouldReportNothing() =
      let counts = Dictionary<string, Dictionary<int, int * Base.Track list>>()
      let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
      let unique = Path.Combine(where, Guid.NewGuid().ToString())
      do use s = File.Create(unique + ".0.acv")
         s.Close()
      let r = Runner.GetMonitor counts unique List.length []
      Assert.That(r, Is.EqualTo 0)
      Assert.That(File.Exists(unique + ".acv"))
      Assert.That(counts, Is.Empty)

    [<Test>]
    member self.ActivePayloadShouldReportAsExpected() =
      let counts = Dictionary<string, Dictionary<int, int * Base.Track list>>()
      let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
      let unique = Path.Combine(where, Guid.NewGuid().ToString())

      let r =
        Runner.GetMonitor counts unique (fun l ->
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
      let expected = Dictionary<string, Dictionary<int, int * Base.Track list>>()
      let a = Dictionary<int, int * Base.Track list>()
      a.Add(0, (1, []))
      let b = Dictionary<int, int * Base.Track list>()
      b.Add(1, (1, []))
      let c = Dictionary<int, int * Base.Track list>()
      c.Add(3, (1, []))
      expected.Add ("a", a)
      expected.Add ("b", b)
      expected.Add ("c", c)

      Assert.That(counts,
                  Is.EquivalentTo expected)
      if File.Exists(unique + ".acv") then File.Delete(unique + ".acv")

    [<Test>]
    member self.CollectShouldReportAsExpected() =
      try
        Runner.collect := true
        let counts = Dictionary<string, Dictionary<int, int * Base.Track list>>()
        let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
        let unique = Path.Combine(where, Guid.NewGuid().ToString())

        let r =
          Runner.GetMonitor counts unique (fun l ->
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
        let doc = Runner.LoadReport(unique + ".acv")
        Assert.That(doc.Nodes(), Is.Empty)
        Assert.That(counts, Is.Empty)
      finally
        Runner.collect := false

    [<Test>]
    member self.JunkPayloadShouldReportAsExpected() =
      let counts = Dictionary<string, Dictionary<int, int * Base.Track list>>()
      let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
      let unique = Path.Combine(where, Guid.NewGuid().ToString())
      let formatter = System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()

      let r =
        Runner.GetMonitor counts unique (fun l ->
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

    [<Test>]
    member self.TrackingPayloadShouldReportAsExpected() =
      let counts = Dictionary<string, Dictionary<int, int * Base.Track list>>()
      let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
      let unique = Path.Combine(where, Guid.NewGuid().ToString())

      let payloads =
        [ Base.Null
          Base.Call 17
          Base.Time 23L
          Base.Both(5L, 42)
          Base.Time 42L
          Base.Call 5 ]

      let inputs = [ "a"; "b"; "c"; "d"; String.Empty; "e" ]

      let r =
        Runner.GetMonitor counts unique (fun l ->
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
               | Both(t', t) ->
                 formatter.Write(Base.Tag.Both |> byte)
                 formatter.Write(t')
                 formatter.Write(t)
               x)
          |> List.length) inputs

      let expected = Dictionary<string, Dictionary<int, int * Base.Track list>>()
      let a = Dictionary<int, int * Base.Track list>()
      a.Add(0, (1, []))
      let b = Dictionary<int, int * Base.Track list>()
      b.Add(1, (0, [Call 17]))
      let c = Dictionary<int, int * Base.Track list>()
      c.Add(2, (0, [Time 23L]))
      let d = Dictionary<int, int * Base.Track list>()
      d.Add(3, (0, [Both (5L, 42)]))
      let e = Dictionary<int, int * Base.Track list>()
      e.Add(5, (0, [Call 5]))
      expected.Add ("a", a)
      expected.Add ("b", b)
      expected.Add ("c", c)
      expected.Add ("d", d)
      expected.Add ("e", e)

      Assert.That(r, Is.EqualTo 6)
      Assert.That(File.Exists(unique + ".acv"))
      Assert.That(counts, Is.EquivalentTo expected)

    [<Test>]
    member self.PointProcessShouldCaptureTimes() =
      let x = XmlDocument()
      x.LoadXml("<root />")
      let root = x.DocumentElement

      let hits =
        [ Base.Null
          Base.Call 17
          Base.Time 23L
          Base.Both(5L, 42)
          Base.Time 42L
          Base.Time 5L ]
      Runner.PointProcess root hits
      Assert.That
        (x.DocumentElement.OuterXml,
         Is.EqualTo
           """<root><Times><Time time="5" vc="2" /><Time time="23" vc="1" /><Time time="42" vc="1" /></Times><TrackedMethodRefs><TrackedMethodRef uid="17" vc="1" /><TrackedMethodRef uid="42" vc="1" /></TrackedMethodRefs></root>""")

    [<Test>]
    member self.PostprocessShouldRestoreKnownOpenCoverState() =
      Counter.measureTime <- DateTime.ParseExact
                               ("2017-12-29T16:33:40.9564026+00:00", "o", null)
      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(self.resource2)
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
      let empty = Dictionary<string, Dictionary<int, int * Track list>>()
      Runner.PostProcess empty Base.ReportFormat.OpenCover after
      Assert.That
        (after.OuterXml.Replace("uspid=\"100663298", "uspid=\"13"), Is.EqualTo before,
         after.OuterXml)

    [<Test>]
    member self.PostprocessShouldRestoreKnownOpenCoverStateFromMono() =
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
      let visits = Dictionary<string, Dictionary<int, int * Track list>>()
      let visit = Dictionary<int, int * Track list>()
      visits.Add("6A-33-AA-93-82-ED-22-9D-F8-68-2C-39-5B-93-9F-74-01-76-00-9F", visit)
      visit.Add(100663297, (1, [])) // should fill in the expected non-zero value
      visit.Add(100663298, (23, [])) // should be ignored
      Runner.PostProcess visits Base.ReportFormat.OpenCover after
      Assert.That
        (after.OuterXml.Replace("uspid=\"100663298", "uspid=\"13"), Is.EqualTo before,
         after.OuterXml)

    [<Test>]
    member self.PostprocessShouldRestoreDegenerateOpenCoverState() =
      Counter.measureTime <- DateTime.ParseExact
                               ("2017-12-29T16:33:40.9564026+00:00", "o", null)
      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(self.resource2)
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
      let empty = Dictionary<string, Dictionary<int, int * Track list>>()
      Runner.PostProcess empty Base.ReportFormat.OpenCover after
      Assert.That(after.OuterXml, Is.EqualTo before, after.OuterXml)

    [<Test>]
    member self.PostprocessShouldRestoreBranchOnlyOpenCoverState() =
      Counter.measureTime <- DateTime.ParseExact
                               ("2017-12-29T16:33:40.9564026+00:00", "o", null)
      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(self.resource2)
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
      let empty = Dictionary<string, Dictionary<int, int * Track list>>()
      Runner.PostProcess empty Base.ReportFormat.OpenCover after
      Assert.That(after.OuterXml, Is.EqualTo before, after.OuterXml)

    [<Test>]
    member self.JunkTokenShouldDefaultZero() =
      let visits = Dictionary<int, int * Track list>()
      let key = " "
      let result = Runner.LookUpVisitsByToken key visits
      match result with
      | (0, []) -> ()
      | _ -> Assert.Fail(sprintf "%A" result)

    [<Test>]
    member self.EmptyNCoverGeneratesExpectedSummary() =
      let report = XDocument()
      let builder = System.Text.StringBuilder()
      try
        Output.Info <- (fun s -> builder.Append(s).Append("|") |> ignore)
        let r = Runner.StandardSummary report Base.ReportFormat.NCover 0
        Assert.That(r, Is.EqualTo 0)
        Assert.That
          (builder.ToString(),
           Is.EqualTo
             "Visited Classes 0 of 0 (n/a)|Visited Methods 0 of 0 (n/a)|Visited Points 0 of 0 (n/a)|")
      finally
        Output.Info <- ignore

    [<Test>]
    member self.NCoverShouldGeneratePlausibleSummary() =
      let resource =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find (fun n -> n.EndsWith("SimpleCoverage.xml", StringComparison.Ordinal))
      use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
      let baseline = XDocument.Load(stream)
      let builder = System.Text.StringBuilder()
      try
        Output.Info <- (fun s -> builder.Append(s).Append("|") |> ignore)
        let r =
          try
            Runner.threshold <- Some 25
            Runner.StandardSummary baseline Base.ReportFormat.NCover 42
          finally
            Runner.threshold <- None
        // 80% coverage > threshold so expect return code coming in
        Assert.That(r, Is.EqualTo 42)
        Assert.That
          (builder.ToString(),
           Is.EqualTo
             "Visited Classes 1 of 1 (100)|Visited Methods 1 of 1 (100)|Visited Points 8 of 10 (80)|")
      finally
        Output.Info <- ignore

    [<Test>]
    member self.EmptyOpenCoverGeneratesExpectedSummary() =
      let report = XDocument.Load(new System.IO.StringReader("""<CoverageSession>
  <Summary numSequencePoints="0" visitedSequencePoints="0" numBranchPoints="0" visitedBranchPoints="0" sequenceCoverage="0" branchCoverage="0" maxCyclomaticComplexity="0" minCyclomaticComplexity="1" visitedClasses="0" numClasses="0" visitedMethods="0" numMethods="0" />
</CoverageSession>"""))
      let builder = System.Text.StringBuilder()
      try
        Output.Info <- (fun s -> builder.Append(s).Append("|") |> ignore)
        let r = Runner.StandardSummary report Base.ReportFormat.OpenCover 0
        Assert.That(r, Is.EqualTo 0)
        Assert.That
          (builder.ToString(),
           Is.EqualTo
             ("Visited Classes 0 of 0 (n/a)|Visited Methods 0 of 0 (n/a)|"
              + "Visited Points 0 of 0 (0)|Visited Branches 0 of 0 (0)||"
              + "==== Alternative Results (includes all methods including those without corresponding source) ====|"
              + "Alternative Visited Classes 0 of 0 (n/a)|Alternative Visited Methods 0 of 0 (n/a)|"))
      finally
        Output.Info <- ignore

    [<Test>]
    member self.OpenCoverShouldGeneratePlausibleSummary() =
      let resource =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find
             (fun n -> n.EndsWith("Sample1WithOpenCover.xml", StringComparison.Ordinal))
      use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
      let baseline = XDocument.Load(stream)
      let builder = System.Text.StringBuilder()
      try
        Output.Info <- (fun s -> builder.Append(s).Append("|") |> ignore)
        let r =
          try
            Runner.threshold <- Some 75
            Runner.StandardSummary baseline Base.ReportFormat.OpenCover 23
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
              + "Alternative Visited Classes 1 of 1 (100)|Alternative Visited Methods 1 of 2 (50)|"))
      finally
        Output.Info <- ignore

    [<Test>]
    member self.OpenCoverShouldGeneratePlausibleLcov() =
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
        let r = LCov.Summary baseline Base.ReportFormat.OpenCover 0
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

    [<Test>]
    member self.NCoverShouldGeneratePlausibleLcov() =
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
        let r = LCov.Summary baseline Base.ReportFormat.NCover 0
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

    [<Test>]
    member self.NCoverShouldGeneratePlausibleLcovWithMissingFullName() =
      let resource =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find
             (fun n -> n.EndsWith("Sample1WithNCover.xml", StringComparison.Ordinal))
      use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
      let baseline = XDocument.Load(stream)
      let excluded = XName.Get "excluded"
      baseline.Descendants()
      |> Seq.iter (fun x -> if x.Attribute(excluded) |> isNull |> not then
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
        let r = LCov.Summary baseline Base.ReportFormat.NCover 0
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

    [<Test>]
    member self.MultiSortDoesItsThing() =
      let input =
        [ ("m", [ 3; 2; 1 ])
          ("a", [ 4; 9; 7 ])
          ("z", [ 3; 5 ]) ]
        |> List.map (fun (x, y) ->
             (x,
              y
              |> List.map (sprintf "<x><seqpnt line=\"%d\" /></x>")
              |> List.map (fun x -> XDocument.Load(new System.IO.StringReader(x)))
              |> List.map (fun x -> x.Descendants(XName.Get "x") |> Seq.head)
              |> List.toSeq))
        |> List.toSeq

      let result =
        LCov.multiSortByNameAndStartLine input
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

    [<Test>]
    member self.NCoverShouldGeneratePlausibleCobertura() =
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
        let r = Cobertura.Summary baseline Base.ReportFormat.NCover 0
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
                         + typeof<Tracer>.Assembly.GetName().Version.ToString())
        Assert.That(result.Replace("\r", String.Empty), Is.EqualTo expected, result)
      finally
        Cobertura.path := None

    [<Test>]
    member self.NCoverShouldGeneratePlausibleCoberturaWithMissingFullName() =
      let resource =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find
             (fun n -> n.EndsWith("Sample1WithNCover.xml", StringComparison.Ordinal))
      use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
      let baseline = XDocument.Load(stream)
      let excluded = XName.Get "excluded"
      baseline.Descendants()
      |> Seq.iter (fun x -> if x.Attribute(excluded) |> isNull |> not then
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
        let r = Cobertura.Summary baseline Base.ReportFormat.NCover 0
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
                         + typeof<Tracer>.Assembly.GetName().Version.ToString())
        Assert.That(result.Replace("\r", String.Empty), Is.EqualTo expected, result)
      finally
        Cobertura.path := None

    [<Test>]
    member self.OpenCoverShouldGeneratePlausibleCobertura() =
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
        let r = Cobertura.Summary baseline Base.ReportFormat.OpenCover 0
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
                         + typeof<Tracer>.Assembly.GetName().Version.ToString())
        Assert.That
          (result.Replace("\r", String.Empty).Replace("\\", "/"), Is.EqualTo expected,
           result)
      finally
        Cobertura.path := None

    [<Test>]
    member self.ThresholdViolationShouldBeReported() =
      let saveErr = Output.Error
      let saveSummaries = Runner.Summaries
      let builder = System.Text.StringBuilder()
      let saved = Runner.threshold
      try
        Runner.Summaries <- [ (fun _ _ _ -> 23) ]
        Output.Error <- (fun s -> builder.Append(s).Append("|") |> ignore)
        Runner.threshold <- Some 42
        let delta = Runner.DoSummaries (XDocument()) Base.ReportFormat.NCover 0
        Assert.That(delta, Is.EqualTo 23)
        Assert.That
          (builder.ToString(),
           Is.EqualTo "Coverage percentage achieved is 23% below the threshold of 42%.|")
      finally
        Output.Error <- saveErr
        Runner.Summaries <- saveSummaries
        Runner.threshold <- saved

    [<Test>]
    member self.TryGetValueHandlesNull() =
      let dict : Dictionary<int, int> = null
      Assert.That(Runner.TryGetValue dict 0 |> fst, Is.False)
  end