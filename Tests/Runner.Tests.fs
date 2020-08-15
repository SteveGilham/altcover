namespace Tests
// fsharplint:disable  MemberNames NonPublicValuesNames RedundantNewKeyword

open System
open System.Collections.Generic
open System.IO
open System.IO.Compression
open System.Reflection
open System.Text.RegularExpressions
open System.Xml
open System.Xml.Linq
open System.Xml.Schema

open AltCover
open Microsoft.FSharp.Reflection
open Mono.Options

#nowarn "25" // partial pattern match

module AltCoverUsage =
    let internal usageText =
        let usage = Assembly.GetExecutingAssembly().GetManifestResourceNames()
                    |> Seq.find (fun n -> n.EndsWith("AltCover.Usage.txt", StringComparison.Ordinal))
        use stream =
            Assembly.GetExecutingAssembly().GetManifestResourceStream(usage)
        use reader = new StreamReader(stream)
        reader.ReadToEnd()

    let internal runnerText =
        let usage = Assembly.GetExecutingAssembly().GetManifestResourceNames()
                    |> Seq.find (fun n -> n.EndsWith("AltCover.Runner.Usage.txt", StringComparison.Ordinal))
        use stream =
            Assembly.GetExecutingAssembly().GetManifestResourceStream(usage)
        use reader = new StreamReader(stream)
        reader.ReadToEnd()

module AltCoverRunnerTests =
    // fs

    [<Test>]
    let MaxTimeFirst () =
      let now = DateTime.Now
      let ago = now - TimeSpan(1,0,0,0)
      test <@ (Counter.I.maxTime now ago) = now @>

    [<Test>]
    let MaxTimeLast () =
      let now = DateTime.Now
      let ago = now - TimeSpan(1,0,0,0)
      test <@ (Counter.I.maxTime ago now) = now @>

    [<Test>]
    let MinTimeFirst () =
      let now = DateTime.Now
      let ago = now - TimeSpan(1,0,0,0)
      test <@ (Counter.I.minTime ago now) = ago @>

    [<Test>]
    let MinTimeLast () =
      let now = DateTime.Now
      let ago = now - TimeSpan(1,0,0,0)
      test <@ (Counter.I.minTime now ago) = ago @>

    [<Test>]
    let JunkUspidGivesNegativeIndex() =
      Runner.init()
      let key = " "
      let index = Counter.I.findIndexFromUspid 0 key
      test <@ index < 0 @>

    [<Test>]
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

    [<Test>]
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

    [<Test>]
    let DistinctIdShouldBeDistinct() =
      Runner.init()
      let visits = new Dictionary<string, Dictionary<int, PointVisit>>()
      let key = " "
      let v3 = Counter.addVisit visits key 23 Null
      Assert.That(v3, Is.EqualTo 1)
      let v4 = Counter.addVisit visits "key" 42 Null
      Assert.That(visits.Count, Is.EqualTo 2)
      Assert.That(v4, Is.EqualTo 1)

    [<Test>]
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

    [<Test>]
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

    [<Test>]
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

    [<Test>]
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

    [<Test>]
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
        do
          use coverageFile =
            new FileStream(reportFile, FileMode.Open, FileAccess.ReadWrite, FileShare.None, 4096,
                            FileOptions.SequentialScan)

          Counter.doFlushStream ignore (fun _ _ -> ()) true visits
            AltCover.ReportFormat.NCover coverageFile None |> ignore
        use worker' = new FileStream(reportFile, FileMode.Open)
        let after = XmlDocument()
        after.Load worker'
        Assert.That
          (after.SelectNodes("//seqpnt")
           |> Seq.cast<XmlElement>
           |> Seq.map (fun x -> x.GetAttribute("visitcount")),
           Is.EquivalentTo [ "11"; "10"; "9"; "8"; "7"; "6"; "4"; "3"; "2"; "1" ])
      finally
        maybeDeleteFile reportFile
        Console.SetOut saved
        Directory.SetCurrentDirectory(here)
        maybeIOException (fun () -> Directory.Delete(unique))

    [<Test>]
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
        use coverageFile =
          new FileStream(reportFile, FileMode.Open, FileAccess.ReadWrite, FileShare.None, 4096,
                      FileOptions.SequentialScan)

        Counter.doFlushStream ignore (fun _ _ -> ()) true visits
          AltCover.ReportFormat.NCover coverageFile (Some outputFile) |> ignore
        use worker' = new FileStream(outputFile, FileMode.Open)
        let after = XmlDocument()
        after.Load worker'
        Assert.That
          (after.SelectNodes("//seqpnt")
           |> Seq.cast<XmlElement>
           |> Seq.map (fun x -> x.GetAttribute("visitcount")),
           Is.EquivalentTo [ "11"; "10"; "9"; "8"; "7"; "6"; "4"; "3"; "2"; "1" ])
      finally
        maybeDeleteFile reportFile
        Console.SetOut saved
        Directory.SetCurrentDirectory(here)
        maybeIOException (fun () -> Directory.Delete(unique))

    // Runner.fs and CommandLine.fs
    [<Test>]
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
        let expected = "Error - usage is:\n" +
                       AltCoverUsage.runnerText +
                       "\nor\n" +
                       "  ImportModule               Prints out the PowerShell script to import the\n" +
                       "                               associated PowerShell module\n" +
                       "or\n" +
                       "  Version                    Prints out the AltCover build version\n" +
                       "or, for the global tool only\n" +
                       "  TargetsPath                Prints out the path to the 'altcover.global.targets' file\n" +
                       "                               (as the tool cannot be 'dotnet add'ed to the project).\n" +
                       "                               The 'altcover.global.props' file is present in the same directory\n"
        Assert.That
          (result.Replace("\u200b",String.Empty).Replace("\r\n", "\n"),
           Is.EqualTo(expected.Replace("\r\n", "\n")), "*" + result + "*")
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
      CommandLine.toConsole()
      let saved = (Console.Out, Console.Error)
      let e0 = Console.Out.Encoding
      let e1 = Console.Error.Encoding
      try
        use stdout =
          { new StringWriter() with
              member self.Encoding = e0 }
        test <@ stdout.Encoding = e0 @>

        use stderr =
          { new StringWriter() with
              member self.Encoding = e1 }
        test <@ stderr.Encoding = e1 @>

        Console.SetOut stdout
        Console.SetError stderr
        let nonWindows = System.Environment.GetEnvironmentVariable("OS") <> "Windows_NT"

        let exe, args =
          maybe nonWindows ("mono", "\"" + program + "\"") (program, String.Empty)

        let r =
          CommandLine.I.launch exe args
            (Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location))
        Assert.That(r, Is.EqualTo 0)
        Assert.That(stderr.ToString(), Is.Empty)
        let result = stdout.ToString()

        let quote =
          maybe (System.Environment.GetEnvironmentVariable("OS") = "Windows_NT")
            "\"" String.Empty

        let expected =
          "Command line : '" + quote + exe + quote + " " + args + "\'"
          + Environment.NewLine + "Where is my rocket pack? " + Environment.NewLine
        Assert.That(result, Is.EqualTo(expected))
      finally
        Console.SetOut(fst saved)
        Console.SetError(snd saved)

    [<Test>]
    let ShouldHaveExpectedOptions() =
      Runner.init()
      let options = Runner.declareOptions()
      let optionCount = 10
      let optionNames = options
                        |> Seq.map (fun o -> (o.GetNames() |> Seq.maxBy(fun n -> n.Length)).ToLowerInvariant())
                        |> Seq.sort
                        |> Seq.toList

      // Options add "<>" and "help"
      Assert.That(options.Count, Is.EqualTo (optionCount + 2), String.Join("; ", optionNames))

      let optionNames = options
                        |> Seq.map (fun o -> (o.GetNames() |> Seq.maxBy(fun n -> n.Length)).ToLowerInvariant())
                        |> Seq.sort
                        |> Seq.toList

      let primitiveNames = typeof<Primitive.CollectOptions>
                           |> FSharpType.GetRecordFields
                           |> Seq.map (fun p -> p.Name.ToLowerInvariant())
                           |> Seq.sort
                           |> Seq.toList

      // swap "collect" and "commandline"
      Assert.That(primitiveNames |> List.length, Is.EqualTo optionCount,
                  "expected " + String.Join("; ", optionNames) + Environment.NewLine +
                  "but got  " + String.Join("; ", primitiveNames))

      let typesafeNames = typeof<TypeSafe.CollectOptions>
                          |> FSharpType.GetRecordFields
                          |> Seq.map (fun p -> p.Name.ToLowerInvariant())
                          |> Seq.sort
                          |> Seq.toList

      Assert.That(typesafeNames |> List.length, Is.EqualTo optionCount,
                  "expected " + String.Join("; ", optionNames) + Environment.NewLine +
                  "but got  " + String.Join("; ", typesafeNames))

      let fsapiNames = typeof<AltCover.CollectOptions>.GetProperties()
                       |> Seq.map (fun p -> p.Name.ToLowerInvariant())
                       |> Seq.sort
                       |> Seq.toList
      let fsapiCases = (typeof<AltCover.CollectOptions>
                        |> FSharpType.GetUnionCases).Length

      let args = Primitive.CollectOptions.Create() |> AltCover.CollectOptions.Primitive
      let commandFragments = Args.buildCollect args

      // adds Runner and the trailing command line arguments
      Assert.That(commandFragments |> List.length, Is.EqualTo (optionCount + 2),
                  "expected " + String.Join("; ", optionNames) + Environment.NewLine +
                  "but got  " + String.Join("; ", typesafeNames))  // todo

      // Adds "Tag", "IsPrimitive", "IsTypeSafe"
      Assert.That(fsapiNames
                  |> Seq.length, Is.EqualTo (optionCount + fsapiCases + 1),
                  "expected " + String.Join("; ", primitiveNames) + Environment.NewLine +
                  "but got  " + String.Join("; ", fsapiNames))

      let taskNames = typeof<Collect>.GetProperties(BindingFlags.DeclaredOnly ||| BindingFlags.Public ||| BindingFlags.Instance)
                      |> Seq.map (fun p -> p.Name.ToLowerInvariant())
                      |> Seq.sort
                      |> Seq.toList

      // gains summary (output)
      Assert.That(taskNames
                  |> Seq.length, Is.EqualTo (optionCount + 1),
                  "expected " + String.Join("; ", primitiveNames) + Environment.NewLine +
                  "but got  " + String.Join("; ", taskNames))

      let targets =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find
             (fun n -> n.EndsWith("AltCover.targets", StringComparison.Ordinal))
      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(targets)
      let doc = XDocument.Load stream
      let collect = doc.Descendants()
                    |> Seq.filter (fun d -> d.Name.LocalName = "AltCover.Collect")
                    |> Seq.head
      let attributeNames = collect.Attributes()
                           |> Seq.map (fun p -> p.Name.LocalName.ToLowerInvariant())
                           |> Seq.sort
                           |> Seq.toList

      // loses commandline; executable; exposereturncode; outputfile; workingdirectory
      //       N/A,         N/A,        N/A,              fixed,      N/A
      Assert.That(attributeNames
                  |> Seq.length, Is.EqualTo (optionCount - 5),
                  "expected " + String.Join("; ", primitiveNames) + Environment.NewLine +
                  "but got  " + String.Join("; ", attributeNames))

      Assert.That
        (options
         |> Seq.filter (fun x -> x.Prototype <> "<>")
         |> Seq.forall (fun x -> (String.IsNullOrWhiteSpace >> not) x.Description))
      Assert.That(options
                  |> Seq.filter (fun x -> x.Prototype = "<>")
                  |> Seq.length, Is.EqualTo 1)

    [<Test>]
    let ParsingJunkIsAnError() =
      Runner.init()
      let options = Runner.declareOptions()
      let parse = CommandLine.parseCommandLine [| "/@thisIsNotAnOption" |] options
      match parse with
      | Left(x, y) ->
        Assert.That(x, Is.EqualTo "UsageError")
        Assert.That(y, Is.SameAs options)

    [<Test>]
    let ParsingJunkAfterSeparatorIsExpected() =
      Runner.init()
      let options = Runner.declareOptions()
      let input = [| "--"; "/@thisIsNotAnOption"; "this should be OK" |]
      let parse = CommandLine.parseCommandLine input options
      match parse with
      | Right(x, y) ->
        Assert.That(x, Is.EquivalentTo(input |> Seq.skip 1))
        Assert.That(y, Is.SameAs options)

    [<Test>]
    let ParsingHelpGivesHelp() =
      Runner.init()
      let options = Runner.declareOptions()
      let input = [| "--?" |]
      let parse = CommandLine.parseCommandLine input options
      match parse with
      | Right(x, y) -> Assert.That(y, Is.SameAs options)
      match CommandLine.processHelpOption parse with
      | Left(x, y) ->
        Assert.That(x, Is.EqualTo "HelpText")
        Assert.That(y, Is.SameAs options)
      // a "not sticky" test
      lock Runner.executable (fun () ->
        Runner.executable := None
        match CommandLine.parseCommandLine [| "/x"; "x" |] options
              |> CommandLine.processHelpOption with
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty))

    [<Test>]
    let ParsingErrorHelpGivesHelp() =
      Runner.init()
      let options = Runner.declareOptions()

      let input =
        [| "--zzz"
           Path.GetInvalidPathChars() |> String |]

      let parse = CommandLine.parseCommandLine input options
      match parse with
      | Left(x, y) ->
        Assert.That(x, Is.EqualTo "UsageError")
        Assert.That(y, Is.SameAs options)
      match CommandLine.processHelpOption parse with
      | Left(x, y) ->
        Assert.That(x, Is.EqualTo "UsageError")
        Assert.That(y, Is.SameAs options)
      // a "not sticky" test
      lock Runner.executable (fun () ->
        Runner.executable := None
        match CommandLine.parseCommandLine [| "/x"; "x" |] options
              |> CommandLine.processHelpOption with
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty))

    [<Test>]
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
          | Right(x, y) ->
            Assert.That(y, Is.SameAs options)
            Assert.That(x, Is.Empty)
          match !Runner.executable with
          | Some x -> Assert.That(Path.GetFileName x, Is.EqualTo unique)
        finally
          Runner.executable := None)

    [<Test>]
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
          | Left(x, y) ->
            Assert.That(y, Is.SameAs options)
            Assert.That(x, Is.EqualTo "UsageError")
            Assert.That(CommandLine.error |> Seq.head, Is.EqualTo "--executable : specify this only once")
        finally
          Runner.executable := None)

    [<Test>]
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
          | Left(x, y) ->
            Assert.That(y, Is.SameAs options)
            Assert.That(x, Is.EqualTo "UsageError")
        finally
          Runner.executable := None)

    [<Test>]
    let ParsingWorkerGivesWorker() =
      Runner.init()
      try
        Runner.workingDirectory <- None
        let options = Runner.declareOptions()
        let unique = Path.GetFullPath(".")
        let input = [| "-w"; unique |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        match Runner.workingDirectory with
        | Some x -> Assert.That(x, Is.EqualTo unique)
      finally
        Runner.workingDirectory <- None

    [<Test>]
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
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
          Assert.That(CommandLine.error |> Seq.head, Is.EqualTo "--workingDirectory : specify this only once")
      finally
        Runner.workingDirectory <- None

    [<Test>]
    let ParsingBadWorkerGivesFailure() =
      Runner.init()
      try
        Runner.workingDirectory <- None
        let options = Runner.declareOptions()
        let unique = Guid.NewGuid().ToString().Replace("-", "*")
        let input = [| "-w"; unique |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.workingDirectory <- None

    [<Test>]
    let ParsingNoWorkerGivesFailure() =
      Runner.init()
      try
        Runner.workingDirectory <- None
        let options = Runner.declareOptions()
        let input = [| "-w" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.workingDirectory <- None

    [<Test>]
    let ParsingRecorderGivesRecorder() =
      Runner.init()
      try
        Runner.recordingDirectory <- None
        let options = Runner.declareOptions()
        let unique = Path.GetFullPath(".")
        let input = [| "-r"; unique |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        match Runner.recordingDirectory with
        | Some x -> Assert.That(x, Is.EqualTo unique)
      finally
        Runner.recordingDirectory <- None

    [<Test>]
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
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
          Assert.That(CommandLine.error |> Seq.head, Is.EqualTo "--recorderDirectory : specify this only once")
      finally
        Runner.recordingDirectory <- None

    [<Test>]
    let ParsingBadRecorderGivesFailure() =
      Runner.init()
      try
        Runner.recordingDirectory <- None
        let options = Runner.declareOptions()
        let unique = Guid.NewGuid().ToString().Replace("-", "*")
        let input = [| "-r"; unique |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.recordingDirectory <- None

    [<Test>]
    let ParsingNoRecorderGivesFailure() =
      Runner.init()
      try
        Runner.recordingDirectory <- None
        let options = Runner.declareOptions()
        let input = [| "-r" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.recordingDirectory <- None

    [<Test>]
    let ParsingCollectGivesCollect() =
      Runner.init()
      try
        Runner.collect := false
        let options = Runner.declareOptions()
        let input = [| "--collect" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        Assert.That(!Runner.collect, Is.True)
      finally
        Runner.collect := false

    [<Test>]
    let ParsingMultipleCollectGivesFailure() =
      Runner.init()
      try
        Runner.collect := false
        let options = Runner.declareOptions()
        let input = [| "--collect"; "--collect" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
          Assert.That(CommandLine.error |> Seq.head, Is.EqualTo "--collect : specify this only once")
      finally
        Runner.collect := false

    [<Test>]
    let ParsingLcovGivesLcov() =
      Runner.init()
      lock LCov.path (fun () ->
        try
          LCov.path := None
          Runner.I.initSummary()
          let options = Runner.declareOptions()
          let unique = "some exe"
          let input = [| "-l"; unique |]
          let parse = CommandLine.parseCommandLine input options
          match parse with
          | Right(x, y) ->
            Assert.That(y, Is.SameAs options)
            Assert.That(x, Is.Empty)
          match !LCov.path with
          | Some x -> Assert.That(Path.GetFileName x, Is.EqualTo unique)
          Assert.That(Runner.I.summaries.Length, Is.EqualTo 2)
        finally
          Runner.I.initSummary()
          LCov.path := None)

    [<Test>]
    let ParsingMultipleLcovGivesFailure() =
      Runner.init()
      lock LCov.path (fun () ->
        try
          LCov.path := None
          Runner.I.initSummary()
          let options = Runner.declareOptions()
          let unique = Guid.NewGuid().ToString()

          let input =
            [| "-l"
               unique
               "/l"
               unique.Replace("-", "+") |]

          let parse = CommandLine.parseCommandLine input options
          match parse with
          | Left(x, y) ->
            Assert.That(y, Is.SameAs options)
            Assert.That(x, Is.EqualTo "UsageError")
            Assert.That(CommandLine.error |> Seq.head, Is.EqualTo "--lcovReport : specify this only once")
        finally
          Runner.I.initSummary()
          LCov.path := None)

    [<Test>]
    let ParsingNoLcovGivesFailure() =
      Runner.init()
      lock LCov.path (fun () ->
        try
          LCov.path := None
          Runner.I.initSummary()
          let options = Runner.declareOptions()
          let blank = " "
          let input = [| "-l"; blank |]
          let parse = CommandLine.parseCommandLine input options
          match parse with
          | Left(x, y) ->
            Assert.That(y, Is.SameAs options)
            Assert.That(x, Is.EqualTo "UsageError")
        finally
          Runner.I.initSummary()
          LCov.path := None)

    [<Test>]
    let ParsingThresholdGivesThreshold() =
      Runner.init()
      try
        Runner.threshold <- None
        let options = Runner.declareOptions()
        let input = [| "-t"; "57" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        match Runner.threshold with
        | Some x -> Assert.That(x, Is.EqualTo {
                                                Statements = 57uy
                                                Branches = 0uy
                                                Methods = 0uy
                                                Crap = 0uy
                                                AltMethods = 0uy
                                                AltCrap = 0uy
                                              })
      finally
        Runner.threshold <- None

    [<Test>]
    let ParsingComplexThresholdGivesThreshold() =
      Runner.init()
      try
        Runner.threshold <- None
        let options = Runner.declareOptions()
        let input = [| "-t"; "M57C42S16B7AM14AC101" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        match Runner.threshold with
        | Some x -> Assert.That(x, Is.EqualTo {
                                                Statements = 16uy
                                                Branches = 7uy
                                                Methods = 57uy
                                                Crap = 42uy
                                                AltMethods = 14uy
                                                AltCrap = 101uy
                                              })
      finally
        Runner.threshold <- None

    [<Test>]
    let ParsingTopThresholdGivesThreshold() =
      Runner.init()
      try
        Runner.threshold <- None
        let options = Runner.declareOptions()
        let input = [| "-t"; "M100C255S100B100AM100AC255" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        match Runner.threshold with
        | Some x -> Assert.That(x, Is.EqualTo {
                                                Statements = 100uy
                                                Branches = 100uy
                                                Methods = 100uy
                                                Crap = 255uy
                                                AltMethods = 100uy
                                                AltCrap = 255uy
                                              })
      finally
        Runner.threshold <- None

    [<Test>]
    let ParsingLowThresholdGivesThreshold() =
      Runner.init()
      try
        Runner.threshold <- None
        let options = Runner.declareOptions()
        let input = [| "-t"; "M0C0S0B0AM0AC0" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        match Runner.threshold with
        | Some x -> Assert.That(x, Is.EqualTo {
                                                Statements = 0uy
                                                Branches = 0uy
                                                Methods = 0uy
                                                Crap = 0uy
                                                AltMethods = 0uy
                                                AltCrap = 0uy
                                              })
      finally
        Runner.threshold <- None

    [<Test>]
    let ParsingMultipleThresholdGivesFailure() =
      Runner.init()
      try
        Runner.threshold <- None
        let options = Runner.declareOptions()
        let input = [| "-t"; "23"; "/t"; "42" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
          Assert.That(CommandLine.error |> Seq.head, Is.EqualTo "--threshold : specify this only once")
      finally
        Runner.threshold <- None

    [<Test>]
    let ParsingBadThresholdGivesFailure() =
      Runner.init()
      try
        Runner.threshold <- None
        let options = Runner.declareOptions()
        let input = [| "-t"; "-111" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.threshold <- None

    [<Test>]
    let ParsingBadThreshold2GivesFailure() =
      Runner.init()
      try
        Runner.threshold <- None
        let options = Runner.declareOptions()
        let input = [| "-t"; "S" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.threshold <- None

    [<Test>]
    let ParsingBadThreshold3GivesFailure() =
      Runner.init()
      try
        Runner.threshold <- None
        let options = Runner.declareOptions()
        let input = [| "-t"; "X666" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.threshold <- None

    [<Test>]
    let ParsingBadThreshold4GivesFailure() =
      Runner.init()
      try
        Runner.threshold <- None
        let options = Runner.declareOptions()
        let input = [| "-t"; "S101" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.threshold <- None

    [<Test>]
    let ParsingBadThreshold5GivesFailure() =
      Runner.init()
      try
        Runner.threshold <- None
        let options = Runner.declareOptions()
        let input = [| "-t"; "M101" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.threshold <- None

    [<Test>]
    let ParsingBadThreshold6GivesFailure() =
      Runner.init()
      try
        Runner.threshold <- None
        let options = Runner.declareOptions()
        let input = [| "-t"; "B101" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.threshold <- None

    [<Test>]
    let ParsingBadThreshold7GivesFailure() =
      Runner.init()
      try
        Runner.threshold <- None
        let options = Runner.declareOptions()
        let input = [| "-t"; "C256" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.threshold <- None

    [<Test>]
    let ParsingEmptyThresholdGivesFailure() =
      Runner.init()
      try
        Runner.threshold <- None
        let options = Runner.declareOptions()
        let input = [| "-t"; "  " |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.threshold <- None

    [<Test>]
    let ParsingNoThresholdGivesFailure() =
      Runner.init()
      try
        Runner.threshold <- None
        let options = Runner.declareOptions()
        let input = [| "-t" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.threshold <- None

    [<Test>]
    let ParsingCoberturaGivesCobertura() =
      Runner.init()
      lock Cobertura.path (fun () ->
        try
          Cobertura.path := None
          Runner.I.initSummary()
          let options = Runner.declareOptions()
          let unique = "some exe"
          let input = [| "-c"; unique |]
          let parse = CommandLine.parseCommandLine input options
          match parse with
          | Right(x, y) ->
            Assert.That(y, Is.SameAs options)
            Assert.That(x, Is.Empty)
          match !Cobertura.path with
          | Some x -> Assert.That(Path.GetFileName x, Is.EqualTo unique)
          Assert.That(Runner.I.summaries.Length, Is.EqualTo 2)
        finally
          Runner.I.initSummary()
          Cobertura.path := None)

    [<Test>]
    let ParsingMultipleCoberturaGivesFailure() =
      Runner.init()
      lock Cobertura.path (fun () ->
        try
          Cobertura.path := None
          Runner.I.initSummary()
          let options = Runner.declareOptions()
          let unique = Guid.NewGuid().ToString()

          let input =
            [| "-c"
               unique
               "/c"
               unique.Replace("-", "+") |]

          let parse = CommandLine.parseCommandLine input options
          match parse with
          | Left(x, y) ->
            Assert.That(y, Is.SameAs options)
            Assert.That(x, Is.EqualTo "UsageError")
            Assert.That(CommandLine.error |> Seq.head, Is.EqualTo "--cobertura : specify this only once")
        finally
          Runner.I.initSummary()
          Cobertura.path := None)

    [<Test>]
    let ParsingNoCoberturaGivesFailure() =
      Runner.init()
      lock Cobertura.path (fun () ->
        try
          Cobertura.path := None
          Runner.I.initSummary()
          let options = Runner.declareOptions()
          let blank = " "
          let input = [| "-c"; blank |]
          let parse = CommandLine.parseCommandLine input options
          match parse with
          | Left(x, y) ->
            Assert.That(y, Is.SameAs options)
            Assert.That(x, Is.EqualTo "UsageError")
        finally
          Runner.I.initSummary()
          Cobertura.path := None)

    [<Test>]
    let ParsingOutputGivesOutput() =
      Runner.init()
      try
        Runner.output <- None
        let options = Runner.declareOptions()
        let unique = Guid.NewGuid().ToString()
        let input = [| "-o"; unique |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        match Runner.output with
        | Some x -> Assert.That(Path.GetFileName x, Is.EqualTo unique)
      finally
        Runner.output <- None

    [<Test>]
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
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
          Assert.That(CommandLine.error |> Seq.head, Is.EqualTo "--outputFile : specify this only once")
      finally
        Runner.output <- None

    [<Test>]
    let ParsingNoOutputGivesFailure() =
      Runner.init()
      try
        Runner.output <- None
        let options = Runner.declareOptions()
        let blank = " "
        let input = [| "-o"; blank |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.output <- None

    [<Test>]
    let ParsingDropGivesDrop() =
      Runner.init()
      try
        CommandLine.dropReturnCode := false
        let options = Runner.declareOptions()
        let input = [| "--dropReturnCode" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        Assert.That(!CommandLine.dropReturnCode, Is.True)
      finally
        CommandLine.dropReturnCode := false

    [<Test>]
    let ParsingMultipleDropGivesFailure() =
      Runner.init()
      try
        CommandLine.dropReturnCode := false
        let options = Runner.declareOptions()
        let input = [| "--dropReturnCode"; "--dropReturnCode" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
          Assert.That(CommandLine.error |> Seq.head, Is.EqualTo "--dropReturnCode : specify this only once")
      finally
        CommandLine.dropReturnCode := false

    [<Test>]
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

    [<Test>]
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
          | Right(x, y) ->
            Assert.That(y, Is.SameAs options)
            Assert.That(x, Is.Empty)
          match Runner.summaryFormat with
          | x when v = x -> ()))

    [<Test>]
    let ParsingMultipleTCGivesFailure() =
      Runner.init()
      lock Runner.summaryFormat (fun () ->
        Runner.summaryFormat <- Default
        let options = Runner.declareOptions()
        let input = [| "--teamcity"; "--teamcity" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
          Assert.That(CommandLine.error |> Seq.head, Is.EqualTo "--teamcity : specify this only once"))

    [<Test>]
    let ParsingBadTCGivesFailure() =
      Runner.init()
      lock Runner.summaryFormat (fun () ->
        Runner.summaryFormat <- Default
        let options = Runner.declareOptions()
        let blank = " "
        let input = [| "--teamcity:junk" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError"))

    [<Test>]
    let ShouldRequireExe() =
      Runner.init()
      lock Runner.executable (fun () ->
        try
          Runner.executable := None
          let options = Runner.declareOptions()
          let parse = Runner.J.requireExe(Right([], options))
          match parse with
          | Left(x, y) ->
            Assert.That(y, Is.SameAs options)
            Assert.That(x, Is.EqualTo "UsageError")
        finally
          Runner.executable := None)

    [<Test>]
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
        finally
          Runner.executable := None)

    [<Test>]
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
        finally
          Runner.collect := false
          Runner.executable := None)

    [<Test>]
    let ShouldRejectExeIfCollect() =
      Runner.init()
      lock Runner.executable (fun () ->
        try
          Runner.executable := Some "xxx"
          Runner.collect := true
          let options = Runner.declareOptions()
          let parse = Runner.J.requireExe(Right([ "b" ], options))
          match parse with
          | Left(x, y) ->
            Assert.That(y, Is.SameAs options)
            Assert.That(x, Is.EqualTo "UsageError")
        finally
          Runner.collect := false
          Runner.executable := None)

    [<Test>]
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
      finally
        Runner.workingDirectory <- None

    [<Test>]
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
      finally
        Runner.workingDirectory <- None

    [<Test>]
    let ShouldRequireRecorder() =
      Runner.init()
      try
        Runner.recordingDirectory <- None
        let options = Runner.declareOptions()
        let input = (Right([], options))
        let parse = Runner.J.requireRecorder input
        match parse with
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.recordingDirectory <- None

    [<Test>]
    let ShouldRequireRecorderDll() =
      Runner.init()
      let path = Path.Combine(SolutionRoot.location, "_Mono/Sample1")
      try
        Runner.recordingDirectory <- Some path
        let options = Runner.declareOptions()
        let input = (Right([], options))
        let parse = Runner.J.requireRecorder input
        match parse with
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.recordingDirectory <- None

    [<Test>]
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
      finally
        Runner.recordingDirectory <- None

    [<Test>]
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
          maybe nonWindows [ "mono"; path ] [ path ]
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
      CommandLine.toConsole()
      let saved = (Console.Out, Console.Error)
      let e0 = Console.Out.Encoding
      let e1 = Console.Error.Encoding
      try
        use stdout =
          { new StringWriter() with
              member self.Encoding = e0 }
        test <@ stdout.Encoding = e0 @>

        use stderr =
          { new StringWriter() with
              member self.Encoding = e1 }
        test <@ stderr.Encoding = e1 @>

        Console.SetOut stdout
        Console.SetError stderr
        let u1 = Guid.NewGuid().ToString()
        let u2 = Guid.NewGuid().ToString()
        let baseArgs = [ program; u1; u2 ]
        let nonWindows = System.Environment.GetEnvironmentVariable("OS") <> "Windows_NT"

        let args =
          maybe nonWindows ("mono" :: baseArgs) baseArgs

        let r = CommandLine.processTrailingArguments args <| DirectoryInfo(path)
        Assert.That(r, Is.EqualTo 0)
        Assert.That(stderr.ToString(), Is.Empty)
        stdout.Flush()
        let result = stdout.ToString()

        let quote =
          maybe (System.Environment.GetEnvironmentVariable("OS") = "Windows_NT")
            "\"" String.Empty

        let expected =
          "Command line : '" + quote + args.Head + quote + " "
          + String.Join(" ", args.Tail) + "'" + Environment.NewLine
          + "Where is my rocket pack? " + u1 + "*" + u2 + Environment.NewLine
        Assert.That(result, Is.EqualTo expected)
      finally
        Console.SetOut(fst saved)
        Console.SetError(snd saved)

    [<Test>]
    let ShouldNoOp() =
      Runner.init()
      let where = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
      let r = CommandLine.processTrailingArguments [] <| DirectoryInfo(where)
      Assert.That(r, Is.EqualTo 0)

    [<Test>]
    let ErrorResponseIsAsExpected() =
      Runner.init()
      let saved = Console.Error
      try
        use stderr = new StringWriter()
        Console.SetError stderr
        let unique = Guid.NewGuid().ToString()
        let main =
          typeof<Marker>.Assembly.GetType("AltCover.EntryPoint")
            .GetMethod("main", BindingFlags.NonPublic ||| BindingFlags.Static)
        let returnCode = main.Invoke(null, [| [| "RuNN"; "-r"; unique |] |])
        Assert.That(returnCode, Is.EqualTo 255)
        let result = stderr.ToString().Replace("\r\n", "\n")
        let expected = "\"RuNN\" \"-r\" \"" + unique + "\"\n"
                       + "--recorderDirectory : Directory " + unique + " not found\n" +
                       "Error - usage is:\n" +
                       AltCoverUsage.usageText +
                       "\nor\n" +
                       AltCoverUsage.runnerText +
                       "\nor\n" +
                       "  ImportModule               Prints out the PowerShell script to import the\n" +
                       "                               associated PowerShell module\n" +
                       "or\n" +
                       "  Version                    Prints out the AltCover build version\n" +
                       "or, for the global tool only\n" +
                       "  TargetsPath                Prints out the path to the 'altcover.global.targets' file\n" +
                       "                               (as the tool cannot be 'dotnet add'ed to the project).\n" +
                       "                               The 'altcover.global.props' file is present in the same directory\n"

        Assert.That
          (result.Replace("\r\n", "\n").Replace("\u200b",String.Empty),
          Is.EqualTo(expected.Replace("\r\n", "\n")))
      finally
        Console.SetError saved

    let synchronized = Object()

    [<Test>]
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
      CommandLine.toConsole()
      let saved = (Console.Out, Console.Error)
      Runner.workingDirectory <- Some path
      let e0 = Console.Out.Encoding
      let e1 = Console.Error.Encoding
      try
        use stdout =
          { new StringWriter() with
              member self.Encoding = e0 }
        test <@ stdout.Encoding = e0 @>

        use stderr =
          { new StringWriter() with
              member self.Encoding = e1 }
        test <@ stderr.Encoding = e1 @>

        Console.SetOut stdout
        Console.SetError stderr
        let u1 = Guid.NewGuid().ToString()
        let u2 = Guid.NewGuid().ToString()
        let baseArgs = [ program; u1; u2 ]
        let nonWindows = System.Environment.GetEnvironmentVariable("OS") <> "Windows_NT"

        let args =
          maybe nonWindows ("mono" :: baseArgs) baseArgs

        let r = Runner.J.getPayload args
        Assert.That(r, Is.EqualTo 0)
        Assert.That(stderr.ToString(), Is.Empty)
        stdout.Flush()
        let result = stdout.ToString()

        let quote =
          maybe (System.Environment.GetEnvironmentVariable("OS") = "Windows_NT")
           "\"" String.Empty

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
    let WriteLeavesExpectedTraces() =
      Runner.init()
      let saved = Console.Out
      let here = Directory.GetCurrentDirectory()
      let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
      let unique = Path.Combine(where, Guid.NewGuid().ToString())
      let reportFile = Path.Combine(unique, "FlushLeavesExpectedTraces.xml")
      let junkfile = (reportFile + "." + (Path.GetFileName unique))
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
        let hits = List<string * int * Track>()
        [ 0..9 ]
        |> Seq.iter (fun i ->
             for j = 1 to i + 1 do
               hits.Add("f6e3edb3-fb20-44b3-817d-f69d1a22fc2f", i, Null)
               ignore j)

        let counts = Dictionary<string, Dictionary<int, PointVisit>>()
        hits
        |> Seq.iter
             (fun (moduleId, hitPointId, hit) ->
             AltCover.Counter.addVisit counts moduleId hitPointId hit |> ignore)

        // degenerate case
        Assert.That(junkfile |> File.Exists |> not)
        do use junkworker = new FileStream(junkfile, FileMode.CreateNew)
           junkworker.Write(buffer, 0, 0)
           ()
        Runner.J.doReport counts AltCover.ReportFormat.NCover junkfile None |> ignore

        let (c0, w0) = Zip.openUpdate junkfile
        try
          Assert.That(c0 |> isNull)
          Assert.That(w0, Is.InstanceOf<FileStream>())
          Assert.That(w0.Length, Is.EqualTo 8L)
        finally
          w0.Dispose()

        Runner.J.doReport counts AltCover.ReportFormat.NCover reportFile None |> ignore
        use worker' = new FileStream(reportFile, FileMode.Open)
        let after = XmlDocument()
        after.Load worker'
        Assert.That
          (after.SelectNodes("//seqpnt")
           |> Seq.cast<XmlElement>
           |> Seq.map (fun x -> x.GetAttribute("visitcount")),
           Is.EquivalentTo [ "11"; "10"; "9"; "8"; "7"; "6"; "4"; "3"; "2"; "1" ])
      finally
        maybeDeleteFile reportFile
        maybeDeleteFile junkfile
        Console.SetOut saved
        Directory.SetCurrentDirectory(here)
        maybeIOException (fun () -> Directory.Delete(unique))

    [<Test>]
    let ZipWriteLeavesExpectedTraces() =
      Runner.init()
      let saved = Console.Out
      let here = Directory.GetCurrentDirectory()
      let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
      let unique = Path.Combine(where, Guid.NewGuid().ToString())
      let reportFile = Path.Combine(unique, "FlushLeavesExpectedTraces.xml")
      let junkfile = (reportFile + "." + (Path.GetFileName unique))
      let junkfile2 = junkfile + ".xml"
      try
        use stdout = new StringWriter()
        Console.SetOut stdout
        Directory.CreateDirectory(unique) |> ignore
        Directory.SetCurrentDirectory(unique)
        Counter.measureTime <- DateTime.ParseExact
                                 ("2017-12-29T16:33:40.9564026+00:00", "o", null)
        use stream =
          Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

        let doc = XDocument.Load stream
        Zip.save doc reportFile true

        let hits = List<string * int * Track>()
        [ 0..9 ]
        |> Seq.iter (fun i ->
             for j = 1 to i + 1 do
               hits.Add("f6e3edb3-fb20-44b3-817d-f69d1a22fc2f", i, Null)
               ignore j)

        let counts = Dictionary<string, Dictionary<int, PointVisit>>()
        hits
        |> Seq.iter
             (fun (moduleId, hitPointId, hit) ->
             AltCover.Counter.addVisit counts moduleId hitPointId hit |> ignore)

        // degenerate case 1
        Assert.That(junkfile |> File.Exists |> not)
        let (c0, w0) = Zip.openUpdate junkfile
        try
          Assert.That(c0.IsNotNull)
          Assert.That(w0, Is.InstanceOf<MemoryStream>())
          Assert.That(w0.Length, Is.EqualTo 0L)
        finally
          c0.Dispose()
          w0.Dispose()

        // degenerate case 2
        Assert.That(junkfile2 |> File.Exists |> not)
        Runner.J.doReport counts AltCover.ReportFormat.NCover junkfile2 None |> ignore

        Assert.That(junkfile2 |> File.Exists |> not)
        let (c1, w1) = Zip.openUpdate junkfile2
        try
          Assert.That(c1.IsNotNull)
          Assert.That(w1, Is.InstanceOf<MemoryStream>())
          Assert.That(w1.Length, Is.EqualTo 0L)
        finally
          c0.Dispose()
          w0.Dispose()
          Assert.That(junkfile2 |> File.Exists |> not)

        Runner.J.doReport counts AltCover.ReportFormat.NCover reportFile None |> ignore
        let (container, worker) = Zip.openUpdate reportFile
        use worker' = worker
        use container' = container
        let after = XmlDocument()
        after.Load worker'
        Assert.That
          (after.SelectNodes("//seqpnt")
           |> Seq.cast<XmlElement>
           |> Seq.map (fun x -> x.GetAttribute("visitcount")),
           Is.EquivalentTo [ "11"; "10"; "9"; "8"; "7"; "6"; "4"; "3"; "2"; "1" ])
      finally
        Assert.That(junkfile |> File.Exists |> not)
        Assert.That(junkfile2 |> File.Exists |> not)
        maybeDeleteFile reportFile
        Console.SetOut saved
        Directory.SetCurrentDirectory(here)
        maybeIOException (fun () -> Directory.Delete(unique))

    [<Test>]
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

    [<Test>]
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

      maybeDeleteFile (unique + ".acv")

    [<Test>]
    let CollectShouldReportAsExpected() =
      Runner.init()
      try
        Runner.collect := true
        let counts = Dictionary<string, Dictionary<int, PointVisit>>()
        let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
        let unique = Path.Combine(where, Guid.NewGuid().ToString())
        let processing name (l : String list) = 
            use sink =
              new DeflateStream(File.OpenWrite(name + ".0.acv"),
                                CompressionMode.Compress)
            use formatter = new BinaryWriter(sink)
            l
            |> List.mapi (fun i x ->
                 formatter.Write x
                 formatter.Write i
                 formatter.Write 0uy
                 x)
            |> List.length
        let test = Path.Combine(where, Guid.NewGuid().ToString())
        let dryrun = processing test  [ "a"; "b"; String.Empty; "c" ]
        Assert.That (dryrun, Is.EqualTo 4)
        Assert.That(File.Exists(test + ".0.acv"))

        let r =
          Runner.J.getMonitor counts unique (processing unique) 
            [ "a"; "b"; String.Empty; "c" ]
        Assert.That(r, Is.EqualTo 0)
        Assert.That(File.Exists(unique + ".acv") |> not)
        let doc = Runner.J.loadReport(unique + ".acv")
        Assert.That(doc.Nodes(), Is.Empty)
        Assert.That(counts, Is.Empty)
      finally
        Runner.collect := false

    [<Test>]
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
               formatter.Serialize(sink, (x, i, Null, DateTime.UtcNow))
               x)
          |> List.length) [ "a"; "b"; String.Empty; "c" ]
      Assert.That(r, Is.EqualTo 4)
      Assert.That(File.Exists(unique + ".acv"))
      Assert.That(counts, Is.EquivalentTo [])

    [<Test>]
    let TrackingPayloadShouldReportAsExpected() =
      Runner.init()
      let counts = Dictionary<string, Dictionary<int, PointVisit>>()
      let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
      let unique = Path.Combine(where, Guid.NewGuid().ToString())

      let payloads0 =
        [ Null
          Call 17
          Time 23L
          Both { Time = 5L; Call = 42 }
          Time 42L
          Call 5 ]

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
               | Null -> formatter.Write(Tag.Null |> byte)
               | Time t ->
                 formatter.Write(Tag.Time |> byte)
                 formatter.Write(t)
               | Call t ->
                 formatter.Write(Tag.Call |> byte)
                 formatter.Write(t)
               | Both b ->
                 formatter.Write(Tag.Both |> byte)
                 formatter.Write(b.Time)
                 formatter.Write(b.Call)
               | Table t ->
                 formatter.Write(Tag.Table |> byte)
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
                                                                                      formatter.Write(Tag.Time |> byte)
                                                                                      formatter.Write(t)
                                                                                    | Call t ->
                                                                                      formatter.Write(Tag.Call |> byte)
                                                                                      formatter.Write(t)
                                                                                    | Both b ->
                                                                                      formatter.Write(Tag.Both |> byte)
                                                                                      formatter.Write(b.Time)
                                                                                      formatter.Write(b.Call)
                                                                                    //| _ -> tx |> (sprintf "%A") |> Assert.Fail
                                                             )
                                                             formatter.Write(Tag.Null |> byte)))
                 formatter.Write String.Empty
               x)
          |> List.length) inputs

      let expected = Dictionary<string, Dictionary<int, int64 * Track list>>()
      let a = Dictionary<int, int64 * Track list>()
      a.Add(1, (1L, []))
      let b = Dictionary<int, int64 * Track list>()
      b.Add(2, (0L, [Call 17]))
      let c = Dictionary<int, int64 * Track list>()
      c.Add(3, (0L, [Time 23L]))
      let d = Dictionary<int, int64 * Track list>()
      d.Add(4, (0L, [Both { Time = 5L; Call = 42 } ]))
      let e = Dictionary<int, int64 * Track list>()
      e.Add(6, (0L, [Call 5]))
      let f = Dictionary<int, int64 * Track list>()
      f.Add(3, (42L, payloads0 |> List.tail))

      expected.Add ("a", a)
      expected.Add ("b", b)
      expected.Add ("c", c)
      expected.Add ("d", d)
      expected.Add ("e", e)
      expected.Add ("Extra", f)

      Assert.That(r, Is.EqualTo 7)
      Assert.That(File.Exists(unique + ".acv"))

      let result = Dictionary<string, Dictionary<int, int64 * Track list>>()
      counts.Keys
      |> Seq.iter (fun k -> let inner = Dictionary<int, int64 * Track list>()
                            result.Add(k, inner)
                            counts.[k].Keys
                            |> Seq.iter (fun k2 ->
                                let v = counts.[k].[k2]
                                inner.Add(k2, (v.Count, v.Tracks |> Seq.toList))
                            ))

      Assert.That(result, Is.EquivalentTo expected)

    [<Test>]
    let PointProcessShouldCaptureTimes() =
      Runner.init()
      let x = XmlDocument()
      x.LoadXml("<root />")
      let root = x.DocumentElement

      let hits =
        [ Null
          Call 17
          Time 23L
          Both { Time = 5L;  Call = 42 }
          Time 42L
          Time 5L ]
      Runner.J.pointProcess root hits
      Assert.That
        (x.DocumentElement.OuterXml,
         Is.EqualTo
           """<root><Times><Time time="5" vc="2" /><Time time="23" vc="1" /><Time time="42" vc="1" /></Times><TrackedMethodRefs><TrackedMethodRef uid="17" vc="1" /><TrackedMethodRef uid="42" vc="1" /></TrackedMethodRefs></root>""")

    [<Test>]
    let PostprocessShouldHandleNullCase() =
      let minimal = """<?xml version="1.0" encoding="utf-8"?>
<CoverageSession xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
<Summary numSequencePoints="0" visitedSequencePoints="0" numBranchPoints="0" visitedBranchPoints="0" sequenceCoverage="0" branchCoverage="0" maxCyclomaticComplexity="0" minCyclomaticComplexity="0" visitedClasses="0" numClasses="0" visitedMethods="0" numMethods="0" minCrapScore="0" maxCrapScore="0" />
</CoverageSession>"""
      let after = XmlDocument()
      use reader= new StringReader(minimal)
      after.Load(reader)
      let empty = Dictionary<string, Dictionary<int, PointVisit>>()
      Runner.J.postProcess empty ReportFormat.OpenCover after
      let summary = after.DocumentElement.SelectNodes("//Summary")
                    |> Seq.cast<XmlElement>
                    |> Seq.toList

      test <@ summary |> Seq.length = 1 @>
      let attr = (summary |> Seq.head).GetAttribute("minCrapScore")
      test <@ attr = "0" @>

    [<Test>]
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
      Runner.J.postProcess empty ReportFormat.OpenCover after
      Assert.That
        (after.OuterXml.Replace("uspid=\"100663298", "uspid=\"13"), Is.EqualTo before,
         after.OuterXml)

    [<Test>]
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
      Runner.J.postProcess visits ReportFormat.OpenCover after
      Assert.That
        (after.OuterXml.Replace("uspid=\"100663298", "uspid=\"13"), Is.EqualTo before,
         after.OuterXml)

    [<Test>]
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
           if el.GetAttribute("maxCrapScore") = "2.11" then
             el.SetAttribute("maxCrapScore", "2.15")
           if el.GetAttribute("minCrapScore") = "2.11" then
             el.SetAttribute("minCrapScore", "2.15"))
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
      Runner.J.postProcess empty ReportFormat.OpenCover after
      Assert.That(after.OuterXml, Is.EqualTo before, after.OuterXml)

    [<Test>]
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
           if el.GetAttribute("maxCrapScore") = "2.11" then
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
      Runner.J.postProcess empty ReportFormat.OpenCover after
      Assert.That(after.OuterXml, Is.EqualTo before, after.OuterXml)

    [<Test>]
    let PostprocessShouldRestoreBranchOnlyOpenCoverStateXDoc() =
      Counter.measureTime <- DateTime.ParseExact
                                ("2017-12-29T16:33:40.9564026+00:00", "o", null)
      let resource2 =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find
             (fun n -> n.EndsWith("Sample1WithOpenCover.xml", StringComparison.Ordinal))
      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)
      let after = XDocument.Load(stream)
      let setAttribute (x:XElement) n v =
        x.Attribute(XName.Get n).Value <- v
      let getAttribute (x:XElement) n =
        x.Attribute(XName.Get n).Value

      after.Descendants(XName.Get "Summary")
      |> Seq.iter (fun el ->
            setAttribute el "visitedSequencePoints" "0"
            setAttribute el "sequenceCoverage" "0"
            if getAttribute el "minCrapScore" = "2.11" then
              setAttribute el "minCrapScore" "2.15"
            if getAttribute el "maxCrapScore" = "2.11" then
              setAttribute el "maxCrapScore" "2.15")

      after.Descendants(XName.Get "Method")
      |> Seq.iter
            (fun el ->
            setAttribute el "sequenceCoverage" "0"
            if getAttribute el "crapScore" = "2.11" then
              setAttribute el "crapScore" "2.15")
      after.Descendants(XName.Get "SequencePoint")
      |> Seq.toList
      |> List.iter (fun el ->
            el.Remove())
      after.Descendants(XName.Get "MethodPoint")
      |> Seq.iter (fun el -> setAttribute el "vc" "0")
      let before =
        after.ToString().Replace("uspid=\"13", "uspid=\"100663298")
               .Replace("uspid=\"1\"", "uspid=\"100663297\"")
        |> XDocument.Parse

      after.Descendants(XName.Get "Summary")
      |> Seq.iter (fun el ->
            setAttribute el "visitedBranchPoints" "0"
            setAttribute el "branchCoverage" "0"
            setAttribute el "visitedSequencePoints" "0"
            setAttribute el "sequenceCoverage" "0"
            setAttribute el "visitedClasses" "0"
            setAttribute el "visitedMethods" "0"
            if getAttribute el "minCrapScore"
              |> String.IsNullOrWhiteSpace
              |> not
            then
              setAttribute el "minCrapScore" "0"
              setAttribute el "maxCrapScore" "0")
      after.Descendants(XName.Get "Method")
      |> Seq.iter (fun el ->
            setAttribute el "visited" "false"
            setAttribute el "sequenceCoverage" "0"
            setAttribute el "branchCoverage" "0"
            if getAttribute el "crapScore"
              |> String.IsNullOrWhiteSpace
              |> not
            then setAttribute el "crapScore" "0")
      let counts = Dictionary<string, Dictionary<int, PointVisit>>()
      PostProcess.action "offset" counts ReportFormat.OpenCover (XmlAbstraction.XDoc after)
  #if !NETCOREAPP3_0
      NUnit.Framework.Assert.That(after.ToString(),
          NUnit.Framework.Is.EqualTo(before.ToString()))
  #endif

      test <@ after.ToString() = before.ToString() @>

    [<Test>]
    let JunkTokenShouldDefaultZero() =
      Runner.init()
      let visits = Dictionary<int, PointVisit>()
      let key = " "
      let result = PostProcess.lookUpVisitsByToken key visits
      match (result.Count, result.Tracks |> Seq.toList) with
      | (0L, []) -> ()
//      | _ -> Assert.Fail(sprintf "%A" result)

    [<Test>]
    let EmptyNCoverGeneratesExpectedSummary() =
      let resetInfo () = Output.info <- ignore
      resetInfo()
      Output.info "info"
      Runner.init()
      let report = XDocument()
      let builder = System.Text.StringBuilder()
      Runner.summary.Clear() |> ignore
      try
        lock Runner.summaryFormat (fun () ->
          Runner.summaryFormat <- Default
          let task = Collect()
          Output.info <- (fun s -> builder.Append(s).Append("|") |> ignore)
          let r = Runner.I.standardSummary report ReportFormat.NCover 0
          Assert.That(r, Is.EqualTo (0, 0, String.Empty))
          let expected = "Visited Classes 0 of 0 (n/a)|Visited Methods 0 of 0 (n/a)|Visited Points 0 of 0 (n/a)|"
          Assert.That
            (builder.ToString(),
             Is.EqualTo expected
               )
          let collected = task.Summary.Replace("\r",String.Empty).Replace("\n", "|")
          Assert.That(collected, Is.EqualTo expected))
      finally
        resetInfo()

    [<Test>]
    let EmptyNCoverGeneratesExpectedTCSummary() =
      let resetInfo () = Output.info <- ignore
      resetInfo()
      Output.info "info"
      Runner.init()
      let report = XDocument()
      let builder = System.Text.StringBuilder()
      Runner.summary.Clear() |> ignore
      try
        lock Runner.summaryFormat (fun () ->
          Runner.summaryFormat <- B
          let task = Collect()
          Output.info <- (fun s -> builder.Append(s).Append("|") |> ignore)
          let r = Runner.I.standardSummary report ReportFormat.NCover 0
          Assert.That(r, Is.EqualTo (0, 0, String.Empty))
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
        resetInfo()

    [<Test>]
    let EmptyNCoverGeneratesExpectedSummaries() =
      let resetInfo () = Output.info <- ignore
      resetInfo()
      Output.info "info"
      Runner.init()
      let report = XDocument()
      let builder = System.Text.StringBuilder()
      Runner.summary.Clear() |> ignore
      try
        lock Runner.summaryFormat (fun () ->
          Runner.summaryFormat <- BPlus
          let task = Collect()
          Output.info <- (fun s -> builder.Append(s).Append("|") |> ignore)
          let r = Runner.I.standardSummary report ReportFormat.NCover 0
          Assert.That(r, Is.EqualTo (0, 0, String.Empty))
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
        resetInfo()

    [<Test>]
    let NCoverShouldGeneratePlausibleSummary() =
      let resetInfo () = Output.info <- ignore
      resetInfo()
      Output.info "info"
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
              Runner.threshold <- Some { Statements = 25uy
                                         Branches = 0uy
                                         Methods = 0uy
                                         Crap = 0uy
                                         AltMethods = 0uy
                                         AltCrap = 0uy }
              Runner.I.standardSummary baseline ReportFormat.NCover 42
            finally
              Runner.threshold <- None
          // 80% coverage > threshold so expect return code coming in
          Assert.That(r, Is.EqualTo (42, 0, String.Empty))
          Assert.That
            (builder.ToString(),
             Is.EqualTo
               ("Visited Classes 1 of 1 (100)|Visited Methods 1 of 1 (100)|Visited Points 8 of 10 (80)|" +
                "##teamcity[buildStatisticValue key='CodeCoverageAbsCTotal' value='1']|##teamcity[buildStatisticValue key='CodeCoverageAbsCCovered' value='1']|##teamcity[buildStatisticValue key='CodeCoverageAbsMTotal' value='1']|##teamcity[buildStatisticValue key='CodeCoverageAbsMCovered' value='1']|##teamcity[buildStatisticValue key='CodeCoverageAbsSTotal' value='10']|##teamcity[buildStatisticValue key='CodeCoverageAbsSCovered' value='8']|")
            ))
      finally
        resetInfo()

    [<Test>]
    let EmptyOpenCoverGeneratesExpectedSummary() =
      let resetInfo () = Output.info <- ignore
      resetInfo()
      Output.info "info"
      Runner.init()
      let report = XDocument.Load(new System.IO.StringReader("""<CoverageSession>
  <Summary numSequencePoints="0" visitedSequencePoints="0" numBranchPoints="0" visitedBranchPoints="0" sequenceCoverage="0" branchCoverage="0" maxCyclomaticComplexity="0" minCyclomaticComplexity="1" visitedClasses="0" numClasses="0" visitedMethods="0" numMethods="0" />
</CoverageSession>"""))
      let builder = System.Text.StringBuilder()
      try
        lock Runner.summaryFormat (fun () ->
          Runner.summaryFormat <- Default
          Output.info <- (fun s -> builder.Append(s).Append("|") |> ignore)
          let r = Runner.I.standardSummary report ReportFormat.OpenCover 0
          Assert.That(r, Is.EqualTo (0, 0, String.Empty))
          Assert.That
            (builder.ToString(),
             Is.EqualTo
               ("Visited Classes 0 of 0 (n/a)|Visited Methods 0 of 0 (n/a)|"
                + "Visited Points 0 of 0 (0)|Visited Branches 0 of 0 (0)||"
                + "==== Alternative Results (includes all methods including those without corresponding source) ====|"
                + "Alternative Visited Classes 0 of 0 (n/a)|Alternative Visited Methods 0 of 0 (n/a)|")))
      finally
        resetInfo()

    [<Test>]
    let EmptyOpenCoverGeneratesExpectedTCSummary() =
      let resetInfo () = Output.info <- ignore
      resetInfo()
      Output.info "info"
      Runner.init()
      let report = XDocument.Load(new System.IO.StringReader("""<CoverageSession>
  <Summary numSequencePoints="0" visitedSequencePoints="0" numBranchPoints="0" visitedBranchPoints="0" sequenceCoverage="0" branchCoverage="0" maxCyclomaticComplexity="0" minCyclomaticComplexity="1" visitedClasses="0" numClasses="0" visitedMethods="0" numMethods="0" />
</CoverageSession>"""))
      let builder = System.Text.StringBuilder()
      try
        lock Runner.summaryFormat (fun () ->
          Runner.summaryFormat <- B
          Output.info <- (fun s -> builder.Append(s).Append("|") |> ignore)
          let r = Runner.I.standardSummary report ReportFormat.OpenCover 0
          Assert.That(r, Is.EqualTo (0, 0, String.Empty))
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
        resetInfo()

    [<Test>]
    let EmptyOpenCoverGeneratesExpectedSummaries() =
      let resetInfo () = Output.info <- ignore
      resetInfo()
      Output.info "info"
      Runner.init()
      let report = XDocument.Load(new System.IO.StringReader("""<CoverageSession>
  <Summary numSequencePoints="0" visitedSequencePoints="0" numBranchPoints="0" visitedBranchPoints="0" sequenceCoverage="0" branchCoverage="0" maxCyclomaticComplexity="0" minCyclomaticComplexity="1" visitedClasses="0" numClasses="0" visitedMethods="0" numMethods="0" />
</CoverageSession>"""))
      let builder = System.Text.StringBuilder()
      try
        lock Runner.summaryFormat (fun () ->
          Runner.summaryFormat <- RPlus
          Output.info <- (fun s -> builder.Append(s).Append("|") |> ignore)
          let r = Runner.I.standardSummary report ReportFormat.OpenCover 0
          Assert.That(r, Is.EqualTo (0, 0, String.Empty))
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
        resetInfo()

    [<Test>]
    let OpenCoverShouldGeneratePlausibleSummary() =
      let resetInfo () = Output.info <- ignore
      resetInfo()
      Output.info "info"
      Runner.init()
      let resource =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find
             (fun n -> n.EndsWith("Sample1WithOpenCover.xml", StringComparison.Ordinal))
      use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
      let baseline = XDocument.Load(stream)
      let thresholds = [
        Threshold.Default()
        { Threshold.Default() with Statements = 75uy }
        { Threshold.Default() with Branches = 70uy }
        { Threshold.Default() with Methods = 100uy }
        { Threshold.Default() with AltMethods = 100uy }
        { Threshold.Default() with Crap = 1uy }
        { Threshold.Default() with AltCrap = 1uy }
        { Threshold.Default() with Crap = 255uy }
        { Threshold.Default() with Statements = 75uy
                                   Branches = 70uy }
        { Threshold.Default() with Statements = 75uy
                                   AltMethods = 100uy }
      ]
      let results = [
        (23, 0, String.Empty)
        (5, 75, "Statements")
        (4, 70, "Branches")
        (23, 0, String.Empty)
        (50, 100, "AltMethods")
        (2, 1, "Crap")
        (2, 1, "AltCrap")
        (23, 0, String.Empty)
        (5, 75, "Statements")
        (50, 100, "AltMethods")
      ]

      List.zip thresholds results
      |> List.iter (fun (threshold, expected) ->
        try
          lock Runner.summaryFormat (fun () ->
            Runner.summaryFormat <- BPlus
            let builder = System.Text.StringBuilder()
            Output.info <- (fun s -> builder.Append(s).Append("|") |> ignore)
            let r =
              try
                Runner.threshold <- Some threshold
                Runner.I.standardSummary baseline ReportFormat.OpenCover 23
              finally
                Runner.threshold <- None
            // 70% coverage < threshold so expect shortfall
            Assert.That(r, Is.EqualTo expected)
            Assert.That
              (builder.ToString(),
               Is.EqualTo
                 ("Visited Classes 1 of 1 (100)|Visited Methods 1 of 1 (100)|"
                  + "Visited Points 7 of 10 (70)|Visited Branches 2 of 3 (66.67)|Maximum CRAP score 2.11||"
                  + "==== Alternative Results (includes all methods including those without corresponding source) ====|"
                  + "Alternative Visited Classes 1 of 1 (100)|Alternative Visited Methods 1 of 2 (50)|Alternative maximum CRAP score 2.11|"
                  + "##teamcity[buildStatisticValue key='CodeCoverageAbsCTotal' value='1']|##teamcity[buildStatisticValue key='CodeCoverageAbsCCovered' value='1']|##teamcity[buildStatisticValue key='CodeCoverageAbsMTotal' value='1']|##teamcity[buildStatisticValue key='CodeCoverageAbsMCovered' value='1']|##teamcity[buildStatisticValue key='CodeCoverageAbsSTotal' value='10']|##teamcity[buildStatisticValue key='CodeCoverageAbsSCovered' value='7']|"
                  + "##teamcity[buildStatisticValue key='CodeCoverageAbsBTotal' value='3']|##teamcity[buildStatisticValue key='CodeCoverageAbsBCovered' value='2']|")
                 ))
        finally
          resetInfo())

    [<Test>]
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
        let r = summarize baseline ReportFormat.OpenCover 0
        Assert.That(r, Is.EqualTo (0, 0, String.Empty))
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
        let r = LCov.summary baseline ReportFormat.NCover 0
        Assert.That(r, Is.EqualTo (0, 0, String.Empty))
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
        let r = LCov.summary baseline ReportFormat.NCover 0
        Assert.That(r, Is.EqualTo (0, 0, String.Empty))
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
                  .GetManifestResourceStream("AltCover.Tests.coverage-04.xsd")
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

    [<Test>]
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
        let r = summarize baseline ReportFormat.NCover 0
        Assert.That(r, Is.EqualTo (0, 0, String.Empty))
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

    [<Test>]
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
        let r = Cobertura.summary baseline ReportFormat.NCover 0
        Assert.That(r, Is.EqualTo (0, 0, String.Empty))
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

    [<Test>]
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
        let r = Cobertura.summary baseline ReportFormat.OpenCover 0
        Assert.That(r, Is.EqualTo (0, 0, String.Empty))
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

    [<Test>]
    let ThresholdViolationShouldBeReported() =
      Runner.init()
      let saveErr = Output.error
      let saveSummaries = Runner.I.summaries
      let builder = System.Text.StringBuilder()
      let saved = Runner.threshold
      try
        Runner.I.summaries <- [ (fun _ _ a  -> (a, 0uy, String.Empty))
                                (fun _ _ _ -> (23, 42uy, "Statements"))
                                (fun _ _ a  -> (a, 0uy, String.Empty))]
        Output.error <- (fun s -> builder.Append(s).Append("|") |> ignore)
        let delta = Runner.J.doSummaries (XDocument()) ReportFormat.NCover 0
        Assert.That(delta, Is.EqualTo 23)
        Assert.That
          (builder.ToString(),
           Is.EqualTo "Statement coverage percentage achieved is 23% below the threshold of 42%.|")
      finally
        Output.error <- saveErr
        Runner.I.summaries <- saveSummaries
        Runner.threshold <- saved

    [<Test>]
    let TryGetValueHandlesNull() =
      Runner.init()
      let dict : Dictionary<int, int> = null
      Assert.That(PostProcess.tryGetValue dict 0 |> fst, Is.False)