﻿namespace Tests
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
#nowarn "3559" // TODO

module Usage =
  let internal usageText =
    let usage =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("AltCover.Usage.txt", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(usage)

    use reader = new StreamReader(stream)
    reader.ReadToEnd()

  let internal runnerText =
    let usage =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("AltCover.Runner.Usage.txt", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(usage)

    use reader = new StreamReader(stream)
    reader.ReadToEnd()

module Runner =
  // TODO needs tests breaking out for other modules (LCov, Json etc.)

  [<Test>]
  let ShouldFailXmlDataForNativeJson () =
    Assert.Throws<NotSupportedException>(fun () ->
      ignore (ReportFormat.NativeJson |> Counter.I.XmlByFormat))
    |> ignore

  [<Test>]
  let MaxTimeFirst () =
    let now = DateTime.Now
    let ago = now - TimeSpan(1, 0, 0, 0)
    test <@ (Counter.I.MaxTime(now, ago)) = now @>

  [<Test>]
  let MaxTimeLast () =
    let now = DateTime.Now
    let ago = now - TimeSpan(1, 0, 0, 0)
    test <@ (Counter.I.MaxTime(ago, now)) = now @>

  [<Test>]
  let MinTimeFirst () =
    let now = DateTime.Now
    let ago = now - TimeSpan(1, 0, 0, 0)
    test <@ (Counter.I.MinTime(ago, now)) = ago @>

  [<Test>]
  let MinTimeLast () =
    let now = DateTime.Now
    let ago = now - TimeSpan(1, 0, 0, 0)
    test <@ (Counter.I.MinTime(now, ago)) = ago @>

  [<Test>]
  let JunkUspidGivesNegativeIndex () =
    AltCover.Runner.init ()
    let key = " "

    let index =
      Counter.I.FindIndexFromUspid(0, key)

    test <@ index < 0 @>

  [<Test>]
  let RealIdShouldIncrementCount () =
    Runner.init ()

    let visits =
      new Dictionary<string, Dictionary<int, PointVisit>>()

    visits.Add(" ", Dictionary<int, PointVisit>())

    let key = " "

    let v1 =
      Counter.AddVisit(visits, key, 23, new Null())

    Assert.That(v1, Is.EqualTo 1)
    Assert.That(visits.Count, Is.EqualTo 1)
    Assert.That(visits.[key].Count, Is.EqualTo 1)
    let x = visits.[key].[23]
    Assert.That(x.Count, Is.EqualTo 1)
    Assert.That(x.Tracks, Is.Empty)

  [<Test>]
  let RealIdShouldIncrementList () =
    Runner.init ()

    let visits =
      new Dictionary<string, Dictionary<int, PointVisit>>()

    visits.Add(" ", Dictionary<int, PointVisit>())

    let key = " "
    let payload = Time DateTime.UtcNow.Ticks

    let v2 =
      Counter.AddVisit(visits, key, 23, payload)

    Assert.That(v2, Is.EqualTo 1)
    Assert.That(visits.Count, Is.EqualTo 1)
    Assert.That(visits.[key].Count, Is.EqualTo 1)
    let x = visits.[key].[23]
    Assert.That(x.Count, Is.EqualTo 0)
    Assert.That(x.Tracks, Is.EquivalentTo [ payload ])

  [<Test>]
  let DistinctIdShouldBeDistinct () =
    Runner.init ()

    let visits =
      new Dictionary<string, Dictionary<int, PointVisit>>()

    visits.Add(" ", Dictionary<int, PointVisit>())
    visits.Add("key", Dictionary<int, PointVisit>())

    let key = " "

    let v3 =
      Counter.AddVisit(visits, key, 23, new Null())

    Assert.That(v3, Is.EqualTo 1)

    let v4 =
      Counter.AddVisit(visits, "key", 42, new Null())

    Assert.That(visits.Count, Is.EqualTo 2)
    Assert.That(v4, Is.EqualTo 1)

  [<Test>]
  let DistinctLineShouldBeDistinct () =
    Runner.init ()

    let visits =
      new Dictionary<string, Dictionary<int, PointVisit>>()

    visits.Add(" ", Dictionary<int, PointVisit>())

    let key = " "

    let v5 =
      Counter.AddVisit(visits, key, 23, new Null())

    Assert.That(v5, Is.EqualTo 1)

    let v6 =
      Counter.AddVisit(visits, key, 42, new Null())

    Assert.That(v6, Is.EqualTo 1)
    Assert.That(visits.Count, Is.EqualTo 1)
    Assert.That(visits.[key].Count, Is.EqualTo 2)

  [<Test>]
  let RepeatVisitsShouldIncrementCount () =
    Runner.init ()

    let visits =
      new Dictionary<string, Dictionary<int, PointVisit>>()

    visits.Add(" ", Dictionary<int, PointVisit>())

    let key = " "

    let v7 =
      Counter.AddVisit(visits, key, 23, new Null())

    Assert.That(v7, Is.EqualTo 1)

    let v8 =
      Counter.AddVisit(visits, key, 23, new Null())

    Assert.That(v8, Is.EqualTo 1)
    let x = visits.[key].[23]
    Assert.That(x.Count, Is.EqualTo 2)
    Assert.That(x.Tracks, Is.Empty)

  [<Test>]
  let RepeatVisitsShouldIncrementTotal () =
    Runner.init ()

    let visits =
      new Dictionary<string, Dictionary<int, PointVisit>>()

    visits.Add(" ", Dictionary<int, PointVisit>())

    let key = " "
    let payload = Time DateTime.UtcNow.Ticks

    let v9 =
      Counter.AddVisit(visits, key, 23, new Null())

    Assert.That(v9, Is.EqualTo 1)

    let v10 =
      Counter.AddVisit(visits, key, 23, payload)

    Assert.That(v10, Is.EqualTo 1)
    let x = visits.[key].[23]
    Assert.That(x.Count, Is.EqualTo 1)
    Assert.That(x.Tracks, Is.EquivalentTo [ payload ])

  let resource =
    Assembly.GetExecutingAssembly().GetManifestResourceNames()
    |> Seq.find _.EndsWith("SimpleCoverage.xml", StringComparison.Ordinal)

  let resource2 =
    Assembly.GetExecutingAssembly().GetManifestResourceNames()
    |> Seq.find _.EndsWith("Sample1WithOpenCover.xml", StringComparison.Ordinal)

  let internal init n l =
    let mutable tmp = PointVisit.Create()
    tmp.Count <- n
    tmp.Tracks.AddRange l
    tmp

  [<Test>]
  let KnownModuleWithPayloadMakesExpectedChangeInOpenCover () =
    Runner.init ()

    Counter.measureTime <-
      DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)

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

    [ 0..9 ]
    |> Seq.iter (fun i -> payload.[10 - i] <- init (int64 (i + 1)) [])

    [ 11..12 ]
    |> Seq.iter (fun i -> payload.[i ||| Counter.branchFlag] <- init (int64 (i - 10)) [])

    let item =
      Dictionary<string, Dictionary<int, PointVisit>>()

    item.Add("7C-CD-66-29-A3-6C-6D-5F-A7-65-71-0E-22-7D-B2-61-B5-1F-65-9A", payload)

    Counter.I.UpdateReport(
      ignore,
      (fun _ _ -> ()),
      true,
      item,
      ReportFormat.OpenCover,
      worker,
      worker2
    )
    |> ignore

    worker2.Position <- 0L
    let after = XmlDocument()
    after.Load worker2

    Assert.That(
      after.SelectNodes("//SequencePoint")
      |> Seq.cast<XmlElement>
      |> Seq.map _.GetAttribute("vc"),
      Is.EquivalentTo
        [ "11"
          "10"
          "9"
          "8"
          "7"
          "6"
          "4"
          "3"
          "2"
          "1" ]
    )

    Assert.That(
      after.SelectNodes("//BranchPoint")
      |> Seq.cast<XmlElement>
      |> Seq.map _.GetAttribute("vc"),
      Is.EquivalentTo [ "2"; "2" ]
    )

  // Runner.fs and CommandLine.fs
  [<Test>]
  let UsageIsAsExpected () =
    Runner.init ()
    let options = Runner.declareOptions ()
    let saved = Console.Error

    try
      use stderr = new StringWriter()
      Console.SetError stderr
      let empty = OptionSet()

      CommandLine.usageBase
        { Intro = "UsageError"
          Options = empty
          Options2 = options }

      let result =
        stderr.ToString().Replace("\r\n", "\n")

      let expected =
        "Error - usage is:\n"
        + Usage.runnerText
        + "\nor\n"
        + "  ImportModule               Prints out the PowerShell script to import the\n"
        + "                               associated PowerShell module\n"
        + "or\n"
        + "  Version                    Prints out the AltCover build version\n"
        + "or, for the global tool only\n"
        + "  TargetsPath                Prints out the path to the 'altcover.global.targets' file\n"
        + "                               (as the tool cannot be 'dotnet add'ed to the project).\n"
        + "                               The 'altcover.global.props' file is present in the same directory\n"

      Assert.That(
        result.Replace("\u200b", String.Empty).Replace("\r\n", "\n"),
        Is.EqualTo(expected.Replace("\r\n", "\n")),
        "*" + result + "*"
      )
    finally
      Console.SetError saved

  [<Test>]
  let ShouldLaunchWithExpectedOutput () =
    Runner.init ()

    let path =
      Path.Combine(SolutionRoot.location, "_Mono/Sample1")

    maybeIgnore (fun () -> path |> Directory.Exists |> not)
    let files = Directory.GetFiles(path)

    let program =
      files
      |> Seq.filter _.EndsWith(".exe", StringComparison.OrdinalIgnoreCase)
      |> Seq.head

    CommandLine.toConsole ()
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

      let nonWindows =
        System.Environment.GetEnvironmentVariable("OS")
        <> "Windows_NT"

      let exe, args =
        Maybe nonWindows ("mono", "\"" + program + "\"") (program, String.Empty)

      let r =
        CommandLine.I.launch
          exe
          args
          (Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location))

      Assert.That(r, Is.EqualTo 0)
      Assert.That(stderr.ToString(), Is.Empty)
      let result = stdout.ToString()

      let quote =
        Maybe
          (System.Environment.GetEnvironmentVariable("OS") = "Windows_NT")
          "\""
          String.Empty

      let expected =
        "Command line : '"
        + quote
        + exe
        + quote
        + " "
        + args
        + "\'"
        + Environment.NewLine
        + "Where is my rocket pack? "
        + Environment.NewLine

      Assert.That(result, Is.EqualTo(expected))
    finally
      Console.SetOut(fst saved)
      Console.SetError(snd saved)
      Output.verbose <- ignore

  [<Test>]
  let ShouldHaveExpectedOptions () =
    Runner.init ()
    let options = Runner.declareOptions ()
    let optionCount = 13

    let optionNames =
      options
      |> Seq.map (fun o -> (o.GetNames() |> Seq.maxBy _.Length).ToLowerInvariant())
      |> Seq.sort
      |> Seq.toList

    // Options add "<>" and "help"
    Assert.That(
      options.Count,
      Is.EqualTo(optionCount + 2),
      String.Join("; ", optionNames)
    )

    let optionNames =
      options
      |> Seq.map (fun o -> (o.GetNames() |> Seq.maxBy _.Length).ToLowerInvariant())
      |> Seq.sort
      |> Seq.toList

    let primitiveNames =
      typeof<Primitive.CollectOptions>
      |> FSharpType.GetRecordFields
      |> Seq.map _.Name.ToLowerInvariant()
      |> Seq.sort
      |> Seq.toList

    // swap "collect" and "commandline"
    Assert.That(
      primitiveNames |> List.length,
      Is.EqualTo(optionCount - 1), // drop -q/--verbose => verbosity
      "expected "
      + String.Join("; ", optionNames)
      + Environment.NewLine
      + "but got primitives "
      + String.Join("; ", primitiveNames)
    )

    let typesafeNames =
      typeof<TypeSafe.CollectOptions>
      |> FSharpType.GetRecordFields
      |> Seq.map _.Name.ToLowerInvariant()
      |> Seq.sort
      |> Seq.toList

    Assert.That(
      typesafeNames |> List.length,
      Is.EqualTo(optionCount - 1), // drop -q/--verbose => verbosity
      "expected "
      + String.Join("; ", optionNames)
      + Environment.NewLine
      + "but got typesafe "
      + String.Join("; ", typesafeNames)
    )

    let fsapiNames =
      typeof<AltCover.CollectOptions>.GetProperties()
      |> Seq.map _.Name.ToLowerInvariant()
      |> Seq.sort
      |> Seq.toList

    let fsapiCases =
      (typeof<AltCover.CollectOptions>
       |> FSharpType.GetUnionCases)
        .Length

    let args =
      Primitive.CollectOptions.Create()
      |> AltCover.CollectOptions.Primitive

    let commandFragments =
      Args.buildCollect args

    // adds Runner and the trailing command line arguments
    Assert.That(
      commandFragments |> List.length,
      Is.EqualTo(optionCount + 1), // drop -q/--verbose => verbosity
      "expected "
      + String.Join("; ", optionNames)
      + Environment.NewLine
      + "but got fragments "
      + String.Join("; ", typesafeNames)
    ) // todo

    // Adds "Tag", "IsPrimitive", "IsTypeSafe"
    Assert.That(
      fsapiNames |> Seq.length,
      Is.EqualTo(optionCount + fsapiCases), // drop -q/--verbose => verbosity
      "expected "
      + String.Join("; ", primitiveNames)
      + Environment.NewLine
      + "but got apinames "
      + String.Join("; ", fsapiNames)
    )

    let taskNames =
      typeof<Collect>
        .GetProperties(
          BindingFlags.DeclaredOnly
          ||| BindingFlags.Public
          ||| BindingFlags.Instance
        )
      |> Seq.map _.Name.ToLowerInvariant()
      |> Seq.sort
      |> Seq.toList

    // gains summary (output)
    Assert.That(
      taskNames |> Seq.length,
      Is.EqualTo(optionCount), // drop -q/--verbose => verbosity
      "expected "
      + String.Join("; ", primitiveNames)
      + Environment.NewLine
      + "but got tasks "
      + String.Join("; ", taskNames)
    )

    let targets =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("AltCover.proj", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(targets)

    let doc = XDocument.Load stream

    let collect =
      doc.Descendants()
      |> Seq.filter (fun d -> d.Name.LocalName = "AltCover.Collect")
      |> Seq.head

    let attributeNames =
      collect.Attributes()
      |> Seq.map _.Name.LocalName.ToLowerInvariant()
      |> Seq.sort
      |> Seq.toList

    // loses commandline; executable; exposereturncode; outputfile; workingdirectory
    //       N/A,         N/A,        N/A,              fixed,      N/A
    Assert.That(
      attributeNames |> Seq.length,
      Is.EqualTo(optionCount - 6), // drop -q/--verbose => verbosity
      "expected "
      + String.Join("; ", primitiveNames)
      + Environment.NewLine
      + "but got  "
      + String.Join("; ", attributeNames)
    )

    Assert.That(
      options
      |> Seq.filter (fun x -> x.Prototype <> "<>")
      |> Seq.forall (_.Description >> String.IsNullOrWhiteSpace >> not)
    )

    Assert.That(
      options
      |> Seq.filter (fun x -> x.Prototype = "<>")
      |> Seq.length,
      Is.EqualTo 1
    )

  [<Test>]
  let ParsingJunkIsAnError () =
    Runner.init ()
    let options = Runner.declareOptions ()

    let parse =
      CommandLine.parseCommandLine [| "/@thisIsNotAnOption" |] options

    match parse with
    | Left(x, y) ->
      Assert.That(x, Is.EqualTo "UsageError")
      Assert.That(y, Is.SameAs options)

  [<Test>]
  let ParsingJunkAfterSeparatorIsExpected () =
    Runner.init ()
    let options = Runner.declareOptions ()

    let input =
      [| "--"
         "/@thisIsNotAnOption"
         "this should be OK" |]

    let parse =
      CommandLine.parseCommandLine input options

    match parse with
    | Right(x, y) ->
      Assert.That(x, Is.EquivalentTo(input |> Seq.skip 1))
      Assert.That(y, Is.SameAs options)

  [<Test>]
  let ParsingHelpGivesHelp () =
    Runner.init ()
    let options = Runner.declareOptions ()
    let input = [| "--?" |]

    let parse =
      CommandLine.parseCommandLine input options

    match parse with
    | Right(x, y) -> Assert.That(y, Is.SameAs options)

    match CommandLine.processHelpOption parse with
    | Left(x, y) ->
      Assert.That(x, Is.EqualTo "HelpText")
      Assert.That(y, Is.SameAs options)
    // a "not sticky" test
    lock Runner.executable (fun () ->
      Runner.executable.Value <- None

      match
        CommandLine.parseCommandLine [| "/x"; "x" |] options
        |> CommandLine.processHelpOption
      with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty))

  [<Test>]
  let ParsingErrorHelpGivesHelp () =
    Runner.init ()
    let options = Runner.declareOptions ()

    let input =
      [| "--zzz"
         Path.GetInvalidPathChars() |> String |]

    let parse =
      CommandLine.parseCommandLine input options

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
      Runner.executable.Value <- None

      match
        CommandLine.parseCommandLine [| "/x"; "x" |] options
        |> CommandLine.processHelpOption
      with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty))

  [<Test>]
  let ParsingExeGivesExe () =
    Runner.init ()

    lock Runner.executable (fun () ->
      try
        Runner.executable.Value <- None
        let options = Runner.declareOptions ()
        let unique = "some exe"
        let input = [| "-x"; unique |]

        let parse =
          CommandLine.parseCommandLine input options

        match parse with
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)

        match Runner.executable.Value with
        | Some x -> Assert.That(Path.GetFileName x, Is.EqualTo unique)
      finally
        Runner.executable.Value <- None)

  [<Test>]
  let ParsingMultipleExeGivesFailure () =
    Runner.init ()

    lock Runner.executable (fun () ->
      try
        Runner.executable.Value <- None
        let options = Runner.declareOptions ()
        let unique = Guid.NewGuid().ToString()

        let input =
          [| "-x"
             unique
             "/x"
             unique.Replace("-", "+") |]

        let parse =
          CommandLine.parseCommandLine input options

        match parse with
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")

          Assert.That(
            CommandLine.error |> Seq.head,
            Is.EqualTo "--executable : specify this only once"
          )
      finally
        Runner.executable.Value <- None)

  [<Test>]
  let ParsingNoExeGivesFailure () =
    Runner.init ()

    lock Runner.executable (fun () ->
      try
        Runner.executable.Value <- None
        let options = Runner.declareOptions ()
        let blank = " "
        let input = [| "-x"; blank |]

        let parse =
          CommandLine.parseCommandLine input options

        match parse with
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.executable.Value <- None)

  [<Test>]
  let ParsingWorkerGivesWorker () =
    Runner.init ()

    try
      Runner.workingDirectory <- None
      let options = Runner.declareOptions ()
      let unique = "."
      let input = [| "-w"; unique |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      match Runner.workingDirectory with
      | Some x -> Assert.That(x, Is.EqualTo(canonicalDirectory unique))
    finally
      Runner.workingDirectory <- None

  [<Test>]
  let ParsingMultipleWorkerGivesFailure () =
    Runner.init ()

    try
      Runner.workingDirectory <- None
      let options = Runner.declareOptions ()

      let input =
        [| "-w"
           Path.GetFullPath(".")
           "/w"
           Path.GetFullPath("..") |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")

        Assert.That(
          CommandLine.error |> Seq.head,
          Is.EqualTo "--workingDirectory : specify this only once"
        )
    finally
      Runner.workingDirectory <- None

  [<Test>]
  let ParsingBadWorkerGivesFailure () =
    Runner.init ()

    try
      Runner.workingDirectory <- None
      let options = Runner.declareOptions ()

      let unique =
        Guid.NewGuid().ToString().Replace("-", "*")

      let input = [| "-w"; unique |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      Runner.workingDirectory <- None

  [<Test>]
  let ParsingNoWorkerGivesFailure () =
    Runner.init ()

    try
      Runner.workingDirectory <- None
      let options = Runner.declareOptions ()
      let input = [| "-w" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      Runner.workingDirectory <- None

  [<Test>]
  let ParsingRecorderGivesRecorder () =
    Runner.init ()

    try
      Runner.recordingDirectory <- None
      let options = Runner.declareOptions ()
      let unique = "."
      let input = [| "-r"; unique |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      match Runner.recordingDirectory with
      | Some x -> Assert.That(x, Is.EqualTo(canonicalDirectory unique))
    finally
      Runner.recordingDirectory <- None

  [<Test>]
  let ParsingMultipleRecorderGivesFailure () =
    Runner.init ()

    try
      Runner.recordingDirectory <- None
      let options = Runner.declareOptions ()

      let input =
        [| "-r"
           Path.GetFullPath(".")
           "/r"
           Path.GetFullPath("..") |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")

        Assert.That(
          CommandLine.error |> Seq.head,
          Is.EqualTo "--recorderDirectory : specify this only once"
        )
    finally
      Runner.recordingDirectory <- None

  [<Test>]
  let ParsingBadRecorderGivesFailure () =
    Runner.init ()

    try
      Runner.recordingDirectory <- None
      let options = Runner.declareOptions ()

      let unique =
        Guid.NewGuid().ToString().Replace("-", "*")

      let input = [| "-r"; unique |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      Runner.recordingDirectory <- None

  [<Test>]
  let ParsingNoRecorderGivesFailure () =
    Runner.init ()

    try
      Runner.recordingDirectory <- None
      let options = Runner.declareOptions ()
      let input = [| "-r" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      Runner.recordingDirectory <- None

  [<Test>]
  let ParsingCollectGivesCollect () =
    Runner.init ()

    try
      Runner.collect.Value <- false
      let options = Runner.declareOptions ()
      let input = [| "--collect" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      Assert.That(Runner.collect.Value, Is.True)
    finally
      Runner.collect.Value <- false

  [<Test>]
  let ParsingMultipleCollectGivesFailure () =
    Runner.init ()

    try
      Runner.collect.Value <- false
      let options = Runner.declareOptions ()
      let input = [| "--collect"; "--collect" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")

        Assert.That(
          CommandLine.error |> Seq.head,
          Is.EqualTo "--collect : specify this only once"
        )
    finally
      Runner.collect.Value <- false

  [<Test>]
  let ParsingLcovGivesLcov () =
    Runner.init ()

    lock LCov.path (fun () ->
      try
        LCov.path.Value <- None
        Runner.I.initSummary ()

        let options = Runner.declareOptions ()
        let unique = "some exe"
        let input = [| "-l"; unique |]

        let parse =
          CommandLine.parseCommandLine input options

        match parse with
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)

        match LCov.path.Value with
        | Some x -> Assert.That(Path.GetFileName x, Is.EqualTo unique)

        Assert.That(Runner.I.summaries.Length, Is.EqualTo 2)
      finally
        Runner.I.initSummary ()
        LCov.path.Value <- None)

  [<Test>]
  let ParsingMultipleLcovGivesFailure () =
    Runner.init ()

    lock LCov.path (fun () ->
      try
        LCov.path.Value <- None
        Runner.I.initSummary ()

        let options = Runner.declareOptions ()
        let unique = Guid.NewGuid().ToString()

        let input =
          [| "-l"
             unique
             "/l"
             unique.Replace("-", "+") |]

        let parse =
          CommandLine.parseCommandLine input options

        match parse with
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")

          Assert.That(
            CommandLine.error |> Seq.head,
            Is.EqualTo "--lcovReport : specify this only once"
          )
      finally
        Runner.I.initSummary ()
        LCov.path.Value <- None)

  [<Test>]
  let ParsingNoLcovGivesFailure () =
    Runner.init ()

    lock LCov.path (fun () ->
      try
        LCov.path.Value <- None
        Runner.I.initSummary ()

        let options = Runner.declareOptions ()
        let blank = " "
        let input = [| "-l"; blank |]

        let parse =
          CommandLine.parseCommandLine input options

        match parse with
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.I.initSummary ()
        LCov.path.Value <- None)

  [<Test>]
  let ParsingThresholdGivesThreshold () =
    Runner.init ()

    try
      Runner.threshold <- None
      let options = Runner.declareOptions ()
      let input = [| "-t"; "57" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      match Runner.threshold with
      | Some x ->
        Assert.That(
          x,
          Is.EqualTo
            { Statements = 57uy
              Branches = 0uy
              Methods = 0uy
              Crap = 0uy
              AltMethods = 0uy
              AltCrap = 0uy }
        )
    finally
      Runner.threshold <- None

  [<Test>]
  let ParsingComplexThresholdGivesThreshold () =
    Runner.init ()

    try
      Runner.threshold <- None
      let options = Runner.declareOptions ()

      let input =
        [| "-t"; "M57C42S16B7AM14AC101" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      match Runner.threshold with
      | Some x ->
        Assert.That(
          x,
          Is.EqualTo
            { Statements = 16uy
              Branches = 7uy
              Methods = 57uy
              Crap = 42uy
              AltMethods = 14uy
              AltCrap = 101uy }
        )
    finally
      Runner.threshold <- None

  [<Test>]
  let ParsingTopThresholdGivesThreshold () =
    Runner.init ()

    try
      Runner.threshold <- None
      let options = Runner.declareOptions ()

      let input =
        [| "-t"; "M100C255S100B100AM100AC255" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      match Runner.threshold with
      | Some x ->
        Assert.That(
          x,
          Is.EqualTo
            { Statements = 100uy
              Branches = 100uy
              Methods = 100uy
              Crap = 255uy
              AltMethods = 100uy
              AltCrap = 255uy }
        )
    finally
      Runner.threshold <- None

  [<Test>]
  let ParsingLowThresholdGivesThreshold () =
    Runner.init ()

    try
      Runner.threshold <- None
      let options = Runner.declareOptions ()
      let input = [| "-t"; "M0C0S0B0AM0AC0" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      match Runner.threshold with
      | Some x ->
        Assert.That(
          x,
          Is.EqualTo
            { Statements = 0uy
              Branches = 0uy
              Methods = 0uy
              Crap = 0uy
              AltMethods = 0uy
              AltCrap = 0uy }
        )
    finally
      Runner.threshold <- None

  [<Test>]
  let ParsingMultipleThresholdGivesFailure () =
    Runner.init ()

    try
      Runner.threshold <- None
      let options = Runner.declareOptions ()
      let input = [| "-t"; "23"; "/t"; "42" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")

        Assert.That(
          CommandLine.error |> Seq.head,
          Is.EqualTo "--threshold : specify this only once"
        )
    finally
      Runner.threshold <- None

  [<Test>]
  let ParsingBadThresholdGivesFailure () =
    Runner.init ()

    try
      Runner.threshold <- None
      let options = Runner.declareOptions ()
      let input = [| "-t"; "-111" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      Runner.threshold <- None

  [<Test>]
  let ParsingBadThreshold2GivesFailure () =
    Runner.init ()

    try
      Runner.threshold <- None
      let options = Runner.declareOptions ()
      let input = [| "-t"; "S" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      Runner.threshold <- None

  [<Test>]
  let ParsingBadThreshold3GivesFailure () =
    Runner.init ()

    try
      Runner.threshold <- None
      let options = Runner.declareOptions ()
      let input = [| "-t"; "X666" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      Runner.threshold <- None

  [<Test>]
  let ParsingBadThreshold4GivesFailure () =
    Runner.init ()

    try
      Runner.threshold <- None
      let options = Runner.declareOptions ()
      let input = [| "-t"; "S101" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      Runner.threshold <- None

  [<Test>]
  let ParsingBadThreshold5GivesFailure () =
    Runner.init ()

    try
      Runner.threshold <- None
      let options = Runner.declareOptions ()
      let input = [| "-t"; "M101" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      Runner.threshold <- None

  [<Test>]
  let ParsingBadThreshold6GivesFailure () =
    Runner.init ()

    try
      Runner.threshold <- None
      let options = Runner.declareOptions ()
      let input = [| "-t"; "B101" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      Runner.threshold <- None

  [<Test>]
  let ParsingBadThreshold7GivesFailure () =
    Runner.init ()

    try
      Runner.threshold <- None
      let options = Runner.declareOptions ()
      let input = [| "-t"; "C256" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      Runner.threshold <- None

  [<Test>]
  let ParsingEmptyThresholdGivesFailure () =
    Runner.init ()

    try
      Runner.threshold <- None
      let options = Runner.declareOptions ()
      let input = [| "-t"; "  " |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      Runner.threshold <- None

  [<Test>]
  let ParsingNoThresholdGivesFailure () =
    Runner.init ()

    try
      Runner.threshold <- None
      let options = Runner.declareOptions ()
      let input = [| "-t" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      Runner.threshold <- None

  [<Test>]
  let ParsingCoberturaGivesCobertura () =
    Runner.init ()

    lock Cobertura.path (fun () ->
      try
        Cobertura.path.Value <- None
        Runner.I.initSummary ()

        let options = Runner.declareOptions ()
        let unique = "some exe"
        let input = [| "-c"; unique |]

        let parse =
          CommandLine.parseCommandLine input options

        match parse with
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)

        match Cobertura.path.Value with
        | Some x -> Assert.That(Path.GetFileName x, Is.EqualTo unique)

        Assert.That(Runner.I.summaries.Length, Is.EqualTo 2)
      finally
        Runner.I.initSummary ()
        Cobertura.path.Value <- None)

  [<Test>]
  let ParsingMultipleCoberturaGivesFailure () =
    Runner.init ()

    lock Cobertura.path (fun () ->
      try
        Cobertura.path.Value <- None
        Runner.I.initSummary ()

        let options = Runner.declareOptions ()
        let unique = Guid.NewGuid().ToString()

        let input =
          [| "-c"
             unique
             "/c"
             unique.Replace("-", "+") |]

        let parse =
          CommandLine.parseCommandLine input options

        match parse with
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")

          Assert.That(
            CommandLine.error |> Seq.head,
            Is.EqualTo "--cobertura : specify this only once"
          )
      finally
        Runner.I.initSummary ()
        Cobertura.path.Value <- None)

  [<Test>]
  let ParsingNoCoberturaGivesFailure () =
    Runner.init ()

    lock Cobertura.path (fun () ->
      try
        Cobertura.path.Value <- None
        Runner.I.initSummary ()

        let options = Runner.declareOptions ()
        let blank = " "
        let input = [| "-c"; blank |]

        let parse =
          CommandLine.parseCommandLine input options

        match parse with
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.I.initSummary ()
        Cobertura.path.Value <- None)

  [<Test>]
  let ParsingPackagesGivesPackages () =
    Runner.init ()

    lock Cobertura.packages (fun () ->
      try
        Cobertura.packages.Value <- []
        Runner.I.initSummary ()

        let options = Runner.declareOptions ()
        let unique = Guid.NewGuid().ToString()
        let unique2 = Guid.NewGuid().ToString()

        let input =
          [| "-p"; unique; "--package"; unique2 |]

        let parse =
          CommandLine.parseCommandLine input options

        match parse with
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
          test <@ Cobertura.packages.Value = [ unique2; unique ] @>

      finally
        Runner.I.initSummary ()
        Cobertura.packages.Value <- [])

  [<Test>]
  let ParsingNoPackagesGivesFailure () =
    Runner.init ()

    lock Cobertura.packages (fun () ->
      try
        Cobertura.packages.Value <- []
        Runner.I.initSummary ()

        let options = Runner.declareOptions ()
        let blank = " "
        let input = [| "-p"; blank |]

        let parse =
          CommandLine.parseCommandLine input options

        match parse with
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
          test <@ CommandLine.error = [ "--package : cannot be ' '" ] @>
      finally
        Runner.I.initSummary ()
        Cobertura.packages.Value <- [])

  [<Test>]
  let ParsingOutputGivesOutput () =
    Runner.init ()

    try
      Runner.output <- None
      let options = Runner.declareOptions ()
      let unique = Guid.NewGuid().ToString()
      let input = [| "-o"; unique |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      match Runner.output with
      | Some x -> Assert.That(Path.GetFileName x, Is.EqualTo unique)
    finally
      Runner.output <- None

  [<Test>]
  let ParsingMultipleOutputGivesFailure () =
    Runner.init ()

    try
      Runner.output <- None
      Runner.collect.Value <- false

      let options = Runner.declareOptions ()
      let unique = Guid.NewGuid().ToString()

      let input =
        [| "-o"
           unique
           "/o"
           unique.Replace("-", "+") |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")

        Assert.That(
          CommandLine.error |> Seq.head,
          Is.EqualTo "--outputFile : specify this only once"
        )
    finally
      Runner.output <- None

  [<Test>]
  let ParsingNoOutputGivesFailure () =
    Runner.init ()

    try
      Runner.output <- None
      let options = Runner.declareOptions ()
      let blank = " "
      let input = [| "-o"; blank |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      Runner.output <- None

  [<Test>]
  let ParsingDropGivesDrop () =
    Runner.init ()

    try
      CommandLine.dropReturnCode.Value <- false
      let options = Runner.declareOptions ()
      let input = [| "--dropReturnCode" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      Assert.That(CommandLine.dropReturnCode.Value, Is.True)
    finally
      CommandLine.dropReturnCode.Value <- false

  [<Test>]
  let ParsingMultipleDropGivesFailure () =
    Runner.init ()

    try
      CommandLine.dropReturnCode.Value <- false
      let options = Runner.declareOptions ()

      let input =
        [| "--dropReturnCode"
           "--dropReturnCode" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")

        Assert.That(
          CommandLine.error |> Seq.head,
          Is.EqualTo "--dropReturnCode : specify this only once"
        )
    finally
      CommandLine.dropReturnCode.Value <- false

  [<Test>]
  let ParsingTCString () =
    Runner.init ()

    [ (String.Empty, Many [ B ])
      ("+", Many([ B; O; C ] |> List.rev))
      ("+b", Many([ O; C; B ] |> List.rev))
      ("+B", Many([ O; C; B ] |> List.rev))
      ("b+", Many([ B; O; C ] |> List.rev))
      ("B+", Many([ B; O; C ] |> List.rev))
      ("b", Many [ B ])
      ("B", Many [ B ])
      ("+r", Many([ O; C; R ] |> List.rev))
      ("+R", Many([ O; C; R ] |> List.rev))
      ("r+", Many([ R; O; C ] |> List.rev))
      ("R+", Many([ R; O; C ] |> List.rev))
      ("r", Many [ R ])
      ("R", Many [ R ])
      ("O", Many [ O ])
      ("CCC", Many [ C ])
      ("OCRNBOC", N)
      ("true", Default) ]
    |> List.iter (fun (x, y) -> Assert.That(SummaryFormat.Factory x, Is.EqualTo y, x))

  [<Test>]
  let ParsingTCGivesTC () =
    Runner.init ()

    [ (String.Empty, B)
      (":+", Many([ B; O; C ] |> List.rev))
      (":+b", Many([ O; C; B ] |> List.rev))
      (":+B", Many([ O; C; B ] |> List.rev))
      (":b+", Many([ B; O; C ] |> List.rev))
      (":B+", Many([ B; O; C ] |> List.rev))
      (":b", Many [ B ])
      (":B", Many [ B ])
      (":+r", Many([ O; C; R ] |> List.rev))
      (":+R", Many([ O; C; R ] |> List.rev))
      (":r+", Many([ R; O; C ] |> List.rev))
      (":R+", Many([ R; O; C ] |> List.rev))
      (":r", Many [ R ])
      (":R", Many [ R ])
      (":O", Many [ O ])
      (":CCC", Many [ C ])
      (":OCRNBOC", N) ]
    |> List.iter (fun (a, v) ->
      lock Runner.summaryFormat (fun () ->
        Runner.summaryFormat <- Default
        let options = Runner.declareOptions ()
        let input = [| "--teamcity" + a |]

        let parse =
          CommandLine.parseCommandLine input options

        match parse with
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)

        match Runner.summaryFormat with
        | x when v = x -> ()))

  [<Test>]
  let ParsingMultipleTCGivesFailure () =
    Runner.init ()

    lock Runner.summaryFormat (fun () ->
      Runner.summaryFormat <- Default
      let options = Runner.declareOptions ()
      let input = [| "--teamcity"; "--teamcity" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")

        Assert.That(
          CommandLine.error |> Seq.head,
          Is.EqualTo "--summary : specify this only once"
        ))

  [<Test>]
  let ParsingBadTCGivesFailure () =
    Runner.init ()

    lock Runner.summaryFormat (fun () ->
      Runner.summaryFormat <- Default
      let options = Runner.declareOptions ()
      let blank = " "
      let input = [| "--teamcity:junk" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError"))

  [<Test>]
  let ParsingQuietWorks () =
    Runner.init ()

    try
      let options = Runner.declareOptions ()
      let input = [| "-q" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      Assert.That(CommandLine.verbosity, Is.EqualTo 1)
    finally
      CommandLine.verbosity <- 0

  [<Test>]
  let ParsingVerboseWorks () =
    Runner.init ()

    try
      let options = Runner.declareOptions ()
      let input = [| "--verbose" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      Assert.That(CommandLine.verbosity, Is.EqualTo -1)
    finally
      CommandLine.verbosity <- 0

  [<Test>]
  let ParsingMultiQuietWorks () =
    Runner.init ()

    try
      let options = Runner.declareOptions ()
      let input = [| "-q"; "-q"; "-q" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      Assert.That(CommandLine.verbosity, Is.EqualTo 3)
    finally
      CommandLine.verbosity <- 0

  [<Test>]
  let ParsingMixedQuietWorks () =
    Runner.init ()

    try
      let options = Runner.declareOptions ()
      let input = [| "-qqq"; "--verbose"; "-q" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      Assert.That(CommandLine.verbosity, Is.EqualTo 3)
    finally
      CommandLine.verbosity <- 0

  [<Test>]
  let ParsingBatchMultiQuietWorks () =
    Runner.init ()

    try
      let options = Runner.declareOptions ()
      let input = [| "-qq" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      Assert.That(CommandLine.verbosity, Is.EqualTo 2)
    finally
      CommandLine.verbosity <- 0

  [<Test>]
  let ShouldRequireExe () =
    Runner.init ()

    lock Runner.executable (fun () ->
      try
        Runner.executable.Value <- None
        let options = Runner.declareOptions ()

        let parse =
          Runner.J.requireExe (Right([], options))

        match parse with
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.executable.Value <- None)

  [<Test>]
  let ShouldAcceptExe () =
    Runner.init ()

    lock Runner.executable (fun () ->
      try
        Runner.executable.Value <- Some "xxx"
        let options = Runner.declareOptions ()

        let parse =
          Runner.J.requireExe (Right([ "b" ], options))

        match parse with
        | Right(x :: y, z) ->
          Assert.That(z, Is.SameAs options)
          Assert.That(x, Is.EqualTo "xxx")
          Assert.That(y, Is.EquivalentTo [ "b" ])
      finally
        Runner.executable.Value <- None)

  [<Test>]
  let ShouldRequireCollectIfNotExe () =
    Runner.init ()

    lock Runner.executable (fun () ->
      try
        Runner.executable.Value <- None
        Runner.collect.Value <- true

        let options = Runner.declareOptions ()

        let parse =
          Runner.J.requireExe (Right([ "a"; "b" ], options))

        match parse with
        | Right([], z) -> Assert.That(z, Is.SameAs options)
      finally
        Runner.collect.Value <- false
        Runner.executable.Value <- None)

  [<Test>]
  let ShouldRejectExeIfCollect () =
    Runner.init ()

    lock Runner.executable (fun () ->
      try
        Runner.executable.Value <- Some "xxx"
        Runner.collect.Value <- true

        let options = Runner.declareOptions ()

        let parse =
          Runner.J.requireExe (Right([ "b" ], options))

        match parse with
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Runner.collect.Value <- false
        Runner.executable.Value <- None)

  [<Test>]
  let ShouldRequireWorker () =
    Runner.init ()

    try
      Runner.workingDirectory <- None
      let options = Runner.declareOptions ()
      let input = (Right([], options))
      let parse = Runner.J.requireWorker input

      match parse with
      | Right _ ->
        Assert.That(parse, Is.SameAs input)
        Assert.That(Option.isSome Runner.workingDirectory)
    finally
      Runner.workingDirectory <- None

  [<Test>]
  let ShouldAcceptWorker () =
    Runner.init ()

    try
      Runner.workingDirectory <- Some "ShouldAcceptWorker"
      let options = Runner.declareOptions ()
      let input = (Right([], options))
      let parse = Runner.J.requireWorker input

      match parse with
      | Right _ ->
        Assert.That(parse, Is.SameAs input)
        Assert.That(Runner.workingDirectory, Is.EqualTo(Some "ShouldAcceptWorker"))
    finally
      Runner.workingDirectory <- None

  [<Test>]
  let ShouldRequireRecorder () =
    Runner.init ()

    try
      Runner.recordingDirectory <- None
      let options = Runner.declareOptions ()
      let input = (Right([], options))
      let parse = Runner.J.requireRecorder input

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      Runner.recordingDirectory <- None

  [<Test>]
  let ShouldRequireRecorderDll () =
    Runner.init ()

    let path =
      Path.Combine(SolutionRoot.location, "_Mono/Sample1")

    try
      Runner.recordingDirectory <- Some path
      let options = Runner.declareOptions ()
      let input = (Right([], options))
      let parse = Runner.J.requireRecorder input

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      Runner.recordingDirectory <- None

  [<Test>]
  let ShouldAcceptRecorder () =
    Runner.init ()

    try
      let here =
        (Assembly.GetExecutingAssembly().Location
         |> Path.GetDirectoryName)

      let where =
        Path.Combine(here, Guid.NewGuid().ToString())

      Directory.CreateDirectory(where) |> ignore
      Runner.recordingDirectory <- Some where

      let create =
        Path.Combine(where, "AltCover.Recorder.g.dll")

      if create |> File.Exists |> not then
        do
          let recorder =
            Assembly.GetExecutingAssembly().GetManifestResourceNames()
            |> Seq.find
              _.EndsWith("AltCover.Recorder.net20.dll", StringComparison.Ordinal)

          use frombytes =
            Assembly.GetExecutingAssembly().GetManifestResourceStream(recorder)

          use libstream =
            new FileStream(create, FileMode.Create)

          frombytes.CopyTo libstream

      let options = Runner.declareOptions ()
      let input = (Right([], options))
      let parse = Runner.J.requireRecorder input

      match parse with
      | Right _ -> Assert.That(parse, Is.SameAs input)
    finally
      Runner.recordingDirectory <- None

  [<Test>]
  let ShouldHandleReturnCodes () =
    Runner.init ()
    // Hack for running while instrumented
    let where =
      Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)

#if NET472
    let path =
      Path.Combine(
        SolutionRoot.location,
        "_Binaries/Sample12/Debug+AnyCPU/net472/Sample12.exe"
      )
#else
    let path =
      Path.Combine(
        SolutionRoot.location,
        "_Binaries/Sample12/Debug+AnyCPU/net9.0/Sample12.dll"
      )
#endif

    let nonWindows =
      System.Environment.GetEnvironmentVariable("OS")
      <> "Windows_NT"

    let args =
#if NET472
      Maybe nonWindows [ "mono"; path ] [ path ]
#else
      [ "dotnet"; path ]
#endif

    let r =
      CommandLine.processTrailingArguments args
      <| DirectoryInfo(where)

    Assert.That(r, Is.EqualTo 0)

    let r2 =
      CommandLine.processTrailingArguments (args @ [ "1"; "2" ])
      <| DirectoryInfo(where)

    Assert.That(r2, Is.EqualTo 2)

    try
      CommandLine.dropReturnCode.Value <- true

      let r0 =
        CommandLine.processTrailingArguments (args @ [ "1"; "2" ])
        <| DirectoryInfo(where)

      Assert.That(r0, Is.EqualTo 0)
    finally
      CommandLine.dropReturnCode.Value <- false

  [<Test>]
  let ShouldProcessTrailingArguments () =
    Runner.init ()

    let path =
      Path.Combine(SolutionRoot.location, "_Mono/Sample1")

    maybeIgnore (fun () -> path |> Directory.Exists |> not)
    let files = Directory.GetFiles(path)

    let program =
      files
      |> Seq.filter _.EndsWith(".exe", StringComparison.OrdinalIgnoreCase)
      |> Seq.head

    CommandLine.toConsole ()
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

      let nonWindows =
        System.Environment.GetEnvironmentVariable("OS")
        <> "Windows_NT"

      let args =
        Maybe nonWindows ("mono" :: baseArgs) baseArgs

      let r =
        CommandLine.processTrailingArguments args
        <| DirectoryInfo(path)

      Assert.That(r, Is.EqualTo 0)
      Assert.That(stderr.ToString(), Is.Empty)
      stdout.Flush()
      let result = stdout.ToString()

      let quote =
        Maybe
          (System.Environment.GetEnvironmentVariable("OS") = "Windows_NT")
          "\""
          String.Empty

      let expected =
        "Command line : '"
        + quote
        + args.Head
        + quote
        + " "
        + String.Join(" ", args.Tail)
        + "'"
        + Environment.NewLine
        + "Where is my rocket pack? "
        + u1
        + "*"
        + u2
        + Environment.NewLine

      Assert.That(result, Is.EqualTo expected)
    finally
      Console.SetOut(fst saved)
      Console.SetError(snd saved)
      Output.verbose <- ignore

  [<Test>]
  let ShouldNoOp () =
    Runner.init ()

    let where =
      Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)

    let r =
      CommandLine.processTrailingArguments []
      <| DirectoryInfo(where)

    Assert.That(r, Is.EqualTo 0)

  [<Test>]
  let ErrorResponseIsAsExpected () =
    Runner.init ()
    let saved = Console.Error

    try
      use stderr = new StringWriter()
      Console.SetError stderr
      let unique = Guid.NewGuid().ToString()

      let returnCode =
        AltCover.Main.main false [| "RuNN"; "-r"; unique |]

      Assert.That(returnCode, Is.EqualTo 255)

      let result =
        stderr.ToString().Replace("\r\n", "\n")

      let expected =
        "\"RuNN\" \"-r\" \""
        + unique
        + "\"\n"
        + "--recorderDirectory : Directory "
        + unique
        + " not found\n"
        + "Error - usage is:\n"
        + Usage.usageText
        + "\nor\n"
        + Usage.runnerText
        + "\nor\n"
        + "  ImportModule               Prints out the PowerShell script to import the\n"
        + "                               associated PowerShell module\n"
        + "or\n"
        + "  Version                    Prints out the AltCover build version\n"
        + "or, for the global tool only\n"
        + "  TargetsPath                Prints out the path to the 'altcover.global.targets' file\n"
        + "                               (as the tool cannot be 'dotnet add'ed to the project).\n"
        + "                               The 'altcover.global.props' file is present in the same directory\n"

      Assert.That(
        result.Replace("\r\n", "\n").Replace("\u200b", String.Empty),
        Is.EqualTo(expected.Replace("\r\n", "\n"))
      )
    finally
      Console.SetError saved
      Output.verbose <- ignore

  let synchronized = Object()

  [<Test>]
  let ShouldGetStringConstants () =
    Runner.init ()

    let here =
      Assembly.GetExecutingAssembly().Location
      |> Path.GetDirectoryName

    let save = Runner.J.recorderName

    lock synchronized (fun () ->
      try
        let where =
          Path.Combine(here, Guid.NewGuid().ToString())

        Directory.CreateDirectory(where) |> ignore
        Runner.recordingDirectory <- Some where

        let create =
          Path.Combine(where, "AltCover.Recorder.dll")

        if create |> File.Exists |> not then
          do
            let recorder =
              Assembly.GetExecutingAssembly().GetManifestResourceNames()
              |> Seq.find
                _.EndsWith("AltCover.Recorder.net20.dll", StringComparison.Ordinal)

            use frombytes =
              Assembly.GetExecutingAssembly().GetManifestResourceStream(recorder)

            use libstream =
              new FileStream(create, FileMode.Create)

            frombytes.CopyTo libstream

        Runner.recordingDirectory <- Some where
        Runner.J.recorderName <- "AltCover.Recorder.dll"

        let instance =
          Runner.J.recorderInstance () |> snd

        Assert.That(
          instance.FullName,
          Is.EqualTo "AltCover.Recorder.Instance",
          "should be the instance"
        )

        let token =
          (Runner.J.getMethod instance "get_Token")
          |> Runner.J.getFirstOperandAsString

        Assert.That(token, Is.EqualTo "AltCover", "should be plain token")

        let report =
          (Runner.J.getMethod instance "get_ReportFile")
          |> Runner.J.getFirstOperandAsString

        Assert.That(
          report,
          Is.EqualTo "Coverage.Default.xml",
          "should be default coverage file"
        )
      finally
        Runner.recordingDirectory <- None
        Runner.J.recorderName <- save)

  [<Test>]
  let ShouldProcessPayload () =
    Runner.init ()

    let path =
      Path.Combine(SolutionRoot.location, "_Mono/Sample1")

    maybeIgnore (fun () -> path |> Directory.Exists |> not)
    let files = Directory.GetFiles(path)

    let program =
      files
      |> Seq.filter _.EndsWith(".exe", StringComparison.OrdinalIgnoreCase)
      |> Seq.head

    CommandLine.toConsole ()
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

      let nonWindows =
        System.Environment.GetEnvironmentVariable("OS")
        <> "Windows_NT"

      let args =
        Maybe nonWindows ("mono" :: baseArgs) baseArgs

      let r = Runner.J.getPayload args
      Assert.That(r, Is.EqualTo 0)
      Assert.That(stderr.ToString(), Is.Empty)
      stdout.Flush()
      let result = stdout.ToString()

      let quote =
        Maybe
          (System.Environment.GetEnvironmentVariable("OS") = "Windows_NT")
          "\""
          String.Empty

      let expected =
        "Command line : '"
        + quote
        + args.Head
        + quote
        + " "
        + String.Join(" ", args.Tail)
        + "'"
        + Environment.NewLine
        + "Where is my rocket pack? "
        + u1
        + "*"
        + u2
        + Environment.NewLine

      Assert.That(result, Is.EqualTo expected)
    finally
      Console.SetOut(fst saved)
      Console.SetError(snd saved)
      Runner.workingDirectory <- None
      Output.verbose <- ignore

  [<Test>]
  let WriteLeavesExpectedTraces () =
    Runner.init ()
    let saved = Console.Out
    let here = Directory.GetCurrentDirectory()

    let where =
      Assembly.GetExecutingAssembly().Location
      |> Path.GetDirectoryName

    let unique =
      Path.Combine(where, Guid.NewGuid().ToString())

    let reportFile =
      Path.Combine(unique, "FlushLeavesExpectedTraces.xml")

    let junkfile =
      (reportFile + "." + (Path.GetFileName unique))

    try
      use stdout = new StringWriter()
      Console.SetOut stdout
      Directory.CreateDirectory(unique) |> ignore
      Directory.SetCurrentDirectory(unique)

      Counter.measureTime <-
        DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)

      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

      do
        use worker =
          new FileStream(reportFile, FileMode.CreateNew)

        stream.CopyTo worker

      let hits = List<string * int * Track>()

      [ 0..9 ]
      |> Seq.iter (fun i ->
        for j = 1 to i + 1 do
          hits.Add("f6e3edb3-fb20-44b3-817d-f69d1a22fc2f", i, new Null())
          ignore j)

      let counts =
        Dictionary<string, Dictionary<int, PointVisit>>()

      hits
      |> Seq.iter (fun (moduleId, hitPointId, hit) ->
        if counts.ContainsKey moduleId |> not then
          counts.Add(moduleId, Dictionary<int, PointVisit>())

        AltCover.Counter.AddVisit(counts, moduleId, hitPointId, hit)
        |> ignore)

      // degenerate case
      Assert.That(junkfile |> File.Exists |> not)

      do
        use junkworker =
          new FileStream(junkfile, FileMode.CreateNew)

        junkworker.Write([||], 0, 0)
        ()

      Runner.J.doReport counts AltCover.ReportFormat.NCover junkfile None // WriteLeavesExpectedTraces
      |> ignore

      let (c0, w0) = Zip.openUpdate junkfile false

      try
        Assert.That(c0 |> isNull)
        Assert.That(w0, Is.InstanceOf<FileStream>())
        Assert.That(w0.Length, Is.EqualTo 8L)
      finally
        w0.Dispose()

      Runner.J.doReport counts AltCover.ReportFormat.NCover reportFile None // WriteLeavesExpectedTraces
      |> ignore

      use worker' =
        new FileStream(reportFile, FileMode.Open)

      let after = XmlDocument()
      after.Load worker'

      Assert.That(
        after.SelectNodes("//seqpnt")
        |> Seq.cast<XmlElement>
        |> Seq.map _.GetAttribute("visitcount"),
        Is.EquivalentTo
          [ "11"
            "10"
            "9"
            "8"
            "7"
            "6"
            "4"
            "3"
            "2"
            "1" ]
      )
    finally
      maybeDeleteFile reportFile
      maybeDeleteFile junkfile
      Console.SetOut saved
      Directory.SetCurrentDirectory(here)
      maybeIOException (fun () -> Directory.Delete(unique))

  [<Test>]
  let WriteJsonLeavesExpectedTraces () =
    Runner.init ()
    let saved = Console.Out
    let here = Directory.GetCurrentDirectory()

    let where =
      Assembly.GetExecutingAssembly().Location
      |> Path.GetDirectoryName

    let unique =
      Path.Combine(where, Guid.NewGuid().ToString())

    let reportFile =
      Path.Combine(unique, "WriteJsonLeavesExpectedTraces.json")

    let junkFile =
      (reportFile + "." + (Path.GetFileName unique))

    try
      use stdout = new StringWriter()
      Console.SetOut stdout
      Directory.CreateDirectory(unique) |> ignore
      Directory.SetCurrentDirectory(unique)

      let nativeJson =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find _.EndsWith("Sample4.native.json", StringComparison.Ordinal)

      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(nativeJson)

      do
        use worker =
          new FileStream(reportFile, FileMode.CreateNew)

        stream.CopyTo worker

      let tracks t : Track array =
        [| Null() :> Track
           Call(0) :> Track
           Time(t) :> Track
           Both(Pair.Create(t, 0)) |]

      let t0 = tracks 0L

      let hits = List<string * int * Track>()

      [ 0..9 ]
      |> Seq.iter (fun i ->
        for j = 1 to i + 1 do
          hits.Add("Sample4.dll", i ||| (Counter.branchFlag * (i % 2)), t0.[i % 4])
          ignore j)

      let counts =
        Dictionary<string, Dictionary<int, PointVisit>>()

      hits
      |> Seq.iter (fun (moduleId, hitPointId, hit) ->
        if counts.ContainsKey moduleId |> not then
          counts.Add(moduleId, Dictionary<int, PointVisit>())

        AltCover.Counter.AddVisit(counts, moduleId, hitPointId, hit)
        |> ignore)

      let entries = Dictionary<int, PointVisit>()
      let pv = PointVisit.Create()
      pv.Track(Time 512L)
      tracks (1L) |> Seq.iter pv.Track
      entries.Add(1, pv)
      counts.Add(Track.Entry, entries)

      let exits = Dictionary<int, PointVisit>()
      let pv = PointVisit.Create()
      tracks (2L) |> Seq.iter pv.Track
      pv.Track(Time 1024L)
      exits.Add(1, pv)
      exits.Add(2, pv)
      counts.Add(Track.Exit, exits)

      Runner.J.doReport counts AltCover.ReportFormat.NativeJson reportFile None // WriteJsonLeavesExpectedTraces
      |> ignore

      let jsonText =
        use worker' =
          new FileStream(reportFile, FileMode.Open)

        use reader = new StreamReader(worker')
        reader.ReadToEnd()
      // saved.WriteLine jsonText  // NOT printfn "%s" jsonText

      let visitedJson =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find
          _.EndsWith("Sample4.syntheticvisits.native.json", StringComparison.Ordinal)

      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(visitedJson)

      use reader = new StreamReader(stream)

      let expected =
        reader
          .ReadToEnd()
          .Replace('\r', '\u00FF')
          .Replace('\n', '\u00FF')
          .Replace("\u00FF\u00FF", "\u00FF")
          .Trim([| '\u00FF' |])

      Assert.That(
        jsonText
          .Replace('\r', '\u00FF')
          .Replace('\n', '\u00FF')
          .Replace("\u00FF\u00FF", "\u00FF")
          .Trim([| '\u00FF' |]),
        Is.EqualTo expected
      )
    finally
      maybeDeleteFile reportFile
      maybeDeleteFile junkFile
      Console.SetOut saved
      Directory.SetCurrentDirectory(here)
      maybeIOException (fun () -> Directory.Delete(unique))

  [<Test>]
  let ZipWriteLeavesExpectedTraces () =
    Runner.init ()
    let saved = Console.Out
    let here = Directory.GetCurrentDirectory()

    let where =
      Assembly.GetExecutingAssembly().Location
      |> Path.GetDirectoryName

    let unique =
      Path.Combine(where, Guid.NewGuid().ToString())

    let reportFile =
      Path.Combine(unique, "FlushLeavesExpectedTraces.xml")

    let reportZip = reportFile + ".zip"

    let junkfile =
      (reportFile + "." + (Path.GetFileName unique))

    let junkfile1 = junkfile + ".1"

    let junkfile2 = junkfile + ".xml"

    try
      use stdout = new StringWriter()
      Console.SetOut stdout
      Directory.CreateDirectory(unique) |> ignore
      Directory.SetCurrentDirectory(unique)

      Counter.measureTime <-
        DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)

      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

      let doc = XDocument.Load stream
      Zip.save (fun s -> doc.Save s) reportFile true // fsharplint:disable-line

      let hits = List<string * int * Track>()

      [ 0..9 ]
      |> Seq.iter (fun i ->
        for j = 1 to i + 1 do
          hits.Add("f6e3edb3-fb20-44b3-817d-f69d1a22fc2f", i, new Null())
          ignore j)

      let counts =
        Dictionary<string, Dictionary<int, PointVisit>>()

      hits
      |> Seq.iter (fun (moduleId, hitPointId, hit) ->
        if counts.ContainsKey moduleId |> not then
          counts.Add(moduleId, Dictionary<int, PointVisit>())

        AltCover.Counter.AddVisit(counts, moduleId, hitPointId, hit)
        |> ignore)

      // degenerate case 1
      Assert.That(junkfile |> File.Exists |> not)
      let (c0, w0) = Zip.openUpdate junkfile true

      try
        Assert.That(c0.IsNotNull)
        Assert.That(w0, Is.InstanceOf<MemoryStream>())
        Assert.That(w0.Length, Is.EqualTo 0L)
      finally
        w0.Dispose()
        c0.Dispose()

      // degenerate case 1a
      let junkzip = junkfile + ".zip"
      Assert.That(junkzip |> File.Exists)

      let junk1zip = junkfile1 + ".zip"

      do
        use archive =
          ZipFile.Open(junk1zip, ZipArchiveMode.Create)

        let entry =
          Guid.NewGuid().ToString() |> archive.CreateEntry

        use sink = entry.Open()
        sink.Write([| 0uy |], 0, 1)
        ()

      let (c0, w0) = Zip.openUpdate junkfile1 true

      try
        Assert.That(c0.IsNotNull)
        Assert.That(w0, Is.InstanceOf<MemoryStream>())
        Assert.That(w0.Length, Is.EqualTo 0L)
      finally
        c0.Dispose()
        w0.Dispose()

      // degenerate case 2
      Assert.That(junkfile2 |> File.Exists |> not)

      Runner.J.doReport
        counts
        (ReportFormat.NCover ||| ReportFormat.Zipped)
        junkfile2
        None // ZipWriteLeavesExpectedTraces
      |> ignore

      Assert.That(junkfile2 |> File.Exists |> not)
      let (c1, w1) = Zip.openUpdate junkfile2 true

      try
        Assert.That(c1.IsNotNull)
        Assert.That(w1, Is.InstanceOf<MemoryStream>())
        Assert.That(w1.Length, Is.EqualTo 0L)
      finally
        w1.Dispose()
        c1.Dispose()
        Assert.That(junkfile2 |> File.Exists |> not)

      Runner.J.doReport
        counts
        (ReportFormat.NCover ||| ReportFormat.Zipped)
        reportFile
        None // ZipWriteLeavesExpectedTraces
      |> ignore

      let (container, worker) =
        Zip.openUpdate reportFile true

      use worker' = worker
      use container' = container
      let after = XmlDocument()
      after.Load worker'

      Assert.That(
        after.SelectNodes("//seqpnt")
        |> Seq.cast<XmlElement>
        |> Seq.map _.GetAttribute("visitcount"),
        Is.EquivalentTo
          [ "11"
            "10"
            "9"
            "8"
            "7"
            "6"
            "4"
            "3"
            "2"
            "1" ]
      )
    finally
      Assert.That(reportFile |> File.Exists |> not)
      Assert.That(junkfile |> File.Exists |> not)
      Assert.That(junkfile1 |> File.Exists |> not)
      Assert.That(junkfile2 |> File.Exists |> not)
      maybeDeleteFile reportZip
      maybeDeleteFile (junkfile + ".zip")
      maybeDeleteFile (junkfile1 + ".zip")
      maybeDeleteFile (junkfile2 + ".zip")
      Console.SetOut saved
      Directory.SetCurrentDirectory(here)
      maybeIOException (fun () -> Directory.Delete(unique))

  [<Test>]
  let ZipWriteJsonLeavesExpectedTraces () =
    Runner.init ()
    let saved = Console.Out
    let here = Directory.GetCurrentDirectory()

    let where =
      Assembly.GetExecutingAssembly().Location
      |> Path.GetDirectoryName

    let unique =
      Path.Combine(where, Guid.NewGuid().ToString())

    let reportFile =
      Path.Combine(unique, "WriteJsonLeavesExpectedTraces.json")

    let reportZip = reportFile + ".zip"

    let junkfile =
      (reportFile + "." + (Path.GetFileName unique))

    let junkfile2 = junkfile + ".json"

    try
      use stdout = new StringWriter()
      Console.SetOut stdout
      Directory.CreateDirectory(unique) |> ignore
      Directory.SetCurrentDirectory(unique)

      let nativeJson =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find _.EndsWith("Sample4.native.json", StringComparison.Ordinal)

      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(nativeJson)

      Zip.save (stream.CopyTo) reportFile true // fsharplint:disable-line

      let tracks t : Track array =
        [| Null() :> Track
           Call(0) :> Track
           Time(t) :> Track
           Both(Pair.Create(t, 0)) |]

      let t0 = tracks 0L

      let hits = List<string * int * Track>()

      [ 0..9 ]
      |> Seq.iter (fun i ->
        for j = 1 to i + 1 do
          hits.Add("Sample4.dll", i ||| (Counter.branchFlag * (i % 2)), t0.[i % 4])
          ignore j)

      let counts =
        Dictionary<string, Dictionary<int, PointVisit>>()

      hits
      |> Seq.iter (fun (moduleId, hitPointId, hit) ->
        if counts.ContainsKey moduleId |> not then
          counts.Add(moduleId, Dictionary<int, PointVisit>())

        AltCover.Counter.AddVisit(counts, moduleId, hitPointId, hit)
        |> ignore)

      let entries = Dictionary<int, PointVisit>()
      let pv = PointVisit.Create()
      pv.Track(Time 512L)
      tracks (1L) |> Seq.iter pv.Track
      entries.Add(1, pv)
      entries.Add(2, pv)
      counts.Add(Track.Entry, entries)

      let exits = Dictionary<int, PointVisit>()
      let pv = PointVisit.Create()
      tracks (2L) |> Seq.iter pv.Track
      pv.Track(Time 1024L)
      exits.Add(1, pv)
      counts.Add(Track.Exit, exits)

      // degenerate case 1
      Assert.That(junkfile |> File.Exists |> not)
      let (c0, w0) = Zip.openUpdate junkfile true

      try
        Assert.That(c0.IsNotNull)
        Assert.That(w0, Is.InstanceOf<MemoryStream>())
        Assert.That(w0.Length, Is.EqualTo 0L)
      finally
        w0.Dispose()
        c0.Dispose()

      Runner.J.doReport
        counts
        (ReportFormat.NativeJson ||| ReportFormat.Zipped)
        reportFile
        None // ZipWriteJsonLeavesExpectedTraces
      |> ignore

      let (container, worker) =
        Zip.openUpdate reportFile true

      use container' = container

      let jsonText =
        use worker' = worker
        use reader = new StreamReader(worker')
        reader.ReadToEnd()

      let visitedJson =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find
          _.EndsWith("Sample4.syntheticvisits.native.json", StringComparison.Ordinal)

      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(visitedJson)

      use reader = new StreamReader(stream)

      let expected =
        reader
          .ReadToEnd()
          .Replace('\r', '\u00FF')
          .Replace('\n', '\u00FF')
          .Replace("\u00FF\u00FF", "\u00FF")
          .Trim([| '\u00FF' |])

      Assert.That(
        jsonText
          .Replace('\r', '\u00FF')
          .Replace('\n', '\u00FF')
          .Replace("\u00FF\u00FF", "\u00FF")
          .Trim([| '\u00FF' |]),
        Is.EqualTo expected
      )
    finally
      Assert.That(reportFile |> File.Exists |> not)
      Assert.That(junkfile |> File.Exists |> not)
      Assert.That(junkfile2 |> File.Exists |> not)
      maybeDeleteFile reportZip
      maybeDeleteFile (junkfile + ".zip")
      Console.SetOut saved
      Directory.SetCurrentDirectory(here)
      maybeIOException (fun () -> Directory.Delete(unique))

  [<Test>]
  let DivertedWriteLeavesExpectedTraces () =
    Runner.init ()
    let saved = Console.Out
    let here = Directory.GetCurrentDirectory()

    let where =
      Assembly.GetExecutingAssembly().Location
      |> Path.GetDirectoryName

    let unique =
      Path.Combine(where, Guid.NewGuid().ToString())

    let reportFile =
      Path.Combine(unique, "FlushLeavesExpectedTraces.xml")

    let outputFile =
      Path.Combine(unique, "DivertedFlushLeavesExpectedTraces.xml")

    let junkfile =
      (reportFile + "." + (Path.GetFileName unique))

    try
      use stdout = new StringWriter()
      Console.SetOut stdout
      Directory.CreateDirectory(unique) |> ignore
      Directory.SetCurrentDirectory(unique)

      Counter.measureTime <-
        DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)

      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

      do
        use worker =
          new FileStream(reportFile, FileMode.CreateNew)

        stream.CopyTo worker

      let hits = List<string * int * Track>()

      [ 0..9 ]
      |> Seq.iter (fun i ->
        for j = 1 to i + 1 do
          hits.Add("f6e3edb3-fb20-44b3-817d-f69d1a22fc2f", i, new Null())
          ignore j)

      let counts =
        Dictionary<string, Dictionary<int, PointVisit>>()

      hits
      |> Seq.iter (fun (moduleId, hitPointId, hit) ->
        if counts.ContainsKey moduleId |> not then
          counts.Add(moduleId, Dictionary<int, PointVisit>())

        AltCover.Counter.AddVisit(counts, moduleId, hitPointId, hit)
        |> ignore)

      // degenerate case
      Assert.That(junkfile |> File.Exists |> not)

      do
        use junkworker =
          new FileStream(junkfile, FileMode.CreateNew)

        junkworker.Write([||], 0, 0)
        ()

      Runner.J.doReport counts AltCover.ReportFormat.NCover junkfile None // DivertedWriteLeavesExpectedTraces
      |> ignore

      let (c0, w0) = Zip.openUpdate junkfile false

      try
        Assert.That(c0 |> isNull)
        Assert.That(w0, Is.InstanceOf<FileStream>())
        Assert.That(w0.Length, Is.EqualTo 8L)
      finally
        w0.Dispose()

      Runner.J.doReport counts AltCover.ReportFormat.NCover reportFile (Some outputFile) // DivertedWriteLeavesExpectedTraces
      |> ignore

      use worker2 =
        new FileStream(outputFile, FileMode.Open)

      let after = XmlDocument()
      after.Load worker2

      Assert.That(
        after.SelectNodes("//seqpnt")
        |> Seq.cast<XmlElement>
        |> Seq.map _.GetAttribute("visitcount"),
        Is.EquivalentTo
          [ "11"
            "10"
            "9"
            "8"
            "7"
            "6"
            "4"
            "3"
            "2"
            "1" ]
      )
    finally
      maybeDeleteFile outputFile
      maybeDeleteFile reportFile
      maybeDeleteFile junkfile
      Console.SetOut saved
      Directory.SetCurrentDirectory(here)
      maybeIOException (fun () -> Directory.Delete(unique))

  [<Test>]
  let DivertedWriteJsonLeavesExpectedTraces () =
    Runner.init ()
    let saved = Console.Out
    let here = Directory.GetCurrentDirectory()

    let where =
      Assembly.GetExecutingAssembly().Location
      |> Path.GetDirectoryName

    let unique =
      Path.Combine(where, Guid.NewGuid().ToString())

    let reportFile =
      Path.Combine(unique, "WriteJsonLeavesExpectedTraces.json")

    let outputFile =
      Path.Combine(unique, "DivertedWriteJsonLeavesExpectedTraces.json")

    let junkFile =
      (reportFile + "." + (Path.GetFileName unique))

    try
      use stdout = new StringWriter()
      Console.SetOut stdout
      Directory.CreateDirectory(unique) |> ignore
      Directory.SetCurrentDirectory(unique)

      let nativeJson =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find _.EndsWith("Sample4.native.json", StringComparison.Ordinal)

      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(nativeJson)

      do
        use worker =
          new FileStream(reportFile, FileMode.CreateNew)

        stream.CopyTo worker

      let tracks t : Track array =
        [| Null() :> Track
           Call(0) :> Track
           Time(t) :> Track
           Both(Pair.Create(t, 0)) |]

      let t0 = tracks 0L

      let hits = List<string * int * Track>()

      [ 0..9 ]
      |> Seq.iter (fun i ->
        for j = 1 to i + 1 do
          hits.Add("Sample4.dll", i ||| (Counter.branchFlag * (i % 2)), t0.[i % 4])
          ignore j)

      let counts =
        Dictionary<string, Dictionary<int, PointVisit>>()

      hits
      |> Seq.iter (fun (moduleId, hitPointId, hit) ->
        if counts.ContainsKey moduleId |> not then
          counts.Add(moduleId, Dictionary<int, PointVisit>())

        AltCover.Counter.AddVisit(counts, moduleId, hitPointId, hit)
        |> ignore)

      let entries = Dictionary<int, PointVisit>()
      let pv = PointVisit.Create()
      pv.Track(Time 512L)
      tracks (1L) |> Seq.iter pv.Track
      entries.Add(1, pv)
      counts.Add(Track.Entry, entries)

      let exits = Dictionary<int, PointVisit>()
      let pv = PointVisit.Create()
      tracks (2L) |> Seq.iter pv.Track
      pv.Track(Time 1024L)
      exits.Add(1, pv)
      exits.Add(2, pv)
      counts.Add(Track.Exit, exits)

      Runner.J.doReport
        counts
        AltCover.ReportFormat.NativeJson
        reportFile
        (Some outputFile) // DivertedWriteJsonLeavesExpectedTraces
      |> ignore

      let jsonText =
        use worker' =
          new FileStream(outputFile, FileMode.Open)

        use reader = new StreamReader(worker')
        reader.ReadToEnd()
      // saved.WriteLine jsonText  // NOT printfn "%s" jsonText

      let visitedJson =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find
          _.EndsWith("Sample4.syntheticvisits.native.json", StringComparison.Ordinal)

      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(visitedJson)

      use reader = new StreamReader(stream)

      let expected =
        reader
          .ReadToEnd()
          .Replace('\r', '\u00FF')
          .Replace('\n', '\u00FF')
          .Replace("\u00FF\u00FF", "\u00FF")
          .Trim([| '\u00FF' |])

      Assert.That(
        jsonText
          .Replace('\r', '\u00FF')
          .Replace('\n', '\u00FF')
          .Replace("\u00FF\u00FF", "\u00FF")
          .Trim([| '\u00FF' |]),
        Is.EqualTo expected
      )
    finally
      maybeDeleteFile reportFile
      maybeDeleteFile outputFile
      maybeDeleteFile junkFile
      Console.SetOut saved
      Directory.SetCurrentDirectory(here)
      maybeIOException (fun () -> Directory.Delete(unique))

  [<Test>]
  let DivertedZipWriteLeavesExpectedTraces () =
    Runner.init ()
    let saved = Console.Out
    let here = Directory.GetCurrentDirectory()

    let where =
      Assembly.GetExecutingAssembly().Location
      |> Path.GetDirectoryName

    let unique =
      Path.Combine(where, Guid.NewGuid().ToString())

    let reportFile =
      Path.Combine(unique, "FlushLeavesExpectedTraces.xml")

    let reportZip = reportFile + ".zip"

    let outputFile =
      Path.Combine(unique, "DivertedFlushLeavesExpectedTraces.xml")

    let outputZip = outputFile + ".zip"

    let junkfile =
      (reportFile + "." + (Path.GetFileName unique))

    let junkfile1 = junkfile + ".1"

    let junkfile2 = junkfile + ".xml"

    let junkfile3 = junkfile + ".3.xml"

    try
      use stdout = new StringWriter()
      Console.SetOut stdout
      Directory.CreateDirectory(unique) |> ignore
      Directory.SetCurrentDirectory(unique)

      Counter.measureTime <-
        DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)

      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

      let doc = XDocument.Load stream
      Zip.save (fun s -> doc.Save s) reportFile true // fsharplint:disable-line

      let hits = List<string * int * Track>()

      [ 0..9 ]
      |> Seq.iter (fun i ->
        for j = 1 to i + 1 do
          hits.Add("f6e3edb3-fb20-44b3-817d-f69d1a22fc2f", i, new Null())
          ignore j)

      let counts =
        Dictionary<string, Dictionary<int, PointVisit>>()

      hits
      |> Seq.iter (fun (moduleId, hitPointId, hit) ->
        if counts.ContainsKey moduleId |> not then
          counts.Add(moduleId, Dictionary<int, PointVisit>())

        AltCover.Counter.AddVisit(counts, moduleId, hitPointId, hit)
        |> ignore)

      // degenerate case 1
      Assert.That(junkfile |> File.Exists |> not)
      let (c0, w0) = Zip.openUpdate junkfile true

      try
        Assert.That(c0.IsNotNull)
        Assert.That(w0, Is.InstanceOf<MemoryStream>())
        Assert.That(w0.Length, Is.EqualTo 0L)
      finally
        w0.Dispose()
        c0.Dispose()

      // degenerate case 1a
      let junkzip = junkfile1 + ".zip"
      Assert.That(junkzip |> File.Exists |> not)

      do
        use archive =
          ZipFile.Open(junkzip, ZipArchiveMode.Create)

        let entry =
          Guid.NewGuid().ToString() |> archive.CreateEntry

        use sink = entry.Open()
        sink.Write([| 0uy |], 0, 1)
        ()

      let (c0, w0) = Zip.openUpdate junkfile1 true

      try
        Assert.That(c0.IsNotNull)
        Assert.That(w0, Is.InstanceOf<MemoryStream>())
        Assert.That(w0.Length, Is.EqualTo 0L)
      finally
        c0.Dispose()
        w0.Dispose()

      // degenerate case 2
      Assert.That(junkfile2 |> File.Exists |> not)

      Runner.J.doReport
        counts
        (ReportFormat.NCover ||| ReportFormat.Zipped)
        junkfile2
        (Some junkfile3)
      |> ignore

      Assert.That(junkfile2 |> File.Exists |> not)
      let (c1, w1) = Zip.openUpdate junkfile2 true

      try
        Assert.That(c1.IsNotNull)
        Assert.That(w1, Is.InstanceOf<MemoryStream>())
        Assert.That(w1.Length, Is.EqualTo 0L)
      finally
        w1.Dispose()
        c1.Dispose()

      Assert.That(junkfile2 |> File.Exists |> not)

      Runner.J.doReport
        counts
        (ReportFormat.NCover ||| ReportFormat.Zipped)
        reportFile
        (Some outputFile)
      |> ignore

      let (container, worker) =
        Zip.openUpdate outputFile true

      use worker' = worker
      use container' = container
      let after = XmlDocument()
      after.Load worker'

      Assert.That(
        after.SelectNodes("//seqpnt")
        |> Seq.cast<XmlElement>
        |> Seq.map _.GetAttribute("visitcount"),
        Is.EquivalentTo
          [ "11"
            "10"
            "9"
            "8"
            "7"
            "6"
            "4"
            "3"
            "2"
            "1" ]
      )
    finally
      Assert.That(reportFile |> File.Exists |> not, "unexpected reportfile")
      Assert.That(outputFile |> File.Exists |> not, "unexpected outputfile")
      Assert.That(junkfile |> File.Exists |> not, "unexpected junkfile")
      Assert.That(junkfile1 |> File.Exists |> not, "unexpected junkfile1")
      Assert.That(junkfile2 |> File.Exists |> not, "unexpected junkfile2")
      Assert.That(junkfile3 |> File.Exists |> not, "unexpected junkfile3")
      maybeDeleteFile reportZip
      maybeDeleteFile outputZip
      maybeDeleteFile (junkfile + ".zip")
      maybeDeleteFile (junkfile1 + ".zip")
      maybeDeleteFile (junkfile2 + ".zip")
      maybeDeleteFile (junkfile3 + ".zip")
      Console.SetOut saved
      Directory.SetCurrentDirectory(here)
      maybeIOException (fun () -> Directory.Delete(unique))

  [<Test>]
  let DivertedZipWriteJsonLeavesExpectedTraces () =
    Runner.init ()
    let saved = Console.Out
    let here = Directory.GetCurrentDirectory()

    let where =
      Assembly.GetExecutingAssembly().Location
      |> Path.GetDirectoryName

    let unique =
      Path.Combine(where, Guid.NewGuid().ToString())

    let reportFile =
      Path.Combine(unique, "WriteJsonLeavesExpectedTraces.json")

    let reportZip = reportFile + ".zip"

    let outputFile =
      Path.Combine(unique, "DivertedWriteJsonLeavesExpectedTraces.json")

    let outputZip = outputFile + ".zip"

    let junkfile =
      (reportFile + "." + (Path.GetFileName unique))

    let junkfile2 = junkfile + ".json"

    try
      use stdout = new StringWriter()
      Console.SetOut stdout
      Directory.CreateDirectory(unique) |> ignore
      Directory.SetCurrentDirectory(unique)

      let nativeJson =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find _.EndsWith("Sample4.native.json", StringComparison.Ordinal)

      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(nativeJson)

      Zip.save (stream.CopyTo) reportFile true // fsharplint:disable-line

      let tracks t : Track array =
        [| Null() :> Track
           Call(0) :> Track
           Time(t) :> Track
           Both(Pair.Create(t, 0)) |]

      let t0 = tracks 0L

      let hits = List<string * int * Track>()

      [ 0..9 ]
      |> Seq.iter (fun i ->
        for j = 1 to i + 1 do
          hits.Add("Sample4.dll", i ||| (Counter.branchFlag * (i % 2)), t0.[i % 4])
          ignore j)

      let counts =
        Dictionary<string, Dictionary<int, PointVisit>>()

      hits
      |> Seq.iter (fun (moduleId, hitPointId, hit) ->
        if counts.ContainsKey moduleId |> not then
          counts.Add(moduleId, Dictionary<int, PointVisit>())

        AltCover.Counter.AddVisit(counts, moduleId, hitPointId, hit)
        |> ignore)

      let entries = Dictionary<int, PointVisit>()
      let pv = PointVisit.Create()
      pv.Track(Time 512L)
      tracks (1L) |> Seq.iter pv.Track
      entries.Add(1, pv)
      entries.Add(2, pv)
      counts.Add(Track.Entry, entries)

      let exits = Dictionary<int, PointVisit>()
      let pv = PointVisit.Create()
      tracks (2L) |> Seq.iter pv.Track
      pv.Track(Time 1024L)
      exits.Add(1, pv)
      counts.Add(Track.Exit, exits)

      // degenerate case 1
      Assert.That(junkfile |> File.Exists |> not)
      let (c0, w0) = Zip.openUpdate junkfile true

      try
        Assert.That(c0.IsNotNull)
        Assert.That(w0, Is.InstanceOf<MemoryStream>())
        Assert.That(w0.Length, Is.EqualTo 0L)
      finally
        w0.Dispose()
        c0.Dispose()

      Runner.J.doReport
        counts
        (ReportFormat.NativeJson ||| ReportFormat.Zipped)
        reportFile
        (Some outputFile) // DivertedZipWriteJsonLeavesExpectedTraces
      |> ignore

      let (container, worker) =
        Zip.openUpdate outputFile true

      use container' = container

      let jsonText =
        use worker' = worker
        use reader = new StreamReader(worker')
        reader.ReadToEnd()

      let visitedJson =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find
          _.EndsWith("Sample4.syntheticvisits.native.json", StringComparison.Ordinal)

      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(visitedJson)

      use reader = new StreamReader(stream)

      let expected =
        reader
          .ReadToEnd()
          .Replace('\r', '\u00FF')
          .Replace('\n', '\u00FF')
          .Replace("\u00FF\u00FF", "\u00FF")
          .Trim([| '\u00FF' |])

      Assert.That(
        jsonText
          .Replace('\r', '\u00FF')
          .Replace('\n', '\u00FF')
          .Replace("\u00FF\u00FF", "\u00FF")
          .Trim([| '\u00FF' |]),
        Is.EqualTo expected
      )
    finally
      Assert.That(reportFile |> File.Exists |> not, "unexpected report file")
      Assert.That(outputFile |> File.Exists |> not, "unexpected outputfile")
      Assert.That(junkfile |> File.Exists |> not, "unexpected junk file")
      Assert.That(junkfile2 |> File.Exists |> not, "unexpected junk2 file")
      maybeDeleteFile reportZip
      maybeDeleteFile outputZip
      maybeDeleteFile (junkfile + ".zip")
      Console.SetOut saved
      Directory.SetCurrentDirectory(here)
      maybeIOException (fun () -> Directory.Delete(unique))

  [<Test>]
  let NullPayloadShouldReportNothing () =
    Runner.init ()

    let counts =
      Dictionary<string, Dictionary<int, PointVisit>>()

    let where =
      Assembly.GetExecutingAssembly().Location
      |> Path.GetDirectoryName

    let unique =
      Path.Combine(where, Guid.NewGuid().ToString())

    do
      use s = File.Create(unique + ".0.acv")
      s.Close()

    let r =
      Runner.J.getMonitor counts unique List.length []

    Assert.That(r, Is.EqualTo 0)
    Assert.That(File.Exists(unique + ".acv"))
    Assert.That(counts, Is.Empty)

  [<Test>]
  let ActivePayloadShouldReportAsExpected () =
    Runner.init ()

    let counts =
      Dictionary<string, Dictionary<int, PointVisit>>()

    let where =
      Assembly.GetExecutingAssembly().Location
      |> Path.GetDirectoryName

    let unique =
      Path.Combine(where, Guid.NewGuid().ToString())

    let r =
      Runner.J.getMonitor
        counts
        unique
        (fun l ->
          use sink =
            new DeflateStream(
              File.OpenWrite(unique + ".0.acv"),
              CompressionMode.Compress
            )

          use formatter = new BinaryWriter(sink)

          l
          |> List.mapi (fun i x ->
            formatter.Write x
            formatter.Write i
            formatter.Write 0uy
            x)
          |> List.length)
        [ "a"; "b"; String.Empty; "c" ]

    Assert.That(r, Is.EqualTo 4)
    Assert.That(File.Exists(unique + ".acv"))

    let expected =
      Dictionary<string, Dictionary<int, PointVisit>>()

    let a = Dictionary<int, PointVisit>()
    a.Add(0, init 1L [])
    let b = Dictionary<int, PointVisit>()
    b.Add(1, init 1L [])
    let c = Dictionary<int, PointVisit>()
    c.Add(3, init 1L [])
    expected.Add("a", a)
    expected.Add("b", b)
    expected.Add("c", c)

    Assert.That(counts.Count, Is.EqualTo 3)
    Assert.That(counts.["a"].Count, Is.EqualTo 1)
    Assert.That(counts.["b"].Count, Is.EqualTo 1)
    Assert.That(counts.["c"].Count, Is.EqualTo 1)
    Assert.That(counts.["a"].[0].Count, Is.EqualTo 1)
    Assert.That(counts.["a"].[0].Tracks, Is.Empty)
    Assert.That(counts.["b"].[1].Count, Is.EqualTo 1)
    Assert.That(counts.["b"].[1].Tracks, Is.Empty)
    Assert.That(counts.["c"].[3].Count, Is.EqualTo 1)
    Assert.That(counts.["c"].[3].Tracks, Is.Empty)

    maybeDeleteFile (unique + ".acv")

  [<Test>]
  let CollectShouldReportAsExpected () =
    Runner.init ()

    try
      Runner.collect.Value <- true

      let counts =
        Dictionary<string, Dictionary<int, PointVisit>>()

      let where =
        Assembly.GetExecutingAssembly().Location
        |> Path.GetDirectoryName

      let unique =
        Path.Combine(where, Guid.NewGuid().ToString())

      let processing name (l: String list) =
        use sink =
          new DeflateStream(File.OpenWrite(name + ".0.acv"), CompressionMode.Compress)

        use formatter = new BinaryWriter(sink)

        l
        |> List.mapi (fun i x ->
          formatter.Write x
          formatter.Write i
          formatter.Write 0uy
          x)
        |> List.length

      let test =
        Path.Combine(where, Guid.NewGuid().ToString())

      let dryrun =
        processing test [ "a"; "b"; String.Empty; "c" ]

      Assert.That(dryrun, Is.EqualTo 4)
      Assert.That(File.Exists(test + ".0.acv"))

      let r =
        Runner.J.getMonitor
          counts
          unique
          (processing unique)
          [ "a"; "b"; String.Empty; "c" ]

      Assert.That(r, Is.EqualTo 0)
      Assert.That(File.Exists(unique + ".acv") |> not)

      use stream = new MemoryStream()

      let doc =
        DocumentType.LoadReportStream ReportFormat.OpenCover stream

      Assert.That(doc, Is.EqualTo DocumentType.Unknown)
      Assert.That(counts, Is.Empty)
    finally
      Runner.collect.Value <- false

  [<Test>]
  let JunkPayloadShouldReportAsExpected () =
    Runner.init ()

    let counts =
      Dictionary<string, Dictionary<int, PointVisit>>()

    let where =
      Assembly.GetExecutingAssembly().Location
      |> Path.GetDirectoryName

    let unique =
      Path.Combine(where, Guid.NewGuid().ToString())

    let formatter =
      System.Runtime.Serialization.DataContractSerializer(
        typeof<Tuple<String, Int32, DateTime>>
      )

    let r =
      Runner.J.getMonitor
        counts
        unique
        (fun l ->
          use sink =
            new DeflateStream(
              File.OpenWrite(unique + ".0.acv"),
              CompressionMode.Compress
            )

          let settings = XmlWriterSettings()
          settings.ConformanceLevel <- System.Xml.ConformanceLevel.Auto
          use pipe = XmlWriter.Create(sink, settings)

          l
          |> List.mapi (fun i x ->
            formatter.WriteObject(pipe, (x, i, DateTime.UtcNow))
            x)
          |> List.length)
        [ "a"; "b"; String.Empty; "c" ]

    Assert.That(r, Is.EqualTo 4)
    Assert.That(File.Exists(unique + ".acv"))
    Assert.That(counts, Is.Empty)

  [<Test>]
  let TrackingPayloadShouldReportAsExpected () =
    Runner.init ()

    let counts =
      Dictionary<string, Dictionary<int, PointVisit>>()

    let where =
      Assembly.GetExecutingAssembly().Location
      |> Path.GetDirectoryName

    let unique =
      Path.Combine(where, Guid.NewGuid().ToString())

    let payloads0 =
      [ Null() :> Track
        Call(17) :> Track
        Time(23L) :> Track
        Both(Pair.Create(5, 42)) :> Track
        Time(42L) :> Track
        Call(5) :> Track ]

    let pv = init 42L (payloads0 |> List.tail)

    let table =
      Dictionary<string, Dictionary<int, PointVisit>>()

    table.Add("Extra", Dictionary<int, PointVisit>())
    table.["Extra"].Add(3, pv)

    let payloads =
      ((Table table) :> Track) :: payloads0

    let inputs =
      [ String.Empty
        "a"
        "b"
        "c"
        "d"
        String.Empty
        "e" ]

    let r =
      Runner.J.getMonitor
        counts
        unique
        (fun l ->
          use sink =
            new DeflateStream(
              File.OpenWrite(unique + ".0.acv"),
              CompressionMode.Compress
            )

          use formatter = new BinaryWriter(sink)

          l
          |> List.zip payloads
          |> List.mapi (fun i (y, x) ->
            formatter.Write x
            formatter.Write i

            match y with
            | :? Null -> formatter.Write(Tag.Null |> byte)
            | :? Time as t ->
              formatter.Write(Tag.Time |> byte)
              formatter.Write(t.Value)
            | :? Call as t ->
              formatter.Write(Tag.Call |> byte)
              formatter.Write(t.Value)
            | :? Both as b ->
              formatter.Write(Tag.Both |> byte)
              formatter.Write(b.Value.Time)
              formatter.Write(b.Value.Call)
            | :? Table as t ->
              formatter.Write(Tag.Table |> byte)

              t.Value.Keys
              |> Seq.iter (fun m ->
                formatter.Write m
                formatter.Write t.Value.[m].Keys.Count

                t.Value.[m].Keys
                |> Seq.iter (fun p ->
                  formatter.Write p
                  let v = t.Value.[m].[p]
                  formatter.Write v.Count

                  v.Tracks
                  |> Seq.iter (fun tx ->
                    match tx with
                    | :? Time as t ->
                      formatter.Write(Tag.Time |> byte)
                      formatter.Write(t.Value)
                    | :? Call as t ->
                      formatter.Write(Tag.Call |> byte)
                      formatter.Write(t.Value)
                    | :? Both as b ->
                      formatter.Write(Tag.Both |> byte)
                      formatter.Write(b.Value.Time)
                      formatter.Write(b.Value.Call)
                  //| _ -> tx |> (sprintf "%A") |> Assert.Fail
                  )

                  formatter.Write(Tag.Null |> byte)))

              formatter.Write String.Empty

            x)
          |> List.length)
        inputs

    let expected =
      Dictionary<string, Dictionary<int, int64 * Track list>>()

    let a =
      Dictionary<int, int64 * Track list>()

    a.Add(1, (1L, []))

    let b =
      Dictionary<int, int64 * Track list>()

    b.Add(2, (0L, [ Call 17 ]))

    let c =
      Dictionary<int, int64 * Track list>()

    c.Add(3, (0L, [ Time 23L ]))

    let d =
      Dictionary<int, int64 * Track list>()

    d.Add(4, (0L, [ Both(Pair.Create(5, 42)) ]))

    let e =
      Dictionary<int, int64 * Track list>()

    e.Add(6, (0L, [ Call 5 ]))

    let f =
      Dictionary<int, int64 * Track list>()

    f.Add(3, (42L, payloads0 |> List.tail))

    expected.Add("a", a)
    expected.Add("b", b)
    expected.Add("c", c)
    expected.Add("d", d)
    expected.Add("e", e)
    expected.Add("Extra", f)

    Assert.That(r, Is.EqualTo 7)
    Assert.That(File.Exists(unique + ".acv"))

    let result =
      Dictionary<string, Dictionary<int, int64 * Track list>>()

    counts.Keys
    |> Seq.iter (fun k ->
      let inner =
        Dictionary<int, int64 * Track list>()

      result.Add(k, inner)

      counts.[k].Keys
      |> Seq.iter (fun k2 ->
        let v = counts.[k].[k2]
        inner.Add(k2, (v.Count, v.Tracks |> Seq.toList))))

    Assert.That(result, Is.EquivalentTo expected)

  [<Test>]
  let PointProcessShouldCaptureTimes () =
    Runner.init ()
    let x = XmlDocument()
    x.LoadXml("<root />")
    let root = x.DocumentElement

    let hits =
      [ Null() :> Track
        Call(17) :> Track
        Time(23L) :> Track
        Both(Pair.Create(5, 42)) :> Track
        Time(42L) :> Track
        Time(5L) :> Track ]

    Runner.J.pointProcess root hits

    Assert.That(
      x.DocumentElement.OuterXml,
      Is.EqualTo
        """<root><Times><Time time="5" vc="2" /><Time time="23" vc="1" /><Time time="42" vc="1" /></Times><TrackedMethodRefs><TrackedMethodRef uid="17" vc="1" /><TrackedMethodRef uid="42" vc="1" /></TrackedMethodRefs></root>"""
    )

  [<Test>]
  let PostprocessShouldHandleNullCase () =
    let minimal =
      """<?xml version="1.0" encoding="utf-8"?>
<CoverageSession xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
<Summary numSequencePoints="0" visitedSequencePoints="0" numBranchPoints="0" visitedBranchPoints="0" sequenceCoverage="0" branchCoverage="0" maxCyclomaticComplexity="0" minCyclomaticComplexity="0" visitedClasses="0" numClasses="0" visitedMethods="0" numMethods="0" minCrapScore="0" maxCrapScore="0" />
</CoverageSession>"""

    let after = XmlDocument()
    use reader = new StringReader(minimal)
    after.Load(reader)

    let empty =
      Dictionary<string, Dictionary<int, PointVisit>>()

    Runner.J.postProcess empty ReportFormat.OpenCover after

    let summary =
      after.DocumentElement.SelectNodes("//Summary")
      |> Seq.cast<XmlElement>
      |> Seq.toList

    test <@ summary |> Seq.length = 1 @>

    let attr =
      (summary |> Seq.head).GetAttribute("minCrapScore")

    test <@ attr = "0" @>

  [<Test>]
  let PostprocessShouldHandleEntryAndExitTimes () =
    let minimal =
      """<?xml version="1.0" encoding="utf-8" standalone="yes"?>
<CoverageSession xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
  <Summary numSequencePoints="0" visitedSequencePoints="0" numBranchPoints="0" visitedBranchPoints="0" sequenceCoverage="0" branchCoverage="0" maxCyclomaticComplexity="0" minCyclomaticComplexity="0" visitedClasses="0" numClasses="0" visitedMethods="0" numMethods="0" minCrapScore="0" maxCrapScore="0" />
  <Modules>
    <Module hash="3C-49-7E-1B-01-1F-9C-44-DA-C1-13-A1-A8-48-DB-6C-2A-0B-DE-2F">
      <Summary numSequencePoints="0" visitedSequencePoints="0" numBranchPoints="0" visitedBranchPoints="0" sequenceCoverage="0" branchCoverage="0" maxCyclomaticComplexity="0" minCyclomaticComplexity="0" visitedClasses="0" numClasses="0" visitedMethods="0" numMethods="0" minCrapScore="0" maxCrapScore="0" />
      <ModulePath>Sample4.dll</ModulePath>
      <ModuleTime>2020-11-07T13:49:28.5801620Z</ModuleTime>
      <ModuleName>Sample4</ModuleName>
      <Files />
      <Classes />
      <TrackedMethods>
        <TrackedMethod uid="1" token="100663300" name="System.Void Tests.DU::testMakeUnion()" strategy="[Fact]" />
        <TrackedMethod uid="2" token="100663345" name="System.Void Tests.M::testMakeThing()" strategy="[Fact]" />
      </TrackedMethods>
    </Module>
  </Modules>
</CoverageSession>"""

    let after = XmlDocument()
    use reader = new StringReader(minimal)
    after.Load(reader)
    let epoch = DateTime(2020, 11, 7)
    let ticks = epoch.Ticks // = 637403040000000000
    let v1 = PointVisit.Create()
    v1.Track(Time(ticks + 10L))
    v1.Track(Call 23)
    v1.Track(Time(ticks + 20L))
    let v2 = PointVisit.Create()
    v2.Track(Time(ticks + 11L))
    let v3 = PointVisit.Create()
    v3.Track(Time(ticks + 30L))
    let v4 = PointVisit.Create()
    v4.Track(Call 42)

    let entries = Dictionary<int, PointVisit>()
    entries.Add(1, v1)
    entries.Add(2, v3)
    let exits = Dictionary<int, PointVisit>()
    exits.Add(1, v2)
    exits.Add(2, v4)

    let counts =
      Dictionary<string, Dictionary<int, PointVisit>>()

    counts.Add(Track.Entry, entries)
    counts.Add(Track.Exit, exits)
    Runner.J.postProcess counts ReportFormat.OpenCover after

    let processed =
      after.DocumentElement.OuterXml.Replace("\r\n", "\n")

    let expected =
      """<CoverageSession xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
<Summary numSequencePoints="0" visitedSequencePoints="0" numBranchPoints="0" visitedBranchPoints="0" sequenceCoverage="0" branchCoverage="0" maxCyclomaticComplexity="0" minCyclomaticComplexity="0" visitedClasses="0" numClasses="0" visitedMethods="0" numMethods="0" minCrapScore="0" maxCrapScore="0" />
<Modules>
<Module hash="3C-49-7E-1B-01-1F-9C-44-DA-C1-13-A1-A8-48-DB-6C-2A-0B-DE-2F">
<Summary numSequencePoints="0" visitedSequencePoints="0" numBranchPoints="0" visitedBranchPoints="0" sequenceCoverage="0" branchCoverage="0" maxCyclomaticComplexity="0" minCyclomaticComplexity="0" visitedClasses="0" numClasses="0" visitedMethods="0" numMethods="0" minCrapScore="0" maxCrapScore="0" />
<ModulePath>Sample4.dll</ModulePath>
<ModuleTime>2020-11-07T13:49:28.5801620Z</ModuleTime>
<ModuleName>Sample4</ModuleName>
<Files />
<Classes />
<TrackedMethods>
<TrackedMethod uid="1" token="100663300" name="System.Void Tests.DU::testMakeUnion()" strategy="[Fact]" entry="637403040000000010;637403040000000020" exit="637403040000000011" />
<TrackedMethod uid="2" token="100663345" name="System.Void Tests.M::testMakeThing()" strategy="[Fact]" entry="637403040000000030" />
</TrackedMethods>
</Module>
</Modules>
</CoverageSession>"""

    Assert.That(
      processed,
      Is.EqualTo(expected.Replace("\r", String.Empty).Replace("\n", String.Empty))
    )

    test
      <@ processed = expected.Replace("\r", String.Empty).Replace("\n", String.Empty) @>

  [<Test>]
  let PostprocessShouldRestoreKnownOpenCoverState () =
    Runner.init ()

    Counter.measureTime <-
      DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)

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
    |> Seq.iter _.SetAttribute("bev", "0")

    let empty =
      Dictionary<string, Dictionary<int, PointVisit>>()

    Runner.J.postProcess empty ReportFormat.OpenCover after

    Assert.That(
      after.OuterXml.Replace("uspid=\"100663298", "uspid=\"13"),
      Is.EqualTo before,
      after.OuterXml
    )

  [<Test>]
  let PostprocessShouldRestoreKnownOpenCoverStateFromMono () =
    Counter.measureTime <-
      DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)

    let resource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("HandRolledMonoCoverage.xml", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

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
    |> Seq.iter _.SetAttribute("bev", "0")

    after.DocumentElement.SelectNodes("//MethodPoint")
    |> Seq.cast<XmlElement>
    |> Seq.toList
    |> List.iter _.RemoveAllAttributes()

    let visits =
      Dictionary<string, Dictionary<int, PointVisit>>()

    let visit = Dictionary<int, PointVisit>()
    visits.Add("6A-33-AA-93-82-ED-22-9D-F8-68-2C-39-5B-93-9F-74-01-76-00-9F", visit)
    visit.Add(100663297, init 1L []) // should fill in the expected non-zero value
    visit.Add(100663298, init 23L []) // should be ignored
    Runner.J.postProcess visits ReportFormat.OpenCover after

    Assert.That(
      after.OuterXml.Replace("uspid=\"100663298", "uspid=\"13"),
      Is.EqualTo before,
      after.OuterXml
    )

  [<Test>]
  let PostprocessShouldRestoreDegenerateOpenCoverState () =
    Runner.init ()

    Counter.measureTime <-
      DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)

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
    |> Seq.iter (fun el ->
      el.SetAttribute("sequenceCoverage", "0")

      if el.GetAttribute("crapScore") = "2.11" then
        el.SetAttribute("crapScore", "2.15"))

    after.DocumentElement.SelectNodes("//SequencePoint")
    |> Seq.cast<XmlElement>
    |> Seq.toList
    |> List.iter (fun el -> el |> el.ParentNode.RemoveChild |> ignore) // fsharplint:disable-line

    after.DocumentElement.SelectNodes("//MethodPoint")
    |> Seq.cast<XmlElement>
    |> Seq.toList
    |> List.iter (fun el -> el |> el.ParentNode.RemoveChild |> ignore) // fsharplint:disable-line

    let before =
      after.OuterXml.Replace("uspid=\"13", "uspid=\"100663298")

    after.DocumentElement.SelectNodes("//Summary")
    |> Seq.cast<XmlElement>
    |> Seq.iter (fun el ->
      el.SetAttribute("visitedBranchPoints", "0")
      el.SetAttribute("branchCoverage", "0")
      el.SetAttribute("visitedSequencePoints", "0")
      el.SetAttribute("sequenceCoverage", "0")
      el.SetAttribute("visitedClasses", "0")
      el.SetAttribute("visitedMethods", "0")

      if
        el.GetAttribute "minCrapScore"
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

      if
        el.GetAttribute "crapScore"
        |> String.IsNullOrWhiteSpace
        |> not
      then
        el.SetAttribute("crapScore", "0"))

    let empty =
      Dictionary<string, Dictionary<int, PointVisit>>()

    Runner.J.postProcess empty ReportFormat.OpenCover after
    Assert.That(after.OuterXml, Is.EqualTo before, after.OuterXml)

  [<Test>]
  let PostprocessShouldRestoreBranchOnlyOpenCoverState () =
    Runner.init ()

    Counter.measureTime <-
      DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)

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
    |> Seq.iter (fun el ->
      el.SetAttribute("sequenceCoverage", "0")

      if el.GetAttribute("crapScore") = "2.11" then
        el.SetAttribute("crapScore", "2.15"))

    after.DocumentElement.SelectNodes("//SequencePoint")
    |> Seq.cast<XmlElement>
    |> Seq.toList
    |> List.iter (fun el -> el |> el.ParentNode.RemoveChild |> ignore) // fsharplint:disable-line

    after.DocumentElement.SelectNodes("//MethodPoint")
    |> Seq.cast<XmlElement>
    |> Seq.iter _.SetAttribute("vc", "0")

    let before =
      after.OuterXml
        .Replace("uspid=\"13", "uspid=\"100663298")
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

      if
        el.GetAttribute "minCrapScore"
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

      if
        el.GetAttribute "crapScore"
        |> String.IsNullOrWhiteSpace
        |> not
      then
        el.SetAttribute("crapScore", "0"))

    let empty =
      Dictionary<string, Dictionary<int, PointVisit>>()

    Runner.J.postProcess empty ReportFormat.OpenCover after
    Assert.That(after.OuterXml, Is.EqualTo before, after.OuterXml)

  [<Test>]
  let PostprocessShouldRestoreBranchOnlyOpenCoverStateXDoc () =
    Counter.measureTime <-
      DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)

    let resource2 =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("Sample1WithOpenCover.xml", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)

    let after = XDocument.Load(stream)
    let setAttribute (x: XElement) n v = x.Attribute(XName.Get n).Value <- v
    let getAttribute (x: XElement) n = x.Attribute(XName.Get n).Value

    after.Descendants(XName.Get "Summary")
    |> Seq.iter (fun el ->
      setAttribute el "visitedSequencePoints" "0"
      setAttribute el "sequenceCoverage" "0"

      if getAttribute el "minCrapScore" = "2.11" then
        setAttribute el "minCrapScore" "2.15"

      if getAttribute el "maxCrapScore" = "2.11" then
        setAttribute el "maxCrapScore" "2.15")

    after.Descendants(XName.Get "Method")
    |> Seq.iter (fun el ->
      setAttribute el "sequenceCoverage" "0"

      if getAttribute el "crapScore" = "2.11" then
        setAttribute el "crapScore" "2.15")

    after.Descendants(XName.Get "SequencePoint")
    |> Seq.toList
    |> List.iter _.Remove()

    after.Descendants(XName.Get "MethodPoint")
    |> Seq.iter (fun el -> setAttribute el "vc" "0")

    let before =
      after
        .ToString()
        .Replace("uspid=\"13", "uspid=\"100663298")
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

      if
        getAttribute el "minCrapScore"
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

      if
        getAttribute el "crapScore"
        |> String.IsNullOrWhiteSpace
        |> not
      then
        setAttribute el "crapScore" "0")

    let counts =
      Dictionary<string, Dictionary<int, PointVisit>>()

    PostProcess.action "offset" counts ReportFormat.OpenCover (XmlAbstraction.XDoc after)
    //#if NET472
    NUnit.Framework.Assert.That(
      after.ToString(),
      NUnit.Framework.Is.EqualTo(before.ToString())
    )
    //#endif

    test <@ after.ToString() = before.ToString() @>

  [<Test>]
  let JunkTokenShouldDefaultZero () =
    Runner.init ()
    let visits = Dictionary<int, PointVisit>()
    let key = " "

    let result =
      PostProcess.lookUpVisitsByToken key visits

    match (result.Count, result.Tracks |> Seq.toList) with
    | (0L, []) -> ()
  //      | _ -> Assert.Fail(sprintf "%A" result)

  [<Test>]
  let UnknownGeneratesExpectedSummary () =
    let resetInfo () = Output.info <- ignore
    resetInfo ()
    Output.info "info"
    Runner.init ()
    let report = XDocument()
    let builder = System.Text.StringBuilder()
    Runner.summary.Clear() |> ignore

    try
      lock Runner.summaryFormat (fun () ->
        Runner.summaryFormat <- Default

        Runner.threshold <-
          Some
            { Statements = 25uy
              Branches = 0uy
              Methods = 0uy
              Crap = 0uy
              AltMethods = 0uy
              AltCrap = 0uy }

        let task = Collect()
        Output.info <- (fun s -> builder.Append(s).Append("|") |> ignore)

        let r =
          Runner.I.standardSummary DocumentType.Unknown ReportFormat.NCover 0

        test <@ r = (0, 0uy, String.Empty) @>

        Assert.That(builder.ToString(), Is.EqualTo String.Empty)

        let collected =
          task.Summary.Replace("\r", String.Empty).Replace("\n", "|")

        Assert.That(collected, Is.EqualTo String.Empty))
    finally
      resetInfo ()

  [<Test>]
  let EmptyNCoverGeneratesExpectedSummary () =
    let resetInfo () = Output.info <- ignore
    resetInfo ()
    Output.info "info"
    Runner.init ()
    let report = XDocument()
    let builder = System.Text.StringBuilder()
    Runner.summary.Clear() |> ignore

    try
      lock Runner.summaryFormat (fun () ->
        Runner.summaryFormat <- Default
        let task = Collect()
        Output.info <- (fun s -> builder.Append(s).Append("|") |> ignore)

        let r =
          Runner.I.standardSummary (XML report) ReportFormat.NCover 0

        test <@ r = (0, 0uy, String.Empty) @>

        let expected =
          "Visited Classes 0 of 0 (n/a)|Visited Methods 0 of 0 (n/a)|Visited Points 0 of 0 (n/a)|"

        Assert.That(builder.ToString(), Is.EqualTo expected)

        let collected =
          task.Summary.Replace("\r", String.Empty).Replace("\n", "|")

        Assert.That(collected, Is.EqualTo expected))
    finally
      resetInfo ()

  [<Test>]
  let EmptyNCoverGeneratesExpectedTCSummary () =
    let resetInfo () = Output.info <- ignore
    resetInfo ()
    Output.info "info"
    Runner.init ()
    let report = XDocument()
    let builder = System.Text.StringBuilder()
    Runner.summary.Clear() |> ignore

    try
      lock Runner.summaryFormat (fun () ->
        Runner.summaryFormat <- B
        let task = Collect()
        Output.info <- (fun s -> builder.Append(s).Append("|") |> ignore)

        let r =
          Runner.I.standardSummary (XML report) ReportFormat.NCover 0

        test <@ r = (0, 0uy, String.Empty) @>

        let expected =
          "##teamcity[buildStatisticValue key='CodeCoverageAbsCTotal' value='0']|##teamcity[buildStatisticValue key='CodeCoverageAbsCCovered' value='0']|##teamcity[buildStatisticValue key='CodeCoverageAbsMTotal' value='0']|##teamcity[buildStatisticValue key='CodeCoverageAbsMCovered' value='0']|##teamcity[buildStatisticValue key='CodeCoverageAbsSTotal' value='0']|##teamcity[buildStatisticValue key='CodeCoverageAbsSCovered' value='0']|"

        let result = builder.ToString()
        Assert.That(result, Is.EqualTo expected, result)

        let collected =
          task.Summary.Replace("\r", String.Empty).Replace("\n", "|")

        Assert.That(collected, Is.EqualTo expected, collected))
    finally
      resetInfo ()

  [<Test>]
  let EmptyNCoverGeneratesExpectedSummaries () =
    let resetInfo () = Output.info <- ignore
    resetInfo ()
    Output.info "info"
    Runner.init ()
    let report = XDocument()
    let builder = System.Text.StringBuilder()
    Runner.summary.Clear() |> ignore

    try
      lock Runner.summaryFormat (fun () ->
        Runner.summaryFormat <- Many [ B; O; C ]
        let task = Collect()
        Output.info <- (fun s -> builder.Append(s).Append("|") |> ignore)

        let r =
          Runner.I.standardSummary (XML report) ReportFormat.NCover 0

        test <@ r = (0, 0uy, String.Empty) @>

        let expected =
          "Visited Classes 0 of 0 (n/a)|Visited Methods 0 of 0 (n/a)|Visited Points 0 of 0 (n/a)|##teamcity[buildStatisticValue key='CodeCoverageAbsCTotal' value='0']|##teamcity[buildStatisticValue key='CodeCoverageAbsCCovered' value='0']|##teamcity[buildStatisticValue key='CodeCoverageAbsMTotal' value='0']|##teamcity[buildStatisticValue key='CodeCoverageAbsMCovered' value='0']|##teamcity[buildStatisticValue key='CodeCoverageAbsSTotal' value='0']|##teamcity[buildStatisticValue key='CodeCoverageAbsSCovered' value='0']|"

        let result = builder.ToString()
        Assert.That(result, Is.EqualTo expected, result)

        let collected =
          task.Summary.Replace("\r", String.Empty).Replace("\n", "|")

        Assert.That(collected, Is.EqualTo expected, collected))
    finally
      resetInfo ()

  [<Test>]
  let NCoverShouldGeneratePlausibleSummary () =
    let resetInfo () = Output.info <- ignore
    resetInfo ()
    Output.info "info"
    Runner.init ()

    let resource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("SimpleCoverage.xml", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

    let baseline = XDocument.Load(stream)
    let builder = System.Text.StringBuilder()

    try
      lock Runner.summaryFormat (fun () ->
        Runner.summaryFormat <- Many [ R; O; C ]
        Output.info <- (fun s -> builder.Append(s).Append("|") |> ignore)

        let r =
          try
            Runner.threshold <-
              Some
                { Statements = 25uy
                  Branches = 0uy
                  Methods = 0uy
                  Crap = 0uy
                  AltMethods = 0uy
                  AltCrap = 0uy }

            Runner.I.standardSummary (XML baseline) ReportFormat.NCover 42
          finally
            Runner.threshold <- None
        // 80% coverage > threshold so expect return code coming in
        test <@ r = (42, 0uy, String.Empty) @>

        Assert.That(
          builder.ToString(),
          Is.EqualTo(
            "Visited Classes 1 of 1 (100)|Visited Methods 1 of 1 (100)|Visited Points 8 of 10 (80)|"
            + "##teamcity[buildStatisticValue key='CodeCoverageAbsCTotal' value='1']|##teamcity[buildStatisticValue key='CodeCoverageAbsCCovered' value='1']|##teamcity[buildStatisticValue key='CodeCoverageAbsMTotal' value='1']|##teamcity[buildStatisticValue key='CodeCoverageAbsMCovered' value='1']|##teamcity[buildStatisticValue key='CodeCoverageAbsSTotal' value='10']|##teamcity[buildStatisticValue key='CodeCoverageAbsSCovered' value='8']|"
          )
        ))
    finally
      resetInfo ()

  [<Test>]
  let EmptyOpenCoverGeneratesExpectedSummary () =
    let resetInfo () = Output.info <- ignore
    resetInfo ()
    Output.info "info"
    Runner.init ()

    let report =
      XDocument.Load(
        new System.IO.StringReader(
          """<CoverageSession>
  <Summary numSequencePoints="0" visitedSequencePoints="0" numBranchPoints="0" visitedBranchPoints="0" sequenceCoverage="0" branchCoverage="0" maxCyclomaticComplexity="0" minCyclomaticComplexity="1" visitedClasses="0" numClasses="0" visitedMethods="0" numMethods="0" />
</CoverageSession>"""
        )
      )

    let builder = System.Text.StringBuilder()

    try
      lock Runner.summaryFormat (fun () ->
        Runner.summaryFormat <- Default
        Output.info <- (fun s -> builder.Append(s).Append("|") |> ignore)

        let r =
          Runner.I.standardSummary (XML report) ReportFormat.OpenCover 0

        test <@ r = (0, 0uy, String.Empty) @>

        Assert.That(
          builder.ToString(),
          Is.EqualTo(
            "Visited Classes 0 of 0 (n/a)|Visited Methods 0 of 0 (n/a)|"
            + "Visited Points 0 of 0 (0)|Visited Branches 0 of 0 (0)||"
            + "==== Alternative Results (includes all methods including those without corresponding source) ====|"
            + "Alternative Visited Classes 0 of 0 (n/a)|Alternative Visited Methods 0 of 0 (n/a)|"
          )
        ))
    finally
      resetInfo ()

  [<Test>]
  let EmptyOpenCoverGeneratesExpectedTCSummary () =
    let resetInfo () = Output.info <- ignore
    resetInfo ()
    Output.info "info"
    Runner.init ()

    let report =
      XDocument.Load(
        new System.IO.StringReader(
          """<CoverageSession>
  <Summary numSequencePoints="0" visitedSequencePoints="0" numBranchPoints="0" visitedBranchPoints="0" sequenceCoverage="0" branchCoverage="0" maxCyclomaticComplexity="0" minCyclomaticComplexity="1" visitedClasses="0" numClasses="0" visitedMethods="0" numMethods="0" />
</CoverageSession>"""
        )
      )

    let builder = System.Text.StringBuilder()

    try
      lock Runner.summaryFormat (fun () ->
        Runner.summaryFormat <- B
        Output.info <- (fun s -> builder.Append(s).Append("|") |> ignore)

        let r =
          Runner.I.standardSummary (XML report) ReportFormat.OpenCover 0

        test <@ r = (0, 0uy, String.Empty) @>

        Assert.That(
          builder.ToString(),
          Is.EqualTo(
            "##teamcity[buildStatisticValue key='CodeCoverageAbsCTotal' value='0']|"
            + "##teamcity[buildStatisticValue key='CodeCoverageAbsCCovered' value='0']|"
            + "##teamcity[buildStatisticValue key='CodeCoverageAbsMTotal' value='0']|"
            + "##teamcity[buildStatisticValue key='CodeCoverageAbsMCovered' value='0']|"
            + "##teamcity[buildStatisticValue key='CodeCoverageAbsSTotal' value='0']|"
            + "##teamcity[buildStatisticValue key='CodeCoverageAbsSCovered' value='0']|"
            + "##teamcity[buildStatisticValue key='CodeCoverageAbsBTotal' value='0']|"
            + "##teamcity[buildStatisticValue key='CodeCoverageAbsBCovered' value='0']|"
          )
        ))
    finally
      resetInfo ()

  [<Test>]
  let EmptyOpenCoverGeneratesExpectedSummaries () =
    let resetInfo () = Output.info <- ignore
    resetInfo ()
    Output.info "info"
    Runner.init ()

    let report =
      XDocument.Load(
        new System.IO.StringReader(
          """<CoverageSession>
  <Summary numSequencePoints="0" visitedSequencePoints="0" numBranchPoints="0" visitedBranchPoints="0" sequenceCoverage="0" branchCoverage="0" maxCyclomaticComplexity="0" minCyclomaticComplexity="1" visitedClasses="0" numClasses="0" visitedMethods="0" numMethods="0" />
</CoverageSession>"""
        )
      )

    let builder = System.Text.StringBuilder()

    try
      lock Runner.summaryFormat (fun () ->
        Runner.summaryFormat <- Many [ R; O; C ]
        Output.info <- (fun s -> builder.Append(s).Append("|") |> ignore)

        let r =
          Runner.I.standardSummary (XML report) ReportFormat.OpenCover 0

        test <@ r = (0, 0uy, String.Empty) @>

        Assert.That(
          builder.ToString(),
          Is.EqualTo(
            "Visited Classes 0 of 0 (n/a)|Visited Methods 0 of 0 (n/a)|"
            + "Visited Points 0 of 0 (0)|Visited Branches 0 of 0 (0)||"
            + "==== Alternative Results (includes all methods including those without corresponding source) ====|"
            + "Alternative Visited Classes 0 of 0 (n/a)|Alternative Visited Methods 0 of 0 (n/a)|"
            + "##teamcity[buildStatisticValue key='CodeCoverageAbsCTotal' value='0']|"
            + "##teamcity[buildStatisticValue key='CodeCoverageAbsCCovered' value='0']|"
            + "##teamcity[buildStatisticValue key='CodeCoverageAbsMTotal' value='0']|"
            + "##teamcity[buildStatisticValue key='CodeCoverageAbsMCovered' value='0']|"
            + "##teamcity[buildStatisticValue key='CodeCoverageAbsSTotal' value='0']|"
            + "##teamcity[buildStatisticValue key='CodeCoverageAbsSCovered' value='0']|"
            + "##teamcity[buildStatisticValue key='CodeCoverageAbsRTotal' value='0']|"
            + "##teamcity[buildStatisticValue key='CodeCoverageAbsRCovered' value='0']|"
          )
        ))
    finally
      resetInfo ()

  [<Test>]
  let OpenCoverShouldGeneratePlausibleSummary () =
    let resetInfo () = Output.info <- ignore
    resetInfo ()
    Output.info "info"
    Runner.init ()

    let resource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("Sample1WithOpenCover.xml", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

    let baseline = XDocument.Load(stream)

    let thresholds =
      [ Threshold.Default()
        { Threshold.Default() with
            Statements = 75uy }
        { Threshold.Default() with
            Branches = 70uy }
        { Threshold.Default() with
            Methods = 100uy }
        { Threshold.Default() with
            AltMethods = 100uy }
        { Threshold.Default() with Crap = 1uy }
        { Threshold.Default() with
            AltCrap = 1uy }
        { Threshold.Default() with
            Crap = 255uy }
        { Threshold.Default() with
            Statements = 75uy
            Branches = 70uy }
        { Threshold.Default() with
            Statements = 75uy
            AltMethods = 100uy } ]

    let results =
      [ (23, 0, String.Empty)
        (5, 75, "Statements")
        (4, 70, "Branches")
        (23, 0, String.Empty)
        (50, 100, "AltMethods")
        (2, 1, "Crap")
        (2, 1, "AltCrap")
        (23, 0, String.Empty)
        (5, 75, "Statements")
        (50, 100, "AltMethods") ]

    List.zip thresholds results
    |> List.iter (fun (threshold, expected) ->
      try
        lock Runner.summaryFormat (fun () ->
          Runner.summaryFormat <- Many [ B; O; C ]
          let builder = System.Text.StringBuilder()
          Output.info <- (fun s -> builder.Append(s).Append("|") |> ignore)

          let r =
            try
              Runner.threshold <- Some threshold
              Runner.I.standardSummary (XML baseline) ReportFormat.OpenCover 23
            finally
              Runner.threshold <- None
          // 70% coverage < threshold so expect shortfall
          Assert.That(r, Is.EqualTo expected)

          Assert.That(
            builder.ToString(),
            Is.EqualTo(
              "Visited Classes 1 of 1 (100)|Visited Methods 1 of 1 (100)|"
              + "Visited Points 7 of 10 (70)|Visited Branches 2 of 3 (66.67)|Maximum CRAP score 2.11||"
              + "==== Alternative Results (includes all methods including those without corresponding source) ====|"
              + "Alternative Visited Classes 1 of 1 (100)|Alternative Visited Methods 1 of 2 (50)|Alternative maximum CRAP score 2.11|"
              + "##teamcity[buildStatisticValue key='CodeCoverageAbsCTotal' value='1']|##teamcity[buildStatisticValue key='CodeCoverageAbsCCovered' value='1']|##teamcity[buildStatisticValue key='CodeCoverageAbsMTotal' value='1']|##teamcity[buildStatisticValue key='CodeCoverageAbsMCovered' value='1']|##teamcity[buildStatisticValue key='CodeCoverageAbsSTotal' value='10']|##teamcity[buildStatisticValue key='CodeCoverageAbsSCovered' value='7']|"
              + "##teamcity[buildStatisticValue key='CodeCoverageAbsBTotal' value='3']|##teamcity[buildStatisticValue key='CodeCoverageAbsBCovered' value='2']|"
            )
          ))
      finally
        resetInfo ())

  [<Test>]
  let OpenCoverShouldGeneratePlausiblePartialSummary () =
    let resetInfo () = Output.info <- ignore
    resetInfo ()
    Output.info "info"
    Runner.init ()

    let resource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("Sample1WithOpenCover.xml", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

    let baseline = XDocument.Load(stream)

    let summary =
      [ Many [ C ]
        Many [ B; O; C ]
        Many [ B; O ]
        Many [ B; R ]
        Many [ B; R; O ]
        N ]

    summary
    |> List.iter (fun summary ->
      try
        lock Runner.summaryFormat (fun () ->
          Runner.summaryFormat <- summary
          let l = SummaryFormat.ToList summary
          let builder = System.Text.StringBuilder()
          Output.info <- (fun s -> builder.Append(s).Append("|") |> ignore)

          let r =
            try
              Runner.I.standardSummary (XML baseline) ReportFormat.OpenCover 23
            finally
              Runner.threshold <- None

          let elementO =
            if l |> List.contains O then
              "Visited Classes 1 of 1 (100)|Visited Methods 1 of 1 (100)|"
              + "Visited Points 7 of 10 (70)|Visited Branches 2 of 3 (66.67)|"
            else
              String.Empty

          let elementC =
            if l |> List.contains C then
              "Maximum CRAP score 2.11|"
            else
              String.Empty

          let elementOC =
            if l |> List.contains O || l |> List.contains C then
              "|==== Alternative Results (includes all methods including those without corresponding source) ====|"
            else
              String.Empty

          let elementAltO =
            if l |> List.contains O then
              "Alternative Visited Classes 1 of 1 (100)|Alternative Visited Methods 1 of 2 (50)|"
            else
              String.Empty

          let elementAltC =
            if l |> List.contains C then
              "Alternative maximum CRAP score 2.11|"
            else
              String.Empty

          let elementBR =
            if l |> List.contains B || l |> List.contains R then
              "##teamcity[buildStatisticValue key='CodeCoverageAbsCTotal' value='1']|##teamcity[buildStatisticValue key='CodeCoverageAbsCCovered' value='1']|##teamcity[buildStatisticValue key='CodeCoverageAbsMTotal' value='1']|##teamcity[buildStatisticValue key='CodeCoverageAbsMCovered' value='1']|##teamcity[buildStatisticValue key='CodeCoverageAbsSTotal' value='10']|##teamcity[buildStatisticValue key='CodeCoverageAbsSCovered' value='7']|"
            else
              String.Empty

          let elementB =
            if l |> List.contains B then
              "##teamcity[buildStatisticValue key='CodeCoverageAbsBTotal' value='3']|##teamcity[buildStatisticValue key='CodeCoverageAbsBCovered' value='2']|"
            else
              String.Empty

          let elementR =
            if l |> List.contains R then
              "##teamcity[buildStatisticValue key='CodeCoverageAbsRTotal' value='3']|##teamcity[buildStatisticValue key='CodeCoverageAbsRCovered' value='2']|"
            else
              String.Empty

          Assert.That(
            builder.ToString(),
            Is.EqualTo(
              elementO
              + elementC
              + elementOC
              + elementAltO
              + elementAltC
              + elementBR
              + elementB
              + elementR
            ),
            sprintf "%A" summary
          ))
      finally
        resetInfo ())

  [<Test>]
  let EmptyJsonGeneratesExpectedSummary () =
    let resetInfo () = Output.info <- ignore
    resetInfo ()
    Output.info "info"
    Runner.init ()

    let report =
      """{"Sample4.dll": {"Tests.fs": {}}}"""
      |> NativeJson.fromJsonText

    let builder = System.Text.StringBuilder()
    Runner.summary.Clear() |> ignore

    try
      lock Runner.summaryFormat (fun () ->
        Runner.summaryFormat <- Default
        let task = Collect()
        Output.info <- (fun s -> builder.Append(s).Append("|") |> ignore)

        let r =
          Runner.I.standardSummary (JSON report) ReportFormat.NCover 0

        test <@ r = (0, 0uy, String.Empty) @>

        let expected =
          "Visited Classes 0 of 0 (n/a)|Visited Methods 0 of 0 (n/a)|Visited Points 0 of 0 (n/a)|Visited Branches 0 of 0 (n/a)||"
        //printfn "%s" <| builder.ToString()
        Assert.That(builder.ToString(), Is.EqualTo expected)

        let collected =
          task.Summary.Replace("\r", String.Empty).Replace("\n", "|")

        Assert.That(collected, Is.EqualTo expected))
    finally
      resetInfo ()

  [<Test>]
  let EmptyJsonGeneratesExpectedTCSummary () =
    let resetInfo () = Output.info <- ignore
    resetInfo ()
    Output.info "info"
    Runner.init ()

    let report =
      """{"Sample4.dll": {"Tests.fs": {}}}"""
      |> NativeJson.fromJsonText

    let builder = System.Text.StringBuilder()
    Runner.summary.Clear() |> ignore

    try
      lock Runner.summaryFormat (fun () ->
        Runner.summaryFormat <- B
        let task = Collect()
        Output.info <- (fun s -> builder.Append(s).Append("|") |> ignore)

        let r =
          Runner.I.standardSummary (JSON report) ReportFormat.NCover 0

        test <@ r = (0, 0uy, String.Empty) @>

        let expected =
          "##teamcity[buildStatisticValue key='CodeCoverageAbsCTotal' value='0']|##teamcity[buildStatisticValue key='CodeCoverageAbsCCovered' value='0']|##teamcity[buildStatisticValue key='CodeCoverageAbsMTotal' value='0']|##teamcity[buildStatisticValue key='CodeCoverageAbsMCovered' value='0']|##teamcity[buildStatisticValue key='CodeCoverageAbsSTotal' value='0']|##teamcity[buildStatisticValue key='CodeCoverageAbsSCovered' value='0']|##teamcity[buildStatisticValue key='CodeCoverageAbsBTotal' value='0']|##teamcity[buildStatisticValue key='CodeCoverageAbsBCovered' value='0']|"

        let result = builder.ToString()
        //printfn "%s" result
        Assert.That(result, Is.EqualTo expected, result)

        let collected =
          task.Summary.Replace("\r", String.Empty).Replace("\n", "|")

        Assert.That(collected, Is.EqualTo expected, collected))
    finally
      resetInfo ()

  [<Test>]
  let EmptyJsonGeneratesExpectedSummaries () =
    let resetInfo () = Output.info <- ignore
    resetInfo ()
    Output.info "info"
    Runner.init ()

    let report =
      """{"Sample4.dll": {"Tests.fs": {}}}"""
      |> NativeJson.fromJsonText

    let builder = System.Text.StringBuilder()
    Runner.summary.Clear() |> ignore

    try
      lock Runner.summaryFormat (fun () ->
        Runner.summaryFormat <- Many [ B; O; C ]
        let task = Collect()
        Output.info <- (fun s -> builder.Append(s).Append("|") |> ignore)

        let r =
          Runner.I.standardSummary (JSON report) ReportFormat.NCover 0

        test <@ r = (0, 0uy, String.Empty) @>

        let expected =
          "Visited Classes 0 of 0 (n/a)|Visited Methods 0 of 0 (n/a)|Visited Points 0 of 0 (n/a)|Visited Branches 0 of 0 (n/a)||##teamcity[buildStatisticValue key='CodeCoverageAbsCTotal' value='0']|##teamcity[buildStatisticValue key='CodeCoverageAbsCCovered' value='0']|##teamcity[buildStatisticValue key='CodeCoverageAbsMTotal' value='0']|##teamcity[buildStatisticValue key='CodeCoverageAbsMCovered' value='0']|##teamcity[buildStatisticValue key='CodeCoverageAbsSTotal' value='0']|##teamcity[buildStatisticValue key='CodeCoverageAbsSCovered' value='0']|##teamcity[buildStatisticValue key='CodeCoverageAbsBTotal' value='0']|##teamcity[buildStatisticValue key='CodeCoverageAbsBCovered' value='0']|"

        let result = builder.ToString()
        //printfn "%s" result
        Assert.That(result, Is.EqualTo expected, result)

        let collected =
          task.Summary.Replace("\r", String.Empty).Replace("\n", "|")

        Assert.That(collected, Is.EqualTo expected, collected))
    finally
      resetInfo ()

  [<Test>]
  let SimpleJsonShouldGeneratePlausibleSummary () =
    let resetInfo () = Output.info <- ignore
    resetInfo ()
    Output.info "info"
    Runner.init ()

    let resource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("SimpleCoverage.json", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

    use reader = new StreamReader(stream)

    let baseline =
      reader.ReadToEnd() |> NativeJson.fromJsonText

    let builder = System.Text.StringBuilder()

    try
      lock Runner.summaryFormat (fun () ->
        Runner.summaryFormat <- Many [ R; O; C ]
        Output.info <- (fun s -> builder.Append(s).Append("|") |> ignore)

        let r =
          try
            Runner.threshold <-
              Some
                { Statements = 25uy
                  Branches = 0uy
                  Methods = 0uy
                  Crap = 0uy
                  AltMethods = 0uy
                  AltCrap = 0uy }

            Runner.I.standardSummary (JSON baseline) ReportFormat.NativeJson 42
          finally
            Runner.threshold <- None
        //printfn "%s" <| builder.ToString()

        // 60% coverage > threshold so expect return code coming in
        test <@ r = (42, 0uy, String.Empty) @>

        Assert.That(
          builder.ToString(),
          Is.EqualTo(
            "Visited Classes 1 of 1 (100)|Visited Methods 1 of 1 (100)|Visited Points 6 of 10 (60)|Visited Branches 0 of 0 (n/a)|"
            + "|##teamcity[buildStatisticValue key='CodeCoverageAbsCTotal' value='1']|##teamcity[buildStatisticValue key='CodeCoverageAbsCCovered' value='1']|##teamcity[buildStatisticValue key='CodeCoverageAbsMTotal' value='1']|##teamcity[buildStatisticValue key='CodeCoverageAbsMCovered' value='1']|##teamcity[buildStatisticValue key='CodeCoverageAbsSTotal' value='10']|##teamcity[buildStatisticValue key='CodeCoverageAbsSCovered' value='6']|##teamcity[buildStatisticValue key='CodeCoverageAbsRTotal' value='0']|##teamcity[buildStatisticValue key='CodeCoverageAbsRCovered' value='0']|"
          )
        ))
    finally
      resetInfo ()

  [<Test>]
  let ComplexJsonShouldGeneratePlausibleSummary () =
    let resetInfo () = Output.info <- ignore
    resetInfo ()
    Output.info "info"
    Runner.init ()

    let resource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find
        _.EndsWith("Sample4.syntheticvisits.native.json", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

    use reader = new StreamReader(stream)

    let baseline =
      reader.ReadToEnd() |> NativeJson.fromJsonText

    let builder = System.Text.StringBuilder()

    try
      lock Runner.summaryFormat (fun () ->
        Runner.summaryFormat <- Many [ R; O; C ]
        Output.info <- (fun s -> builder.Append(s).Append("|") |> ignore)

        let r =
          try
            Runner.threshold <-
              Some
                { Statements = 20uy
                  Branches = 0uy
                  Methods = 0uy
                  Crap = 0uy
                  AltMethods = 0uy
                  AltCrap = 0uy }

            Runner.I.standardSummary (JSON baseline) ReportFormat.NativeJson 42
          finally
            Runner.threshold <- None
        //printfn "%s" <| builder.ToString()

        // 25% coverage > threshold so expect return code coming in
        test <@ r = (42, 0uy, String.Empty) @>

        Assert.That(
          builder.ToString(),
          Is.EqualTo(
            "Visited Classes 4 of 6 (66.67)|Visited Methods 5 of 10 (50)|Visited Points 5 of 20 (25)|Visited Branches 5 of 10 (50)|"
            + "|##teamcity[buildStatisticValue key='CodeCoverageAbsCTotal' value='6']|##teamcity[buildStatisticValue key='CodeCoverageAbsCCovered' value='4']|##teamcity[buildStatisticValue key='CodeCoverageAbsMTotal' value='10']|##teamcity[buildStatisticValue key='CodeCoverageAbsMCovered' value='5']|##teamcity[buildStatisticValue key='CodeCoverageAbsSTotal' value='20']|##teamcity[buildStatisticValue key='CodeCoverageAbsSCovered' value='5']|##teamcity[buildStatisticValue key='CodeCoverageAbsRTotal' value='10']|##teamcity[buildStatisticValue key='CodeCoverageAbsRCovered' value='5']|"
          )
        ))
    finally
      resetInfo ()

  [<Test>]
  let DegenerateCasesShouldNotGenerateLcov () =
    Runner.init ()

    let unique =
      Path.Combine(
        Assembly.GetExecutingAssembly().Location
        |> Path.GetDirectoryName,
        Guid.NewGuid().ToString() + "/None.lcov"
      )

    LCov.path.Value <- Some unique

    unique
    |> Path.GetDirectoryName
    |> Directory.CreateDirectory
    |> ignore

    try
      Runner.I.addLCovSummary ()

      let summarize =
        Runner.I.summaries |> Seq.head

      let r =
        summarize Unknown ReportFormat.NCover 0

      test <@ r = (0, 0uy, String.Empty) @>
      Assert.That(unique |> File.Exists |> not)
    finally
      LCov.path.Value <- None

  [<Test>]
  let OpenCoverShouldGeneratePlausibleLcov () =
    Runner.init ()

    let resource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("Sample1WithOpenCover.xml", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

    let baseline = XDocument.Load(stream)

    let unique =
      Path.Combine(
        Assembly.GetExecutingAssembly().Location
        |> Path.GetDirectoryName,
        Guid.NewGuid().ToString() + "/OpenCover.lcov"
      )

    LCov.path.Value <- Some unique

    unique
    |> Path.GetDirectoryName
    |> Directory.CreateDirectory
    |> ignore

    try
      Runner.I.addLCovSummary ()

      let summarize =
        Runner.I.summaries |> Seq.head

      let r =
        summarize (XML baseline) ReportFormat.OpenCover 0

      test <@ r = (0, 0uy, String.Empty) @>
      let result = File.ReadAllText unique

      let resource2 =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find _.EndsWith("OpenCover.lcov", StringComparison.Ordinal)

      use stream2 =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)

      use reader = new StreamReader(stream2)
      //printfn "%A" result
      let expected =
        reader.ReadToEnd().Replace("\r", String.Empty).Replace("\\", "/")

      Assert.That(
        result.Replace("\r", String.Empty).Replace("\\", "/"),
        Is.EqualTo expected
      )
    finally
      LCov.path.Value <- None

  [<Test>]
  let OpenCoverWithPartialsShouldGeneratePlausibleLcov () =
    Runner.init ()

    let resource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("OpenCoverWithPartials.xml", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

    let baseline = XDocument.Load(stream)

    let unique =
      Path.Combine(
        Assembly.GetExecutingAssembly().Location
        |> Path.GetDirectoryName,
        Guid.NewGuid().ToString()
        + "/OpenCoverWithPartials.lcov"
      )

    LCov.path.Value <- Some unique

    unique
    |> Path.GetDirectoryName
    |> Directory.CreateDirectory
    |> ignore

    try
      Runner.I.addLCovSummary ()

      let summarize =
        Runner.I.summaries |> Seq.head

      let r =
        summarize (XML baseline) ReportFormat.OpenCover 0

      test <@ r = (0, 0uy, String.Empty) @>
      let result = File.ReadAllText unique

      let resource2 =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find _.EndsWith("OpenCoverWithPartials.lcov", StringComparison.Ordinal)

      use stream2 =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)

      use reader = new StreamReader(stream2)
      //printfn "%A" result
      let expected =
        reader.ReadToEnd().Replace("\r", String.Empty).Replace("\\", "/")

      Assert.That(
        result.Replace("\r", String.Empty).Replace("\\", "/"),
        Is.EqualTo expected
      )
    finally
      LCov.path.Value <- None

  [<Test>]
  let NCoverShouldGeneratePlausibleLcov () =
    Runner.init ()

    let resource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("SimpleCoverage.xml", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

    let baseline = XDocument.Load(stream)

    let unique =
      Path.Combine(
        Assembly.GetExecutingAssembly().Location
        |> Path.GetDirectoryName,
        Guid.NewGuid().ToString() + "/NCover.lcov"
      )

    LCov.path.Value <- Some unique

    unique
    |> Path.GetDirectoryName
    |> Directory.CreateDirectory
    |> ignore

    try
      let r =
        LCov.summary (XML baseline) ReportFormat.NCover 0

      test <@ r = (0, 0uy, String.Empty) @>
      let result = File.ReadAllText unique

      let resource2 =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find _.EndsWith("Tests.NCover.lcov", StringComparison.Ordinal)

      use stream2 =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)

      use reader = new StreamReader(stream2)

      let expected =
        reader.ReadToEnd().Replace("\r", String.Empty).Replace("\\", "/")

      Assert.That(
        result.Replace("\r", String.Empty).Replace("\\", "/"),
        Is.EqualTo expected
      )
    finally
      LCov.path.Value <- None

  [<Test>]
  let NCoverWithPartialsShouldGeneratePlausibleLcov () =
    Runner.init ()

    let resource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("NCoverWithPartials.xml", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

    let baseline = XDocument.Load(stream)

    let unique =
      Path.Combine(
        Assembly.GetExecutingAssembly().Location
        |> Path.GetDirectoryName,
        Guid.NewGuid().ToString() + "/NCover.lcov"
      )

    LCov.path.Value <- Some unique

    unique
    |> Path.GetDirectoryName
    |> Directory.CreateDirectory
    |> ignore

    try
      let r =
        LCov.summary (XML baseline) ReportFormat.NCover 0

      test <@ r = (0, 0uy, String.Empty) @>
      let result = File.ReadAllText unique
      //printfn "%s" result

      let resource2 =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find _.EndsWith("NCoverWithPartials.lcov", StringComparison.Ordinal)

      use stream2 =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)

      use reader = new StreamReader(stream2)

      let expected =
        reader.ReadToEnd().Replace("\r", String.Empty).Replace("\\", "/")

      Assert.That(
        result.Replace("\r", String.Empty).Replace("\\", "/"),
        Is.EqualTo expected
      )
    finally
      LCov.path.Value <- None

  [<Test>]
  let NCoverWithOverloadsShouldGeneratePlausibleLcov () =
    Runner.init ()

    let resource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("Issue222.NCover.xml", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

    let baseline = XDocument.Load(stream)

    let unique =
      Path.Combine(
        Assembly.GetExecutingAssembly().Location
        |> Path.GetDirectoryName,
        Guid.NewGuid().ToString() + "/NCover.lcov"
      )

    LCov.path.Value <- Some unique

    unique
    |> Path.GetDirectoryName
    |> Directory.CreateDirectory
    |> ignore

    try
      let r =
        LCov.summary (XML baseline) ReportFormat.NCover 0

      test <@ r = (0, 0uy, String.Empty) @>
      let result = File.ReadAllText unique
      //printfn "%s" result

      let resource2 =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find _.EndsWith("Issue222.NCover.lcov", StringComparison.Ordinal)

      use stream2 =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)

      use reader = new StreamReader(stream2)

      let expected =
        reader.ReadToEnd().Replace("\r", String.Empty).Replace("\\", "/")

      Assert.That(
        result.Replace("\r", String.Empty).Replace("\\", "/"),
        Is.EqualTo expected
      )
    finally
      LCov.path.Value <- None

  [<Test>]
  let NCoverShouldGenerateMorePlausibleLcov () =
    Runner.init ()

    let resource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("Sample5.ncover.xml", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

    let baseline = XDocument.Load(stream)

    let unique =
      Path.Combine(
        Assembly.GetExecutingAssembly().Location
        |> Path.GetDirectoryName,
        Guid.NewGuid().ToString() + "/Sample5.ncover.lcov"
      )

    LCov.path.Value <- Some unique

    unique
    |> Path.GetDirectoryName
    |> Directory.CreateDirectory
    |> ignore

    try
      let r =
        LCov.summary (XML baseline) ReportFormat.NCover 0

      test <@ r = (0, 0uy, String.Empty) @>
      let result = File.ReadAllText unique
      //printfn "%s" result
      let resource2 =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find _.EndsWith("Sample5.ncover.lcov", StringComparison.Ordinal)

      use stream2 =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)

      use reader = new StreamReader(stream2)

      let expected =
        reader.ReadToEnd().Replace("\r", String.Empty).Replace("\\", "/")

      Assert.That(
        result.Replace("\r", String.Empty).Replace("\\", "/"),
        Is.EqualTo expected
      )
    finally
      LCov.path.Value <- None

  [<Test>]
  let JsonShouldGeneratePlausibleLcov () =
    Runner.init ()

    let resource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("Sample4.coverlet.json", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

    let baseline =
      use r = new StreamReader(stream)
      r.ReadToEnd() |> NativeJson.fromJsonText

    let unique =
      Path.Combine(
        Assembly.GetExecutingAssembly().Location
        |> Path.GetDirectoryName,
        Guid.NewGuid().ToString()
        + "/Sample4.coverlet.lcov"
      )

    LCov.path.Value <- Some unique

    unique
    |> Path.GetDirectoryName
    |> Directory.CreateDirectory
    |> ignore

    try
      let r =
        LCov.summary (JSON baseline) ReportFormat.NativeJson 0

      test <@ r = (0, 0uy, String.Empty) @>
      let result = File.ReadAllText unique

      let resource2 =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find _.EndsWith("Sample4.coverlet.lcov", StringComparison.Ordinal)

      use stream2 =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)

      use reader = new StreamReader(stream2)

      let expected =
        reader.ReadToEnd().Replace("\r", String.Empty).Replace("\\", "/")
      //printfn "%s" result
      Assert.That(
        result.Replace("\r", String.Empty).Replace("\\", "/"),
        Is.EqualTo expected
      )
    finally
      LCov.path.Value <- None

  [<Test>]
  let JsonWithOverloadsShouldGeneratePlausibleLcov () =
    Runner.init ()

    let resource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("Issue222.json", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

    let baseline =
      use r = new StreamReader(stream)
      r.ReadToEnd() |> NativeJson.fromJsonText

    let unique =
      Path.Combine(
        Assembly.GetExecutingAssembly().Location
        |> Path.GetDirectoryName,
        Guid.NewGuid().ToString()
        + "/JsonWithOverloads.lcov"
      )

    LCov.path.Value <- Some unique

    unique
    |> Path.GetDirectoryName
    |> Directory.CreateDirectory
    |> ignore

    try
      let r =
        LCov.summary (JSON baseline) ReportFormat.NativeJson 0

      test <@ r = (0, 0uy, String.Empty) @>
      let result = File.ReadAllText unique
      //printfn "%s" result

      let resource2 =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find _.EndsWith("Issue222.json.lcov", StringComparison.Ordinal)

      use stream2 =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)

      use reader = new StreamReader(stream2)

      let expected =
        reader.ReadToEnd().Replace("\r", String.Empty).Replace("\\", "/")

      Assert.That(
        result.Replace("\r", String.Empty).Replace("\\", "/"),
        Is.EqualTo expected
      )
    finally
      LCov.path.Value <- None

  [<Test>]
  let JsonWithPartialsShouldGeneratePlausibleLcov () =
    Runner.init ()

    let resource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("JsonWithPartials.json", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

    let baseline =
      use r = new StreamReader(stream)
      r.ReadToEnd() |> NativeJson.fromJsonText

    let unique =
      Path.Combine(
        Assembly.GetExecutingAssembly().Location
        |> Path.GetDirectoryName,
        Guid.NewGuid().ToString()
        + "/JsonWithPartials.lcov"
      )

    LCov.path.Value <- Some unique

    unique
    |> Path.GetDirectoryName
    |> Directory.CreateDirectory
    |> ignore

    try
      let r =
        LCov.summary (JSON baseline) ReportFormat.NativeJson 0

      test <@ r = (0, 0uy, String.Empty) @>
      let result = File.ReadAllText unique

      let resource2 =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find _.EndsWith("OpenCoverWithPartials.lcov", StringComparison.Ordinal)

      use stream2 =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)

      use reader = new StreamReader(stream2)

      let expected =
        reader.ReadToEnd().Replace("\r", String.Empty).Replace("\\", "/")
      //printfn "%s" result
      Assert.That(
        result.Replace("\r", String.Empty).Replace("\\", "/"),
        Is.EqualTo expected
      )
    finally
      LCov.path.Value <- None

  [<Test>]
  let NCoverShouldGeneratePlausibleLcovWithMissingFullName () =
    Runner.init ()

    let resource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("Sample1WithNCover.xml", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

    let baseline = XDocument.Load(stream)
    let excluded = XName.Get "excluded"

    baseline.Descendants()
    |> Seq.iter (fun x ->
      if x.Attribute(excluded).IsNotNull then
        x.SetAttributeValue(excluded, "false"))

    let unique =
      Path.Combine(
        Assembly.GetExecutingAssembly().Location
        |> Path.GetDirectoryName,
        Guid.NewGuid().ToString() + "/NCoverBugFix.lcov"
      )

    LCov.path.Value <- Some unique

    unique
    |> Path.GetDirectoryName
    |> Directory.CreateDirectory
    |> ignore

    try
      let r =
        LCov.summary (XML baseline) ReportFormat.NCover 0

      test <@ r = (0, 0uy, String.Empty) @>
      let result = File.ReadAllText unique

      let resource2 =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find _.EndsWith("NCoverBugFix.lcov", StringComparison.Ordinal)

      use stream2 =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)

      use reader = new StreamReader(stream2)

      let expected =
        reader.ReadToEnd().Replace("\r", String.Empty).Replace("\\", "/")

      Assert.That(
        result.Replace("\r", String.Empty).Replace("\\", "/"),
        Is.EqualTo expected
      )
    finally
      LCov.path.Value <- None

  [<Test>]
  let MultiSortDoesItsThing () =
    Runner.init ()

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
         |> List.map (
           sprintf "<x><seqpnt line=\"%d\" /></x>"
           >> load
           >> (_.Descendants(XName.Get "x") >> Seq.head)
           >> (fun m -> (m, m.Descendants(XName.Get "seqpnt")))
         )
         |> List.toSeq))
      |> List.toSeq

    let result =
      LCov.I.multiSortByNameAndPartialStartLine input
      |> Seq.map (fun (f, ms) ->
        (f,
         ms
         |> Seq.map (fun (m, l) ->
           m
             .ToString()
             .Replace("\r", String.Empty)
             .Replace("\n", String.Empty)
             .Replace("  <", "<")
           + "|"
           + String.Join("|", l |> Seq.map string))
         |> Seq.toList))
      |> Seq.toList

    Assert.That(
      result,
      Is.EquivalentTo
        [ ("a",
           [ """<x><seqpnt line="4" /></x>|<seqpnt line="4" />"""
             """<x><seqpnt line="7" /></x>|<seqpnt line="7" />"""
             """<x><seqpnt line="9" /></x>|<seqpnt line="9" />""" ])

          ("m",
           [ """<x><seqpnt line="1" /></x>|<seqpnt line="1" />"""
             """<x><seqpnt line="2" /></x>|<seqpnt line="2" />"""
             """<x><seqpnt line="3" /></x>|<seqpnt line="3" />""" ])
          ("z",
           [ """<x><seqpnt line="3" /></x>|<seqpnt line="3" />"""
             """<x><seqpnt line="5" /></x>|<seqpnt line="5" />""" ]) ]
    )

  // Approved way is ugly -- https://docs.microsoft.com/en-us/visualstudio/code-quality/ca2202?view=vs-2019
  // Also, this rule is deprecated
  let private loadSchema () =
    let schemas = new XmlSchemaSet()

    use stream =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Engine.Tests.coverage-04.xsd")

    use reader = new StreamReader(stream)
    use xreader = XmlReader.Create(reader)
    schemas.Add(String.Empty, xreader) |> ignore
    schemas

  let private validate result =
    let schema = loadSchema ()
    let xmlDocument = XmlDocument()
    xmlDocument.LoadXml(result)
    xmlDocument.Schemas <- schema
    xmlDocument.Validate(null)

  [<Test>]
  let PathsSplitOK () =
    let cases =
      [
#if WINDOWS
        ("C:/Users/anon/OneDrive/Pictures/wallpaper.jpg",
         [ "C:\\"
           "Users"
           "anon"
           "OneDrive"
           "Pictures"
           "wallpaper.jpg" ])
#else
        ("C:/Users/anon/OneDrive/Pictures/wallpaper.jpg",
         [ "C:"
           "Users"
           "anon"
           "OneDrive"
           "Pictures"
           "wallpaper.jpg" ])
#endif
        ("/usr/home/anon/project/src/code.cs",
         [ String([| Path.DirectorySeparatorChar |])
           "usr"
           "home"
           "anon"
           "project"
           "src"
           "code.cs" ])
        ("partial/path/OK", [ "partial"; "path"; "OK" ])
        (String.Empty, [ String.Empty ])
        (null, [ String.Empty ]) ]

    cases
    |> List.iter (fun (case, expect) ->
      test <@ Cobertura.I.splitPath case = expect @>

      if case.IsNotNull then
        test <@ case = (Path.Combine(List.toArray expect)).Replace("\\", "/") @>)

  [<Test>]
  let PathsGroupOK () =
    let cases =
      [ ([ "C:\\"
           "Users"
           "anon"
           "OneDrive"
           "Pictures"
           "wallpaper.jpg" ],
         "C:\\")
        ([ "/"
           "usr"
           "home"
           "anon"
           "project"
           "src"
           "code.cs" ],
         "/usr")
        ([ "\\"
           "usr"
           "home"
           "anon"
           "project"
           "src"
           "code.cs" ],
         "/usr")
        ([ "/" ], "/")
        ([ "\\" ], "/")
        ([ "partial"; "path"; "OK" ], "partial")
        ([], String.Empty)
        ([ String.Empty ], String.Empty) ]

    cases
    |> List.iter (fun (case, expect) -> test <@ Cobertura.I.grouping case = expect @>)

  [<Test>]
  let ExtractSourcesOK () =
    let sep =
      String([| Path.DirectorySeparatorChar |])

    let s = [ "some dummy value" ] |> List.toSeq

    let cases =
      [ ([ "a" ], "a")
        ([ "a/b/"; "a/b/c" ], "a/b" + sep)
        ([ "a/b/x/y"; "a/c/d" ], "a" + sep)
        ([ "c:\\b\\x\\y"; "c:\\b\\c\\d" ], "c:\\b" + sep) ]
      |> List.map (fun (inputs, expect) ->
        (inputs
         |> List.map (fun x -> ((x, s), Cobertura.I.splitPath x))),
        expect)

    Assert.Multiple(fun () ->
      cases
      |> Seq.iter (fun (case, expect) ->
        test <@ case |> Cobertura.I.extractSource |> fst = expect @>))

  [<Test>]
  let DegenerateCasesShouldNotGenerateCobertura () =
    Runner.init ()

    let unique =
      Path.Combine(
        Assembly.GetExecutingAssembly().Location
        |> Path.GetDirectoryName,
        Guid.NewGuid().ToString() + "/None.cobertura"
      )

    Cobertura.path.Value <- Some unique

    unique
    |> Path.GetDirectoryName
    |> Directory.CreateDirectory
    |> ignore

    try
      Runner.I.addCoberturaSummary ()

      let summarize =
        Runner.I.summaries |> Seq.head

      let r =
        summarize Unknown ReportFormat.NCover 0

      test <@ r = (0, 0uy, String.Empty) @>
      Assert.That(unique |> File.Exists |> not)
    finally
      Cobertura.path.Value <- None

  [<Test>]
  let NCoverShouldGeneratePlausibleCobertura () =
    Runner.init ()

    let resource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("NCover122.xml", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

    let baseline = XDocument.Load(stream)

    let unique =
      Path.Combine(
        Assembly.GetExecutingAssembly().Location
        |> Path.GetDirectoryName,
        Guid.NewGuid().ToString() + "/NCover122.cobertura"
      )

    Cobertura.path.Value <- Some unique
    Cobertura.packages.Value <- [ "altcover" ]

    unique
    |> Path.GetDirectoryName
    |> Directory.CreateDirectory
    |> ignore

    try
      Runner.I.addCoberturaSummary ()

      let summarize =
        Runner.I.summaries |> Seq.head

      let r =
        summarize (XML baseline) ReportFormat.NCover 0

      test <@ r = (0, 0uy, String.Empty) @>

      let result =
        Regex
          .Replace(
            File.ReadAllText unique,
            """timestamp=\"\d*\">""",
            """timestamp="xx">"""
          )
          .Replace("\\", "/")
      //printfn "%s" result
      let resource2 =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find _.EndsWith("NCover122.cobertura", StringComparison.Ordinal)

      use stream2 =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)

      use reader = new StreamReader(stream2)

      let expected =
        reader
          .ReadToEnd()
          .Replace("\r", String.Empty)
          .Replace("\\", "/")
          .Replace(
            """version="3.0.0.0""",
            "version=\""
            + AssemblyVersionInformation.AssemblyVersion
          )

      Assert.That(result.Replace("\r", String.Empty), Is.EqualTo expected, result)
      validate result
    finally
      Cobertura.path.Value <- None

  [<Test>]
  let NCoverWithOverloadsShouldGeneratePlausibleCobertura () =
    Runner.init ()

    let resource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("Issue222.NCover.xml", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

    let baseline = XDocument.Load(stream)

    let unique =
      Path.Combine(
        Assembly.GetExecutingAssembly().Location
        |> Path.GetDirectoryName,
        Guid.NewGuid().ToString()
        + "/Issue222.NCover.cobertura"
      )

    Cobertura.path.Value <- Some unique

    unique
    |> Path.GetDirectoryName
    |> Directory.CreateDirectory
    |> ignore

    try
      Runner.I.addCoberturaSummary ()

      let summarize =
        Runner.I.summaries |> Seq.head

      let r =
        summarize (XML baseline) ReportFormat.NCover 0

      test <@ r = (0, 0uy, String.Empty) @>

      let result =
        Regex
          .Replace(
            File.ReadAllText unique,
            """timestamp=\"\d*\">""",
            """timestamp="xx">"""
          )
          .Replace("\\", "/")
      //printfn "%s" result
      let resource2 =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find _.EndsWith("Issue222.NCover.cobertura", StringComparison.Ordinal)

      use stream2 =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)

      use reader = new StreamReader(stream2)

      let expected =
        reader
          .ReadToEnd()
          .Replace("\r", String.Empty)
          .Replace("\\", "/")
          .Replace(
            """version="8.8.0.0""",
            "version=\""
            + AssemblyVersionInformation.AssemblyVersion
          )

      Assert.That(result.Replace("\r", String.Empty), Is.EqualTo expected, result)
      validate result
    finally
      Cobertura.path.Value <- None

  [<Test>]
  let NCoverWithPartialsShouldGeneratePlausibleCobertura () =
    Runner.init ()

    let resource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("NCoverWithPartials.xml", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

    let baseline = XDocument.Load(stream)

    let unique =
      Path.Combine(
        Assembly.GetExecutingAssembly().Location
        |> Path.GetDirectoryName,
        Guid.NewGuid().ToString()
        + "/NCoverWithPartials.cobertura"
      )

    Cobertura.path.Value <- Some unique
    Cobertura.packages.Value <- [ "d:/a01/_work/5/s/src/" ]

    unique
    |> Path.GetDirectoryName
    |> Directory.CreateDirectory
    |> ignore

    try
      Runner.I.addCoberturaSummary ()

      let summarize =
        Runner.I.summaries |> Seq.head

      let r =
        summarize (XML baseline) ReportFormat.NCover 0

      test <@ r = (0, 0uy, String.Empty) @>

      let result =
        Regex
          .Replace(
            File.ReadAllText unique,
            """timestamp=\"\d*\">""",
            """timestamp="xx">"""
          )
          .Replace("\\", "/")
      //printfn "%s" result
      let resource2 =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find _.EndsWith("NCoverWithPartials.cob.xml", StringComparison.Ordinal)

      use stream2 =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)

      use reader = new StreamReader(stream2)

      let expected =
        reader
          .ReadToEnd()
          .Replace("\r", String.Empty)
          .Replace("\\", "/")
          .Replace(
            """version="8.2.0.0""",
            "version=\""
            + AssemblyVersionInformation.AssemblyVersion
          )

      Assert.That(result.Replace("\r", String.Empty), Is.EqualTo expected, result)
      validate result
    finally
      Cobertura.path.Value <- None

  [<Test>]
  let NCoverShouldGenerateMorePlausibleCobertura () =
    Runner.init ()

    let resource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("Sample5.ncover.xml", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

    let baseline = XDocument.Load(stream)

    let unique =
      Path.Combine(
        Assembly.GetExecutingAssembly().Location
        |> Path.GetDirectoryName,
        Guid.NewGuid().ToString()
        + "/Sample5.ncover.cobertura"
      )

    Cobertura.path.Value <- Some unique

    unique
    |> Path.GetDirectoryName
    |> Directory.CreateDirectory
    |> ignore

    try
      Runner.I.addCoberturaSummary ()

      let summarize =
        Runner.I.summaries |> Seq.head

      let r =
        summarize (XML baseline) ReportFormat.NCover 0

      test <@ r = (0, 0uy, String.Empty) @>

      let result =
        Regex
          .Replace(
            File.ReadAllText unique,
            """timestamp=\"\d*\">""",
            """timestamp="xx">"""
          )
          .Replace("\\", "/")
      //printfn "%s" result
      let resource2 =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find _.EndsWith("Sample5.ncover.cobertura", StringComparison.Ordinal)

      use stream2 =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)

      use reader = new StreamReader(stream2)

      let expected =
        reader
          .ReadToEnd()
          .Replace("\r", String.Empty)
          .Replace("\\", "/")
          .Replace(
            """version="3.0.0.0""",
            "version=\""
            + AssemblyVersionInformation.AssemblyVersion
          )

      Assert.That(result.Replace("\r", String.Empty), Is.EqualTo expected, result)
      validate result
    finally
      Cobertura.path.Value <- None

  [<Test>]
  let JsonShouldGeneratePlausibleCobertura () =
    Runner.init ()

    let resource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("Sample4FullTracking.json", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

    let baseline =
      use r = new StreamReader(stream)
      r.ReadToEnd() |> NativeJson.fromJsonText

    let unique =
      Path.Combine(
        Assembly.GetExecutingAssembly().Location
        |> Path.GetDirectoryName,
        Guid.NewGuid().ToString()
        + "/Sample4FullTracking.cobertura"
      )

    Cobertura.path.Value <- Some unique

    unique
    |> Path.GetDirectoryName
    |> Directory.CreateDirectory
    |> ignore

    try
      Runner.I.addCoberturaSummary ()

      let summarize =
        Runner.I.summaries |> Seq.head

      let r =
        summarize (JSON baseline) ReportFormat.NativeJson 0

      test <@ r = (0, 0uy, String.Empty) @>

      let result =
        Regex
          .Replace(
            File.ReadAllText unique,
            """timestamp=\"\d*\">""",
            """timestamp="xx">"""
          )
          .Replace("\\", "/")
      //printfn "%s" result
      let resource2 =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find _.EndsWith("Sample4FullTracking.cobertura", StringComparison.Ordinal)

      use stream2 =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)

      use reader = new StreamReader(stream2)

      let expected =
        reader
          .ReadToEnd()
          .Replace("\r", String.Empty)
          .Replace("\\", "/")
          .Replace(
            """version="3.0.0.0""",
            "version=\""
            + AssemblyVersionInformation.AssemblyVersion
          )

      Assert.That(result.Replace("\r", String.Empty), Is.EqualTo expected, result)
      validate result
    finally
      Cobertura.path.Value <- None

  [<Test>]
  let JsonWithPartialsShouldGeneratePlausibleCobertura () =
    Runner.init ()

    let resource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("JsonWithPartials.json", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

    let baseline =
      use r = new StreamReader(stream)
      r.ReadToEnd() |> NativeJson.fromJsonText

    let unique =
      Path.Combine(
        Assembly.GetExecutingAssembly().Location
        |> Path.GetDirectoryName,
        Guid.NewGuid().ToString()
        + "/JsonWithPartials.cobertura"
      )

    Cobertura.path.Value <- Some unique

    unique
    |> Path.GetDirectoryName
    |> Directory.CreateDirectory
    |> ignore

    try
      Runner.I.addCoberturaSummary ()

      let summarize =
        Runner.I.summaries |> Seq.head

      let r =
        summarize (JSON baseline) ReportFormat.NativeJson 0

      test <@ r = (0, 0uy, String.Empty) @>

      let result =
        Regex
          .Replace(
            File.ReadAllText unique,
            """timestamp=\"\d*\">""",
            """timestamp="xx">"""
          )
          .Replace("\\", "/")
      //printfn "%s" result
      let resource2 =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find _.EndsWith("OpenCoverWithPartials.cob.xml", StringComparison.Ordinal)

      use stream2 =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)

      use reader = new StreamReader(stream2)

      let expected =
        reader
          .ReadToEnd()
          .Replace("\r", String.Empty)
          .Replace("\\", "/")
          .Replace(
            """version="3.0.0.0""",
            "version=\""
            + AssemblyVersionInformation.AssemblyVersion
          )
          .Replace( // different computations TODO!!
            """complexity="2.2""",
            """complexity="1.8"""
          )
          .Replace( // different computations TODO!!
            """complexity="2""",
            """complexity="1"""
          )
          .Replace( // different computations TODO!!
            """.cpp" line-rate="0.78" branch-rate="0""",
            """.cpp" line-rate="0.78" branch-rate="1"""
          )

      Assert.That(result.Replace("\r", String.Empty), Is.EqualTo expected, result)
      validate result
    finally
      Cobertura.path.Value <- None

  [<Test>]
  let JsonFromComplexNestingShouldGeneratePlausibleCobertura () =
    Runner.init ()

    let resource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("Sample5.native.json", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

    let baseline =
      use r = new StreamReader(stream)
      r.ReadToEnd() |> NativeJson.fromJsonText

    let unique =
      Path.Combine(
        Assembly.GetExecutingAssembly().Location
        |> Path.GetDirectoryName,
        Guid.NewGuid().ToString()
        + "/Sample5.native.cobertura"
      )

    Cobertura.path.Value <- Some unique

    unique
    |> Path.GetDirectoryName
    |> Directory.CreateDirectory
    |> ignore

    try
      Runner.I.addCoberturaSummary ()

      let summarize =
        Runner.I.summaries |> Seq.head

      let r =
        summarize (JSON baseline) ReportFormat.NativeJson 0

      test <@ r = (0, 0uy, String.Empty) @>

      let result =
        Regex
          .Replace(
            File.ReadAllText unique,
            """timestamp=\"\d*\">""",
            """timestamp="xx">"""
          )
          .Replace("\\", "/")
      //printfn "%s" result
      let resource2 =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find _.EndsWith("Sample5.native.cobertura", StringComparison.Ordinal)

      use stream2 =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)

      use reader = new StreamReader(stream2)

      let expected =
        reader
          .ReadToEnd()
          .Replace("\r", String.Empty)
          .Replace("\\", "/")
          .Replace(
            """version="3.0.0.0""",
            "version=\""
            + AssemblyVersionInformation.AssemblyVersion
          )

      Assert.That(result.Replace("\r", String.Empty), Is.EqualTo expected, result)
      validate result
    finally
      Cobertura.path.Value <- None

  [<Test>]
  let JsonShouldGeneratePlausibleXml () =
    Runner.init ()

    let resource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("Sample5.native.json", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

    let baseline =
      use r = new StreamReader(stream)
      r.ReadToEnd() |> NativeJson.fromJsonText

    let xml =
      baseline
      |> NativeJson.jsonToXml
      |> NativeJson.orderXml

    let unique =
      Path.Combine(
        Assembly.GetExecutingAssembly().Location
        |> Path.GetDirectoryName,
        Guid.NewGuid().ToString() + "/Sample5.native.xml"
      )

    unique
    |> Path.GetDirectoryName
    |> Directory.CreateDirectory
    |> ignore

    xml.Save(unique)
    let result = File.ReadAllText unique

    let resource2 =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("Sample5.raw-native.xml", StringComparison.Ordinal)

    use stream2 =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)

    use reader = new StreamReader(stream2)
    let expected = reader.ReadToEnd()
    //printfn "%s" result
    Assert.That(
      result
        .Replace('\r', '\u00FF')
        .Replace('\n', '\u00FF')
        .Replace("\u00FF\u00FF", "\u00FF")
        .Trim([| '\u00FF' |]),
      Is.EqualTo(
        expected
          .Replace('\r', '\u00FF')
          .Replace('\n', '\u00FF')
          .Replace("\u00FF\u00FF", "\u00FF")
          .Trim([| '\u00FF' |])
      )
    )

    test
      <@
        result
          .Replace('\r', '\u00FF')
          .Replace('\n', '\u00FF')
          .Replace("\u00FF\u00FF", "\u00FF")
          .Trim([| '\u00FF' |]) = expected
          .Replace('\r', '\u00FF')
          .Replace('\n', '\u00FF')
          .Replace("\u00FF\u00FF", "\u00FF")
          .Trim([| '\u00FF' |])
      @>

  [<Test>]
  let JsonWithPartialsShouldGeneratePlausibleXml () =
    Runner.init ()

    let resource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("JsonWithPartials.json", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

    let baseline =
      use r = new StreamReader(stream)
      r.ReadToEnd() |> NativeJson.fromJsonText

    let xml =
      baseline
      |> NativeJson.jsonToXml
      |> NativeJson.orderXml

    let unique =
      Path.Combine(
        Assembly.GetExecutingAssembly().Location
        |> Path.GetDirectoryName,
        Guid.NewGuid().ToString()
        + "/JsonWithPartialsToRawXml.xml"
      )

    unique
    |> Path.GetDirectoryName
    |> Directory.CreateDirectory
    |> ignore

    xml.Save(unique)
    let result = File.ReadAllText unique

    let resource2 =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("JsonWithPartialsToRawXml.xml", StringComparison.Ordinal)

    use stream2 =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)

    use reader = new StreamReader(stream2)
    let expected = reader.ReadToEnd()
    //printfn "%s" result
    let result1 =
      result
        .Replace('\r', '\u00FF')
        .Replace('\n', '\u00FF')
        .Replace("\u00FF\u00FF", "\u00FF")
        .Trim([| '\u00FF' |])

    let expected1 =
      expected
        .Replace('\r', '\u00FF')
        .Replace('\n', '\u00FF')
        .Replace("\u00FF\u00FF", "\u00FF")
        .Trim([| '\u00FF' |])

    testEqualValue result1 expected1

  [<Test>]
  let NCoverShouldGeneratePlausibleCoberturaWithMissingFullName () =
    Runner.init ()

    let resource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("Sample1WithNCover.xml", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

    let baseline = XDocument.Load(stream)
    let excluded = XName.Get "excluded"

    baseline.Descendants()
    |> Seq.iter (fun x ->
      if x.Attribute(excluded).IsNotNull then
        x.SetAttributeValue(excluded, "false"))

    let unique =
      Path.Combine(
        Assembly.GetExecutingAssembly().Location
        |> Path.GetDirectoryName,
        Guid.NewGuid().ToString() + "/NCover.cobertura"
      )

    Cobertura.path.Value <- Some unique

    unique
    |> Path.GetDirectoryName
    |> Directory.CreateDirectory
    |> ignore

    try
      let r =
        Cobertura.summary (XML baseline) ReportFormat.NCover 0

      test <@ r = (0, 0uy, String.Empty) @>

      let result =
        Regex
          .Replace(
            File.ReadAllText unique,
            """timestamp=\"\d*\">""",
            """timestamp="xx">"""
          )
          .Replace("\\", "/")

      let resource2 =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find _.EndsWith("NCoverBugFix.cobertura", StringComparison.Ordinal)

      use stream2 =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)

      use reader = new StreamReader(stream2)

      let expected =
        reader
          .ReadToEnd()
          .Replace("\r", String.Empty)
          .Replace("\\", "/")
          .Replace(
            """version="3.5.0.0""",
            "version=\""
            + AssemblyVersionInformation.AssemblyVersion
          )

      Assert.That(result.Replace("\r", String.Empty), Is.EqualTo expected, result)
      validate result
    finally
      Cobertura.path.Value <- None

  [<Test>]
  let OpenCoverShouldGeneratePlausibleCobertura () =
    Runner.init ()

    let resource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("issue122.xml", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

    let baseline = XDocument.Load(stream)

    let unique =
      Path.Combine(
        Assembly.GetExecutingAssembly().Location
        |> Path.GetDirectoryName,
        Guid.NewGuid().ToString() + "/issue122.cobertura"
      )

    Cobertura.path.Value <- Some unique

    unique
    |> Path.GetDirectoryName
    |> Directory.CreateDirectory
    |> ignore

    try
      let r =
        Cobertura.summary (XML baseline) ReportFormat.OpenCover 0

      test <@ r = (0, 0uy, String.Empty) @>

      let result =
        Regex.Replace(
          File.ReadAllText unique,
          """timestamp=\"\d*\">""",
          """timestamp="xx">"""
        )
      //printfn "%s" result
      let resource2 =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find _.EndsWith("issue122.cobertura", StringComparison.Ordinal)

      use stream2 =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)

      use reader = new StreamReader(stream2)

      let expected =
        reader
          .ReadToEnd()
          .Replace("\r", String.Empty)
          .Replace("\\", "/")
          .Replace(
            """version="8.2.0.0""",
            "version=\""
            + AssemblyVersionInformation.AssemblyVersion
          )

      Assert.That(
        result.Replace("\r", String.Empty).Replace("\\", "/"),
        Is.EqualTo expected,
        result
      )

      validate result
    finally
      Cobertura.path.Value <- None

  [<Test>]
  let OpenCoverWithOverloadsShouldGeneratePlausibleCobertura () =
    Runner.init ()

    let resource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("Issue222.OpenCover.xml", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

    let baseline = XDocument.Load(stream)

    let unique =
      Path.Combine(
        Assembly.GetExecutingAssembly().Location
        |> Path.GetDirectoryName,
        Guid.NewGuid().ToString()
        + "/OpenCoverWithOverloads.cobertura"
      )

    Cobertura.path.Value <- Some unique

    unique
    |> Path.GetDirectoryName
    |> Directory.CreateDirectory
    |> ignore

    try
      let r =
        Cobertura.summary (XML baseline) ReportFormat.OpenCover 0

      test <@ r = (0, 0uy, String.Empty) @>

      let result =
        Regex.Replace(
          File.ReadAllText unique,
          """timestamp=\"\d*\">""",
          """timestamp="xx">"""
        )
      //printfn "%s" result
      let resource2 =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find _.EndsWith("Issue222.OpenCover.cobertura", StringComparison.Ordinal)

      use stream2 =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)

      use reader = new StreamReader(stream2)

      let expected =
        reader
          .ReadToEnd()
          .Replace("\r", String.Empty)
          .Replace("\\", "/")
          .Replace(
            """version="8.8.0.0""",
            "version=\""
            + AssemblyVersionInformation.AssemblyVersion
          )

      Assert.That(
        result.Replace("\r", String.Empty).Replace("\\", "/"),
        Is.EqualTo expected,
        result
      )

      validate result
    finally
      Cobertura.path.Value <- None

  [<Test>]
  let OpenCoverWithPartialsShouldGeneratePlausibleCobertura () =
    Runner.init ()

    let resource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("OpenCoverWithPartials.xml", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

    let baseline = XDocument.Load(stream)

    let unique =
      Path.Combine(
        Assembly.GetExecutingAssembly().Location
        |> Path.GetDirectoryName,
        Guid.NewGuid().ToString()
        + "/OpenCoverWithPartials.cobertura"
      )

    Cobertura.path.Value <- Some unique

    unique
    |> Path.GetDirectoryName
    |> Directory.CreateDirectory
    |> ignore

    try
      let r =
        Cobertura.summary (XML baseline) ReportFormat.OpenCover 0

      test <@ r = (0, 0uy, String.Empty) @>

      let result =
        Regex.Replace(
          File.ReadAllText unique,
          """timestamp=\"\d*\">""",
          """timestamp="xx">"""
        )
      //printfn "%s" result
      let resource2 =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find _.EndsWith("OpenCoverWithPartials.cob.xml", StringComparison.Ordinal)

      use stream2 =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)

      use reader = new StreamReader(stream2)

      let expected =
        reader
          .ReadToEnd()
          .Replace("\r", String.Empty)
          .Replace("\\", "/")
          .Replace(
            """version="3.0.0.0""",
            "version=\""
            + AssemblyVersionInformation.AssemblyVersion
          )

      Assert.That(
        result.Replace("\r", String.Empty).Replace("\\", "/"),
        Is.EqualTo expected,
        result
      )

      validate result
    finally
      Cobertura.path.Value <- None

  [<Test>]
  let ThresholdViolationShouldBeReported () =
    Runner.init ()
    let saveErr = Output.error
    let saveSummaries = Runner.I.summaries
    let builder = System.Text.StringBuilder()
    let saved = Runner.threshold

    try
      Runner.I.summaries <-
        [ (fun _ _ a -> (a, 0uy, String.Empty))
          (fun _ _ _ -> (23, 42uy, "Statements"))
          (fun _ _ a -> (a, 0uy, String.Empty)) ]

      Output.error <- (fun s -> builder.Append(s).Append("|") |> ignore)

      let delta =
        Runner.J.doSummaries (XML <| XDocument()) ReportFormat.NCover 0

      Assert.That(delta, Is.EqualTo 23)

      Assert.That(
        builder.ToString(),
        Is.EqualTo
          "Statement coverage percentage achieved is 23% below the threshold of 42%.|"
      )
    finally
      Output.error <- saveErr
      Runner.I.summaries <- saveSummaries
      Runner.threshold <- saved

  [<Test>]
  let TryGetValueHandlesNull () =
    Runner.init ()
    let dict: Dictionary<int, int> = null
    Assert.That(PostProcess.tryGetValue dict 0 |> fst, Is.False)

  [<Test>]
  let ShouldDoCoverage () =
    let start = Directory.GetCurrentDirectory()

    let where =
      Path.Combine(dir, Guid.NewGuid().ToString())

    Directory.CreateDirectory(where) |> ignore
    Directory.SetCurrentDirectory where

    let create =
      Path.Combine(where, "AltCover.Recorder.g.dll")

    if create |> File.Exists |> not then
      try
        CoverageParameters.theReportFormat <- Some ReportFormat.NCover

        use from =
          Assembly
            .GetExecutingAssembly()
            .GetManifestResourceStream(
              "AltCover.Engine.Tests.AltCover.Recorder.net20.dll"
            )

        let updated =
          Instrument.I.prepareAssembly from

        Instrument.I.writeAssembly updated create
      finally
        CoverageParameters.theReportFormat <- None

    let save = Runner.J.recorderName
    let save1 = Runner.J.getPayload
    let save2 = Runner.J.getMonitor
    let save3 = Runner.J.doReport

    let codedreport =
      "coverage.xml" |> Path.GetFullPath

    let alternate =
      "not-coverage.xml" |> Path.GetFullPath

    try
      Runner.J.recorderName <- "AltCover.Recorder.g.dll"

      let payload (rest: string list) =
        test <@ rest = [ "test"; "1" ] @>
        255

      test <@ payload [ "test"; "1" ] = 255 @>

      let monitor
        (hits: Dictionary<string, Dictionary<int, PointVisit>>)
        (token: string)
        _
        _
        =
        test' <@ token = codedreport @> "should be default coverage file"
        test <@ hits |> Seq.isEmpty @>
        127

      let write
        (hits: Dictionary<string, Dictionary<int, PointVisit>>)
        format
        (report: string)
        (output: String option)
        =
        test' <@ report = codedreport @> "should be default coverage file"
        test <@ output = Some alternate @>

        use stream =
          Assembly
            .GetExecutingAssembly()
            .GetManifestResourceStream("AltCover.Engine.Tests.GenuineNCover158.Xml")

        use fs = File.Create(alternate)
        stream.CopyTo fs

        test <@ hits |> Seq.isEmpty @>
        TimeSpan.Zero

      Runner.J.getPayload <- payload
      Runner.J.getMonitor <- monitor
      Runner.J.doReport <- write
      let empty = OptionSet()
      let dummy = codedreport + ".xx.acv"

      do
        use temp = File.Create dummy
        test <@ dummy |> File.Exists @>

      let r =
        Runner.doCoverage
          [| "Runner"
             "-x"
             "test"
             "-r"
             where
             "-o"
             alternate
             "--"
             "1" |]
          empty

      test <@ dummy |> File.Exists |> not @>
      test <@ r = 127 @>
    finally
      Runner.J.getPayload <- save1
      Runner.J.getMonitor <- save2
      Runner.J.doReport <- save3
      Runner.J.recorderName <- save
      Directory.SetCurrentDirectory start