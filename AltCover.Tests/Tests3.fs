namespace Tests
// fsharplint:disable  MemberNames NonPublicValuesNames RedundantNewKeyword

open System
open System.IO
open System.Reflection
open System.Xml.Linq

open AltCover
open Microsoft.FSharp.Reflection
open Mono.Options
open Mono.Cecil.Cil

#nowarn "25"

module AltCoverTests3 =
  // Main.fs and CommandLine.fs
  [<Test>]
  let ShouldLaunchWithExpectedOutput () =
    Main.init ()

    let path =
      Path.Combine(SolutionRoot.location, "_Mono/Sample1")

    maybeIgnore (fun () -> path |> Directory.Exists |> not)
    let files = Directory.GetFiles(path)

    let program =
      files
      |> Seq.filter _.EndsWith(".exe", StringComparison.OrdinalIgnoreCase)
      |> Seq.head

    let saved = (Console.Out, Console.Error)
    let e0 = Console.Out.Encoding
    let e1 = Console.Error.Encoding
    CommandLine.toConsole ()

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
    Main.init ()
    let options = Main.I.declareOptions ()
    let optionCount = 37

    let optionNames =
      options
      |> Seq.map (fun o ->
        (o.GetNames() |> Seq.maxBy _.Length)
          .ToLowerInvariant())
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
      |> Seq.map (fun o ->
        (o.GetNames() |> Seq.maxBy _.Length)
          .ToLowerInvariant())
      |> Seq.sort
      |> Seq.toList

    let primitiveNames =
      typeof<Primitive.PrepareOptions>
      |> FSharpType.GetRecordFields
      |> Seq.map _.Name.ToLowerInvariant()
      |> Seq.sort
      |> Seq.toList

    // add "commandline"
    testWithFallback
      <@ (primitiveNames) |> List.length = optionCount + 1 @> // adds optionroot
      (primitiveNames |> List.length)
      (Is.EqualTo(optionCount + 1))

    let typesafeNames =
      typeof<TypeSafe.PrepareOptions>
      |> FSharpType.GetRecordFields
      |> Seq.map _.Name.ToLowerInvariant()
      |> Seq.sort
      |> Seq.toList

    testWithFallback
      <@ (typesafeNames) |> List.length = optionCount + 1 @> // adds optionroot
      (typesafeNames |> List.length)
      (Is.EqualTo(optionCount + 1))

    let fsapiNames =
      typeof<AltCover.PrepareOptions>.GetProperties()
      |> Seq.map _.Name.ToLowerInvariant()
      |> Seq.sort
      |> Seq.toList

    let fsapiCases =
      (typeof<AltCover.PrepareOptions>
       |> FSharpType.GetUnionCases)
        .Length

    let args =
      { Primitive.PrepareOptions.Create() with
          Verbosity = System.Diagnostics.TraceLevel.Warning }
      |> AltCover.PrepareOptions.Primitive

    let commandFragments =
      [ Args.listItems >> (List.map fst)
        Args.plainItems >> (List.map fst)
        Args.options >> List.map (fun (a, _, _) -> a)
        Args.flagItems >> (List.map fst)
        Args.countItems >> (List.map fst) ]
      |> List.collect (fun f -> f args)
      |> List.map _.Trim('-')
      |> List.sort

    testWithFallback
      <@ (commandFragments) |> List.length = optionCount @> // drop -q/--verbose => verbosity
      (commandFragments |> List.length)
      (Is.EqualTo(optionCount))

    // Adds "Tag", "IsPrimitive", "IsTypeSafe"
    testWithFallback
      <@ (fsapiNames) |> List.length = optionCount + fsapiCases + 2 @> // drop -q/--verbose => verbosity
      (fsapiNames |> List.length)
      (Is.EqualTo(optionCount + fsapiCases + 2))

    let taskNames =
      typeof<Prepare>
        .GetProperties(
          BindingFlags.DeclaredOnly
          ||| BindingFlags.Public
          ||| BindingFlags.Instance
        )
      |> Seq.map _.Name.ToLowerInvariant()
      |> Seq.sort
      |> Seq.toList

    testWithFallback
      <@ (taskNames) |> List.length = optionCount @> // drop -q/--verbose => verbosity
      (taskNames |> List.length)
      (Is.EqualTo(optionCount))

    let targets =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceNames()
      |> Seq.find _.EndsWith("AltCover.proj", StringComparison.Ordinal)

    use stream =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream(targets)

    let doc = XDocument.Load stream

    let prepare =
      doc.Descendants()
      |> Seq.filter (fun d -> d.Name.LocalName = "AltCover.Prepare")
      |> Seq.head

    let attributeNames =
      prepare.Attributes()
      |> Seq.map _.Name.LocalName.ToLowerInvariant()
      |> Seq.sort
      |> Seq.toList

    // dotnet test loses commandline, eager, exposereturncode, save
    //                   N/A,         fixed, N/A,              fixed
    // inplace is explicitly hard-coded
    testWithFallback
      <@ (attributeNames) |> List.length = optionCount - 5 @> // drop --portable/-q/--verbose => verbosity
      (attributeNames |> List.length)
      (Is.EqualTo(optionCount - 4))

    Assert.That(
      options
      |> Seq.filter (fun x -> x.Prototype <> "<>")
      |> Seq.forall (_.Description >> String.IsNullOrWhiteSpace >> not),
      "empty description for one or more items"
    )

    Assert.That(
      options
      |> Seq.filter (fun x -> x.Prototype = "<>")
      |> Seq.length,
      Is.EqualTo 1,
      "more than one fallback"
    )

  [<Test>]
  let ParsingJunkIsAnError () =
    Main.init ()
    let options = Main.I.declareOptions ()

    let parse =
      CommandLine.parseCommandLine [| "/@thisIsNotAnOption" |] options

    match parse with
    | Left(x, y) ->
      Assert.That(x, Is.EqualTo "UsageError")
      Assert.That(y, Is.SameAs options)

  [<Test>]
  let ParsingJunkBeforeSeparatorIsAnError () =
    Main.init ()
    let options = Main.I.declareOptions ()

    let parse =
      CommandLine.parseCommandLine
        [| "/@thisIsNotAnOption"
           "--"
           "this should be OK" |]
        options

    match parse with
    | Left(x, y) ->
      Assert.That(x, Is.EqualTo "UsageError")
      Assert.That(y, Is.SameAs options)

  [<Test>]
  let ParsingJunkAfterSeparatorIsExpected () =
    Main.init ()
    let options = Main.I.declareOptions ()

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
    Main.init ()
    let options = Main.I.declareOptions ()
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
    match
      CommandLine.parseCommandLine [| "/t"; "x" |] options
      |> CommandLine.processHelpOption
    with
    | Right(x, y) ->
      Assert.That(y, Is.SameAs options)
      Assert.That(x, Is.Empty)

  [<Test>]
  let ParsingErrorHelpGivesHelp () =
    Main.init ()
    let options = Main.I.declareOptions ()

    let input =
      [| "--o"
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
    match
      CommandLine.parseCommandLine [| "/t"; "x" |] options
      |> CommandLine.processHelpOption
    with
    | Right(x, y) ->
      Assert.That(y, Is.SameAs options)
      Assert.That(x, Is.Empty)

  [<Test>]
  let ParsingAttributesGivesAttributes () =
    Main.init ()

    try
      CoverageParameters.nameFilters.Clear()
      let options = Main.I.declareOptions ()

      let input =
        [| "-a"
           "1;a"
           "--a"
           "2"
           "/a"
           "3"
           "-a=4"
           "--a=5"
           "/a=6" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      Assert.That(CoverageParameters.nameFilters.Count, Is.EqualTo 7)

      Assert.That(
        CoverageParameters.nameFilters
        |> Seq.forall (fun x ->
          match x.Scope with
          | FilterScope.Attribute -> true)
      )

      Assert.That(
        CoverageParameters.nameFilters
        |> Seq.map (fun x ->
          match x.Scope with
          | FilterScope.Attribute -> x.Regex.ToString()),
        Is.EquivalentTo([| "1"; "a"; "2"; "3"; "4"; "5"; "6" |])
      )

      Assert.That(
        CoverageParameters.nameFilters
        |> Seq.forall (fun x -> x.Sense = Exclude)
      )
    finally
      CoverageParameters.nameFilters.Clear()

  [<Test>]
  let ParsingTopLevelGivesTopLevel () =
    [ "attributetoplevel", FilterScope.Attribute
      "methodtoplevel", FilterScope.Method
      "typetoplevel", FilterScope.Type ]
    |> List.iter (fun (key, value) ->
      Main.init ()

      try
        CoverageParameters.topLevel.Clear()
        let options = Main.I.declareOptions ()

        let input =
          [| "--" + key
             "1;a"
             "/" + key
             "2"
             "--" + key
             "3"
             "--" + key + "=4"
             "--" + key + "=5"
             "/" + key + "=6" |]

        let parse =
          CommandLine.parseCommandLine input options

        match parse with
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)

        Assert.That(CoverageParameters.topLevel.Count, Is.EqualTo 7)

        Assert.That(
          CoverageParameters.topLevel
          |> Seq.map (fun x ->
            match x.Scope with
            | scope when scope = value -> x.Regex.ToString()),
          Is.EquivalentTo([| "1"; "a"; "2"; "3"; "4"; "5"; "6" |])
        )

        Assert.That(
          CoverageParameters.topLevel
          |> Seq.forall (fun x -> x.Sense = Exclude)
        )
      finally
        CoverageParameters.topLevel.Clear())

  [<Test>]
  let ParsingMethodsGivesMethods () =
    Main.init ()

    try
      CoverageParameters.nameFilters.Clear()
      let options = Main.I.declareOptions ()

      let input =
        [| "-m"
           "1"
           "--m"
           "2;b;c"
           "/m"
           "3"
           "-m=4"
           "--m=5"
           "/m=6" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      Assert.That(CoverageParameters.nameFilters.Count, Is.EqualTo 8)

      Assert.That(
        CoverageParameters.nameFilters
        |> Seq.forall (fun x ->
          match x.Scope with
          | FilterScope.Method -> true)
      )

      Assert.That(
        CoverageParameters.nameFilters
        |> Seq.map (fun x ->
          match x.Scope with
          | FilterScope.Method -> x.Regex.ToString()),
        Is.EquivalentTo(
          [| "1"
             "2"
             "b"
             "c"
             "3"
             "4"
             "5"
             "6" |]
        )
      )

      Assert.That(
        CoverageParameters.nameFilters
        |> Seq.forall (fun x -> x.Sense = Exclude)
      )
    finally
      CoverageParameters.nameFilters.Clear()

  [<Test>]
  let ParsingTypesGivesTypes () =
    Main.init ()

    try
      CoverageParameters.nameFilters.Clear()
      let options = Main.I.declareOptions ()

      let input =
        [| "-t"
           "1"
           "--t"
           "2"
           "/t"
           "3;x;y;z"
           "-t=4"
           "--t=5"
           "/t=6" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      Assert.That(CoverageParameters.nameFilters.Count, Is.EqualTo 9)

      Assert.That(
        CoverageParameters.nameFilters
        |> Seq.forall (fun x ->
          match x.Scope with
          | FilterScope.Type -> true)
      )

      Assert.That(
        CoverageParameters.nameFilters
        |> Seq.map (fun x ->
          match x.Scope with
          | FilterScope.Type -> x.Regex.ToString()),
        Is.EquivalentTo(
          [| "1"
             "2"
             "3"
             "x"
             "y"
             "z"
             "4"
             "5"
             "6" |]
        )
      )

      Assert.That(
        CoverageParameters.nameFilters
        |> Seq.forall (fun x -> x.Sense = Exclude)
      )
    finally
      CoverageParameters.nameFilters.Clear()

  [<Test>]
  let ParsingAssembliesGivesAssemblies () =
    Main.init ()

    try
      CoverageParameters.nameFilters.Clear()
      let options = Main.I.declareOptions ()

      let input =
        [| "-s"
           "?1"
           "--s"
           "2"
           "/s"
           "3"
           "-s=4;p;q"
           "--s=5"
           "/s=6" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      Assert.That(CoverageParameters.nameFilters.Count, Is.EqualTo 8)

      Assert.That(
        CoverageParameters.nameFilters
        |> Seq.forall (fun x ->
          match x.Scope with
          | FilterScope.Assembly -> true)
      )

      Assert.That(
        CoverageParameters.nameFilters
        |> Seq.map (fun x ->
          match x.Scope with
          | FilterScope.Assembly -> x.Regex.ToString()),
        Is.EquivalentTo(
          [| "1"
             "2"
             "3"
             "4"
             "p"
             "q"
             "5"
             "6" |]
        )
      )

      Assert.That(
        CoverageParameters.nameFilters
        |> Seq.map (fun x -> if x.Sense = Include then 1 else 0),
        Is.EquivalentTo([| 1; 0; 0; 0; 0; 0; 0; 0 |])
      )
    finally
      CoverageParameters.nameFilters.Clear()

  [<Test>]
  let ParsingEscapeCasesWork () =
    Main.init ()

    try
      CoverageParameters.nameFilters.Clear()
      let options = Main.I.declareOptions ()

      let input =
        [| "-s"
           "1\u0001a"
           "--s"
           "\u0000d"
           "/s"
           "3"
           "-s=4;;p;q"
           "--s=5"
           "/s=6" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      Assert.That(CoverageParameters.nameFilters.Count, Is.EqualTo 7)

      Assert.That(
        CoverageParameters.nameFilters
        |> Seq.forall (fun x ->
          match x.Scope with
          | FilterScope.Assembly -> true)
      )

      Assert.That(
        CoverageParameters.nameFilters
        |> Seq.map (fun x ->
          match x.Scope with
          | FilterScope.Assembly -> x.Regex.ToString()),
        Is.EquivalentTo(
          [| "1|a"
             "\\d"
             "3"
             "4;p"
             "q"
             "5"
             "6" |]
        )
      )

      Assert.That(
        CoverageParameters.nameFilters
        |> Seq.forall (fun x -> x.Sense = Exclude)
      )
    finally
      CoverageParameters.nameFilters.Clear()

  [<Test>]
  let ParsingModulesGivesModules () =
    Main.init ()

    try
      CoverageParameters.nameFilters.Clear()
      let options = Main.I.declareOptions ()

      let input =
        [| "-e"
           "1"
           "--e"
           "2"
           "/e"
           "3"
           "-e=4;p;q"
           "--e=5"
           "/e=6" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      Assert.That(CoverageParameters.nameFilters.Count, Is.EqualTo 8)

      Assert.That(
        CoverageParameters.nameFilters
        |> Seq.forall (fun x ->
          match x.Scope with
          | FilterScope.Module -> true)
      )

      Assert.That(
        CoverageParameters.nameFilters
        |> Seq.map (fun x ->
          match x.Scope with
          | FilterScope.Module -> x.Regex.ToString()),
        Is.EquivalentTo(
          [| "1"
             "2"
             "3"
             "4"
             "p"
             "q"
             "5"
             "6" |]
        )
      )

      Assert.That(
        CoverageParameters.nameFilters
        |> Seq.forall (fun x -> x.Sense = Exclude)
      )
    finally
      CoverageParameters.nameFilters.Clear()

  [<Test>]
  let ParsingFilesGivesFiles () =
    Main.init ()

    try
      CoverageParameters.nameFilters.Clear()
      let options = Main.I.declareOptions ()

      let input =
        [| "-f"
           "1"
           "--f"
           "2"
           "/f"
           "3"
           "-f=4"
           "--f=5;m;n"
           "/f=6" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      Assert.That(CoverageParameters.nameFilters.Count, Is.EqualTo 8)

      Assert.That(
        CoverageParameters.nameFilters
        |> Seq.forall (fun x ->
          match x.Scope with
          | FilterScope.File -> true)
      )

      Assert.That(
        CoverageParameters.nameFilters
        |> Seq.map (fun x ->
          match x.Scope with
          | FilterScope.File -> x.Regex.ToString()),
        Is.EquivalentTo(
          [| "1"
             "2"
             "3"
             "4"
             "5"
             "m"
             "n"
             "6" |]
        )
      )

      Assert.That(
        CoverageParameters.nameFilters
        |> Seq.forall (fun x -> x.Sense = Exclude)
      )
    finally
      CoverageParameters.nameFilters.Clear()

  [<Test>]
  let ParsingPathsGivesPaths () =
    Main.init ()

    try
      CoverageParameters.nameFilters.Clear()
      let options = Main.I.declareOptions ()

      let input =
        [| "-p"
           "1"
           "--p"
           "2"
           "/p"
           "3"
           "-p=4"
           "--p=5;m;n"
           "/p=6" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      Assert.That(CoverageParameters.nameFilters.Count, Is.EqualTo 8)

      Assert.That(
        CoverageParameters.nameFilters
        |> Seq.forall (fun x ->
          match x.Scope with
          | FilterScope.Path -> true)
      )

      Assert.That(
        CoverageParameters.nameFilters
        |> Seq.map (fun x ->
          match x.Scope with
          | FilterScope.Path -> x.Regex.ToString()),
        Is.EquivalentTo(
          [| "1"
             "2"
             "3"
             "4"
             "5"
             "m"
             "n"
             "6" |]
        )
      )

      Assert.That(
        CoverageParameters.nameFilters
        |> Seq.forall (fun x -> x.Sense = Exclude)
      )
    finally
      CoverageParameters.nameFilters.Clear()

  [<Test>]
  let ParsingReportGivesReport () =
    Main.init ()

    try
      CoverageParameters.theReportPath <- None
      let options = Main.I.declareOptions ()
      let unique = Guid.NewGuid().ToString()

      let where =
        Assembly.GetExecutingAssembly().Location

      let path =
        Path.Combine(Path.GetDirectoryName(where), unique)

      let input = [| "-r"; path |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      match CoverageParameters.theReportPath with
      | Some x -> Assert.That(Path.GetFileName x, Is.EqualTo unique)
    finally
      CoverageParameters.theReportPath <- None

  [<Test>]
  let ParsingMultipleReportGivesFailure () =
    Main.init ()

    try
      CoverageParameters.theReportPath <- None
      let options = Main.I.declareOptions ()
      let unique = Guid.NewGuid().ToString()

      let input =
        [| "-r"
           unique
           "/r"
           unique.Replace("-", "+") |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")

        Assert.That(
          CommandLine.error |> Seq.head,
          Is.EqualTo "--report : specify this only once"
        )
    finally
      CoverageParameters.theReportPath <- None

  [<Test>]
  let ParsingBadReportGivesFailure () =
    Main.init ()

    try
      CoverageParameters.theReportPath <- None
      let options = Main.I.declareOptions ()
      let unique = Guid.NewGuid().ToString()

      let input =
        [| "-r"
           unique.Replace("-", Path.GetInvalidPathChars() |> String) |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      CoverageParameters.theReportPath <- None

  [<Test>]
  let ParsingNoReportGivesFailure () =
    Main.init ()

    try
      CoverageParameters.theReportPath <- None
      let options = Main.I.declareOptions ()
      let unique = Guid.NewGuid().ToString()
      let input = [| "-r" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      CoverageParameters.theReportPath <- None

  [<Test>]
  let ParsingEmptyReportGivesFailure () =
    Main.init ()

    try
      CoverageParameters.theReportPath <- None
      let options = Main.I.declareOptions ()
      let unique = Guid.NewGuid().ToString()
      let input = [| "-r"; " " |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      CoverageParameters.theReportPath <- None

  [<Test>]
  let ParsingInputGivesInput () =
    Main.init ()

    try
      CoverageParameters.theInputDirectories.Clear()
      let options = Main.I.declareOptions ()
      let unique = "."
      let input = [| "-i"; unique |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      match
        CoverageParameters.theInputDirectories
        |> Seq.toList
      with
      | [ x ] -> Assert.That(x, Is.EqualTo(canonicalDirectory unique))
    finally
      CoverageParameters.theInputDirectories.Clear()

  [<Test>]
  let ParsingMultipleInputIsOKToo () =
    Main.init ()

    try
      CoverageParameters.theInputDirectories.Clear()
      CoverageParameters.theOutputDirectories.Clear()
      CoverageParameters.inplace.Value <- false

      let options = Main.I.declareOptions ()

      let input = [| "-i"; "."; "/i"; ".." |]

      let parse =
        CommandLine.parseCommandLine input options

      let pcom a b =
        Path.Combine(b, a) |> canonicalDirectory

      let AssertAreEqual (a: obj, b: obj) = Assert.That(a, Is.EqualTo b)

      match parse with
      | Right _ ->
        CoverageParameters.inputDirectories ()
        |> Seq.toList
        |> List.zip ([ "."; ".." ] |> List.map canonicalDirectory)
        |> List.iter AssertAreEqual

        CoverageParameters.outputDirectories ()
        |> Seq.toList
        |> List.zip ([ "."; ".." ] |> List.map (pcom "__Instrumented"))
        |> List.iter AssertAreEqual

        CoverageParameters.inplace.Value <- true
        CoverageParameters.theOutputDirectories.Add "maybe"

        CoverageParameters.outputDirectories ()
        |> Seq.toList
        |> List.zip
          [ canonicalDirectory "maybe"
            ".." |> (pcom "__Saved") ]
        |> List.iter AssertAreEqual

    finally
      CoverageParameters.theOutputDirectories.Clear()
      CoverageParameters.theInputDirectories.Clear()
      CoverageParameters.inplace.Value <- false

  [<Test>]
  let ParsingDuplicateInputGivesFailure () =
    Main.init ()

    try
      CoverageParameters.theInputDirectories.Clear()
      let options = Main.I.declareOptions ()
      let here = "."
      let input = [| "-i"; here; "-i"; here |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")

        Assert.That(
          CommandLine.error,
          Is.EquivalentTo(
            [ (canonicalDirectory here)
              + " was already specified for --inputDirectory" ]
          )
        )
    finally
      CoverageParameters.theInputDirectories.Clear()

  [<Test>]
  let ParsingBadInputGivesFailure () =
    Main.init ()

    try
      CoverageParameters.theInputDirectories.Clear()
      let options = Main.I.declareOptions ()

      let unique =
        Guid.NewGuid().ToString().Replace("-", "*")

      let input = [| "-i"; unique |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      CoverageParameters.theInputDirectories.Clear()

  [<Test>]
  let ParsingNoInputGivesFailure () =
    Main.init ()

    try
      CoverageParameters.theInputDirectories.Clear()
      let options = Main.I.declareOptions ()
      let input = [| "-i" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      CoverageParameters.theInputDirectories.Clear()

  [<Test>]
  let ParsingOutputGivesOutput () =
    Main.init ()

    try
      CoverageParameters.theOutputDirectories.Clear()
      let options = Main.I.declareOptions ()
      let unique = Guid.NewGuid().ToString()
      let input = [| "-o"; unique |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      match
        CoverageParameters.outputDirectories ()
        |> Seq.toList
      with
      | [ x ] -> Assert.That(canonicalDirectory x, Is.EqualTo(canonicalDirectory unique))
    finally
      CoverageParameters.theOutputDirectories.Clear()

  [<Test>]
  let ParsingDuplicateOutputGivesFailure () =
    Main.init ()

    try
      CoverageParameters.theOutputDirectories.Clear()
      let options = Main.I.declareOptions ()
      let unique = Guid.NewGuid().ToString()
      let input = [| "-o"; unique; "-o"; unique |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")

        Assert.That(
          CommandLine.error,
          Is.EquivalentTo(
            [ (canonicalDirectory unique)
              + " was already specified for --outputDirectory" ]
          )
        )
    finally
      CoverageParameters.theOutputDirectories.Clear()

  [<Test>]
  let ParsingMultipleOutputIsOK () =
    Main.init ()

    try
      CoverageParameters.theInputDirectories.Clear()
      CoverageParameters.theOutputDirectories.Clear()

      let options = Main.I.declareOptions ()
      let unique = Guid.NewGuid().ToString()
      let u2 = unique.Replace("-", "+")

      let outs =
        [ unique; u2 ] |> List.map canonicalDirectory

      let input = [| "-o"; unique; "/o"; u2 |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right _ ->
        Assert.That(CoverageParameters.theOutputDirectories, Is.EquivalentTo outs)

        Assert.That(
          CoverageParameters.outputDirectories (),
          Is.EquivalentTo(outs |> Seq.take 1)
        )
    finally
      CoverageParameters.theOutputDirectories.Clear()

  [<Test>]
  let ParsingBadOutputGivesFailure () =
    Main.init ()

    try
      CoverageParameters.theOutputDirectories.Clear()
      let options = Main.I.declareOptions ()
      let unique = Guid.NewGuid().ToString()

      let input =
        [| "-o"
           unique.Replace("-", Path.GetInvalidPathChars() |> String) |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      CoverageParameters.theOutputDirectories.Clear()

  [<Test>]
  let ParsingNoOutputGivesFailure () =
    Main.init ()

    try
      CoverageParameters.theOutputDirectories.Clear()
      let options = Main.I.declareOptions ()
      let input = [| "-o" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      CoverageParameters.theOutputDirectories.Clear()

  [<Test>]
  let ParsingEmptyOutputGivesFailure () =
    Main.init ()

    try
      CoverageParameters.theOutputDirectories.Clear()
      let options = Main.I.declareOptions ()
      let input = [| "-o"; " " |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      CoverageParameters.theOutputDirectories.Clear()

  [<Test>]
  let ParsingSymbolGivesSymbol () =
    Main.init ()

    try
      ProgramDatabase.symbolFolders.Clear()
      let options = Main.I.declareOptions ()
      let unique = Path.GetFullPath(".")
      let symbol = [| "-y"; unique |]

      let parse =
        CommandLine.parseCommandLine symbol options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      match ProgramDatabase.symbolFolders.Count with
      | 1 -> Assert.That(ProgramDatabase.symbolFolders, Is.EquivalentTo [ unique ])
    finally
      ProgramDatabase.symbolFolders.Clear()

  [<Test>]
  let ParsingMultipleSymbolGivesOK () =
    Main.init ()

    try
      ProgramDatabase.symbolFolders.Clear()
      let options = Main.I.declareOptions ()

      let symbol =
        [| "-y"
           Path.GetFullPath(".")
           "/y"
           Path.GetFullPath("..") |]

      let parse =
        CommandLine.parseCommandLine symbol options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      match ProgramDatabase.symbolFolders.Count with
      | 2 ->
        Assert.That(
          ProgramDatabase.symbolFolders,
          Is.EquivalentTo(symbol |> Seq.filter (fun x -> x.Length > 2))
        )
    finally
      ProgramDatabase.symbolFolders.Clear()

  [<Test>]
  let ParsingBadSymbolGivesFailure () =
    Main.init ()

    try
      ProgramDatabase.symbolFolders.Clear()
      let options = Main.I.declareOptions ()

      let unique =
        Guid.NewGuid().ToString().Replace("-", "*")

      let symbol = [| "-y"; unique |]

      let parse =
        CommandLine.parseCommandLine symbol options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      ProgramDatabase.symbolFolders.Clear()

  [<Test>]
  let ParsingNoSymbolGivesFailure () =
    Main.init ()

    try
      ProgramDatabase.symbolFolders.Clear()
      let options = Main.I.declareOptions ()
      let symbol = [| "-y" |]

      let parse =
        CommandLine.parseCommandLine symbol options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      ProgramDatabase.symbolFolders.Clear()

  [<Test>]
  let ParsingMultipleDependencyIsOk () =
    Main.init ()

    try
      AssemblyConstants.resolutionTable.Clear()
      let options = Main.I.declareOptions ()

      let here =
        Assembly.GetExecutingAssembly().Location

      let next =
        Path.Combine(Path.GetDirectoryName here, "AltCover.Engine.dll")

      let input = [| "-d"; here; "/d"; next |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      let expected =
        AssemblyConstants.resolutionTable.Keys
        |> Seq.map (fun a -> AssemblyConstants.resolutionTable.[a].Name.Name)
        |> Seq.sort

      Assert.That(
        String.Join(" ", expected),
        Is.EqualTo("AltCover.Engine AltCover.Tests")
      )
    finally
      AssemblyConstants.resolutionTable.Clear()

  //let ParsingNoDependencyGivesFailure() =
  //  Main.init()
  //  try
  //    Instrument.resolutionTable.Clear()
  //    let options = Main.I.declareOptions()
  //    let input = [| "-d" |]
  //    let parse = CommandLine.parseCommandLine input options
  //    match parse with
  //    | Left(x, y) ->
  //      Assert.That(y, Is.SameAs options)
  //      Assert.That(x, Is.EqualTo "UsageError")
  //  finally
  //    Instrument.resolutionTable.Clear()

  [<Test>]
  let ParsingBadDependencyGivesFailure () =
    Main.init ()

    try
      AssemblyConstants.resolutionTable.Clear()
      let options = Main.I.declareOptions ()

      let unique =
        Guid.NewGuid().ToString().Replace("-", "*")

      let input = [| "-d"; unique |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      AssemblyConstants.resolutionTable.Clear()

  [<Test>]
  let ParsingNonDependencyGivesFailure () =
    Main.init ()

    try
      CoverageParameters.defaultStrongNameKey <- None
      CoverageParameters.keys.Clear()

      let options = Main.I.declareOptions ()

      let unique =
        Assembly.GetExecutingAssembly().Location + ".txt"

      let input = [| "-d"; unique |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      CoverageParameters.defaultStrongNameKey <- None
      CoverageParameters.keys.Clear()

  [<Test>]
  let ParsingStrongNameGivesStrongName () =
    Main.init ()

    try
      CoverageParameters.defaultStrongNameKey <- None
      CoverageParameters.keys.Clear()

      let options = Main.I.declareOptions ()

      let input =
        [| "-sn"
           Path.Combine(SolutionRoot.location, "Build/Infrastructure.snk") |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      match CoverageParameters.defaultStrongNameKey with
      | Some x ->
        let token =
          x
          |> KeyStore.tokenOfKey
          |> List.map _.ToString("x2")

        Assert.That(String.Join(String.Empty, token), Is.EqualTo("c02b1a9f5b7cade8"))
    finally
      CoverageParameters.defaultStrongNameKey <- None
      CoverageParameters.keys.Clear()

  [<Test>]
  let ParsingMultipleStrongNameGivesFailure () =
    Main.init ()

    try
      CoverageParameters.defaultStrongNameKey <- None
      CoverageParameters.keys.Clear()

      let options = Main.I.declareOptions ()
      let path = SolutionRoot.location

      let input =
        [| "-sn"
           Path.Combine(path, "Build/Infrastructure.snk")
           "/sn"
           Path.Combine(path, "Build/Recorder.snk") |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")

        Assert.That(
          CommandLine.error |> Seq.head,
          Is.EqualTo "--strongNameKey : specify this only once"
        )
    finally
      CoverageParameters.defaultStrongNameKey <- None
      CoverageParameters.keys.Clear()

  [<Test>]
  let ParsingBadStrongNameGivesFailure () =
    Main.init ()

    try
      CoverageParameters.defaultStrongNameKey <- None
      CoverageParameters.keys.Clear()

      let options = Main.I.declareOptions ()

      let unique =
        Guid.NewGuid().ToString().Replace("-", "*")

      let input = [| "-sn"; unique |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      CoverageParameters.defaultStrongNameKey <- None
      CoverageParameters.keys.Clear()

  [<Test>]
  let ParsingNonStrongNameGivesFailure () =
    Main.init ()

    try
      CoverageParameters.defaultStrongNameKey <- None
      CoverageParameters.keys.Clear()

      let options = Main.I.declareOptions ()

      let unique =
        Assembly.GetExecutingAssembly().Location

      let input = [| "-sn"; unique |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      CoverageParameters.defaultStrongNameKey <- None
      CoverageParameters.keys.Clear()

  [<Test>]
  let ParsingNoStrongNameGivesFailure () =
    Main.init ()

    try
      CoverageParameters.defaultStrongNameKey <- None
      CoverageParameters.keys.Clear()

      let options = Main.I.declareOptions ()
      let input = [| "-sn" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      CoverageParameters.defaultStrongNameKey <- None
      CoverageParameters.keys.Clear()

  [<Test>]
  let ParsingMultipleAltStrongNameIsOk () =
    Main.init ()

    try
      CoverageParameters.defaultStrongNameKey <- None
      CoverageParameters.keys.Clear()

      let options = Main.I.declareOptions ()
      let path = SolutionRoot.location

      let input =
        [| "-k"
           Path.Combine(path, "Build/Infrastructure.snk")
           "/k"
           Path.Combine(path, "Build/Recorder.snk") |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      let expected =
        CoverageParameters.keys.Keys
        |> Seq.map (fun x ->
          String.Join(
            String.Empty,
            BitConverter.GetBytes(x)
            |> Seq.map _.ToString("x2")
          ))
        |> Seq.sort

      Assert.That(
        String.Join(" ", expected),
        Is.EqualTo("4ebffcaabf10ce6a c02b1a9f5b7cade8")
      )
    finally
      CoverageParameters.defaultStrongNameKey <- None
      CoverageParameters.keys.Clear()

  [<Test>]
  let ParsingNoAltStrongNameGivesFailure () =
    Main.init ()

    try
      CoverageParameters.defaultStrongNameKey <- None
      CoverageParameters.keys.Clear()

      let options = Main.I.declareOptions ()
      let input = [| "-k" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      CoverageParameters.defaultStrongNameKey <- None
      CoverageParameters.keys.Clear()

  [<Test>]
  let ParsingBadAltStrongNameGivesFailure () =
    Main.init ()

    try
      CoverageParameters.defaultStrongNameKey <- None
      CoverageParameters.keys.Clear()

      let options = Main.I.declareOptions ()

      let unique =
        Guid.NewGuid().ToString().Replace("-", "*")

      let input = [| "-k"; unique |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      CoverageParameters.defaultStrongNameKey <- None
      CoverageParameters.keys.Clear()

  [<Test>]
  let ParsingNonAltsStrongNameGivesFailure () =
    Main.init ()

    try
      CoverageParameters.defaultStrongNameKey <- None
      CoverageParameters.keys.Clear()

      let options = Main.I.declareOptions ()

      let unique =
        Assembly.GetExecutingAssembly().Location

      let input = [| "-k"; unique |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      CoverageParameters.defaultStrongNameKey <- None
      CoverageParameters.keys.Clear()

  [<Test>]
  let ParsingLocalGivesLocal () =
    Main.init ()

    try
      CoverageParameters.local.Value <- false
      let options = Main.I.declareOptions ()
      let input = [| "--localSource" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      Assert.That(CoverageParameters.local.Value, Is.True)
    finally
      CoverageParameters.local.Value <- false

  [<Test>]
  let ParsingMultipleLocalGivesFailure () =
    Main.init ()

    try
      CoverageParameters.local.Value <- false
      let options = Main.I.declareOptions ()
      let input = [| "-l"; "--localSource" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")

        Assert.That(
          CommandLine.error |> Seq.head,
          Is.EqualTo "--localSource : specify this only once"
        )
    finally
      CoverageParameters.local.Value <- false

  [<Test>]
  let ParsingVisibleGivesVisible () =
    Main.init ()

    try
      CoverageParameters.coalesceBranches.Value <- false
      let options = Main.I.declareOptions ()
      let input = [| "--visibleBranches" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      Assert.That(CoverageParameters.coalesceBranches.Value, Is.True)
    finally
      CoverageParameters.coalesceBranches.Value <- false

  [<Test>]
  let ParsingMultipleVisibleGivesFailure () =
    Main.init ()

    try
      CoverageParameters.coalesceBranches.Value <- false
      let options = Main.I.declareOptions ()
      let input = [| "-v"; "--visibleBranches" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")

        Assert.That(
          CommandLine.error |> Seq.head,
          Is.EqualTo "--visibleBranches : specify this only once"
        )
    finally
      CoverageParameters.coalesceBranches.Value <- false

  [<Test>]
  let ParsingStaticGivesStatic () =
    Main.init ()
    let options = Main.I.declareOptions ()
    let input = [| "--showstatic" |]

    let parse =
      CommandLine.parseCommandLine input options

    match parse with
    | Right(x, y) ->
      Assert.That(y, Is.SameAs options)
      Assert.That(x, Is.Empty)

    Assert.That(
      CoverageParameters.staticFilter,
      StaticFilter.AsCovered |> Some |> Is.EqualTo
    )

  [<Test>]
  let ParsingStaticPlusGivesStatic () =
    Main.init ()
    let options = Main.I.declareOptions ()
    let input = [| "--showstatic:+" |]

    let parse =
      CommandLine.parseCommandLine input options

    match parse with
    | Right(x, y) ->
      Assert.That(y, Is.SameAs options)
      Assert.That(x, Is.Empty)

    Assert.That(
      CoverageParameters.staticFilter,
      StaticFilter.AsCovered |> Some |> Is.EqualTo
    )

  [<Test>]
  let ParsingStaticPlusPlusGivesStaticPlus () =
    Main.init ()
    let options = Main.I.declareOptions ()
    let input = [| "--showstatic:++" |]

    let parse =
      CommandLine.parseCommandLine input options

    match parse with
    | Right(x, y) ->
      Assert.That(y, Is.SameAs options)
      Assert.That(x, Is.Empty)

    Assert.That(
      CoverageParameters.staticFilter,
      StaticFilter.NoFilter |> Some |> Is.EqualTo
    )

  [<Test>]
  let ParsingStaticMinusGivesNoStatic () =
    Main.init ()
    let options = Main.I.declareOptions ()
    let input = [| "--showstatic=-" |]

    let parse =
      CommandLine.parseCommandLine input options

    match parse with
    | Right(x, y) ->
      Assert.That(y, Is.SameAs options)
      Assert.That(x, Is.Empty)

    Assert.That(
      CoverageParameters.staticFilter,
      StaticFilter.Hidden |> Some |> Is.EqualTo
    )

  [<Test>]
  let ParsingMultipleStaticGivesFailure () =
    Main.init ()
    let options = Main.I.declareOptions ()

    let input =
      [| "--showstatic:++"
         "--showstatic:-" |]

    let parse =
      CommandLine.parseCommandLine input options

    match parse with
    | Left(x, y) ->
      Assert.That(y, Is.SameAs options)
      Assert.That(x, Is.EqualTo "UsageError")

      Assert.That(
        CommandLine.error |> Seq.head,
        Is.EqualTo "--showstatic : specify this only once"
      )

      Assert.That(
        CoverageParameters.staticFilter,
        StaticFilter.NoFilter |> Some |> Is.EqualTo
      )

  [<Test>]
  let ParsingJunkStaticGivesFailure () =
    Main.init ()
    let options = Main.I.declareOptions ()
    let tag = Guid.NewGuid().ToString()
    let input = [| "--showstatic:" + tag |]

    let parse =
      CommandLine.parseCommandLine input options

    match parse with
    | Left(x, y) ->
      Assert.That(y, Is.SameAs options)
      Assert.That(x, Is.EqualTo "UsageError")

      Assert.That(
        CommandLine.error |> Seq.head,
        Is.EqualTo("--showstatic : cannot be '" + tag + "'")
      )

  [<Test>]
  let ParsingTimeGivesTime () =
    Main.init ()

    try
      CoverageParameters.trackingNames.Clear()
      CoverageParameters.theInterval <- None

      let options = Main.I.declareOptions ()
      let input = [| "-c"; "5" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      Assert.That(CoverageParameters.interval (), Is.EqualTo 100)
    finally
      CoverageParameters.theInterval <- None
      CoverageParameters.trackingNames.Clear()

  [<Test>]
  let ParsingOnlyArabicNumeralsNotThatSortofArabicNumeralsGivesTime () =
    Main.init ()

    try
      CoverageParameters.trackingNames.Clear()
      CoverageParameters.theInterval <- None

      let options = Main.I.declareOptions ()
      let input = [| "-c"; "٣" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")

      Assert.That(CoverageParameters.interval (), Is.EqualTo 0)
    finally
      CoverageParameters.theInterval <- None
      CoverageParameters.trackingNames.Clear()

  [<Test>]
  let ParsingMultipleTimesGivesFailure () =
    Main.init ()

    try
      CoverageParameters.theInterval <- None
      CoverageParameters.trackingNames.Clear()

      let options = Main.I.declareOptions ()
      let path = SolutionRoot.location
      let input = [| "-c"; "3"; "/c"; "5" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
        Assert.That(CoverageParameters.interval (), Is.EqualTo 10000)

        Assert.That(
          CommandLine.error |> Seq.head,
          Is.EqualTo "--callContext : specify this only once"
        )
    finally
      CoverageParameters.theInterval <- None
      CoverageParameters.trackingNames.Clear()

  [<Test>]
  let ParsingTimeAndNamesGivesOK () =
    Main.init ()

    try
      CoverageParameters.theInterval <- None
      CoverageParameters.trackingNames.Clear()

      let options = Main.I.declareOptions ()
      let path = SolutionRoot.location

      let input =
        [| "-c"
           "3"
           "/c"
           "x"
           "--callContext"
           "Hello, World!" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      Assert.That(CoverageParameters.interval (), Is.EqualTo 10000)

      Assert.That(
        CoverageParameters.trackingNames,
        Is.EquivalentTo [ "x"; "Hello, World!" ]
      )
    finally
      CoverageParameters.theInterval <- None
      CoverageParameters.trackingNames.Clear()

  [<Test>]
  let ParsingBadTimeGivesNoOp () =
    Main.init ()

    try
      CoverageParameters.theInterval <- None
      CoverageParameters.trackingNames.Clear()

      let options = Main.I.declareOptions ()

      let unique =
        Guid.NewGuid().ToString().Replace("-", "*")

      let input = [| "-c"; "9" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      Assert.That(CoverageParameters.interval (), Is.EqualTo 0)
    finally
      CoverageParameters.theInterval <- None
      CoverageParameters.trackingNames.Clear()

  [<Test>]
  let ParsingNonTimeGivesFailure () =
    Main.init ()

    try
      CoverageParameters.theInterval <- None
      CoverageParameters.trackingNames.Clear()

      let options = Main.I.declareOptions ()

      let unique =
        Assembly.GetExecutingAssembly().Location

      let input = [| "-c"; "99" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      CoverageParameters.theInterval <- None
      CoverageParameters.trackingNames.Clear()

  [<Test>]
  let ParsingNoTimeGivesFailure () =
    Main.init ()

    try
      CoverageParameters.theInterval <- None
      CoverageParameters.trackingNames.Clear()

      let options = Main.I.declareOptions ()
      let input = [| "-c"; " " |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      CoverageParameters.theInterval <- None
      CoverageParameters.trackingNames.Clear()

  [<Test>]
  let ParsingNCoverFormatGivesNCover () =
    Main.init ()

    try
      CoverageParameters.theReportFormat <- None
      let options = Main.I.declareOptions ()
      let input = [| "--reportFormat"; "ncover" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      match CoverageParameters.theReportFormat with
      | Some x -> Assert.That(x, Is.EqualTo AltCover.ReportFormat.NCover)
    finally
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let ParsingJsonFormatGivesJson () =
    Main.init ()

    try
      CoverageParameters.theReportFormat <- None
      let options = Main.I.declareOptions ()
      let input = [| "--reportFormat"; "Json" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      match CoverageParameters.theReportFormat with
      | Some x -> Assert.That(x, Is.EqualTo AltCover.ReportFormat.NativeJson)
    finally
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let ParsingOpenCoverFormatGivesOpenCover () =
    Main.init ()

    try
      CoverageParameters.theReportFormat <- None
      let options = Main.I.declareOptions ()
      let input = [| "--reportFormat"; "any" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      match CoverageParameters.theReportFormat with
      | Some x -> Assert.That(x, Is.EqualTo AltCover.ReportFormat.OpenCover)
    finally
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let ParsingMultipleReportFormatGivesFailure () =
    Main.init ()

    try
      CoverageParameters.theReportFormat <- None
      let options = Main.I.declareOptions ()

      let input =
        [| "--reportFormat=opencover"
           "--reportFormat=ncover" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")

        Assert.That(
          CommandLine.error |> Seq.head,
          Is.EqualTo "--reportFormat : specify this only once"
        )
    finally
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let ParsingInPlaceGivesInPlace () =
    Main.init ()

    try
      CoverageParameters.inplace.Value <- false
      let options = Main.I.declareOptions ()
      let input = [| "--inplace" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      Assert.That(CoverageParameters.inplace.Value, Is.True)
    finally
      CoverageParameters.inplace.Value <- false

  [<Test>]
  let ParsingMultipleInPlaceGivesFailure () =
    Main.init ()

    try
      CoverageParameters.inplace.Value <- false
      let options = Main.I.declareOptions ()
      let input = [| "--inplace"; "--inplace" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")

        Assert.That(
          CommandLine.error |> Seq.head,
          Is.EqualTo "--inplace : specify this only once"
        )
    finally
      CoverageParameters.inplace.Value <- false

  [<Test>]
  let ParsingSaveGivesSave () =
    Main.init ()

    try
      CoverageParameters.collect.Value <- false
      let options = Main.I.declareOptions ()
      let input = [| "--save" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)
        Assert.That(CoverageParameters.collect.Value, Is.True)
    finally
      CoverageParameters.collect.Value <- false

  [<Test>]
  let ParsingMultipleSaveGivesFailure () =
    Main.init ()

    try
      CoverageParameters.collect.Value <- false
      let options = Main.I.declareOptions ()
      let input = [| "--save"; "--save" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")

        Assert.That(
          CommandLine.error |> Seq.head,
          Is.EqualTo "--save : specify this only once"
        )
    finally
      CoverageParameters.collect.Value <- false

  [<Test>]
  let ParsingSingleGivesSingle () =
    Main.init ()

    try
      CoverageParameters.single <- false
      let options = Main.I.declareOptions ()
      let input = [| "--single" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      Assert.That(CoverageParameters.single, Is.True)
    finally
      CoverageParameters.single <- false

  [<Test>]
  let ParsingMultipleSingleGivesFailure () =
    Main.init ()

    try
      CoverageParameters.single <- false
      let options = Main.I.declareOptions ()
      let input = [| "--single"; "--single" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")

        Assert.That(
          CommandLine.error |> Seq.head,
          Is.EqualTo "--single : specify this only once"
        )
    finally
      CoverageParameters.single <- false

  [<Test>]
  let ParsingLineCoverGivesLineCover () =
    Main.init ()

    try
      CoverageParameters.coverstyle <- CoverStyle.All
      let options = Main.I.declareOptions ()
      let input = [| "--linecover" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      Assert.That(CoverageParameters.coverstyle, Is.EqualTo CoverStyle.LineOnly)

      Assert.That(
        CoverageParameters.theReportFormat
        |> Option.isNone
      )
    finally
      CoverageParameters.coverstyle <- CoverStyle.All

  [<Test>]
  let OpenCoverIsCompatibleWithLineCover () =
    Main.init ()

    try
      CoverageParameters.coverstyle <- CoverStyle.All
      CoverageParameters.theReportFormat <- None

      let options = Main.I.declareOptions ()

      let input =
        [| "--linecover"
           "--reportFormat=opencover" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      Assert.That(CoverageParameters.coverstyle, Is.EqualTo CoverStyle.LineOnly)

      match CoverageParameters.theReportFormat with
      | Some x -> Assert.That(x, Is.EqualTo AltCover.ReportFormat.OpenCover)
    finally
      CoverageParameters.theReportFormat <- None
      CoverageParameters.coverstyle <- CoverStyle.All

  [<Test>]
  let LineCoverIsCompatibleWithOpenCover () =
    Main.init ()

    try
      CoverageParameters.coverstyle <- CoverStyle.All
      CoverageParameters.theReportFormat <- None

      let options = Main.I.declareOptions ()

      let input =
        [| "--reportFormat=opencover"
           "--linecover" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      Assert.That(CoverageParameters.coverstyle, Is.EqualTo CoverStyle.LineOnly)

      match CoverageParameters.theReportFormat with
      | Some x -> Assert.That(x, Is.EqualTo AltCover.ReportFormat.OpenCover)
    finally
      CoverageParameters.theReportFormat <- None
      CoverageParameters.coverstyle <- CoverStyle.All

  [<Test>]
  let ParsingMultipleLineCoverGivesFailure () =
    Main.init ()

    try
      CoverageParameters.coverstyle <- CoverStyle.All
      let options = Main.I.declareOptions ()

      let input =
        [| "--linecover"; "--linecover" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")

        Assert.That(
          CommandLine.error |> Seq.head,
          Is.EqualTo "--linecover : specify this only once"
        )
    finally
      CoverageParameters.coverstyle <- CoverStyle.All

  [<Test>]
  let LineCoverIsNotCompatibleWithBranchCover () =
    Main.init ()

    try
      CoverageParameters.coverstyle <- CoverStyle.All
      let options = Main.I.declareOptions ()

      let input =
        [| "--linecover"; "--branchcover" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      CoverageParameters.coverstyle <- CoverStyle.All

  [<Test>]
  let ParsingBranchCoverGivesBranchCover () =
    Main.init ()

    try
      CoverageParameters.coverstyle <- CoverStyle.All
      let options = Main.I.declareOptions ()
      let input = [| "--branchcover" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      Assert.That(CoverageParameters.coverstyle, Is.EqualTo CoverStyle.BranchOnly)

      Assert.That(
        CoverageParameters.theReportFormat
        |> Option.isNone
      )
    finally
      CoverageParameters.coverstyle <- CoverStyle.All

  [<Test>]
  let OpenCoverIsCompatibleWithBranchCover () =
    Main.init ()

    try
      CoverageParameters.theReportFormat <- None
      CoverageParameters.coverstyle <- CoverStyle.All

      let options = Main.I.declareOptions ()

      let input =
        [| "--branchcover"
           "--reportFormat=opencover" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      Assert.That(CoverageParameters.coverstyle, Is.EqualTo CoverStyle.BranchOnly)

      match CoverageParameters.theReportFormat with
      | Some x -> Assert.That(x, Is.EqualTo AltCover.ReportFormat.OpenCover)
    finally
      CoverageParameters.theReportFormat <- None
      CoverageParameters.coverstyle <- CoverStyle.All

  [<Test>]
  let BranchCoverIsCompatibleWithOpenCover () =
    Main.init ()

    try
      CoverageParameters.theReportFormat <- None
      CoverageParameters.coverstyle <- CoverStyle.All

      let options = Main.I.declareOptions ()

      let input =
        [| "--reportFormat=opencover"
           "--branchcover" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      Assert.That(CoverageParameters.coverstyle, Is.EqualTo CoverStyle.BranchOnly)

      match CoverageParameters.theReportFormat with
      | Some x -> Assert.That(x, Is.EqualTo AltCover.ReportFormat.OpenCover)
    finally
      CoverageParameters.coverstyle <- CoverStyle.All
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let ParsingMultipleBranchCoverGivesFailure () =
    Main.init ()

    try
      CoverageParameters.coverstyle <- CoverStyle.All
      let options = Main.I.declareOptions ()

      let input =
        [| "--branchcover"; "--branchcover" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")

        Assert.That(
          CommandLine.error |> Seq.head,
          Is.EqualTo "--branchcover : specify this only once"
        )
    finally
      CoverageParameters.coverstyle <- CoverStyle.All

  [<Test>]
  let BranchCoverIsNotCompatibleWithLineCover () =
    Main.init ()

    try
      CoverageParameters.coverstyle <- CoverStyle.All
      let options = Main.I.declareOptions ()

      let input =
        [| "--branchcover"; "--linecover" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
    finally
      CoverageParameters.coverstyle <- CoverStyle.All

  [<Test>]
  let ParsingDropGivesDrop () =
    Main.init ()

    try
      CommandLine.dropReturnCode.Value <- false
      let options = Main.I.declareOptions ()
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
    Main.init ()

    try
      CommandLine.dropReturnCode.Value <- false
      let options = Main.I.declareOptions ()

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
  let ParsingEagerWorks () =
    Main.init ()

    try
      let options = Main.I.declareOptions ()
      let input = [| "--eager" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

      Assert.That(CoverageParameters.eager.Value)
      Assert.That(CoverageParameters.eagerOpCode (), Is.EqualTo OpCodes.Ldc_I4_1)
    finally
      CoverageParameters.eager.Value <- false

  [<Test>]
  let ParsingMultipleEagerGivesFailure () =
    Main.init ()

    try
      let options = Main.I.declareOptions ()
      let input = [| "--eager"; "--eager" |]

      let parse =
        CommandLine.parseCommandLine input options

      match parse with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")

        Assert.That(
          CommandLine.error |> Seq.head,
          Is.EqualTo "--eager : specify this only once"
        )

    finally
      CoverageParameters.eager.Value <- false

  [<Test>]
  let ParsingQuietWorks () =
    Main.init ()

    try
      let options = Main.I.declareOptions ()
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
    Main.init ()

    try
      let options = Main.I.declareOptions ()
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
    Main.init ()

    try
      let options = Main.I.declareOptions ()
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
    Main.init ()

    try
      let options = Main.I.declareOptions ()
      let input = [| "-qq"; "--verbose"; "-qq" |]

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
    Main.init ()

    try
      let options = Main.I.declareOptions ()
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
  let PortableFailsOnMultiInputs () =
    Main.init ()
    let options = Main.I.declareOptions ()
    let saved = (Console.Out, Console.Error)

    try
      use stdout = new StringWriter()
      use stderr = new StringWriter()
      Console.SetOut stdout
      Console.SetError stderr

      let here =
        Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
        |> canonicalDirectory

      CoverageParameters.portable.Value <- true
      CoverageParameters.theInputDirectories.Clear()
      CoverageParameters.theInputDirectories.Add here
      CoverageParameters.theInputDirectories.Add "."

      CoverageParameters.theOutputDirectories.Clear()

      CoverageParameters.theOutputDirectories.AddRange
        CoverageParameters.theInputDirectories

      let arg = ([], options)
      let fail = Right arg

      match Main.I.processOutputLocation fail with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
        Assert.That(stderr.ToString(), Is.Empty)

        Assert.That(
          CommandLine.error,
          Is.EquivalentTo
            [ "The --portable option is not compatible with multiple output locations." ]
        )

        Assert.That(stdout.ToString(), Is.Empty)
    finally
      Console.SetOut(fst saved)
      Console.SetError(snd saved)
      Main.init ()

  [<Test>]
  let OutputLeftPassesThrough () =
    Main.init ()

    let arg =
      (Guid.NewGuid().ToString(), Main.I.declareOptions ())

    let fail = Left arg

    match Main.I.processOutputLocation fail with
    | Left x -> Assert.That(x, Is.SameAs arg)

  [<Test>]
  let OutputInPlaceFails () =
    Main.init ()
    let options = Main.I.declareOptions ()
    let saved = (Console.Out, Console.Error)

    try
      use stdout = new StringWriter()
      use stderr = new StringWriter()
      Console.SetOut stdout
      Console.SetError stderr

      let here =
        Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
        |> canonicalDirectory

      CoverageParameters.theInputDirectories.Clear()
      CoverageParameters.theInputDirectories.Add here

      CoverageParameters.theOutputDirectories.Clear()

      CoverageParameters.theOutputDirectories.AddRange
        CoverageParameters.theInputDirectories

      let arg = ([], options)
      let fail = Right arg

      match Main.I.processOutputLocation fail with
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
        Assert.That(stderr.ToString(), Is.Empty)

        Assert.That(
          CommandLine.error,
          Is.EquivalentTo
            [ "From and to directories "
              + here
              + " are identical" ]
        )

        Assert.That(stdout.ToString(), Is.Empty)
    finally
      Console.SetOut(fst saved)
      Console.SetError(snd saved)
      Main.init ()

  [<Test>]
  let OutputToNewPlaceIsOK () =
    Main.init ()
    let options = Main.I.declareOptions ()
    let saved = (Console.Out, Console.Error)
    CommandLine.toConsole ()
    CommandLine.error <- []

    try
      use stdout = new StringWriter()
      use stderr = new StringWriter()
      Console.SetOut stdout
      Console.SetError stderr

      let here =
        Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)

      CoverageParameters.portable.Value <- true
      CoverageParameters.theInputDirectories.Clear()
      CoverageParameters.theInputDirectories.Add here
      CoverageParameters.theOutputDirectories.Clear()
      CoverageParameters.theOutputDirectories.Add(Path.GetDirectoryName here)
      let rest = [ Guid.NewGuid().ToString() ]
      let arg = (rest, options)
      let ok = Right arg

      match Main.I.processOutputLocation ok with
      | Right(x, y, z, t) ->
        Assert.That(x, Is.SameAs rest)

        y
        |> Seq.iter (fun y' ->
          Assert.That(
            y'.FullName |> canonicalDirectory,
            Is.EqualTo(canonicalDirectory here)
          ))

        z
        |> Seq.iter (fun z' ->
          Assert.That(
            z'.FullName |> canonicalDirectory,
            Is.EqualTo(canonicalDirectory (Path.GetDirectoryName here))
          ))

        t
        |> Seq.zip y
        |> Seq.iter (fun (t', y') -> Assert.That(t'.FullName, Is.EqualTo y'.FullName))

        Assert.That(
          stdout.ToString().Replace("\r", String.Empty),
          Is.EqualTo(
            "Instrumenting files from "
            + here
            + "\nWriting files to "
            + (canonicalDirectory (Path.GetDirectoryName here))
            + "\n"
          )
        )

        Assert.That(stderr.ToString(), Is.Empty)
    finally
      Console.SetOut(fst saved)
      Console.SetError(snd saved)
      Output.verbose <- ignore
      Main.init ()

  [<Test>]
  let OutputToReallyNewPlaceIsOK () =
    Main.init ()
    let options = Main.I.declareOptions ()
    CommandLine.toConsole ()
    let saved = (Console.Out, Console.Error)
    CommandLine.error <- []

    try
      use stdout = new StringWriter()
      use stderr = new StringWriter()
      Console.SetOut stdout
      Console.SetError stderr

      let here =
        Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)

      let there =
        Path.Combine(here, Guid.NewGuid().ToString())

      CoverageParameters.theOutputDirectories.Clear()
      CoverageParameters.theInputDirectories.Clear()

      CoverageParameters.theInputDirectories.Add here
      CoverageParameters.theOutputDirectories.Add there
      let rest = [ Guid.NewGuid().ToString() ]
      let arg = (rest, options)
      let ok = Right arg
      Assert.That(Directory.Exists there, Is.False)

      match Main.I.processOutputLocation ok with
      | Right(x, y, z, t) ->
        Assert.That(x, Is.SameAs rest)

        y
        |> Seq.iter (fun y' -> Assert.That(y'.FullName, Is.EqualTo here))

        z
        |> Seq.iter (fun z' ->
          Assert.That(z'.FullName, Is.EqualTo(canonicalDirectory there)))

        t
        |> Seq.iter (fun t' -> Assert.That(t'.FullName, Is.EqualTo here))

        Assert.That(
          stdout.ToString().Replace("\r", String.Empty),
          Is.EqualTo(
            "Creating folder "
            + (canonicalDirectory there)
            + "\nInstrumenting files from "
            + here
            + "\nWriting files to "
            + (canonicalDirectory there)
            + "\n"
          )
        )

        Assert.That(stderr.ToString(), Is.Empty)
        Assert.That(Directory.Exists there)
    finally
      Console.SetOut(fst saved)
      Console.SetError(snd saved)
      Output.verbose <- ignore
      Main.init ()

  [<Test>]
  let InPlaceToExistingPlaceFails () =
    Main.init ()
    let options = Main.I.declareOptions ()
    let saved = (Console.Out, Console.Error)
    CommandLine.error <- []
    CoverageParameters.inplace.Value <- true

    try
      use stdout = new StringWriter()
      use stderr = new StringWriter()
      Console.SetOut stdout
      Console.SetError stderr

      let here =
        Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)

      CoverageParameters.theOutputDirectories.Clear()
      CoverageParameters.theInputDirectories.Clear()
      CoverageParameters.theInputDirectories.Add here
      CoverageParameters.theOutputDirectories.Add(Path.GetDirectoryName here)
      let rest = [ Guid.NewGuid().ToString() ]
      let arg = (rest, options)
      let ok = Right arg

      match Main.I.processOutputLocation ok with
      | Left _ ->
        Assert.That(stdout.ToString(), Is.Empty)
        Assert.That(stderr.ToString(), Is.Empty)

        Assert.That(
          CommandLine.error,
          Is.EquivalentTo
            [ "Output directory for saved files "
              + (CoverageParameters.outputDirectories ()
                 |> Seq.head)
              + " already exists" ]
        )
    finally
      CoverageParameters.inplace.Value <- false
      Console.SetOut(fst saved)
      Console.SetError(snd saved)
      Output.verbose <- ignore

  [<Test>]
  let InPlaceOperationIsAsExpected () =
    CommandLine.toConsole ()
    Main.init ()
    let options = Main.I.declareOptions ()
    let saved = (Console.Out, Console.Error)
    CommandLine.error <- []
    CoverageParameters.inplace.Value <- true

    try
      use stdout = new StringWriter()
      use stderr = new StringWriter()
      Console.SetOut stdout
      Console.SetError stderr

      let here =
        Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
        |> canonicalDirectory

      let there =
        Path.Combine(here, Guid.NewGuid().ToString())
        |> canonicalDirectory

      CoverageParameters.theOutputDirectories.Clear()
      CoverageParameters.theInputDirectories.Clear()
      CoverageParameters.theInputDirectories.Add here
      CoverageParameters.theOutputDirectories.Add there
      let rest = [ Guid.NewGuid().ToString() ]
      let arg = (rest, options)
      let ok = Right arg
      Assert.That(Directory.Exists there, Is.False)

      match Main.I.processOutputLocation ok with
      | Right(x, y, z, t) ->
        Assert.That(x, Is.SameAs rest)

        y
        |> Seq.iter (fun y' -> Assert.That(y'.FullName, Is.EqualTo here))

        z
        |> Seq.iter (fun z' -> Assert.That(z'.FullName, Is.EqualTo there))

        t
        |> Seq.iter (fun t' -> Assert.That(t'.FullName, Is.EqualTo there))

        Assert.That(
          stdout.ToString().Replace("\r", String.Empty),
          Is.EqualTo(
            "Creating folder "
            + there
            + "\nSaving files to "
            + there
            + "\nInstrumenting files in "
            + here
            + "\n"
          )
        )

        Assert.That(stderr.ToString(), Is.Empty)
        Assert.That(Directory.Exists there)

        Assert.That(
          CoverageParameters.sourceDirectories ()
          |> Seq.head,
          Is.EqualTo there
        )

        Assert.That(
          CoverageParameters.instrumentDirectories ()
          |> Seq.head,
          Is.EqualTo here
        )
    finally
      CoverageParameters.inplace.Value <- false
      Console.SetOut(fst saved)
      Console.SetError(snd saved)
      Output.verbose <- ignore

  [<Test>]
  let ImageLoadResilientPassesThrough () =
    Main.init ()
    let one = ref false
    let two = ref false
    let set2 _ = two.Value <- true
    Main.I.imageLoadResilient (fun _ -> one.Value <- true) set2
    Assert.That(one.Value)
    Assert.That(two.Value, Is.False)
    set2 ()
    Assert.That(two.Value, Is.True)

  [<Test>]
  let ResilientHandlesIOException () =
    Main.init ()
    let one = ref false
    let two = ref false

    let set1 f () =
      f ()
      one.Value <- true

    let io = IOException("fail")
    let fio () = io |> raise

    Main.I.imageLoadResilient (set1 fio) (fun x ->
      Assert.That(x, Is.SameAs io)
      two.Value <- true)

    Assert.That(one.Value, Is.False)
    Assert.That(two.Value)
    set1 ignore ()
    Assert.That(one.Value, Is.True)

  [<Test>]
  let ResilientHandlesBadImageFormatException () =
    Main.init ()
    let one = ref false
    let two = ref false

    let set1 f () =
      f ()
      one.Value <- true

    let bif = BadImageFormatException("fail")
    let fbif () = bif |> raise

    Main.I.imageLoadResilient (set1 fbif) (fun x ->
      Assert.That(x, Is.SameAs bif)
      two.Value <- true)

    Assert.That(one.Value, Is.False)
    Assert.That(two.Value)
    set1 ignore ()
    Assert.That(one.Value, Is.True)

  [<Test>]
  let ResilientHandlesArgumentException () =
    Main.init ()
    let one = ref false
    let two = ref false

    let set1 f () =
      f ()
      one.Value <- true

    let arg = ArgumentException("fail")
    let farg () = arg |> raise

    Main.I.imageLoadResilient (set1 farg) (fun x ->
      Assert.That(x, Is.SameAs arg)
      two.Value <- true)

    Assert.That(one.Value, Is.False)
    Assert.That(two.Value)
    set1 ignore ()
    Assert.That(one.Value, Is.True)

  [<Test>]
  let FolderNestingIsDetectedCorrectly () =
    let dir = canonicalDirectory "some/path/"

    let file1 =
      Path.Combine(canonicalDirectory "different", "path")

    test <@ (Main.I.isInDirectory file1 dir) |> not @>

    let file2 =
      Path.Combine(canonicalDirectory "some/pathway", "a.b")

    test <@ (Main.I.isInDirectory file2 dir) |> not @>

    let file3 =
      Path.Combine(canonicalDirectory "some/path/nested", "a.b")

    test <@ (Main.I.isInDirectory file3 dir) @>

  [<Test>]
  let ScreeningFilesShouldRejectTheInstrumentedOnes () =
    Main.init ()
    // because mono symbol-writing is broken, work around trying to
    // examine the instrumented files in a self-test run.
    let here =
      Path.Combine(
        SolutionRoot.location,
        "_Binaries/Sample4LongForm/Debug+AnyCPU/legacy/net472"
      )

    maybeIgnore (fun () -> here |> Directory.Exists |> not)

    //let there =
    //  Path.Combine(
    //    Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location),
    //    Guid.NewGuid().ToString()
    //  )

    let save = Output.warn

    try
      let sb = System.Text.StringBuilder()
      let warn (s: String) = s |> sb.Append |> ignore
      Output.warn <- warn

      here
      |> Directory.GetFiles
      |> Seq.filter (fun f ->
        match Path.GetExtension f with
        | ".dll"
        | ".exe" -> true
        | _ -> false)
      |> Seq.iter (fun f ->
        use stream = File.OpenRead(f)

        use def =
          AssemblyResolver.ReadAssembly(stream)

        ProgramDatabase.readSymbols def

        let result =
          Main.I.screenAssembly (Path.GetFileName f) def

        Assert.That(result |> Option.isSome, f))

      Assert.That(sb.ToString(), Is.Empty)

      let path =
        Path.Combine(AltCoverTests.dir, "Sample3.dll")

      let prepared =
        Instrument.I.prepareAssembly (File.OpenRead path)

      //Instrument.I.writeAssembly prepared (Path.Combine(there, "Sample3.dll"))

      let second =
        Path.Combine(here, "Sample4.dll")

      use assembly =
        AssemblyResolver.ReadAssembly second

      assembly.MainModule.AssemblyReferences.Add(prepared.Name)

      let entry =
        { Assembly = assembly
          Inspection = Inspections.Ignore
          Destinations = []
          Identity =
            { Assembly = "Sample4.dll"
              Configuration = "ScreeningFilesShouldRejectTheInstrumentedOnes" } }

      Instrument.I.injectInstrumentation prepared entry
      //Instrument.I.writeAssembly assembly (Path.Combine(there, "Sample4.dll")

      let s1 =
        Main.I.screenAssembly "Sample3.dll" prepared

      Assert.That(s1 |> Option.isNone, "Sample3.dll")

      Environment.NewLine |> sb.Append |> ignore

      let s1 =
        Main.I.screenAssembly "Sample4.dll" assembly

      Assert.That(s1 |> Option.isNone, "Sample4.dll")

      let expected =
        "Skipping Sample3.dll as it has already been instrumented."
        + Environment.NewLine
        + "Skipping Sample4.dll as it has already been instrumented."

      Assert.That(sb.ToString(), Is.EqualTo expected)

    finally
      Output.warn <- save

  [<Test>]
  let PreparingNewPlaceShouldCopyEverything () =
    Main.init ()
    // because mono symbol-writing is broken, work around trying to
    // examine the instrumented files in a self-test run.
    let here =
      Path.Combine(
        SolutionRoot.location,
        "_Binaries/Sample4LongForm/Debug+AnyCPU/legacy/net472"
      )

    maybeIgnore (fun () -> here |> Directory.Exists |> not)

    let there =
      Path.Combine(
        Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location),
        Guid.NewGuid().ToString()
      )

    let toInfo =
      [ Directory.CreateDirectory there ]

    let fromInfo = [ DirectoryInfo(here) ]

    let x, y =
      Main.I.prepareTargetFiles fromInfo toInfo fromInfo [ there ]

    let f, t =
      Seq.zip fromInfo toInfo |> Seq.head

    let flen = here.Length
    let tlen = there.Length

    Assert.That(
      t.EnumerateFiles("*", SearchOption.AllDirectories)
      |> Seq.map _.FullName.Substring(tlen),
      Is.EquivalentTo(
        f.EnumerateFiles("*", SearchOption.AllDirectories)
        |> Seq.map _.FullName.Substring(flen)
      ),
      "Simple to-from comparison failed"
    )

    //t.EnumerateFiles("*", SearchOption.AllDirectories) |> Seq.map (fun x -> x.FullName.Substring(tlen))
    //|> Seq.iter (printfn "%A")
    //f.EnumerateFiles("*", SearchOption.AllDirectories) |> Seq.map (fun x -> x.FullName.Substring(flen))
    //|> Seq.iter (printfn "%A")

    Assert.That(
      File.Exists
      <| Path.Combine(here, "eo/Sample4.resources.dll")
    )

    Assert.That(
      File.Exists
      <| Path.Combine(there, "eo/Sample4.resources.dll")
    )

    Assert.That(
      y,
      Is.EquivalentTo(
        x
        |> Seq.map (_.AssemblyPath >> Path.GetFileNameWithoutExtension)
      ),
      "Prepared lists mismatch"
    )

    test <@ (x |> List.head).Destinations |> Seq.head = t.FullName @>

    let assemblyPaths =
      x
      |> Seq.map _.AssemblyPath
      |> Seq.sort
      |> Seq.toList

    let assemblies =
      f.EnumerateFiles()
      |> Seq.map _.FullName
      |> Seq.filter (fun f ->
        f.EndsWith(".exe", StringComparison.OrdinalIgnoreCase)
        || f.EndsWith(".dll", StringComparison.OrdinalIgnoreCase))
      |> Seq.filter (
        AssemblyResolver.ReadAssembly
        >> ProgramDatabase.getPdbFromImage
        >> snd // TODO
        >> Option.isSome
      )
      |> Seq.sort
      |> Seq.toList

    test <@ assemblies = assemblyPaths @>

  [<Test>]
  let ShouldProcessTrailingArguments () =
    let where =
      Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)

    let path =
      Path.Combine(SolutionDir(), "_Mono/Sample1")

    maybeIgnore (fun () -> path |> Directory.Exists |> not)
    let files = Directory.GetFiles(path)

    let program =
      files
      |> Seq.filter _.EndsWith(".exe", StringComparison.OrdinalIgnoreCase)
      |> Seq.head

    let saved = (Console.Out, Console.Error)
    let e0 = Console.Out.Encoding
    let e1 = Console.Error.Encoding
    CommandLine.toConsole ()

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
        CommandLine.processTrailingArguments args (DirectoryInfo(where))

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
  let StoresAsExpected () =
    Main.init ()
    TaskIO.store <- String.Empty
    TaskIO.logToStore.Info "23"
    Assert.That(TaskIO.store, Is.EqualTo "23")

  [<Test>]
  let ImportModuleIsAsExpected () =
    Main.init ()
    let saved = Console.Out

    try
      let unique = "any"

      let here =
        Assembly.GetExecutingAssembly().Location
        |> Path.GetDirectoryName
        |> Path.GetDirectoryName

      let placeholder = Path.Combine(here, unique)

      let info =
        Directory.CreateDirectory(placeholder)

      let psh =
        Path.Combine(info.FullName, "AltCover.PowerShell.dll")

      maybeDeleteFile psh

      do
        use _dummy = File.Create psh
        ()

      use stdout = new StringWriter()
      Console.SetOut stdout
      CommandLine.toConsole ()

      let rc =
        AltCover.Main.effectiveMain [| "i" |]

      Assert.That(rc, Is.EqualTo 0)
      let result = stdout.ToString()

      let expected =
        "Import-Module \""
        + (Path.Combine(here, unique + "/AltCover.PowerShell.dll")
           |> Path.GetFullPath)
        + "\""
        + Environment.NewLine

      test <@ result.Equals(expected, StringComparison.Ordinal) @>
    //Assert.That(result, Is.EqualTo(expected))
    finally
      Console.SetOut saved
      Output.verbose <- ignore

  [<Test>]
  let VersionIsAsExpected () =
    Main.init ()
    let saved = Console.Out

    try
      use stdout = new StringWriter()
      Console.SetOut stdout
      CommandLine.toConsole ()

      let rc =
        AltCover.Main.effectiveMain [| "v" |]

      Assert.That(rc, Is.EqualTo 0)

      let result =
        stdout.ToString().Replace("\r\n", "\n")

      let expected =
        "AltCover version "
        + AssemblyVersionInformation.AssemblyPackageVersion
        + Environment.NewLine

      Assert.That(
        result.Replace("\r\n", "\n"),
        Is.EqualTo(expected.Replace("\r\n", "\n"))
      )

      Assert.That(
        result.Replace("\r\n", "\n"),
        (AltCover.CommandLine.Format.Local(
          "AltCover.Version",
          [| Command.Version() :> obj |]
         )
         + "\n")
        |> Is.EqualTo
      )
    finally
      Console.SetOut saved
      Output.verbose <- ignore

  [<Test>]
  let UsageIsAsExpected () =
    Main.init ()
    let options = Main.I.declareOptions ()
    let saved = Console.Error

    try
      use stderr = new StringWriter()
      Console.SetError stderr
      let empty = OptionSet()

      CommandLine.usageBase
        { Intro = "UsageError"
          Options = options
          Options2 = empty }

      let result =
        stderr.ToString().Replace("\r\n", "\n")

      let expected =
        "Error - usage is:\n"
        + AltCoverUsage.usageText
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
        result.Replace("\r\n", "\n"),
        Is.EqualTo(expected.Replace("\r\n", "\n"))
      )
    finally
      Console.SetError saved
      Output.verbose <- ignore

#if !NET472
  [<Test>]
  let TargetsPathIsAsExpected () =
    Main.init ()
    let saved = (Console.Out, Console.Error)

    try
      use stdout = new StringWriter()
      use stderr = new StringWriter()
      Console.SetOut stdout
      Console.SetError stderr

      let main =
        typeof<Marker>.Assembly
          .GetType("AltCover.EntryPoint")
          .GetMethod("main", BindingFlags.NonPublic ||| BindingFlags.Static)

      let returnCode =
        main.Invoke(nullObject, [| [| "TargetsPath" |] |])

      Assert.That(returnCode, Is.EqualTo 0)
      test <@ stderr.ToString() |> String.IsNullOrEmpty @>

      let here =
        Assembly.GetExecutingAssembly().Location

      let expected =
        Path.Combine(
          here |> Path.GetDirectoryName,
          "../../../build/netstandard2.0/altcover.global.targets"
        )
        |> Path.GetFullPath

      test
        <@
          stdout
            .ToString()
            .Equals(
              expected.Replace("\\\\", "\\")
              + Environment.NewLine,
              StringComparison.Ordinal
            )
        @>
    finally
      Console.SetOut(fst saved)
      Console.SetError(snd saved)
#endif

  [<Test>]
  let ErrorResponseIsAsExpected () =
    Main.init ()
    let saved = Console.Error

    try
      use stderr = new StringWriter()
      Console.SetError stderr
      let unique = Guid.NewGuid().ToString()

      let main =
        typeof<Marker>.Assembly
          .GetType("AltCover.EntryPoint")
          .GetMethod("main", BindingFlags.NonPublic ||| BindingFlags.Static)

      let returnCode =
        main.Invoke(nullObject, [| [| "-i"; unique |] |])

      Assert.That(returnCode, Is.EqualTo 255)

      let result =
        stderr.ToString().Replace("\r\n", "\n")

      let expected =
        "\"-i\" \""
        + unique
        + "\"\n"
        + "--inputDirectory : Directory "
        + unique
        + " not found\n"
        + "Error - usage is:\n"
        + AltCoverUsage.usageText
        + "\nor\n"
        + AltCoverUsage.runnerText
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
        result
          .Replace("\r\n", "\n")
          .Replace("\u200b", String.Empty),
        Is.EqualTo(expected.Replace("\r\n", "\n"))
      )

      let helptext =
        Output.resources
          .GetString("HelpText")
          .Replace("\r\n", "\n")

      let fixupBase (s: String) =

        let valued =
          s.EndsWith("=", StringComparison.Ordinal)

        let optional =
          s.EndsWith(":", StringComparison.Ordinal)

        let abbrev =
          match s |> Seq.take 3 |> Seq.toList with
          | [ x; '|'; y ] when x = y -> true
          | _ -> false

        let core =
          if abbrev then
            let h =
              s |> Seq.take 1 |> Seq.toArray |> String

            let t =
              s |> Seq.skip 3 |> Seq.toArray |> String

            (h + "[" + t + "]").Replace("=]", "]=")
          else
            s

        if valued then
          "[/" + core + "VALUE]"
        else if optional then
          ("[/" + core + "[VALUE]]").Replace(":[", "[=")
        else
          "[--" + core + "]"

      let fixup (s: String) =
        if s.Length < 2 then
          sprintf "[-%s]" s
        else
          fixupBase s

      let mainHelp =
        Main.I.declareOptions ()
        |> Seq.map _.Prototype
        |> Seq.filter (fun s -> s.Length <> 2)
        |> Seq.map fixup

      let runnerHelp =
        Runner.declareOptions ()
        |> Seq.map _.Prototype
        |> Seq.filter (fun s -> s.Length <> 2)
        |> Seq.map fixup

      let synthetic =
        "AltCover "
        + String.Join(" ", mainHelp)
        + " [-- ] [...]\nor\nAltCover Runner "
        + String.Join(" ", runnerHelp)
        + " [-- ] [...]\nor\nAltCover ImportModule\nor\nAltCover Version\n"
        + "or, for the global tool only\nAltCover TargetsPath\n\n"
        + "See https://stevegilham.github.io/altcover/Usage for full details.\n"

      Assert.That(synthetic, Is.EqualTo helptext)
      test <@ synthetic = helptext @>

#if !MONO // Mono won't play nicely with Esperanto placeholder locale  // remove for fantomas
      let dir =
        System.Reflection.Assembly
          .GetExecutingAssembly()
          .Location
        |> Path.GetDirectoryName

      let eo =
        Path.Combine(dir, "./eo/AltCover.Engine.resources.dll")

      let resources =
        System.Resources.ResourceManager("AltCover.Strings.eo", Assembly.LoadFile eo)

      let helptexteo =
        resources
          .GetString("HelpText")
          .Replace("\r\n", "\n")

      let syntheticeo =
        "AltCover "
        + String
          .Join(" ", mainHelp)
          .Replace("VALUE", "VALO")
        + " [-- ] [...]\naŭ\nAltCover Runner "
        + String
          .Join(" ", runnerHelp)
          .Replace("VALUE", "VALO")
        + " [-- ] [...]\naŭ\nAltCover ImportModule\naŭ\nAltCover Version\n"
        + "aŭ, nur por la tutmonda ilo\nAltCover TargetsPath\n\n"
        + "Vidu https://stevegilham.github.io/altcover/Usage por plenaj detaloj.\n"

      test <@ syntheticeo = helptexteo @>
#endif // Mono   // remove for fantomas
    finally
      Console.SetError saved

  // Tasks.fs
  type Logging() =
    member val Info: Action<String> = null with get, set
    member val Warn: Action<String> = null with get, set
    member val Failure: Action<String> = null with get, set
    member val Echo: Action<String> = null with get, set
    member val Verbose: Action<String> = null with get, set

    interface Abstract.ILoggingOptions with
      member self.Info = self.Info
      member self.Warn = self.Warn
      member self.Failure = self.Failure
      member self.Echo = self.Echo
      member self.Verbose = self.Verbose

  [<Test>]
  let LoggingCanBeExercised () =
    Main.init ()
    Assert.That(AltCover.LoggingOptions.ActionAdapter null, Is.Not.Null)
    (AltCover.LoggingOptions.ActionAdapter null) "23"

    let ignoreAction =
      new Action<String>(ignore)

    ignoreAction.Invoke("ignoreAction")
    Assert.That(AltCover.LoggingOptions.ActionAdapter(ignoreAction), Is.Not.Null)
    let mutable x = String.Empty
    let f = (fun s -> x <- s)
    (AltCover.LoggingOptions.ActionAdapter(new Action<String>(f))) "42"
    Assert.That(x, Is.EqualTo "42")
    AltCover.LoggingOptions.Create().Info "32"
    AltCover.LoggingOptions.Create().Warn "32"
    AltCover.LoggingOptions.Create().Error "32"
    AltCover.LoggingOptions.Create().Echo "32"
    AltCover.LoggingOptions.Create().Verbose "32"

    let o = Logging()
    o.Info <- null
    o.Warn <- null
    o.Failure <- null
    o.Echo <- null
    o.Verbose <- null

    Assert.That(o.Info, Is.Null)
    Assert.That(o.Warn, Is.Null)
    Assert.That(o.Failure, Is.Null)
    Assert.That(o.Echo, Is.Null)
    Assert.That(o.Verbose, Is.Null)

    let p = AltCover.LoggingOptions.Translate o
    Assert.That(p.Warn, Is.Not.Null)
    let p2 = AltCover.LoggingOptions.Abstract o
    p2.Info "32"
    p2.Warn "32"
    p2.Error "32"
    p2.Echo "32"
    p2.Verbose "32"

  [<Test>]
  let EmptyInstrumentIsJustTheDefaults () =
    Main.init ()
    let subject = Prepare()

    subject.GetType().GetProperties()
    |> Seq.iter (fun p ->
      let v = p.GetValue(subject)

      if p.CanWrite then
        p.SetValue(subject, v))

    let save = Main.effectiveMain
    let mutable args = [| "some junk " |]
    let saved = (Output.info, Output.error)

    let aclog =
      subject
        .GetType()
        .GetProperty("ACLog", BindingFlags.Instance ||| BindingFlags.NonPublic)

    try
      // subject.ACLog <- Some <| FSApi.Logging.Create()
      aclog.SetValue(subject, Some <| AltCover.LoggingOptions.Create())

      Main.effectiveMain <-
        (fun a ->
          args <- a
          255)

      let result = subject.Execute()
      Assert.That(result, Is.False)

      Assert.That(
        args,
        Is.EquivalentTo
          [ "--reportFormat"
            "OpenCover"
            "--save"
            "--eager" ]
      )
    finally
      Main.effectiveMain <- save
      Output.info <- fst saved
      Output.error <- snd saved

  [<Test>]
  let InstrumentLevelsCanBeSet () =
    Main.init ()
    let subject = Prepare()

    subject.GetType().GetProperties()
    |> Seq.iter (fun p ->
      let v = p.GetValue(subject)

      if p.CanWrite then
        p.SetValue(subject, v))

    let save = Main.effectiveMain
    let mutable args = [| "some junk " |]
    let saved = (Output.info, Output.error)

    let aclog =
      subject
        .GetType()
        .GetProperty("ACLog", BindingFlags.Instance ||| BindingFlags.NonPublic)

    try
      // subject.ACLog <- Some <| FSApi.Logging.Create()
      [ "Off", [ "-q"; "-q"; "-q" ]
        "Verbose", [ "--verbose" ]
        "NoneOfTheAbove", []
        "Info", []
        "Warning", [ "-q" ]
        "Error", [ "-q"; "-q" ] ]
      |> List.iter (fun (level, q) ->
        subject
          .GetType()
          .GetProperty("Verbosity")
          .SetValue(subject, level)

        aclog.SetValue(subject, Some <| AltCover.LoggingOptions.Create())

        Main.effectiveMain <-
          (fun a ->
            args <- a
            255)

        let result = subject.Execute()
        Assert.That(result, Is.False)

        Assert.That(
          args,
          Is.EquivalentTo(
            [ "--reportFormat"
              "OpenCover"
              "--save"
              "--eager" ]
            @ q
          ),
          level
        ))
    finally
      CommandLine.verbosity <- 0
      Main.effectiveMain <- save
      Output.info <- fst saved
      Output.error <- snd saved

  [<Test>]
  let NonDefaultInstrumentObsoleteIsOK () =
    Main.init ()
    let subject = Prepare()
    let save = Main.effectiveMain
    let mutable args = [| "some junk " |]
    let saved = (Output.info, Output.error)

    let aclog =
      subject
        .GetType()
        .GetProperty("ACLog", BindingFlags.Instance ||| BindingFlags.NonPublic)

    try
      // subject.ACLog <- Some <| FSApi.Logging.Create()
      aclog.SetValue(subject, Some <| AltCover.LoggingOptions.Create())

      Main.effectiveMain <-
        (fun a ->
          args <- a
          0)

      subject.ReportFormat <- "Ncover"
      subject.CommandLine <- [| "testing"; "1"; "2"; "3" |]
      subject.SymbolDirectories <- [| "a"; "b" |]

      let result = subject.Execute()
      Assert.That(result, Is.True)

      Assert.That(
        args,
        Is.EquivalentTo
          [ "-y"
            "a"
            "-y"
            "b"
            "--reportFormat"
            "Ncover"
            "--save"
            "--eager"
            "--"
            "testing"
            "1"
            "2"
            "3" ]
      )
    finally
      Main.effectiveMain <- save
      Output.info <- fst saved
      Output.error <- snd saved

  [<Test>]
  let NonDefaultInstrumentIsOK () =
    Main.init ()
    let subject = Prepare()
    let save = Main.effectiveMain
    let mutable args = [| "some junk " |]
    let saved = (Output.info, Output.error)

    try
      Main.effectiveMain <-
        (fun a ->
          args <- a
          0)

      subject.ReportFormat <- "ncover"
      subject.CommandLine <- [| "testing"; "1"; "2"; "3" |]
      subject.SymbolDirectories <- [| "a"; "b" |]

      let result = subject.Execute()
      Assert.That(result, Is.True)

      Assert.That(
        args,
        Is.EquivalentTo
          [ "-y"
            "a"
            "-y"
            "b"
            "--reportFormat"
            "ncover"
            "--save"
            "--eager"
            "--"
            "testing"
            "1"
            "2"
            "3" ]
      )

      let message =
        subject
          .GetType()
          .GetMethod("Message", BindingFlags.Instance ||| BindingFlags.NonPublic)

      let x =
        Assert.Throws<System.Reflection.TargetInvocationException>(fun () ->
          ignore (message.Invoke(subject, [| "x" :> obj |])))

      Assert.That(
        x.InnerException,
        Is.Not.Null.And.InstanceOf<InvalidOperationException>()
      )

      Assert.Throws<InvalidOperationException>(fun () -> Output.info "x")
      |> ignore

      Assert.Throws<InvalidOperationException>(fun () -> Output.warn "x")
      |> ignore

      Assert.Throws<InvalidOperationException>(fun () -> Output.error "x")
      |> ignore
    finally
      Main.effectiveMain <- save
      Output.info <- fst saved
      Output.error <- snd saved

  [<Test>]
  let EmptyCollectIsJustTheDefaults () =
    Main.init ()
    let subject = Collect()

    subject.GetType().GetProperties()
    |> Seq.iter (fun p ->
      let v = p.GetValue(subject)

      if p.CanWrite then
        p.SetValue(subject, v))

    let save = Main.effectiveMain
    let mutable args = [| "some junk " |]
    let saved = (Output.info, Output.error)

    let aclog =
      subject
        .GetType()
        .GetProperty("ACLog", BindingFlags.Instance ||| BindingFlags.NonPublic)

    try
      // subject.ACLog <- Some <| FSApi.Logging.Create()
      aclog.SetValue(subject, Some <| AltCover.LoggingOptions.Create())

      Main.effectiveMain <-
        (fun a ->
          args <- a
          255)

      let result = subject.Execute()
      Assert.That(result, Is.False)

      Assert.That(args, Is.EquivalentTo [ "Runner"; "--collect" ])
    finally
      Main.effectiveMain <- save
      Output.info <- fst saved
      Output.error <- snd saved

  [<Test>]
  let CollectLevelsCanBeSet () =
    Main.init ()
    let subject = Collect()

    subject.GetType().GetProperties()
    |> Seq.iter (fun p ->
      let v = p.GetValue(subject)

      if p.CanWrite then
        p.SetValue(subject, v))

    let save = Main.effectiveMain
    let mutable args = [| "some junk " |]
    let saved = (Output.info, Output.error)

    let aclog =
      subject
        .GetType()
        .GetProperty("ACLog", BindingFlags.Instance ||| BindingFlags.NonPublic)

    try
      [ "Off", [ "-q"; "-q"; "-q" ]
        "Verbose", [ "--verbose" ]
        "NoneOfTheAbove", []
        "Info", []
        "Warning", [ "-q" ]
        "Error", [ "-q"; "-q" ] ]
      |> List.iter (fun (level, q) ->
        // subject.ACLog <- Some <| FSApi.Logging.Create()
        subject
          .GetType()
          .GetProperty("Verbosity")
          .SetValue(subject, level)

        aclog.SetValue(subject, Some <| AltCover.LoggingOptions.Create())

        Main.effectiveMain <-
          (fun a ->
            args <- a
            255)

        let result = subject.Execute()
        Assert.That(result, Is.False)
        Assert.That(args, Is.EquivalentTo([ "Runner"; "--collect" ] @ q), level))
    finally
      CommandLine.verbosity <- 0
      Main.effectiveMain <- save
      Output.info <- fst saved
      Output.error <- snd saved

  [<Test>]
  let CollectWithExeIsNotCollecting () =
    Main.init ()
    let subject = Collect()
    let save = Main.effectiveMain
    let mutable args = [| "some junk " |]
    let saved = (Output.info, Output.error)

    try
      Main.effectiveMain <-
        (fun a ->
          args <- a
          0)

      subject.Executable <- "dotnet"
      subject.CommandLine <- [| "test" |]

      let result = subject.Execute()
      Assert.That(result, Is.True)

      Assert.That(
        args,
        Is.EquivalentTo
          [ "Runner"
            "-x"
            "dotnet"
            "--"
            "test" ]
      )

      let message =
        subject
          .GetType()
          .GetMethod("Message", BindingFlags.Instance ||| BindingFlags.NonPublic)

      let x =
        Assert.Throws<System.Reflection.TargetInvocationException>(fun () ->
          ignore (message.Invoke(subject, [| "x" :> obj |])))

      Assert.That(
        x.InnerException,
        Is.Not.Null.And.InstanceOf<InvalidOperationException>()
      )

      Assert.Throws<InvalidOperationException>(fun () -> Output.info "x")
      |> ignore

      Assert.Throws<InvalidOperationException>(fun () -> Output.warn "x")
      |> ignore

      Assert.Throws<InvalidOperationException>(fun () -> Output.error "x")
      |> ignore
    finally
      Main.effectiveMain <- save
      Output.info <- fst saved
      Output.error <- snd saved

  [<Test>]
  let EmptyPowerShellIsJustTheDefaults () =
    Main.init ()
    let subject = PowerShell()
    let save = Main.effectiveMain
    let mutable args = [| "some junk " |]
    let saved = (Output.info, Output.error)
    let warned = Output.warn

    let io =
      subject
        .GetType()
        .GetProperty("IO", BindingFlags.Instance ||| BindingFlags.NonPublic)

    let defaultIO =
      io.GetValue(subject) :?> AltCover.LoggingOptions

    Assert.Throws<InvalidOperationException>(fun () -> defaultIO.Warn "x")
    |> ignore

    Assert.Throws<InvalidOperationException>(fun () -> defaultIO.Error "x")
    |> ignore
    // subject.IO <- FSApi.Logging.Create()
    io.SetValue(subject, AltCover.LoggingOptions.Create())

    try
      Main.effectiveMain <-
        (fun a ->
          args <- a
          0)

      let result = subject.Execute()
      Assert.That(result, Is.True)
      Assert.That(args, Is.EquivalentTo [ "ImportModule" ])
      Output.warn "x"
      Output.error "x"
    finally
      Main.effectiveMain <- save
      Output.info <- fst saved
      Output.error <- snd saved
      Output.warn <- warned

  [<Test>]
  let EmptyVersionIsJustTheDefaults () =
    Main.init ()
    let subject = GetVersion()
    let save = Main.effectiveMain
    let mutable args = [| "some junk " |]
    let saved = (Output.info, Output.error)
    let warned = Output.warn

    let io =
      subject
        .GetType()
        .GetProperty("IO", BindingFlags.Instance ||| BindingFlags.NonPublic)

    let defaultIO =
      io.GetValue(subject) :?> AltCover.LoggingOptions

    Assert.Throws<InvalidOperationException>(fun () -> defaultIO.Warn "x")
    |> ignore

    Assert.Throws<InvalidOperationException>(fun () -> defaultIO.Error "x")
    |> ignore
    // subject.IO <- FSApi.Logging.Create()
    io.SetValue(subject, AltCover.LoggingOptions.Create())

    try
      Main.effectiveMain <-
        (fun a ->
          args <- a
          0)

      let result = subject.Execute()
      Assert.That(result, Is.True)
      Assert.That(args, Is.EquivalentTo [ "Version" ])
      Output.warn "x"
      Output.error "x"
    finally
      Main.effectiveMain <- save
      Output.info <- fst saved
      Output.error <- snd saved
      Output.warn <- warned

  [<Test>]
  let EchoWorks () =
    Main.init ()
    let saved = (Console.Out, Console.Error)
    let e0 = Console.Out.Encoding
    let e1 = Console.Error.Encoding
    let before = Console.ForegroundColor

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

      let subject = Echo()
      let unique = Guid.NewGuid().ToString()
      subject.Text <- unique
      subject.Colour <- "cyan"
      Assert.That(subject.Execute(), Is.True)
      Assert.That(Console.ForegroundColor, Is.EqualTo before)
      Assert.That(stderr.ToString(), Is.Empty)
      Assert.That(stdout.ToString(), Is.EqualTo(unique + Environment.NewLine))
    finally
      Console.SetOut(fst saved)
      Console.SetError(snd saved)

  [<Test>]
  let EchoFallsSilent () =
    Main.init ()
    let saved = (Console.Out, Console.Error)
    let e0 = Console.Out.Encoding
    let e1 = Console.Error.Encoding
    let before = Console.ForegroundColor

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

      let subject = Echo()
      subject.Verbosity <- "Off"
      let unique = Guid.NewGuid().ToString()
      subject.Text <- unique
      subject.Colour <- "cyan"
      Assert.That(subject.Execute(), Is.True)
      Assert.That(Console.ForegroundColor, Is.EqualTo before)
      Assert.That(stderr.ToString(), Is.Empty)
      Assert.That(stdout.ToString(), Is.Empty)
    finally
      Console.SetOut(fst saved)
      Console.SetError(snd saved)

  let MakeRunSettings () =
    let subject = RunSettings()

    let temper =
      subject
        .GetType()
        .GetProperty("GetTempFileName", BindingFlags.Instance ||| BindingFlags.NonPublic)

    let basic =
      temper.GetValue(subject) :?> (unit -> string)

    let badge =
      (fun () ->
        let t1 = basic ()
        Path.Combine(Path.GetDirectoryName t1, "altcover.test." + Path.GetFileName(t1)))

    temper.SetValue(subject, badge)
    subject

  [<Test>]
  let RunSettingsFailsIfCollectorNotFound () =
    Main.init ()
    let subject = MakeRunSettings()

    let dc =
      subject
        .GetType()
        .GetProperty("DataCollector", BindingFlags.Instance ||| BindingFlags.NonPublic)
    // subject.DataCollector <- Guid.NewGuid().ToString()
    dc.SetValue(subject, Guid.NewGuid().ToString())
    subject.Verbosity <- "Verbose"

    let write =
      subject
        .GetType()
        .GetProperty("MessageIO", BindingFlags.Instance ||| BindingFlags.NonPublic)

    write.SetValue(subject, Some(fun (s: string) -> ()))

    Assert.That(subject.Execute(), Is.False)
    Assert.That(subject.Extended, Is.Empty)
    CommandLine.verbosity <- 0

  let template =
    """<?xml version="1.0" encoding="utf-8"?>
<RunSettings>
{1}  <InProcDataCollectionRunSettings>
    <InProcDataCollectors>
      <InProcDataCollector friendlyName="AltCover" uri="InProcDataCollector://AltCover/Recorder/1.0.0.0" assemblyQualifiedName="AltCover.DataCollector, {2}" codebase="{0}">
        <Configuration>
          <Offload>true</Offload>
        </Configuration>
      </InProcDataCollector>
    </InProcDataCollectors>
  </InProcDataCollectionRunSettings>
</RunSettings>"""

  [<Test>]
  let RunSettingsWorksIfOK () =
    Main.init ()
    let subject = MakeRunSettings()

    let dc =
      subject
        .GetType()
        .GetProperty("DataCollector", BindingFlags.Instance ||| BindingFlags.NonPublic)
    // subject.DataCollector <- Assembly.GetExecutingAssembly().Location
    dc.SetValue(subject, Assembly.GetExecutingAssembly().Location)

    let assembly =
      AssemblyName.GetAssemblyName
      <| Assembly.GetExecutingAssembly().Location

    let write =
      subject
        .GetType()
        .GetProperty("MessageIO", BindingFlags.Instance ||| BindingFlags.NonPublic)

    write.SetValue(subject, Some(fun (s: string) -> ()))
    Assert.That(subject.Execute(), Is.True)
    Assert.That(subject.Extended.EndsWith(".altcover.runsettings"))

    Assert.That(
      Path
        .GetFileName(subject.Extended)
        .StartsWith("altcover.test.")
    )

    let result =
      subject.Extended |> File.ReadAllText

    [ subject.Extended ]
    |> Seq.iter (fun f -> maybeIOException (fun () -> File.Delete f))

    Assert.That(
      result
        .Replace("\r", String.Empty)
        .Replace(
          "Collector://AltCover/Recorder/"
          + assembly.Version.ToString(),
          "Collector://AltCover/Recorder/1.0.0.0"
        ),
      Is.EqualTo(
        (String.Format(
          template,
          Assembly.GetExecutingAssembly().Location,
          String.Empty,
          Assembly.GetExecutingAssembly().FullName
        ))
          .Replace("\r", String.Empty)
      )
    )

  [<Test>]
  let RunSettingsExtendsOK () =
    Main.init ()
    let subject = MakeRunSettings()

    let dc =
      subject
        .GetType()
        .GetProperty("DataCollector", BindingFlags.Instance ||| BindingFlags.NonPublic)
    // subject.DataCollector <- Assembly.GetExecutingAssembly().Location
    dc.SetValue(subject, Assembly.GetExecutingAssembly().Location)

    let write =
      subject
        .GetType()
        .GetProperty("MessageIO", BindingFlags.Instance ||| BindingFlags.NonPublic)

    write.SetValue(subject, Some(fun (s: string) -> ()))

    let assembly =
      AssemblyName.GetAssemblyName
      <| Assembly.GetExecutingAssembly().Location

    let settings = Path.GetTempFileName()
    File.WriteAllText(settings, "<RunSettings><stuff /></RunSettings>")
    subject.TestSetting <- settings
    Assert.That(subject.Execute(), Is.True)
    Assert.That(subject.Extended.EndsWith(".altcover.runsettings"))

    let result =
      subject.Extended |> File.ReadAllText

    [ subject.Extended ]
    |> Seq.iter (fun f -> maybeIOException (fun () -> File.Delete f))

    Assert.That(
      result
        .Replace("\r", String.Empty)
        .Replace(
          "Collector://AltCover/Recorder/"
          + assembly.Version.ToString(),
          "Collector://AltCover/Recorder/1.0.0.0"
        ),
      Is.EqualTo(
        (String.Format(
          template,
          Assembly.GetExecutingAssembly().Location,
          "  <stuff />\r\n",
          Assembly.GetExecutingAssembly().FullName
        ))
          .Replace("\r", String.Empty)
      )
    )

  [<Test>]
  let RunSettingsThrowsIfUninitialized () =
    Main.init ()
    let subject = MakeRunSettings()

    let dc =
      subject
        .GetType()
        .GetProperty("DataCollector", BindingFlags.Instance ||| BindingFlags.NonPublic)
    // subject.DataCollector <- Assembly.GetExecutingAssembly().Location
    dc.SetValue(subject, Assembly.GetExecutingAssembly().Location)

    let assembly =
      AssemblyName.GetAssemblyName
      <| Assembly.GetExecutingAssembly().Location

    let settings = Path.GetTempFileName()
    File.WriteAllText(settings, "<RunSettings><stuff /></RunSettings>")
    subject.TestSetting <- settings

    Assert.Throws<InvalidOperationException>(fun () -> subject.Execute() |> ignore)
    |> ignore

  [<Test>]
  let RunSettingsRecoversOK () =
    Main.init ()
    let subject = MakeRunSettings()

    let dc =
      subject
        .GetType()
        .GetProperty("DataCollector", BindingFlags.Instance ||| BindingFlags.NonPublic)
    // subject.DataCollector <- Assembly.GetExecutingAssembly().Location
    dc.SetValue(subject, Assembly.GetExecutingAssembly().Location)
    //let write = subject.GetType().GetProperty("MessageIO", BindingFlags.Instance ||| BindingFlags.NonPublic)
    //write.SetValue(subject, Some (fun (s:string) -> ()))
    subject.Verbosity <- "Off"

    let assembly =
      AssemblyName.GetAssemblyName
      <| Assembly.GetExecutingAssembly().Location

    let settings = Path.GetTempFileName()
    File.WriteAllText(settings, "<Not XML")
    subject.TestSetting <- settings
    Assert.That(subject.Execute(), Is.True)
    Assert.That(subject.Extended.EndsWith(".altcover.runsettings"))

    let result =
      subject.Extended |> File.ReadAllText

    [ subject.Extended ]
    |> Seq.iter (fun f -> maybeIOException (fun () -> File.Delete f))

    Assert.That(
      result
        .Replace("\r", String.Empty)
        .Replace(
          "Collector://AltCover/Recorder/"
          + assembly.Version.ToString(),
          "Collector://AltCover/Recorder/1.0.0.0"
        ),
      Is.EqualTo(
        (String.Format(
          template,
          Assembly.GetExecutingAssembly().Location,
          String.Empty,
          Assembly.GetExecutingAssembly().FullName
        ))
          .Replace("\r", String.Empty)
      )
    )

  [<Test>]
  let ContingentCopyTest () =
    Main.init ()
    let subject = ContingentCopy()
    let unique = Guid.NewGuid().ToString()

    let where =
      Assembly.GetExecutingAssembly().Location
      |> Path.GetDirectoryName

    let relative = where |> Path.GetFileName

    subject.BuildOutputDirectory <- where |> Path.GetDirectoryName
    subject.InstrumentDirectory <- Path.Combine(where, unique)
    test <@ subject.Execute() @>

    test
      <@
        subject.InstrumentDirectory
        |> Directory.Exists
        |> not
      @>

    subject.FileName <- "Sample2.pdb"

    let from =
      Path.Combine(subject.BuildOutputDirectory, relative, subject.FileName)

    test <@ from |> File.Exists @>

    test
      <@
        subject.InstrumentDirectory
        |> Directory.Exists
        |> not
      @>

    test <@ subject.Execute() @>

    test
      <@
        subject.InstrumentDirectory
        |> Directory.Exists
        |> not
      @>

    subject.RelativeDir <- relative
    test <@ subject.Execute() @>

    test
      <@
        subject.InstrumentDirectory
        |> Directory.Exists
        |> not
      @>

    subject.RelativeDir <- String.Empty
    subject.CopyToOutputDirectory <- "Always"
    test <@ subject.Execute() @>

    test
      <@
        subject.InstrumentDirectory
        |> Directory.Exists
        |> not
      @>

    subject.RelativeDir <- where
    test <@ subject.Execute() @>

    test
      <@
        subject.InstrumentDirectory
        |> Directory.Exists
        |> not
      @>

    subject.RelativeDir <- relative
    test <@ subject.Execute() @>

    let target =
      Path.Combine(subject.InstrumentDirectory, relative, subject.FileName)

    test <@ target |> File.Exists @>

    subject.FileName <- "Foo.txt"

    let projectdir =
      Path.Combine(SolutionRoot.location, "Samples/Sample4")

    let builddir =
      Path.Combine(
        SolutionRoot.location,
#if !NET472
        "_Binaries/Sample4/Debug+AnyCPU/net8.0"
      )
#else
        "_Binaries/Sample4/Debug+AnyCPU/net472"
      )
#endif

    subject.RelativeDir <- Path.Combine(projectdir, "Data/Deeper")
    subject.BuildOutputDirectory <- builddir
    subject.ProjectDir <- projectdir

    let target2 =
      Path.Combine(subject.InstrumentDirectory, "Data/Deeper/Foo.txt")

    test
      <@
        Path.Combine(builddir, "Data/Deeper/Foo.txt")
        |> File.Exists
      @>

    test <@ target2 |> File.Exists |> not @>
    test <@ subject.Execute() @>
    test <@ target2 |> File.Exists @>

    let write =
      subject
        .GetType()
        .GetMethod("Write", BindingFlags.NonPublic ||| BindingFlags.Instance)

    let ex =
      Assert.Throws<TargetInvocationException>(fun () ->
        write.Invoke(subject, [| "xx" |]) |> ignore)

    test <@ ex.InnerException.GetType().FullName = "System.InvalidOperationException" @>

  [<Test>]
  let RetryDeleteTest () =
    Main.init ()
    let subject = RetryDelete()
    let unique = Guid.NewGuid().ToString()

    let where =
      Assembly.GetExecutingAssembly().Location
      |> Path.GetDirectoryName

    let from =
      Path.Combine(where, "Sample2.pdb")

    let target = Path.Combine(where, unique)
    File.Copy(from, target, true)
    test <@ File.Exists target @>
    subject.Files <- [| target |]
    test <@ subject.Execute() @>
    test <@ target |> File.Exists |> not @>

    [ IOException() :> Exception
      System.Security.SecurityException() :> Exception
      UnauthorizedAccessException() :> Exception ]
    |> List.iter (fun ex ->
      let builder = System.Text.StringBuilder()

      CommandLine.I.doRetry (fun _ -> raise ex) (builder.Append >> ignore) 2 0 0 target

      test
        <@
          builder
            .ToString()
            .StartsWith(ex.GetType().FullName, StringComparison.Ordinal)
        @>)

    let builder = System.Text.StringBuilder()
    let monitor (s: string) = s |> builder.Append |> ignore

    Assert.Throws<InvalidDataException>(fun () ->
      CommandLine.I.doRetry
        (fun _ -> raise <| InvalidDataException())
        monitor
        2
        0
        0
        target)
    |> ignore

    test <@ builder.ToString() |> String.IsNullOrEmpty @>

    monitor ("Hello")
    test <@ builder.ToString() = "Hello" @>

    let write0 =
      subject
        .GetType()
        .GetMethod("Write0", BindingFlags.NonPublic ||| BindingFlags.Instance)

    let mutable written = "written"

    write0.Invoke(
      subject,
      [| (fun x -> written <- x)
         "xx"
         "yy" |]
    )
    |> ignore

    test <@ written = "Failed to delete file xx" @>

    let write =
      subject
        .GetType()
        .GetMethod("Write", BindingFlags.NonPublic ||| BindingFlags.Instance)

    let ex =
      Assert.Throws<TargetInvocationException>(fun () ->
        write.Invoke(subject, [| "xx"; "yy" |]) |> ignore)

    test <@ ex.InnerException.GetType().FullName = "System.InvalidOperationException" @>
// Recorder.fs => Recorder.Tests