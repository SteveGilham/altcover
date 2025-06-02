namespace Tests
// fsharplint:disable  MemberNames NonPublicValuesNames RedundantNewKeyword

open System
open System.IO
open System.Reflection
open System.Text.RegularExpressions

open AltCover

#nowarn "25"

module Arguments =
  [<Test>]
  let NullListsAreEmpty () =
    let subject =
      Args.itemList String.Empty null

    test <@ subject |> List.isEmpty @>

  [<Test>]
  let CollectOptionsCanBeValidated () =
    CommandLine.error <- []

    let subject =
      { Primitive.CollectOptions.Create() with
          Threshold = "23"
          Verbosity = System.Diagnostics.TraceLevel.Error
          CommandLine = null }

    let instance =
      AltCover.CollectOptions.Primitive subject

    let scan = instance.Validate(false)
    test <@ scan.Length = 0 @>
    test <@ (instance.GetHashCode() :> obj).IsNotNull @>

    test
      <@
        instance |> Args.collect = [ "Runner"
                                     "-t"
                                     "23"
                                     "--collect"
                                     "-q"
                                     "-q" ]
      @>
    // hack
    let rerun =
      AltCover.CollectOptions.Abstract instance

    let scan = rerun.Validate(false)
    test <@ scan.Length = 0 @>
    test <@ (rerun.GetHashCode() :> obj).IsNotNull @>

    test
      <@
        rerun |> Args.collect = [ "Runner"
                                  "-t"
                                  "23"
                                  "--collect"
                                  "-q"
                                  "-q" ]
      @>

  [<Test>]
  let TypeSafeEmptyThresholdCanBeValidated () =
    let empty =
      TypeSafe.Threshold <| TypeSafe.Thresholds.Create()

    test <@ empty.AsString() = String.Empty @>

  [<Test>]
  let TypeSafeCollectOptionsCanBeValidated () =
    CommandLine.error <- []

    let here =
      Assembly.GetExecutingAssembly().Location

    let t =
      { TypeSafe.Thresholds.Create() with
          Statements = 23uy
          Branches = 16uy
          Methods = 7uy
          MaxCrap = 3uy }

    let subject =
      { TypeSafe.CollectOptions.Create() with
          Threshold = TypeSafe.Threshold t
          SummaryFormat = TypeSafe.BPlus
          Verbosity = System.Diagnostics.TraceLevel.Verbose
          Packages = TypeSafe.Packages [ TypeSafe.Package "/agent/" ]
          Executable = TypeSafe.Tool "dotnet" }

    let instance =
      AltCover.CollectOptions.TypeSafe subject

    test <@ (instance.GetHashCode() :> obj).IsNotNull @>

    let scan = instance.Validate(false)
    test <@ scan.Length = 0 @>

    test
      <@
        instance |> Args.collect = [ "Runner"
                                     "-x"
                                     "dotnet"
                                     "-t"
                                     "S23B16M7C3"
                                     "-p"
                                     "/agent/"
                                     "--summary:BOC"
                                     "--verbose" ]
      @>

    let validate = instance.WhatIf(false)
    test <@ (validate.GetHashCode() :> obj).IsNotNull @>

    test
      <@
        validate.ToString() = "altcover Runner -x dotnet -t S23B16M7C3 -p /agent/ --summary:BOC --verbose"
      @>

  [<Test>]
  let TypeSafeCollectSummaryCanBeValidated () =
    let inputs =
      [ TypeSafe.Default
        TypeSafe.B
        TypeSafe.BPlus
        TypeSafe.R
        TypeSafe.RPlus
        TypeSafe.C
        TypeSafe.N
        TypeSafe.O
        TypeSafe.Many
          [ TypeSafe.BPlus
            TypeSafe.R
            TypeSafe.O ] ]

    let expected =
      [ String.Empty
        "B"
        "BOC"
        "R"
        "ROC"
        "C"
        "N"
        "O"
        "BOCR" ]

    inputs
    |> List.map _.AsString()
    |> List.zip expected
    |> List.iter (fun (a, b) -> test <@ a = b @>)

  [<Test>]
  let CollectOptionsCanBeValidatedWithErrors () =
    CommandLine.error <- []

    let subject =
      Primitive.CollectOptions.Create()

    let scan =
      (AltCover.CollectOptions.Primitive subject).Validate(true)

    test <@ scan.Length = 1 @>

  [<Test>]
  let TypeSafeCollectOptionsCanBeValidatedWithErrors () =
    CommandLine.error <- []

    let subject =
      TypeSafe.CollectOptions.Create()

    let instance =
      AltCover.CollectOptions.TypeSafe subject

    let scan = instance.Validate(true)

    test <@ instance |> Args.collect = [ "Runner"; "--collect" ] @>

    test <@ scan.Length = 1 @>

  [<Test>]
  let CollectOptionsCanBePositivelyValidatedWithErrors () =
    CommandLine.error <- []

    let test =
      { Primitive.CollectOptions.Create() with
          RecorderDirectory = Guid.NewGuid().ToString() }

    let instance =
      AltCover.CollectOptions.Primitive test

    let scan = instance.Validate(true)

    test' <@ scan.Length = 2 @>
    <| String.Join(Environment.NewLine, scan)

    // hack
    let rerun =
      AltCover.CollectOptions.Abstract instance

    let scan = rerun.Validate(true)

    test' <@ scan.Length = 2 @>
    <| String.Join(Environment.NewLine, scan)

  [<Test>]
  let TypeSafeCollectOptionsCanBePositivelyValidatedWithErrors () =
    CommandLine.error <- []

    let test =
      { TypeSafe.CollectOptions.Create() with
          RecorderDirectory =
            TypeSafe.DInfo
            <| DirectoryInfo(Guid.NewGuid().ToString()) }

    let scan =
      (AltCover.CollectOptions.TypeSafe test).Validate(true)

    test' <@ scan.Length = 2 @>
    <| String.Join(Environment.NewLine, scan)

  [<Test>]
  let PrepareOptionsCanBeValidated () =
    let here =
      Assembly.GetExecutingAssembly().Location
      |> Path.GetDirectoryName

    let subject =
      { Primitive.PrepareOptions.Create() with
          InputDirectories = [| here |]
          OutputDirectories = [| here |]
          SymbolDirectories = [| here |]
          InPlace = true
          Dependencies = [| Assembly.GetExecutingAssembly().Location |]
          CallContext = [| "[Fact]" |]
          PathFilter = [| "ok" |] }

    let instance =
      AltCover.PrepareOptions.Primitive subject

    let scan = instance.Validate()
    test <@ scan.Length = 0 @>
    test <@ (instance.GetHashCode() :> obj).IsNotNull @>
    let rendered = instance |> Args.prepare

    let location =
      Assembly.GetExecutingAssembly().Location

    test
      <@
        rendered = [ "-i"
                     here
                     "-o"
                     here
                     "-y"
                     here
                     "-d"
                     location
                     "-p"
                     "ok"
                     "-c"
                     "[Fact]"
                     "--reportFormat"
                     "OpenCover"
                     "--inplace"
                     "--save" ]
      @>

    // hack
    let rerun =
      AltCover.PrepareOptions.Abstract instance

    let scan = rerun.Validate()
    test <@ scan.Length = 0 @>
    test <@ (rerun.GetHashCode() :> obj).IsNotNull @>
    let rendered = rerun |> Args.prepare

    test
      <@
        rendered = [ "-i"
                     here
                     "-o"
                     here
                     "-y"
                     here
                     "-d"
                     location
                     "-p"
                     "ok"
                     "-c"
                     "[Fact]"
                     "--reportFormat"
                     "OpenCover"
                     "--inplace"
                     "--save" ]
      @>

  [<Test>]
  let TypeSafePrepareOptionsCanBeValidated () =
    let here =
      Assembly.GetExecutingAssembly().Location
      |> Path.GetDirectoryName

    test <@ (TypeSafe.Tool ".").AsString() = "." @>
    test <@ (TypeSafe.FilePath ".").AsString() = ("." |> Path.GetFullPath) @>

    test <@ ("fred" |> Regex |> TypeSafe.NegateMatchItem).AsString() = "?fred" @>

    let subject =
      { TypeSafe.PrepareOptions.Create() with
          InputDirectories = TypeSafe.DirectoryPaths [| TypeSafe.DirectoryPath here |]
          OutputDirectories =
            TypeSafe.DirectoryPaths [| TypeSafe.DInfo(DirectoryInfo(here)) |]
          SymbolDirectories = TypeSafe.DirectoryPaths [| TypeSafe.DirectoryPath here |]
          Dependencies =
            TypeSafe.FilePaths
              [| TypeSafe.FilePath
                 <| Assembly.GetExecutingAssembly().Location |]
          CallContext =
            TypeSafe.Context
              [| TypeSafe.AttributeName "Fact"
                 TypeSafe.AttributeKind typeof<SerializableAttribute>
                 TypeSafe.Caller(
                   Assembly
                     .GetExecutingAssembly()
                     .GetType("Tests.Arguments")
                     .GetMethod("TypeSafePrepareOptionsCanBeValidated")
                 )
                 TypeSafe.CallerName "Test" |]
          MethodPoint = TypeSafe.Set
          InPlace = TypeSafe.Set
          PathFilter = TypeSafe.Unfiltered.Join [| TypeSafe.Raw "ok" |] }

    let instance =
      AltCover.PrepareOptions.TypeSafe subject

    test <@ (instance.GetHashCode() :> obj).IsNotNull @>

    let scan = instance.Validate()
    test <@ scan.Length = 0 @>

    let location =
      Assembly.GetExecutingAssembly().Location

    let arglist = instance |> Args.prepare

    let expected =
      [ "-i"
        here
        "-o"
        here
        "-y"
        here
        "-d"
        location
        "-p"
        "ok"
        "-c"
        "[Fact]"
        "-c"
        "[System.SerializableAttribute]"
        "-c"
        "Tests.Arguments.TypeSafePrepareOptionsCanBeValidated"
        "-c"
        "Test"
        "--reportFormat"
        "OpenCover"
        "--inplace"
        "--save"
        "--methodpoint" ]

    //List.zip arglist expected
    //|> List.iter (fun (a, b) -> Assert.That(a, Is.EqualTo b))

    test <@ arglist = expected @>

    let validate =
      (AltCover.PrepareOptions.TypeSafe subject).WhatIf().ToString()

    test
      <@
        validate = "altcover -i "
                   + here
                   + " -o "
                   + here
                   + " -y "
                   + here
                   + " -d "
                   + location
                   + " -p ok -c [Fact] -c [System.SerializableAttribute] -c "
                   + "Tests.Arguments.TypeSafePrepareOptionsCanBeValidated -c Test --reportFormat OpenCover --inplace --save --methodpoint"
      @>

  [<Test>]
  let TypeSafePrepareOptionsCanBeValidatedAgain () =
    let here =
      Assembly.GetExecutingAssembly().Location
      |> Path.GetDirectoryName

    let subject =
      { TypeSafe.PrepareOptions.Create() with
          InputDirectories = TypeSafe.DirectoryPaths [| TypeSafe.DirectoryPath here |]
          OutputDirectories =
            TypeSafe.DirectoryPaths [| TypeSafe.DInfo(DirectoryInfo(here)) |]
          SymbolDirectories = TypeSafe.DirectoryPaths [| TypeSafe.DirectoryPath here |]
          Dependencies =
            TypeSafe.FilePaths
              [| TypeSafe.FilePath
                 <| Assembly.GetExecutingAssembly().Location |]
          CommandLine = TypeSafe.CommandArguments [| TypeSafe.CommandArgument "[Fact]" |]
          ReportFormat = TypeSafe.ReportFormat.NCover
          PathFilter = (TypeSafe.Filters [| TypeSafe.MatchItem <| Regex "ok" |]).Join [] }

    let scan =
      (AltCover.PrepareOptions.TypeSafe subject).Validate()

    test <@ scan.Length = 0 @>

    let location =
      Assembly.GetExecutingAssembly().Location

    test
      <@
        (AltCover.PrepareOptions.TypeSafe subject)
        |> Args.prepare = [ "-i"
                            here
                            "-o"
                            here
                            "-y"
                            here
                            "-d"
                            location
                            "-p"
                            "ok"
                            "--reportFormat"
                            "NCover"
                            "--save"
                            "--"
                            "[Fact]" ]
      @>

  [<Test>]
  let PrepareOptionsStrongNamesCanBeValidated () =
    let input =
      Path.Combine(AltCover.SolutionRoot.location, "Build/Infrastructure.snk")

    let subject =
      { Primitive.PrepareOptions.Create() with
          StrongNameKey = input
          Keys = [| input |] }

    let scan =
      (AltCover.PrepareOptions.Primitive subject).Validate()

    test <@ scan.Length = 0 @>

  [<Test>]
  let TypeSafePrepareOptionsStrongNamesCanBeValidated () =
    let input =
      Path.Combine(AltCover.SolutionRoot.location, "Build/Infrastructure.snk")

    let subject =
      { TypeSafe.PrepareOptions.Create() with
          StrongNameKey = TypeSafe.FInfo <| FileInfo(input)
          Keys = TypeSafe.FilePaths [| TypeSafe.FilePath input |] }

    let scan =
      (AltCover.PrepareOptions.TypeSafe subject).Validate()

    test <@ scan.Length = 0 @>

  [<Test>]
  let PrepareOptionsCanBeValidatedWithNulls () =
    let subject =
      { Primitive.PrepareOptions.Create() with
          CallContext = null }

    let scan =
      (AltCover.PrepareOptions.Primitive subject).Validate()

    test <@ scan.Length = 0 @>

  [<Test>]
  let PrepareOptionsCanBeValidatedAndDetectInconsistency () =
    let subject =
      { Primitive.PrepareOptions.Create() with
          BranchCover = true
          LineCover = true
          All = true
          CallContext = [| "0" |] }

    let scan =
      (AltCover.PrepareOptions.Primitive subject).Validate()

    test <@ scan.Length = 1 @>

  [<Test>]
  let TypeSafePrepareOptionsCanBeValidatedAndDetectInconsistency () =
    let subject =
      { TypeSafe.PrepareOptions.Create() with
          BranchCover = TypeSafe.Flag true
          LineCover = TypeSafe.Flag true
          All = TypeSafe.Flag true
          CallContext = TypeSafe.Context [| TypeSafe.TimeItem 0uy |] }
      |> AltCover.PrepareOptions.TypeSafe

    let scan = subject.Validate()
    test <@ scan.Length = 1 @>
    let rendered = subject |> Args.prepare

    test
      <@
        rendered = [ "-c"
                     "0"
                     "--reportFormat"
                     "OpenCover"
                     "--save"
                     "--all"
                     "--linecover"
                     "--branchcover" ]
      @>

  [<Test>]
  let TypeSafePrepareStaticCanBeValidated () =
    let inputs =
      [ TypeSafe.StaticFormat.Default
        TypeSafe.StaticFormat.Show
        TypeSafe.StaticFormat.ShowZero ]

    let expected = [ "-"; "+"; "++" ]
    test <@ inputs |> List.map _.AsString() = expected @>

  [<Test>]
  let PrepareOptionsCanBeValidatedWithErrors () =
    let subject =
      { Primitive.PrepareOptions.Create() with
          Report = String(Path.GetInvalidPathChars())
          CallContext = [| "0"; "1" |] }

    let scan =
      (AltCover.PrepareOptions.Primitive subject).Validate()

    test <@ scan.Length = 2 @>