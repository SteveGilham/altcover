namespace Tests
// fsharplint:disable  MemberNames NonPublicValuesNames

open System
open System.IO
open System.Reflection

open AltCover
open Mono.Options
open System.Security.Cryptography
open System.Security

#nowarn "25"

module CommandLine =

  [<Test>]
  let VerbosityShouldBeHonoured () =
    let saved = (Console.Out, Console.Error)
    let e0 = Console.Out.Encoding
    let e1 = Console.Error.Encoding

    let expected =
      [ [ true; true; true; true; true ], "info|warn", "echo|error||or|  ImportModule"
        [ false; false; true; true; true ], "warn", "error||or|  ImportModule"
        [ false; false; false; true; true ], String.Empty, "error||or|  ImportModule"
        [ false; false; false; false; false ], String.Empty, String.Empty
        [ false; false; false; false; false ], String.Empty, String.Empty ]

    try
      expected
      |> Seq.iteri (fun verbosity (expect, toOut, toErr) ->
        CommandLine.toConsole ()

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

        let first =
          [ Output.info :> obj
            Output.echo :> obj
            Output.warn :> obj
            Output.error :> obj
            Output.usage :> obj ]

        CommandLine.verbosity <- verbosity
        CommandLine.applyVerbosity ()

        Output.info "info"
        Output.echo "echo"
        Output.warn "warn"
        Output.error "error"

        Output.usage
          { Intro = "intro"
            Options = Mono.Options.OptionSet()
            Options2 = Mono.Options.OptionSet() }

        test
          <@
            [ Output.info :> obj
              Output.echo :> obj
              Output.warn :> obj
              Output.error :> obj
              Output.usage :> obj ]
            |> List.zip first
            |> List.map (fun (a, b) -> Object.ReferenceEquals(a, b)) = expect
          @>

        test <@ stdout.ToString().Trim().Replace(Environment.NewLine, "|") = toOut @>

        if toErr.Length = 0 then
          test <@ stderr.ToString().Length = 0 @>
        else
          test
            <@
              stderr
                .ToString()
                .Trim()
                .Replace(Environment.NewLine, "|")
                .StartsWith(toErr, StringComparison.Ordinal)
            @>

      )
    finally
      CommandLine.toConsole ()
      Output.verbose <- ignore
      CommandLine.verbosity <- 0
      Console.SetOut(fst saved)
      Console.SetError(snd saved)

  [<Test>]
  let StrongNameKeyCanBeValidated () =
    let input =
      Path.Combine(AltCover.SolutionRoot.location, "Build/Infrastructure.snk")

    let (pair, ok) =
      CommandLine.validateStrongNameKey "key" input

    Assert.That(ok, Is.True, "Strong name is OK")
    Assert.That(pair, Is.Not.Null)
    Assert.That(pair.PublicKey, Is.Not.Null)

    Assert.That(
      CommandLine.validateStrongNameKey "key" (String(Path.GetInvalidPathChars())),
      Is.EqualTo((StrongNameKeyData.Empty(), false))
    )

    Assert.That(
      CommandLine.validateStrongNameKey "key"
      <| Assembly.GetExecutingAssembly().Location,
      Is.EqualTo((StrongNameKeyData.Empty(), false))
    )

  [<Test>]
  let CryptographicExceptionIsTransformed () =
    let unique = Guid.NewGuid().ToString()

    let raiser =
      fun x -> Maybe x (unique |> CryptographicException |> raise) ()

    let arg = fun () -> raiser true

    let arranged =
      fun () -> CommandLine.I.transformCryptographicException arg

    let ex =
      Assert.Throws<SecurityException>(fun () -> arranged ())

    Assert.That(ex.Message, Is.EqualTo unique)
    Assert.That(ex.InnerException, Is.InstanceOf<CryptographicException>())

  [<Test>]
  let OutputCanBeExercised () =
    let sink = StringSink(ignore)
    let setInfo (x: StringSink) = Output.info <- x.Invoke
    let setError (x: StringSink) = Output.error <- x.Invoke
    let setWarn (x: StringSink) = Output.warn <- x.Invoke

    setInfo sink
    setError sink
    setWarn sink
    Output.echo <- ignore
    Output.usage <- ignore
    Output.echo "echo"

    Output.usage
      { Intro = "usage"
        Options = OptionSet()
        Options2 = OptionSet() }

    Assert.That(Output.usage, Is.Not.Null)

    typeof<SummaryFormat>.Assembly.GetTypes()
    |> Seq.filter (fun t ->
      (string t = "AltCover.Output")
      || (string t = "AltCover.AltCover"))
    |> Seq.collect _.GetNestedTypes(BindingFlags.NonPublic)
    |> Seq.filter (fun t ->
      let tokens =
        [ "info"
          "echo"
          "error"
          "usage"
          "warn"
          "toConsole" ]

      let name = t.Name
      tokens |> List.exists name.StartsWith)
    |> Seq.iter (fun t ->
      let p =
        t.GetType().GetProperty("DeclaredConstructors")

      let c =
        p.GetValue(t, null) :?> ConstructorInfo[]

      let c0 = c |> Seq.head
      let p = c0.GetParameters().Length

      let o =
        c0.Invoke(Maybe (p = 0) null [| sink |])

      let invoke = t.GetMethod("Invoke")

      let param =
        invoke.GetParameters() |> Seq.head

      let arg: obj =
        if param.ParameterType = typeof<String> then
          String.Empty :> obj
        else
          { Intro = String.Empty
            Options = OptionSet()
            Options2 = OptionSet() }
          :> obj

      invoke.Invoke(o, [| arg |]) |> ignore)

    setWarn sink
    setError sink |> ignore
    setInfo sink |> ignore
    Output.warn "warn"
    Output.error "error"
    Output.info "info"

  [<Test>]
  let NoThrowNoErrorLeavesAllOK () =
    try
      CommandLine.error <- []
      CommandLine.exceptions <- []
      CommandLine.doPathOperation ignore () true
      Assert.That(CommandLine.error, Is.Empty)
      Assert.That(CommandLine.exceptions, Is.Empty)
    finally
      CommandLine.error <- []
      CommandLine.exceptions <- []

  [<Test>]
  let NoThrowWithErrorIsSignalled () =
    try
      CommandLine.error <- []
      CommandLine.exceptions <- []

      CommandLine.doPathOperation
        (fun () -> CommandLine.error <- [ "NoThrowWithErrorIsSignalled" ])
        ()
        true

      Assert.That(CommandLine.error, Is.Not.Empty)
      Assert.That(CommandLine.exceptions, Is.Empty)
    finally
      CommandLine.error <- []
      CommandLine.exceptions <- []

  [<Test>]
  let ArgumentExceptionWrites () =
    let saved = (Output.info, Output.error)
    let err = System.Text.StringBuilder()
    let info = System.Text.StringBuilder()

    let f1 =
      fun (s: String) -> info.Append(s).Append("|") |> ignore

    let f2 =
      fun (s: String) -> err.Append(s).Append("|") |> ignore

    f1 "f1"
    Assert.That(info.ToString(), Is.Not.Empty)
    info.Clear() |> ignore
    Assert.That(info.ToString(), Is.Empty)

    f2 "f2"
    Assert.That(err.ToString(), Is.Not.Empty)
    err.Clear() |> ignore
    Assert.That(err.ToString(), Is.Empty)

    try
      Output.info <- f1
      Output.error <- f2

      let unique =
        "ArgumentException " + Guid.NewGuid().ToString()

      CommandLine.error <- []
      CommandLine.exceptions <- []

      CommandLine.doPathOperation
        (fun () ->
          let inner = InvalidOperationException()
          ArgumentException(unique, inner) |> raise)
        ()
        true

      Assert.That(CommandLine.error, Is.EquivalentTo [ unique ])

      Assert.That(
        CommandLine.exceptions |> List.map _.Message,
        Is.EquivalentTo [ unique ]
      )

      let here =
        Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)

      let there =
        Path.Combine(here, Guid.NewGuid().ToString())

      let toInfo = Directory.CreateDirectory there
      CoverageParameters.theOutputDirectories.Clear()
      CoverageParameters.theInputDirectories.Clear()
      CoverageParameters.theOutputDirectories.Add toInfo.FullName
      CoverageParameters.theInputDirectories.Add here
      Assert.That(info.ToString(), Is.Empty)
      Assert.That(err.ToString(), Is.Empty)
      let name = "ArgumentExceptionWrites"
      CommandLine.I.logExceptionsToFile name false

      let target =
        Path.Combine(toInfo.FullName, name)

      let target' = Path.Combine(here, name)
      Assert.That(File.Exists target, target)

      let lines' =
        target |> File.ReadAllLines |> Seq.toList

      let head = lines' |> List.head
#if !NET472
      Assert.That(head.Length, Is.LessThanOrEqualTo 80)

      let lines =
        let t = lines' |> List.tail
        (head + List.head t) :: (List.tail t)
#else
      Assert.That(head.Length, Is.GreaterThan 80)
      let lines = lines'
#endif

      Assert.That(
        lines.[0],
        Is.EqualTo(
          "System.ArgumentException: "
          + unique
          + " ---> System.InvalidOperationException: Operation is not valid due to the current state of the object."
        ),
        sprintf "lines = %A" lines
      )

      Assert.That(
        lines.[1],
        Does.StartWith("   --- End of inner exception stack trace ---")
      )

      Assert.That(
        lines.[2].Replace("+", ".").Trim(),
        Does.StartWith("at Tests.CommandLine.ArgumentExceptionWrites@")
      )

      Assert.That(
        lines.[3].Trim().Replace("Line+I.doPath", "Line.I.doPath"),
        Does.StartWith("at AltCover.PathOperation.DoPathOperation")
      )

      Assert.That(lines |> List.skip 4, Is.Not.Empty)
      Assert.That(info.ToString(), Is.Empty)
      Assert.That(err.ToString().Trim(), Is.EqualTo("Details written to " + target + "|"))
    finally
      CommandLine.error <- []
      CommandLine.exceptions <- []
      Output.info <- (fst saved)
      Output.error <- (snd saved)
      CoverageParameters.theOutputDirectories.Clear()

  [<Test>]
  let ArgumentExceptionWritesEx () =
    let saved = (Output.info, Output.error)
    let err = System.Text.StringBuilder()
    let info = System.Text.StringBuilder()

    let f1 =
      fun (s: String) -> info.Append(s).Append("|") |> ignore

    let f2 =
      fun (s: String) -> err.Append(s).Append("|") |> ignore

    f1 "f1"
    Assert.That(info.ToString(), Is.Not.Empty)
    info.Clear() |> ignore
    Assert.That(info.ToString(), Is.Empty)

    f2 "f2"
    Assert.That(err.ToString(), Is.Not.Empty)
    err.Clear() |> ignore
    Assert.That(err.ToString(), Is.Empty)

    try
      Output.info <- f1
      Output.error <- f2

      let unique =
        "ArgumentException " + Guid.NewGuid().ToString()

      CommandLine.error <- []
      CommandLine.exceptions <- []

      CommandLine.doPathOperation
        (fun () ->
          let inner = InvalidOperationException()
          ArgumentException(unique, inner) |> raise)
        ()
        true

      Assert.That(CommandLine.error, Is.EquivalentTo [ unique ])

      Assert.That(
        CommandLine.exceptions |> List.map _.Message,
        Is.EquivalentTo [ unique ]
      )

      let here =
        Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)

      let there =
        Path.Combine(here, Guid.NewGuid().ToString())

      let toInfo = Directory.CreateDirectory there
      CoverageParameters.theOutputDirectories.Clear()
      CoverageParameters.theInputDirectories.Clear()
      CoverageParameters.theOutputDirectories.Add toInfo.FullName
      CoverageParameters.theInputDirectories.Add here
      Assert.That(info.ToString(), Is.Empty)
      Assert.That(err.ToString(), Is.Empty)
      let name = "ArgumentExceptionWrites"
      CommandLine.I.logExceptionsToFile name true

      let target =
        Path.Combine(toInfo.FullName, name)

      let target' = Path.Combine(here, name)
      Assert.That(File.Exists target, target)

      let lines' =
        target |> File.ReadAllLines |> Seq.toList

      let head = lines' |> List.head
#if !NET472
      Assert.That(head.Length, Is.LessThanOrEqualTo 80)

      let lines =
        let t = lines' |> List.tail
        (head + List.head t) :: (List.tail t)
#else
      Assert.That(head.Length, Is.GreaterThan 80)
      let lines = lines'
#endif

      Assert.That(
        lines.[0],
        Is.EqualTo(
          "System.ArgumentException: "
          + unique
          + " ---> System.InvalidOperationException: Operation is not valid due to the current state of the object."
        ),
        sprintf "lines = %A" lines
      )

      Assert.That(
        lines.[1],
        Does.StartWith("   --- End of inner exception stack trace ---")
      )

      Assert.That(
        lines.[2].Replace("+", ".").Trim(),
        Does.StartWith("at Tests.CommandLine.ArgumentExceptionWritesEx@")
      )

      Assert.That(
        lines.[3].Trim().Replace("Line+I.doPath", "Line.I.doPath"),
        Does.StartWith("at AltCover.PathOperation.DoPathOperation")
      )

      Assert.That(lines |> List.skip 4, Is.Not.Empty)
      Assert.That(info.ToString(), Is.Empty)

      Assert.That(
        err.ToString().Trim().Replace("\r", String.Empty).Replace("\n", "|"),
        Is.EqualTo(
          "Details written to "
          + target
          + "|If this problem was detected in the pre-test instrumentation stage of `dotnet test`, then the file may have been moved to "
          + target'
          + " when the task completes.|"
        )
      )
    finally
      CommandLine.error <- []
      CommandLine.exceptions <- []
      Output.info <- (fst saved)
      Output.error <- (snd saved)
      CoverageParameters.theOutputDirectories.Clear()

  [<Test>]
  let IOExceptionWrites () =
    let saved = (Output.info, Output.error)
    let err = System.Text.StringBuilder()
    let info = System.Text.StringBuilder()

    let f1 =
      fun (s: String) -> info.Append(s).Append("|") |> ignore

    let f2 =
      fun (s: String) -> err.Append(s).Append("|") |> ignore

    f1 "f1"
    Assert.That(info.ToString(), Is.Not.Empty)
    info.Clear() |> ignore
    Assert.That(info.ToString(), Is.Empty)

    f2 "f2"
    Assert.That(err.ToString(), Is.Not.Empty)
    err.Clear() |> ignore
    Assert.That(err.ToString(), Is.Empty)

    try
      Output.info <- f1
      Output.error <- f2

      let unique =
        "IOException " + Guid.NewGuid().ToString()

      CommandLine.error <- []
      CommandLine.exceptions <- []
      CommandLine.doPathOperation (fun () -> IOException(unique) |> raise) () false
      Assert.That(CommandLine.error, Is.EquivalentTo [ unique ])
      CommandLine.reportErrors "Instrumentation" false
      Assert.That(info.ToString(), Is.Empty)

      let logged =
        err.ToString().Replace("\r", String.Empty).Replace("\n", "|")

      Assert.That(
        logged,
        Is.EqualTo(
          "|ERROR *** Instrumentation phase failed|||"
          + unique
          + "|"
        )
      )

      Assert.That(CommandLine.exceptions, Is.Empty)
    finally
      CommandLine.error <- []
      CommandLine.exceptions <- []
      Output.info <- (fst saved)
      Output.error <- (snd saved)

  [<Test>]
  let NotSupportedExceptionWrites () =
    let saved = (Output.info, Output.error)
    let err = System.Text.StringBuilder()
    let info = System.Text.StringBuilder()

    let f1 =
      fun (s: String) -> info.Append(s).Append("|") |> ignore

    let f2 =
      fun (s: String) -> err.Append(s).Append("|") |> ignore

    f1 "f1"
    Assert.That(info.ToString(), Is.Not.Empty)
    info.Clear() |> ignore
    Assert.That(info.ToString(), Is.Empty)

    f2 "f2"
    Assert.That(err.ToString(), Is.Not.Empty)
    err.Clear() |> ignore
    Assert.That(err.ToString(), Is.Empty)

    try
      Output.info <- f1
      Output.error <- f2

      let unique =
        "NotSupportedException "
        + Guid.NewGuid().ToString()

      CommandLine.error <- []

      CommandLine.doPathOperation
        (fun () -> NotSupportedException(unique) |> raise)
        ()
        false

      Assert.That(CommandLine.error, Is.EquivalentTo [ unique ])
      Assert.That(info.ToString(), Is.Empty)
      Assert.That(err.ToString(), Is.Empty)
      Assert.That(CommandLine.exceptions, Is.Empty)
    finally
      CommandLine.error <- []
      Output.info <- (fst saved)
      Output.error <- (snd saved)

  [<Test>]
  let SecurityExceptionWrites () =
    let saved = (Output.info, Output.error)
    let err = System.Text.StringBuilder()
    let info = System.Text.StringBuilder()

    let f1 =
      fun (s: String) -> info.Append(s).Append("|") |> ignore

    let f2 =
      fun (s: String) -> err.Append(s).Append("|") |> ignore

    f1 "f1"
    Assert.That(info.ToString(), Is.Not.Empty)
    info.Clear() |> ignore
    Assert.That(info.ToString(), Is.Empty)

    f2 "f2"
    Assert.That(err.ToString(), Is.Not.Empty)
    err.Clear() |> ignore
    Assert.That(err.ToString(), Is.Empty)

    try
      Output.info <- f1
      Output.error <- f2

      let unique =
        "SecurityException " + Guid.NewGuid().ToString()

      CommandLine.error <- []
      CommandLine.exceptions <- []

      CommandLine.doPathOperation
        (fun () -> System.Security.SecurityException(unique) |> raise)
        ()
        false

      Assert.That(CommandLine.error, Is.EquivalentTo [ unique ])
      Assert.That(info.ToString(), Is.Empty)
      Assert.That(err.ToString(), Is.Empty)
      Assert.That(CommandLine.exceptions, Is.Empty)
    finally
      CommandLine.error <- []
      CommandLine.exceptions <- []
      Output.info <- (fst saved)
      Output.error <- (snd saved)

  [<Test>]
  let UnauthorizedAccessExceptionWrites () =
    let saved = (Output.info, Output.error)
    let err = System.Text.StringBuilder()
    let info = System.Text.StringBuilder()

    let f1 =
      fun (s: String) -> info.Append(s).Append("|") |> ignore

    let f2 =
      fun (s: String) -> err.Append(s).Append("|") |> ignore

    f1 "f1"
    Assert.That(info.ToString(), Is.Not.Empty)
    info.Clear() |> ignore
    Assert.That(info.ToString(), Is.Empty)

    f2 "f2"
    Assert.That(err.ToString(), Is.Not.Empty)
    err.Clear() |> ignore
    Assert.That(err.ToString(), Is.Empty)

    try
      Output.info <- f1
      Output.error <- f2

      let unique =
        "UnauthorizedAccessException "
        + Guid.NewGuid().ToString()

      CommandLine.error <- []
      CommandLine.exceptions <- []

      CommandLine.doPathOperation
        (fun () -> UnauthorizedAccessException(unique) |> raise)
        ()
        false

      Assert.That(CommandLine.error, Is.EquivalentTo [ unique ])
      Assert.That(info.ToString(), Is.Empty)
      Assert.That(err.ToString(), Is.Empty)
      Assert.That(CommandLine.exceptions, Is.Empty)
    finally
      CommandLine.error <- []
      CommandLine.exceptions <- []
      Output.info <- (fst saved)
      Output.error <- (snd saved)

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
  let OutputVerbose () =
    let save1 = Output.info
    let save2 = CommandLine.verbosity

    try
      let mutable buffer = String.Empty
      Output.verbose <- ignore

      Output.maybeVerbose false "OutputVerbose"
      test <@ buffer |> String.IsNullOrEmpty @>

      Output.maybeVerbose true "OutputVerbose"
      test <@ buffer |> String.IsNullOrEmpty @>

      Output.verbose <- fun x -> buffer <- x

      Output.maybeVerbose false "OutputVerbose"
      test <@ buffer |> String.IsNullOrEmpty @>

      Output.maybeVerbose true "OutputVerbose"
      test <@ buffer = "OutputVerbose" @>

    finally
      Output.info <- save1
      CommandLine.verbosity <- save2
      Output.verbose <- ignore