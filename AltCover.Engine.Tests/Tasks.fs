namespace Tests

open System
open System.IO
open System.Reflection
open System.Xml.Linq

open AltCover
open Microsoft.FSharp.Reflection
open Mono.Options
open Mono.Cecil.Cil

#nowarn "25"

module Tasks =
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

    let ignoreAction = Action<String>(ignore)

    ignoreAction.Invoke("ignoreAction")
    Assert.That(AltCover.LoggingOptions.ActionAdapter(ignoreAction), Is.Not.Null)
    let mutable x = String.Empty
    let f = (fun s -> x <- s)
    (AltCover.LoggingOptions.ActionAdapter(Action<String>(f))) "42"
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
            "--save" ]
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
        subject.GetType().GetProperty("Verbosity").SetValue(subject, level)

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
              "--save" ]
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
        subject.GetType().GetProperty("Verbosity").SetValue(subject, level)

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

    Assert.That(Path.GetFileName(subject.Extended).StartsWith("altcover.test."))

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
        "_Binaries/Sample4/Debug+AnyCPU/net11.0"
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
        <@ builder.ToString().StartsWith(ex.GetType().FullName, StringComparison.Ordinal) @>)

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