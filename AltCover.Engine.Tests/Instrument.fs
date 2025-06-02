namespace Tests
// fsharplint:disable  MemberNames NonPublicValuesNames RedundantNewKeyword

open System
open System.Collections.Generic
open System.IO
open System.Reflection

open AltCover
open Mono.Cecil
open Mono.Cecil.Cil
open Mono.Cecil.Rocks

#nowarn "25"

[<AutoOpen>]
module HallmarkExtra =
  type Hallmark with
    static member internal Build() =
      { Assembly = String.Empty
        Configuration = String.Empty }

module Instrument =

  let recorderSnk =
    typeof<AltCover.Node>.Assembly.GetManifestResourceNames()
    |> Seq.find _.EndsWith(".Recorder.snk", StringComparison.Ordinal)

  let infrastructureSnk =
    Assembly.GetExecutingAssembly().GetManifestResourceNames()
    |> Seq.find _.EndsWith("Infrastructure.snk", StringComparison.Ordinal)

  let ivtSnk =
    Assembly.GetExecutingAssembly().GetManifestResourceNames()
    |> Seq.find _.EndsWith("InternalsVisibleTest.snk", StringComparison.Ordinal)

  let private provideKeyPair () =
    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(infrastructureSnk)

    use buffer = new MemoryStream()
    stream.CopyTo(buffer)
    StrongNameKeyData.Make(buffer.ToArray())

  let recorderStream () =
    let recorder =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("AltCover.Recorder.net20.dll", StringComparison.Ordinal)

    Assembly.GetExecutingAssembly().GetManifestResourceStream(recorder)

  // Instrument.I.fs
#if IDEMPOTENT_INSTRUMENT
  [<Test>]
  let ShouldGetAbandonedMutex () =
    use testMutex = new System.Threading.Mutex()
    let abandon () = Instrument.I.safeWait testMutex

    let t =
      Threading.Thread(Threading.ThreadStart(abandon))

    t.Start()
    t.Join()

    try
      abandon ()
    finally
      testMutex.ReleaseMutex()

    test <@ "Didn't lock!" |> String.IsNullOrWhiteSpace |> not @>
#endif

  [<Test>]
  let ShouldBeAbleToGetTheVisitReportMethod () =
    use recstream = recorderStream ()

    use def =
      AssemblyResolver.ReadAssembly recstream

    let recorder =
      AltCover.Instrument.I.recordingMethod def

    recorder
    |> List.zip
      [ "System.Void AltCover.Recorder.Instance.Visit(System.String,System.Int32)"
        "System.Void AltCover.Recorder.Instance.Push(System.Int32)"
        "System.Void AltCover.Recorder.Instance.Pop()" ]
    |> List.iter (fun (n, m) -> test <@ Naming.fullMethodName m = n @>)

  [<Test>]
  let ShouldBeAbleToClearTheStrongNameKey () =
    let path = Path.Combine(dir, "Sample3.dll")

    use def = AssemblyResolver.ReadAssembly path

    Assert.That(def.Name.HasPublicKey)
    let key0 = def.Name.PublicKey
    Assert.That(key0, Is.Not.Empty)
    let token0 = def.Name.PublicKeyToken
    Assert.That(token0, Is.Not.Empty)

    AltCover.Instrument.I.updateStrongNaming def None
    Assert.That(def.Name.HasPublicKey, Is.False)
    let key1 = def.Name.PublicKey
    Assert.That(key1, Is.Empty)
    let token1 = def.Name.PublicKeyToken
    Assert.That(token1, Is.Empty)

  [<Test>]
  let ShouldBeAbleToUpdateTheStrongNameKeyWherePossible () =
    let path = Path.Combine(dir, "Sample3.dll")

    use def = AssemblyResolver.ReadAssembly path

    let key0 = def.Name.PublicKey
    let token0 = def.Name.PublicKeyToken

    Assert.That(def.Name.HasPublicKey)
    Assert.That(key0, Is.Not.Empty)
    Assert.That(token0, Is.Not.Empty)

    use stream =
      typeof<AltCover.Node>.Assembly.GetManifestResourceStream(recorderSnk)

    use buffer = new MemoryStream()
    stream.CopyTo(buffer)

    let key =
      StrongNameKeyData.Make(buffer.ToArray())

    AltCover.Instrument.I.updateStrongNaming def (Some key)

    Assert.That(def.Name.HasPublicKey)
    let key1 = def.Name.PublicKey
    Assert.That(key1, Is.Not.Null)
    Assert.That(key1, Is.Not.EquivalentTo(key0))
    let token1 = def.Name.PublicKeyToken
    Assert.That(token1, Is.Not.Null)
    Assert.That(token1, Is.Not.EquivalentTo(token0))

    let token' =
      String.Join(String.Empty, token1 |> Seq.map _.ToString("x2"))

    Assert.That(token', Is.EqualTo("4ebffcaabf10ce6a"))

  [<Test>]
  let NoKnownKeyInEmptyIndex () =
    try
      CoverageParameters.keys.Clear()

      let path = Path.Combine(dir, "Sample3.dll")

      use def = AssemblyResolver.ReadAssembly path

      Assert.That(Option.isNone (Instrument.I.knownKey def.Name))
    finally
      CoverageParameters.keys.Clear()

  [<Test>]
  let KnownKeyMatchedInIndex () =
    try
      CoverageParameters.keys.Clear()

      let path = Path.Combine(dir, "Sample3.dll")

      use def = AssemblyResolver.ReadAssembly path

      provideKeyPair () |> CoverageParameters.add
      Assert.That(Option.isSome (Instrument.I.knownKey def.Name))
    finally
      CoverageParameters.keys.Clear()

  [<Test>]
  let ThirdPartyKeyNotMatchedInIndex () =
    try
      CoverageParameters.keys.Clear()

      let path =
        typeof<System.IO.FileAccess>.Assembly.Location

      use def = AssemblyResolver.ReadAssembly path

      provideKeyPair () |> CoverageParameters.add
      Assert.That(Option.isNone (Instrument.I.knownKey def.Name))
    finally
      CoverageParameters.keys.Clear()

  [<Test>]
  let FakedUpKeyIsMatchedInIndex () =
    try
      CoverageParameters.keys.Clear()

      let path =
        typeof<Microsoft.FSharp.Core.CompilationMappingAttribute>.Assembly.Location

      use def = AssemblyResolver.ReadAssembly path

      let key =
        KeyStore.arrayToIndex def.Name.PublicKey

      CoverageParameters.keys.Add(
        key,
        { Pair = StrongNameKeyData.Empty()
          Token = [] }
      )

      Assert.That(Option.isSome (Instrument.I.knownKey def.Name))
    finally
      CoverageParameters.keys.Clear()

  [<Test>]
  let NoKnownKeyIfAssemblyHasNone () =
    try
      CoverageParameters.keys.Clear()

      let where =
        Assembly.GetExecutingAssembly().Location

      let path = Path.Combine(dir, "Sample3.dll")

      use def = AssemblyResolver.ReadAssembly path

      AltCover.Instrument.I.updateStrongNaming def None
      provideKeyPair () |> CoverageParameters.add
      Assert.That(Option.isNone (Instrument.I.knownKey def.Name))
    finally
      CoverageParameters.keys.Clear()

  [<Test>]
  let NoKnownTokenInEmptyIndex () =
    try
      CoverageParameters.keys.Clear()

      let where =
        Assembly.GetExecutingAssembly().Location

      let path = Path.Combine(dir, "Sample3.dll")

      use def = AssemblyResolver.ReadAssembly path

      Assert.That(Option.isNone (Instrument.I.knownToken def.Name))
    finally
      CoverageParameters.keys.Clear()

  [<Test>]
  let KnownTokenMatchedInIndex () =
    try
      CoverageParameters.keys.Clear()

      let where =
        Assembly.GetExecutingAssembly().Location

      let path = Path.Combine(dir, "Sample3.dll")

      use def = AssemblyResolver.ReadAssembly path

      provideKeyPair () |> CoverageParameters.add
      Assert.That(Option.isSome (Instrument.I.knownToken def.Name))
    finally
      CoverageParameters.keys.Clear()

  [<Test>]
  let NoKnownTokenIfAssemblyHasNone () =
    try
      CoverageParameters.keys.Clear()

      let where =
        Assembly.GetExecutingAssembly().Location

      let path = Path.Combine(dir, "Sample3.dll")

      use def = AssemblyResolver.ReadAssembly path

      AltCover.Instrument.I.updateStrongNaming def None
      provideKeyPair () |> CoverageParameters.add
      Assert.That(Option.isNone (Instrument.I.knownToken def.Name))
    finally
      CoverageParameters.keys.Clear()

  [<Test>]
  let ForeignTokenIsNotMatchedInIndex () =
    try
      CoverageParameters.keys.Clear()
      provideKeyPair () |> CoverageParameters.add

      let path =
        typeof<System.IO.FileAccess>.Assembly.Location

      use def = AssemblyResolver.ReadAssembly path

      let key =
        KeyStore.arrayToIndex def.Name.PublicKey

      Assert.That(Option.isNone (Instrument.I.knownToken def.Name))
    finally
      CoverageParameters.keys.Clear()

  [<Test>]
  let FakedUpTokenIsMatchedInIndex () =
    try
      CoverageParameters.keys.Clear()

      let path =
        typeof<Microsoft.FSharp.Core.CompilationMappingAttribute>.Assembly.Location

      use def = AssemblyResolver.ReadAssembly path

      let key =
        KeyStore.arrayToIndex def.Name.PublicKey

      CoverageParameters.keys.Add(
        key,
        { Pair = StrongNameKeyData.Empty()
          Token = [] }
      )

      Assert.That(Option.isSome (Instrument.I.knownToken def.Name))
    finally
      CoverageParameters.keys.Clear()

  [<Test>]
  let GuardShouldDisposeRecordingAssemblyOnException () =
    let path = Path.Combine(dir, "Sample3.dll")

    use prepared =
      AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols prepared

    let bang =
      fun () -> InvalidOperationException("Bang") |> raise

    Assert.Throws<InvalidOperationException>(fun () ->
      Instrument.I.guard prepared bang |> ignore)
    |> ignore

    let output = Path.GetTempFileName()
    let outputdll = output + ".dll"

    try
      Assert.Throws<ArgumentException>(fun () ->
        Instrument.I.writeAssembly prepared outputdll)
      |> ignore
    finally
      Directory.EnumerateFiles(
        Path.GetDirectoryName output,
        (Path.GetFileNameWithoutExtension output) + ".*"
      )
      |> Seq.iter (fun f -> maybeIOException (fun () -> File.Delete f))

  [<Test>]
  let ShouldBeAbleToTellAnAssembly () =
    let where =
      Assembly.GetExecutingAssembly().Location

    let here = Path.GetDirectoryName where
    let pdb = Directory.GetFiles(here, "*.pdb")
    Assert.That(pdb, Is.Not.Empty, "no pdb")

    pdb
    |> Seq.iter (fun p ->
      let a = AssemblyConstants.findAssemblyName p
      Assert.That(String.IsNullOrWhiteSpace a, p))

    let dll = Directory.GetFiles(here, "*.dll")
    Assert.That(dll, Is.Not.Empty, "no dll")

    dll
    |> Seq.iter (fun d ->
      let a = AssemblyConstants.findAssemblyName d
      Assert.That(a |> String.IsNullOrWhiteSpace |> not, d))

  [<Test>]
  let ShouldBeAbleToValidateAnAssembly () =
    let where =
      Assembly.GetExecutingAssembly().Location

    let here = Path.GetDirectoryName where
    let pdb = Directory.GetFiles(here, "*.pdb")
    Assert.That(pdb, Is.Not.Empty, "no pdb")
    CommandLine.error <- []

    pdb
    |> Seq.iter (fun p ->
      let (a, b) =
        CommandLine.validateAssembly "*" p

      Assert.That(String.IsNullOrWhiteSpace a, p)
      Assert.That(b |> not))

    Assert.That(CommandLine.error.Length, Is.EqualTo pdb.Length, "pdb length")
    CommandLine.error <- []
    let dll = Directory.GetFiles(here, "*.dll")
    Assert.That(dll, Is.Not.Empty, "no dll")

    dll
    |> Seq.iter (fun d ->
      let (a, b) =
        CommandLine.validateAssembly "*" d

      Assert.That(a |> String.IsNullOrWhiteSpace |> not, d)
      Assert.That(b))

    Assert.That(CommandLine.error |> List.isEmpty)

    let x =
      CommandLine.validateAssembly "*" "**"

    Assert.That(x, Is.EqualTo((String.Empty, false)))

  [<Test>]
  let ShouldBeAbleToLocateAReference () =
    let where =
      Assembly.GetExecutingAssembly().Location

    let here = Path.GetDirectoryName where
#if !NET472
    let json =
      Directory.GetFiles(here, "*.json")

    test' <@ json |> Seq.isEmpty |> not @> "no json"

    json
    |> Seq.iter (fun j ->
      let a = AssemblyConstants.findAssemblyName j
      test' <@ String.IsNullOrWhiteSpace a @> j)
#endif
    use raw =
      AssemblyResolver.ReadAssembly where

    try
      Assert.That(
        raw.MainModule.AssemblyResolver.GetType(),
        Is.EqualTo typeof<AssemblyResolver>
      )

      // Discover by searching
      AssemblyConstants.resolutionTable.Clear()

      raw.MainModule.AssemblyReferences
      |> Seq.filter (fun f ->
        f.Name.IndexOf("Mono.Cecil.Rocks", StringComparison.Ordinal)
        >= 0)
      |> Seq.iter (fun f ->
        let resolved =
          AssemblyResolver.ResolveFromNugetCache () f

        test <@ AssemblyConstants.resolutionTable.Count = 1 @>
        test' <@ resolved.IsNotNull @> <| f.ToString())

      // Discover by searching
      AssemblyConstants.resolutionTable.Clear()

      raw.MainModule.AssemblyReferences
      |> Seq.filter (fun f ->
        f.Name.IndexOf("Mono.Cecil.Rocks", StringComparison.Ordinal)
        >= 0)
      |> Seq.iter (fun f ->
        let resolved =
          raw.MainModule.AssemblyResolver.Resolve(f)

        test' <@ resolved.IsNotNull @> <| f.ToString())

      // Resolve failure
      AssemblyConstants.resolutionTable.Clear()

      raw.MainModule.AssemblyReferences
      |> Seq.filter (fun f ->
        f.Name.IndexOf("Mono.Cecil.Rocks", StringComparison.Ordinal)
        >= 0)
      |> Seq.iter (fun f ->
        f.Version <- System.Version("666.666.666.666")

        let resolved =
          AssemblyResolver.ResolveFromNugetCache () f

        test <@ AssemblyConstants.resolutionTable.Count = 0 @>
        test' <@ resolved |> isNull @> <| f.ToString())

      // Resolve from cache
      AssemblyConstants.resolutionTable.Clear()

      raw.MainModule.AssemblyReferences
      |> Seq.filter (fun f ->
        f.Name.IndexOf("Mono.Cecil.Rocks", StringComparison.Ordinal)
        >= 0)
      |> Seq.iter (fun f ->
        f.Version <- System.Version("666.666.666.666")
        AssemblyConstants.resolutionTable.[f.ToString()] <- raw

        let resolved =
          AssemblyResolver.ResolveFromNugetCache () f

        test <@ AssemblyConstants.resolutionTable.Count = 1 @>
        test' <@ resolved = raw @> <| f.ToString())

      // Also resolve from cache
      AssemblyConstants.resolutionTable.Clear()

      raw.MainModule.AssemblyReferences
      |> Seq.filter (fun f ->
        f.Name.IndexOf("Mono.Cecil.Rocks", StringComparison.Ordinal)
        >= 0)
      |> Seq.iter (fun f ->
        f.Version <- System.Version("666.666.666.666")
        AssemblyConstants.resolutionTable.[f.ToString()] <- raw

        let resolved =
          raw.MainModule.AssemblyResolver.Resolve f

        test <@ AssemblyConstants.resolutionTable.Count = 1 @>
        test' <@ resolved = raw @> <| f.ToString())
    finally
      AssemblyConstants.resolutionTable.Clear()

  [<Test>]
  let ShouldBeAbleToPrepareTheAssembly () =
    try
      CoverageParameters.keys.Clear()
      Main.init ()

      let path = Path.Combine(dir, "Sample3.dll")

      let prepared =
        Instrument.I.prepareAssembly (File.OpenRead path)

      use raw = AssemblyResolver.ReadAssembly path

      ProgramDatabase.readSymbols raw
      Assert.That(prepared.Name.Name, Is.EqualTo(raw.Name.Name + ".g"))
      Assert.That(prepared.Name.HasPublicKey)
      Assert.That(prepared.Name.PublicKey, Is.Not.EquivalentTo(raw.Name.PublicKey))

      let token' =
        String.Join(
          String.Empty,
          prepared.Name.PublicKeyToken
          |> Seq.map _.ToString("x2")
        )

      Assert.That(token', Is.EqualTo("4ebffcaabf10ce6a"))

      Assert.That(
        prepared.CustomAttributes
        |> Seq.exists (fun a ->
          a.AttributeType.FullName = "AltCover.Recorder.InstrumentationAttribute"),
        "Hallmark missing"
      )

      let before =
        raw.MainModule.GetTypes()
        |> Seq.filter (fun t -> t.Name = "Class4")
        |> Seq.toList

      Assert.That(before.Length = 1)

      let before' =
        before.[0].Methods
        |> Seq.filter (fun t -> t.Name = "get_ReportFile")
        |> Seq.toList

      Assert.That(before'.Length = 1)

      let after =
        prepared.MainModule.GetTypes()
        |> Seq.filter (fun t -> t.Name = "Class4")
        |> Seq.toList

      Assert.That(after.Length = 1)

      let after' =
        after.[0].Methods
        |> Seq.filter (fun t -> t.Name = "get_ReportFile")
        |> Seq.toList

      Assert.That(after'.Length = 1)
      Assert.That(after'.[0].Body.Instructions.Count, Is.EqualTo(2))
    finally
      CoverageParameters.keys.Clear()

  [<Test>]
  let ShouldGetTrackingStyleIfSet () =
    Main.init ()

    let save2 =
      CoverageParameters.theReportFormat

    let save3 = CoverageParameters.theInterval
    CoverageParameters.trackingNames.Clear()

    try
      CoverageParameters.theReportFormat <- Some AltCover.ReportFormat.OpenCover
      CoverageParameters.theInterval <- Some 1234567890

      Assert.That(
        CoverageParameters.reportFormat (),
        Is.EqualTo(
          AltCover.ReportFormat.OpenCover
          ||| ReportFormat.WithTracking
        )
      )

      CoverageParameters.theInterval <- None
      CoverageParameters.trackingNames.Add("dummy")

      Assert.That(
        CoverageParameters.reportFormat (),
        Is.EqualTo(
          AltCover.ReportFormat.OpenCover
          ||| ReportFormat.WithTracking
        )
      )

      CoverageParameters.trackingNames.Clear()

      Assert.That(
        CoverageParameters.reportFormat (),
        Is.EqualTo AltCover.ReportFormat.OpenCover
      )

      CoverageParameters.theReportFormat <- Some AltCover.ReportFormat.NCover
      CoverageParameters.theInterval <- Some 1234567890

      Assert.That(
        CoverageParameters.reportFormat (),
        Is.EqualTo AltCover.ReportFormat.NCover
      )

      CoverageParameters.theInterval <- None
      CoverageParameters.trackingNames.Add("dummy")

      Assert.That(
        CoverageParameters.reportFormat (),
        Is.EqualTo AltCover.ReportFormat.NCover
      )

      CoverageParameters.trackingNames.Clear()

      Assert.That(
        CoverageParameters.reportFormat (),
        Is.EqualTo AltCover.ReportFormat.NCover
      )
    finally
      CoverageParameters.theReportFormat <- save2
      CoverageParameters.theInterval <- save3

    CoverageParameters.trackingNames.Clear()

#if !NET472
  type TestAssemblyLoadContext(_dummy: string, _dummy2: string) =
    inherit System.Runtime.Loader.AssemblyLoadContext(true)
    override self.Load(name: AssemblyName) = null

    member self.CreateObject() =
      let proxyObject = ProxyObject()
      proxyObject.Context <- self
      proxyObject
#else
  type TestAssemblyLoadContext(domain: string, where: string) =
    let ad =
      AppDomain.CreateDomain(
        domain,
        null,
        let setup = AppDomainSetup()
        setup.ApplicationBase <- Path.GetDirectoryName(where)
        setup
      )

    member self.CreateObject() =
      ad.CreateInstanceFromAndUnwrap(
        typeof<ProxyObject>.Assembly.Location,
        "Tests.ProxyObject"
      )
      :?> ProxyObject

    member self.Unload() = AppDomain.Unload(ad)
#endif

  [<Test>]
  let ShouldGetNewFilePathFromPreparedAssembly () =
    try
      CoverageParameters.keys.Clear()
      Main.init ()

      let path = Path.Combine(dir, "Sample3.dll")

      let unique = Guid.NewGuid().ToString()
      let output = Path.GetTempFileName()
      let outputdll = output + ".dll"
      let where = output |> Path.GetDirectoryName
      let what = outputdll |> Path.GetFileName

      let second =
        Path.Combine(where, Guid.NewGuid().ToString())

      let alter = Path.Combine(second, what)
      Directory.CreateDirectory(second) |> ignore
      let save = CoverageParameters.theReportPath

      let save2 =
        CoverageParameters.theReportFormat

      let save3 = CoverageParameters.theInterval

      try
        CoverageParameters.theReportPath <- Some unique
        CoverageParameters.theReportFormat <- Some AltCover.ReportFormat.OpenCover
        CoverageParameters.theInterval <- Some 1234567890
        CoverageParameters.all <- false
        CoverageParameters.zipReport.Value <- true

        Assert.That(
          CoverageParameters.sampling (),
          Sampling.Single |> int |> Is.EqualTo,
          "Unexpected sampling"
        )

        let prepared =
          Instrument.I.prepareAssembly (File.OpenRead path)

        let traces =
          System.Collections.Generic.List<string>()

        Instrument.I.writeAssemblies prepared what [ where; second ] (fun s ->
          s.Replace("\r", String.Empty).Replace("\n", String.Empty)
          |> traces.Add)

        let expectedTraces =
          [ "    "
            + outputdll
            + "                <=  Sample3.g, Version=0.0.0.0, Culture=neutral, PublicKeyToken=4ebffcaabf10ce6a"
            "    "
            + alter
            + "                <=  Sample3.g, Version=0.0.0.0, Culture=neutral, PublicKeyToken=4ebffcaabf10ce6a" ]

        Assert.That(traces, Is.EquivalentTo expectedTraces, "unexpected traces")

        let expectedSymbols =
          Maybe ("Mono.Runtime" |> Type.GetType).IsNotNull ".dll.mdb" ".pdb"

        use raw =
          AssemblyResolver.ReadAssembly outputdll

        use raw2 =
          AssemblyResolver.ReadAssembly alter

        Assert.That(
          raw.MainModule.Mvid,
          Is.EqualTo raw2.MainModule.Mvid,
          "unexpected mvid"
        )

        Assert.That(raw.Name.HasPublicKey, "missing public key")
        // Assert.That (Option.isSome <| Instrument.I.knownKey raw.Name) <- not needed
        let token' =
          String.Join(
            String.Empty,
            raw.Name.PublicKeyToken
            |> Seq.map _.ToString("x2")
          )

        Assert.That(token', Is.EqualTo("4ebffcaabf10ce6a"), "wrong token")

        let alc =
          new TestAssemblyLoadContext("ShouldGetNewFilePathFromPreparedAssembly", where)

        try
          let proxyObject = alc.CreateObject()
          proxyObject.InstantiateObject(outputdll, "AltCover.Sample3.Class3+Class4", [||])

          let report =
            proxyObject.InvokeMethod("get_ReportFile", [||]).ToString()

          Assert.That(
            report,
            Is.EqualTo(Path.GetFullPath unique),
            "report path " + report + " not " + unique
          )

          let report2 =
            proxyObject.InvokeMethod("get_CoverageFormat", [||]) :?> System.Int32

          Assert.That(
            report2,
            (AltCover.ReportFormat.OpenCover
             ||| ReportFormat.WithTracking
             ||| ReportFormat.Zipped)
            |> int
            |> Is.EqualTo,
            "wrong tracking format"
          )

          let report3 =
            proxyObject.InvokeMethod("get_Timer", [||]) :?> System.Int64

          Assert.That(report3, 1234567890L |> Is.EqualTo, "wrong timer")

          let report4 =
            proxyObject.InvokeMethod("get_Sample", [||]) :?> System.Int32

          Assert.That(
            report4,
            AltCover.Sampling.Single |> int |> Is.EqualTo,
            "wrong outro sampling"
          )
        finally
          alc.Unload()
      finally
        CoverageParameters.all <- false
        CoverageParameters.theReportPath <- save
        CoverageParameters.theReportFormat <- save2
        CoverageParameters.theInterval <- save3

        Directory.EnumerateFiles(
          Path.GetDirectoryName output,
          (Path.GetFileNameWithoutExtension output) + ".*"
        )
        |> Seq.iter (fun f -> maybeIOException (fun () -> File.Delete f))

        Directory.EnumerateFiles(
          Path.GetDirectoryName alter,
          (Path.GetFileNameWithoutExtension alter) + ".*"
        )
        |> Seq.iter (fun f -> maybeIOException (fun () -> File.Delete f))

        Assert.That(CoverageParameters.sampling (), Sampling.Single |> int |> Is.EqualTo)
    finally
      CoverageParameters.keys.Clear()

  [<Test>]
  let ShouldHandleNullConstantsOK () =
    // Workround for Cecil 11.4
    let path = // Use a known good (bad) build rather than a local new one each time
      Path.Combine(SolutionRoot.location, "AltCover.TestData/NullConst.dll")

    use ``module`` =
      AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols ``module``

    // big test -- if we can write w/o crashing when the rescope is removed
    let output = Path.GetTempFileName()
    let outputdll = output + ".dll"

    let writer = WriterParameters()
    writer.SymbolWriterProvider <- Mono.Cecil.Cil.EmbeddedPortablePdbWriterProvider()
    writer.WriteSymbols <- true

    let def =
      ``module``.MainModule.GetTypes()
      |> Seq.collect _.Methods
      |> Seq.find _.Name.Equals("MakeConst")

    use sink =
      File.Open(outputdll, FileMode.Create, FileAccess.ReadWrite)

    // should not throw
    ``module``.Write(sink, writer)

  [<Test>]
  let ShouldRescopeMonoMethodOK () =
    // Workround for Cecil 11.4
    let path = // Use a known good (bad) build rather than a local new one each time
      Path.Combine(SolutionRoot.location, "AltCover.TestData/Sample31.dll")

    use ``module`` =
      AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols ``module``

    let pathGetterDef =
      ``module``.MainModule.GetTypes()
      |> Seq.collect _.Methods
      |> Seq.find _.Name.Equals("get_Defer")

    let body = pathGetterDef.Body
    let worker = body.GetILProcessor()

    let initialBody =
      body.Instructions |> Seq.toList

    let head = initialBody |> Seq.head
    let opcode = worker.Create(OpCodes.Ldc_I4_1)
    worker.InsertBefore(head, opcode)

    Assert.That(
      pathGetterDef.DebugInformation.Scope.Start.IsEndOfMethod,
      Is.False,
      "Scope.Start.IsEndOfMethod"
    )

    Assert.That(
      pathGetterDef.DebugInformation.Scope.Scopes
      |> Seq.exists _.Start.IsEndOfMethod,
      Is.False,
      "subscope.Start.IsEndOfMethod"
    )

    // big test -- if we can write w/o crashing when the previous asserts are removed
    let output = Path.GetTempFileName()
    let outputdll = output + ".dll"

    let writer = WriterParameters()
    writer.SymbolWriterProvider <- Mono.Cecil.Cil.EmbeddedPortablePdbWriterProvider()

    writer.WriteSymbols <- true

    use sink =
      File.Open(outputdll, FileMode.Create, FileAccess.ReadWrite)

    // should not throw
    ``module``.Write(sink, writer)

    Assert.That(
      pathGetterDef.DebugInformation.Scope.Start.IsEndOfMethod,
      Is.False,
      "pruned Scope.Start.IsEndOfMethod"
    )

    (* was
    IL_0000: ldarg.0
    IL_0001: ldfld bool Sample31.Class3/Class4::'<Eager>k__BackingField'
    IL_0006: stloc.0
    IL_0007: br IL_000c

    IL_000c: ldloc.0
    IL_000d: ret
has been prefixed with Ldc_I4_1 (1 byte)
  *)

    let start =
      pathGetterDef.Body.Instructions |> Seq.head

    let finish =
      pathGetterDef.Body.Instructions |> Seq.last

    let rescope =
      ScopeDebugInformation(start, null)

    let size = finish.Offset + finish.GetSize()

    let f =
      pathGetterDef.Body.Instructions.[2].Operand :?> FieldDefinition

    let primitive = f.FieldType
    let np = ArrayType(primitive, 23)

    pathGetterDef.DebugInformation.Scope <- rescope

    [ -1
      4
      finish.Offset
      size - 1
      size
      Int32.MaxValue ]
    |> Seq.iter (fun i ->
      let s = ScopeDebugInformation(start, null)
      s.Start <- InstructionOffset(i)
      s.End <- InstructionOffset(size)
      let n = i.ToString().Replace("-", "_")

      s.Constants.Add
      <| ConstantDebugInformation("I" + n, primitive, nullObject)

      s.Constants.Add
      <| ConstantDebugInformation("A" + n, np, nullObject)

      rescope.Scopes.Add s)

    rescope.Scopes.Add rescope

    Assert.That(InstructionOffset().IsEndOfMethod, "End should go to none")

    // prune 1 recursion and 2 at end => 3
    Assert.That(rescope.Scopes |> Seq.length, Is.EqualTo(4 + 3))

    test <@ start.Offset = 0 @>
    test <@ start.Next.Offset = 1 @>
    test <@ start.Next.Next.Offset = 2 @>
    test <@ start.Next.Next.Next.Offset = 7 @>

  [<Test>]
  let ShouldWriteMonoAssemblyOK () =
    try
      CoverageParameters.keys.Clear()
      Main.init ()

      let where =
        Assembly.GetExecutingAssembly().Location

      let path =
        Path.Combine(SolutionRoot.location, "_Mono/Sample3/Sample3.dll")

      maybeIgnore (fun () -> path |> File.Exists |> not)
      let unique = Guid.NewGuid().ToString()
      let output = Path.GetTempFileName()
      let outputdll = output + ".dll"
      let save = CoverageParameters.theReportPath

      use stream =
        typeof<AltCover.Node>.Assembly.GetManifestResourceStream(recorderSnk)

      use buffer = new MemoryStream()
      stream.CopyTo(buffer)

      let key =
        StrongNameKeyData.Make(buffer.ToArray())

      CoverageParameters.defaultStrongNameKey <- Some key
      CoverageParameters.add key

      try
        CoverageParameters.theReportPath <- Some unique

        let prepared =
          Instrument.I.prepareAssembly (File.OpenRead path)

        Instrument.I.writeAssembly prepared outputdll
        // TODO -- see Instrument.I.WriteAssembly       Assert.That (File.Exists (outputdll + ".mdb"))
        use raw =
          AssemblyResolver.ReadAssembly outputdll

        Assert.That raw.Name.HasPublicKey
        // Assert.That (Option.isSome <| Instrument.I.knownKey raw.Name) <- not needed
        let token' =
          String.Join(
            String.Empty,
            raw.Name.PublicKeyToken
            |> Seq.map _.ToString("x2")
          )

        Assert.That(token', Is.EqualTo("4ebffcaabf10ce6a"))

        let alc =
          new TestAssemblyLoadContext("ShouldGetNewFilePathFromPreparedAssembly", where)

        try
          let proxyObject = alc.CreateObject()
          proxyObject.InstantiateObject(outputdll, "AltCover.Sample3.Class3+Class4", [||])

          let report =
            proxyObject.InvokeMethod("get_ReportFile", [||]).ToString()

          Assert.That(report, Is.EqualTo(Path.GetFullPath unique))
        finally
          alc.Unload()
      finally
        CoverageParameters.theReportPath <- save

        Directory.EnumerateFiles(
          Path.GetDirectoryName output,
          (Path.GetFileNameWithoutExtension output) + ".*"
        )
        |> Seq.iter (fun f -> maybeIOException (fun () -> File.Delete f))
    finally
      CoverageParameters.keys.Clear()
      CoverageParameters.defaultStrongNameKey <- None

  [<Test>]
  let ShouldGetVisitFromWrittenAssembly () =
    try
      CoverageParameters.keys.Clear()

      let path = Path.Combine(dir, "Sample3.dll")

      let unique = Guid.NewGuid().ToString()
      let output = Path.GetTempFileName()
      let outputdll = output + ".dll"
      let save = CoverageParameters.theReportPath

      try
        use def = AssemblyResolver.ReadAssembly path

        ProgramDatabase.readSymbols def

        let clazz =
          def.MainModule.GetType("AltCover.Sample3.Class1")

        let func =
          clazz.GetMethods()
          |> Seq.find (fun x -> x.Name = "get_Property")

        let clazz' =
          def.MainModule.GetType("AltCover.Sample3.Class3")

        let func' =
          clazz'.GetMethods()
          |> Seq.find (fun x -> x.Name = "Log")

        let newValue =
          Instrument.I.insertVisit
            (func.Body.Instructions.[0])
            (func.Body.GetILProcessor())
            func'
            unique
            42

        Assert.That(newValue.Operand, Is.EqualTo unique, "bad operand")
        Assert.That(newValue.OpCode, Is.EqualTo OpCodes.Ldstr, "bad opcode")
        Instrument.I.writeAssembly def outputdll

        use raw =
          AssemblyResolver.ReadAssembly outputdll

        Assert.That(raw.Name.HasPublicKey, "bad public key")
        // Assert.That (Option.isSome <| Instrument.I.knownKey raw.Name) <- not needed
        let token' =
          String.Join(
            String.Empty,
            raw.Name.PublicKeyToken
            |> Seq.map _.ToString("x2")
          )

        Assert.That(token', Is.EqualTo("c02b1a9f5b7cade8"), "wrong token")

        let where =
          Assembly.GetExecutingAssembly().Location

        let alc =
          new TestAssemblyLoadContext("ShouldGetNewFilePathFromPreparedAssembly", where)

        try
          let proxyObject = alc.CreateObject()
          proxyObject.InstantiateObject(outputdll, "AltCover.Sample3.Class1", [||])

          let setting =
            proxyObject.InvokeMethod("set_Property", [| 17 |])

          Assert.That(setting, Is.Null, "bad setting")

          let getting =
            proxyObject.InvokeMethod("get_Property", [||]) :?> int

          Assert.That(getting, Is.EqualTo 17, "bad getting")

          let isMsft =
#if !NET472
            true
#else
            System.Environment.GetEnvironmentVariable("OS") = "Windows_NT"
#endif
          let proxyObject' = alc.CreateObject()
          proxyObject'.InstantiateObject(outputdll, "AltCover.Sample3.Class3", [||])

          let log =
            proxyObject'.InvokeMethod("get_Visits", [||]) :?> seq<Tuple<string, int>>
            |> Seq.toList

          let result =
            Maybe
              isMsft // HACK HACK HACK
              log
              [ (unique, 42) ]

          Assert.That(result, Is.EquivalentTo [ (unique, 42) ], "bad call")
        finally
          alc.Unload()
      finally
        CoverageParameters.theReportPath <- save

        Directory.EnumerateFiles(
          Path.GetDirectoryName output,
          (Path.GetFileNameWithoutExtension output) + ".*"
        )
        |> Seq.iter (fun f -> maybeIOException (fun () -> File.Delete f))
    finally
      CoverageParameters.keys.Clear()

  [<Test>]
  let ShouldUpdateHandlerOK ([<NUnit.Framework.Range(0, 31)>] selection) =
    let path = sample1path

    use def = AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols def

    let program =
      def.MainModule.GetType("TouchTest.Program")

    let main =
      program.GetMethods()
      |> Seq.find (fun x -> x.Name = "Main")

    let oldValue = main.Body.Instructions.[0]
    let proc = main.Body.GetILProcessor()

    let newValue =
      proc.Create(OpCodes.Ldc_I4, 23)

    let other = main.Body.Instructions.[1]

    let handler =
      ExceptionHandler(ExceptionHandlerType())

    handler.FilterStart <-
      if selection &&& 1 = 1 then
        oldValue
      else
        other

    handler.HandlerStart <-
      if selection &&& 2 = 2 then
        oldValue
      else
        other

    handler.HandlerEnd <-
      if selection &&& 4 = 4 then
        oldValue
      else
        other

    handler.TryStart <-
      if selection &&& 8 = 8 then
        oldValue
      else
        other

    handler.TryEnd <-
      if selection &&& 16 = 16 then
        oldValue
      else
        other

    CecilExtension.substituteExceptionBoundary oldValue newValue handler

    Assert.That(
      handler.FilterStart,
      Is.EqualTo(
        if selection &&& 1 = 1 then
          newValue
        else
          other
      )
    )

    Assert.That(
      handler.HandlerStart,
      Is.EqualTo(
        if selection &&& 2 = 2 then
          newValue
        else
          other
      )
    )

    Assert.That(
      handler.HandlerEnd,
      Is.EqualTo(
        if selection &&& 4 = 4 then
          newValue
        else
          other
      )
    )

    Assert.That(
      handler.TryStart,
      Is.EqualTo(
        if selection &&& 8 = 8 then
          newValue
        else
          other
      )
    )

    Assert.That(
      handler.TryEnd,
      Is.EqualTo(
        if selection &&& 16 = 16 then
          newValue
        else
          other
      )
    )

  [<Test>]
  let ShouldSubstituteInstructionOperand () =
    let path = sample2path

    use def = AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols def
    let module' = def.MainModule.GetType("N.DU")

    let du =
      module'.NestedTypes
      |> Seq.filter (fun t -> t.Name = "MyUnion")
      |> Seq.head

    let main =
      du.GetMethods()
      |> Seq.find (fun x -> x.Name = "as_bar")

    let proc = main.Body.GetILProcessor()

    let newValue =
      proc.Create(OpCodes.Ldc_I4, 23)

    main.Body.Instructions
    |> Seq.filter (fun i ->
      match i.Operand with
      | :? Instruction -> true
      | _ -> false)
    |> Seq.iter (fun i ->
      CecilExtension.substituteInstructionOperand (i.Operand :?> Instruction) newValue i

      Assert.That(i.Operand, Is.EqualTo newValue))

  [<Test>]
  let ShouldNotSubstituteDifferentInstructionOperand () =
    let path = sample2path

    use def = AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols def
    let module' = def.MainModule.GetType("N.DU")

    let du =
      module'.NestedTypes
      |> Seq.filter (fun t -> t.Name = "MyUnion")
      |> Seq.head

    let main =
      du.GetMethods()
      |> Seq.find (fun x -> x.Name = "as_bar")

    let proc = main.Body.GetILProcessor()

    let newValue =
      proc.Create(OpCodes.Ldc_I4, 23)

    main.Body.Instructions
    |> Seq.filter (fun i ->
      match i.Operand with
      | :? Instruction -> true
      | _ -> false)
    |> Seq.iter (fun i ->
      let before = i.Operand
      CecilExtension.substituteInstructionOperand i newValue i
      Assert.That(i.Operand, Is.SameAs before))

  // work around weird compiler error with array indexing
  let private asIArray (x: obj) (i: int) =
    (x :?> Instruction[])
    |> Seq.mapi (fun index instr -> (index, instr))
    |> Seq.filter (fun (x, y) -> x = i)
    |> Seq.map snd
    |> Seq.head

  [<Test>]
  let ShouldSubstituteIntoInstructionOperandArray () =
    let path = sample2path

    use def = AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols def
    let module' = def.MainModule.GetType("N.DU")

    let du =
      module'.NestedTypes
      |> Seq.filter (fun t -> t.Name = "MyUnion")
      |> Seq.head

    let main =
      du.GetMethods()
      |> Seq.find (fun x -> x.Name = "as_bar")

    let proc = main.Body.GetILProcessor()

    let newValue =
      proc.Create(OpCodes.Ldc_I4, 23)

    main.Body.Instructions
    |> Seq.filter (fun i ->
      match i.Operand with
      | :? (Instruction array) -> true
      | _ -> false)
    |> Seq.collect (fun i ->
      i.Operand :?> Instruction[]
      |> Seq.mapi (fun o t -> (i, o, t)))
    |> Seq.iter (fun (i, o, t) ->
      Assert.That(asIArray i.Operand o, (Is.SameAs t))
      Assert.That(t, Is.Not.EqualTo newValue)
      CecilExtension.substituteInstructionOperand t newValue i

      let t' = asIArray i.Operand
      Assert.That(t' o, Is.EqualTo newValue))

  [<Test>]
  let ShouldNotSubstituteOutsideInstructionOperandArray () =
    let path = sample2path

    use def = AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols def
    let module' = def.MainModule.GetType("N.DU")

    let du =
      module'.NestedTypes
      |> Seq.filter (fun t -> t.Name = "MyUnion")
      |> Seq.head

    let main =
      du.GetMethods()
      |> Seq.find (fun x -> x.Name = "as_bar")

    let proc = main.Body.GetILProcessor()

    let newValue =
      proc.Create(OpCodes.Ldc_I4, 23)

    main.Body.Instructions
    |> Seq.filter (fun i ->
      match i.Operand with
      | :? (Instruction array) -> true
      | _ -> false)
    |> Seq.iter (fun i ->
      let before =
        (i.Operand :?> Instruction[]) |> Seq.toList

      CecilExtension.substituteInstructionOperand i newValue i

      Seq.zip (i.Operand :?> Instruction[]) before
      |> Seq.iter (fun (after, before) -> Assert.That(after, Is.SameAs before)))

  [<Test>]
  let ShouldNotSubstituteOtherOperand () =
    let path = sample2path

    use def = AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols def
    let module' = def.MainModule.GetType("N.DU")

    let du =
      module'.NestedTypes
      |> Seq.filter (fun t -> t.Name = "MyUnion")
      |> Seq.head

    let main =
      du.GetMethods()
      |> Seq.find (fun x -> x.Name = "as_bar")

    let proc = main.Body.GetILProcessor()

    let newValue =
      proc.Create(OpCodes.Ldc_I4, 23)

    main.Body.Instructions
    |> Seq.filter (fun i ->
      match i.Operand with
      | :? Instruction
      | :? (Instruction array) -> false
      | _ -> true)
    |> Seq.collect (fun i ->
      main.Body.Instructions
      |> Seq.map (fun other -> (i, other)))
    |> Seq.iter (fun (i, other) ->
      let before = i.Operand
      CecilExtension.substituteInstructionOperand other newValue i
      Assert.That(i.Operand, Is.SameAs before))

  [<Test>]
  let ShouldBeAbleToTrackAMethod () =
    use recstream = recorderStream ()

    use def =
      AssemblyResolver.ReadAssembly recstream

    let recorder =
      AltCover.Instrument.I.recordingMethod def

    let raw =
      AltCover.InstrumentContext.Build([])

    let state =
      { raw with
          RecordingMethodRef =
            { Visit = null
              Push = recorder.[1]
              Pop = recorder.[2] } }

    let countBefore =
      recorder.Head.Body.Instructions.Count

    let tailsBefore =
      recorder.Head.Body.Instructions
      |> Seq.filter (fun i -> i.OpCode = OpCodes.Tail)
      |> Seq.length

    let nopsBefore =
      recorder.Head.Body.Instructions
      |> Seq.filter (fun i -> i.OpCode = OpCodes.Nop)
      |> Seq.length

    let handlersBefore =
      recorder.Head.Body.ExceptionHandlers.Count

    let state2 =
      AltCover.Instrument.I.doTrack
        state
        { Method = recorder.Head
          VisibleMethod = recorder.Head
          Inspection = Inspections.Track
          Track = Some(42, "hello")
          DefaultVisitCount = Exemption.None }

    Assert.That(state2.AsyncSupport |> Option.isNone)
    Assert.That(recorder.Head.Body.Instructions.Count, Is.EqualTo(countBefore + 7))

    Assert.That(
      recorder.Head.Body.ExceptionHandlers.Count,
      Is.EqualTo(handlersBefore + 1)
    )

    let nopsAfter =
      recorder.Head.Body.Instructions
      |> Seq.filter (fun i -> i.OpCode = OpCodes.Nop)
      |> Seq.length
    // add 2 extra nop now and replace rather than remove .tails
    Assert.That(nopsAfter, Is.EqualTo(nopsBefore + tailsBefore + 2))

  [<Test>]
  let ShouldBeAbleToTrackAMethodWithTailCalls () =
    use recstream = recorderStream ()

    let res =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("TailCallSample.dl_", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(res)

    use def =
      AssemblyResolver.ReadAssembly stream

    use rdef =
      AssemblyResolver.ReadAssembly recstream

    let recorder =
      AltCover.Instrument.I.recordingMethod rdef

    let target =
      def.MainModule.GetType("Tests.Problematic").Methods
      |> Seq.find (fun m -> m.Name = "Using FsUnit")

    let raw =
      AltCover.InstrumentContext.Build([])

    let state =
      { raw with
          RecordingMethodRef =
            { Visit = null
              Push = recorder.[1]
              Pop = recorder.[2] } }

    let countBefore =
      target.Body.Instructions.Count

    let tailsBefore =
      target.Body.Instructions
      |> Seq.filter (fun i -> i.OpCode = OpCodes.Tail)
      |> Seq.length

    let nopsBefore =
      target.Body.Instructions
      |> Seq.filter (fun i -> i.OpCode = OpCodes.Nop)
      |> Seq.length

    let handlersBefore =
      target.Body.ExceptionHandlers.Count

    let state2 =
      AltCover.Instrument.I.doTrack
        state
        { Method = target
          VisibleMethod = target
          Inspection = Inspections.Track
          Track = Some(42, "hello")
          DefaultVisitCount = Exemption.None }

    Assert.That(state2.AsyncSupport |> Option.isNone)
    Assert.That(target.Body.Instructions.Count, Is.EqualTo(countBefore + 7))
    Assert.That(target.Body.ExceptionHandlers.Count, Is.EqualTo(handlersBefore + 1))

    let nopsAfter =
      target.Body.Instructions
      |> Seq.filter (fun i -> i.OpCode = OpCodes.Nop)
      |> Seq.length
    // add 2 extra nop now and replace rather than remove .tails
    Assert.That(nopsAfter, Is.EqualTo(nopsBefore + tailsBefore + 2))

  [<Test>]
  let ShouldBeAbleToTrackAMethodWithNonVoidReturn () =
    use recstream = recorderStream ()

    let sample24 = sample24path

    use def =
      AssemblyResolver.ReadAssembly sample24

    use rdef =
      AssemblyResolver.ReadAssembly recstream

    let recorder =
      AltCover.Instrument.I.recordingMethod rdef

    let target =
      def.MainModule.GetType("NUnitTestProject1.Tests").Methods
      |> Seq.find (fun m -> m.Name = "AddSynch")

    let raw =
      AltCover.InstrumentContext.Build([])

    let state =
      { raw with
          RecordingMethodRef =
            { Visit = null
              Push = recorder.[1]
              Pop = recorder.[2] } }

    let countBefore =
      target.Body.Instructions.Count

    let tailsBefore =
      target.Body.Instructions
      |> Seq.filter (fun i -> i.OpCode = OpCodes.Tail)
      |> Seq.length

    let nopsBefore =
      target.Body.Instructions
      |> Seq.filter (fun i -> i.OpCode = OpCodes.Nop)
      |> Seq.length

    let handlersBefore =
      target.Body.ExceptionHandlers.Count

    let state2 =
      AltCover.Instrument.I.doTrack
        state
        { Method = target
          VisibleMethod = target
          Inspection = Inspections.Track
          Track = Some(42, "hello")
          DefaultVisitCount = Exemption.None }

    Assert.That(state2.AsyncSupport |> Option.isNone)

    Assert.That( // Adding the return value, too
      target.Body.Instructions.Count,
      Is.EqualTo(countBefore + 9)
    )

    Assert.That(target.Body.ExceptionHandlers.Count, Is.EqualTo(handlersBefore + 1))

    let nopsAfter =
      target.Body.Instructions
      |> Seq.filter (fun i -> i.OpCode = OpCodes.Nop)
      |> Seq.length
    // add 2 extra nop now and replace rather than remove .tails
    Assert.That(nopsAfter, Is.EqualTo(nopsBefore + tailsBefore + 2))

  [<Test>]
  let ShouldBeAbleToTrackAnAsyncMethod () =
    use recstream = recorderStream ()

    let sample24 = sample24path

    use def =
      AssemblyResolver.ReadAssembly sample24

    use rdef =
      AssemblyResolver.ReadAssembly recstream

    let recorder =
      AltCover.Instrument.I.recordingMethod rdef

    let target =
      def.MainModule.GetType("NUnitTestProject1.Tests").Methods
      |> Seq.find (fun m -> m.Name = "AddAsync")

    let raw =
      AltCover.InstrumentContext.Build([])

    let state =
      { raw with
          RecordingMethodRef =
            { Visit = null
              Push = recorder.[1]
              Pop = recorder.[2] } }

    let countBefore =
      target.Body.Instructions.Count

    let tailsBefore =
      target.Body.Instructions
      |> Seq.filter (fun i -> i.OpCode = OpCodes.Tail)
      |> Seq.length

    let nopsBefore =
      target.Body.Instructions
      |> Seq.filter (fun i -> i.OpCode = OpCodes.Nop)
      |> Seq.length

    let handlersBefore =
      target.Body.ExceptionHandlers.Count

    let state2 =
      AltCover.Instrument.I.doTrack
        state
        { Method = target
          VisibleMethod = target
          Inspection = Inspections.Track
          Track = Some(42, "hello")
          DefaultVisitCount = Exemption.None }

    Assert.That(state2.AsyncSupport |> Option.isSome)

    Assert.That( // Adding the return value, too
      target.Body.Instructions.Count,
      Is.EqualTo(countBefore + 9 + 4)
    )

    Assert.That(target.Body.ExceptionHandlers.Count, Is.EqualTo(handlersBefore + 1))

    let nopsAfter =
      target.Body.Instructions
      |> Seq.filter (fun i -> i.OpCode = OpCodes.Nop)
      |> Seq.length
    // add 2 extra nop now and replace rather than remove .tails
    Assert.That(nopsAfter, Is.EqualTo(nopsBefore + tailsBefore + 2))

  [<Test>]
  let ShouldBeAbleToTrackAnFSAsyncMethod () =
    use recstream = recorderStream ()

    let sample27 = sample27path

    use def =
      AssemblyResolver.ReadAssembly sample27

    use rdef =
      AssemblyResolver.ReadAssembly recstream

    let recorder =
      AltCover.Instrument.I.recordingMethod rdef

    let target =
      def.MainModule.GetType("Sample27.Tests").Methods
      |> Seq.find (fun m -> m.Name = "AddAsync")

    let raw =
      AltCover.InstrumentContext.Build([])

    let state =
      { raw with
          RecordingMethodRef =
            { Visit = null
              Push = recorder.[1]
              Pop = recorder.[2] } }

    let countBefore =
      target.Body.Instructions.Count

    let tailsBefore =
      target.Body.Instructions
      |> Seq.filter (fun i -> i.OpCode = OpCodes.Tail)
      |> Seq.length

    let nopsBefore =
      target.Body.Instructions
      |> Seq.filter (fun i -> i.OpCode = OpCodes.Nop)
      |> Seq.length

    let handlersBefore =
      target.Body.ExceptionHandlers.Count

    let state2 =
      AltCover.Instrument.I.doTrack
        state
        { Method = target
          VisibleMethod = target
          Inspection = Inspections.Track
          Track = Some(42, "hello")
          DefaultVisitCount = Exemption.None }

    Assert.That(state2.AsyncSupport |> Option.isSome)

    Assert.That( // Adding the return value, too
      target.Body.Instructions.Count,
      Is.EqualTo(countBefore + 9 + 5)
    )

    Assert.That(target.Body.ExceptionHandlers.Count, Is.EqualTo(handlersBefore + 1))

    let nopsAfter =
      target.Body.Instructions
      |> Seq.filter (fun i -> i.OpCode = OpCodes.Nop)
      |> Seq.length
    // add 2 extra nop now and replace rather than remove .tails
    Assert.That(nopsAfter, Is.EqualTo(nopsBefore + tailsBefore + 2))

  [<Test>]
  let ShouldBeAbleToTrackAnFSTaskMethod () =
    use recstream = recorderStream ()

    let sample30 = sample30path

    use def =
      AssemblyResolver.ReadAssembly sample30

    use rdef =
      AssemblyResolver.ReadAssembly recstream

    let recorder =
      AltCover.Instrument.I.recordingMethod rdef

    let target =
      def.MainModule.GetType("Sample30.Tests").Methods
      |> Seq.find (fun m -> m.Name = "AddTaskReturnsTheSumOfXAndY")

    let raw =
      AltCover.InstrumentContext.Build([])

    let state =
      { raw with
          RecordingMethodRef =
            { Visit = null
              Push = recorder.[1]
              Pop = recorder.[2] } }

    let countBefore =
      target.Body.Instructions.Count

    let tailsBefore =
      target.Body.Instructions
      |> Seq.filter (fun i -> i.OpCode = OpCodes.Tail)
      |> Seq.length

    let nopsBefore =
      target.Body.Instructions
      |> Seq.filter (fun i -> i.OpCode = OpCodes.Nop)
      |> Seq.length

    let handlersBefore =
      target.Body.ExceptionHandlers.Count

    let state2 =
      AltCover.Instrument.I.doTrack
        state
        { Method = target
          VisibleMethod = target
          Inspection = Inspections.Track
          Track = Some(42, "hello")
          DefaultVisitCount = Exemption.None }

    Assert.That(state2.AsyncSupport |> Option.isSome)

    Assert.That( // Adding the return value, too
      target.Body.Instructions.Count,
      Is.EqualTo(countBefore + 9 + 6)
    )

    Assert.That(target.Body.ExceptionHandlers.Count, Is.EqualTo(handlersBefore + 1))

    let nopsAfter =
      target.Body.Instructions
      |> Seq.filter (fun i -> i.OpCode = OpCodes.Nop)
      |> Seq.length
    // add 2 extra nop now and replace rather than remove .tails
    Assert.That(nopsAfter, Is.EqualTo(nopsBefore + tailsBefore + 2))

  [<Test>]
  let ShouldBeAbleToInstrumentASwitchForNCover () =
    use recstream = recorderStream ()

    let res =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("SwitchSample.dl_", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(res)

    let res2 =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("SwitchSample.pd_", StringComparison.Ordinal)

    use stream2 =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(res2)

    use def =
      AssemblyResolver.ReadAssembly stream

    let r = Mono.Cecil.Pdb.PdbReaderProvider()

    use rr =
      r.GetSymbolReader(def.MainModule, stream2)

    def.MainModule.ReadSymbols(rr)

    use rdef =
      AssemblyResolver.ReadAssembly recstream

    let recorder =
      AltCover.Instrument.I.recordingMethod rdef

    let target =
      def.MainModule.GetType("Sample15.Class1").Methods
      |> Seq.find (fun m -> m.Name = "OpenCoverSummary")

    let raw =
      AltCover.InstrumentContext.Build([])

    let state =
      { raw with
          RecordingMethodRef =
            { Visit = recorder.[1]
              Push = recorder.[1]
              Pop = recorder.[2] } }

    let switch =
      target.Body.Instructions
      |> Seq.find (fun i -> i.OpCode = OpCodes.Switch)

    let targets =
      switch.Operand :?> Instruction array
      |> Array.map _.Offset

    Assert.That(targets, Is.EquivalentTo [ 31; 33; 31; 33; 31 ])

    let m =
      Node.Method
        { Method = target
          VisibleMethod = target
          Inspection = Inspections.Instrument
          Track = None
          DefaultVisitCount = Exemption.None }

    let steps = Visitor.I.sequenceBuilder m

    Assert.That(steps, Is.Not.Empty)

    let visitors =
      [ Visitor.encloseState Instrument.I.instrumentationVisitor state ]

    steps
    |> Seq.fold Visitor.I.apply (visitors |> Seq.toList)
    |> ignore

    let switch2 =
      target.Body.Instructions
      |> Seq.find (fun i -> i.OpCode = OpCodes.Switch)

    let targets2 =
      switch2.Operand :?> Instruction array
      |> Array.map _.Offset

    let next = switch2.Next.Offset
    let n2 = next + 2
    // Need to check the heisenstate here

    //case of 43
    //IL_0000: ldstr ""
    //IL_0005: ldc.i4.s 24
    //IL_0007: call System.Void AltCover.Recorder.Instance::Push(System.Int32)
    //IL_000c: ldarg.0
    //IL_000d: call System.Int32 Sample15.TeamCityFormat::get_Tag()
    //IL_0012: switch IL_002b,IL_002d,IL_002b,IL_002d,IL_002b
    //IL_002b: br.s IL_0041

#if !NET472
    Assert.That(next, Is.GreaterThanOrEqualTo(42).And.LessThanOrEqualTo(46))

    let expected = next
#else
    let expected = 43
#endif
    //if next <> expected
    //then target.Body.Instructions
    //     |> Seq.iter (printfn "%A")
    Assert.That(next, Is.EqualTo expected)

    Assert.That(targets2, Is.EquivalentTo [ next; n2; next; n2; next ])

  [<Test>]
  let ShouldNotChangeAnUntrackedMethod () =
    use recstream = recorderStream ()

    use def =
      AssemblyResolver.ReadAssembly recstream

    let recorder =
      AltCover.Instrument.I.recordingMethod def

    let state =
      AltCover.InstrumentContext.Build([])

    let countBefore =
      recorder.Head.Body.Instructions.Count

    let handlersBefore =
      recorder.Head.Body.ExceptionHandlers.Count

    let state2 =
      AltCover.Instrument.I.doTrack
        state
        { Method = recorder.Head
          VisibleMethod = recorder.Head
          Inspection = Inspections.Track
          Track = None
          DefaultVisitCount = Exemption.None }

    Assert.That(state2.AsyncSupport |> Option.isNone)
    Assert.That(recorder.Head.Body.Instructions.Count, Is.EqualTo countBefore)
    Assert.That(recorder.Head.Body.ExceptionHandlers.Count, Is.EqualTo handlersBefore)

  [<Test>]
  let SwitchBranchesShouldInstrumentByPushingDown () =
    let where =
      Assembly.GetExecutingAssembly().Location

    let path =
      Path.Combine(Path.GetDirectoryName(where), "Sample2.dll")

    use def = AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols def

    let method =
      def.MainModule.GetAllTypes()
      |> Seq.collect _.Methods
      |> Seq.find (fun m -> m.Name = "as_bar")

    Visitor.visit [] [] // cheat reset

    try
      CoverageParameters.theReportFormat <- Some ReportFormat.OpenCover

      let branches =
        Visitor.I.deeper
        <| Node.Method
          { Method = method
            VisibleMethod = method
            Inspection = Inspections.Instrument
            Track = None
            DefaultVisitCount = Exemption.None }
        |> Seq.choose (fun n ->
          match n with
          | BranchPoint b -> Some b
          | _ -> None)
        |> Seq.take 2 // start of a switch
        |> Seq.toList

      match branches with
      | [ b1; b2 ] ->
        Assert.That(b1.Start.OpCode, Is.EqualTo OpCodes.Switch)
        Assert.That(b2.Start.OpCode, Is.EqualTo OpCodes.Switch)
        Assert.That(b1.Start.Offset, Is.EqualTo b2.Start.Offset)

      let raw =
        AltCover.InstrumentContext.Build([])

      let state =
        { raw with
            RecordingMethodRef =
              { Visit = method
                Push = null
                Pop = null }
            MethodWorker = method.Body.GetILProcessor() }

      let next = branches.Head.Start.Next

      branches
      |> Seq.iter (fun b -> Instrument.I.visitBranchPoint state b |> ignore)

      let inject =
        Seq.unfold
          (fun (state: Cil.Instruction) ->
            if isNull state || state = next then
              None
            else
              Some(state, state.Next))
          branches.Head.Start
        |> Seq.skip 1
        |> Seq.toList

      Assert.That(inject.Length, Is.EqualTo 8)

      let switches =
        branches.Head.Start.Operand :?> Instruction[]
        |> Seq.toList

      Assert.That(switches.[0], Is.EqualTo inject.[1])
      Assert.That(switches.[1], Is.EqualTo inject.[0])
      Assert.That(inject.[0].Operand, Is.EqualTo inject.[5])

      Assert.That(
        (inject.[2].Operand :?> int)
        &&& Counter.branchMask,
        Is.EqualTo 1
      )

      Assert.That(
        (inject.[6].Operand :?> int)
        &&& Counter.branchMask,
        Is.EqualTo 0
      )
    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let PseudoSwitchVisibleBranchesShouldSkipNonRepresentativeCases () =
    let where =
      Assembly.GetExecutingAssembly().Location

    let path =
      Path.Combine(Path.GetDirectoryName(where), "Sample16.dll")

    use def = AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols def
    CoverageParameters.coalesceBranches.Value <- true

    let method =
      def.MainModule.GetAllTypes()
      |> Seq.collect _.Methods
      |> Seq.find (fun m -> m.Name = "Bar")

    Visitor.visit [] [] // cheat reset

    try
      CoverageParameters.theReportFormat <- Some ReportFormat.OpenCover

      let branches =
        Visitor.I.deeper
        <| Node.Method
          { Method = method
            VisibleMethod = method
            Inspection = Inspections.Instrument
            Track = None
            DefaultVisitCount = Exemption.None }
        |> Seq.choose (fun n ->
          match n with
          | BranchPoint b -> Some b
          | _ -> None)
        |> Seq.skip 2
        |> Seq.take 2 // first of "switch"
        |> Seq.toList

      match branches with
      | [ b1; b2 ] ->
        //Assert.That(b1.Start.OpCode, Is.EqualTo OpCodes.Brfalse_S)
        //Assert.That(b2.Start.OpCode, Is.EqualTo OpCodes.Brfalse_S)

        Assert.That(b1.Start.Offset, Is.EqualTo b2.Start.Offset)

      let raw =
        AltCover.InstrumentContext.Build([])

      let state =
        { raw with
            RecordingMethodRef =
              { Visit = method
                Push = null
                Pop = null }
            MethodWorker = method.Body.GetILProcessor() }

      let next = branches.Head.Start.Next

      branches
      |> Seq.iter (fun b -> Instrument.I.visitBranchPoint state b |> ignore)

      let inject =
        Seq.unfold
          (fun (state: Cil.Instruction) ->
            if isNull state || state = next then
              None
            else
              Some(state, state.Next))
          branches.Head.Start
        |> Seq.skip 1
        |> Seq.toList

      Assert.That(inject.Length, Is.EqualTo 5)

      let jump =
        branches.Head.Start.Operand :?> Instruction

      Assert.That(jump, Is.EqualTo inject.[1])
      Assert.That(inject.[0].Operand, Is.EqualTo inject.[4].Next)

      Assert.That(
        (inject.[2].Operand :?> int)
        &&& Counter.branchMask,
        Is.EqualTo branches.[1].Uid
      )
    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None
      CoverageParameters.coalesceBranches.Value <- false

  [<Test>]
  let SimpleBranchShouldInstrumentByPushingDown () =
    let where =
      Assembly.GetExecutingAssembly().Location

    let path = sample1path

    use def = AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols def

    let method =
      def.MainModule.GetAllTypes()
      |> Seq.collect _.Methods
      |> Seq.find (fun m -> m.Name = "Main")

    Visitor.visit [] [] // cheat reset

    try
      CoverageParameters.theReportFormat <- Some ReportFormat.OpenCover

      let branches =
        Visitor.I.deeper
        <| Node.Method
          { Method = method
            VisibleMethod = method
            Inspection = Inspections.Instrument
            Track = None
            DefaultVisitCount = Exemption.None }
        |> Seq.choose (fun n ->
          match n with
          | BranchPoint b -> Some b
          | _ -> None)
        |> Seq.take 2 // start of a switch
        |> Seq.toList

      Assert.That(branches |> List.length, Is.EqualTo 2)

      let raw =
        AltCover.InstrumentContext.Build([])

      let state =
        { raw with
            RecordingMethodRef =
              { Visit = method
                Push = null
                Pop = null }
            MethodWorker = method.Body.GetILProcessor() }

      let next = branches.Head.Start.Next

      branches
      |> Seq.iter (fun b -> Instrument.I.visitBranchPoint state b |> ignore)

      let inject =
        Seq.unfold
          (fun (state: Cil.Instruction) ->
            if isNull state || state = next then
              None
            else
              Some(state, state.Next))
          branches.Head.Start
        |> Seq.skip 1
        |> Seq.toList

      Assert.That(inject.Length, Is.EqualTo 8)
      Assert.That(inject.[0].Operand, Is.EqualTo inject.[5])

      Assert.That(
        (inject.[2].Operand :?> int)
        &&& Counter.branchMask,
        Is.EqualTo 1
      )

      Assert.That(
        (inject.[6].Operand :?> int)
        &&& Counter.branchMask,
        Is.EqualTo 0
      )
    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let StartShouldLoadRecordingAssembly () =
    let def =
      Instrument.I.instrumentationVisitor (InstrumentContext.Build []) (Start [])

    Assert.That(def.RecordingAssembly.Name.Name, Is.EqualTo "AltCover.Recorder.g")

  [<Test>]
  let TypeShouldNotChangeState () =
    let input = InstrumentContext.Build []

    let output =
      Instrument.I.instrumentationVisitor
        input
        (Node.Type
          { Type = null
            VisibleType = null
            Inspection = Inspections.Ignore
            DefaultVisitCount = Exemption.None })

    Assert.That(output, Is.SameAs input)

  [<Test>]
  let ExcludedMethodShouldNotChangeState () =
    let input = InstrumentContext.Build []

    let output =
      Instrument.I.instrumentationVisitor
        input
        (Node.Method
          { Method = null
            VisibleMethod = null
            Inspection = Inspections.Ignore
            Track = None
            DefaultVisitCount = Exemption.None })

    Assert.That(output, Is.SameAs input)

  [<Test>]
  let IncludedMethodShouldChangeState () =
    let path = sample2path

    use def = AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols def
    let module' = def.MainModule.GetType("N.DU")

    let du =
      module'.NestedTypes
      |> Seq.filter (fun t -> t.Name = "MyUnion")
      |> Seq.head

    let func =
      du.GetMethods()
      |> Seq.find (fun x -> x.Name = "as_bar")

    let input = InstrumentContext.Build []

    let output =
      Instrument.I.instrumentationVisitor
        input
        (Node.Method
          { Method = func
            VisibleMethod = func
            Inspection = Inspections.Instrument
            Track = None
            DefaultVisitCount = Exemption.None })

    Assert.That(output.MethodBody, Is.SameAs func.Body)

  [<Test>]
  let ExcludedAfterMethodShouldNotChangeState () =
    let path = sample2path

    use def = AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols def
    let module' = def.MainModule.GetType("N.DU")

    let du =
      module'.NestedTypes
      |> Seq.filter (fun t -> t.Name = "MyUnion")
      |> Seq.head

    let func =
      du.GetMethods()
      |> Seq.find (fun x -> x.Name = "as_bar")

    let opcodes =
      func.Body.Instructions
      |> Seq.map _.OpCode
      |> Seq.toList

    let input =
      { InstrumentContext.Build [] with
          MethodBody = func.Body }

    input.MethodBody.SimplifyMacros()

    let paired =
      Seq.zip opcodes input.MethodBody.Instructions
      |> Seq.toList

    Assert.That(paired |> Seq.exists (fun (i, j) -> i <> j.OpCode))

    let diff =
      paired
      |> List.map (fun (i, j) -> (i, i = j.OpCode))

    let output =
      Instrument.I.instrumentationVisitor
        input
        (Node.AfterMethod
          { Method = func
            VisibleMethod = func
            Inspection = Inspections.Ignore
            Track = None
            DefaultVisitCount = Exemption.None })

    Assert.That(output, Is.SameAs input)

    let paired' =
      Seq.zip diff input.MethodBody.Instructions

    Assert.That(
      paired'
      |> Seq.forall (fun ((i, x), j) -> x = (i = j.OpCode))
    )

  [<Test>]
  let IncludedAfterMethodShouldRewriteMethod () =
    let path = sample2path

    use def = AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols def
    let module' = def.MainModule.GetType("N.DU")

    let du =
      module'.NestedTypes
      |> Seq.filter (fun t -> t.Name = "MyUnion")
      |> Seq.head

    let func =
      du.GetMethods()
      |> Seq.find (fun x -> x.Name = "as_bar")

    let opcodes =
      func.Body.Instructions
      |> Seq.map _.OpCode
      |> Seq.toList

    let input =
      { InstrumentContext.Build [] with
          MethodBody = func.Body }

    input.MethodBody.SimplifyMacros()

    let paired =
      Seq.zip opcodes input.MethodBody.Instructions

    Assert.That(paired |> Seq.exists (fun (i, j) -> i <> j.OpCode))

    let output =
      Instrument.I.instrumentationVisitor
        input
        (Node.AfterMethod
          { Method = func
            VisibleMethod = func
            Inspection = Inspections.Instrument
            Track = None
            DefaultVisitCount = Exemption.None })

    Assert.That(output, Is.SameAs input)

    let paired' =
      Seq.zip opcodes input.MethodBody.Instructions

    Assert.That(paired' |> Seq.forall (fun (i, j) -> i = j.OpCode))

  [<Test>]
  let NoStrongNameShouldUpdateVisibleTo () =
    let path = sample2path

    use def = AssemblyResolver.ReadAssembly path
    ProgramDatabase.readSymbols def

    CoverageParameters.defaultStrongNameKey <- None

    let result =
      Instrument.I.updateVisibleTo def

    let va =
      result.CustomAttributes
      |> Seq.filter (fun a ->
        a.AttributeType.FullName.Equals(
          "System.Runtime.CompilerServices.InternalsVisibleToAttribute"
        ))
      |> Seq.toList
      |> List.map (_.ConstructorArguments >> Seq.head)
      |> List.map (_.Value >> string)

    let expected =
      [ "AltCover.Tests.Visualizer, PublicKey=0024000004800000940000000602000000240000525341310004000001000100916443A2EE1D294E8CFA7666FB3F512D998D7CEAC4909E35EDB2AC1E104DE68890A93716D1D1931F7228AAC0523CACF50FD82CDB4CCF4FF4BF0DED95E3A383F4F371E3B82C45502CE74D7D572583495208C1905E0F1E8A3CCE66C4C75E4CA32E9A8F8DEE64E059C0DC0266E8D2CB6D7EBD464B47E062F80B63D390E389217FB7"
        "AltCover.Tests.Visualizer" ]

    test <@ va = expected @>

  [<Test>]
  let NewStrongNameShouldUpdateVisibleTo () =
    let path = sample2path

    use def = AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols def
    let token0 = def.Name.PublicKeyToken

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(ivtSnk)

    use buffer = new MemoryStream()
    stream.CopyTo(buffer)

    CoverageParameters.defaultStrongNameKey <-
      Some(StrongNameKeyData.Make(buffer.ToArray()))

    let result =
      Instrument.I.updateVisibleTo def

    let va =
      result.CustomAttributes
      |> Seq.filter (fun a ->
        a.AttributeType.FullName.Equals(
          "System.Runtime.CompilerServices.InternalsVisibleToAttribute"
        ))
      |> Seq.toList
      |> List.map (_.ConstructorArguments >> Seq.head)
      |> List.map (_.Value >> string)

    let expected =
      [ "AltCover.Tests.Visualizer, PublicKey=0024000004800000940000000602000000240000525341310004000001000100916443A2EE1D294E8CFA7666FB3F512D998D7CEAC4909E35EDB2AC1E104DE68890A93716D1D1931F7228AAC0523CACF50FD82CDB4CCF4FF4BF0DED95E3A383F4F371E3B82C45502CE74D7D572583495208C1905E0F1E8A3CCE66C4C75E4CA32E9A8F8DEE64E059C0DC0266E8D2CB6D7EBD464B47E062F80B63D390E389217FB7"
        "AltCover.Tests.Visualizer, PublicKey=0024000004800000940000000602000000240000525341310004000001000100F9A856F46C312969423375AB83A0509FA025FB6EC6C8CC5655AD7EEBADCF63011850CE7367C183B914C95CF7801FE7CB7DC836DBF661FC841CDB5401DFFD3DF995B2204A2CB7E3665DB1381BD966B1D15447424030640829C653DE4E47C54D79BDFA5620864CE46846B87A6901887F6CB5474BCEA43DDC12EA90DFC5DEC9DBAB" ]

    test <@ va = expected @>

  [<Test>]
  let UpdateStrongReferencesShouldChangeSigningKeyWherePossible () =
    let path = sample2path

    use def = AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols def
    let token0 = def.Name.PublicKeyToken

    use stream =
      typeof<AltCover.Node>.Assembly.GetManifestResourceStream(recorderSnk)

    use buffer = new MemoryStream()
    stream.CopyTo(buffer)

    CoverageParameters.defaultStrongNameKey <-
      Some(StrongNameKeyData.Make(buffer.ToArray()))

    let result =
      Instrument.I.updateStrongReferences def []

    let token1 = def.Name.PublicKeyToken
    Assert.That(token1, Is.Not.Null)
    Assert.That(token1, Is.Not.EquivalentTo(token0))

    let token' =
      String.Join(String.Empty, token1 |> Seq.map _.ToString("x2"))

    Assert.That(token', Is.EqualTo "4ebffcaabf10ce6a")
    Assert.That(result, Is.Empty)

  [<Test>]
  let UpdateStrongReferencesShouldChangeSigningKeyWherePossible2 () =
    let here =
      Assembly.GetExecutingAssembly().Location

    let path =
      Path.Combine(dir, Path.GetFileName(here))

    use def = AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols def
    let token0 = def.Name.PublicKeyToken

    use stream =
      typeof<AltCover.Node>.Assembly.GetManifestResourceStream(recorderSnk)

    use buffer = new MemoryStream()
    stream.CopyTo(buffer)

    CoverageParameters.defaultStrongNameKey <-
      Some(StrongNameKeyData.Make(buffer.ToArray()))

    use stream2 =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(infrastructureSnk)

    use buffer2 = new MemoryStream()
    stream2.CopyTo(buffer2)

    try
      CoverageParameters.add
      <| StrongNameKeyData.Make(buffer2.ToArray())

      let result =
        Instrument.I.updateStrongReferences def [ "Sample2" ]

      let token1 = def.Name.PublicKeyToken
      Assert.That(token1, Is.Not.Null)

      // If the current assembly is un-strongnamed at any time,
      // then empty token0 -> empty token1.
      // Fortunately, coverlet doesn't mess with this assembly
      Assert.That(token1, Is.Not.EquivalentTo(token0))

      let token' =
        String.Join(String.Empty, token1 |> Seq.map _.ToString("x2"))

      Assert.That(token', Is.EqualTo "4ebffcaabf10ce6a")
      Assert.That(result, Is.Empty)
    finally
      CoverageParameters.keys.Clear()

  [<Test>]
  let UpdateStrongReferencesShouldRemoveSigningKeyIfRequired () =
    let path = sample2path

    use def = AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols def
    let token0 = def.Name.PublicKeyToken
    CoverageParameters.defaultStrongNameKey <- None

    let result =
      Instrument.I.updateStrongReferences def [ "nunit.framework" ]

    let token1 = def.Name.PublicKeyToken
    Assert.That(token1, Is.Empty)
    Assert.That(token1, Is.Not.EquivalentTo(token0))
    let mapping (x: byte) = x.ToString("x2")
    let smapping = Seq.map mapping
    test <@ [| 0uy |] |> smapping |> Seq.toList = [ "00" ] @>

    let token' =
      String.Join(String.Empty, token1 |> smapping)

    Assert.That(token', Is.EqualTo String.Empty)
    Assert.That(result.Count, Is.EqualTo 1)
    let key = result.Keys |> Seq.head
    let value = result.Values |> Seq.head
    let ptr = key.LastIndexOf("=")
    Assert.That(key.Substring(0, ptr), Is.EqualTo(value.Substring(0, ptr)))
    Assert.That(value.Substring(ptr), Is.EqualTo "=null")

  [<Test>]
  let UpdateStrongReferencesShouldNotAddASigningKey () =
    let mdir =
      Path.Combine(SolutionDir(), "_Mono/Sample1")

    let path = Path.Combine(mdir, "Sample1.exe")

    maybeIgnore (fun () -> path |> File.Exists |> not)

    use def = AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols def

    use stream =
      typeof<AltCover.Node>.Assembly.GetManifestResourceStream(recorderSnk)

    use buffer = new MemoryStream()
    stream.CopyTo(buffer)

    CoverageParameters.defaultStrongNameKey <-
      Some(StrongNameKeyData.Make(buffer.ToArray()))

    let result =
      Instrument.I.updateStrongReferences def []

    let token1 = def.Name.PublicKeyToken
    Assert.That(token1, Is.Empty)
    Assert.That(result, Is.Empty)

  [<Test>]
  let UpdateStrongReferencesShouldTrackReferences () =
    let path = sample2path

    use def = AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols def |> ignore

    use stream =
      typeof<AltCover.Node>.Assembly.GetManifestResourceStream(recorderSnk)

    use buffer = new MemoryStream()
    stream.CopyTo(buffer)

    CoverageParameters.defaultStrongNameKey <-
      Some(StrongNameKeyData.Make(buffer.ToArray()))

    let result =
      Instrument.I.updateStrongReferences def [ "nunit.framework"; "nonesuch" ]

    Assert.That(result.Count, Is.EqualTo 1)

    Assert.That(result.Values |> Seq.head, Does.EndWith "PublicKeyToken=4ebffcaabf10ce6a")
    let key = result.Keys |> Seq.head

    Assert.That(
      result.Values |> Seq.head,
      Is.EqualTo(
        key.Substring(0, key.Length - 16)
        + "4ebffcaabf10ce6a"
      )
    )

  [<Test>]
  let UpdateStrongReferencesShouldTrackReferencesEvenFakes () =
    try
      let path = sample2path

      use def = AssemblyResolver.ReadAssembly path

      ProgramDatabase.readSymbols def |> ignore

      let npath =
        typeof<NUnit.Framework.TestAttribute>.Assembly.Location

      use ndef =
        AssemblyResolver.ReadAssembly npath

      let key =
        KeyStore.arrayToIndex ndef.Name.PublicKey

      use stream =
        typeof<AltCover.Node>.Assembly.GetManifestResourceStream(recorderSnk)

      use buffer = new MemoryStream()
      stream.CopyTo(buffer)

      let ourKeyPair =
        StrongNameKeyData.Make(buffer.ToArray())

      CoverageParameters.defaultStrongNameKey <- Some ourKeyPair
      CoverageParameters.keys.Add(key, { Pair = ourKeyPair; Token = [] })

      let result =
        Instrument.I.updateStrongReferences def [ "nunit.framework"; "nonesuch" ]

      Assert.That(result.Count, Is.EqualTo 1)

      Assert.That(
        result.Values |> Seq.head,
        Does.EndWith "PublicKeyToken=4ebffcaabf10ce6a"
      )

      let key = result.Keys |> Seq.head

      Assert.That(
        result.Values |> Seq.head,
        Is.EqualTo(
          key.Substring(0, key.Length - 16)
          + "4ebffcaabf10ce6a"
        )
      )
    finally
      CoverageParameters.keys.Clear()

  [<Test>]
  let ExcludedAssemblyRefsAreNotUpdated () =
    let path = sample2path

    use def = AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols def

    let refs =
      def.MainModule.AssemblyReferences |> Seq.toList

    use stream =
      typeof<AltCover.Node>.Assembly.GetManifestResourceStream(recorderSnk)

    use buffer = new MemoryStream()
    stream.CopyTo(buffer)

    CoverageParameters.defaultStrongNameKey <-
      Some(StrongNameKeyData.Make(buffer.ToArray()))

    use fake =
      AssemblyResolver.ReadAssembly(Assembly.GetExecutingAssembly().Location)

    let state =
      InstrumentContext.Build [ "nunit.framework"; "nonesuch" ]

    let visited =
      Node.Assembly
        { Assembly = def
          Inspection = Inspections.Ignore
          Identity = Hallmark.Build()
          Destinations = [] }

    let result =
      Instrument.I.instrumentationVisitor { state with RecordingAssembly = fake } visited

    Assert.That(def.MainModule.AssemblyReferences, Is.EquivalentTo refs)

  [<Test>]
  let IncludedAssemblyRefsAreUpdated () =
    // do trivial coverage here where the type's absence is felt
    let dummy =
      AltCover.Recorder.InstrumentationAttribute() // Constructor has all the instrumented code

    let path = sample2path

    use def = AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols def

    let refs =
      def.MainModule.AssemblyReferences |> Seq.toList

    use stream =
      typeof<AltCover.Node>.Assembly.GetManifestResourceStream(recorderSnk)

    use buffer = new MemoryStream()
    stream.CopyTo(buffer)

    CoverageParameters.defaultStrongNameKey <-
      Some(StrongNameKeyData.Make(buffer.ToArray()))

    use fake =
      AssemblyResolver.ReadAssembly(Assembly.GetExecutingAssembly().Location)

    let state =
      InstrumentContext.Build [ "nunit.framework"; "nonesuch" ]

    let visited =
      Node.Assembly
        { Assembly = def
          Inspection = Inspections.Instrument
          Identity = Hallmark.Build()
          Destinations = [] }

    let result =
      Instrument.I.instrumentationVisitor { state with RecordingAssembly = fake } visited

    Assert.That(def.MainModule.AssemblyReferences, Is.EquivalentTo(refs @ [ fake.Name ]))

  [<Test>]
  let ExcludedModuleJustRecordsMVid () =
    try
      CoverageParameters.theReportFormat <- Some ReportFormat.NCover

      let path = sample2path

      use def = AssemblyResolver.ReadAssembly path

      ProgramDatabase.readSymbols def

      let visited =
        Node.Module
          { Module = def.MainModule
            Inspection = Inspections.Ignore }

      let state =
        InstrumentContext.Build [ "nunit.framework"; "nonesuch" ]

      let result =
        Instrument.I.instrumentationVisitor state visited

      Assert.That(
        result,
        Is.EqualTo
          { state with
              ModuleId = def.MainModule.Mvid.ToString() }
      )
    finally
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let ExcludedModuleJustRecordsNameForJson () =
    try
      CoverageParameters.theReportFormat <- Some ReportFormat.NativeJson

      let path = sample2path

      use def = AssemblyResolver.ReadAssembly path

      ProgramDatabase.readSymbols def

      let visited =
        Node.Module
          { Module = def.MainModule
            Inspection = Inspections.Ignore }

      let state =
        InstrumentContext.Build [ "nunit.framework"; "nonesuch" ]

      let result =
        Instrument.I.instrumentationVisitor state visited

      Assert.That(result, Is.EqualTo { state with ModuleId = "Sample2.dll" })
    finally
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let ExcludedModuleJustRecordsHashForOpenCover () =
    try
      CoverageParameters.theReportFormat <- Some ReportFormat.OpenCover

      let path = sample2path

      use def = AssemblyResolver.ReadAssembly path

      ProgramDatabase.readSymbols def

      let visited =
        Node.Module
          { Module = def.MainModule
            Inspection = Inspections.Ignore }

      let state =
        InstrumentContext.Build [ "nunit.framework"; "nonesuch" ]

      let result =
        Instrument.I.instrumentationVisitor state visited

      Assert.That(
        result,
        Is.EqualTo
          { state with
              ModuleId = path |> KeyStore.hashFile }
      )
    finally
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let IncludedModuleEnsuresRecorder () =
    try
      CoverageParameters.theReportFormat <- Some ReportFormat.NCover

      let path = sample2path

      use def = AssemblyResolver.ReadAssembly path

      ProgramDatabase.readSymbols def

      let visited =
        Node.Module
          { Module = def.MainModule
            Inspection = Inspections.Instrument }

      let state =
        InstrumentContext.Build [ "nunit.framework"; "nonesuch" ]

      use recstream = recorderStream ()

      use def' =
        AssemblyResolver.ReadAssembly recstream

      let visit =
        def'.MainModule.GetAllTypes()
        |> Seq.filter (fun t -> t.FullName = "AltCover.Recorder.Instance")
        |> Seq.collect _.Methods
        |> Seq.filter (fun m ->
          m.Name = "Visit"
          || m.Name = "Push"
          || m.Name = "Pop")
        |> Seq.sortBy _.Name
        |> Seq.toList
        |> List.rev

      let state' =
        { state with RecordingAssembly = def' }

      let result =
        Instrument.I.instrumentationVisitor state' visited

      test <@ result.RecordingMethodRef.Visit.Module = def.MainModule @>
      test <@ string result.RecordingMethodRef.Visit = (visit |> Seq.head |> string) @>

      test
        <@
          string result.RecordingMethodRef.Push = (visit
                                                   |> Seq.skip 1
                                                   |> Seq.head
                                                   |> string)
        @>

      test
        <@
          string result.RecordingMethodRef.Pop = (visit
                                                  |> Seq.skip 2
                                                  |> Seq.head
                                                  |> string)
        @>

      test
        <@
          { result with
              RecordingMethodRef =
                { Visit = null
                  Push = null
                  Pop = null } } = { state' with
                                       ModuleId = def.MainModule.Mvid.ToString()
                                       RecordingMethod = visit
                                       RecordingMethodRef =
                                         { Visit = null
                                           Push = null
                                           Pop = null } }
        @>
    finally
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let ExcludedMethodPointIsPassThrough () =
    let path = sample2path

    use def = AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols def

    let visited =
      Node.MethodPoint
        { Instruction = null
          SeqPnt = None
          Uid = 0
          Interesting = false
          DefaultVisitCount = Exemption.None }

    let state = InstrumentContext.Build []

    let result =
      Instrument.I.instrumentationVisitor state visited

    Assert.That(result, Is.SameAs state)

  [<Test>]
  let IncludedMethodPointInsertsVisit () =
    let path = sample2path

    use def = AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols def
    let module' = def.MainModule.GetType("N.DU")

    let du =
      module'.NestedTypes
      |> Seq.filter (fun t -> t.Name = "MyUnion")
      |> Seq.head

    let main =
      du.GetMethods()
      |> Seq.find (fun x -> x.Name = "as_bar")

    let proc = main.Body.GetILProcessor()
    let dbg = main.DebugInformation

    let target =
      main.Body.Instructions
      |> Seq.filter (dbg.GetSequencePoint >> isNull >> not)
      |> Seq.head

    let visited =
      Node.MethodPoint
        { Instruction = target
          SeqPnt = None
          Uid = 32767
          Interesting = true
          DefaultVisitCount = Exemption.None }

    Assert.That(target.Previous, Is.Null)

    let state =
      { (InstrumentContext.Build []) with
          MethodWorker = proc
          MethodBody = main.Body
          RecordingMethodRef =
            { Visit = def.MainModule.ImportReference main
              Push = null
              Pop = null } }

    let result =
      Instrument.I.instrumentationVisitor state visited

    Assert.That(result, Is.SameAs state)
    Assert.That(target.Previous.OpCode, Is.EqualTo OpCodes.Call)

  [<Test>]
  let IncludedModuleDoesNotChangeRecorderJustTheReference () =
    try
      CoverageParameters.theReportFormat <- Some ReportFormat.NCover

      let where =
        Assembly.GetExecutingAssembly().Location

      let path = sample2path

      use def = AssemblyResolver.ReadAssembly path

      ProgramDatabase.readSymbols def

      let visited =
        Node.Module
          { Module = def.MainModule
            Inspection = Inspections.Instrument }

      let state =
        InstrumentContext.Build [ "nunit.framework"; "nonesuch" ]

      use recstream = recorderStream ()

      use def' =
        AssemblyResolver.ReadAssembly recstream

      let visit =
        def'.MainModule.GetAllTypes()
        |> Seq.collect _.Methods
        |> Seq.filter (fun m -> m.Name = "Visit")
        |> Seq.head

      use def'' =
        AssemblyResolver.ReadAssembly where

      let v =
        def''.MainModule.ImportReference visit

      let r = { Visit = v; Push = v; Pop = v }

      let state' =
        { state with
            RecordingAssembly = def'
            RecordingMethod = [ visit; visit; visit ]
            RecordingMethodRef = r
            AsyncSupport = visit |> AsyncSupport.Update |> Some }

      let async = state'.AsyncSupport.Value
      let waitBefore = async.Wait
      // let localBefore = async.LocalWait

      let result =
        Instrument.I.instrumentationVisitor state' visited

      let asyncAfter = result.AsyncSupport.Value

      // let ref'' = def.MainModule.ImportReference visit
      // let localExpect = def.MainModule.ImportReference waitBefore
      Assert.That(result.RecordingMethodRef.Visit.Module, Is.EqualTo(def.MainModule))
      Assert.That(async.LocalWait.Module, Is.Not.EqualTo(def.MainModule))
      Assert.That(asyncAfter.LocalWait.Module, Is.SameAs(def.MainModule))
      Assert.That(asyncAfter.Wait, Is.SameAs(waitBefore))
      Assert.That(string result.RecordingMethodRef, Is.EqualTo(string r))

      Assert.That(
        { result with
            RecordingMethodRef = RecorderRefs.Build() },
        Is.EqualTo
          { state' with
              ModuleId = def.MainModule.Mvid.ToString()
              RecordingMethod = [ visit; visit; visit ]
              RecordingMethodRef = RecorderRefs.Build()
              AsyncSupport = Some asyncAfter }
      )
    finally
      CoverageParameters.theReportFormat <- None

  [<Test>]
  let AfterModuleShouldNotChangeState () =
    let input = InstrumentContext.Build []

    let output =
      Instrument.I.instrumentationVisitor input AfterModule

    Assert.That(output, Is.SameAs input)

  [<Test>]
  let JSONInjectionTransformsSimpleFileAsExpected () =
    let inputName =
      infrastructureSnk.Replace("Infrastructure.snk", "Sample8.deps.baseline.json")

    let resultName =
      infrastructureSnk.Replace("Infrastructure.snk", "Sample8.deps.newtonsoft.json")

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(inputName)

    use reader = new StreamReader(stream)

    let result =
      Instrument.I.injectJSON <| reader.ReadToEnd()

    use stream' =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resultName)

    use reader' = new StreamReader(stream')
    let expected = reader'.ReadToEnd()

    let version =
      AltCover.AssemblyVersionInformation.AssemblyVersion

    let transform (s: string) =
      s
        .Replace("\r\n", "\n")
        .Replace("AltCover.Recorder.g/7.1.0.0", "AltCover.Recorder.g/" + version)
        .Replace(
          "AltCover.Recorder.g\": \"7.1.0.0",
          "AltCover.Recorder.g\": \"" + version
        )

    let r = transform result
    Assert.That(r, Is.EqualTo(transform expected), r)

  [<Test>]
  let JSONInjectionTransformsStandaloneFileAsExpected () =
    let inputName =
      infrastructureSnk.Replace("Infrastructure.snk", "Sample1.deps.json")

    let resultName =
      infrastructureSnk.Replace("Infrastructure.snk", "Sample1.deps.after.json")

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(inputName)

    use reader = new StreamReader(stream)

    let result =
      reader.ReadToEnd() |> Instrument.I.injectJSON
    // |> Instrument.I.injectJSON
    use stream' =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resultName)

    use reader' = new StreamReader(stream')
    let expected = reader'.ReadToEnd()

    let version =
      AltCover.AssemblyVersionInformation.AssemblyVersion

    let transform (s: string) =
      s
        .Replace("\r\n", "\n")
        .Replace("AltCover.Recorder.g/1.4.0.0", "AltCover.Recorder.g/" + version)
        .Replace(
          "AltCover.Recorder.g\": \"1.4.0.0",
          "AltCover.Recorder.g\": \"" + version
        )

    let r = transform result
    Assert.That(r, Is.EqualTo(transform expected), r)

  [<Test>]
  let JSONInjectionTransformsDependencyFileAsExpected () =
    let inputName =
      infrastructureSnk.Replace("Infrastructure.snk", "Sample2.deps.json")

    let resultName =
      infrastructureSnk.Replace("Infrastructure.snk", "Sample2.deps.after.json")

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(inputName)

    use reader = new StreamReader(stream)

    let result =
      reader.ReadToEnd() |> Instrument.I.injectJSON
    //|> Instrument.I.injectJSON

    use stream' =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resultName)

    use reader' = new StreamReader(stream')
    let expected = reader'.ReadToEnd()

    let version =
      AltCover.AssemblyVersionInformation.AssemblyVersion

    let transform (s: string) =
      s
        .Replace("\r\n", "\n")
        .Replace("AltCover.Recorder.g/2.0.0.0", "AltCover.Recorder.g/" + version)
        .Replace(
          "AltCover.Recorder.g\": \"2.0.0.0",
          "AltCover.Recorder.g\": \"" + version
        )

    let r = transform result
    Assert.That(r, Is.EqualTo(transform expected), r)

  [<Test>]
  let JSONInjectionIsIdempotent () =
    let resultName =
      infrastructureSnk.Replace("Infrastructure.snk", "Sample1.deps.after.json")

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resultName)

    use reader = new StreamReader(stream)
    let expected = reader.ReadToEnd()

    let recorderVersion () =
      use stream = recorderStream ()

      use def =
        AssemblyResolver.ReadAssembly stream

      def.Name.Version.ToString()

    let version = recorderVersion ()

    let result =
      expected
      |> Instrument.I.injectJSON
      |> Instrument.I.injectJSON

    let r = result.Replace("\r\n", "\n")

    Assert.That(
      r,
      Is.EqualTo(expected.Replace("\r\n", "\n").Replace("1.4.0.0", version)),
      r
    )

  [<Test>]
  let NonFinishShouldDisposeRecordingAssembly () =
    let path = Path.Combine(dir, "Sample3.dll")

    use prepared =
      AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols prepared

    let state =
      { InstrumentContext.Build [] with
          RecordingAssembly = prepared }

    Assert.Throws<InvalidOperationException>(fun () ->
      ignore (
        Instrument.I.instrumentationVisitorWrapper
          (fun _ _ -> InvalidOperationException("Bang") |> raise)
          state
          AfterType
      ))
    |> ignore

    let output = Path.GetTempFileName()
    let outputdll = output + ".dll"

    try
      Assert.Throws<ArgumentException>(fun () ->
        Instrument.I.writeAssembly prepared outputdll)
      |> ignore
    finally
      Directory.EnumerateFiles(
        Path.GetDirectoryName output,
        (Path.GetFileNameWithoutExtension output) + ".*"
      )
      |> Seq.iter (fun f -> maybeIOException (fun () -> File.Delete f))

  [<Test>]
  let NonFinishShouldDisposeThreadingAssembly () =
    let path = Path.Combine(dir, "Sample3.dll")

    use prepared =
      AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols prepared

    let md =
      prepared.MainModule.Types
      |> Seq.filter (fun t -> t.FullName = "AltCover.Sample3.Class3")
      |> Seq.collect _.Methods
      |> Seq.filter (fun m -> m.Name = "Log")
      |> Seq.head

    let support = AsyncSupport.Update md

    let state =
      { InstrumentContext.Build [] with
          RecordingAssembly = prepared
          AsyncSupport = Some support }

    Assert.Throws<InvalidOperationException>(fun () ->
      ignore (
        Instrument.I.instrumentationVisitorWrapper
          (fun _ _ -> InvalidOperationException("Bang") |> raise)
          state
          AfterType
      ))
    |> ignore

    Assert.That(support.TaskAssembly.FullName, Is.Not.Null) // nothing to raise an object disposed exception with
    let output = Path.GetTempFileName()
    let outputdll = output + ".dll"

    try
      Assert.Throws<ArgumentException>(fun () ->
        Instrument.I.writeAssembly prepared outputdll)
      |> ignore
    finally
      Directory.EnumerateFiles(
        Path.GetDirectoryName output,
        (Path.GetFileNameWithoutExtension output) + ".*"
      )
      |> Seq.iter (fun f -> maybeIOException (fun () -> File.Delete f))

  [<Test>]
  let NonFinishShouldNotDisposeNullRecordingAssembly () =
    let path = Path.Combine(dir, "Sample3.dll")

    let state =
      { InstrumentContext.Build [] with
          RecordingAssembly = null }
    // Would be NullreferenceException if we tried it
    Assert.Throws<InvalidOperationException>(fun () ->
      ignore (
        Instrument.I.instrumentationVisitorWrapper
          (fun _ _ -> InvalidOperationException("Bang") |> raise)
          state
          AfterType
      ))
    |> ignore

  [<Test>]
  let FinishShouldLeaveRecordingAssembly () =
    let path = Path.Combine(dir, "Sample3.dll")

    let state =
      { InstrumentContext.Build [] with
          RecordingAssembly = null }

    use prepared =
      AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols prepared

    Assert.Throws<InvalidOperationException>(fun () ->
      ignore (
        Instrument.I.instrumentationVisitorWrapper
          (fun _ _ -> InvalidOperationException("Bang") |> raise)
          state
          Finish
      ))
    |> ignore

    let output = Path.GetTempFileName()
    let outputdll = output + ".dll"

    try
      Instrument.I.writeAssembly prepared outputdll
    finally
      Directory.EnumerateFiles(
        Path.GetDirectoryName output,
        (Path.GetFileNameWithoutExtension output) + ".*"
      )
      |> Seq.iter (fun f -> maybeIOException (fun () -> File.Delete f))

  [<Test>]
  let AfterAssemblyCommitsThatAssembly () =
    let path = sample4path

    let def = AssemblyResolver.ReadAssembly path

    use recstream = recorderStream ()

    use recdef =
      AssemblyResolver.ReadAssembly recstream

    ProgramDatabase.readSymbols def
    let unique = Guid.NewGuid().ToString()

    let output =
      Path.Combine(Path.GetDirectoryName(dir), unique)

    Directory.CreateDirectory(output) |> ignore

    let saved =
      CoverageParameters.theOutputDirectories
      |> Seq.toList

    try
      CoverageParameters.theOutputDirectories.Clear()
      CoverageParameters.theOutputDirectories.Add output

      let visited =
        Node.AfterAssembly
          { Assembly = def
            Inspection = Inspections.Instrument
            Identity = Hallmark.Build()
            Destinations = CoverageParameters.outputDirectories () }

      let input =
        { InstrumentContext.Build [] with
            RecordingAssembly = recdef
            RecorderSource = recstream }

      let result =
        Instrument.I.instrumentationVisitor input visited

      test' <@ Object.ReferenceEquals(result, input) @> "result differs"

      let created =
        Path.Combine(output, "Sample4.dll")

      test' <@ File.Exists created @> (created + " not found")

    finally
      CoverageParameters.theOutputDirectories.AddRange saved

  [<Test>]
  let AfterAssemblyCommitsThatAssemblyForMono () =
    // Hack for running while instrumented
    let where =
      Assembly.GetExecutingAssembly().Location

    let path = monoSample1path
    maybeIgnore (fun () -> path |> File.Exists |> not)

    let def = AssemblyResolver.ReadAssembly path

    use recstream = recorderStream ()

    use recdef =
      AssemblyResolver.ReadAssembly recstream

    ProgramDatabase.readSymbols def
    let unique = Guid.NewGuid().ToString()

    let output =
      Path.Combine(Path.GetDirectoryName(where), unique)

    Directory.CreateDirectory(output) |> ignore

    let saved =
      CoverageParameters.theOutputDirectories
      |> Seq.toList

    try
      CoverageParameters.theOutputDirectories.Clear()
      CoverageParameters.theOutputDirectories.AddRange [ output ]

      let visited =
        Node.AfterAssembly
          { Assembly = def
            Inspection = Inspections.Instrument
            Identity = Hallmark.Build()
            Destinations = CoverageParameters.outputDirectories () }

      let input =
        { InstrumentContext.Build [] with
            RecordingAssembly = recdef
            RecorderSource = recstream }

      let result =
        Instrument.I.instrumentationVisitor input visited

      test' <@ Object.ReferenceEquals(result, input) @> "result differs"

      let created =
        Path.Combine(output, "Sample1.exe")

      test' <@ File.Exists created @> (created + " not found")

    finally
      CoverageParameters.theOutputDirectories.Clear()
      CoverageParameters.theOutputDirectories.AddRange saved

  [<Test>]
  let FinishCommitsTheRecordingAssembly () =
    let where =
      Assembly.GetExecutingAssembly().Location

    let path = Path.Combine(dir, "Sample3.dll")

    let def = AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols def
    let unique = Guid.NewGuid().ToString()

    let output =
      Path.Combine(Path.GetDirectoryName(dir), unique)

    Directory.CreateDirectory(output) |> ignore

    let saved =
      CoverageParameters.theOutputDirectories
      |> Seq.toList

    try
      CoverageParameters.theOutputDirectories.Clear()
      CoverageParameters.theOutputDirectories.Add output

      let input =
        { InstrumentContext.Build [] with
            RecordingAssembly = def }

      let result =
        Instrument.I.instrumentationVisitor input Finish

      test <@ result.RecordingAssembly |> isNull @>

      let created =
        Path.Combine(output, "Sample3.dll")

      test' <@ File.Exists created @> (created + " not found")

    finally
      CoverageParameters.theOutputDirectories.Clear()
      CoverageParameters.theOutputDirectories.AddRange saved

  [<Test>]
  let FinishCommitsTheAsyncRecordingAssembly () =
    let path = sample4path

    let def = AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols def

    use from =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Engine.Tests.AltCover.Recorder.net20.dll")

    use recorder =
      AssemblyResolver.ReadAssembly from

    ProgramDatabase.readSymbols recorder

    let md =
      def.MainModule.Types
      |> Seq.filter (fun t -> t.FullName = "Tests.M")
      |> Seq.collect _.Methods
      |> Seq.filter (fun m -> m.Name = "makeThing")
      |> Seq.head

    let support = AsyncSupport.Update md

    let unique = Guid.NewGuid().ToString()

    let output =
      Path.Combine(Path.GetDirectoryName(dir), unique)

    Directory.CreateDirectory(output) |> ignore

    let saved =
      CoverageParameters.theOutputDirectories
      |> Seq.toList

    try
      CoverageParameters.theOutputDirectories.Clear()
      CoverageParameters.theOutputDirectories.Add output

      let input =
        { InstrumentContext.Build [] with
            RecordingAssembly = recorder
            AsyncSupport = Some support }

      let result =
        Instrument.I.instrumentationVisitor input Finish

      test <@ result.RecordingAssembly |> isNull @>
      test <@ result.AsyncSupport |> Option.isNone @>

      let created =
        Path.Combine(output, "AltCover.Recorder.dll")

      test' <@ File.Exists created @> (created + " not found")
      printfn "%A" created

      let alc =
        new TestAssemblyLoadContext(
          "FinishCommitsTheAsyncRecordingAssembly",
          created |> Path.GetDirectoryName
        )

      try
#if !NET472
        let assembly =
          alc.LoadFromAssemblyPath(created) //LoadFrom loads dependent DLLs (assuming they are in the app domain's base directory
#else
        let assembly = Assembly.LoadFrom(created) //LoadFrom loads dependent DLLs (assuming they are in the app domain's base directory
#endif

        let t =
          assembly.DefinedTypes
          |> Seq.filter (fun t -> t.FullName = "AltCover.Recorder.Instance")
          |> Seq.head

        let t1 =
          t.GetNestedType("I", BindingFlags.NonPublic)

        let t2 =
          t1.GetNestedType("CallTrack", BindingFlags.NonPublic)

        let p =
          t2.GetProperty("Value", BindingFlags.NonPublic ||| BindingFlags.Static)

        let v = p.GetValue(nullObject)

        test <@ v.IsNotNull @>
        test <@ v.GetType() = typeof<System.Threading.AsyncLocal<Stack<int>>> @>

        let m =
          t2.GetMethod("Instance", BindingFlags.NonPublic ||| BindingFlags.Static)

        let v2 = m.Invoke(nullObject, [||])

        test <@ v2.IsNotNull @>
        test <@ v2.GetType() = typeof<Stack<int>> @>

      finally
        alc.Unload()

    finally
      CoverageParameters.theOutputDirectories.Clear()
      CoverageParameters.theOutputDirectories.AddRange saved