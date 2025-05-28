namespace Tests
// fsharplint:disable  MemberNames NonPublicValuesNames RedundantNewKeyword

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Text.RegularExpressions
open System.Xml.Linq

open AltCover
open Mono.Options
open Newtonsoft.Json.Linq

#nowarn "25"

module AltCoverTests =

#if !NET472
  let dir =
    Path.Combine(SolutionDir(), "_Binaries/AltCover.Tests/Debug+AnyCPU/net9.0")
#else
  let dir =
    Path.Combine(SolutionDir(), "_Binaries/AltCover.Tests/Debug+AnyCPU/net472")
#endif

[<AutoOpen>]
module HallmarkExtra =
  type Hallmark with
    static member internal Build() =
      { Assembly = String.Empty
        Configuration = String.Empty }

module AltCoverXTests =

#if !NET472
  type TestAssemblyLoadContext(_dummy: string, _dummy2: string) =
    inherit System.Runtime.Loader.AssemblyLoadContext(true)
    override self.Load(name: AssemblyName) = null

#else
  type TestAssemblyLoadContext(domain: string, where: string) =
    member self.Unload() = ()
#endif

  let recorderStream () =
    let recorder =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("AltCover.Recorder.net20.dll", StringComparison.Ordinal)

    Assembly.GetExecutingAssembly().GetManifestResourceStream(recorder)

  let monoSample1path =
    Path.Combine(SolutionDir(), "_Mono/Sample1/Sample1.exe")

  let MonoBaseline =
    "<?xml-stylesheet type='text/xsl' href='coverage.xsl'?>
<coverage profilerVersion=\"0\" driverVersion=\"0\" startTime=\"\" measureTime=\"\">
  <module moduleId=\"\" name=\"Sample1.exe\" assembly=\"Sample1\" assemblyIdentity=\"Sample1, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null\">
    <method name=\"Main\" class=\"TouchTest.Program\" metadataToken=\"0\" excluded=\"false\" instrumented=\"true\" fullname=\"System.Void TouchTest.Program.Main(System.String[])\">
      <seqpnt visitcount=\"0\" line=\"11\" column=\"3\" endline=\"11\" endcolumn=\"4\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"12\" column=\"23\" endline=\"12\" endcolumn=\"24\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"13\" column=\"4\" endline=\"13\" endcolumn=\"5\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"13\" column=\"12\" endline=\"13\" endcolumn=\"13\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"14\" column=\"4\" endline=\"14\" endcolumn=\"5\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"15\" column=\"5\" endline=\"15\" endcolumn=\"6\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"15\" column=\"60\" endline=\"15\" endcolumn=\"61\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"15\" column=\"13\" endline=\"15\" endcolumn=\"14\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"16\" column=\"4\" endline=\"16\" endcolumn=\"5\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"18\" column=\"4\" endline=\"18\" endcolumn=\"5\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"19\" column=\"5\" endline=\"19\" endcolumn=\"6\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"19\" column=\"13\" endline=\"19\" endcolumn=\"14\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"20\" column=\"4\" endline=\"20\" endcolumn=\"5\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"21\" column=\"3\" endline=\"21\" endcolumn=\"4\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
    </method>
  </module>
</coverage>"

  [<TailCall>]
  let rec RecursiveValidate result expected depth zero =
    let rcount = result |> Seq.length
    let ecount = expected |> Seq.length
    test' <@ rcount = ecount @> ("Mismatch at depth " + depth.ToString())

    Seq.zip result expected
    |> Seq.iter (fun (r: XElement, e: XElement) ->
      test <@ r.Name = e.Name @>
      let ra = r.Attributes()
      let ea = e.Attributes()

      Seq.zip ra ea
      |> Seq.iter (fun (a1: XAttribute, a2: XAttribute) ->
        test <@ a1.Name = a2.Name @>

        match a1.Name.ToString() with
        | "profilerVersion"
        | "driverVersion"
        | "moduleId"
        | "metadataToken"
        | "startTime"
        | "measureTime" -> ()
        | "document" ->
          test'
            <@ a1.Value.Replace("\\", "/").EndsWith(a2.Value.Replace("\\", "/")) @>
            (a1.Name.ToString()
             + " : "
             + r.ToString()
             + " -> document")
        | "visitcount" ->
          let expected = Maybe zero "0" a2.Value
          test' <@ expected = a1.Value @> (r.ToString() + " -> visitcount")
        | _ ->
          test'
            <@ a1.Value.Replace("\\", "/") = a2.Value.Replace("\\", "/") @>
            (r.ToString() + " -> " + a1.Name.ToString()))

      RecursiveValidate (r.Elements()) (e.Elements()) (depth + 1) zero)

  [<TailCall>]
  let rec RecursiveValidateOpenCover result expected' depth zero expectSkipped =
    let xn name = XName.Get(name)
    let rcount = result |> Seq.length

    let expected =
      expected'
      |> Seq.filter (fun (el: XElement) ->
        el.Name.LocalName <> "Module"
        || expectSkipped
        || "skippedDueTo"
           |> xn
           |> el.Attributes
           |> Seq.isEmpty)
      |> Seq.toList

    let ecount = expected |> Seq.length

    test'
      <@ rcount = ecount @>
      ("Mismatch at depth "
       + depth.ToString()
       + " : "
       + expected.ToString()
       + " but got"
       + (result |> Seq.toList).ToString())

    Seq.zip result expected
    |> Seq.iter (fun (r: XElement, e: XElement) ->
      test <@ r.Name = e.Name @>
      let ra = r.Attributes()
      let ea = e.Attributes()

      Seq.zip ra ea
      |> Seq.iter (fun (a1: XAttribute, a2: XAttribute) ->
        test <@ a1.Name = a2.Name @>

        match a1.Name.ToString() with
        | "bev"
        | "visited"
        | "visitedSequencePoints"
        | "visitedBranchPoints"
        | "visitedClasses"
        | "visitedMethods"
        | "sequenceCoverage"
        | "branchCoverage"
        | "uspid"
        | "minCrapScore"
        | "maxCrapScore"
        | "crapScore"
        | "hash" -> ()
        | "fullPath" ->
          test'
            <@
              a1.Value
                .Replace("\\", "/")
                .Replace("altcover", "AltCover")
                .Replace("Samples/", String.Empty)
                .EndsWith(a2.Value.Replace("\\", "/").Replace("altcover", "AltCover"))
            @>
            (a1.Name.ToString()
             + " : "
             + r.ToString()
             + " -> document")
        | "vc" ->
          let expected = Maybe zero "0" a2.Value
          test' <@ expected = a1.Value @> (r.ToString() + " -> visitcount")
        | _ ->
          test' <@ a1.Value = a2.Value @> (r.ToString() + " -> " + a1.Name.ToString()))

      RecursiveValidateOpenCover
        (r.Elements())
        (e.Elements())
        (depth + 1)
        zero
        expectSkipped)

  [<Test>]
  let NullListsAreEmpty () =
    let subject =
      Args.itemList String.Empty null

    test <@ subject |> List.isEmpty @>

  [<Test>]
  let ValidateAssemblyOption () =
    test
      <@
        Assembly.GetExecutingAssembly()
        |> Some
        |> Main.I.isMSBuild
        |> not
      @>

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

  [<Test>]
  let AfterAssemblyCommitsThatAssembly () =
    let path =
      Path.Combine(AltCoverTests.dir, "Sample4.dll")

    let def = AssemblyResolver.ReadAssembly path

    use recstream = recorderStream ()

    use recdef =
      AssemblyResolver.ReadAssembly recstream

    ProgramDatabase.readSymbols def
    let unique = Guid.NewGuid().ToString()

    let output =
      Path.Combine(Path.GetDirectoryName(AltCoverTests.dir), unique)

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

    let path =
      Path.Combine(AltCoverTests.dir, "Sample3.dll")

    let def = AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols def
    let unique = Guid.NewGuid().ToString()

    let output =
      Path.Combine(Path.GetDirectoryName(AltCoverTests.dir), unique)

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
    let path =
      Path.Combine(AltCoverTests.dir, "Sample4.dll")

    let def = AssemblyResolver.ReadAssembly path

    ProgramDatabase.readSymbols def

    use from =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Tests.AltCover.Recorder.net20.dll")

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
      Path.Combine(Path.GetDirectoryName(AltCoverTests.dir), unique)

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

  [<Test>]
  let ShouldDoCoverage () =
    let start = Directory.GetCurrentDirectory()

    let where =
      Path.Combine(AltCoverTests.dir, Guid.NewGuid().ToString())

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
            .GetManifestResourceStream("AltCover.Tests.AltCover.Recorder.net20.dll")

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
            .GetManifestResourceStream("AltCover.Tests.GenuineNCover158.Xml")

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

  [<Test>]
  let ShouldGenerateExpectedXmlReportFromMono () =
    let visitor, documentSource =
      Report.reportGenerator ()

    let path = monoSample1path
    maybeIgnore (fun () -> path |> File.Exists |> not)

    Visitor.visit
      [ visitor ]
      (Visitor.I.toSeq
        { AssemblyPath = path
          Identity = Hallmark.Build()
          Destinations = [] })

    let expectedText =
      MonoBaseline.Replace(
        "name=\"Sample1.exe\"",
        "name=\"" + (path |> Path.GetFullPath) + "\""
      )

    let baseline =
      XDocument.Load(new System.IO.StringReader(expectedText))

    let document =
      use stash = new MemoryStream()
      stash |> documentSource
      stash.Position <- 0L
      XDocument.Load stash

    let result = document.Elements()
    let expected = baseline.Elements()
    RecursiveValidate result expected 0 true

  [<Test>]
  let ShouldGenerateExpectedXmlReportFromMonoOpenCoverStyle () =
    let visitor, documentSource =
      OpenCover.reportGenerator ()

    let path = monoSample1path
    maybeIgnore (fun () -> path |> File.Exists |> not)

    try
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- Some ReportFormat.OpenCover

      Visitor.visit
        [ visitor ]
        (Visitor.I.toSeq
          { AssemblyPath = path
            Identity = Hallmark.Build()
            Destinations = [] })

      let resource =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find _.EndsWith("HandRolledMonoCoverage.xml", StringComparison.Ordinal)

      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

      let baseline = XDocument.Load(stream)

      ("ModulePath"
       |> XName.Get
       |> baseline.Descendants
       |> Seq.head)
        .SetValue
        path

      let document =
        use stash = new MemoryStream()
        stash |> documentSource
        stash.Position <- 0L
        XDocument.Load stash

      let result = document.Elements()
      let expected = baseline.Elements()
      RecursiveValidateOpenCover result expected 0 true false
    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None

#if !NET472
  let runnerInit () = AltCover.Runner.init ()
  let mainInit () = AltCover.Main.init ()
#endif