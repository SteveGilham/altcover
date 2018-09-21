namespace AltCover

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Xml.Linq

open AltCover
open Mono.Options
open Newtonsoft.Json.Linq
open Xunit

module XTests =
  let Hack() =
    let where = Assembly.GetExecutingAssembly().Location

    let dir =
      where
      |> Path.GetDirectoryName
      |> Path.GetFileName
    match dir.IndexOf "__" with
    | 0 -> "/.."
    | _ -> String.Empty

  let SolutionDir() =
#if NETCOREAPP2_0
    let where = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    where.Substring(0, where.IndexOf("_Binaries"))
#else
    SolutionRoot.location
#endif

#if NETCOREAPP2_0
  let sample1 = "Sample1.dll"
  let monoSample1 = "../_Mono/Sample1"
#else
  let sample1 = "Sample1.exe"
  let monoSample1 = "_Mono/Sample1"
  let recorderSnk = typeof<AltCover.Node>.Assembly.GetManifestResourceNames()
                    |> Seq.find (fun n -> n.EndsWith(".Recorder.snk", StringComparison.Ordinal))
#endif

  let MonoBaseline = "<?xml-stylesheet type='text/xsl' href='coverage.xsl'?>
<coverage profilerVersion=\"0\" driverVersion=\"0\" startTime=\"\" measureTime=\"\">
  <module moduleId=\"\" name=\"Sample1.exe\" assembly=\"Sample1\" assemblyIdentity=\"Sample1, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null\">
    <method name=\"Main\" class=\"TouchTest.Program\" metadataToken=\"0\" excluded=\"false\" instrumented=\"true\" fullname=\"System.Void TouchTest.Program.Main(System.String[])\">
      <seqpnt visitcount=\"0\" line=\"11\" column=\"9\" endline=\"11\" endcolumn=\"10\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"12\" column=\"32\" endline=\"12\" endcolumn=\"33\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"13\" column=\"13\" endline=\"13\" endcolumn=\"14\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"13\" column=\"21\" endline=\"13\" endcolumn=\"22\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"14\" column=\"13\" endline=\"14\" endcolumn=\"14\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"15\" column=\"17\" endline=\"15\" endcolumn=\"18\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"15\" column=\"72\" endline=\"15\" endcolumn=\"73\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"15\" column=\"25\" endline=\"15\" endcolumn=\"26\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"16\" column=\"13\" endline=\"16\" endcolumn=\"14\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"18\" column=\"13\" endline=\"18\" endcolumn=\"14\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"19\" column=\"17\" endline=\"19\" endcolumn=\"18\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"19\" column=\"25\" endline=\"19\" endcolumn=\"26\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"20\" column=\"13\" endline=\"20\" endcolumn=\"14\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
      <seqpnt visitcount=\"0\" line=\"21\" column=\"9\" endline=\"21\" endcolumn=\"10\" excluded=\"false\" document=\"Sample1\\Program.cs\" />
    </method>
  </module>
</coverage>"

  let rec RecursiveValidate result expected depth zero =
    let rcount = result |> Seq.length
    let ecount = expected |> Seq.length
    Assert.Equal(rcount, (ecount)) //, "Mismatch at depth " + depth.ToString())
    Seq.zip result expected
    |> Seq.iter (fun (r : XElement, e : XElement) ->
         Assert.Equal(r.Name, (e.Name)) //, "Expected name " + e.Name.ToString())
         let ra = r.Attributes()
         let ea = e.Attributes()
         Seq.zip ra ea
         |> Seq.iter (fun (a1 : XAttribute, a2 : XAttribute) ->
              Assert.Equal(a1.Name, (a2.Name))
              match a1.Name.ToString() with
              | "profilerVersion" | "driverVersion" | "moduleId" | "metadataToken" | "startTime" | "measureTime" ->
                ()
              | "document" ->
                Assert.True
                  (a1.Value.Replace("\\", "/").EndsWith(a2.Value.Replace("\\", "/")),
                   a1.Name.ToString() + " : " + r.ToString() + " -> document")
              | "visitcount" ->
                let expected =
                  if zero then "0"
                  else a2.Value
                Assert.Equal(a1.Value, (expected)) //, r.ToString() + " -> visitcount")
              | _ -> Assert.Equal(a1.Value, (a2.Value)) //, r.ToString() + " -> " + a1.Name.ToString())
                                                        )
         RecursiveValidate (r.Elements()) (e.Elements()) (depth + 1) zero)

  let rec RecursiveValidateOpenCover result expected' depth zero expectSkipped =
    let X name = XName.Get(name)
    let rcount = result |> Seq.length

    let expected =
      expected'
      |> Seq.filter (fun (el : XElement) ->
           el.Name.LocalName <> "Module" || expectSkipped || "skippedDueTo"
                                                             |> X
                                                             |> el.Attributes
                                                             |> Seq.isEmpty)
      |> Seq.toList

    let ecount = expected |> Seq.length
    Assert.Equal(rcount, (ecount)) //, "Mismatch at depth " + depth.ToString() + " : " +
    //        expected.ToString() + " but got" + (result |> Seq.toList).ToString())
    Seq.zip result expected
    |> Seq.iter
         (fun (r : XElement, e : XElement) ->
         Assert.Equal(r.Name, (e.Name)) //, "Expected name " + e.Name.ToString())
         let ra = r.Attributes()
         let ea = e.Attributes()
         Seq.zip ra ea
         |> Seq.iter (fun (a1 : XAttribute, a2 : XAttribute) ->
              Assert.Equal(a1.Name, (a2.Name))
              match a1.Name.ToString() with
              | "bev" | "visited" | "visitedSequencePoints" | "visitedBranchPoints" | "visitedClasses" | "visitedMethods" | "sequenceCoverage" | "branchCoverage" | "uspid" | "minCrapScore" | "maxCrapScore" | "crapScore" | "hash" ->
                ()
              | "fullPath" ->
                Assert.True
                  (a1.Value.Replace("\\", "/").EndsWith(a2.Value.Replace("\\", "/")),
                   a1.Name.ToString() + " : " + r.ToString() + " -> document")
              | "vc" ->
                let expected =
                  if zero then "0"
                  else a2.Value
                Assert.Equal(a1.Value, (expected)) //, r.ToString() + " -> visitcount")
              | _ -> Assert.Equal(a1.Value, (a2.Value)) //, r.ToString() + " -> " + a1.Name.ToString())
                                                        )
         RecursiveValidateOpenCover (r.Elements()) (e.Elements()) (depth + 1) zero
           expectSkipped)

  [<Fact>]
  let CollectParamsCanBeValidated() =
    let test = { CollectParams.Default with Threshold = "23" }
    let scan = test.Validate(false)
    Assert.Equal(0, scan.Length)

  [<Fact>]
  let CollectParamsCanBeValidatedWithErrors() =
    let test = CollectParams.Default
    let scan = test.Validate(true)
    Assert.Equal(1, scan.Length)

  [<Fact>]
  let CollectParamsCanBePositivelyValidatedWithErrors() =
    let test =
      { CollectParams.Default with RecorderDirectory = Guid.NewGuid().ToString() }
    let scan = test.Validate(true)
    Assert.Equal(2, scan.Length)

  [<Fact>]
  let PrepareParamsCanBeValidated() =
    let here = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName

    let test =
      { PrepareParams.Default with InputDirectory = here
                                   OutputDirectory = here
                                   SymbolDirectories = [| here |]
                                   Dependencies =
                                     [| Assembly.GetExecutingAssembly().Location |]
                                   CallContext = [| "[Fact]" |]
                                   PathFilter = [| "ok" |] }

    let scan = test.Validate()
    Assert.Equal(0, scan.Length)

  [<Fact>]
  let PrepareParamsStrongNamesCanBeValidated() =
    let input = Path.Combine(AltCover.SolutionRoot.location, "Build/Infrastructure.snk")

    let test =
      { PrepareParams.Default with StrongNameKey = input
                                   Keys = [| input |] }

    let scan = test.Validate()
#if NETCOREAPP2_0
    ()
#else
    Assert.Equal (0, scan.Length)
#endif

  [<Fact>]
  let PrepareParamsCanBeValidatedWithNulls() =
    let test = { PrepareParams.Default with CallContext = null }
    let scan = test.Validate()
    Assert.Equal(0, scan.Length)

  [<Fact>]
  let PrepareParamsCanBeValidatedAndDetectInconsistency() =
    let test =
      { PrepareParams.Default with BranchCover = true
                                   LineCover = true
                                   Single = true
                                   CallContext = [| "0" |] }

    let scan = test.Validate()
    Assert.Equal(2, scan.Length)

  [<Fact>]
  let PrepareParamsCanBeValidatedWithErrors() =
    let test =
      { PrepareParams.Default with XmlReport = String(Path.GetInvalidPathChars())
                                   CallContext = [| "0"; "1" |] }

    let scan = test.Validate()
    Assert.Equal(2, scan.Length)

  [<Fact>]
  let NullListsAreEmpty() =
    let test = Args.ItemList String.Empty null
    Assert.True(test |> List.isEmpty)

  [<Fact>]
  let ADotNetDryRunLooksAsExpected() =
    let where = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    let here = SolutionDir()
    let path = Path.Combine(here, "_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.1")
    let key0 = Path.Combine(here, "Build/SelfTest.snk")
#if NETCOREAPP2_1
    let input =
      if Directory.Exists path then path
      else
        Path.Combine
          (where.Substring(0, where.IndexOf("_Binaries")),
           "../_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.0")

    let key =
      if File.Exists key0 then key0
      else
        Path.Combine
          (where.Substring(0, where.IndexOf("_Binaries")), "../Build/SelfTest.snk")
#else
    let input = path
    let key = key0
#endif
    let unique = Guid.NewGuid().ToString()
    let unique' = Path.Combine(where, Guid.NewGuid().ToString())
    Directory.CreateDirectory unique' |> ignore
    let report = Path.Combine(unique', "ADotNetDryRunLooksAsExpected.xml")
    let output = Path.Combine(Path.GetDirectoryName(where), unique)
    let outputSaved = Visitor.outputDirectory
    let inputSaved = Visitor.inputDirectory
    let reportSaved = Visitor.reportPath
    let keySaved = Visitor.defaultStrongNameKey
    let saved = (Console.Out, Console.Error)
    Visitor.keys.Clear()
    let save2 = (Output.Info, Output.Error)
    try
      Output.Error <- CommandLine.WriteErr
      Output.Info <- CommandLine.WriteOut
      use stdout = new StringWriter()
      use stderr = new StringWriter()
      Console.SetOut stdout
      Console.SetError stderr
      let args =
        [| "-i"; input; "-o"; output; "-x"; report; "--opencover"
#if NETCOREAPP2_1
#else
           "-sn"; key
#endif
           "-s=Adapter";
           "-s=nunit"; "-e=Sample"; "-c=[Test]"; "--save" |]
      let result = Main.DoInstrumentation args
      Assert.Equal(result, 0)
      Assert.Empty(stderr.ToString())
      let expected =
        "Creating folder " + output + "\nInstrumenting files from "
        + (Path.GetFullPath input) + "\nWriting files to " + output + "\n   => "
        + Path.Combine(Path.GetFullPath input, "Sample4.dll") + "\n\nCoverage Report: "
        + report + "\n\n\n    " + Path.Combine(Path.GetFullPath output, "Sample4.dll")
        + "\n                <=  Sample4, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null\n"
      Assert.Equal
        (stdout.ToString().Replace("\r\n", "\n").Replace("\\", "/"),
         (expected.Replace("\\", "/")))
      Assert.Equal(Visitor.OutputDirectory(), output)
      Assert.Equal
        (Visitor.InputDirectory().Replace("\\", "/"),
         ((Path.GetFullPath input).Replace("\\", "/")))
      Assert.Equal(Visitor.ReportPath(), report)
      use stream = new FileStream(key, FileMode.Open)
      use buffer = new MemoryStream()
      stream.CopyTo(buffer)
      let snk = StrongNameKeyPair(buffer.ToArray())
#if NETCOREAPP2_1
      Assert.Equal(Visitor.keys.Count, 0)
#else
      Assert.True (Visitor.keys.ContainsKey(KeyStore.KeyToIndex snk))
      Assert.Equal (Visitor.keys.Count, 1)
#endif

      Assert.True(File.Exists report)
      Assert.True(File.Exists(report + ".acv"))
      let pdb = Path.ChangeExtension(Assembly.GetExecutingAssembly().Location, ".pdb")
      let isWindows =
#if NETCOREAPP2_1
                        true
#else
                        System.Environment.GetEnvironmentVariable("OS") = "Windows_NT"
#endif
      let expected =
        [ "AltCover.Recorder.g.dll"
#if NETCOREAPP2_1
          "FSharp.Core.dll"
#endif
          "AltCover.Recorder.g.pdb";
          "Sample4.deps.json"; "Sample4.dll"; "Sample4.runtimeconfig.dev.json";
          "Sample4.runtimeconfig.json"; "Sample4.pdb";
          "xunit.runner.reporters.netcoreapp10.dll";
          "xunit.runner.utility.netcoreapp10.dll";
          "xunit.runner.visualstudio.dotnetcore.testadapter.dll" ]
#if NETCOREAPP2_1
#else
        |> List.filter (fun f -> isWindows || f = "Sample4.pdb" ||
                                 f = "Sample1.exe.mdb" ||
                                 (f.EndsWith("db", StringComparison.Ordinal) |> not))
#endif
      let theFiles =
        if pdb
           |> File.Exists
           |> not
        then
          List.concat [ expected
                        [ "AltCover.Recorder.g.dll.mdb"; "Sample4.dll.mdb" ] ]
          |> List.filter (fun f -> f.EndsWith(".g.pdb", StringComparison.Ordinal) |> not)
          |> List.filter
               (fun f ->
               isWindows || f = "Sample4.pdb"
               || (f.EndsWith("db", StringComparison.Ordinal) |> not))
          |> List.sortBy (fun f -> f.ToUpperInvariant())
        else expected |> List.sortBy (fun f -> f.ToUpperInvariant())

      let actualFiles =
        Directory.GetFiles(output)
        |> Seq.map Path.GetFileName
        |> Seq.filter (fun f -> f.EndsWith(".tmp", StringComparison.Ordinal) |> not)
        |> Seq.sortBy (fun f -> f.ToUpperInvariant())
        |> Seq.toList

      Assert.Equal<IEnumerable<String>>(theFiles, actualFiles)
    finally
      Output.Usage("dummy", OptionSet(), OptionSet())
      Visitor.TrackingNames.Clear()
      Visitor.reportFormat <- None
      Visitor.outputDirectory <- outputSaved
      Visitor.inputDirectory <- inputSaved
      Visitor.reportPath <- reportSaved
      Visitor.defaultStrongNameKey <- keySaved
      Console.SetOut(fst saved)
      Console.SetError(snd saved)
      Visitor.keys.Clear()
      Visitor.NameFilters.Clear()
      Output.Error <- snd save2
      Output.Info <- fst save2
    let before = File.ReadAllText(Path.Combine(input, "Sample4.deps.json"))
    Assert.Equal(before.IndexOf("AltCover.Recorder.g"), -1)
    let o = JObject.Parse(File.ReadAllText(Path.Combine(output, "Sample4.deps.json")))
    let target =
      ((o.Property "runtimeTarget").Value :?> JObject).Property("name").Value.ToString()
    let targets =
      (o.Properties() |> Seq.find (fun p -> p.Name = "targets")).Value :?> JObject
    let targeted =
      (targets.Properties() |> Seq.find (fun p -> p.Name = target)).Value :?> JObject
    let app = (targeted.PropertyValues() |> Seq.head) :?> JObject
    let existingDependencies =
      app.Properties() |> Seq.tryFind (fun p -> p.Name = "dependencies")

    let reset =
      match existingDependencies with
      | None -> Set.empty<string>
      | Some p ->
        (p.Value :?> JObject).Properties()
        |> Seq.map (fun p -> p.Name)
        |> Set.ofSeq
    Assert.True(reset |> Set.contains "AltCover.Recorder.g")
    let aux =
      targeted.Properties()
      |> Seq.map (fun p -> p.Name)
      |> Set.ofSeq
    Assert.True
      (aux
       |> Set.contains
            ("AltCover.Recorder.g/" + System.AssemblyVersionInformation.AssemblyVersion))
    let libraries =
      (o.Properties() |> Seq.find (fun p -> p.Name = "libraries")).Value :?> JObject

    let lib =
      libraries.Properties()
      |> Seq.map (fun p -> p.Name)
      |> Set.ofSeq
    Assert.True
      (lib
       |> Set.contains
            ("AltCover.Recorder.g/" + System.AssemblyVersionInformation.AssemblyVersion))

  [<Fact>]
  let ADryRunLooksAsExpected() =
    let where = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    let here = SolutionDir()
    let path = Path.Combine(here, "_Mono/Sample1")
    let key0 = Path.Combine(here, "Build/SelfTest.snk")
#if NETCOREAPP2_1
    let input =
      if Directory.Exists path then path
      else Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), monoSample1)

    let key =
      if File.Exists key0 then key0
      else
        Path.Combine
          (where.Substring(0, where.IndexOf("_Binaries")), "../Build/SelfTest.snk")
#else
    let input = path
    let key = key0
#endif
    let unique = Guid.NewGuid().ToString()
    let unique' = Path.Combine(where, Guid.NewGuid().ToString())
    Directory.CreateDirectory unique' |> ignore
    let report = Path.Combine(unique', "ADryRunLooksAsExpected.xml")
    let output = Path.Combine(Path.GetDirectoryName(where), unique)
    let outputSaved = Visitor.outputDirectory
    let inputSaved = Visitor.inputDirectory
    let reportSaved = Visitor.reportPath
    let keySaved = Visitor.defaultStrongNameKey
    let saved = (Console.Out, Console.Error)
    let save2 = (Output.Info, Output.Error)
    Visitor.keys.Clear()
    try
      Output.Error <- CommandLine.WriteErr
      Output.Info <- CommandLine.WriteOut
      use stdout = new StringWriter()
      use stderr = new StringWriter()
      Console.SetOut stdout
      Console.SetError stderr
      let args = [| "-i"; input; "-o"; output; "-x"; report
#if NETCOREAPP2_1
#else
                    "-sn"; key
#endif
                 |]
      let result = Main.DoInstrumentation args
      Assert.Equal(result, 0)
      Assert.Empty(stderr.ToString())
      let expected =
        "Creating folder " + output + "\nInstrumenting files from "
        + (Path.GetFullPath input) + "\nWriting files to " + output + "\n   => "
        + Path.Combine(Path.GetFullPath input, "Sample1.exe") + "\n\nCoverage Report: "
        + report + "\n\n\n    " + Path.Combine(Path.GetFullPath output, "Sample1.exe")
        + "\n                <=  Sample1, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null\n"
      let console = stdout.ToString()
      Assert.Equal
        (console.Replace("\r\n", "\n").Replace("\\", "/"), (expected.Replace("\\", "/")))
      Assert.Equal(Visitor.OutputDirectory(), output)
      Assert.Equal
        (Visitor.InputDirectory().Replace("\\", "/"),
         ((Path.GetFullPath input).Replace("\\", "/")))
      Assert.Equal(Visitor.ReportPath(), report)
      use stream = new FileStream(key, FileMode.Open)
      use buffer = new MemoryStream()
      stream.CopyTo(buffer)
      let snk = StrongNameKeyPair(buffer.ToArray())
#if NETCOREAPP2_1
      Assert.Equal(Visitor.keys.Count, 0)
#else
      Assert.True (Visitor.keys.ContainsKey(KeyStore.KeyToIndex snk))
      Assert.Equal (Visitor.keys.Count, 1)
#endif

      Assert.True(File.Exists report)
      let pdb = Path.ChangeExtension(Assembly.GetExecutingAssembly().Location, ".pdb")
      let isWindows =
#if NETCOREAPP2_1
                        true
#else
                        System.Environment.GetEnvironmentVariable("OS") = "Windows_NT"
#endif

      let theFiles =
        if File.Exists(pdb) then
          [ "AltCover.Recorder.g.dll"; "AltCover.Recorder.g.pdb"
#if NETCOREAPP2_1
            "FSharp.Core.dll"
#endif
            "Sample1.exe"; "Sample1.exe.mdb" ]
           // See Instrument.WriteAssembly
#if NETCOREAPP2_1
#else
          |> List.filter (fun f -> isWindows || f = "Sample1.exe.mdb" ||
                                   (f.EndsWith("db", StringComparison.Ordinal) |> not))
#endif
        else
          [ "AltCover.Recorder.g.dll"; "AltCover.Recorder.g.dll.mdb"; "Sample1.exe";
            "Sample1.exe.mdb" ]
          |> List.filter
               (fun f ->
               isWindows || f = "Sample1.exe.mdb"
               || (f.EndsWith("db", StringComparison.Ordinal) |> not))

      let actual =
        Directory.GetFiles(output)
        |> Seq.map Path.GetFileName
        |> Seq.toList
        |> List.sortBy (fun f -> f.ToUpperInvariant())

      Assert.Equal<IEnumerable<String>>(theFiles, actual)
      let expectedXml = XDocument.Load(new System.IO.StringReader(MonoBaseline))
      let recordedXml = Runner.LoadReport report
      RecursiveValidate (recordedXml.Elements()) (expectedXml.Elements()) 0 true
    finally
      Visitor.outputDirectory <- outputSaved
      Visitor.inputDirectory <- inputSaved
      Visitor.reportPath <- reportSaved
      Visitor.defaultStrongNameKey <- keySaved
      Console.SetOut(fst saved)
      Console.SetError(snd saved)
      Visitor.keys.Clear()
      Output.Error <- snd save2
      Output.Info <- fst save2

  [<Fact>]
  let AfterAssemblyCommitsThatAssembly() =
    let hack = Path.Combine(SolutionDir(), "_Binaries/AltCover.XTests/Debug+AnyCPU")
    let local = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName

    let where =
      if local.IndexOf("_Binaries") > 0 then local
      else hack

    let path = Path.Combine(where + Hack(), "Sample4.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def
    let unique = Guid.NewGuid().ToString()
    let output = Path.Combine(Path.GetDirectoryName(where), unique)
    Directory.CreateDirectory(output) |> ignore
    let saved = Visitor.outputDirectory
    try
      Visitor.outputDirectory <- Some output
      let visited = Node.AfterAssembly def
      let input = InstrumentContext.Build []
      let result = Instrument.InstrumentationVisitor input visited
      Assert.Same(result, input) //, "result differs")
      let created = Path.Combine(output, "Sample4.dll")
      Assert.True(File.Exists created, created + " not found")
      let pdb = Path.ChangeExtension(Assembly.GetExecutingAssembly().Location, ".pdb")
      if File.Exists pdb then
        let isWindows =
#if NETCOREAPP2_0
                        true
#else
                        System.Environment.GetEnvironmentVariable("OS") = "Windows_NT"
#endif
        Assert.True
          (isWindows
           |> not
           || File.Exists(Path.ChangeExtension(created, ".pdb")),
           created + " pdb not found")
    finally
      Visitor.outputDirectory <- saved

  [<Fact>]
  let AfterAssemblyCommitsThatAssemblyForMono() =
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
    let here = SolutionDir()
    let path = Path.Combine(here, "_Mono/Sample1/Sample1.exe")
#if NETCOREAPP2_0

    let path' =
      if File.Exists path then path
      else
        Path.Combine
          (where.Substring(0, where.IndexOf("_Binaries")) + monoSample1, "Sample1.exe")
#else
    let path' = path
#endif

    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path'
    ProgramDatabase.ReadSymbols def
    let unique = Guid.NewGuid().ToString()
    let output = Path.Combine(Path.GetDirectoryName(where), unique)
    Directory.CreateDirectory(output) |> ignore
    let saved = Visitor.outputDirectory
    try
      Visitor.outputDirectory <- Some output
      let visited = Node.AfterAssembly def
      let input = InstrumentContext.Build []
      let result = Instrument.InstrumentationVisitor input visited
      Assert.Same(result, input) //, "result differs")
      let created = Path.Combine(output, "Sample1.exe")
      Assert.True(File.Exists created, created + " not found")
      let isDotNet =
#if NETCOREAPP2_0
                     true
#else
                     System.Environment.GetEnvironmentVariable("OS") = "Windows_NT"
#endif

      if isDotNet then
        Assert.True(File.Exists(created + ".mdb"), created + ".mdb not found")
    finally
      Visitor.outputDirectory <- saved

  [<Fact>]
  let FinishCommitsTheRecordingAssembly() =
    let hack = Path.Combine(SolutionDir(), "_Binaries/AltCover.XTests/Debug+AnyCPU")
    let local = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName

    let where =
      if local.IndexOf("_Binaries") > 0 then local
      else hack

    let path = Path.Combine(where + Hack(), "Sample4.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.ReadSymbols def
    let unique = Guid.NewGuid().ToString()
    let output = Path.Combine(Path.GetDirectoryName(where), unique)
    Directory.CreateDirectory(output) |> ignore
    let saved = Visitor.outputDirectory
    try
      Visitor.outputDirectory <- Some output
      let input = { InstrumentContext.Build [] with RecordingAssembly = def }
      let result = Instrument.InstrumentationVisitor input Finish
      Assert.True(result.RecordingAssembly |> isNull)
      let created = Path.Combine(output, "Sample4.dll")
      Assert.True(File.Exists created, created + " not found")
#if NETCOREAPP2_0
      Assert.True(File.Exists(Path.Combine(output, "FSharp.Core.dll")), "Core not found")
#else
      let pdb = Path.ChangeExtension(Assembly.GetExecutingAssembly().Location, ".pdb")
      if File.Exists pdb then
        let isWindows =
#if NETCOREAPP2_0
                        true
#else
                        System.Environment.GetEnvironmentVariable("OS") = "Windows_NT"
#endif
        Assert.True (isWindows |> not ||
                     File.Exists (Path.ChangeExtension(created, ".pdb")), created + " pdb not found")
#endif

    finally
      Visitor.outputDirectory <- saved

  [<Fact>]
  let ShouldDoCoverage() =
    let start = Directory.GetCurrentDirectory()
    let hack = Path.Combine(SolutionDir(), "_Binaries/AltCover.XTests/Debug+AnyCPU")
    let local = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName

    let here =
      if local.IndexOf("_Binaries") > 0 then local
      else hack

    let where = Path.Combine(here, Guid.NewGuid().ToString())
    Directory.CreateDirectory(where) |> ignore
    Directory.SetCurrentDirectory where
    let create = Path.Combine(where, "AltCover.Recorder.g.dll")
    if create
       |> File.Exists
       |> not
    then
      do let from = Path.Combine(here, "AltCover.Recorder.dll")
         let updated = Instrument.PrepareAssembly from
         Instrument.WriteAssembly updated create
    let save = Runner.RecorderName
    let save1 = Runner.GetPayload
    let save2 = Runner.GetMonitor
    let save3 = Runner.DoReport
    let codedreport = "coverage.xml" |> Path.GetFullPath
    let alternate = "not-coverage.xml" |> Path.GetFullPath
    try
      Runner.RecorderName <- "AltCover.Recorder.g.dll"
      let payload (rest : string list) =
        Assert.Equal(rest, [| "test"; "1" |])
        255

      let monitor (hits : ICollection<string * int * Base.Track>) (token : string) _ _ =
        Assert.Equal(token, codedreport) //, "should be default coverage file")
        Assert.Empty(hits)
        127

      let write (hits : ICollection<string * int * Base.Track>) format (report : string)
          (output : String option) =
        Assert.Equal(report, codedreport) //, "should be default coverage file")
        Assert.Equal(output, Some alternate)
        Assert.Empty(hits)
        TimeSpan.Zero

      Runner.GetPayload <- payload
      Runner.GetMonitor <- monitor
      Runner.DoReport <- write
      let empty = OptionSet()
      let dummy = codedreport + ".xx.acv"
      do use temp = File.Create dummy
         dummy
         |> File.Exists
         |> Assert.True
      let r =
        Runner.DoCoverage
          [| "Runner"; "-x"; "test"; "-r"; where; "-o"; alternate; "--"; "1" |] empty
      dummy
      |> File.Exists
      |> not
      |> Assert.True
      Assert.Equal(r, 127)
    finally
      Runner.GetPayload <- save1
      Runner.GetMonitor <- save2
      Runner.DoReport <- save3
      Runner.RecorderName <- save
      Directory.SetCurrentDirectory start

  [<Fact>]
  let ShouldGenerateExpectedXmlReportFromMono() =
    let visitor, document = Report.ReportGenerator()
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
    let here = SolutionDir()
    let path = Path.Combine(here, "_Mono/Sample1/Sample1.exe")
#if NETCOREAPP2_0
    let path' =
      if File.Exists path then path
      else
        Path.Combine
          (where.Substring(0, where.IndexOf("_Binaries")) + monoSample1, "Sample1.exe")
#else
    let path' = path
#endif
    Visitor.Visit [ visitor ] (Visitor.ToSeq path')
    let baseline = XDocument.Load(new System.IO.StringReader(MonoBaseline))
    let result = document.Elements()
    let expected = baseline.Elements()
    RecursiveValidate result expected 0 true

  [<Fact>]
  let ShouldGenerateExpectedXmlReportFromMonoOpenCoverStyle() =
    let visitor, document = OpenCover.ReportGenerator()
    // Hack for running while instrumented
    let here = SolutionDir()
    let path = Path.Combine(here, "_Mono/Sample1/Sample1.exe")
#if NETCOREAPP2_0
    let where = Assembly.GetExecutingAssembly().Location

    let path' =
      if File.Exists path then path
      else
        Path.Combine
          (where.Substring(0, where.IndexOf("_Binaries")) + monoSample1, "Sample1.exe")
#else
    let path' = path
#endif
    try
      Visitor.NameFilters.Clear()
      Visitor.reportFormat <- Some Base.ReportFormat.OpenCover
      Visitor.Visit [ visitor ] (Visitor.ToSeq path')
      let resource =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find
             (fun n -> n.EndsWith("HandRolledMonoCoverage.xml", StringComparison.Ordinal))
      use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
      let baseline = XDocument.Load(stream)
      let result = document.Elements()
      let expected = baseline.Elements()
      RecursiveValidateOpenCover result expected 0 true false
    finally
      Visitor.NameFilters.Clear()
      Visitor.reportFormat <- None