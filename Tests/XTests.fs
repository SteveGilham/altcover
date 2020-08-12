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
open Swensen.Unquote

module AltCoverXTests =
  let test' x message =
    try
      test x
    with fail ->
      AssertionFailedException(message + Environment.NewLine + fail.Message, fail)
      |> raise

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
    SolutionRoot.location

  let monoSample1path = Path.Combine(SolutionDir(), "_Mono/Sample1/Sample1.exe")

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
    test' <@ rcount = ecount @> ("Mismatch at depth " + depth.ToString())
    Seq.zip result expected
    |> Seq.iter (fun (r : XElement, e : XElement) ->
         test <@ r.Name = e.Name @>
         let ra = r.Attributes()
         let ea = e.Attributes()
         Seq.zip ra ea
         |> Seq.iter
              (fun (a1 : XAttribute, a2 : XAttribute) ->
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
                  (a1.Name.ToString() + " : " + r.ToString() + " -> document")
              | "visitcount" ->
                let expected =
                  if zero then "0"
                  else a2.Value
                test' <@ expected = a1.Value @> (r.ToString() + " -> visitcount")
              | _ ->
                test' <@ a1.Value.Replace("\\", "/") = a2.Value.Replace("\\", "/") @>
                  (r.ToString() + " -> " + a1.Name.ToString()))
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
    test' <@ rcount = ecount @>
      ("Mismatch at depth " + depth.ToString() + " : " + expected.ToString() + " but got"
       + (result |> Seq.toList).ToString())
    Seq.zip result expected
    |> Seq.iter
         (fun (r : XElement, e : XElement) ->
         test <@ r.Name = e.Name @>
         let ra = r.Attributes()
         let ea = e.Attributes()
         Seq.zip ra ea
         |> Seq.iter
              (fun (a1 : XAttribute, a2 : XAttribute) ->
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
                  <@ a1.Value.Replace("\\", "/").Replace("altcover", "AltCover").
                              EndsWith(a2.Value.Replace("\\", "/").Replace("altcover", "AltCover")) @>
                  (a1.Name.ToString() + " : " + r.ToString() + " -> document")
              | "vc" ->
                let expected =
                  if zero then "0"
                  else a2.Value
                test' <@ expected = a1.Value @> (r.ToString() + " -> visitcount")
              | _ ->
                test' <@ a1.Value = a2.Value @>
                  (r.ToString() + " -> " + a1.Name.ToString()))
         RecursiveValidateOpenCover (r.Elements()) (e.Elements()) (depth + 1) zero
           expectSkipped)

  [<Test>]
  let CollectOptionsCanBeValidated() =
    let subject =
      { Primitive.CollectOptions.Create() with
                                              Threshold = "23"
                                              CommandLine = null }

    let instance = AltCover.CollectOptions.Primitive subject
    let scan = instance.Validate(false)
    test <@ scan.Length = 0 @>
    test <@ (instance.GetHashCode() :> obj).IsNotNull @> // gratuitous coverage for coverlet
    test <@ instance
            |> Args.collect = [ "Runner"; "-t"; "23"; "--collect" ] @>
    // hack
    let rerun = AltCover.CollectOptions.Abstract instance
    let scan = rerun.Validate(false)
    test <@ scan.Length = 0 @>
    test <@ (rerun.GetHashCode() :> obj).IsNotNull @> // gratuitous coverage for coverlet
    test <@ rerun
            |> Args.collect = [ "Runner"; "-t"; "23"; "--collect" ] @>

  [<Test>]
  let TypeSafeEmptyThresholdCanBeValidated() =
      let empty = TypeSafe.Threshold <| TypeSafe.Thresholds.Create()
      test <@ empty.AsString() = String.Empty @>

  [<Test>]
  let TypeSafeCollectOptionsCanBeValidated() =
    let here = Assembly.GetExecutingAssembly().Location
    let t = { TypeSafe.Thresholds.Create() with Statements = 23uy
                                                Branches = 16uy
                                                Methods = 7uy
                                                MaxCrap = 3uy
                                                           }
    let subject =
      { TypeSafe.CollectOptions.Create() with
                                             Threshold = TypeSafe.Threshold t
                                             SummaryFormat = TypeSafe.BPlus
                                             Executable = TypeSafe.Tool "dotnet" }

    let instance = AltCover.CollectOptions.TypeSafe subject
    test <@ (instance.GetHashCode() :> obj).IsNotNull @> // gratuitous coverage for coverlet

    let scan = instance.Validate(false)
    test <@ scan.Length = 0 @>
    test
      <@ instance
         |> Args.collect = [ "Runner"; "-x"; "dotnet"; "-t"; "S23B16M7C3"; "--teamcity:+B" ] @>
    let validate = instance.WhatIf(false)
    test <@ (validate.GetHashCode() :> obj).IsNotNull @> // gratuitous coverage for coverlet
    test <@ validate.ToString() = "altcover Runner -x dotnet -t S23B16M7C3 --teamcity:+B" @>

  [<Test>]
  let TypeSafeCollectSummaryCanBeValidated() =
    let inputs =
      [ TypeSafe.Default; TypeSafe.B; TypeSafe.BPlus; TypeSafe.R; TypeSafe.RPlus ]
    let expected = [ String.Empty; "B"; "+B"; "R"; "+R" ]
    inputs
    |> List.map (fun i -> i.AsString())
    |> List.zip expected
    |> List.iter (fun (a, b) -> test <@ a = b @>)

  [<Test>]
  let CollectOptionsCanBeValidatedWithErrors() =
    let subject = Primitive.CollectOptions.Create()
    let scan = (AltCover.CollectOptions.Primitive subject).Validate(true)
    test <@ scan.Length = 1 @>

  [<Test>]
  let TypeSafeCollectOptionsCanBeValidatedWithErrors() =
    let subject = TypeSafe.CollectOptions.Create()
    let scan = (AltCover.CollectOptions.TypeSafe subject).Validate(true)
    test <@ scan.Length = 1 @>

  [<Test>]
  let CollectOptionsCanBePositivelyValidatedWithErrors() =
    let test =
      { Primitive.CollectOptions.Create() with
                                              RecorderDirectory =
                                                Guid.NewGuid().ToString() }
    let instance = AltCover.CollectOptions.Primitive test
    let scan = instance.Validate(true)
    test' <@ scan.Length = 2 @> <| String.Join(Environment.NewLine, scan)

    // hack
    let rerun = AltCover.CollectOptions.Abstract instance
    let scan = rerun.Validate(true)
    test' <@ scan.Length = 2 @> <| String.Join(Environment.NewLine, scan)

  [<Test>]
  let TypeSafeCollectOptionsCanBePositivelyValidatedWithErrors() =
    let test =
      { TypeSafe.CollectOptions.Create() with
                                             RecorderDirectory =
                                               TypeSafe.DInfo
                                               <| DirectoryInfo(Guid.NewGuid().ToString()) }
    let scan = (AltCover.CollectOptions.TypeSafe test).Validate(true)
    test' <@ scan.Length = 2 @> <| String.Join(Environment.NewLine, scan)

  [<Test>]
  let PrepareOptionsCanBeValidated() =
    let here = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName

    let subject =
      { Primitive.PrepareOptions.Create() with
                                              InputDirectories = [| here |]
                                              OutputDirectories = [| here |]
                                              SymbolDirectories = [| here |]
                                              Dependencies =
                                                [| Assembly.GetExecutingAssembly().Location |]
                                              CallContext = [| "[Fact]" |]
                                              PathFilter = [| "ok" |] }

    let instance = AltCover.PrepareOptions.Primitive subject
    let scan = instance.Validate()
    test <@ scan.Length = 0 @>
    test <@ (instance.GetHashCode() :> obj).IsNotNull @> // gratuitous coverage for coverlet
    let rendered = instance |> Args.prepare
    let location = Assembly.GetExecutingAssembly().Location
    test
      <@ rendered = [ "-i"; here; "-o"; here; "-y"; here; "-d"; location; "-p"; "ok"; "-c";
                      "[Fact]"; "--reportFormat"; "OpenCover"; "--inplace"; "--save" ] @>

    // hack
    let rerun = AltCover.PrepareOptions.Abstract instance
    let scan = rerun.Validate()
    test <@ scan.Length = 0 @>
    test <@ (rerun.GetHashCode() :> obj).IsNotNull @> // gratuitous coverage for coverlet
    let rendered = rerun |> Args.prepare
    test
      <@ rendered = [ "-i"; here; "-o"; here; "-y"; here; "-d"; location; "-p"; "ok"; "-c";
                      "[Fact]"; "--reportFormat"; "OpenCover"; "--inplace"; "--save" ] @>

  [<Test>]
  let TypeSafePrepareOptionsCanBeValidated() =
    let here = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName

    test <@ (TypeSafe.Tool ".").AsString() = "." @>
    test <@ (TypeSafe.FilePath ".").AsString() = ("." |> Path.GetFullPath) @>
    test <@ ("fred" |> Regex |> TypeSafe.NegateMatchItem ).AsString() = "?fred" @>

    let subject =
      { TypeSafe.PrepareOptions.Create() with
                                             InputDirectories =
                                               TypeSafe.DirectoryPaths
                                                  [| TypeSafe.DirectoryPath here |]
                                             OutputDirectories =
                                               TypeSafe.DirectoryPaths
                                                [| TypeSafe.DInfo(DirectoryInfo(here)) |]
                                             SymbolDirectories =
                                               TypeSafe.DirectoryPaths
                                                 [| TypeSafe.DirectoryPath here |]
                                             Dependencies =
                                               TypeSafe.FilePaths
                                                 [| TypeSafe.FilePath
                                                    <| Assembly.GetExecutingAssembly().Location |]
                                             CallContext =
                                               TypeSafe.Context
                                                 [| TypeSafe.AttributeName "Fact"
                                                    TypeSafe.AttributeKind typeof<SerializableAttribute>
                                                    TypeSafe.Caller (Assembly.GetExecutingAssembly().
                                                                      GetType("Tests.AltCoverXTests").
                                                                      GetMethod("TypeSafePrepareOptionsCanBeValidated"))
                                                    TypeSafe.CallerName "Test" |]
                                             MethodPoint = TypeSafe.Set
                                             PathFilter =
                                               TypeSafe.Unfiltered.Join [| TypeSafe.Raw "ok" |] }

    let instance = AltCover.PrepareOptions.TypeSafe subject
    test <@ (instance.GetHashCode() :> obj).IsNotNull @> // gratuitous coverage for coverlet

    let scan = instance.Validate()
    test <@ scan.Length = 0 @>
    let location = Assembly.GetExecutingAssembly().Location
    test
      <@ instance
         |> Args.prepare = [ "-i"; here; "-o"; here; "-y"; here; "-d"; location;
                                   "-p"; "ok"; "-c"; "[Fact]"; "-c"; "[System.SerializableAttribute]";
                                   "-c"; "Tests.AltCoverXTests.TypeSafePrepareOptionsCanBeValidated";
                                   "-c"; "Test"; "--reportFormat"; "OpenCover"; "--inplace";
                                   "--save"; "--methodpoint" ] @>
    let validate = (AltCover.PrepareOptions.TypeSafe subject).WhatIf().ToString()
    test <@ validate = "altcover -i " + here + " -o " + here + " -y " + here + " -d " + location +
                 " -p ok -c [Fact] -c [System.SerializableAttribute] -c " +
                 "Tests.AltCoverXTests.TypeSafePrepareOptionsCanBeValidated -c Test --reportFormat OpenCover --inplace --save --methodpoint" @>

  [<Test>]
  let TypeSafePrepareOptionsCanBeValidatedAgain() =
    let here = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName

    let subject =
      { TypeSafe.PrepareOptions.Create() with
                                             InputDirectories =
                                               TypeSafe.DirectoryPaths [| TypeSafe.DirectoryPath here |]
                                             OutputDirectories =
                                               TypeSafe.DirectoryPaths [| TypeSafe.DInfo(DirectoryInfo(here)) |]
                                             SymbolDirectories =
                                               TypeSafe.DirectoryPaths
                                                 [| TypeSafe.DirectoryPath here |]
                                             Dependencies =
                                               TypeSafe.FilePaths
                                                 [| TypeSafe.FilePath
                                                    <| Assembly.GetExecutingAssembly().Location |]
                                             CommandLine =
                                               TypeSafe.CommandArguments
                                                 [| TypeSafe.CommandArgument "[Fact]" |]
                                             ReportFormat = TypeSafe.ReportFormat.NCover
                                             PathFilter =
                                               (TypeSafe.Filters
                                                 [| TypeSafe.MatchItem <| Regex "ok" |]).Join[] }

    let scan = (AltCover.PrepareOptions.TypeSafe subject).Validate()
    test <@ scan.Length = 0 @>
    let location = Assembly.GetExecutingAssembly().Location
    test
      <@ (AltCover.PrepareOptions.TypeSafe subject)
         |> Args.prepare = [ "-i"; here; "-o"; here; "-y"; here; "-d"; location;
                                   "-p"; "ok"; "--reportFormat"; "NCover"; "--inplace"; "--save"; "--";
                                   "[Fact]" ] @>

  [<Test>]
  let PrepareOptionsStrongNamesCanBeValidated() =
    let input = Path.Combine(AltCover.SolutionRoot.location, "Build/Infrastructure.snk")

    let subject =
      { Primitive.PrepareOptions.Create() with
                                              StrongNameKey = input
                                              Keys = [| input |] }

    let scan = (AltCover.PrepareOptions.Primitive subject).Validate()
#if NETCOREAPP2_1
    ()
#else
    test <@ scan.Length = 0 @>
#endif

  [<Test>]
  let TypeSafePrepareOptionsStrongNamesCanBeValidated() =
    let input = Path.Combine(AltCover.SolutionRoot.location, "Build/Infrastructure.snk")

    let subject =
      { TypeSafe.PrepareOptions.Create() with
                                             StrongNameKey =
                                               TypeSafe.FInfo <| FileInfo(input)
                                             Keys =
                                               TypeSafe.FilePaths
                                                 [| TypeSafe.FilePath input |] }

    let scan = (AltCover.PrepareOptions.TypeSafe subject).Validate()
#if NETCOREAPP2_1
    ()
#else
    test <@ scan.Length = 0 @>
#endif

  [<Test>]
  let PrepareOptionsCanBeValidatedWithNulls() =
    let subject = { Primitive.PrepareOptions.Create() with CallContext = null }
    let scan = (AltCover.PrepareOptions.Primitive subject).Validate()
    test <@ scan.Length = 0 @>

  [<Test>]
  let PrepareOptionsCanBeValidatedAndDetectInconsistency() =
    let subject =
      { Primitive.PrepareOptions.Create() with
                                              BranchCover = true
                                              LineCover = true
                                              SingleVisit = true
                                              CallContext = [| "0" |] }

    let scan = (AltCover.PrepareOptions.Primitive subject).Validate()
    test <@ scan.Length = 2 @>

  [<Test>]
  let TypeSafePrepareOptionsCanBeValidatedAndDetectInconsistency() =
    let subject =
      { TypeSafe.PrepareOptions.Create() with
                                             BranchCover = TypeSafe.Flag true
                                             LineCover = TypeSafe.Flag true
                                             SingleVisit = TypeSafe.Flag true
                                             CallContext =
                                               TypeSafe.Context
                                                 [| TypeSafe.TimeItem 0uy |] }
      |> AltCover.PrepareOptions.TypeSafe

    let scan = subject.Validate()
    test <@ scan.Length = 2 @>
    let rendered = subject |> Args.prepare
    test
      <@ rendered = [ "-c"; "0"; "--reportFormat"; "OpenCover"; "--inplace"; "--save"; "--single";
                      "--linecover"; "--branchcover" ] @>

  [<Test>]
  let TypeSafePrepareStaticCanBeValidated() =
    let inputs =
      [
        TypeSafe.StaticFormat.Default
        TypeSafe.StaticFormat.Show
        TypeSafe.StaticFormat.ShowZero
      ]
    let expected = [ "-"; "+"; "++" ]
    test <@  inputs |> List.map (fun i -> i.AsString()) = expected @>

  [<Test>]
  let PrepareOptionsCanBeValidatedWithErrors() =
    let subject =
      { Primitive.PrepareOptions.Create() with
                                              XmlReport =
                                                String(Path.GetInvalidPathChars())
                                              CallContext = [| "0"; "1" |] }

    let scan = (AltCover.PrepareOptions.Primitive subject).Validate()
    test <@ scan.Length = 2 @>

  [<Test>]
  let NullListsAreEmpty() =
    let subject = Args.itemList String.Empty null
    test <@ subject |> List.isEmpty @>

  [<Test>]
  let ValidateAssemblyOption() =
    test <@ Assembly.GetExecutingAssembly()
            |> Some
            |> Main.I.isMSBuild
            |> not @>

  [<Test>]
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
           "../_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.1")

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
    let outputSaved = CoverageParameters.theOutputDirectories |> Seq.toList
    let inputSaved = CoverageParameters.theInputDirectories |> Seq.toList
    let reportSaved = CoverageParameters.theReportPath
    let keySaved = CoverageParameters.defaultStrongNameKey
    let saved = (Console.Out, Console.Error)
    Main.init()
    let save2 = (Output.info, Output.error)
    try
      Output.error <- CommandLine.writeErr
      Output.info <- CommandLine.writeOut
      use stdout = new StringWriter()
      use stderr = new StringWriter()
      Console.SetOut stdout
      Console.SetError stderr
      let args =
        [| "-i"; input; "-o"; output; "-x"; report;
           "--sn"; key
           "-s=Adapter"; "-s=xunit"
           "-s=nunit"; "-e=Sample"; "-c=[Test]"; "--save" |]
      let result = Main.I.doInstrumentation args
      test <@ result = 0 @>
      test <@ stderr.ToString() |> Seq.isEmpty @>
      let expected =
        "Creating folder " + output + "\nInstrumenting files from "
        + (Path.GetFullPath input) + "\nWriting files to " + output + "\n   => "
        + Path.Combine(Path.GetFullPath input, "Sample4.dll") + "\n\nCoverage Report: "
        + report + "\n\n\n    " + Path.Combine(Path.GetFullPath output, "Sample4.dll")
        + "\n                <=  Sample4, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null\n"
      test <@ (expected.Replace("\\", "/")) = stdout.ToString().Replace("\r\n", "\n").Replace("\\", "/") @>
      test <@ CoverageParameters.outputDirectories() |> Seq.head = output @>
      test <@ (CoverageParameters.inputDirectories() |> Seq.head).Replace("\\", "/") =
               ((Path.GetFullPath input).Replace("\\", "/")) @>
      test <@ CoverageParameters.reportPath() = report  @>
      use stream = new FileStream(key, FileMode.Open)
      use buffer = new MemoryStream()
      stream.CopyTo(buffer)
      let snk = StrongNameKeyData.Make(buffer.ToArray())
      test <@ (CoverageParameters.keys.ContainsKey(KeyStore.keyToIndex snk)) @>
      test <@ CoverageParameters.keys.Count = 2 @>

      test <@ (File.Exists report) @>
      test <@ (File.Exists(report + ".acv")) @>

      let expected =
        [ "AltCover.Recorder.g.dll"
          "AltCover.Recorder.g.pdb";
          "Sample4.deps.json"; "Sample4.dll"; "Sample4.runtimeconfig.dev.json";
          "Sample4.runtimeconfig.json"; "Sample4.pdb";
          "xunit.runner.reporters.netcoreapp10.dll";
          "xunit.runner.utility.netcoreapp10.dll";
          "xunit.runner.visualstudio.dotnetcore.testadapter.dll" ]
      let theFiles =
        expected |> List.sortBy (fun f -> f.ToUpperInvariant())

      let actualFiles =
        Directory.GetFiles(output)
        |> Seq.map Path.GetFileName
        |> Seq.filter (fun f -> f.EndsWith(".tmp", StringComparison.Ordinal) |> not)
        |> Seq.filter (fun f -> Path.GetFileNameWithoutExtension f <> "testhost")
        |> Seq.sortBy (fun f -> f.ToUpperInvariant())
        |> Seq.toList

      test <@ String.Join("; ", actualFiles) = String.Join("; ", theFiles) @>
    finally
      CoverageParameters.trackingNames.Clear()
      CoverageParameters.theReportFormat <- None
      CoverageParameters.theOutputDirectories.Clear()
      CoverageParameters.theInputDirectories.Clear()
      CoverageParameters.theOutputDirectories.AddRange outputSaved
      CoverageParameters.theInputDirectories.AddRange inputSaved
      CoverageParameters.theReportPath <- reportSaved
      CoverageParameters.defaultStrongNameKey <- keySaved
      Console.SetOut(fst saved)
      Console.SetError(snd saved)
      CoverageParameters.keys.Clear()
      CoverageParameters.nameFilters.Clear()
      Output.error <- snd save2
      Output.info <- fst save2
    let before = File.ReadAllText(Path.Combine(input, "Sample4.deps.json"))
    test <@ before.IndexOf("AltCover.Recorder.g") =  -1 @>
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
    test <@ reset |> Set.contains "AltCover.Recorder.g" @>
    let aux =
      targeted.Properties()
      |> Seq.map (fun p -> p.Name)
      |> Set.ofSeq
    test <@ (aux
             |> Set.contains
                  ("AltCover.Recorder.g/" + System.AssemblyVersionInformation.AssemblyVersion)) @>
    let libraries =
      (o.Properties() |> Seq.find (fun p -> p.Name = "libraries")).Value :?> JObject

    let lib =
      libraries.Properties()
      |> Seq.map (fun p -> p.Name)
      |> Set.ofSeq
    test <@ (lib
             |> Set.contains
                  ("AltCover.Recorder.g/" + System.AssemblyVersionInformation.AssemblyVersion)) @>

  [<Test>]
  let ADryRunLooksAsExpected() =
    let where = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    let path = monoSample1path |> Path.GetDirectoryName |> Path.GetFullPath
    let key = Path.Combine(SolutionDir(), "Build/SelfTest.snk")
    let unique = Guid.NewGuid().ToString()
    let unique' = Path.Combine(where, Guid.NewGuid().ToString())
    Directory.CreateDirectory unique' |> ignore
    let report = Path.Combine(unique', "ADryRunLooksAsExpected.xml")
    let output = Path.Combine(Path.GetDirectoryName(where), unique)
    let outputSaved = CoverageParameters.theOutputDirectories |> Seq.toList
    let inputSaved = CoverageParameters.theInputDirectories |> Seq.toList
    let reportSaved = CoverageParameters.theReportPath
    let keySaved = CoverageParameters.defaultStrongNameKey
    let saved = (Console.Out, Console.Error)
    let save2 = (Output.info, Output.error)
    Main.init()
    try
      Output.error <- CommandLine.writeErr
      Output.info <- CommandLine.writeOut
      use stdout = new StringWriter()
      use stderr = new StringWriter()
      Console.SetOut stdout
      Console.SetError stderr
      let args = [| "-i"; path; "-o"; output; "-x"; report
                    "--reportFormat"; "ncov"
                    "-sn"; key
                 |]
      let result = Main.I.doInstrumentation args
      test <@ result = 0 @>
      test <@ stderr.ToString() |> Seq.isEmpty @>
      let subjectAssembly = Path.Combine(path, "Sample1.exe")
      let expected =
        "Creating folder " + output + "\nInstrumenting files from "
        + path + "\nWriting files to " + output + "\n   => "
        + monoSample1path + "\n\nCoverage Report: "
        + report + "\n\n\n    " + Path.Combine(Path.GetFullPath output, "Sample1.exe")
        + "\n                <=  Sample1, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null\n"
      let console = stdout.ToString()
      test <@ console.Replace("\r\n", "\n").Replace("\\", "/") = (expected.Replace("\\", "/")) @>
      test <@  CoverageParameters.outputDirectories() |> Seq.head = output @>
      test <@ (CoverageParameters.inputDirectories() |> Seq.head).Replace("\\", "/") =
               (path.Replace("\\", "/")) @>
      test <@ CoverageParameters.reportPath() = report @>
      use stream = new FileStream(key, FileMode.Open)
      use buffer = new MemoryStream()
      stream.CopyTo(buffer)
      let snk = StrongNameKeyData.Make(buffer.ToArray())
      test <@ CoverageParameters.keys.ContainsKey(KeyStore.keyToIndex snk) @>
      test <@ CoverageParameters.keys.Count = 2 @>

      test <@ File.Exists report @>

      let theFiles =
          [ "AltCover.Recorder.g.dll";
            "AltCover.Recorder.g.pdb"
            "Sample1.exe"; "Sample1.exe.mdb" ]

      let actual =
        Directory.GetFiles(output)
        |> Seq.map Path.GetFileName
        |> Seq.toList
        |> List.sortBy (fun f -> f.ToUpperInvariant())

      test <@ actual = theFiles @>
      let expectedText = MonoBaseline.Replace("name=\"Sample1.exe\"", "name=\"" + monoSample1path + "\"")
      let expectedXml = XDocument.Load(new StringReader(expectedText))
      let recordedXml = Runner.J.loadReport report
      RecursiveValidate (recordedXml.Elements()) (expectedXml.Elements()) 0 true
    finally
      CoverageParameters.theOutputDirectories.Clear()
      CoverageParameters.theInputDirectories.Clear()
      CoverageParameters.theOutputDirectories.AddRange outputSaved
      CoverageParameters.theInputDirectories.AddRange inputSaved
      CoverageParameters.theReportPath <- reportSaved
      CoverageParameters.defaultStrongNameKey <- keySaved
      Console.SetOut(fst saved)
      Console.SetError(snd saved)
      CoverageParameters.keys.Clear()
      Output.error <- snd save2
      Output.info <- fst save2

  [<Test>]
  let AfterAssemblyCommitsThatAssembly() =
    let hack = Path.Combine(SolutionDir(), "_Binaries/AltCover.Tests/Debug+AnyCPU")
    let local = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName

    let where =
      if local.IndexOf("_Binaries") > 0 then local
      else hack

    let path = Path.Combine(where + Hack(), "Sample4.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.readSymbols def
    let unique = Guid.NewGuid().ToString()
    let output = Path.Combine(Path.GetDirectoryName(where), unique)
    Directory.CreateDirectory(output) |> ignore
    let saved = CoverageParameters.theOutputDirectories |> Seq.toList
    try
      CoverageParameters.theOutputDirectories.Clear()
      CoverageParameters.theOutputDirectories.Add output
      let visited = Node.AfterAssembly (def, CoverageParameters.outputDirectories())
      let input = InstrumentContext.Build []
      let result = Instrument.I.instrumentationVisitor input visited
      test' <@ Object.ReferenceEquals(result, input) @> "result differs"
      let created = Path.Combine(output, "Sample4.dll")
      test' <@ File.Exists created@> (created + " not found")
      let pdb = Path.ChangeExtension(Assembly.GetExecutingAssembly().Location, ".pdb")
      if File.Exists pdb then
        let isWindows =
#if NETCOREAPP2_1
                        true
#else
                        System.Environment.GetEnvironmentVariable("OS") = "Windows_NT"
#endif
        test' <@
                 isWindows
                 |> not
                 || File.Exists(Path.ChangeExtension(created, ".pdb")) @>
           (created + " pdb not found")
    finally
      CoverageParameters.theOutputDirectories.AddRange saved

  [<Test>]
  let AfterAssemblyCommitsThatAssemblyForMono() =
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
    let path = monoSample1path
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.readSymbols def
    let unique = Guid.NewGuid().ToString()
    let output = Path.Combine(Path.GetDirectoryName(where), unique)
    Directory.CreateDirectory(output) |> ignore
    let saved = CoverageParameters.theOutputDirectories |> Seq.toList
    try
      CoverageParameters.theOutputDirectories.Clear()
      CoverageParameters.theOutputDirectories.AddRange [ output ]
      let visited = Node.AfterAssembly (def, CoverageParameters.outputDirectories())
      let input = InstrumentContext.Build []
      let result = Instrument.I.instrumentationVisitor input visited
      test' <@ Object.ReferenceEquals(result, input) @> "result differs"
      let created = Path.Combine(output, "Sample1.exe")
      test' <@ File.Exists created @> (created + " not found")
      let isDotNet = System.Environment.GetEnvironmentVariable("OS") = "Windows_NT"
      if isDotNet then
        test' <@ File.Exists(created + ".mdb") @> (created + ".mdb not found")
    finally
      CoverageParameters.theOutputDirectories.Clear()
      CoverageParameters.theOutputDirectories.AddRange saved

  [<Test>]
  let FinishCommitsTheRecordingAssembly() =
    let hack = Path.Combine(SolutionDir(), "_Binaries/AltCover.Tests/Debug+AnyCPU")
    let local = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName

    let where =
      if local.IndexOf("_Binaries") > 0 then local
      else hack

    let path = Path.Combine(where + Hack(), "Sample4.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    ProgramDatabase.readSymbols def
    let unique = Guid.NewGuid().ToString()
    let output = Path.Combine(Path.GetDirectoryName(where), unique)
    Directory.CreateDirectory(output) |> ignore
    let saved = CoverageParameters.theOutputDirectories |> Seq.toList
    try
      CoverageParameters.theOutputDirectories.Clear()
      CoverageParameters.theOutputDirectories.Add output
      let input = { InstrumentContext.Build [] with RecordingAssembly = def }
      let result = Instrument.I.instrumentationVisitor input Finish
      test <@ result.RecordingAssembly |> isNull @>
      let created = Path.Combine(output, "Sample4.dll")
      test' <@ File.Exists created @> (created + " not found")
      let pdb = Path.ChangeExtension(Assembly.GetExecutingAssembly().Location, ".pdb")
      if File.Exists pdb then
        let isWindows =
#if NETCOREAPP2_1
                        true
#else
                        System.Environment.GetEnvironmentVariable("OS") = "Windows_NT"
#endif
        test' <@  isWindows |> not ||
                     File.Exists (Path.ChangeExtension(created, ".pdb")) @> (created + " pdb not found")
    finally
      CoverageParameters.theOutputDirectories.Clear()
      CoverageParameters.theOutputDirectories.AddRange saved

  [<Test>]
  let ShouldDoCoverage() =
    let start = Directory.GetCurrentDirectory()
    let hack = Path.Combine(SolutionDir(), "_Binaries/AltCover.Tests/Debug+AnyCPU")
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
      try
         CoverageParameters.theReportFormat <- Some ReportFormat.NCover
         let from = Path.Combine(here, "AltCover.Recorder.dll")
         let updated = Instrument.I.prepareAssembly from
         Instrument.I.writeAssembly updated create
      finally
         CoverageParameters.theReportFormat <- None

    let save = Runner.J.recorderName
    let save1 = Runner.J.getPayload
    let save2 = Runner.J.getMonitor
    let save3 = Runner.J.doReport
    let codedreport = "coverage.xml" |> Path.GetFullPath
    let alternate = "not-coverage.xml" |> Path.GetFullPath
    try
      Runner.J.recorderName <- "AltCover.Recorder.g.dll"
      let payload (rest : string list) =
        test <@ rest = [ "test"; "1" ] @>
        255

      let monitor (hits : Dictionary<string, Dictionary<int, PointVisit>>)
          (token : string) _ _ =
        test' <@ token  = codedreport@> "should be default coverage file"
        test <@ hits |> Seq.isEmpty @>
        127

      let write (hits : Dictionary<string, Dictionary<int, PointVisit>>) format
          (report : string) (output : String option) =
        test' <@ report = codedreport@> "should be default coverage file"
        test <@ output = Some alternate @>
        test <@ hits |> Seq.isEmpty @>
        TimeSpan.Zero

      Runner.J.getPayload <- payload
      Runner.J.getMonitor <- monitor
      Runner.J.doReport <- write
      let empty = OptionSet()
      let dummy = codedreport + ".xx.acv"
      do use temp = File.Create dummy
         test <@ dummy |> File.Exists @>
      let r =
        Runner.doCoverage
          [| "Runner"; "-x"; "test"; "-r"; where; "-o"; alternate; "--"; "1" |] empty
      test <@ dummy
              |> File.Exists
              |> not @>
      test <@ r = 127 @>
    finally
      Runner.J.getPayload <- save1
      Runner.J.getMonitor <- save2
      Runner.J.doReport <- save3
      Runner.J.recorderName <- save
      Directory.SetCurrentDirectory start

  [<Test>]
  let ShouldGenerateExpectedXmlReportFromMono() =
    let visitor, document = Report.reportGenerator()
    let path = monoSample1path
    Visitor.visit [ visitor ] (Visitor.I.toSeq (path,[]))
    let expectedText = MonoBaseline.Replace("name=\"Sample1.exe\"", "name=\"" + (path |> Path.GetFullPath) + "\"")
    let baseline = XDocument.Load(new System.IO.StringReader(expectedText))
    let result = document.Elements()
    let expected = baseline.Elements()
    RecursiveValidate result expected 0 true

  [<Test>]
  let ShouldGenerateExpectedXmlReportFromMonoOpenCoverStyle() =
    let visitor, document = OpenCover.reportGenerator()
    let path = monoSample1path

    try
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- Some ReportFormat.OpenCover
      Visitor.visit [ visitor ] (Visitor.I.toSeq (path, []))
      let resource =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find
             (fun n -> n.EndsWith("HandRolledMonoCoverage.xml", StringComparison.Ordinal))
      use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
      let baseline = XDocument.Load(stream)
      ("ModulePath"
       |> XName.Get
       |> baseline.Descendants
       |> Seq.head).SetValue path

      let result = document.Elements()
      let expected = baseline.Elements()
      RecursiveValidateOpenCover result expected 0 true false
    finally
      CoverageParameters.nameFilters.Clear()
      CoverageParameters.theReportFormat <- None