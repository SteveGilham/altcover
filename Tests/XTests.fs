namespace Tests
// fsharplint:disable  MemberNames NonPublicValuesNames RedundantNewKeyword

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Text.RegularExpressions
open System.Xml.Linq

open AltCover
open AltCover.Augment
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
  let CollectParametersCanBeValidated() =
    let subject =
      { Primitive.CollectParameters.Create() with
                                              Threshold = "23"
                                              CommandLine = null }

    let instance = FSApi.CollectParameters.Primitive subject
    let scan = instance.Validate(false)
    test <@ scan.Length = 0 @>
    test <@ (instance.GetHashCode() :> obj).IsNotNull @> // gratuitous coverage for coverlet
    test <@ (FSApi.CollectParameters.Primitive subject)
            |> FSApi.Args.collect = [ "Runner"; "-t"; "23"; "--collect" ] @>

  [<Test>]
  let TypeSafeCollectParametersCanBeValidated() =
    let here = Assembly.GetExecutingAssembly().Location
    let subject =
      { TypeSafe.CollectParameters.Create() with
                                             Threshold = TypeSafe.Threshold 23uy
                                             SummaryFormat = TypeSafe.BPlus
                                             Executable = TypeSafe.Tool "dotnet" }

    let instance = FSApi.CollectParameters.TypeSafe subject
    test <@ (instance.GetHashCode() :> obj).IsNotNull @> // gratuitous coverage for coverlet

    let scan = instance.Validate(false)
    test <@ scan.Length = 0 @>
    test
      <@ instance
         |> FSApi.Args.collect = [ "Runner"; "-x"; "dotnet"; "-t"; "23"; "--teamcity:+B" ] @>
    let validate = instance.WhatIf(false)
    test <@ (validate.GetHashCode() :> obj).IsNotNull @> // gratuitous coverage for coverlet
    test <@ validate.ToString() = "altcover Runner -x dotnet -t 23 --teamcity:+B" @>

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
  let CollectParametersCanBeValidatedWithErrors() =
    let subject = Primitive.CollectParameters.Create()
    let scan = (FSApi.CollectParameters.Primitive subject).Validate(true)
    test <@ scan.Length = 1 @>

  [<Test>]
  let TypeSafeCollectParametersCanBeValidatedWithErrors() =
    let subject = TypeSafe.CollectParameters.Create()
    let scan = (FSApi.CollectParameters.TypeSafe subject).Validate(true)
    test <@ scan.Length = 1 @>

  [<Test>]
  let CollectParametersCanBePositivelyValidatedWithErrors() =
    let test =
      { Primitive.CollectParameters.Create() with
                                              RecorderDirectory =
                                                Guid.NewGuid().ToString() }
    let scan = (FSApi.CollectParameters.Primitive test).Validate(true)
    test' <@ scan.Length = 2 @> <| String.Join(Environment.NewLine, scan)

  [<Test>]
  let TypeSafeCollectParametersCanBePositivelyValidatedWithErrors() =
    let test =
      { TypeSafe.CollectParameters.Create() with
                                             RecorderDirectory =
                                               TypeSafe.DInfo
                                               <| DirectoryInfo(Guid.NewGuid().ToString()) }
    let scan = (FSApi.CollectParameters.TypeSafe test).Validate(true)
    test' <@ scan.Length = 2 @> <| String.Join(Environment.NewLine, scan)

  [<Test>]
  let PrepareParametersCanBeValidated() =
    let here = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName

    let subject =
      { Primitive.PrepareParameters.Create() with
                                              InputDirectories = [| here |]
                                              OutputDirectories = [| here |]
                                              SymbolDirectories = [| here |]
                                              Dependencies =
                                                [| Assembly.GetExecutingAssembly().Location |]
                                              CallContext = [| "[Fact]" |]
                                              PathFilter = [| "ok" |] }

    let instance = FSApi.PrepareParameters.Primitive subject
    let scan = instance.Validate()
    test <@ scan.Length = 0 @>
    test <@ (instance.GetHashCode() :> obj).IsNotNull @> // gratuitous coverage for coverlet
    let rendered = (FSApi.PrepareParameters.Primitive subject) |> FSApi.Args.prepare
    let location = Assembly.GetExecutingAssembly().Location
    test
      <@ rendered = [ "-i"; here; "-o"; here; "-y"; here; "-d"; location; "-p"; "ok"; "-c";
                      "[Fact]"; "--opencover"; "--inplace"; "--save" ] @>

  [<Test>]
  let TypeSafePrepareParametersCanBeValidated() =
    let here = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName

    test <@ (TypeSafe.Tool ".").AsString() = "." @>
    test <@ (TypeSafe.FilePath ".").AsString() = ("." |> Path.GetFullPath) @>
    test <@ ("fred" |> Regex |> TypeSafe.IncludeItem ).AsString() = "?fred" @>

    let subject =
      { TypeSafe.PrepareParameters.Create() with
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
                                                 [| TypeSafe.CallItem "[Fact]" |]
                                             PathFilter =
                                               TypeSafe.Filters [| TypeSafe.Raw "ok" |] }

    let instance = FSApi.PrepareParameters.TypeSafe subject
    test <@ (instance.GetHashCode() :> obj).IsNotNull @> // gratuitous coverage for coverlet

    let scan = instance.Validate()
    test <@ scan.Length = 0 @>
    let location = Assembly.GetExecutingAssembly().Location
    test
      <@ instance
         |> FSApi.Args.prepare = [ "-i"; here; "-o"; here; "-y"; here; "-d"; location;
                                   "-p"; "ok"; "-c"; "[Fact]"; "--opencover"; "--inplace";
                                   "--save" ] @>
    let validate = (FSApi.PrepareParameters.TypeSafe subject).WhatIf().ToString()
    test <@ validate = "altcover -i " + here + " -o " + here + " -y " + here + " -d " + location + " -p ok -c [Fact] --opencover --inplace --save" @>

  [<Test>]
  let TypeSafePrepareParametersCanBeValidatedAgain() =
    let here = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName

    let subject =
      { TypeSafe.PrepareParameters.Create() with
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
                                               TypeSafe.Command
                                                 [| TypeSafe.CommandArgument "[Fact]" |]
                                             PathFilter =
                                               TypeSafe.Filters
                                                 [| TypeSafe.FilterItem <| Regex "ok" |] }

    let scan = (FSApi.PrepareParameters.TypeSafe subject).Validate()
    test <@ scan.Length = 0 @>
    let location = Assembly.GetExecutingAssembly().Location
    test
      <@ (FSApi.PrepareParameters.TypeSafe subject)
         |> FSApi.Args.prepare = [ "-i"; here; "-o"; here; "-y"; here; "-d"; location;
                                   "-p"; "ok"; "--opencover"; "--inplace"; "--save"; "--";
                                   "[Fact]" ] @>

  [<Test>]
  let PrepareParametersStrongNamesCanBeValidated() =
    let input = Path.Combine(AltCover.SolutionRoot.location, "Build/Infrastructure.snk")

    let subject =
      { Primitive.PrepareParameters.Create() with
                                              StrongNameKey = input
                                              Keys = [| input |] }

    let scan = (FSApi.PrepareParameters.Primitive subject).Validate()
#if NETCOREAPP2_1
    ()
#else
    test <@ scan.Length = 0 @>
#endif

  [<Test>]
  let TypeSafePrepareParametersStrongNamesCanBeValidated() =
    let input = Path.Combine(AltCover.SolutionRoot.location, "Build/Infrastructure.snk")

    let subject =
      { TypeSafe.PrepareParameters.Create() with
                                             StrongNameKey =
                                               TypeSafe.FInfo <| FileInfo(input)
                                             Keys =
                                               TypeSafe.FilePaths
                                                 [| TypeSafe.FilePath input |] }

    let scan = (FSApi.PrepareParameters.TypeSafe subject).Validate()
#if NETCOREAPP2_1
    ()
#else
    test <@ scan.Length = 0 @>
#endif

  [<Test>]
  let PrepareParametersCanBeValidatedWithNulls() =
    let subject = { Primitive.PrepareParameters.Create() with CallContext = null }
    let scan = (FSApi.PrepareParameters.Primitive subject).Validate()
    test <@ scan.Length = 0 @>

  [<Test>]
  let PrepareParametersCanBeValidatedAndDetectInconsistency() =
    let subject =
      { Primitive.PrepareParameters.Create() with
                                              BranchCover = true
                                              LineCover = true
                                              Single = true
                                              CallContext = [| "0" |] }

    let scan = (FSApi.PrepareParameters.Primitive subject).Validate()
    test <@ scan.Length = 2 @>

  [<Test>]
  let TypeSafePrepareParametersCanBeValidatedAndDetectInconsistency() =
    let subject =
      { TypeSafe.PrepareParameters.Create() with
                                             BranchCover = TypeSafe.Flag true
                                             LineCover = TypeSafe.Flag true
                                             Single = TypeSafe.Flag true
                                             CallContext =
                                               TypeSafe.Context
                                                 [| TypeSafe.TimeItem 0uy |] }
      |> FSApi.PrepareParameters.TypeSafe

    let scan = subject.Validate()
    test <@ scan.Length = 2 @>
    let rendered = subject |> FSApi.Args.prepare
    test
      <@ rendered = [ "-c"; "0"; "--opencover"; "--inplace"; "--save"; "--single";
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
  let PrepareParametersCanBeValidatedWithErrors() =
    let subject =
      { Primitive.PrepareParameters.Create() with
                                              XmlReport =
                                                String(Path.GetInvalidPathChars())
                                              CallContext = [| "0"; "1" |] }

    let scan = (FSApi.PrepareParameters.Primitive subject).Validate()
    test <@ scan.Length = 2 @>

  [<Test>]
  let NullListsAreEmpty() =
    let subject = FSApi.Args.itemList String.Empty null
    test <@ subject |> List.isEmpty @>

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
        [| "-i"; input; "-o"; output; "-x"; report; "--opencover"
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
      let pdb = Path.ChangeExtension(Assembly.GetExecutingAssembly().Location, ".pdb")
      let isNT = System.Environment.GetEnvironmentVariable("OS") = "Windows_NT"
      let isWindows =
#if NETCOREAPP2_1
                        true
#else
                        isNT
#endif
      let expected =
        [ "AltCover.Recorder.g.dll"
#if NETCOREAPP2_1
#else
          "AltCover.Recorder.g.pdb";
#endif
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
                        [
#if NETCOREAPP2_1
#else
                            "AltCover.Recorder.g.dll.mdb";
#endif
                            "Sample4.dll.mdb" ] ]
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

      test <@ String.Join("; ", actualFiles) = String.Join("; ", theFiles) @>
    finally
      Output.usage { Intro ="dummy"; Options = OptionSet(); Options2 = OptionSet()}
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
    test <@
            (aux
             |> Set.contains
                  ("AltCover.Recorder.g/" + System.AssemblyVersionInformation.AssemblyVersion)) @>
    let libraries =
      (o.Properties() |> Seq.find (fun p -> p.Name = "libraries")).Value :?> JObject

    let lib =
      libraries.Properties()
      |> Seq.map (fun p -> p.Name)
      |> Set.ofSeq
    test <@
            (lib
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
                    "-sn"; key
                 |]
      let result = Main.I.doInstrumentation args
      test <@ result = 0 @>
      test <@ stderr.ToString() |> Seq.isEmpty @>
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
      let pdb = Path.ChangeExtension(Assembly.GetExecutingAssembly().Location, ".pdb")
      let isWindows =
#if NETCOREAPP2_1
                        true
#else
                        System.Environment.GetEnvironmentVariable("OS") = "Windows_NT"
#endif

      let theFiles =
        if File.Exists(pdb) then
          [ "AltCover.Recorder.g.dll";
#if NETCOREAPP2_1
#else
            "AltCover.Recorder.g.pdb"
#endif
            "Sample1.exe"; "Sample1.exe.mdb" ]
           // See Instrument.WriteAssembly
#if NETCOREAPP2_1
#else
          |> List.filter (fun f -> isWindows || f = "Sample1.exe.mdb" ||
                                   (f.EndsWith("db", StringComparison.Ordinal) |> not))
#endif
        else
          [ "AltCover.Recorder.g.dll";
#if NETCOREAPP2_1
#else
            "AltCover.Recorder.g.dll.mdb";
#endif
            "Sample1.exe";
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
         let updated = Instrument.I.prepareAssembly from
         Instrument.I.writeAssembly updated create
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

      let monitor (hits : Dictionary<string, Dictionary<int, Base.PointVisit>>)
          (token : string) _ _ =
        test' <@ token  = codedreport@> "should be default coverage file"
        test <@ hits |> Seq.isEmpty @>
        127

      let write (hits : Dictionary<string, Dictionary<int, Base.PointVisit>>) format
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
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
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
      CoverageParameters.theReportFormat <- Some Base.ReportFormat.OpenCover
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