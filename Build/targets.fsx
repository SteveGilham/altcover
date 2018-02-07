open System
open System.IO
open System.Xml
open System.Xml.Linq

open Actions

open Fake.Core.Environment
open Fake.Core.Globbing.Operators
open Fake.Core.Globbing.Tools
open Fake.Core.Process
open Fake.Core.Target
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.DotNet.Cli
open Fake.DotNet.AssemblyInfoFile
open Fake.DotNet.NuGet.NuGet
open Fake.DotNet.Testing.NUnit3
open Fake.DotNet.Testing
open Fake.IO
open Fake.IO.Directory
open Fake.IO.FileSystemOperators
open Fake.IO.Path
open Fake.IO.Shell

//=========== awkward cases
open Fake.FileHelper
open Fake.FxCopHelper
open Fake.ILMergeHelper
open Fake.ReportGeneratorHelper
open FSharpLint.Fake
//============
open NUnit.Framework

let Copyright  = ref String.Empty
let Version = ref String.Empty

let OpenCoverFilter = "+[AltCove*]* -[*]Microsoft.* -[*]System.* +[*]N.*"
let AltCoverFilter= @" -s=Mono -s=\.Recorder -s=Sample -s=nunit -e=Tests -t=System. -t=Sample3\.Class2 "
let AltCoverFilterG= @" -s=Mono -s=\.Recorder\.g -s=Sample -s=nunit -e=Tests -t=System. -t=Sample3\.Class2 "

// A more accurate flag for what is going on in travis-ci
let runningInMono = "Mono.Runtime" |> Type.GetType |> isNull |> not

let programFiles = environVar "ProgramFiles"
let programFiles86 = environVar "ProgramFiles(x86) "
let dotnetPath = "dotnet" |> tryFindFileOnPath
let dotnetOptions = match dotnetPath with
                    | Some f -> {DotnetOptions.Default with DotnetCliPath = f}
                    | None -> DotnetOptions.Default

let monoOnWindows = if isWindows then
                       [programFiles; programFiles86]
                       |> List.filter (String.IsNullOrWhiteSpace >> not)
                       |> List.map (fun s -> s @@ "Mono/bin/mono.exe")
                       |> List.filter File.Exists
                       |> List.tryFind (fun _ -> true)
                    else None

let Target s f =
  Description s
  Create s f

// Preparation

Target "Preparation" ignore

Target "Clean" (fun _ ->
    printfn "Cleaning the build and deploy folders"
    Actions.Clean ()
)

Target "SetVersion" (fun _ ->
    let appveyor = environVar "APPVEYOR_BUILD_VERSION"
    let travis = environVar "TRAVIS_JOB_NUMBER"
    let version = Actions.GetVersionFromYaml ()
    let ci = if String.IsNullOrWhiteSpace appveyor then
               if  String.IsNullOrWhiteSpace travis then
                 String.Empty
               else version.Replace("{build}", travis + "-travis")
             else appveyor
    let (v, majmin, y) = Actions.LocalVersion ci version
    Version := v
    let copy = sprintf "© 2010-%d by Steve Gilham <SteveGilham@users.noreply.github.com>" y
    Copyright := "Copyright " + copy

    ensure "./_Generated"
    Actions.InternalsVisibleTo (!Version)
    let v' = !Version
    CreateFSharp "./_Generated/AssemblyVersion.fs"
        [
         AssemblyInfo.Product "AltCover"
         AssemblyInfo.Version (majmin + ".0.0")
         AssemblyInfo.FileVersion v'
         AssemblyInfo.Company "Steve Gilham"
         AssemblyInfo.Trademark ""
         AssemblyInfo.Copyright copy
        ]
)

// Basic compilation

Target "Compilation" ignore

Target "BuildRelease" (fun _ ->
    "AltCover.sln"
    |> MsBuild.build (fun p ->
            { p with
                Verbosity = Some MsBuild.MSBuildVerbosity.Normal
                Properties = [
                               "Configuration", "Release"
                               "DebugSymbols", "True"
                             ]})

    "./altcover.core.sln"
    |> DotnetCompile
        (fun p ->
            { p with
                Configuration = BuildConfiguration.Release
                Common = dotnetOptions
            })
)

Target "BuildDebug" (fun _ ->
    !! "**/AltCove*.sln"  // include demo projects
    |> Seq.filter (fun n -> n.IndexOf(".core.") = -1)
    |> Seq.filter (fun n -> n.IndexOf(".dotnet.") = -1)
    |> Seq.iter (MsBuild.build (fun p ->
            { p with
                Verbosity = Some MsBuild.MSBuildVerbosity.Normal
                Properties = [
                               "Configuration", "Debug"
                               "DebugSymbols", "True"
                             ]}))

    "./altcover.core.sln"
    |> DotnetCompile
        (fun p ->
            { p with
                Configuration = BuildConfiguration.Debug
                Common = dotnetOptions})
)

Target "BuildMonoSamples" (fun _ ->
    let mcs = findToolInSubPath "MCS.exe" ".."

    [
        ("./_Mono/Sample1", "-debug -out:./_Mono/Sample1/Sample1.exe  ./Sample1/Program.cs")
        ("./_Mono/Sample3", "-target:library -debug -out:./_Mono/Sample3/Sample3.dll  ./Sample3/Class1.cs")
    ]
    |> Seq.iter (fun (dir, cmd) -> ensure dir
                                   let result = ExecProcess (fun info -> { info with
                                                                                FileName = mcs
                                                                                WorkingDirectory = "."
                                                                                Arguments = cmd}) (TimeSpan.FromMinutes 5.0)
                                   Assert.That(result, Is.EqualTo 0, "Mono compilation of '" + cmd + "' failed"))

    Actions.FixMVId ["./_Mono/Sample1/Sample1.exe"; "./_Mono/Sample3/Sample3.dll"]
)

// Code Analysis

Target "Analysis" ignore

Target "Lint" (fun _ ->
    !! "**/*.fsproj"
        |> Seq.filter (fun n -> n.IndexOf(".core.") = -1)
        |> Seq.iter (FSharpLint (fun options -> { options with FailBuildIfAnyWarnings = true }) ))

Target "Gendarme" (fun _ -> // Needs debug because release is compiled --standalone which contaminates everything
    ensure "./_Reports"
    let subjects = String.Join(" ",
                               [
                                "_Binaries/AltCover/Debug+AnyCPU/AltCover.exe"
                                "_Binaries/AltCover.Shadow/Debug+AnyCPU/AltCover.Shadow.dll"
                               ])
    let r = ExecProcess (fun info -> { info with
                                            FileName = (findToolInSubPath "gendarme.exe" "./packages")
                                            WorkingDirectory = "."
                                            Arguments = "--severity all --confidence all --config ./Build/rules.xml --console --html ./_Reports/gendarme.html " + subjects}) (TimeSpan.FromMinutes 5.0)
    Assert.That(r, Is.EqualTo 0, "Gendarme Errors were detected")
)
// Travis TODO
(*
------------------------------------------------------------
1. AvoidLongMethodsRule
* Target:   AltCover.Instrument/Context AltCover.Instrument::InstrumentationVisitor(AltCover.Instrument/Context,AltCover.Node)
* Details:  Method IL Size: 324. Maximum Size: 165
* Target:   Microsoft.FSharp.Collections.FSharpList`1<System.Xml.Linq.XElement> AltCover.Report/ReportVisitor@22::Invoke(Microsoft.FSharp.Collections.FSharpList`1<System.Xml.Linq.XElement>,AltCover.Node)
* Details:  Method IL Size: 419. Maximum Size: 165
* Target:   System.Collections.Generic.IEnumerable`1<AltCover.Node> AltCover.Visitor::Deeper(AltCover.Node)
* Details:  Method IL Size: 182. Maximum Size: 165
*)

Target "FxCop" (fun _ -> // Needs debug because release is compiled --standalone which contaminates everything
    ensure "./_Reports"
    let fxCop = combine (environVar "VS150COMNTOOLS") "../../Team Tools/Static Analysis Tools/FxCop/FxCopCmd.exe"
    let rules = ["-Microsoft.Design#CA1004"
                 "-Microsoft.Design#CA1006"
                 "-Microsoft.Design#CA1011" // maybe sometimes
                 "-Microsoft.Design#CA1062" // null checks,  In F#!
                 "-Microsoft.Maintainability#CA1506"
                 "-Microsoft.Naming#CA1704"
                 "-Microsoft.Naming#CA1707"
                 "-Microsoft.Naming#CA1709"
                 "-Microsoft.Naming#CA1715" ]

    [ ([
         "_Binaries/AltCover/Debug+AnyCPU/AltCover.exe"
                                                      ],   ["AltCover.Augment"
                                                            "AltCover.CommandLine"
                                                            "AltCover.Filter"
                                                            "AltCover.FilterClass"
                                                            "AltCover.Fix"
                                                            "AltCover.Instrument"
                                                            "AltCover.KeyRecord"
                                                            "AltCover.KeyStore"
                                                            "AltCover.Main"
                                                            "AltCover.Naming"
                                                            "AltCover.Node"
                                                            "AltCover.ProgramDatabase"
                                                            "AltCover.Report"
                                                            "AltCover.Runner"
                                                            "AltCover.Visitor"])
      (["_Binaries/AltCover.Shadow/Debug+AnyCPU/AltCover.Shadow.dll"], ["AltCover.Recorder.Instance"])]
    |> Seq.iter (fun (files, types) -> files
                                       |> FxCop (fun p -> { p with ToolPath = fxCop
                                                                   WorkingDir = "."
                                                                   UseGACSwitch = true
                                                                   Verbose = false
                                                                   ReportFileName = "_Reports/FxCopReport.xml"
                                                                   TypeList = types
                                                                   Rules = rules
                                                                   IgnoreGeneratedCode  = true})
                                       Assert.That(File.Exists "_Reports/FxCopReport.xml", Is.False, "FxCop Errors were detected"))
)

// Unit Test

Target "UnitTest" (fun _ ->
  let numbers = !! (@"_Reports/*/Summary.xml")
                |> Seq.collect (fun f -> let xml = XDocument.Load f
                                         xml.Descendants(XName.Get("Linecoverage"))
                                         |> Seq.map (fun e -> let coverage = e.Value.Replace("%", String.Empty)
                                                              match Double.TryParse coverage with
                                                              | (false, _) -> Assert.Fail ("Could not parse coverage "+coverage)
                                                                              0.0
                                                              | (_, numeric) -> printfn "%s : %A" (f |> Path.GetDirectoryName |> Path.GetFileName) numeric
                                                                                numeric))
                |> Seq.toList

  if numbers |> List.tryFind (fun n -> n >= 90.0) |> Option.isNone && numbers |> List.length > 2 then
     Assert.Fail("Coverage is too low")
)

Target "JustUnitTest" (fun _ ->
    ensure "./_Reports"
    !! (@"_Binaries/*Tests/Debug+AnyCPU/*.Test*.dll")
    |> NUnit3 (fun p -> { p with ToolPath = findToolInSubPath "nunit3-console.exe" "."
                                 WorkingDir = "."
                                 Labels = LabelsLevel.All
                                 ResultSpecs = ["./_Reports/JustUnitTestReport.xml"] })
)

Target "UnitTestDotNet" (fun _ ->
    ensure "./_Reports"
    !! (@"./*Tests/*.tests.core.fsproj")
    |> Seq.iter (fun f -> printfn "Testing %s" f
                          let r = Dotnet dotnetOptions (" test --configuration Debug " + f)
                          Assert.That (r.ExitCode, Is.EqualTo 0, sprintf "%A" r))
)

Target "UnitTestWithOpenCover" (fun _ ->
    ensure "./_Reports/_UnitTestWithOpenCover"
    let testFiles = !! (@"_Binaries/*Tests/Debug+AnyCPU/*.Test*.dll")
                    //|> Seq.map (fun f -> f.FullName)
    let coverage = getFullName "_Reports/UnitTestWithOpenCover.xml"

    OpenCover.Run (fun p -> { p with
                                 WorkingDir = "."
                                 ExePath = findToolInSubPath "OpenCover.Console.exe" "."
                                 TestRunnerExePath = findToolInSubPath "nunit3-console.exe" "."
                                 Filter = "+[AltCover]* +[AltCover.Shadow]* +[AltCover.Runner]* -[*]Microsoft.* -[*]System.* -[Sample*]*"
                                 MergeByHash = true
                                 OptionalArguments = "-excludebyattribute:*ExcludeFromCodeCoverageAttribute;*ProgIdAttribute"
                                 Register = OpenCover.RegisterType.RegisterUser
                                 Output = coverage })
        (String.Join(" ", testFiles) + " --result=./_Reports/UnitTestWithOpenCoverReport.xml")

    ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                       ReportTypes = [ ReportGeneratorReportType.Html; ReportGeneratorReportType.Badges; ReportGeneratorReportType.XmlSummary ]
                                       TargetDir = "_Reports/_UnitTestWithOpenCover"})
        [coverage]

    if not <| String.IsNullOrWhiteSpace (environVar "APPVEYOR_BUILD_NUMBER") then
            ExecProcess (fun info -> { info with 
                                            FileName = findToolInSubPath "coveralls.net.exe" "."
                                            WorkingDirectory = "_Reports"
                                            Arguments = ("--opencover " + coverage)}) (TimeSpan.FromMinutes 5.0)
            |> ignore
)

// Hybrid (Self) Tests

Target "UnitTestWithAltCover" (fun _ ->
    ensure "./_Reports/_UnitTestWithAltCover"
    let keyfile = getFullName "Build/SelfTest.snk"
    let reports = getFullName "./_Reports"
    let altcover = findToolInSubPath "AltCover.exe" "./_Binaries"

    let testDirectory = getFullName "_Binaries/AltCover.Tests/Debug+AnyCPU"
    if !! (testDirectory @@ "AltCov*.pdb") |> Seq.length > 0 then

      let altReport = reports @@ "UnitTestWithAltCover.xml"
      printfn "Instrumented the code"
      let result = ExecProcess (fun info -> { info with
                                                   FileName = altcover
                                                   WorkingDirectory = testDirectory
                                                   Arguments = ("/sn=" + keyfile + AltCoverFilter + @"/o=./__UnitTestWithAltCover -x=" + altReport)}) (TimeSpan.FromMinutes 5.0)
      Assert.That(result, Is.EqualTo 0, "Re-instrument returned with a non-zero exit code")

      printfn "Unit test the instrumented code"
      [ !! "_Binaries/AltCover.Tests/Debug+AnyCPU/__UnitTestWithAltCover/*.Tests.dll"
        !! "_Binaries/AltCover.Tests/Debug+AnyCPU/__UnitTestWithAltCover/*ple2.dll"]
      |> Seq.concat |> Seq.distinct
      |> NUnit3 (fun p -> { p with ToolPath = findToolInSubPath "nunit3-console.exe" "."
                                   WorkingDir = "."
                                   //Labels = LabelsLevel.All
                                   ResultSpecs = ["./_Reports/UnitTestWithAltCoverReport.xml"] })

      printfn "Instrument the shadow tests"
      let shadowDir = getFullName  "_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU"
      let shadowReport = reports @@ "ShadowTestWithAltCover.xml"
      let result = ExecProcess (fun info -> { info with 
                                                   FileName = altcover
                                                   WorkingDirectory = shadowDir
                                                   Arguments = ("/sn=" + keyfile + AltCoverFilter + @"/o=./__ShadowTestWithAltCover -x=" + shadowReport)}) (TimeSpan.FromMinutes 5.0)
      Assert.That (result, Is.EqualTo 0)

      printfn "Execute the shadow tests"
      !! ("_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU/__ShadowTestWithAltCover/*.Test*.dll")
      |> NUnit3 (fun p -> { p with ToolPath = findToolInSubPath "nunit3-console.exe" "."
                                   WorkingDir = "."
                                   ResultSpecs = ["./_Reports/ShadowTestWithAltCoverReport.xml"] })

      ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                         ReportTypes = [ ReportGeneratorReportType.Html; ReportGeneratorReportType.Badges; ReportGeneratorReportType.XmlSummary ]
                                         TargetDir = "_Reports/_UnitTestWithAltCover"})
          [altReport; shadowReport]
    else
      printfn "Symbols not present; skipping"
)

Target "UnitTestWithAltCoverCore" (fun _ ->
    ensure "./_Reports/_UnitTestWithAltCover"
    let keyfile = getFullName "Build/SelfTest.snk"
    let reports = getFullName "./_Reports"
    let altcover = findToolInSubPath "AltCover.exe" "./_Binaries"

    let testDirectory = getFullName "_Binaries/AltCover.Tests/Debug+AnyCPU/netcoreapp2.0"
    let output = getFullName "Tests/_Binaries/AltCover.Tests/Debug+AnyCPU/netcoreapp2.0"

    let altReport = reports @@ "UnitTestWithAltCoverCore.xml"
    printfn "Instrumented the code"
    let result = ExecProcess (fun info -> { info with
                                                 FileName = altcover
                                                 WorkingDirectory = testDirectory
                                                 Arguments = ("/sn=" + keyfile + AltCoverFilter + @"/o=" + output + " -x=" + altReport)}) (TimeSpan.FromMinutes 5.0)
    Assert.That(result, Is.EqualTo 0, "first instrument returned with a non-zero exit code")

    printfn "Unit test the instrumented code"
    let result = Dotnet {dotnetOptions with WorkingDirectory = getFullName "Tests"}
                                         ("test --no-build --configuration Debug altcover.tests.core.fsproj")
    Assert.That(result.ExitCode, Is.EqualTo 0, "first test returned with a non-zero exit code")

    printfn "Instrument the shadow tests"
    let shadowDir = "_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU/netcoreapp2.0"
    let shadowReport = reports @@ "ShadowTestWithAltCoverCore.xml"
    let shadowOut = getFullName "Shadow.Tests/_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU/netcoreapp2.0"
    let result = ExecProcess (fun info -> { info with
                                                 FileName = altcover
                                                 WorkingDirectory = shadowDir
                                                 Arguments = ("/sn=" + keyfile + AltCoverFilterG + @"/o=" + shadowOut + " -x=" + shadowReport)}) (TimeSpan.FromMinutes 5.0)
    Assert.That(result, Is.EqualTo 0, "second instrument returned with a non-zero exit code")

    printfn "Execute the shadow tests"
    let result = Dotnet {dotnetOptions with WorkingDirectory = getFullName "Shadow.Tests"}
                                         ("test --no-build --configuration Debug altcover.recorder.tests.core.fsproj")
    Assert.That(result.ExitCode, Is.EqualTo 0, "second test returned with a non-zero exit code")

    ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                       ReportTypes = [ ReportGeneratorReportType.Html; ReportGeneratorReportType.Badges; ReportGeneratorReportType.XmlSummary]
                                       TargetDir = "_Reports/_UnitTestWithAltCoverCore"})
          [altReport; shadowReport]
)

Target "UnitTestWithAltCoverCoreRunner" (fun _ ->
    ensure "./_Reports/_UnitTestWithAltCover"
    let reports = getFullName "./_Reports"
    let altcover = getFullName "./AltCover/altcover.core.fsproj"

    let testDirectory = getFullName "_Binaries/AltCover.Tests/Debug+AnyCPU/netcoreapp2.0"
    let output = getFullName "Tests/_Binaries/AltCover.Tests/Debug+AnyCPU/netcoreapp2.0"

    let altReport = reports @@ "UnitTestWithAltCoverCoreRunner.xml"
    printfn "Instrument the code"
    CleanDir output
    let result = Dotnet {dotnetOptions with WorkingDirectory = testDirectory}
                            ("run --project " + altcover +
                             " -- " + AltCoverFilter + " -x \"" + altReport + "\" /o \"" + output + "\"")
    Assert.That (result.ExitCode, Is.EqualTo 0)

    printfn "Unit test the instrumented code"
    let testproject = getFullName "./Tests/altcover.tests.core.fsproj"

    let result = Dotnet {dotnetOptions with WorkingDirectory = output}
                            ("run --project " + altcover +
                             " -- Runner -x \"dotnet\" -r \"" + output +
                             "\" -- test --no-build --configuration Debug " +
                             testproject)
    Assert.That (result.ExitCode, Is.EqualTo 0)

    printfn "Instrument the shadow tests"
    let shadowDir = "_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU/netcoreapp2.0"
    let shadowReport = reports @@ "ShadowTestWithAltCoverCoreRunner.xml"
    let shadowOut = getFullName "Shadow.Tests/_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU/netcoreapp2.0"

    CleanDir shadowOut
    let result = Dotnet {dotnetOptions with WorkingDirectory = shadowDir}
                            ("run --project " + altcover +
                             " -- " + AltCoverFilter + " -x \"" + shadowReport + "\" /o \"" + shadowOut + "\"")
    Assert.That (result.ExitCode, Is.EqualTo 0)

    let shadowProject = getFullName "./Shadow.Tests/altcover.recorder.tests.core.fsproj"
    let result = Dotnet {dotnetOptions with WorkingDirectory = shadowOut}
                            ("run --project " + altcover +
                             " -- Runner -x \"dotnet\" -r \"" + shadowOut +
                             "\" -- test --no-build --configuration Debug " +
                             shadowProject)
    Assert.That (result.ExitCode, Is.EqualTo 0)

    ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                       ReportTypes = [ ReportGeneratorReportType.Html; ReportGeneratorReportType.Badges; ReportGeneratorReportType.XmlSummary]
                                       TargetDir = "_Reports/_UnitTestWithAltCoverCoreRunner"})
          [altReport; shadowReport]
)

// Pure OperationalTests

Target "OperationalTest" ignore

Target "FSharpTypes" ( fun _ ->
    ensure "./_Reports"
    let simpleReport = (getFullName "./_Reports") @@ ( "AltCoverFSharpTypes.xml")
    let binRoot = getFullName "_Binaries/AltCover/Release+AnyCPU"
    let sampleRoot = getFullName "_Binaries/Sample2/Debug+AnyCPU"
    let instrumented = "__FSharpTypes"

    if sampleRoot @@ "Sample2.pdb" |> File.Exists then
      let result = ExecProcess (fun info -> { info with
                                                   FileName = binRoot @@ "AltCover.exe"
                                                   WorkingDirectory = sampleRoot
                                                   Arguments = ("-t=System\. -t=Microsoft\. -x=" + simpleReport + " /o=./" + instrumented)}) (TimeSpan.FromMinutes 5.0)
      Assert.That (result, Is.EqualTo 0)
      Actions.ValidateFSharpTypes simpleReport []
    else
      printfn "Symbols not present; skipping"
)

Target "FSharpTypesDotNet" ( fun _ ->
    ensure "./_Reports"
    let project = getFullName "./AltCover/altcover.core.fsproj"
    let simpleReport = (getFullName "./_Reports") @@ ( "AltCoverFSharpTypesDotNet.xml")
    let sampleRoot = getFullName "_Binaries/Sample2/Debug+AnyCPU/netcoreapp2.0"
    let instrumented = getFullName "Sample2/_Binaries/Sample2/Debug+AnyCPU/netcoreapp2.0"
    let result = Dotnet {dotnetOptions with WorkingDirectory = sampleRoot}
                             ("run --project " + project + " -- -t \"System\\.\" -t \"Microsoft\\.\" -x \"" + simpleReport + "\" /o \"" + instrumented + "\"")
    Assert.That (result.ExitCode, Is.EqualTo 0)

    Actions.ValidateFSharpTypes simpleReport ["main"]

    printfn "Execute the instrumented tests"
    let result = Dotnet {dotnetOptions with WorkingDirectory = getFullName "Sample2"}
                            ("test --no-build --configuration Debug sample2.core.fsproj")
    Assert.That(result.ExitCode, Is.EqualTo 0, "sample test returned with a non-zero exit code")
    Actions.ValidateFSharpTypesCoverage simpleReport
)

Target "FSharpTypesDotNetRunner" ( fun _ ->
    ensure "./_Reports"
    let project = getFullName "./AltCover/altcover.core.fsproj"
    let simpleReport = (getFullName "./_Reports") @@ ( "AltCoverFSharpTypesDotNetRunner.xml")
    let sampleRoot = getFullName "_Binaries/Sample2/Debug+AnyCPU/netcoreapp2.0"
    let instrumented = getFullName "Sample2/_Binaries/Sample2/Debug+AnyCPU/netcoreapp2.0"

    // Instrument the code
    let result = Dotnet {dotnetOptions with WorkingDirectory = sampleRoot}
                             ("run --project " + project + " --configuration Release -- -t \"System\\.\" -t \"Microsoft\\.\" -x \"" + simpleReport + "\" /o \"" + instrumented + "\"")
    Assert.That (result.ExitCode, Is.EqualTo 0)

    Actions.ValidateFSharpTypes simpleReport ["main"]

    printfn "Execute the instrumented tests"
    let sample2 = getFullName "./Sample2/sample2.core.fsproj"

    let result = Dotnet {dotnetOptions with WorkingDirectory = instrumented}
                            ("run --project " + project +
                             " --configuration Release -- Runner -x \"dotnet\" -r \"" + instrumented +
                             "\" -- test --no-build --configuration Debug " +
                             sample2)
    Assert.That (result.ExitCode, Is.EqualTo 0)

    Actions.ValidateFSharpTypesCoverage simpleReport
)

Target "BasicCSharp" (fun _ ->
   Actions.SimpleInstrumentingRun "_Binaries/Sample1/Debug+AnyCPU" "_Binaries/AltCover/Debug+AnyCPU" "BasicCSharp"
)

Target "BasicCSharpMono" (fun _ ->
    Actions.SimpleInstrumentingRun "_Mono/Sample1" "_Binaries/AltCover/Debug+AnyCPU" "BasicCSharpMono"
)

Target "BasicCSharpUnderMono" (fun _ ->
    monoOnWindows |>
    Actions.SimpleInstrumentingRunUnderMono "_Binaries/Sample1/Debug+AnyCPU" "_Binaries/AltCover/Debug+AnyCPU" "BasicCSharp"
)

Target "BasicCSharpMonoUnderMono" (fun _ ->
    monoOnWindows |>
    Actions.SimpleInstrumentingRunUnderMono "_Mono/Sample1" "_Binaries/AltCover/Debug+AnyCPU" "BasicCSharpMono"
)

Target "CSharpMonoWithDotNet" (fun _ ->
    ensure "./_Reports"
    let x = getFullName "./_Reports/CSharpMonoWithDotNet.xml"
    let o = getFullName "./_Mono/__Instrumented.CSharpMonoWithDotNet"
    let i = getFullName "./_Mono/Sample1"
    let result = Dotnet dotnetOptions
                            ("run --project ./AltCover/altcover.core.fsproj -- -t \"System.\" -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")
    Assert.That (result.ExitCode, Is.EqualTo 0)

    let result2 = ExecProcess (fun info -> { info with
                                                  FileName = o @@ "/Sample1.exe"
                                                  WorkingDirectory = o
                                                  Arguments = ""}) (TimeSpan.FromMinutes 5.0)
    Assert.That(result2, Is.EqualTo 0, "Instrumented .exe failed")

    Actions.ValidateSample1 "./_Reports/CSharpMonoWithDotNet.xml" "CSharpMonoWithDotNet"
)

Target "CSharpDotNetWithDotNet" (fun _ ->
    ensure "./_Reports"
    let x = getFullName "./_Reports/CSharpDotNetWithDotNet.xml"
    let o = getFullName "../_Binaries/Sample1/__Instrumented.CSharpDotNetWithDotNet"
    let i = getFullName "./_Binaries/Sample1/Debug+AnyCPU/netcoreapp2.0"
    let result = Dotnet dotnetOptions
                            ("_Binaries/AltCover/Debug+AnyCPU/netcoreapp2.0/AltCover.dll -t \"System.\" -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")
    Assert.That (result.ExitCode, Is.EqualTo 0)

    let result = Dotnet dotnetOptions
                            (o @@ "Sample1.dll")
    Assert.That (result.ExitCode, Is.EqualTo 0)

    Actions.ValidateSample1 "./_Reports/CSharpDotNetWithDotNet.xml" "CSharpDotNetWithDotNet"
)

Target "CSharpDotNetWithFramework" (fun _ ->
    ensure "./_Reports"
    let simpleReport = (getFullName "./_Reports") @@ ( "CSharpDotNetWithFramework.xml")
    let binRoot = getFullName "_Binaries/AltCover/Release+AnyCPU"
    let sampleRoot = getFullName "_Binaries/Sample1/Debug+AnyCPU/netcoreapp2.0"
    let instrumented = getFullName "_Binaries/Sample1/__Instrumented.CSharpDotNetWithFramework"
    let result = ExecProcess (fun info -> { info with
                                                 FileName = binRoot @@ "AltCover.exe"
                                                 WorkingDirectory = sampleRoot
                                                 Arguments = ("-t=System\. -t=Microsoft\. -x=" + simpleReport + " /o=" + instrumented)}) (TimeSpan.FromMinutes 5.0)
    Assert.That (result, Is.EqualTo 0)

    let result = Dotnet dotnetOptions 
                            (instrumented @@ "Sample1.dll")
    Assert.That (result.ExitCode, Is.EqualTo 0)

    Actions.ValidateSample1 "./_Reports/CSharpDotNetWithFramework.xml" "CSharpDotNetWithFramework"
)

Target "SelfTest" (fun _ ->
    ensure "./_Reports/_Instrumented"
    let targetDir = "_Binaries/AltCover.Tests/Debug+AnyCPU"
    let reports = getFullName "./_Reports"
    let report = reports @@ "OpenCoverSelfTest.xml"
    let altReport = reports @@ "AltCoverSelfTest.xml"
    let keyfile = getFullName "Build/SelfTest.snk"

    printfn "Self-instrument under OpenCover"
    OpenCover.Run (fun p -> { p with
                                 WorkingDir = targetDir
                                 ExePath = findToolInSubPath "OpenCover.Console.exe" "."
                                 TestRunnerExePath = findToolInSubPath "AltCover.exe" targetDir
                                 Filter = OpenCoverFilter
                                 MergeByHash = true
                                 OptionalArguments = "-excludebyattribute:*ExcludeFromCodeCoverageAttribute;*ProgIdAttribute"
                                 Register = OpenCover.RegisterType.RegisterUser
                                 Output = report })
        ("/sn=" + keyfile + AltCoverFilter + "-x=" + altReport + " -o __SelfTest")
    ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                       TargetDir = "_Reports/_OpenCoverSelfTest"})
        [report]

    printfn "Re-instrument everything"
    let altReport2 = reports @@ "AltCoverSelfTestDummy.xml"
    let result = ExecProcess (fun info -> { info with
                                                 FileName = "_Binaries/AltCover.Tests/Debug+AnyCPU/__SelfTest/AltCover.exe"
                                                 WorkingDirectory = "_Binaries/AltCover.Tests/Debug+AnyCPU"
                                                 Arguments = ("/sn=" + keyfile + AltCoverFilter + @"/o=./__SelfTestDummy -x=" + altReport2)}) (TimeSpan.FromMinutes 5.0)
    Assert.That(result, Is.EqualTo 0, "Re-instrument returned with a non-zero exit code")

    ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                       TargetDir = "_Reports/_AltCoverSelfTest"})
        [altReport]
)

// Packaging

Target "Packaging" (fun _ ->
    ensure "./_Binaries/Packaging"
    ensure "./_Packaging"

    let AltCover = getFullName "_Binaries/AltCover/AltCover.exe"
    let recorder = getFullName "_Binaries/AltCover/Release+AnyCPU/AltCover.Recorder.dll"
    let packable = getFullName "./_Binaries/README.html"
    let resources = DirectoryInfo.getMatchingFilesRecursive "AltCover.resources.dll" (DirectoryInfo.ofPath (getFullName "_Binaries/AltCover/Release+AnyCPU"))

    let applicationFiles = if runningInMono |> not then
                            [
                                (AltCover, Some "tools/net45", None)
                                (recorder, Some "tools/net45", None)
                                (packable, Some "", None)
                            ]
                           else []
    let resourceFiles = if runningInMono |> not then
                          resources
                          |> Seq.map (fun x -> x.FullName)
                          |> Seq.map (fun x -> (x, Some ("tools/net45/" + Path.GetFileName(Path.GetDirectoryName(x))), None))
                          |> Seq.toList
                        else []

    let root = (getFullName ".").Length
    let netcoreFiles = [
                         [
                             getFullName "./altcover.dotnet.sln"
                         ];
                         ((!! "./AltCover/*")
                          |> Seq.filter (fun n -> n.EndsWith(".fs") || n.EndsWith(".core.fsproj") || n.EndsWith(".resx"))
                          |> Seq.toList);
                         ((!! "./AltCover.Recorder/*")
                          |> Seq.filter (fun n -> n.EndsWith(".fs") || n.EndsWith(".core.fsproj") || n.EndsWith(".resx"))
                          |> Seq.toList);
                         ((!! "./_Generated/*")
                          |> Seq.toList)
                       ]
                       |> List.concat
                       |> List.map (fun x -> (x, Some ("tools/netcoreapp2.0" + Path.GetDirectoryName(x).Substring(root).Replace("\\","/")), None))

    printfn "Executing on %A" Environment.OSVersion
    NuGet (fun p ->
    {p with
        Authors = ["Steve Gilham"]
        Project = "altcover"
        Description = "A pre-instrumented code coverage tool for .net/.net core and Mono"
        OutputPath = "./_Packaging"
        WorkingDir = "./_Binaries/Packaging"
        Files = List.concat [applicationFiles; resourceFiles; netcoreFiles]
        Version = !Version
        Copyright = (!Copyright).Replace("©", "(c)")
        Publish = false
        ReleaseNotes = getFullName "ReleaseNotes.md"
                       |> File.ReadAllText
        ToolPath = if isWindows then p.ToolPath else "/usr/bin/nuget"
        })
        "./Build/AltCover.nuspec"
)

Target "PrepareFrameworkBuild" (fun _ ->
    let toolpath = findToolInSubPath "ILMerge.exe" "./packages"
    let here = Directory.GetCurrentDirectory()

    ILMerge (fun p -> { p with DebugInfo = true
                               ToolPath = toolpath
                               TargetKind = TargetKind.Exe
                               KeyFile = "./Build/Infrastructure.snk"
                               Version = (String.Join(".", (!Version).Split('.') |> Seq.take 2) + ".0.0")
                               Internalize = InternalizeTypes.Internalize
                               Libraries = Seq.concat [!! "./_Binaries/AltCover/Release+AnyCPU/Mono.C*.dll"; !! "./_Binaries/AltCover/Release+AnyCPU/Newton*.dll"]
                                           |> Seq.map (fun f -> f.Replace(here, "."))
                               AttributeFile = "./_Binaries/AltCover/Release+AnyCPU/AltCover.exe"})
                               "./_Binaries/AltCover/AltCover.exe"
                               "./_Binaries/AltCover/Release+AnyCPU/AltCover.exe"
)

Target "PrepareDotNetBuild" ignore

Target "PrepareReadMe" (fun _ ->
    Actions.PrepareReadMe ((!Copyright).Replace("©", "&#xa9;").Replace("<","&lt;").Replace(">", "&gt;"))
)

// Post-packaging deployment touch test

Target "Deployment" ignore

Target "Unpack" (fun _ ->
  let nugget = !! "./_Packaging/*.nupkg" |> Seq.last
  let unpack = getFullName "_Packaging/Unpack"
  System.IO.Compression.ZipFile.ExtractToDirectory (nugget, unpack)
)

Target "SimpleReleaseTest" (fun _ ->
    let unpack = getFullName "_Packaging/Unpack/tools/net45"
    if (unpack @@ "AltCover.exe") |> File.Exists then
      Actions.SimpleInstrumentingRun "_Binaries/Sample1/Debug+AnyCPU" unpack "SimpleReleaseTest"
    else
          let test = findToolInSubPath "AltCover.exe" "./packages"
          if File.Exists test then
            Actions.SimpleInstrumentingRun "_Binaries/Sample1/Debug+AnyCPU" (Path.GetDirectoryName test) "SimpleReleaseTest"
          else printfn "Skipping -- AltCover.exe not packaged"
)

Target "SimpleMonoReleaseTest" (fun _ ->
    let unpack = getFullName "_Packaging/Unpack/tools/net45"
    if (unpack @@ "AltCover.exe") |> File.Exists then
      Actions.SimpleInstrumentingRun "_Mono/Sample1" unpack "SimpleMonoReleaseTest"
    else
          let test = findToolInSubPath "AltCover.exe" "./packages"
          if File.Exists test then
            Actions.SimpleInstrumentingRun "_Binaries/Sample1/Debug+AnyCPU" (Path.GetDirectoryName test) "SimpleReleaseTest"
          else printfn "Skipping -- AltCover.exe not packaged"
)

Target "ReleaseDotNetWithFramework" (fun _ ->
    ensure "./_Reports"
    let unpack0 = getFullName "_Packaging/Unpack/tools/net45"
    let unpack1 = findToolInSubPath "AltCover.exe" "./packages"
    let unpack = if File.Exists (unpack0 @@ "AltCover.exe") then unpack0
                 else Path.GetDirectoryName(unpack1)

    if (unpack @@ "AltCover.exe") |> File.Exists then
      let simpleReport = (getFullName "./_Reports") @@ ( "ReleaseDotNetWithFramework.xml")
      let sampleRoot = getFullName "./_Binaries/Sample1/Debug+AnyCPU/netcoreapp2.0"
      let instrumented = sampleRoot @@ "__Instrumented.ReleaseDotNetWithFramework"
      let result = ExecProcess (fun info -> { info with
                                                   FileName = unpack @@ "AltCover.exe"
                                                   WorkingDirectory = sampleRoot
                                                   Arguments = ("-t=System\. -t=Microsoft\. -x=" + simpleReport + " /o=" + instrumented)}) (TimeSpan.FromMinutes 5.0)
      Assert.That (result, Is.EqualTo 0)

      let result = Dotnet {dotnetOptions with WorkingDirectory = instrumented} "Sample1.dll"
      Assert.That(result.ExitCode, Is.EqualTo 0)

      Actions.ValidateSample1 "./_Reports/ReleaseDotNetWithFramework.xml" "ReleaseDotNetWithFramework"
    else printfn "Skipping -- AltCover.exe not packaged"
)

Target "ReleaseMonoWithDotNet" (fun _ ->
    ensure "./_Reports"
    let unpack = getFullName "_Packaging/Unpack/tools/netcoreapp2.0/AltCover"
    let x = getFullName "./_Reports/ReleaseMonoWithDotNet.xml"
    let o = getFullName "./_Mono/__Instrumented.ReleaseMonoWithDotNet"
    let i = getFullName "./_Mono/Sample1"
    let result = Dotnet {dotnetOptions with WorkingDirectory = unpack}
                          ("run --project altcover.core.fsproj -- -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")
    Assert.That(result.ExitCode, Is.EqualTo 0)

    let result2 = ExecProcess (fun info -> { info with
                                                  FileName = o @@ "Sample1.exe"
                                                  WorkingDirectory = o
                                                  Arguments = ""}) (TimeSpan.FromMinutes 5.0)
    Assert.That(result2, Is.EqualTo 0, "Instrumented .exe failed")

    Actions.ValidateSample1 "./_Reports/ReleaseMonoWithDotNet.xml" "ReleaseMonoWithDotNet"
)

Target "ReleaseDotNetWithDotNet" (fun _ ->
    ensure "./_Reports"
    let unpack = getFullName "_Packaging/Unpack/tools/netcoreapp2.0/AltCover"
    let x = getFullName "./_Reports/ReleaseDotNetWithDotNet.xml"
    let o = getFullName "./_Binaries/Sample1/__Instrumented.ReleaseDotNetWithDotNet"
    let i = getFullName "./_Binaries/Sample1/Debug+AnyCPU/netcoreapp2.0"
    let result = Dotnet {dotnetOptions with WorkingDirectory = unpack}
                          ("run --project altcover.core.fsproj -- -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")
    Assert.That(result.ExitCode, Is.EqualTo 0)

    let result = Dotnet dotnetOptions
                            (o @@ "Sample1.dll")
    Assert.That(result.ExitCode, Is.EqualTo 0)

    Actions.ValidateSample1 "./_Reports/ReleaseDotNetWithDotNet.xml" "ReleaseDotNetWithDotNet"
)

Target "ReleaseXUnitDotNetDemo" (fun _ ->
    ensure "./_Reports"
    "./Demo/xunit-dotnet/bin" |> getFullName |> CleanDir

    "./Demo/xunit-dotnet/xunit-dotnet.csproj"
    |> DotnetCompile
        (fun p ->
            { p with
                Configuration = BuildConfiguration.Debug
                Common = dotnetOptions})

    let unpack = getFullName "_Packaging/Unpack/tools/netcoreapp2.0/AltCover"
    let x = getFullName "./Demo/xunit-dotnet/bin/ReleaseXUnitDotNetDemo.xml"
    let o = getFullName "./Demo/xunit-dotnet/bin/Debug/netcoreapp2.0/__Instrumented.ReleaseXUnitDotNetDemo"
    let i = getFullName "./Demo/xunit-dotnet/bin/Debug/netcoreapp2.0"
    let result = Dotnet {dotnetOptions with WorkingDirectory = unpack}
                            ("run --project altcover.core.fsproj -- -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")
    Assert.That(result.ExitCode, Is.EqualTo 0)

    !! (o @@ "*")
    |> Copy i

    let result = Dotnet {dotnetOptions with WorkingDirectory = getFullName "./Demo/xunit-dotnet"}
                    "test --no-build --configuration Debug xunit-dotnet.csproj"
    Assert.That(result.ExitCode, Is.EqualTo 1, "Unexpected unit test return")

    use coverageFile = new FileStream(x, FileMode.Open, FileAccess.Read, FileShare.None, 4096, FileOptions.SequentialScan)
    let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))
    let recorded = coverageDocument.Descendants(XName.Get("seqpnt"))
                   |> Seq.toList

    let visits = ["0"; "1"; "2"]
                 |> List.map (fun n ->
                        recorded
                        |> Seq.filter (fun x -> x.Attribute(XName.Get("visitcount")).Value = n)
                        |> Seq.length)

    Assert.That(visits, Is.EquivalentTo[3; 5; 3])
)

Target "ReleaseXUnitDotNetRunnerDemo" (fun _ ->
    ensure "./_Reports"
    "./Demo/xunit-dotnet/bin" |> getFullName |> CleanDir

    "./Demo/xunit-dotnet/xunit-dotnet.csproj"
    |> DotnetCompile
        (fun p ->
            { p with
                Configuration = BuildConfiguration.Debug
                Common = dotnetOptions})

    let unpack = getFullName "_Packaging/Unpack/tools/netcoreapp2.0/AltCover"
    let x = getFullName "./Demo/xunit-dotnet/bin/ReleaseXUnitDotNetDemo.xml"
    let o = getFullName "./Demo/xunit-dotnet/bin/Debug/netcoreapp2.0/__Instrumented.ReleaseXUnitDotNetDemo"
    let i = getFullName "./Demo/xunit-dotnet/bin/Debug/netcoreapp2.0"
    let result = Dotnet {dotnetOptions with WorkingDirectory = unpack}
                          ("run --project altcover.core.fsproj -- -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")
    Assert.That (result.ExitCode, Is.EqualTo 0)

    !! (o @@ "*")
    |> Copy i

    let runner = getFullName "_Packaging/Unpack/tools/netcoreapp2.0/AltCover/altcover.core.fsproj"

    // Run
    let result = Dotnet {dotnetOptions with WorkingDirectory  = o}
                          ("run --project " + runner +
                          " -- Runner -x \"dotnet\" -r \"" + i +
                          "\" -w \"" + (getFullName "./Demo/xunit-dotnet") +
                          "\" -- test --no-build --configuration Debug  xunit-dotnet.csproj")
    Assert.That (result.ExitCode, Is.EqualTo 0)

    use coverageFile = new FileStream(x, FileMode.Open, FileAccess.Read, FileShare.None, 4096, FileOptions.SequentialScan)
    let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))
    let recorded = coverageDocument.Descendants(XName.Get("seqpnt"))
                   |> Seq.toList

    let visits = ["0"; "1"; "2"]
                 |> List.map (fun n ->
                        recorded
                        |> Seq.filter (fun x -> x.Attribute(XName.Get("visitcount")).Value = n)
                        |> Seq.length)

    Assert.That(visits, Is.EquivalentTo[3; 5; 3])
)

Target "ReleaseFSharpTypesDotNetRunner" ( fun _ ->
    ensure "./_Reports"
    let unpack = getFullName "_Packaging/Unpack/tools/netcoreapp2.0/AltCover"
    let x = getFullName "./_Reports/AltCoverReleaseFSharpTypesDotNetRunner.xml"
    let o = getFullName "Sample2/_Binaries/Sample2/Debug+AnyCPU/netcoreapp2.0"
    let i = getFullName "_Binaries/Sample2/Debug+AnyCPU/netcoreapp2.0"

    CleanDir o

    // Instrument the code
    let result = Dotnet {dotnetOptions with WorkingDirectory = unpack}
                          ("run --project altcover.core.fsproj --configuration Release -- -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")
    Assert.That (result.ExitCode, Is.EqualTo 0)

    Actions.ValidateFSharpTypes x ["main"]

    printfn "Execute the instrumented tests"
    let sample2 = getFullName "./Sample2/sample2.core.fsproj"
    let runner = getFullName "_Packaging/Unpack/tools/netcoreapp2.0/AltCover/altcover.core.fsproj"

    // Run
    let result = Dotnet {dotnetOptions with WorkingDirectory = o}
                          ("run --project " + runner +
                          " --configuration Release -- Runner -x \"dotnet\" -r \"" + o +
                          "\" -- test --no-build --configuration Debug " +
                          sample2)
    Assert.That (result.ExitCode, Is.EqualTo 0)

    Actions.ValidateFSharpTypesCoverage x
)

Target "ReleaseXUnitFSharpTypesDotNet" ( fun _ ->
    ensure "./_Reports"
    let unpack = getFullName "_Packaging/Unpack/tools/netcoreapp2.0/AltCover"
    let x = getFullName "./_Reports/ReleaseXUnitFSharpTypesDotNet.xml"
    let o = getFullName "Sample4/_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.0"
    let i = getFullName "_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.0"

    CleanDir o

    // Instrument the code
    let result = Dotnet {dotnetOptions with WorkingDirectory = unpack}
                          ("run --project altcover.core.fsproj -- -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")
    Assert.That (result.ExitCode, Is.EqualTo 0)

    Actions.ValidateFSharpTypes x ["main"]

    printfn "Execute the instrumented tests"
    let result = Dotnet {dotnetOptions with WorkingDirectory = getFullName "Sample4"}
                            ("test --no-build --configuration Debug sample4.core.fsproj")
    Assert.That(result.ExitCode, Is.EqualTo 0, "sample test returned with a non-zero exit code")
    Actions.ValidateFSharpTypesCoverage x
)

Target "ReleaseXUnitFSharpTypesDotNetRunner" ( fun _ ->
    ensure "./_Reports"
    let unpack = getFullName "_Packaging/Unpack/tools/netcoreapp2.0/AltCover"
    let x = getFullName "./_Reports/ReleaseXUnitFSharpTypesDotNetRunner.xml"
    let o = getFullName "Sample4/_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.0"
    let i = getFullName "_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.0"

    CleanDir o

    // Instrument the code
    let result = Dotnet {dotnetOptions with WorkingDirectory = unpack}
                          ("run --project altcover.core.fsproj --configuration Release -- -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")
    Assert.That (result.ExitCode, Is.EqualTo 0)

    Actions.ValidateFSharpTypes x ["main"]

    printfn "Execute the instrumented tests"
    let sample4 = getFullName "./Sample4/sample4.core.fsproj"
    let runner = getFullName "_Packaging/Unpack/tools/netcoreapp2.0/AltCover/altcover.core.fsproj"

    // Run
    let result = Dotnet {dotnetOptions with WorkingDirectory = o}
                          ("run --project " + runner +
                          " --configuration Release -- Runner -x \"dotnet\" -r \"" + o +
                          "\" -- test --no-build --configuration Debug " +
                          sample4)
    Assert.That (result.ExitCode, Is.EqualTo 0)
    Actions.ValidateFSharpTypesCoverage x
)

// AOB

Target "BulkReport" (fun _ ->
    printfn "Overall coverage reporting"
    ensure "./_Reports/_BulkReport"

    !! "./_Reports/*.xml"
    |> Seq.filter (fun f -> not <| f.EndsWith("Report.xml", StringComparison.OrdinalIgnoreCase))
    |> Seq.toList
    |> ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                          ReportTypes = [ ReportGeneratorReportType.Html; ReportGeneratorReportType.Badges ]
                                          TargetDir = "_Reports/_BulkReport"})
)

Target "All" ignore

// Dependencies

"Clean"
==> "SetVersion"
==> "Preparation"

"Preparation"
==> "BuildRelease"

"BuildRelease"
==> "BuildDebug"
==> "Compilation"

"BuildRelease"
==> "BuildMonoSamples"
==> "Compilation"

"BuildRelease"
==> "Lint"
==> "Analysis"

"Compilation"
?=> "Analysis"

"Compilation"
==> "FxCop"
=?> ("Analysis", not runningInMono) // not supported

"Compilation"
==> "Gendarme"
=?> ("Analysis", not runningInMono) // TODO -- refactor

"Compilation"
?=> "UnitTest"

"Compilation"
==> "JustUnitTest"
==> "UnitTest"

"Compilation"
==> "UnitTestDotNet"
==> "UnitTest"

"Compilation"
==> "UnitTestWithOpenCover"
=?> ("UnitTest", not runningInMono)  // OpenCover Mono support

"Compilation"
==> "UnitTestWithAltCover"
==> "UnitTest"

"UnitTestDotNet"
==> "UnitTestWithAltCoverCore"
==> "UnitTest"

"UnitTestDotNet"
==> "UnitTestWithAltCoverCoreRunner"
==> "UnitTest"

"Compilation"
?=> "OperationalTest"

"Compilation"
==> "FSharpTypes"
==> "OperationalTest"

"Compilation"
==> "FSharpTypesDotNet"
==> "OperationalTest"

"Compilation"
==> "FSharpTypesDotNetRunner"
==> "OperationalTest"

"Compilation"
==> "BasicCSharp"
==> "OperationalTest"

"Compilation"
==> "BasicCSharpMono"
==> "OperationalTest"

"Compilation"
==> "BasicCSharpUnderMono"
=?> ("OperationalTest", Option.isSome monoOnWindows)

"Compilation"
==> "BasicCSharpMonoUnderMono"
=?> ("OperationalTest", Option.isSome monoOnWindows)

"Compilation"
==> "CSharpMonoWithDotNet"
==> "OperationalTest"

"Compilation"
==> "CSharpDotNetWithDotNet"
==> "OperationalTest"

"Compilation"
==> "CSharpDotNetWithFramework"
==> "OperationalTest"

"Compilation"
==> "SelfTest"
=?> ("OperationalTest", not runningInMono)  // OpenCover Mono support AND Mono + F# + Fake build => no symbols

"Compilation"
?=> "Packaging"

"Compilation"
==> "PrepareFrameworkBuild"
=?> ("Packaging", not runningInMono)  // can't ILMerge

"Compilation"
==> "PrepareDotNetBuild"
==> "Packaging"

"Compilation"
==> "PrepareReadMe"
==> "Packaging"

"Packaging"
==> "Unpack"

"Compilation"
?=> "Deployment"

"Unpack"
==> "SimpleReleaseTest"
==> "Deployment"

"Unpack"
==> "SimpleMonoReleaseTest"
==> "Deployment"

"Unpack"
==> "ReleaseMonoWithDotNet"
==> "Deployment"

"Unpack"
==> "ReleaseDotNetWithDotNet"
==> "Deployment"

"Unpack"
==> "ReleaseDotNetWithFramework"
==> "Deployment"

"Unpack"
==> "ReleaseFSharpTypesDotNetRunner"
==> "Deployment"

"Unpack"
==> "ReleaseXUnitFSharpTypesDotNet"
==> "Deployment"

"Unpack"
==> "ReleaseXUnitFSharpTypesDotNetRunner"
==> "Deployment"

"Unpack"
==> "ReleaseXUnitDotNetDemo"

"Unpack"
==> "ReleaseXUnitDotNetRunnerDemo"

"Analysis"
==> "All"

"UnitTest"
==> "All"

"OperationalTest"
==> "All"

"Deployment"
==> "BulkReport"
==> "All"

RunOrDefault "All"