open System
open System.IO
open System.Xml
open System.Xml.Linq

open Actions

open Fake.Core
open Fake.Core.Globbing.Operators
open Fake.Core.Globbing.Tools
open Fake.Core.Target
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.DotNet.AssemblyInfoFile
open Fake.DotNet.NuGet.NuGet
open Fake.DotNet.Testing.NUnit3
open Fake.DotNet.Testing
open Fake.EnvironmentHelper
open Fake.FileHelper
open Fake.FxCopHelper
open Fake.ILMergeHelper
open Fake.IO
open Fake.IO.Directory
open Fake.IO.FileSystemOperators
open Fake.IO.Path
open Fake.ProcessHelper

open Fake.ReportGeneratorHelper
open FSharpLint.Fake
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

let monoOnWindows = if isWindows then
                       [programFiles; programFiles86]
                       |> List.filter (String.IsNullOrWhiteSpace >> not)
                       |> List.map (fun s -> s @@ "Mono/bin/mono.exe")
                       |> List.filter File.Exists
                       |> List.tryFind (fun _ -> true)
                    else None

let TargetCreate s f =
  Description s
  Target.Create s f

// Preparation

TargetCreate "Preparation" ignore

TargetCreate "Clean" (fun _ ->
    printfn "Cleaning the build and deploy folders"
    Actions.Clean ()
)

TargetCreate "SetVersion" (fun _ ->
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

TargetCreate "Compilation" ignore

TargetCreate "BuildRelease" (fun _ ->
    "AltCover.sln"
    |> MsBuild.build (fun p ->
            { p with
                Verbosity = Some MsBuild.MSBuildVerbosity.Normal
                Properties = [
                               "Configuration", "Release"
                               "DebugSymbols", "True"
                             ]})

    Fake.DotNetCli.Build
        (fun p ->
            { p with
                Configuration = "Release"
                Project =  "./altcover.core.sln"})
)

TargetCreate "BuildDebug" (fun _ ->
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

    Fake.DotNetCli.Build
        (fun p ->
            { p with
                Configuration = "Debug"
                Project =  "./altcover.core.sln"})
)

TargetCreate "BuildMonoSamples" (fun _ ->
    let mcs = findToolInSubPath "MCS.exe" ".."

    [
        ("./_Mono/Sample1", "-debug -out:./_Mono/Sample1/Sample1.exe  ./Sample1/Program.cs")
        ("./_Mono/Sample3", "-target:library -debug -out:./_Mono/Sample3/Sample3.dll  ./Sample3/Class1.cs")
    ]
    |> Seq.iter (fun (dir, cmd) -> ensure dir
                                   let result = ExecProcess (fun info -> info.FileName <- mcs
                                                                         info.WorkingDirectory <- "."
                                                                         info.Arguments <- cmd) (TimeSpan.FromMinutes 5.0)
                                   Assert.That(result, Is.EqualTo 0, "Mono compilation of '" + cmd + "' failed"))

    Actions.FixMVId ["./_Mono/Sample1/Sample1.exe"; "./_Mono/Sample3/Sample3.dll"]
)

// Code Analysis

TargetCreate "Analysis" ignore

TargetCreate "Lint" (fun _ ->
    !! "**/*.fsproj"
        |> Seq.filter (fun n -> n.IndexOf(".core.") = -1)
        |> Seq.iter (FSharpLint (fun options -> { options with FailBuildIfAnyWarnings = true }) ))

TargetCreate "Gendarme" (fun _ -> // Needs debug because release is compiled --standalone which contaminates everything
    ensure "./_Reports"
    let subjects = String.Join(" ",
                               [
                                "_Binaries/AltCover/Debug+AnyCPU/AltCover.exe"
                                "_Binaries/AltCover.Shadow/Debug+AnyCPU/AltCover.Shadow.dll"
                               ])
    let r = ExecProcess (fun info -> info.FileName <- (findToolInSubPath "gendarme.exe" "./packages")
                                     info.WorkingDirectory <- "."
                                     info.Arguments <- "--severity all --confidence all --config ./Build/rules.xml --console --html ./_Reports/gendarme.html " + subjects) (TimeSpan.FromMinutes 5.0)
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

TargetCreate "FxCop" (fun _ -> // Needs debug because release is compiled --standalone which contaminates everything
    ensure "./_Reports"
    let fxCop = combinePaths (environVar "VS150COMNTOOLS") "../../Team Tools/Static Analysis Tools/FxCop/FxCopCmd.exe"
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

TargetCreate "UnitTest" (fun _ ->
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

TargetCreate "JustUnitTest" (fun _ ->
    ensure "./_Reports"
    !! (@"_Binaries/*Tests/Debug+AnyCPU/*.Test*.dll") // Need to figure out why it doesn't build in Release
    |> NUnit3 (fun p -> { p with ToolPath = findToolInSubPath "nunit3-console.exe" "."
                                 WorkingDir = "."
                                 ResultSpecs = ["./_Reports/JustUnitTestReport.xml"] })
)

TargetCreate "UnitTestDotNet" (fun _ ->
    ensure "./_Reports"
    !! (@"./*Tests/*.tests.core.fsproj")
    |> Seq.iter (fun f -> printfn "Testing %s" f
                          Fake.DotNetCli.Test
                             (fun p ->
                                  { p with
                                      Configuration = "Debug"
                                      Project =  f}))
)

TargetCreate "UnitTestWithOpenCover.Run" (fun _ ->
    ensure "./_Reports/_UnitTestWithOpenCover.Run"
    let testFiles = !! (@"_Binaries/*Tests/Debug+AnyCPU/*.Test*.dll")
                    //|> Seq.map (fun f -> f.FullName)
    let coverage = getFullName "_Reports/UnitTestWithOpenCover.Run.xml"

    OpenCover.Run (fun p -> { p with 
                                 WorkingDir = "."
                                 ExePath = findToolInSubPath "OpenCover.Console.exe" "."
                                 TestRunnerExePath = findToolInSubPath "nunit3-console.exe" "."
                                 Filter = "+[AltCover]* +[AltCover.Shadow]* +[AltCover.Runner]* -[*]Microsoft.* -[*]System.* -[Sample*]*"
                                 MergeByHash = true
                                 OptionalArguments = "-excludebyattribute:*ExcludeFromCodeCoverageAttribute;*ProgIdAttribute"
                                 Register = OpenCover.RegisterType.RegisterUser
                                 Output = coverage })
        (String.Join(" ", testFiles) + " --result=./_Reports/UnitTestWithOpenCover.RunReport.xml")

    ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                       ReportTypes = [ ReportGeneratorReportType.Html; ReportGeneratorReportType.Badges; ReportGeneratorReportType.XmlSummary ]
                                       TargetDir = "_Reports/_UnitTestWithOpenCover.Run"})
        [coverage]

    if not <| String.IsNullOrWhiteSpace (environVar "APPVEYOR_BUILD_NUMBER") then
            ExecProcess (fun info -> info.FileName <- findToolInSubPath "coveralls.net.exe" "."
                                     info.WorkingDirectory <- "_Reports"
                                     info.Arguments <- ("--opencover " + coverage)) (TimeSpan.FromMinutes 5.0)
            |> ignore
)

// Hybrid (Self) Tests

TargetCreate "UnitTestWithAltCover" (fun _ ->
    ensure "./_Reports/_UnitTestWithAltCover"
    let keyfile = getFullName "Build/SelfTest.snk"
    let reports = getFullName "./_Reports"
    let altcover = findToolInSubPath "AltCover.exe" "./_Binaries"

    let testDirectory = getFullName "_Binaries/AltCover.Tests/Debug+AnyCPU"
    if !! (testDirectory @@ "AltCov*.pdb") |> Seq.length > 0 then

      let altReport = reports @@ "UnitTestWithAltCover.xml"
      printfn "Instrumented the code"
      let result = ExecProcess (fun info -> info.FileName <- altcover
                                            info.WorkingDirectory <- testDirectory
                                            info.Arguments <- ("/sn=" + keyfile + AltCoverFilter + @"/o=./__UnitTestWithAltCover -x=" + altReport)) (TimeSpan.FromMinutes 5.0)
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
      let result = ExecProcess (fun info -> info.FileName <- altcover
                                            info.WorkingDirectory <- shadowDir
                                            info.Arguments <- ("/sn=" + keyfile + AltCoverFilter + @"/o=./__ShadowTestWithAltCover -x=" + shadowReport)) (TimeSpan.FromMinutes 5.0)

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

TargetCreate "UnitTestWithAltCoverCore" (fun _ ->
    ensure "./_Reports/_UnitTestWithAltCover"
    let keyfile = getFullName "Build/SelfTest.snk"
    let reports = getFullName "./_Reports"
    let altcover = findToolInSubPath "AltCover.exe" "./_Binaries"

    let testDirectory = getFullName "_Binaries/AltCover.Tests/Debug+AnyCPU/netcoreapp2.0"
    let output = getFullName "Tests/_Binaries/AltCover.Tests/Debug+AnyCPU/netcoreapp2.0"

    let altReport = reports @@ "UnitTestWithAltCoverCore.xml"
    printfn "Instrumented the code"
    let result = ExecProcess (fun info -> info.FileName <- altcover
                                          info.WorkingDirectory <- testDirectory
                                          info.Arguments <- ("/sn=" + keyfile + AltCoverFilter + @"/o=" + output + " -x=" + altReport)) (TimeSpan.FromMinutes 5.0)
    Assert.That(result, Is.EqualTo 0, "first instrument returned with a non-zero exit code")

    printfn "Unit test the instrumented code"
    let result = ExecProcess (fun info -> info.FileName <- "dotnet"
                                          info.WorkingDirectory <- getFullName "Tests"
                                          info.Arguments <- ("test --no-build --configuration Debug altcover.tests.core.fsproj")) (TimeSpan.FromMinutes 5.0)
    Assert.That(result, Is.EqualTo 0, "first test returned with a non-zero exit code")

    printfn "Instrument the shadow tests"
    let shadowDir = "_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU/netcoreapp2.0"
    let shadowReport = reports @@ "ShadowTestWithAltCoverCore.xml"
    let shadowOut = getFullName "Shadow.Tests/_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU/netcoreapp2.0"
    let result = ExecProcess (fun info -> info.FileName <- altcover
                                          info.WorkingDirectory <- shadowDir
                                          info.Arguments <- ("/sn=" + keyfile + AltCoverFilterG + @"/o=" + shadowOut + " -x=" + shadowReport)) (TimeSpan.FromMinutes 5.0)
    Assert.That(result, Is.EqualTo 0, "second instrument returned with a non-zero exit code")
    printfn "Execute the shadow tests"
    let result = ExecProcess (fun info -> info.FileName <- "dotnet"
                                          info.WorkingDirectory <- getFullName "Shadow.Tests"
                                          info.Arguments <- ("test --no-build --configuration Debug altcover.recorder.tests.core.fsproj")) (TimeSpan.FromMinutes 5.0)
    Assert.That(result, Is.EqualTo 0, "second test returned with a non-zero exit code")

    ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                       ReportTypes = [ ReportGeneratorReportType.Html; ReportGeneratorReportType.Badges; ReportGeneratorReportType.XmlSummary]
                                       TargetDir = "_Reports/_UnitTestWithAltCoverCore"})
          [altReport; shadowReport]
)

TargetCreate "UnitTestWithAltCoverCoreRunner" (fun _ ->
    ensure "./_Reports/_UnitTestWithAltCover"
    let keyfile = getFullName "Build/SelfTest.snk"
    let reports = getFullName "./_Reports"
    let altcover = getFullName "./AltCover/altcover.core.fsproj"

    let testDirectory = getFullName "_Binaries/AltCover.Tests/Debug+AnyCPU/netcoreapp2.0"
    let output = getFullName "Tests/_Binaries/AltCover.Tests/Debug+AnyCPU/netcoreapp2.0"

    let altReport = reports @@ "UnitTestWithAltCoverCoreRunner.xml"
    printfn "Instrument the code"
    CleanDir output
    Fake.DotNetCli.RunCommand (fun p -> {p with WorkingDir = testDirectory})
                         ("run --project " + altcover + 
                          " -- " + AltCoverFilter + " -x \"" + altReport + "\" /o \"" + output + "\"")

    printfn "Unit test the instrumented code"
    let testproject = getFullName "./Tests/altcover.tests.core.fsproj"

    Fake.DotNetCli.RunCommand (fun info -> {info with WorkingDir = output })
                          ("run --project " + altcover +
                          " -- Runner -x \"dotnet\" -r \"" + output +
                          "\" -- test --no-build --configuration Debug " +
                          testproject)

    printfn "Instrument the shadow tests"
    let shadowDir = "_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU/netcoreapp2.0"
    let shadowReport = reports @@ "ShadowTestWithAltCoverCoreRunner.xml"
    let shadowOut = getFullName "Shadow.Tests/_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU/netcoreapp2.0"

    CleanDir shadowOut
    Fake.DotNetCli.RunCommand (fun p -> {p with WorkingDir = shadowDir})
                         ("run --project " + altcover + 
                          " -- " + AltCoverFilter + " -x \"" + shadowReport + "\" /o \"" + shadowOut + "\"")

    let shadowProject = getFullName "./Shadow.Tests/altcover.recorder.tests.core.fsproj"
    Fake.DotNetCli.RunCommand (fun info -> {info with WorkingDir = shadowOut })
                          ("run --project " + altcover +
                          " -- Runner -x \"dotnet\" -r \"" + shadowOut +
                          "\" -- test --no-build --configuration Debug " +
                          shadowProject)

    ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                       ReportTypes = [ ReportGeneratorReportType.Html; ReportGeneratorReportType.Badges; ReportGeneratorReportType.XmlSummary]
                                       TargetDir = "_Reports/_UnitTestWithAltCoverCoreRunner"})
          [altReport; shadowReport]
)

// Pure OperationalTests

TargetCreate "OperationalTest" ignore

TargetCreate "FSharpTypes" ( fun _ ->
    ensure "./_Reports"
    let simpleReport = (getFullName "./_Reports") @@ ( "AltCoverFSharpTypes.xml")
    let binRoot = getFullName "_Binaries/AltCover/Release+AnyCPU"
    let sampleRoot = getFullName "_Binaries/Sample2/Debug+AnyCPU"
    let instrumented = "__FSharpTypes"

    if sampleRoot @@ "Sample2.pdb" |> File.Exists then
      let result = ExecProcess (fun info -> info.FileName <- binRoot @@ "AltCover.exe"
                                            info.WorkingDirectory <- sampleRoot
                                            info.Arguments <- ("-t=System\. -t=Microsoft\. -x=" + simpleReport + " /o=./" + instrumented)) (TimeSpan.FromMinutes 5.0)
      Actions.ValidateFSharpTypes simpleReport []
    else
      printfn "Symbols not present; skipping"
)

TargetCreate "FSharpTypesDotNet" ( fun _ ->
    ensure "./_Reports"
    let project = getFullName "./AltCover/altcover.core.fsproj"
    let simpleReport = (getFullName "./_Reports") @@ ( "AltCoverFSharpTypesDotNet.xml")
    let sampleRoot = getFullName "_Binaries/Sample2/Debug+AnyCPU/netcoreapp2.0"
    let instrumented = getFullName "Sample2/_Binaries/Sample2/Debug+AnyCPU/netcoreapp2.0"
    Fake.DotNetCli.RunCommand (fun p -> {p with WorkingDir = sampleRoot})
                         ("run --project " + project + " -- -t \"System\\.\" -t \"Microsoft\\.\" -x \"" + simpleReport + "\" /o \"" + instrumented + "\"")

    Actions.ValidateFSharpTypes simpleReport ["main"]

    printfn "Execute the instrumented tests"
    let result = ExecProcess (fun info -> info.FileName <- "dotnet"
                                          info.WorkingDirectory <- getFullName "Sample2"
                                          info.Arguments <- ("test --no-build --configuration Debug sample2.core.fsproj")) (TimeSpan.FromMinutes 5.0)
    Assert.That(result, Is.EqualTo 0, "sample test returned with a non-zero exit code")
    Actions.ValidateFSharpTypesCoverage simpleReport
)

TargetCreate "FSharpTypesDotNetRunner" ( fun _ ->
    ensure "./_Reports"
    let project = getFullName "./AltCover/altcover.core.fsproj"
    let simpleReport = (getFullName "./_Reports") @@ ( "AltCoverFSharpTypesDotNetRunner.xml")
    let sampleRoot = getFullName "_Binaries/Sample2/Debug+AnyCPU/netcoreapp2.0"
    let instrumented = getFullName "Sample2/_Binaries/Sample2/Debug+AnyCPU/netcoreapp2.0"

    // Instrument the code
    Fake.DotNetCli.RunCommand (fun p -> {p with WorkingDir = sampleRoot})
                         ("run --project " + project + " --configuration Release -- -t \"System\\.\" -t \"Microsoft\\.\" -x \"" + simpleReport + "\" /o \"" + instrumented + "\"")

    Actions.ValidateFSharpTypes simpleReport ["main"]

    printfn "Execute the instrumented tests"
    let sample2 = getFullName "./Sample2/sample2.core.fsproj"

    Fake.DotNetCli.RunCommand (fun info -> {info with WorkingDir = instrumented })
                          ("run --project " + project +
                          " --configuration Release -- Runner -x \"dotnet\" -r \"" + instrumented +
                          "\" -- test --no-build --configuration Debug " +
                          sample2)

    Actions.ValidateFSharpTypesCoverage simpleReport
)

TargetCreate "BasicCSharp" (fun _ ->
   Actions.SimpleInstrumentingRun "_Binaries/Sample1/Debug+AnyCPU" "_Binaries/AltCover/Debug+AnyCPU" "BasicCSharp"
)

TargetCreate "BasicCSharpMono" (fun _ ->
    Actions.SimpleInstrumentingRun "_Mono/Sample1" "_Binaries/AltCover/Debug+AnyCPU" "BasicCSharpMono"
)

TargetCreate "BasicCSharpUnderMono" (fun _ ->
    monoOnWindows |>
    Actions.SimpleInstrumentingRunUnderMono "_Binaries/Sample1/Debug+AnyCPU" "_Binaries/AltCover/Debug+AnyCPU" "BasicCSharp"
)

TargetCreate "BasicCSharpMonoUnderMono" (fun _ ->
    monoOnWindows |>
    Actions.SimpleInstrumentingRunUnderMono "_Mono/Sample1" "_Binaries/AltCover/Debug+AnyCPU" "BasicCSharpMono"
)

TargetCreate "CSharpMonoWithDotNet" (fun _ ->
    ensure "./_Reports"
    let x = getFullName "./_Reports/CSharpMonoWithDotNet.xml"
    let o = getFullName "./_Mono/__Instrumented.CSharpMonoWithDotNet"
    let i = getFullName "./_Mono/Sample1"
    Fake.DotNetCli.RunCommand id ("run --project ./AltCover/altcover.core.fsproj -- -t \"System.\" -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")

    let result2 = ExecProcess (fun info -> info.FileName <- o @@ "/Sample1.exe"
                                           info.WorkingDirectory <- o
                                           info.Arguments <- "") (TimeSpan.FromMinutes 5.0)
    Assert.That(result2, Is.EqualTo 0, "Instrumented .exe failed")

    Actions.ValidateSample1 "./_Reports/CSharpMonoWithDotNet.xml" "CSharpMonoWithDotNet"
)

TargetCreate "CSharpDotNetWithDotNet" (fun _ ->
    ensure "./_Reports"
    let x = getFullName "./_Reports/CSharpDotNetWithDotNet.xml"
    let o = getFullName "../_Binaries/Sample1/__Instrumented.CSharpDotNetWithDotNet"
    let i = getFullName "./_Binaries/Sample1/Debug+AnyCPU/netcoreapp2.0"
    Fake.DotNetCli.RunCommand id ("_Binaries/AltCover/Debug+AnyCPU/netcoreapp2.0/AltCover.dll -t \"System.\" -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")

    Fake.DotNetCli.RunCommand id (o @@ "Sample1.dll")

    Actions.ValidateSample1 "./_Reports/CSharpDotNetWithDotNet.xml" "CSharpDotNetWithDotNet"
)

TargetCreate "CSharpDotNetWithFramework" (fun _ ->
    ensure "./_Reports"
    let simpleReport = (getFullName "./_Reports") @@ ( "CSharpDotNetWithFramework.xml")
    let binRoot = getFullName "_Binaries/AltCover/Release+AnyCPU"
    let sampleRoot = getFullName "_Binaries/Sample1/Debug+AnyCPU/netcoreapp2.0"
    let instrumented = getFullName "_Binaries/Sample1/__Instrumented.CSharpDotNetWithFramework"
    let result = ExecProcess (fun info -> info.FileName <- binRoot @@ "AltCover.exe"
                                          info.WorkingDirectory <- sampleRoot
                                          info.Arguments <- ("-t=System\. -t=Microsoft\. -x=" + simpleReport + " /o=" + instrumented)) (TimeSpan.FromMinutes 5.0)

    Fake.DotNetCli.RunCommand id (instrumented @@ "Sample1.dll")

    Actions.ValidateSample1 "./_Reports/CSharpDotNetWithFramework.xml" "CSharpDotNetWithFramework"
)

TargetCreate "SelfTest" (fun _ ->
    ensure "./_Reports/_Instrumented"
    let targetDir = "_Binaries/AltCover.Tests/Debug+AnyCPU"
    let reports = getFullName "./_Reports"
    let report = reports @@ "OpenCover.RunSelfTest.xml"
    let altReport = reports @@ "AltCoverSelfTest.xml"
    let keyfile = getFullName "Build/SelfTest.snk"

    printfn "Self-instrument under OpenCover.Run"
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
                                       TargetDir = "_Reports/_OpenCover.RunSelfTest"})
        [report]

    printfn "Re-instrument everything"
    let altReport2 = reports @@ "AltCoverSelfTestDummy.xml"
    let result = ExecProcess (fun info -> info.FileName <- "_Binaries/AltCover.Tests/Debug+AnyCPU/__SelfTest/AltCover.exe"
                                          info.WorkingDirectory <- "_Binaries/AltCover.Tests/Debug+AnyCPU"
                                          info.Arguments <- ("/sn=" + keyfile + AltCoverFilter + @"/o=./__SelfTestDummy -x=" + altReport2)) (TimeSpan.FromMinutes 5.0)
    Assert.That(result, Is.EqualTo 0, "Re-instrument returned with a non-zero exit code")

    ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                       TargetDir = "_Reports/_AltCoverSelfTest"})
        [altReport]
)

// Packaging

TargetCreate "Packaging" (fun _ ->
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

TargetCreate "PrepareFrameworkBuild" (fun _ ->
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

TargetCreate "PrepareDotNetBuild" (fun _ ->
    Fake.DotNetCli.Publish
      (fun p ->
           { p with
                WorkingDir =  getFullName "./AltCover"
                Project = "altcover.core.fsproj"
                Output = getFullName "./_Binaries/altcover.core"
                Configuration = "Release" })
)

TargetCreate "PrepareReadMe" (fun _ ->
    Actions.PrepareReadMe ((!Copyright).Replace("©", "&#xa9;").Replace("<","&lt;").Replace(">", "&gt;"))
)

// Post-packaging deployment touch test

TargetCreate "Deployment" ignore

TargetCreate "Unpack" (fun _ ->
  let nugget = !! "./_Packaging/*.nupkg" |> Seq.last
  let unpack = getFullName "_Packaging/Unpack"
  System.IO.Compression.ZipFile.ExtractToDirectory (nugget, unpack)
)

TargetCreate "SimpleReleaseTest" (fun _ ->
    let unpack = getFullName "_Packaging/Unpack/tools/net45"
    if (unpack @@ "AltCover.exe") |> File.Exists then
      Actions.SimpleInstrumentingRun "_Binaries/Sample1/Debug+AnyCPU" unpack "SimpleReleaseTest"
    else
          let test = findToolInSubPath "AltCover.exe" "./packages"
          if File.Exists test then
            Actions.SimpleInstrumentingRun "_Binaries/Sample1/Debug+AnyCPU" (Path.GetDirectoryName test) "SimpleReleaseTest"
          else printfn "Skipping -- AltCover.exe not packaged"
)

TargetCreate "SimpleMonoReleaseTest" (fun _ ->
    let unpack = getFullName "_Packaging/Unpack/tools/net45"
    if (unpack @@ "AltCover.exe") |> File.Exists then
      Actions.SimpleInstrumentingRun "_Mono/Sample1" unpack "SimpleMonoReleaseTest"
    else
          let test = findToolInSubPath "AltCover.exe" "./packages"
          if File.Exists test then
            Actions.SimpleInstrumentingRun "_Binaries/Sample1/Debug+AnyCPU" (Path.GetDirectoryName test) "SimpleReleaseTest"
          else printfn "Skipping -- AltCover.exe not packaged"
)

TargetCreate "ReleaseDotNetWithFramework" (fun _ ->
    ensure "./_Reports"
    let unpack0 = getFullName "_Packaging/Unpack/tools/net45"
    let unpack1 = findToolInSubPath "AltCover.exe" "./packages"
    let unpack = if File.Exists (unpack0 @@ "AltCover.exe") then unpack0
                 else Path.GetDirectoryName(unpack1)

    if (unpack @@ "AltCover.exe") |> File.Exists then
      let simpleReport = (getFullName "./_Reports") @@ ( "ReleaseDotNetWithFramework.xml")
      let sampleRoot = getFullName "./_Binaries/Sample1/Debug+AnyCPU/netcoreapp2.0"
      let instrumented = sampleRoot @@ "__Instrumented.ReleaseDotNetWithFramework"
      let result = ExecProcess (fun info -> info.FileName <- unpack @@ "AltCover.exe"
                                            info.WorkingDirectory <- sampleRoot
                                            info.Arguments <- ("-t=System\. -t=Microsoft\. -x=" + simpleReport + " /o=" + instrumented)) (TimeSpan.FromMinutes 5.0)

      Fake.DotNetCli.RunCommand (fun info -> { info with WorkingDir = instrumented }) "Sample1.dll"

      Actions.ValidateSample1 "./_Reports/ReleaseDotNetWithFramework.xml" "ReleaseDotNetWithFramework"
    else printfn "Skipping -- AltCover.exe not packaged"
)

TargetCreate "ReleaseMonoWithDotNet" (fun _ ->
    ensure "./_Reports"
    let unpack = getFullName "_Packaging/Unpack/tools/netcoreapp2.0/AltCover"
    let x = getFullName "./_Reports/ReleaseMonoWithDotNet.xml"
    let o = getFullName "./_Mono/__Instrumented.ReleaseMonoWithDotNet"
    let i = getFullName "./_Mono/Sample1"
    Fake.DotNetCli.RunCommand (fun info -> {info with WorkingDir = unpack })
                          ("run --project altcover.core.fsproj -- -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")

    let result2 = ExecProcess (fun info -> info.FileName <- o @@ "Sample1.exe"
                                           info.WorkingDirectory <- o
                                           info.Arguments <- "") (TimeSpan.FromMinutes 5.0)
    Assert.That(result2, Is.EqualTo 0, "Instrumented .exe failed")

    Actions.ValidateSample1 "./_Reports/ReleaseMonoWithDotNet.xml" "ReleaseMonoWithDotNet"
)

TargetCreate "ReleaseDotNetWithDotNet" (fun _ ->
    ensure "./_Reports"
    let unpack = getFullName "_Packaging/Unpack/tools/netcoreapp2.0/AltCover"
    let x = getFullName "./_Reports/ReleaseDotNetWithDotNet.xml"
    let o = getFullName "./_Binaries/Sample1/__Instrumented.ReleaseDotNetWithDotNet"
    let i = getFullName "./_Binaries/Sample1/Debug+AnyCPU/netcoreapp2.0"
    Fake.DotNetCli.RunCommand (fun info -> {info with WorkingDir = unpack })
                          ("run --project altcover.core.fsproj -- -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")

    Fake.DotNetCli.RunCommand id (o @@ "Sample1.dll")

    Actions.ValidateSample1 "./_Reports/ReleaseDotNetWithDotNet.xml" "ReleaseDotNetWithDotNet"
)

TargetCreate "ReleaseXUnitDotNetDemo" (fun _ ->
    ensure "./_Reports"
    "./Demo/xunit-dotnet/bin" |> getFullName |> CleanDir
    Fake.DotNetCli.Build
        (fun p ->
            { p with
                Configuration = "Debug"
                Project =  "./Demo/xunit-dotnet/xunit-dotnet.csproj"})
    let unpack = getFullName "_Packaging/Unpack/tools/netcoreapp2.0/AltCover"
    let x = getFullName "./Demo/xunit-dotnet/bin/ReleaseXUnitDotNetDemo.xml"
    let o = getFullName "./Demo/xunit-dotnet/bin/Debug/netcoreapp2.0/__Instrumented.ReleaseXUnitDotNetDemo"
    let i = getFullName "./Demo/xunit-dotnet/bin/Debug/netcoreapp2.0"
    Fake.DotNetCli.RunCommand (fun info -> {info with WorkingDir = unpack })
                          ("run --project altcover.core.fsproj -- -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")

    !! (o @@ "*")
    |> Copy i

    let result = ExecProcess (fun info -> info.FileName <- "dotnet"
                                          info.WorkingDirectory <- getFullName "./Demo/xunit-dotnet"
                                          info.Arguments <- ("test --no-build --configuration Debug xunit-dotnet.csproj")) (TimeSpan.FromMinutes 5.0)
    Assert.That(result, Is.EqualTo 1, "Unexpected unit test return")

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

TargetCreate "ReleaseXUnitDotNetRunnerDemo" (fun _ ->
    ensure "./_Reports"
    "./Demo/xunit-dotnet/bin" |> getFullName |> CleanDir
    Fake.DotNetCli.Build
        (fun p ->
            { p with
                Configuration = "Debug"
                Project =  "./Demo/xunit-dotnet/xunit-dotnet.csproj"})
    let unpack = getFullName "_Packaging/Unpack/tools/netcoreapp2.0/AltCover"
    let x = getFullName "./Demo/xunit-dotnet/bin/ReleaseXUnitDotNetDemo.xml"
    let o = getFullName "./Demo/xunit-dotnet/bin/Debug/netcoreapp2.0/__Instrumented.ReleaseXUnitDotNetDemo"
    let i = getFullName "./Demo/xunit-dotnet/bin/Debug/netcoreapp2.0"
    Fake.DotNetCli.RunCommand (fun info -> {info with WorkingDir = unpack })
                          ("run --project altcover.core.fsproj -- -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")

    !! (o @@ "*")
    |> Copy i

    let runner = getFullName "_Packaging/Unpack/tools/netcoreapp2.0/AltCover/altcover.core.fsproj"

    // Run
    Fake.DotNetCli.RunCommand (fun info -> {info with WorkingDir = o })
                          ("run --project " + runner +
                          " -- Runner -x \"dotnet\" -r \"" + i +
                          "\" -w \"" + (getFullName "./Demo/xunit-dotnet") +
                          "\" -- test --no-build --configuration Debug  xunit-dotnet.csproj")

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

TargetCreate "ReleaseFSharpTypesDotNetRunner" ( fun _ ->
    ensure "./_Reports"
    let unpack = getFullName "_Packaging/Unpack/tools/netcoreapp2.0/AltCover"
    let x = getFullName "./_Reports/AltCoverReleaseFSharpTypesDotNetRunner.xml"
    let o = getFullName "Sample2/_Binaries/Sample2/Debug+AnyCPU/netcoreapp2.0"
    let i = getFullName "_Binaries/Sample2/Debug+AnyCPU/netcoreapp2.0"

    CleanDir o

    // Instrument the code
    Fake.DotNetCli.RunCommand (fun info -> {info with WorkingDir = unpack })
                          ("run --project altcover.core.fsproj --configuration Release -- -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")

    Actions.ValidateFSharpTypes x ["main"]

    printfn "Execute the instrumented tests"
    let sample2 = getFullName "./Sample2/sample2.core.fsproj"
    let runner = getFullName "_Packaging/Unpack/tools/netcoreapp2.0/AltCover/altcover.core.fsproj"

    // Run
    Fake.DotNetCli.RunCommand (fun info -> {info with WorkingDir = o })
                          ("run --project " + runner +
                          " --configuration Release -- Runner -x \"dotnet\" -r \"" + o +
                          "\" -- test --no-build --configuration Debug " +
                          sample2)

    Actions.ValidateFSharpTypesCoverage x
)

TargetCreate "ReleaseXUnitFSharpTypesDotNet" ( fun _ ->
    ensure "./_Reports"
    let unpack = getFullName "_Packaging/Unpack/tools/netcoreapp2.0/AltCover"
    let x = getFullName "./_Reports/ReleaseXUnitFSharpTypesDotNet.xml"
    let o = getFullName "Sample4/_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.0"
    let i = getFullName "_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.0"

    CleanDir o

    // Instrument the code
    Fake.DotNetCli.RunCommand (fun info -> {info with WorkingDir = unpack })
                          ("run --project altcover.core.fsproj -- -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")

    Actions.ValidateFSharpTypes x ["main"]

    printfn "Execute the instrumented tests"
    let result = ExecProcess (fun info -> info.FileName <- "dotnet"
                                          info.WorkingDirectory <- getFullName "Sample4"
                                          info.Arguments <- ("test --no-build --configuration Debug sample4.core.fsproj")) (TimeSpan.FromMinutes 5.0)
    Assert.That(result, Is.EqualTo 0, "sample test returned with a non-zero exit code")
    Actions.ValidateFSharpTypesCoverage x
)

TargetCreate "ReleaseXUnitFSharpTypesDotNetRunner" ( fun _ ->
    ensure "./_Reports"
    let unpack = getFullName "_Packaging/Unpack/tools/netcoreapp2.0/AltCover"
    let x = getFullName "./_Reports/ReleaseXUnitFSharpTypesDotNetRunner.xml"
    let o = getFullName "Sample4/_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.0"
    let i = getFullName "_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.0"

    CleanDir o

    // Instrument the code
    Fake.DotNetCli.RunCommand (fun info -> {info with WorkingDir = unpack })
                          ("run --project altcover.core.fsproj --configuration Release -- -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")

    Actions.ValidateFSharpTypes x ["main"]

    printfn "Execute the instrumented tests"
    let sample4 = getFullName "./Sample4/sample4.core.fsproj"
    let runner = getFullName "_Packaging/Unpack/tools/netcoreapp2.0/AltCover/altcover.core.fsproj"

    // Run
    Fake.DotNetCli.RunCommand (fun info -> {info with WorkingDir = o })
                          ("run --project " + runner +
                          " --configuration Release -- Runner -x \"dotnet\" -r \"" + o +
                          "\" -- test --no-build --configuration Debug " +
                          sample4)

    Actions.ValidateFSharpTypesCoverage x
)

// AOB

TargetCreate "BulkReport" (fun _ ->
    printfn "Overall coverage reporting"
    ensure "./_Reports/_BulkReport"

    !! "./_Reports/*.xml"
    |> Seq.filter (fun f -> not <| f.EndsWith("Report.xml", StringComparison.OrdinalIgnoreCase))
    |> Seq.toList
    |> ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                          ReportTypes = [ ReportGeneratorReportType.Html; ReportGeneratorReportType.Badges ]
                                          TargetDir = "_Reports/_BulkReport"})
)

TargetCreate "All" ignore

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
==> "UnitTestWithOpenCover.Run"
=?> ("UnitTest", not runningInMono)  // OpenCover.Run Mono support

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
=?> ("OperationalTest", not runningInMono)  // OpenCover.Run Mono support AND Mono + F# + Fake build => no symbols

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