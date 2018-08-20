open System
open System.IO
open System.Xml
open System.Xml.Linq

open Actions

open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.DotNet.NuGet.NuGet
open Fake.DotNet.Testing.NUnit3
open Fake.Testing
open Fake.DotNet.Testing
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing
open Fake.IO.Globbing.Operators

open FSharpLint.Application
open NUnit.Framework
open System.Reflection
open System

let Copyright  = ref String.Empty
let Version = ref String.Empty

let OpenCoverFilter = "+[AltCove*]* -[*]Microsoft.* -[*]System.* +[*]N.*"
let AltCoverFilter= @" -m=WaitForExitCustom -e=Adapter -s=Mono -s=\.Recorder -s=Sample -s=nunit -e=Tests -t=System. -t=Sample3\.Class2 "
let AltCoverFilterX= @" -m=WaitForExitCustom -e=Adapter --s=Mono -s=\.Recorder -s=Sample -s=nunit -t=System\. -t=Sample3\.Class2 "
let AltCoverFilterG= @" -m=WaitForExitCustom -e=Adapter --s=Mono -s=\.Recorder\.g -s=Sample -s=nunit -e=Tests -t=System. -t=Sample3\.Class2 "

let programFiles = Environment.environVar "ProgramFiles"
let programFiles86 = Environment.environVar "ProgramFiles(x86)"
let dotnetPath = "dotnet" |> Fake.Core.Process.tryFindFileOnPath

let dotnetOptions (o:DotNet.Options) = match dotnetPath with
                                       | Some f -> {o with DotNetCliPath = f}
                                       | None -> o

let monoOnWindows = if Environment.isWindows then
                       [programFiles; programFiles86]
                       |> List.filter (String.IsNullOrWhiteSpace >> not)
                       |> List.map (fun s -> s @@ "Mono/bin/mono.exe")
                       |> List.filter File.Exists
                       |> List.tryFind (fun _ -> true)
                    else None

let dotnetPath86 = if Environment.isWindows then
                                let perhaps = [programFiles86]
                                              |> List.filter (String.IsNullOrWhiteSpace >> not)
                                              |> List.map (fun s -> s @@ "dotnet\dotnet.EXE")
                                              |> List.filter File.Exists
                                              |> List.tryFind (fun _ -> true)
                                match perhaps with
                                | Some path ->
                                    try // detect if we have the SDK
                                        DotNet.info (fun opt -> {opt with Common = { dotnetOptions opt.Common with DotNetCliPath = path }})
                                        |> ignore
                                        perhaps
                                    with
                                    | _ -> None
                                | _ -> None
                   else None

let nugetCache = Path.Combine (Environment.GetFolderPath Environment.SpecialFolder.UserProfile,
                               ".nuget/packages")

let pwsh = if Environment.isWindows then
                    Tools.findToolInSubPath "pwsh.exe" (programFiles @@ "PowerShell")
           else "pwsh"

let _Target s f =
  Target.description s
  Target.create s f

// Preparation

_Target "Preparation" ignore

_Target "Clean" (fun _ ->
    printfn "Cleaning the build and deploy folders"
    Actions.Clean ()
)

_Target "SetVersion" (fun _ ->
    let appveyor = Environment.environVar "APPVEYOR_BUILD_VERSION"
    let travis = Environment.environVar "TRAVIS_JOB_NUMBER"
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

    Directory.ensure "./_Generated"
    Actions.InternalsVisibleTo (!Version)
    let v' = !Version
    AssemblyInfoFile.create "./_Generated/AssemblyVersion.fs"
        [
         AssemblyInfo.Product "AltCover"
         AssemblyInfo.Version (majmin + ".0.0")
         AssemblyInfo.FileVersion v'
         AssemblyInfo.Company "Steve Gilham"
         AssemblyInfo.Trademark ""
         AssemblyInfo.Copyright copy
        ]
        (Some AssemblyInfoFileConfig.Default)

    AssemblyInfoFile.create "./_Generated/AssemblyVersion.cs"
        [
         AssemblyInfo.Product "AltCover"
         AssemblyInfo.Version (majmin + ".0.0")
         AssemblyInfo.FileVersion v'
         AssemblyInfo.Company "Steve Gilham"
         AssemblyInfo.Trademark ""
         AssemblyInfo.Copyright copy
        ]
        (Some AssemblyInfoFileConfig.Default)

    let hack = """namespace AltCover
module SolutionRoot =
  let location = """ + "\"\"\"" + (Path.getFullName ".") + "\"\"\""
    let path = "_Generated/SolutionRoot.fs"
    // Update the file only if it would change
    let old = if File.Exists(path) then File.ReadAllText(path) else String.Empty
    if not (old.Equals(hack)) then File.WriteAllText(path, hack)
)

// Basic compilation

_Target "Compilation" ignore

_Target "BuildRelease" (fun _ ->
  try
    "AltCover.sln"
    |> MSBuild.build (fun p ->
            { p with
                Verbosity = Some MSBuildVerbosity.Normal
                Properties = [
                               "Configuration", "Release"
                               "DebugSymbols", "True"
                             ]})

    "./altcover.core.sln"
    |> DotNet.build
        (fun p ->
            { p with
                Configuration = DotNet.BuildConfiguration.Release
                Common = dotnetOptions p.Common
            })
  with
  | x -> printfn "%A" x
         reraise()
)

_Target "BuildDebug" (fun _ ->
    "AltCover.sln"
    |> MSBuild.build (fun p ->
            { p with
                Verbosity = Some MSBuildVerbosity.Normal
                Properties = [
                               "Configuration", "Debug"
                               "DebugSymbols", "True"
                             ]})

    "./altcover.core.sln"
    |> DotNet.build
        (fun p ->
            { p with
                Configuration = DotNet.BuildConfiguration.Debug
                Common = dotnetOptions p.Common})
)

_Target "BuildMonoSamples" (fun _ ->
    let mcs = "_Binaries/MCS/Release+AnyCPU/MCS.exe"

    [
        ("./_Mono/Sample1", "-debug -out:./_Mono/Sample1/Sample1.exe  ./Sample1/Program.cs")
        ("./_Mono/Sample3", "-target:library -debug -out:./_Mono/Sample3/Sample3.dll  -lib:./packages/Mono.Cecil.0.10.0/lib/net40 -r:Mono.Cecil.dll ./Sample3/Class1.cs")
    ]
    |> Seq.iter (fun (dir, cmd) ->
       Directory.ensure dir
       ("Mono compilation of '" + cmd + "' failed")
       |> Actions.Run (fun info ->
            { info with
                    FileName = mcs
                    WorkingDirectory = "."
                    Arguments = cmd}) )

    Actions.FixMVId ["./_Mono/Sample1/Sample1.exe"; "./_Mono/Sample3/Sample3.dll"]
)

// Code Analysis

_Target "Analysis" ignore

_Target "Lint" (fun _ ->
//    !! "**/*.fsproj"
//        |> Seq.filter (fun n -> n.IndexOf(".core.") = -1)
//        |> Seq.collect (fun n -> !!(Path.GetDirectoryName n @@ "*.fs"))
//        |> Seq.iter (fun f -> match Lint.lintFile (Lint.OptionalLintParameters.Default) f (new Version "4.0") with
//                              | Lint.Failure x -> new InvalidOperationException(x.ToString()) |> raise
//                              | Lint.Success w -> w |> Seq.iter (printfn "%A"))
// => https://github.com/fsprojects/FSharpLint/issues/266
()
)

_Target "Gendarme" (fun _ -> // Needs debug because release is compiled --standalone which contaminates everything
    Directory.ensure "./_Reports"
    let subjects = String.Join(" ",
                               [
                                "_Binaries/AltCover/Debug+AnyCPU/AltCover.exe"
                                "_Binaries/AltCover.Shadow/Debug+AnyCPU/AltCover.Shadow.dll"
                               ])

    let rules = if Environment.isWindows then "./Build/rules.xml" else "./Build/rules-mono.xml"
    Actions.Run (fun info ->
        { info with
                FileName = (Tools.findToolInSubPath "gendarme.exe" "./packages")
                WorkingDirectory = "."
                Arguments = "--severity all --confidence all --config " + rules + " --console --html ./_Reports/gendarme.html " + subjects})
                "Gendarme Errors were detected"

    Actions.Run (fun info ->
            { info with
                    FileName = (Tools.findToolInSubPath "gendarme.exe" "./packages")
                    WorkingDirectory = "."
                    Arguments = "--severity all --confidence all --config ./Build/rules-posh.xml --console --html ./_Reports/gendarme.html _Binaries/AltCover.PowerShell/Debug+AnyCPU/AltCover.PowerShell.dll"})
                    "Gendarme Errors were detected"

    Actions.Run (fun info ->
        { info with
                FileName = (Tools.findToolInSubPath "gendarme.exe" "./packages")
                WorkingDirectory = "."
                Arguments = "--severity all --confidence all --config ./Build/rules-gtk.xml --console --html ./_Reports/gendarme.html _Binaries/AltCover.Visualizer/Debug+AnyCPU/AltCover.Visualizer.exe"})
                "Gendarme Errors were detected"
)

_Target "FxCop" (fun _ -> // Needs debug because release is compiled --standalone which contaminates everything
    Directory.ensure "./_Reports"

    let vsInstallPath = if Environment.isWindows then
                            use hklmKey = Microsoft.Win32.RegistryKey.OpenBaseKey(
                                                Microsoft.Win32.RegistryHive.LocalMachine,
                                                Microsoft.Win32.RegistryView.Registry32)
                            use key = hklmKey.OpenSubKey(@"SOFTWARE\Microsoft\VisualStudio\SxS\VS7")
                            key.GetValue("15.0") :?> string
                        else null

    let fxCop = Path.combine vsInstallPath "Team Tools/Static Analysis Tools/FxCop/FxCopCmd.exe"

    Actions.Run (fun info ->
        { info with
                FileName = fxCop
                WorkingDirectory = "."
                Arguments = "/c /f:\"_Binaries/AltCover/Debug+AnyCPU/AltCover.exe\" /o:\"_Reports/FxCopReport.xml\" /rid:-Microsoft.Design#CA1004 /rid:-Microsoft.Design#CA1006 /rid:-Microsoft.Design#CA1011 /rid:-Microsoft.Design#CA1062 /rid:-Microsoft.Maintainability#CA1506 /rid:-Microsoft.Naming#CA1704 /rid:-Microsoft.Naming#CA1707 /rid:-Microsoft.Naming#CA1709 /rid:-Microsoft.Naming#CA1715 /ignoregeneratedcode /s /t:AltCover.Augment,AltCover.CommandLine,AltCover.Filter,AltCover.FilterClass,AltCover.Fix,AltCover.Instrument,AltCover.KeyRecord,AltCover.KeyStore,AltCover.Main,AltCover.Naming,AltCover.Node,AltCover.ProgramDatabase,AltCover.Report,AltCover.Runner,AltCover.Visitor /gac"
        })
        "FxCop Errors were detected"
    Assert.That(File.Exists "_Reports/FxCopReport.xml", Is.False, "FxCop Errors were detected")

    Actions.Run (fun info ->
        { info with
                FileName = fxCop
                WorkingDirectory = "."
                Arguments = "/c /f:\"_Binaries/AltCover.Shadow/Debug+AnyCPU/AltCover.Shadow.dll\" /o:\"_Reports/FxCopReport.xml\" /rid:-Microsoft.Design#CA1004 /rid:-Microsoft.Design#CA1006 /rid:-Microsoft.Design#CA1011 /rid:-Microsoft.Design#CA1062 /rid:-Microsoft.Maintainability#CA1506 /rid:-Microsoft.Naming#CA1704 /rid:-Microsoft.Naming#CA1707 /rid:-Microsoft.Naming#CA1709 /rid:-Microsoft.Naming#CA1715 /t:AltCover.Recorder.Assist,AltCover.Recorder.Counter,AltCover.Recorder.Assist,AltCover.Recorder.Tracer,AltCover.Recorder.Instance /ignoregeneratedcode /s /gac"
        })
        "FxCop Errors were detected"
    Assert.That(File.Exists "_Reports/FxCopReport.xml", Is.False, "FxCop Errors were detected")

    Actions.Run (fun info ->
        { info with
                FileName = fxCop
                WorkingDirectory = "."
                Arguments = "/c /f:\"_Binaries/AltCover.PowerShell/Debug+AnyCPU/AltCover.PowerShell.dll\" /o:\"_Reports/FxCopReport.xml\" /ignoregeneratedcode /s /gac /r:+ThirdParty/Microsoft.PowerShell.CodeAnalysis.15.dll"
        })
        "FxCop Errors were detected"
    Assert.That(File.Exists "_Reports/FxCopReport.xml", Is.False, "FxCop Errors were detected")

    Actions.Run (fun info ->
        { info with
                FileName = fxCop
                WorkingDirectory = "."
                Arguments = "/c /f:\"_Binaries/AltCover.PowerShell/Debug+AnyCPU/AltCover.PowerShell.dll\" /o:\"_Reports/FxCopReport.xml\" /rid:-Microsoft.Usage#CA2235 /rid:-Microsoft.Performance#CA1819 /rid:-Microsoft.Design#CA1020 /rid:-Microsoft.Design#CA1004 /rid:-Microsoft.Design#CA1006 /rid:-Microsoft.Design#CA1011 /rid:-Microsoft.Design#CA1062 /rid:-Microsoft.Maintainability#CA1506 /rid:-Microsoft.Naming#CA1704 /rid:-Microsoft.Naming#CA1707 /rid:-Microsoft.Naming#CA1709 /rid:-Microsoft.Naming#CA1715 /ignoregeneratedcode /s /gac"
        })
        "FxCop Errors were detected"
    Assert.That(File.Exists "_Reports/FxCopReport.xml", Is.False, "FxCop Errors were detected")

    Actions.Run (fun info ->
        { info with
                FileName = fxCop
                WorkingDirectory = "."
                Arguments = "/c /f:\"_Binaries/AltCover.Visualizer/Debug+AnyCPU/AltCover.Visualizer.exe\" /o:\"_Reports/FxCopReport.xml\" /rid:-Microsoft.Usage#CA2208 /rid:-Microsoft.Usage#CA2235 /rid:-Microsoft.Maintainability#CA1506 /rid:-Microsoft.Design#CA1004 /rid:-Microsoft.Design#CA1006 /rid:-Microsoft.Naming#CA1707 /rid:-Microsoft.Design#CA1006 /rid:-Microsoft.Naming#CA1715 /rid:-Microsoft.Naming#CA1704 /rid:-Microsoft.Naming#CA1709 /t:AltCover.Augment,AltCover.Visualizer.Transformer,AltCover.Visualizer.CoverageFile,AltCover.Visualizer.Extensions,AltCover.Visualizer.Gui /ignoregeneratedcode /s /gac"
        })
        "FxCop Errors were detected"
    Assert.That(File.Exists "_Reports/FxCopReport.xml", Is.False, "FxCop Errors were detected")

    (* where does FakeLib, Version=3.33.0.0 come from??
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
*))

// Unit Test

_Target "UnitTest" (fun _ ->
  let numbers = !! (@"_Reports/_Unit*/Summary.xml")
                |> Seq.collect (fun f -> let xml = XDocument.Load f
                                         xml.Descendants(XName.Get("Linecoverage"))
                                         |> Seq.map (fun e -> let coverage = e.Value.Replace("%", String.Empty)
                                                              match Double.TryParse coverage with
                                                              | (false, _) -> Assert.Fail ("Could not parse coverage "+coverage)
                                                                              0.0
                                                              | (_, numeric) -> printfn "%s : %A" (f |> Path.GetDirectoryName |> Path.GetFileName) numeric
                                                                                numeric))
                |> Seq.toList

  if numbers |> List.tryFind (fun n -> n <= 99.0) |> Option.isSome then
     Assert.Fail("Coverage is too low")
)

_Target "JustUnitTest" (fun _ ->
    Directory.ensure "./_Reports"
    try
      let here = Path.getFullName "."
      !! (@"_Binaries/*Tests/Debug+AnyCPU/*XTest*.dll")
      |> Fake.DotNet.Testing.XUnit2.run (fun p -> { p   with ToolPath = Tools.findToolInSubPath "xunit.console.exe" "."
                                                             NUnitXmlOutputPath = Some "./_Reports/JustXUnitTestReport.xml"
                                                             WorkingDir = Some here
                                                             ShadowCopy = false})

      !! (@"_Binaries/*Tests*/Debug+AnyCPU/*Test*.dll")
      |> Seq.filter (fun f -> Path.GetFileName(f) <> "AltCover.XTests.dll" &&
                              Path.GetFileName(f) <> "xunit.runner.visualstudio.testadapter.dll")
      |> NUnit3.run (fun p -> { p   with ToolPath = Tools.findToolInSubPath "nunit3-console.exe" "."
                                         WorkingDir = "."
                                         // Labels = LabelsLevel.All
                                         ResultSpecs = ["./_Reports/JustUnitTestReport.xml"] })
    with
    | x -> printfn "%A" x
           reraise ()
)

_Target "BuildForUnitTestDotNet" (fun _ ->
      !! (@"./*Tests/*.tests.core.fsproj")
      |> Seq.iter (fun f -> printfn "Building %s" f
                            Actions.RunDotnet dotnetOptions "build"
                                ("--configuration Debug " + f)
                                f)
)

_Target "UnitTestDotNet" (fun _ ->
    Directory.ensure "./_Reports"
    try
      !! (@"./*Tests/*.tests.core.fsproj")
      |> Seq.iter (fun f -> printfn "Testing %s" f
                            Actions.RunDotnet dotnetOptions "test"
                                              ("--no-build --configuration Debug " + f)
                                              f)
    with
    | x -> printfn "%A" x
           reraise ()
)

_Target "BuildForCoverlet" (fun _ ->
      !! (@"./*Tests/*.tests.core.fsproj")
      |> Seq.iter (fun f -> printfn "Building %s" f
                            Actions.RunDotnet dotnetOptions "build"
                                ("--configuration Debug " + f)
                                f)
)

_Target "UnitTestDotNetWithCoverlet" (fun _ ->
    Directory.ensure "./_Reports"
    try
      let xml = !! (@"./*Tests/*.tests.core.fsproj")
                |> Seq.zip [
                    """--no-build --framework netcoreapp2.0 /p:CollectCoverage=true /p:CoverletOutputFormat=opencover /p:Exclude="\"[Sample*]*,[AltCover.Record*]*,[NUnit*]*,[AltCover.Shadow.Adapter]*\"" --configuration Debug """
                    """--no-build --framework netcoreapp2.0 /p:CollectCoverage=true /p:CoverletOutputFormat=opencover /p:Exclude="\"[Sample*]*,[AltCover.Record*]*,[NUnit*]*,[AltCover.Shadow.Adapter]*\"" --configuration Debug """
                    """--no-build --framework netcoreapp2.0 /p:CollectCoverage=true /p:CoverletOutputFormat=opencover /p:Exclude="\"[Sample*]*,[AltCover.Record*]*\"" --configuration Debug """
                           ]
                |> Seq.fold (fun l (p,f) ->
                                            printfn "Testing %s" f
                                            try
                                              Actions.RunDotnet dotnetOptions "test"
                                                                (p + f)
                                                                f
                                            with
                                            | x -> eprintf "%A" x

                                            let here = Path.GetDirectoryName f

                                            (here @@ "coverage.opencover.xml") :: l
                                              ) []
      ReportGenerator.generateReports
              (fun p -> { p with ExePath = Tools.findToolInSubPath "ReportGenerator.exe" "."
                                 ReportTypes = [ ReportGenerator.ReportType.Html; ReportGenerator.ReportType.XmlSummary ]
                                 TargetDir = "_Reports/_UnitTestWithCoverlet"})
              xml
    with
    | x -> printfn "%A" x
           reraise ()
)

_Target "UnitTestWithOpenCover" (fun _ ->
    Directory.ensure "./_Reports/_UnitTestWithOpenCover"
    let testFiles = !! (@"_Binaries/*Tests/Debug+AnyCPU/*Test*.dll")
                    |> Seq.filter (fun f -> Path.GetFileName(f) <> "AltCover.XTests.dll" &&
                                            Path.GetFileName(f) <> "xunit.runner.visualstudio.testadapter.dll")
    let xtestFiles = !! (@"_Binaries/*Tests/Debug+AnyCPU/*XTest*.dll")

    let coverage = Path.getFullName "_Reports/UnitTestWithOpenCover.xml"
    let xcoverage = Path.getFullName "_Reports/XUnitTestWithOpenCover.xml"

    try
      OpenCover.run (fun p -> { p with
                                   WorkingDir = "."
                                   ExePath = Tools.findToolInSubPath "OpenCover.Console.exe" "."
                                   TestRunnerExePath = Tools.findToolInSubPath "xunit.console.exe" "."
                                   Filter = "+[AltCover]* +[AltCover.Shadow]* +[AltCover.Runner]* +[AltCover.WeakNameTests]Alt* -[*]Microsoft.* -[*]System.* -[Sample*]*"
                                   MergeByHash = true
                                   OptionalArguments = "-excludebyattribute:*ExcludeFromCodeCoverageAttribute;*ProgIdAttribute"
                                   Register = OpenCover.RegisterType.RegisterUser
                                   Output = xcoverage })
          (String.Join(" ", xtestFiles) + " -parallel none -noshadow -nunit _Reports/XUnitTestWithOpenCoverReport.xml")

      OpenCover.run (fun p -> { p with
                                   WorkingDir = "."
                                   ExePath = Tools.findToolInSubPath "OpenCover.Console.exe" "."
                                   TestRunnerExePath = Tools.findToolInSubPath "nunit3-console.exe" "."
                                   Filter = "+[AltCover]* +[AltCover.Shadow]* +[AltCover.Runner]* +[AltCover.WeakNameTests]Alt* -[*]Microsoft.* -[*]System.* -[Sample*]*"
                                   MergeByHash = true
                                   OptionalArguments = "-excludebyattribute:*ExcludeFromCodeCoverageAttribute;*ProgIdAttribute"
                                   Register = OpenCover.RegisterType.RegisterUser
                                   Output = coverage })
          (String.Join(" ", testFiles) + " --result=./_Reports/UnitTestWithOpenCoverReport.xml")

    with
    | x -> printfn "%A" x
           reraise ()

    ReportGenerator.generateReports
                        (fun p -> { p with ExePath = Tools.findToolInSubPath "ReportGenerator.exe" "."
                                           ReportTypes = [ ReportGenerator.ReportType.Html; ReportGenerator.ReportType.XmlSummary ]
                                           TargetDir = "_Reports/_UnitTestWithOpenCover"})
        [coverage; xcoverage]
)

// Hybrid (Self) Tests

_Target "UnitTestWithAltCover" (fun _ ->
    Directory.ensure "./_Reports/_UnitTestWithAltCover"
    let keyfile = Path.getFullName "Build/SelfTest.snk"
    let reports = Path.getFullName "./_Reports"
    let altcover = Tools.findToolInSubPath "AltCover.exe" "./_Binaries"
    let here = Path.getFullName "."

    let testDirectory = Path.getFullName "_Binaries/AltCover.Tests/Debug+AnyCPU"
    let xtestDirectory = Path.getFullName "_Binaries/AltCover.XTests/Debug+AnyCPU"
    if !! (testDirectory @@ "AltCov*.pdb") |> Seq.length > 0 then

      let xaltReport = reports @@ "XUnitTestWithAltCover.xml"
      printfn "Instrumented the code"
      Actions.Run (fun info ->
          { info with
                FileName = altcover
                WorkingDirectory = xtestDirectory
                Arguments = (" /sn=" + keyfile + AltCoverFilter + @"/o=./__UnitTestWithAltCover -x=" + xaltReport)})
                "Re-instrument returned with a non-zero exit code"

      printfn "Unit test the instrumented code"
      !! (@"_Binaries/*Tests/Debug+AnyCPU/__UnitTestWithAltCover/*XTest*.dll")
      |> Fake.DotNet.Testing.XUnit2.run (fun p -> { p   with ToolPath = Tools.findToolInSubPath "xunit.console.exe" "."
                                                             NUnitXmlOutputPath = Some "./_Reports/XUnitTestWithAltCoverReport.xml"
                                                             WorkingDir = Some here
                                                             ShadowCopy = false})

      let altReport = reports @@ "UnitTestWithAltCover.xml"
      printfn "Instrumented the code"
      Actions.Run (fun info ->
          { info with
                FileName = altcover
                WorkingDirectory = testDirectory
                Arguments = (" /sn=" + keyfile + AltCoverFilter + @"/o=./__UnitTestWithAltCover -x=" + altReport)})
                "Re-instrument returned with a non-zero exit code"

      printfn "Unit test the instrumented code"
      try
        [ !! "_Binaries/AltCover.Tests/Debug+AnyCPU/__UnitTestWithAltCover/*.Tests.dll"
          !! "_Binaries/AltCover.Tests/Debug+AnyCPU/__UnitTestWithAltCover/*ple2.dll"]
        |> Seq.concat |> Seq.distinct
        |> NUnit3.run (fun p -> { p with ToolPath = Tools.findToolInSubPath "nunit3-console.exe" "."
                                         WorkingDir = "."
                                         // Labels = LabelsLevel.All
                                         ResultSpecs = ["./_Reports/UnitTestWithAltCoverReport.xml"] })
      with
      | x -> printfn "%A" x
             reraise ()

      printfn "Instrument the weakname tests"
      let weakDir = Path.getFullName  "_Binaries/AltCover.WeakNameTests/Debug+AnyCPU"
      let weakReport = reports @@ "WeakNameTestWithAltCover.xml"
      Actions.Run (fun info ->
          { info with
                FileName = altcover
                WorkingDirectory = weakDir
                Arguments = ("--opencover /sn=" + keyfile + AltCoverFilter + " /o=./__WeakNameTestWithAltCover -x=" + weakReport)})
                "Instrumenting the weakname tests failed"

      printfn "Execute the weakname tests"
      !! ("_Binaries/AltCover.WeakNameTests/Debug+AnyCPU/__WeakNameTestWithAltCover/*Test*.dll")
      |> NUnit3.run (fun p -> { p with ToolPath = Tools.findToolInSubPath "nunit3-console.exe" "."
                                       WorkingDir = "."
                                       ResultSpecs = ["./_Reports/WeakNameTestWithAltCoverReport.xml"] })

      printfn "Instrument the shadow tests"
      let shadowDir = Path.getFullName  "_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU"
      let shadowReport = reports @@ "ShadowTestWithAltCover.xml"
      Actions.Run (fun info ->
          { info with
                FileName = altcover
                WorkingDirectory = shadowDir
                Arguments = ("--opencover /sn=" + keyfile + AltCoverFilter + @"/o=./__ShadowTestWithAltCover -x=" + shadowReport)})
                "Instrumenting the shadow tests failed"

      printfn "Execute the shadow tests"
      !! ("_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU/__ShadowTestWithAltCover/*.Test*.dll")
      |> NUnit3.run (fun p -> { p with ToolPath = Tools.findToolInSubPath "nunit3-console.exe" "."
                                       WorkingDir = "."
                                       ResultSpecs = ["./_Reports/ShadowTestWithAltCoverReport.xml"] })

      ReportGenerator.generateReports
                     (fun p -> { p with ExePath = Tools.findToolInSubPath "ReportGenerator.exe" "."
                                        ReportTypes = [ ReportGenerator.ReportType.Html; ReportGenerator.ReportType.XmlSummary ]
                                        TargetDir = "_Reports/_UnitTestWithAltCover"})
          [xaltReport; altReport; weakReport; shadowReport]
    else
      printfn "Symbols not present; skipping"
)

_Target "UnitTestWithAltCoverRunner" (fun _ ->
    Directory.ensure "./_Reports/_UnitTestWithAltCover"
    let keyfile = Path.getFullName "Build/SelfTest.snk"
    let reports = Path.getFullName "./_Reports"
    let altcover = Tools.findToolInSubPath "AltCover.exe" "./_Binaries/AltCover/Debug+AnyCPU"
    let nunit = Tools.findToolInSubPath "nunit3-console.exe" "."
    let here = Path.getFullName "."

    let testDirectory = Path.getFullName "_Binaries/AltCover.Tests/Debug+AnyCPU"
    let xtestDirectory = Path.getFullName "_Binaries/AltCover.XTests/Debug+AnyCPU"
    if !! (testDirectory @@ "AltCov*.pdb") |> Seq.length > 0 then

      let xaltReport = reports @@ "XUnitTestWithAltCoverRunner.xml"
      printfn "Instrumented the code"
      Actions.Run (fun info ->
          { info with
                FileName = altcover
                WorkingDirectory = xtestDirectory
                Arguments = ("--opencover  /sn=" + keyfile + AltCoverFilter + @"/o=./__UnitTestWithAltCoverRunner -x=" + xaltReport)})
                "Re-instrument returned with a non-zero exit code"

      printfn "Unit test the instrumented code"
      !! (@"_Binaries/*Tests/Debug+AnyCPU/__UnitTestWithAltCoverRunner/*XTest*.dll")
      |> Fake.DotNet.Testing.XUnit2.run (fun p -> { p   with ToolPath = Tools.findToolInSubPath "xunit.console.exe" "."
                                                             NUnitXmlOutputPath = Some "./_Reports/XUnitTestWithAltCoverRunnerReport.xml"
                                                             WorkingDir = Some here
                                                             ShadowCopy = false})

      let altReport = reports @@ "UnitTestWithAltCoverRunner.xml"
      printfn "Instrumented the code"
      Actions.Run (fun info ->
          { info with
                FileName = altcover
                WorkingDirectory = testDirectory
                Arguments = ("--opencover /sn=" + keyfile + AltCoverFilter + @"/o=./__UnitTestWithAltCoverRunner -x=" + altReport)})
                "Re-instrument returned with a non-zero exit code"

      printfn "Unit test the instrumented code"
      try
       let RunIt (f:Fake.Core.ProcStartInfo -> Fake.Core.ProcStartInfo) (msg:string) =
           let x = Fake.Core.Process.execSimple (f >> Fake.Core.Process.withFramework) (TimeSpan.FromMinutes 15.0)
           Assert.That(x, Is.EqualTo 0, msg)

       RunIt (fun info ->
          { info with
                FileName = altcover
                WorkingDirectory = "."
                Arguments = ( " Runner -x " + nunit +
                              " -r " + (testDirectory @@ "__UnitTestWithAltCoverRunner") +
                              " -w . -- " +
                              " --noheader --work=. --result=./_Reports/UnitTestWithAltCoverRunnerReport.xml \"" +
                              String.Join ("\" \"", [ Path.getFullName  "_Binaries/AltCover.Tests/Debug+AnyCPU/__UnitTestWithAltCoverRunner/AltCover.Tests.dll"
                                                      Path.getFullName  "_Binaries/AltCover.Tests/Debug+AnyCPU/__UnitTestWithAltCoverRunner/Sample2.dll"]) + "\""
                            )}) "Re-instrument tests returned with a non-zero exit code"
      with
      | x -> printfn "%A" x
             reraise ()

      printfn "Instrument the weakname tests"
      let weakDir = Path.getFullName  "_Binaries/AltCover.WeakNameTests/Debug+AnyCPU"
      let weakReport = reports @@ "WeakNameTestWithAltCoverRunner.xml"
      Actions.Run (fun info ->
          { info with
                FileName = altcover
                WorkingDirectory = weakDir
                Arguments = ("--opencover /sn=" + keyfile + AltCoverFilterX + " -t=WeakNameTest  /o=./__WeakNameTestWithAltCoverRunner -x=" + weakReport)})
                "Instrumenting the weakname tests failed"

      printfn "Execute the weakname tests"

      Actions.Run (fun info ->
          { info with
                FileName = altcover
                WorkingDirectory = "."
                Arguments = ( " Runner -x " + nunit +
                              " -r " + (weakDir @@ "__WeakNameTestWithAltCoverRunner") +
                              " -w . -- " +
                              " --noheader --work=. --result=./_Reports/ShadowTestWithAltCoverRunnerReport.xml \"" +
                              String.Join ("\" \"", [ Path.getFullName  "_Binaries/AltCover.WeakNameTests/Debug+AnyCPU/__WeakNameTestWithAltCoverRunner/AltCover.WeakNameTests.dll"]) + "\""
                            )}) "Re-instrument tests returned with a non-zero exit code"

      printfn "Instrument the shadow tests"
      let shadowDir = Path.getFullName  "_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU"
      let shadowReport = reports @@ "ShadowTestWithAltCoverRunner.xml"
      Actions.Run (fun info ->
          { info with
                FileName = altcover
                WorkingDirectory = shadowDir
                Arguments = ("--opencover /sn=" + keyfile + AltCoverFilter + @"/o=./__ShadowTestWithAltCoverRunner -x=" + shadowReport)})
                "Instrumenting the shadow tests failed"

      printfn "Execute the shadow tests"
      Actions.Run (fun info ->
          { info with
                FileName = altcover
                WorkingDirectory = "."
                Arguments = ( " Runner -x " + nunit +
                              " -r " + (shadowDir @@ "__ShadowTestWithAltCoverRunner") +
                              " -w . -- " +
                              " --noheader --work=. --result=./_Reports/ShadowTestWithAltCoverRunnerReport.xml \"" +
                              String.Join ("\" \"", [ Path.getFullName  "_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU/__ShadowTestWithAltCoverRunner/AltCover.Shadow.Tests.dll"
                                                      Path.getFullName  "_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU/__ShadowTestWithAltCoverRunner/AltCover.Shadow.Tests2.dll"]) + "\""
                            )}) "Re-instrument tests returned with a non-zero exit code"

      printfn "Instrument the GTK# visualizer tests"
      let gtkDir = Path.getFullName  "_Binaries/AltCover.Tests.Visualizer/Debug+AnyCPU"
      let gtkReport = reports @@ "GTKVTestWithAltCoverRunner.xml"
      Actions.Run (fun info ->
          { info with
                FileName = altcover
                WorkingDirectory = gtkDir
                Arguments = ("--opencover -t=Gui -s=\"\\-sharp\" /sn=" + keyfile + AltCoverFilter + @"/o=./__GTKVTestWithAltCoverRunner -x=" + gtkReport)})
                "Instrumenting the shadow tests failed"

      printfn "Execute the the GTK# visualizer tests"
      Actions.Run (fun info ->
          { info with
                FileName = altcover
                WorkingDirectory = "."
                Arguments = ( " Runner -x " + nunit +
                              " -r " + (gtkDir @@ "__GTKVTestWithAltCoverRunner") +
                              " -w . -- " +
                              " --noheader --work=. --result=./_Reports/ShadowTestWithAltCoverRunnerReport.xml \"" +
                              String.Join ("\" \"", [ Path.getFullName  "_Binaries/AltCover.Tests.Visualizer/Debug+AnyCPU/__GTKVTestWithAltCoverRunner/AltCover.Tests.Visualizer.dll"]) + "\""
                            )}) "Re-instrument tests returned with a non-zero exit code"

      let pester = Path.getFullName "_Reports/Pester.xml"
      ReportGenerator.generateReports
                      (fun p -> { p with ExePath = Tools.findToolInSubPath "ReportGenerator.exe" "."
                                         ReportTypes = [ ReportGenerator.ReportType.Html; ReportGenerator.ReportType.XmlSummary ]
                                         TargetDir = "_Reports/_UnitTestWithAltCoverRunner"})
          [xaltReport; altReport; shadowReport; weakReport; pester]

      let cover1 = altReport
                   |> File.ReadAllLines
                   |> Seq.takeWhile (fun l -> l <> "  </Modules>")
      let cover2 = shadowReport
                   |> File.ReadAllLines
                   |> Seq.skipWhile (fun l -> l.StartsWith("    <Module") |> not)
                   |> Seq.takeWhile (fun l -> l <> "  </Modules>")
      let cover3 = weakReport
                   |> File.ReadAllLines
                   |> Seq.skipWhile (fun l -> l.StartsWith("    <Module") |> not)
                   |> Seq.takeWhile (fun l -> l <> "  </Modules>")
      let cover3a = pester
                   |> File.ReadAllLines
                   |> Seq.skipWhile (fun l -> l.StartsWith("    <Module") |> not)
                   |> Seq.takeWhile (fun l -> l <> "  </Modules>")
      let cover4 = xaltReport
                   |> File.ReadAllLines
                   |> Seq.skipWhile (fun l -> l.StartsWith("    <Module") |> not)

      let coverage =  reports @@ "CombinedTestWithAltCoverRunner.coveralls"

      File.WriteAllLines(coverage, Seq.concat [cover1; cover2; cover3; cover3a; cover4] |> Seq.toArray)

      if not <| String.IsNullOrWhiteSpace (Environment.environVar "APPVEYOR_BUILD_NUMBER") then
       Actions.Run (fun info ->
          { info with
                FileName = Tools.findToolInSubPath "coveralls.net.exe" nugetCache
                WorkingDirectory = "_Reports"
                Arguments = ("--opencover " + coverage)}) "Coveralls upload failed"
    else
      printfn "Symbols not present; skipping"
)

_Target "UnitTestWithAltCoverCore" (fun _ ->
    Directory.ensure "./_Reports/_UnitTestWithAltCover"
    let keyfile = Path.getFullName "Build/SelfTest.snk"
    let reports = Path.getFullName "./_Reports"
    let altcover = Tools.findToolInSubPath "AltCover.exe" "./_Binaries"

    let testDirectory = Path.getFullName "_Binaries/AltCover.Tests/Debug+AnyCPU/netcoreapp2.0"
    let output = Path.getFullName "Tests/_Binaries/AltCover.Tests/Debug+AnyCPU/netcoreapp2.0"

    let altReport = reports @@ "UnitTestWithAltCoverCore.xml"
    printfn "Instrumented the code"
    Actions.Run (fun info ->
        { info with
                FileName = altcover
                WorkingDirectory = testDirectory
                Arguments = ("/sn=" + keyfile + AltCoverFilter + @"/o=" + output + " -x=" + altReport)})
                "first instrument returned with a non-zero exit code"

    printfn "Unit test the instrumented code"
    try
      Actions.RunDotnet (fun o -> {dotnetOptions o with WorkingDirectory = Path.getFullName "Tests"}) "test"
                        ("-v m --no-build --configuration Debug --verbosity normal altcover.tests.core.fsproj")
                        "first test returned with a non-zero exit code"
    with
    | x -> printfn "%A" x
           reraise ()

    printfn "Instrument the shadow tests"
    let shadowDir = "_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU/netcoreapp2.0"
    let shadowReport = reports @@ "ShadowTestWithAltCoverCore.xml"
    let shadowOut = Path.getFullName "Shadow.Tests/_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU/netcoreapp2.0"
    Actions.Run (fun info ->
        { info with
                FileName = altcover
                WorkingDirectory = shadowDir
                Arguments = ("/sn=" + keyfile + AltCoverFilterG + @"/o=" + shadowOut + " -x=" + shadowReport)})
                "second instrument returned with a non-zero exit code"

    printfn "Execute the shadow tests"
    Actions.RunDotnet(fun o -> {dotnetOptions o with WorkingDirectory = Path.getFullName "Shadow.Tests"}) "test"
                      ("-v m --no-build --configuration Debug --verbosity normal altcover.recorder.tests.core.fsproj")
                      "second test returned with a non-zero exit code"

    printfn "Instrument the XUnit tests"
    let xDir = "_Binaries/AltCover.XTests/Debug+AnyCPU/netcoreapp2.0"
    let xReport = reports @@ "XTestWithAltCoverCore.xml"
    let xOut = Path.getFullName "XTests/_Binaries/AltCover.XTests/Debug+AnyCPU/netcoreapp2.0"
    Actions.Run (fun info ->
        { info with
                FileName = altcover
                WorkingDirectory = xDir
                Arguments = ("/sn=" + keyfile + AltCoverFilterG + @"/o=" + xOut + " -x=" + xReport)})
                "xuint instrument returned with a non-zero exit code"

    printfn "Execute the XUnit tests"
    Actions.RunDotnet(fun o -> {dotnetOptions o with WorkingDirectory = Path.getFullName "XTests"}) "test"
                      ("-v m --no-build --configuration Debug --verbosity normal altcover.x.tests.core.fsproj")
                      "xuint test returned with a non-zero exit code"

    ReportGenerator.generateReports
                       (fun p -> { p with ExePath = Tools.findToolInSubPath "ReportGenerator.exe" "."
                                          ReportTypes = [ ReportGenerator.ReportType.Html; ReportGenerator.ReportType.XmlSummary]
                                          TargetDir = "_Reports/_UnitTestWithAltCoverCore"})
          [altReport; shadowReport; xReport]
)

_Target "UnitTestWithAltCoverCoreRunner" (fun _ ->
    Directory.ensure "./_Reports/_UnitTestWithAltCover"
    let reports = Path.getFullName "./_Reports"
    let altcover = Path.getFullName "./_Binaries/AltCover/Release+AnyCPU/netcoreapp2.0/AltCover.dll"
    let testDirectory = Path.getFullName "_Binaries/AltCover.Tests/Debug+AnyCPU/netcoreapp2.0"
    let output = Path.getFullName "Tests/_Binaries/AltCover.Tests/Debug+AnyCPU/netcoreapp2.0"

    // W/o --single
    // UnitTestWithAltCoverCoreRunner.xml.0.acv (3,066,500b)
    // 2,319,766 visits recorded in 00:00:03.6291581 (639,202 visits/sec)
    // ShadowTestWithAltCoverCoreRunner.xml.0.acv (5,503b)
    // 3,162 visits recorded in 00:00:00.0589018 (53,683 visits/sec)
    // XTestWithAltCoverCoreRunner.xml.0.acv (50,201b)
    // 32,134 visits recorded in 00:00:00.0859144 (374,023 visits/sec)
    // UnitTestWithAltCoverCoreRunner   00:01:01.8200156

    // W/ --single
    // UnitTestWithAltCoverCoreRunner.xml.0.acv (10,373b)
    // 3,082 visits recorded in 00:00:00.0586471 (52,552 visits/sec)
    // ShadowTestWithAltCoverCoreRunner.xml.0.acv (1,614b)
    // 453 visits recorded in 00:00:00.0542989 (8,343 visits/sec)
    // XTestWithAltCoverCoreRunner.xml.0.acv (5,365b)
    // 1,537 visits recorded in 00:00:00.0556820 (27,603 visits/sec)
    // UnitTestWithAltCoverCoreRunner   00:00:54.7479602

    let altReport = reports @@ "UnitTestWithAltCoverCoreRunner.xml"
    printfn "Instrument the code"
    Shell.cleanDir output
    Actions.RunDotnet (fun o -> {dotnetOptions o with WorkingDirectory = testDirectory}) ""
                      (altcover +
                             " --single --opencover " + AltCoverFilter + " -x \"" + altReport + "\" /o \"" + output + "\"")
                             "Instrument the code"

    printfn "Unit test the instrumented code"
    let testproject = Path.getFullName "./Tests/altcover.tests.core.fsproj"

    Actions.RunDotnet (fun o -> {dotnetOptions o with WorkingDirectory = output}) ""
                            (altcover +
                             " Runner -x \"dotnet\" -r \"" + output +
                             "\" -- test --no-build --configuration Debug --verbosity normal " +
                             testproject)
                             "Unit test the instrumented code"

    printfn "Instrument the shadow tests"
    let shadowDir = "_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU/netcoreapp2.0"
    let shadowReport = reports @@ "ShadowTestWithAltCoverCoreRunner.xml"
    let shadowOut = Path.getFullName "Shadow.Tests/_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU/netcoreapp2.0"

    Shell.cleanDir shadowOut
    Actions.RunDotnet (fun o -> {dotnetOptions o with WorkingDirectory = shadowDir}) ""
                      ( altcover +
                             " --single --opencover " + AltCoverFilter + " -x \"" + shadowReport + "\" /o \"" + shadowOut + "\"")
                             "Instrument the shadow tests"

    let shadowProject = Path.getFullName "./Shadow.Tests/altcover.recorder.tests.core.fsproj"
    Actions.RunDotnet (fun o -> {dotnetOptions o with WorkingDirectory = shadowOut}) ""
                            (altcover +
                             " Runner -x \"dotnet\" -r \"" + shadowOut +
                             "\" -- test --no-build --configuration Debug --verbosity normal " +
                             shadowProject)
                             "Run the shadow tests"

    printfn "Instrument the XUnit tests"
    let xDir = "_Binaries/AltCover.XTests/Debug+AnyCPU/netcoreapp2.0"
    let xReport = reports @@ "XTestWithAltCoverCoreRunner.xml"
    let xOut = Path.getFullName "XTests/_Binaries/AltCover.XTests/Debug+AnyCPU/netcoreapp2.0"
    Shell.cleanDir xOut

    Actions.RunDotnet (fun o -> {dotnetOptions o with WorkingDirectory = xDir}) ""
                      ( altcover +
                             " --single --opencover " + AltCoverFilter + " -x \"" + xReport + "\" /o \"" + xOut + "\"")
                             "Instrument the xunit tests"

    printfn "Execute the XUnit tests"
    let xProject = Path.getFullName "./XTests/altcover.x.tests.core.fsproj"
    Actions.RunDotnet (fun o -> {dotnetOptions o with WorkingDirectory = xOut}) ""
                            (altcover +
                             " Runner -x \"dotnet\" -r \"" + xOut +
                             "\" -- test --no-build --configuration Debug --verbosity normal " +
                             xProject)
                             "Run the shadow tests"

    ReportGenerator.generateReports
                       (fun p -> { p with ExePath = Tools.findToolInSubPath "ReportGenerator.exe" "."
                                          ReportTypes = [ ReportGenerator.ReportType.Html; ReportGenerator.ReportType.XmlSummary]
                                          TargetDir = "_Reports/_UnitTestWithAltCoverCoreRunner"})
          [altReport; shadowReport; xReport]
)

// Pure OperationalTests

_Target "OperationalTest" ignore

_Target "FSharpTypes" ( fun _ ->
    Directory.ensure "./_Reports"
    let simpleReport = (Path.getFullName "./_Reports") @@ ( "AltCoverFSharpTypes.xml")
    let binRoot = Path.getFullName "_Binaries/AltCover/Release+AnyCPU"
    let sampleRoot = Path.getFullName "_Binaries/Sample2/Debug+AnyCPU"
    let instrumented = "__FSharpTypes"

    if sampleRoot @@ "Sample2.pdb" |> File.Exists then
      Actions.Run (fun info ->
          { info with
                FileName = binRoot @@ "AltCover.exe"
                WorkingDirectory = sampleRoot
                Arguments = ("-s=Adapter -s=nunit -t=System\. -t=Microsoft\. -x=" + simpleReport + " /o=./" + instrumented)})
                "FSharpTypes"
      Actions.ValidateFSharpTypes simpleReport []
    else
      printfn "Symbols not present; skipping"
)

_Target "FSharpTypesDotNet" ( fun _ ->
    Directory.ensure "./_Reports"
    let altcover = Path.getFullName "./_Binaries/AltCover/Release+AnyCPU/netcoreapp2.0/AltCover.dll"
    let simpleReport = (Path.getFullName "./_Reports") @@ ( "AltCoverFSharpTypesDotNet.xml")
    let sampleRoot = Path.getFullName "Sample2/_Binaries/Sample2/Debug+AnyCPU/netcoreapp2.0"

    // Test the --inplace operation
    Shell.cleanDir sampleRoot
    Actions.RunDotnet (fun o -> {dotnetOptions o with WorkingDirectory = Path.getFullName "Sample2"}) "test"
                            ("-v m --configuration Debug sample2.core.fsproj")
                             "sample initial test returned with a non-zero exit code"

    // inplace instrument
    Actions.RunDotnet (fun o -> {dotnetOptions o with WorkingDirectory = sampleRoot}) ""
                             (altcover + " --inplace -s=Adapter -t \"System\\.\" -t \"Microsoft\\.\" -x \"" + simpleReport + "\" ")
                             "FSharpTypesDotNet"

    Actions.ValidateFSharpTypes simpleReport ["main"]
    Assert.That(Path.Combine (sampleRoot, "__Saved") |> Directory.Exists)

    printfn "Execute the instrumented tests"
    Actions.RunDotnet (fun o -> {dotnetOptions o with WorkingDirectory = Path.getFullName "Sample2"}) "test"
                            ("-v m --no-build --configuration Debug sample2.core.fsproj")
                             "sample coverage test returned with a non-zero exit code"
    Actions.ValidateFSharpTypesCoverage simpleReport
)

_Target "FSharpTests" ( fun _ ->
    Directory.ensure "./_Reports"
    let altcover = Path.getFullName "./_Binaries/AltCover/Release+AnyCPU/netcoreapp2.0/AltCover.dll"
    let simpleReport = (Path.getFullName "./_Reports") @@ ( "AltCoverFSharpTests.xml")
    let sampleRoot = Path.getFullName "Sample7/_Binaries/Sample7/Debug+AnyCPU/netcoreapp2.0"

    // Test the --inplace operation
    Shell.cleanDir sampleRoot
    Actions.RunDotnet (fun o -> {dotnetOptions o with WorkingDirectory = Path.getFullName "Sample7"}) "test"
                            ("-v m --configuration Debug sample7.core.fsproj")
                             "sample initial test returned with a non-zero exit code"

    // inplace instrument
    Actions.RunDotnet (fun o -> {dotnetOptions o with WorkingDirectory = sampleRoot}) ""
                             (altcover + " --opencover --inplace -c=[Test] -s=Adapter -t \"System\\.\" -t \"Microsoft\\.\" -x \"" + simpleReport + "\" ")
                             "FSharpTypesDotNet"

    printfn "Execute the instrumented tests"
    Actions.RunDotnet (fun o -> {dotnetOptions o with WorkingDirectory = Path.getFullName "Sample7"}) "test"
                            ("-v m --no-build --configuration Debug sample7.core.fsproj")
                             "sample coverage test returned with a non-zero exit code"
)

_Target "FSharpTypesDotNetRunner" ( fun _ ->
    Directory.ensure "./_Reports"
    let altcover = Path.getFullName "./_Binaries/AltCover/Release+AnyCPU/netcoreapp2.0/AltCover.dll"
    let simpleReport = (Path.getFullName "./_Reports") @@ ( "AltCoverFSharpTypesDotNetRunner.xml")
    let sampleRoot = Path.getFullName "_Binaries/Sample2/Debug+AnyCPU/netcoreapp2.0"
    let instrumented = Path.getFullName "Sample2/_Binaries/Sample2/Debug+AnyCPU/netcoreapp2.0"

    // Instrument the code
    Actions.RunDotnet (fun o -> {dotnetOptions o with WorkingDirectory = sampleRoot}) ""
                             (altcover + " -s=Adapter -t \"System\\.\" -t \"Microsoft\\.\" -x \"" + simpleReport + "\" /o \"" + instrumented + "\"")
                             "FSharpTypesDotNetRunner"

    Actions.ValidateFSharpTypes simpleReport ["main"]

    printfn "Execute the instrumented tests"
    let sample2 = Path.getFullName "./Sample2/sample2.core.fsproj"

    Actions.RunDotnet (fun o -> {dotnetOptions o with WorkingDirectory = instrumented}) ""
                            (altcover +
                             " Runner -x \"dotnet\" -r \"" + instrumented +
                             "\" -- test --no-build --configuration Debug " +
                             sample2)
                             "Execute the instrumented tests"

    Actions.ValidateFSharpTypesCoverage simpleReport
)

_Target "FSharpTypesDotNetCollecter" ( fun _ ->
    Directory.ensure "./_Reports"
    let altcover = Path.getFullName "./_Binaries/AltCover/Release+AnyCPU/netcoreapp2.0/AltCover.dll"
    let simpleReport = (Path.getFullName "./_Reports") @@ ( "AltCoverFSharpTypesDotNetCollecter.xml")
    let sampleRoot = Path.getFullName "Sample2/_Binaries/Sample2/Debug+AnyCPU/netcoreapp2.0"

    // Test the --inplace operation
    Shell.cleanDir sampleRoot
    Actions.RunDotnet (fun o -> {dotnetOptions o with WorkingDirectory = Path.getFullName "Sample2"}) "test"
                            ("--configuration Debug sample2.core.fsproj")
                             "sample initial test returned with a non-zero exit code"

    // inplace instrument and save
    Actions.RunDotnet (fun o -> {dotnetOptions o with WorkingDirectory = sampleRoot}) ""
                             (altcover + " --inplace --save -s=Adapter -t \"System\\.\" -t \"Microsoft\\.\" -x \"" + simpleReport + "\" ")
                             "FSharpTypesDotNetCollecter"

    Actions.ValidateFSharpTypes simpleReport ["main"]
    Assert.That(Path.Combine (sampleRoot, "__Saved") |> Directory.Exists)

    printfn "Execute the instrumented tests"
    Actions.RunDotnet (fun o -> {dotnetOptions o with WorkingDirectory = Path.getFullName "Sample2"}) "test"
                            ("-v m --no-build --configuration Debug sample2.core.fsproj")
                             "sample coverage test returned with a non-zero exit code"

    Actions.RunDotnet (fun o -> {dotnetOptions o with WorkingDirectory = sampleRoot}) ""
                            (altcover + " Runner --collect -r \"" + sampleRoot + "\"")
                             "Collect the instrumented test output"

    Actions.ValidateFSharpTypesCoverage simpleReport
)

_Target "BasicCSharp" (fun _ ->
   Actions.SimpleInstrumentingRun "_Binaries/Sample1/Debug+AnyCPU" "_Binaries/AltCover/Debug+AnyCPU" "BasicCSharp"
)

_Target "BasicCSharpMono" (fun _ ->
    Actions.SimpleInstrumentingRun "_Mono/Sample1" "_Binaries/AltCover/Debug+AnyCPU" "BasicCSharpMono"
)

_Target "BasicCSharpUnderMono" (fun _ ->
    monoOnWindows |>
    Actions.SimpleInstrumentingRunUnderMono "_Binaries/Sample1/Debug+AnyCPU" "_Binaries/AltCover/Debug+AnyCPU" "BasicCSharp"
)

_Target "BasicCSharpMonoUnderMono" (fun _ ->
    monoOnWindows |>
    Actions.SimpleInstrumentingRunUnderMono "_Mono/Sample1" "_Binaries/AltCover/Debug+AnyCPU" "BasicCSharpMono"
)

_Target "CSharpMonoWithDotNet" (fun _ ->
    Directory.ensure "./_Reports"
    let x = Path.getFullName "./_Reports/CSharpMonoWithDotNet.xml"
    let o = Path.getFullName "./_Mono/__Instrumented.CSharpMonoWithDotNet"
    let i = Path.getFullName "./_Mono/Sample1"
    let altcover = Path.getFullName "./_Binaries/AltCover/Release+AnyCPU/netcoreapp2.0/AltCover.dll"
    Actions.RunDotnet dotnetOptions ""
                      (altcover + " -t \"System.\" -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")
                       "CSharpMonoWithDotNet"

    Actions.Run (fun info ->
        { info with
                FileName = o @@ "/Sample1.exe"
                WorkingDirectory = o
                Arguments = ""}) "Instrumented .exe failed"

    Actions.ValidateSample1 "./_Reports/CSharpMonoWithDotNet.xml" "CSharpMonoWithDotNet"
)

_Target "CSharpDotNetWithDotNet" (fun _ ->
    Directory.ensure "./_Reports"
    let x = Path.getFullName "./_Reports/CSharpDotNetWithDotNet.xml"
    let o = Path.getFullName "./_Binaries/Sample1/__Instrumented.CSharpDotNetWithDotNet"
    let i = Path.getFullName "./_Binaries/Sample1/Debug+AnyCPU/netcoreapp2.0"
    Actions.RunDotnet dotnetOptions
                       "_Binaries/AltCover/Debug+AnyCPU/netcoreapp2.0/AltCover.dll" (" -t \"System.\" -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")
                        "CSharpDotNetWithDotNet"

    Actions.RunDotnet dotnetOptions
                        (o @@ "Sample1.dll") ""
                        "CSharpDotNetWithDotNet test"

    Actions.ValidateSample1 "./_Reports/CSharpDotNetWithDotNet.xml" "CSharpDotNetWithDotNet"
)

_Target "CSharpDotNetWithFramework" (fun _ ->
    Directory.ensure "./_Reports"
    let simpleReport = (Path.getFullName "./_Reports") @@ ( "CSharpDotNetWithFramework.xml")
    let binRoot = Path.getFullName "_Binaries/AltCover/Release+AnyCPU"
    let sampleRoot = Path.getFullName "_Binaries/Sample1/Debug+AnyCPU/netcoreapp2.0"
    let instrumented = Path.getFullName "_Binaries/Sample1/__Instrumented.CSharpDotNetWithFramework"
    Actions.Run (fun info ->
        { info with
            FileName = binRoot @@ "AltCover.exe"
            WorkingDirectory = sampleRoot
            Arguments = ("-t=System\. -t=Microsoft\. -x=" + simpleReport + " /o=" + instrumented)})
            "CSharpDotNetWithFramework"

    Actions.RunDotnet dotnetOptions
                       (instrumented @@ "Sample1.dll") ""
                       "CSharpDotNetWithFramework test"

    Actions.ValidateSample1 "./_Reports/CSharpDotNetWithFramework.xml" "CSharpDotNetWithFramework"
)

_Target "SelfTest" (fun _ ->
    Directory.ensure "./_Reports/_Instrumented"
    let targetDir = "_Binaries/AltCover.Tests/Debug+AnyCPU"
    let reports = Path.getFullName "./_Reports"
    let report = reports @@ "OpenCoverSelfTest.xml"
    let altReport = reports @@ "AltCoverSelfTest.xml"
    let keyfile = Path.getFullName "Build/SelfTest.snk"

    printfn "Self-instrument under OpenCover"
    OpenCover.run (fun p -> { p with
                                 WorkingDir = targetDir
                                 ExePath = Tools.findToolInSubPath "OpenCover.Console.exe" "."
                                 TestRunnerExePath = Tools.findToolInSubPath "AltCover.exe" targetDir
                                 Filter = OpenCoverFilter
                                 MergeByHash = true
                                 OptionalArguments = "-excludebyattribute:*ExcludeFromCodeCoverageAttribute;*ProgIdAttribute"
                                 Register = OpenCover.RegisterType.RegisterUser
                                 Output = report })
        ("/sn=" + keyfile + AltCoverFilter + "-x=" + altReport + " -o __SelfTest")

    ReportGenerator.generateReports
                       (fun p -> { p with ExePath = Tools.findToolInSubPath "ReportGenerator.exe" "."
                                          TargetDir = "_Reports/_OpenCoverSelfTest"})
        [report]

    printfn "Re-instrument everything"
    let altReport2 = reports @@ "AltCoverSelfTestDummy.xml"
    Actions.Run (fun info ->
        { info with
            FileName = "_Binaries/AltCover.Tests/Debug+AnyCPU/__SelfTest/AltCover.exe"
            WorkingDirectory = "_Binaries/AltCover.Tests/Debug+AnyCPU"
            Arguments = ("/sn=" + keyfile + AltCoverFilter + @"/o=./__SelfTestDummy -x=" + altReport2)})
            "Re-instrument returned with a non-zero exit code"

    ReportGenerator.generateReports
                       (fun p -> { p with ExePath = Tools.findToolInSubPath "ReportGenerator.exe" "."
                                          TargetDir = "_Reports/_AltCoverSelfTest"})
        [altReport]
)

_Target "RecordResumeTest" ( fun _ ->
    Directory.ensure "./_Reports"
    let simpleReport = (Path.getFullName "./_Reports") @@ ( "RecordResumeTest.xml")
    let binRoot = Path.getFullName "_Binaries/AltCover/Release+AnyCPU"
    let sampleRoot = Path.getFullName "_Binaries/Sample8/Debug+AnyCPU"
    let instrumented = "__RecordResumeTest"

    Actions.Run (fun info ->
          { info with
                FileName = binRoot @@ "AltCover.exe"
                WorkingDirectory = sampleRoot
                Arguments = ("-s=Adapter -s=nunit -t=System\. -t=Microsoft\. -x=" + simpleReport + " /o=./" + instrumented)})
                "RecordResumeTest 1"

    let testing = (sampleRoot @@ instrumented) @@ "Sample8.exe"
    Actions.Run (fun info ->
          { info with
                FileName = testing
                WorkingDirectory = sampleRoot
                Arguments = simpleReport + ".acv"})
                "RecordResumeTest 2"
    do
      use coverageFile = new FileStream(simpleReport, FileMode.Open, FileAccess.Read, FileShare.None, 4096, FileOptions.SequentialScan)
      let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))
      let recorded = coverageDocument.Descendants(XName.Get("seqpnt"))
                     |> Seq.map (fun x -> x.Attribute(XName.Get("visitcount")).Value)
                     |> Seq.toList
      let expected = Array.create 20 "0"
      Assert.That(recorded, expected |> Is.EquivalentTo, sprintf "Bad visit list %A -- should be empty now" recorded)

    Actions.Run (fun info ->
          { info with
                FileName = binRoot @@ "AltCover.exe"
                WorkingDirectory = sampleRoot
                Arguments = ("runner --collect /r=./" + instrumented)})
                "RecordResumeTest 3"

    do
      use coverageFile = new FileStream(simpleReport, FileMode.Open, FileAccess.Read, FileShare.None, 4096, FileOptions.SequentialScan)
      let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))
      let recorded = coverageDocument.Descendants(XName.Get("seqpnt"))
                     |> Seq.map (fun x -> x.Attribute(XName.Get("visitcount")).Value)
                     |> Seq.toList
      let expected = Array.create 20 "0"
      Assert.That(recorded, expected |> Is.Not.EquivalentTo, sprintf "Bad visit list %A -- should no longer be empty now" recorded)
      Assert.That(recorded |> Seq.length,  Is.EqualTo 20, sprintf "Bad visit list %A -- should no longer be empty now" recorded)
)

_Target "RecordResumeTrackingTest" ( fun _ ->
    Directory.ensure "./_Reports"
    let simpleReport = (Path.getFullName "./_Reports") @@ ( "RecordResumeTrackingTest.xml")
    let binRoot = Path.getFullName "_Binaries/AltCover/Release+AnyCPU"
    let sampleRoot = Path.getFullName "_Binaries/Sample8/Debug+AnyCPU"
    let instrumented = "__RecordResumeTrackingTest"

    Actions.Run (fun info ->
          { info with
                FileName = binRoot @@ "AltCover.exe"
                WorkingDirectory = sampleRoot
                Arguments = ("--opencover -c=Main -s=Adapter -s=nunit -t=System\. -t=Microsoft\. -x=" + simpleReport + " /o=./" + instrumented)})
                "RecordResumeTrackingTest 1"

    let testing = (sampleRoot @@ instrumented) @@ "Sample8.exe"
    Actions.Run (fun info ->
          { info with
                FileName = testing
                WorkingDirectory = sampleRoot
                Arguments = simpleReport + ".acv"})
                "RecordResumeTrackingTest 2"
    do
      use coverageFile = new FileStream(simpleReport, FileMode.Open, FileAccess.Read, FileShare.None, 4096, FileOptions.SequentialScan)
      let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))
      let recorded = coverageDocument.Descendants(XName.Get("SequencePoint"))
                     |> Seq.map (fun x -> x.Attribute(XName.Get("vc")).Value)
                     |> Seq.toList
      let expected = Array.create 20 "0"
      Assert.That(recorded, expected |> Is.EquivalentTo, sprintf "Bad visit list %A -- should be empty now" recorded)

    Actions.Run (fun info ->
          { info with
                FileName = binRoot @@ "AltCover.exe"
                WorkingDirectory = sampleRoot
                Arguments = ("runner --collect /r=./" + instrumented)})
                "RecordResumeTrackingTest 3"

    do
      use coverageFile = new FileStream(simpleReport, FileMode.Open, FileAccess.Read, FileShare.None, 4096, FileOptions.SequentialScan)
      let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))
      let recorded = coverageDocument.Descendants(XName.Get("SequencePoint"))
                     |> Seq.map (fun x -> x.Attribute(XName.Get("vc")).Value)
                     |> Seq.toList
      let expected = Array.create 20 "0"
      Assert.That(recorded, expected |> Is.Not.EquivalentTo, sprintf "Bad visit list %A -- should no longer be empty now" recorded)
      Assert.That(recorded |> Seq.length,  Is.EqualTo 20, sprintf "Bad visit list %A -- should no longer be empty now" recorded)
      let tracked = coverageDocument.Descendants(XName.Get("TrackedMethodRef"))
                     |> Seq.toList
      Assert.That (tracked, Is.Not.Empty)

)

_Target "RecordResumeTestDotNet" ( fun _ ->
    Directory.ensure "./_Reports"
    let simpleReport = (Path.getFullName "./_Reports") @@ ( "RecordResumeTestDotNet.xml")
    let binRoot = Path.getFullName "_Binaries/AltCover/Release+AnyCPU"
    let sampleRoot = Path.getFullName "_Binaries/Sample8/Debug+AnyCPU/netcoreapp2.0"
    let instrumented = "__RecordResumeTestDotNet"

    Actions.Run (fun info ->
          { info with
                FileName = binRoot @@ "AltCover.exe"
                WorkingDirectory = sampleRoot
                Arguments = ("-s=Adapter -s=nunit -t=System\. -t=Microsoft\. -x=" + simpleReport + " /o=./" + instrumented)})
                "RecordResumeTestDotNet 1"

    let testing = (sampleRoot @@ instrumented) @@ "Sample8.dll"
    Actions.RunDotnet (fun o -> {dotnetOptions o with WorkingDirectory = sampleRoot}) (testing + " " + simpleReport + ".acv") ""
                "RecordResumeTestDotNet 2"
    do
      use coverageFile = new FileStream(simpleReport, FileMode.Open, FileAccess.Read, FileShare.None, 4096, FileOptions.SequentialScan)
      let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))
      let recorded = coverageDocument.Descendants(XName.Get("seqpnt"))
                     |> Seq.map (fun x -> x.Attribute(XName.Get("visitcount")).Value)
                     |> Seq.toList
      let expected = Array.create 20 "0"
      Assert.That(recorded, expected |> Is.EquivalentTo, sprintf "Bad visit list %A -- should be empty now" recorded)

    Actions.Run (fun info ->
          { info with
                FileName = binRoot @@ "AltCover.exe"
                WorkingDirectory = sampleRoot
                Arguments = ("runner --collect /r=./" + instrumented)})
                "RecordResumeTestDotNet 3"

    do
      use coverageFile = new FileStream(simpleReport, FileMode.Open, FileAccess.Read, FileShare.None, 4096, FileOptions.SequentialScan)
      let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))
      let recorded = coverageDocument.Descendants(XName.Get("seqpnt"))
                     |> Seq.map (fun x -> x.Attribute(XName.Get("visitcount")).Value)
                     |> Seq.toList
      let expected = Array.create 20 "0"
      Assert.That(recorded, expected |> Is.Not.EquivalentTo, sprintf "Bad visit list %A -- should no longer be empty now" recorded)
      Assert.That(recorded |> Seq.length,  Is.EqualTo 20, sprintf "Bad visit list %A -- should no longer be empty now" recorded)
)

_Target "RecordResumeTestUnderMono" ( fun _ ->
    Directory.ensure "./_Reports"
    let simpleReport = (Path.getFullName "./_Reports") @@ ( "RecordResumeTestUnderMono.xml")
    let binRoot = Path.getFullName "_Binaries/AltCover/Release+AnyCPU"
    let sampleRoot = Path.getFullName "_Binaries/Sample8/Debug+AnyCPU"
    let instrumented = "__RecordResumeTestUnderMono"

    Actions.Run (fun info ->
          { info with
                FileName = binRoot @@ "AltCover.exe"
                WorkingDirectory = sampleRoot
                Arguments = ("-s=Adapter -s=nunit -t=System\. -t=Microsoft\. -x=" + simpleReport + " /o=./" + instrumented)})
                "RecordResumeTestUnderMono 1"

    match monoOnWindows with
    | Some mono ->
      let RunIt (f:Fake.Core.ProcStartInfo -> Fake.Core.ProcStartInfo) (msg:string) =
           let x = Fake.Core.Process.execSimple (f >> Fake.Core.Process.withFramework) (TimeSpan.FromMinutes 15.0)
           Assert.That(x, Is.EqualTo 0, msg)

      let testing = (sampleRoot @@ instrumented) @@ "Sample8.exe"
      RunIt (fun info ->
          { info with
                FileName = mono
                WorkingDirectory = sampleRoot
                Arguments = testing + " " + simpleReport + ".acv"})
                "RecordResumeTestUnderMono 2"
    | None -> ()

    do
      use coverageFile = new FileStream(simpleReport, FileMode.Open, FileAccess.Read, FileShare.None, 4096, FileOptions.SequentialScan)
      let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))
      let recorded = coverageDocument.Descendants(XName.Get("seqpnt"))
                     |> Seq.map (fun x -> x.Attribute(XName.Get("visitcount")).Value)
                     |> Seq.toList
      let expected = Array.create 20 "0"
      Assert.That(recorded, expected |> Is.EquivalentTo, sprintf "Bad visit list %A -- should be empty now" recorded)

    Actions.Run (fun info ->
          { info with
                FileName = binRoot @@ "AltCover.exe"
                WorkingDirectory = sampleRoot
                Arguments = ("runner --collect /r=./" + instrumented)})
                "RecordResumeTestUnderMono 3"

    do
      use coverageFile = new FileStream(simpleReport, FileMode.Open, FileAccess.Read, FileShare.None, 4096, FileOptions.SequentialScan)
      let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))
      let recorded = coverageDocument.Descendants(XName.Get("seqpnt"))
                     |> Seq.map (fun x -> x.Attribute(XName.Get("visitcount")).Value)
                     |> Seq.toList
      let expected = Array.create 20 "0"
      Assert.That(recorded, expected |> Is.Not.EquivalentTo, sprintf "Bad visit list %A -- should no longer be empty now" recorded)
      Assert.That(recorded |> Seq.length,  Is.EqualTo 20, sprintf "Bad visit list %A -- should no longer be empty now" recorded)
)

// Packaging

_Target "Packaging" (fun _ ->
    let AltCover = Path.getFullName "_Binaries/AltCover/AltCover.exe"
    let fscore = Path.getFullName "_Binaries/AltCover/Release+AnyCPU/FSharp.Core.dll"
    let options = Path.getFullName "_Binaries/AltCover/Release+AnyCPU/Mono.Options.dll"
    let recorder = Path.getFullName "_Binaries/AltCover/Release+AnyCPU/AltCover.Recorder.dll"
    let posh = Path.getFullName "_Binaries/AltCover.PowerShell/Release+AnyCPU/AltCover.PowerShell.dll"
    let csapi = Path.getFullName "_Binaries/AltCover.CSApi/Release+AnyCPU/AltCover.CSApi.dll"
    let cake = Path.getFullName "_Binaries/AltCover.Cake/Release+AnyCPU/AltCover.Cake.dll"
    let vis = Path.getFullName "_Binaries/AltCover.Visualizer/Release+AnyCPU/AltCover.Visualizer.exe"
    let packable = Path.getFullName "./_Binaries/README.html"
    let resources = DirectoryInfo.getMatchingFilesRecursive "AltCover.resources.dll" (DirectoryInfo.ofPath (Path.getFullName "_Binaries/AltCover/Release+AnyCPU"))
    let resources2 = DirectoryInfo.getMatchingFilesRecursive "AltCover.Visualizer.resources.dll" (DirectoryInfo.ofPath (Path.getFullName "_Binaries/AltCover.Visualizer/Release+AnyCPU"))

    let applicationFiles = if File.Exists AltCover then
                            [
                                (AltCover, Some "tools/net45", None)
                                (recorder, Some "tools/net45", None)
                                (posh, Some "tools/net45", None)
                                (vis, Some "tools/net45", None)
                                (fscore, Some "tools/net45", None)
                                (options, Some "tools/net45", None)
                                (packable, Some "", None)
                            ]
                           else []

    let apiFiles = if File.Exists AltCover then
                            [
                                (AltCover, Some "lib/net45", None)
                                (recorder, Some "lib/net45", None)
                                (posh, Some "lib/net45", None)
                                (csapi, Some "lib/net45", None)
                                (cake, Some "lib/net45", None)
                                (fscore, Some "lib/net45", None)
                                (options, Some "lib/net45", None)
                                (packable, Some "", None)
                            ]
                           else []

    let resourceFiles path = if File.Exists AltCover then
                                  Seq.concat [resources; resources2]
                                  |> Seq.map (fun x -> x.FullName)
                                  |> Seq.map (fun x -> (x, Some (path + Path.GetFileName(Path.GetDirectoryName(x))), None))
                                  |> Seq.toList
                              else []

    let nupkg = (Path.getFullName "./nupkg").Length
    let otherFiles = (!! "./nupkg/**/*.*")
                       |> Seq.map (fun x -> (x, Some (Path.GetDirectoryName(x).Substring(nupkg).Replace("\\","/")), None))
                       |> Seq.toList

    Directory.ensure "./_Intermediate/dotnet"
    let otherFilesDotnet = otherFiles
                           |> List.map (fun (a,b,c) -> let text = File.ReadAllText(a).Replace("tools/netcoreapp2.0", "lib/netcoreapp2.0")
                                                       let name = (Path.getFullName"./_Intermediate/dotnet") @@ ("altcover.dotnet" + Path.GetExtension a)
                                                       File.WriteAllText(name, text)
                                                       (name,b,c))

    Directory.ensure "./_Intermediate/global"
    let otherFilesGlobal = otherFiles
                           |> List.map (fun (a,b,c) -> let text = File.ReadAllText(a).Replace("tools/netcoreapp2.0", "tools/netcoreapp2.1/any")
                                                       let name = (Path.getFullName"./_Intermediate/global") @@ ("altcover.global" + Path.GetExtension a)
                                                       File.WriteAllText(name, text)
                                                       (name,b,c))

    Directory.ensure "./_Intermediate/api"
    let otherFilesApi = otherFiles
                           |> List.map (fun (a,b,c) -> let text = File.ReadAllText(a).Replace("tools/netcoreapp2.0", "lib/netstandard2.0")
                                                       let name = (Path.getFullName"./_Intermediate/api") @@ ("altcover.api" + Path.GetExtension a)
                                                       File.WriteAllText(name, text)
                                                       (name,b,c))

    let poshFiles where = (!! "./_Binaries/AltCover.PowerShell/Release+AnyCPU/netcoreapp2.0/*.PowerShell.*")
                           |> Seq.map (fun x -> (x, Some (where + Path.GetFileName x), None))
                           |> Seq.toList

    let cakeFiles where = (!! "./_Binaries/AltCover.Cake/Release+AnyCPU/netstandard2.0/AltCover.C*.*")
                           |> Seq.map (fun x -> (x, Some (where + Path.GetFileName x), None))
                           |> Seq.toList
    let fakeFiles where = (!! "./_Binaries/AltCover.Fake/Release+AnyCPU/netstandard2.0/AltCover.F*.*")
                           |> Seq.map (fun x -> (x, Some (where + Path.GetFileName x), None))
                           |> Seq.toList

    let publish = (Path.getFullName "./_Publish").Length
    let netcoreFiles where = (!! "./_Publish/**/*.*")
                               |> Seq.map (fun x -> (x, Some (where + Path.GetDirectoryName(x).Substring(publish).Replace("\\","/")), None))
                               |> Seq.toList

    let publishapi = (Path.getFullName "./_Publish.api").Length
    let netstdFiles where = (!! "./_Publish.api/**/*.*")
                               |> Seq.filter (fun f -> let n = f |> Path.GetFileName
                                                       n.StartsWith("altcover.", StringComparison.OrdinalIgnoreCase) ||
                                                       n.StartsWith("Mono.", StringComparison.Ordinal) ||
                                                       n.StartsWith("FSharp.Core.", StringComparison.Ordinal)
                               )
                               |> Seq.map (fun x -> (x, Some (where + Path.GetDirectoryName(x).Substring(publishapi).Replace("\\","/")), None))
                               |> Seq.toList

    let dotnetFiles = (!! "./_Binaries/dotnet-altcover/Release+AnyCPU/netcoreapp2.0/dotnet-altcover.*")
                       |> Seq.map (fun x -> (x, Some ("lib/netcoreapp2.0/" + Path.GetFileName x), None))
                       |> Seq.toList

    let globalFiles = (!! "./_Binaries/global-altcover/Release+AnyCPU/netcoreapp2.1/global-altcover.*")
                       |> Seq.map (fun x -> (x, Some ("tools/netcoreapp2.1/any/" + Path.GetFileName x), None))
                       |> Seq.toList

    let auxFiles = (!! "./_Binaries/global-altcover/Release+AnyCPU/netcoreapp2.1/*.xml")
                       |> Seq.map (fun x -> (x, Some ("tools/netcoreapp2.1/any/" + Path.GetFileName x), None))
                       |> Seq.toList

    printfn "Executing on %A" Environment.OSVersion

    [
        (List.concat [applicationFiles
                      resourceFiles "tools/net45/"
                      netcoreFiles "tools/netcoreapp2.0/"
                      poshFiles "tools/netcoreapp2.0/"
                      otherFiles],
         "_Packaging",
         "./Build/AltCover.nuspec",
         "altcover"
        )

        (List.concat [apiFiles
                      resourceFiles "lib/net45/"
                      netstdFiles "lib/netstandard2.0/"
                      cakeFiles "lib/netstandard2.0/"
                      fakeFiles "lib/netstandard2.0/"
                      poshFiles "lib/netstandard2.0/"
                      otherFilesApi
                      ],
         "_Packaging.api",
         "./_Generated/altcover.api.nuspec",
         "altcover.api"
        )

        (List.concat[netcoreFiles "lib/netcoreapp2.0/"
                     poshFiles "lib/netcoreapp2.0/"
                     dotnetFiles
                     otherFilesDotnet],
         "_Packaging.dotnet",
         "./_Generated/altcover.dotnet.nuspec",
         "altcover.dotnet"
        )
        (List.concat [globalFiles
                      netcoreFiles "tools/netcoreapp2.1/any/"
                      poshFiles "tools/netcoreapp2.1/any/"
                      auxFiles
                      otherFilesGlobal],
         "_Packaging.global",
         "./_Generated/altcover.global.nuspec",
         "altcover.global"
        )
    ]
    |> List.iter (fun (files, output, nuspec, project) ->
    let outputPath = "./" + output
    let workingDir = "./_Binaries/" + output
    Directory.ensure workingDir
    Directory.ensure outputPath

    NuGet (fun p ->
    {p with
        Authors = ["Steve Gilham"]
        Project = project
        Description = "A cross-platform pre-instrumenting code coverage tool set for .net/.net core and Mono"
        OutputPath = outputPath
        WorkingDir = workingDir
        Files = files
        Version = !Version
        Copyright = (!Copyright).Replace("©", "(c)")
        Publish = false
        ReleaseNotes = Path.getFullName "ReleaseNotes.md"
                       |> File.ReadAllText
        ToolPath = if Environment.isWindows then p.ToolPath else "/usr/bin/nuget"
        })
        nuspec)
)

_Target "PrepareFrameworkBuild" (fun _ ->
    let toolpath = Tools.findToolInSubPath "ILMerge.exe" "./packages"
    let ver = String.Join(".", (!Version).Split('.') |> Seq.take 2) + ".0.0"

    Actions.Run (fun info ->
        { info with
               FileName = toolpath
               Arguments = "/out:\"./_Binaries/AltCover/AltCover.exe\" /ver:\"" + ver +
                           "\" /attr:\"./_Binaries/AltCover/Release+AnyCPU/AltCover.exe\" /keyfile:\"./Build/Infrastructure.snk\" /target:\"exe\" /internalize ./_Binaries/AltCover/Release+AnyCPU/AltCover.exe .\_Binaries\AltCover\Release+AnyCPU\Mono.Cecil.dll .\_Binaries\AltCover\Release+AnyCPU\Mono.Cecil.Mdb.dll .\_Binaries\AltCover\Release+AnyCPU\Mono.Cecil.Pdb.dll .\_Binaries\AltCover\Release+AnyCPU\Mono.Cecil.Rocks.dll .\_Binaries\AltCover\Release+AnyCPU\Newtonsoft.Json.dll"
                }) "ILMerge failure"

//    let here = Directory.GetCurrentDirectory()
//    ILMerge (fun p -> { p with DebugInfo = true
//                               ToolPath = toolpath
//                               TargetKind = TargetKind.Exe
//                               KeyFile = "./Build/Infrastructure.snk"
//                               Version = (String.Join(".", (!Version).Split('.') |> Seq.take 2) + ".0.0")
//                               Internalize = InternalizeTypes.Internalize
//                               Libraries = Seq.concat [!! "./_Binaries/AltCover/Release+AnyCPU/Mono.C*.dll"; !! "./_Binaries/AltCover/Release+AnyCPU/Newton*.dll"]
//                                           |> Seq.map (fun f -> f.Replace(here, "."))
//                               AttributeFile = "./_Binaries/AltCover/Release+AnyCPU/AltCover.exe"})
//                               "./_Binaries/AltCover/AltCover.exe"
//                               "./_Binaries/AltCover/Release+AnyCPU/AltCover.exe"
)

_Target "PrepareDotNetBuild" (fun _ ->
    let netcoresource =  Path.getFullName "./AltCover/altcover.core.fsproj" //  "./altcover.dotnet.sln"
    let publish = Path.getFullName "./_Publish"
    DotNet.publish (fun options -> { options with OutputPath = Some publish
                                                  Configuration = DotNet.BuildConfiguration.Release
                                                  Framework = Some "netcoreapp2.0"})
                                                  netcoresource
    DotNet.publish (fun options -> { options with OutputPath = Some (publish + ".api")
                                                  Configuration = DotNet.BuildConfiguration.Release
                                                  Framework = Some "netstandard2.0"})
                                                  netcoresource

    // dotnet tooling mods
    [
        ("DotnetCliTool", "./_Generated/altcover.dotnet.nuspec", "AltCover (dotnet CLI tool install)")
        ("DotnetTool", "./_Generated/altcover.global.nuspec", "AltCover (dotnet global tool install)")
        (String.Empty, "./_Generated/altcover.api.nuspec", "AltCover (API install)")
    ]
    |> List.iter (fun (ptype, path, caption) ->
        let x s = XName.Get(s, "http://schemas.microsoft.com/packaging/2010/07/nuspec.xsd")
        let dotnetNupkg = XDocument.Load "./Build/AltCover.nuspec"
        let title = dotnetNupkg.Descendants(x "title") |> Seq.head
        title.ReplaceNodes caption
        if ptype |> String.IsNullOrWhiteSpace |> not then
            let tag = dotnetNupkg.Descendants(x "tags") |> Seq.head
            let insert = XElement(x "packageTypes")
            insert.Add(XElement(x "packageType",
                                XAttribute (XName.Get "name", ptype)))
            tag.AddAfterSelf insert
        dotnetNupkg.Save path)
)

_Target "PrepareReadMe" (fun _ ->
    Actions.PrepareReadMe ((!Copyright).Replace("©", "&#xa9;").Replace("<","&lt;").Replace(">", "&gt;"))
)

// Post-packaging deployment touch test

_Target "Deployment" ignore

_Target "Unpack" (fun _ ->
  let nugget = !! "./_Packaging/*.nupkg" |> Seq.last
  let unpack = Path.getFullName "_Packaging/Unpack"
  System.IO.Compression.ZipFile.ExtractToDirectory (nugget, unpack)
)

_Target "WindowsPowerShell" (fun _ ->
  let v = (!Version).Split([| '-' |]).[0]
  Actions.RunRaw (fun info -> { info with
                                        FileName = "powershell.exe"
                                        WorkingDirectory = "."
                                        Arguments = ("-NoProfile ./Build/powershell.ps1 -ACV " + v)})
                                 "powershell"
)

_Target "Pester" (fun _ ->
  Directory.ensure "./_Reports"
  let nugget = !! "./_Packaging/*.nupkg" |> Seq.last
  let ``module`` = Path.getFullName "_Packaging/Module"
  System.IO.Compression.ZipFile.ExtractToDirectory (nugget, ``module``)
  let unpack = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0"
  let report = Path.getFullName "_Reports/Pester.xml"
  let i = ``module`` @@ "tools/netcoreapp2.0"
  let v = (!Version).Split([| '-' |]).[0]

  Actions.RunDotnet (fun o -> {dotnetOptions o with WorkingDirectory = unpack}) ""
                      ("AltCover.dll --inplace --save --opencover -t=System\. \"-s=^((?!AltCover\.PowerShell).)*$\" -x \"" + report + "\" -i \"" + i + "\"")
                             "Pester instrument"

  printfn "Execute the instrumented tests"

  Actions.RunRaw (fun info -> { info with
                                        FileName = pwsh
                                        WorkingDirectory = "."
                                        Arguments = ("-NoProfile ./Build/pester.ps1 -ACV " + v)})
                                 "pwsh"

  Actions.RunDotnet (fun o -> {dotnetOptions o with WorkingDirectory = unpack}) ""
                            ("AltCover.dll Runner --collect -r \"" + i + "\"")
                             "Collect the output"

  ReportGenerator.generateReports
              (fun p -> { p with ExePath = Tools.findToolInSubPath "ReportGenerator.exe" "."
                                 ReportTypes = [ ReportGenerator.ReportType.Html; ReportGenerator.ReportType.XmlSummary ]
                                 TargetDir = "_Reports/_Pester"})
              [ report ]

  "_Reports/_Pester/Summary.xml"
  |> File.ReadAllText
  |> printfn "%s"
)

_Target "SimpleReleaseTest" (fun _ ->
    let unpack = Path.getFullName "_Packaging/Unpack/tools/net45"
    if (unpack @@ "AltCover.exe") |> File.Exists then
      Actions.SimpleInstrumentingRun "_Binaries/Sample1/Debug+AnyCPU" unpack "SimpleReleaseTest"
    else
      let file = Directory.GetFiles("./packages", "AltCover.exe", SearchOption.AllDirectories)
                 |> Seq.tryFind (fun _ -> true)
      match file with
      | Some test ->
            Actions.SimpleInstrumentingRun "_Binaries/Sample1/Debug+AnyCPU" (Path.GetDirectoryName test) "SimpleReleaseTest"
      | None -> printfn "Skipping -- AltCover.exe not packaged"
)

_Target "SimpleMonoReleaseTest" (fun _ ->
    let unpack = Path.getFullName "_Packaging/Unpack/tools/net45"
    if (unpack @@ "AltCover.exe") |> File.Exists then
      Actions.SimpleInstrumentingRun "_Mono/Sample1" unpack "SimpleMonoReleaseTest"
    else
      let file = Directory.GetFiles("./packages", "AltCover.exe", SearchOption.AllDirectories)
                 |> Seq.tryFind (fun _ -> true)
      match file with
      | Some test ->
            Actions.SimpleInstrumentingRun "_Binaries/Sample1/Debug+AnyCPU" (Path.GetDirectoryName test) "SimpleReleaseTest"
      | None -> printfn "Skipping -- AltCover.exe not packaged"
)

_Target "ReleaseDotNetWithFramework" (fun _ ->
    Directory.ensure "./_Reports"
    let unpack0 = Path.getFullName "_Packaging/Unpack/tools/net45/AltCover.exe"
    let unpack1 = Directory.GetFiles(Path.getFullName "./packages", "AltCover.exe", SearchOption.AllDirectories)
                  |> Seq.tryFind (fun _ -> true)

    let unpack = if File.Exists unpack0 then Some unpack0 else unpack1

    if Option.isSome unpack then
      let simpleReport = (Path.getFullName "./_Reports") @@ ( "ReleaseDotNetWithFramework.xml")
      let sampleRoot = Path.getFullName "./_Binaries/Sample1/Debug+AnyCPU/netcoreapp2.0"
      let instrumented = sampleRoot @@ "__Instrumented.ReleaseDotNetWithFramework"
      Actions.Run (fun info ->
          { info with
                FileName = Option.get unpack
                WorkingDirectory = sampleRoot
                Arguments = ("-t=System\. -t=Microsoft\. -x=" + simpleReport + " /o=" + instrumented)})
                "ReleaseDotNetWithFramework"

      Actions.RunDotnet (fun o -> {dotnetOptions o with WorkingDirectory = instrumented}) "Sample1.dll" ""
                        "ReleaseDotNetWithFramework test"

      Actions.ValidateSample1 "./_Reports/ReleaseDotNetWithFramework.xml" "ReleaseDotNetWithFramework"
    else printfn "Skipping -- AltCover.exe not packaged"
)

_Target "ReleaseMonoWithDotNet" (fun _ ->
    Directory.ensure "./_Reports"
    let unpack = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0"
    let x = Path.getFullName "./_Reports/ReleaseMonoWithDotNet.xml"
    let o = Path.getFullName "./_Mono/__Instrumented.ReleaseMonoWithDotNet"
    let i = Path.getFullName "./_Mono/Sample1"
    Actions.RunDotnet (fun o -> {dotnetOptions o with WorkingDirectory = unpack}) ""
                      ("AltCover.dll -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")
                      "ReleaseMonoWithDotNet"

    Actions.Run (fun info ->
        { info with
            FileName = o @@ "Sample1.exe"
            WorkingDirectory = o
            Arguments = ""}) "Instrumented .exe failed"

    Actions.ValidateSample1 "./_Reports/ReleaseMonoWithDotNet.xml" "ReleaseMonoWithDotNet"
)

_Target "ReleaseDotNetWithDotNet" (fun _ ->
    Directory.ensure "./_Reports"
    let unpack = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0"
    let x = Path.getFullName "./_Reports/ReleaseDotNetWithDotNet.xml"
    let o = Path.getFullName "./_Binaries/Sample1/__Instrumented.ReleaseDotNetWithDotNet"
    let i = Path.getFullName "./_Binaries/Sample1/Debug+AnyCPU/netcoreapp2.0"
    Actions.RunDotnet (fun o -> {dotnetOptions o with WorkingDirectory = unpack}) "run"
                      ("AltCover.dll -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")
                      "ReleaseDotNetWithDotNet"

    Actions.RunDotnet dotnetOptions
                      (o @@ "Sample1.dll") ""
                      "ReleaseDotNetWithDotNet test"

    Actions.ValidateSample1 "./_Reports/ReleaseDotNetWithDotNet.xml" "ReleaseDotNetWithDotNet"
)

_Target "ReleaseFSharpTypesDotNetRunner" ( fun _ ->
    Directory.ensure "./_Reports"
    let unpack = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0"
    let x = Path.getFullName "./_Reports/AltCoverReleaseFSharpTypesDotNetRunner.xml"
    let o = Path.getFullName "Sample2/_Binaries/Sample2/Debug+AnyCPU/netcoreapp2.0"
    let i = Path.getFullName "_Binaries/Sample2/Debug+AnyCPU/netcoreapp2.0"

    Shell.cleanDir o

    // Instrument the code
    Actions.RunDotnet (fun o' -> {dotnetOptions o' with WorkingDirectory = unpack}) ""
                      ("AltCover.dll -s=Adapter -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")
                      "ReleaseFSharpTypesDotNetRunner"

    Actions.ValidateFSharpTypes x ["main"]

    printfn "Execute the instrumented tests"
    let sample2 = Path.getFullName "./Sample2/sample2.core.fsproj"
    let runner = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0/AltCover.dll"

    // Run
    Actions.RunDotnet (fun o' -> {dotnetOptions o' with WorkingDirectory = o}) ""
                          (runner +
                          " Runner -x \"dotnet\" -r \"" + o +
                          "\" -- test --no-build --configuration Debug " +
                          sample2)
                          "ReleaseFSharpTypesDotNetRunner test"

    Actions.ValidateFSharpTypesCoverage x
)

_Target "ReleaseFSharpTypesX86DotNetRunner" ( fun _ ->
    Directory.ensure "./_Reports"
    let unpack = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0"
    let s = Path.getFullName "."
    let x = Path.getFullName "./_Reports/AltCoverReleaseFSharpTypesX86DotNetRunner.xml"
    let o = Path.getFullName "Sample2/_Binaries/Sample2/Debug+x86/netcoreapp2.0"
    let i = Path.getFullName "_Binaries/Sample2/Debug+x86/netcoreapp2.0"

    Shell.cleanDir o
    try
      try
        Environment.SetEnvironmentVariable("platform", "x86")
        Actions.Run (fun info ->
            { info with
                FileName = dotnetPath86 |> Option.get
                Arguments = "--info"}) "dotnet-x86 failed"

        printfn "Build the sample2 code as x86"
        Actions.RunDotnet (fun o' -> {dotnetOptions o' with WorkingDirectory = s }) "build"
                      (" altcover.core.sln --configuration Debug")
                      "ReleaseFSharpTypesX86DotNetRunnerBuild"

        printfn "Instrument the code"
        let altcover = unpack @@ "AltCover.dll"
        Actions.RunDotnet (fun o' -> {dotnetOptions o' with WorkingDirectory = unpack
                                                            DotNetCliPath = dotnetPath86 |> Option.get}) ""
                      (altcover + " -s=Adapter -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")
                      "ReleaseFSharpTypesX86DotNetRunner"

        Actions.ValidateFSharpTypes x ["main"]

        printfn "Execute the instrumented tests"
        let sample2 = Path.getFullName "./Sample2/sample2.core.fsproj"

        // Run
        Actions.RunDotnet (fun o' -> {dotnetOptions o' with WorkingDirectory = o
                                                            DotNetCliPath = dotnetPath86 |> Option.get}) ""
                              (altcover + " Runner -x \"" + (dotnetPath86 |> Option.get) +
                              "\" -r \"" + o + "\" -- test --no-build --configuration Debug " + sample2)
                            "ReleaseFSharpTypesX86DotNetRunner test"

        Actions.ValidateFSharpTypesCoverage x
      with
      | x -> printfn "Failed with %A" x
             reraise()
    finally
        Environment.SetEnvironmentVariable("platform", "")
)

_Target "ReleaseXUnitFSharpTypesDotNet" ( fun _ ->
    Directory.ensure "./_Reports"
    let unpack = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0"
    let x = Path.getFullName "./_Reports/ReleaseXUnitFSharpTypesDotNet.xml"
    let o = Path.getFullName "Sample4/_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.0"
    let i = Path.getFullName "_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.0"

    Shell.cleanDir o

    // Instrument the code
    Actions.RunDotnet (fun o' -> {dotnetOptions o' with WorkingDirectory = unpack}) ""
                      ("AltCover.dll -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")
                      "ReleaseXUnitFSharpTypesDotNet"

    Actions.ValidateFSharpTypes x ["main"]

    printfn "Execute the instrumented tests"
    Actions.RunDotnet (fun o' -> {dotnetOptions o' with WorkingDirectory = Path.getFullName "Sample4"}) "test"
                      ("-v m --no-build --configuration Debug sample4.core.fsproj -v m")
                      "sample test returned with a non-zero exit code"
    Actions.ValidateFSharpTypesCoverage x
)

_Target "ReleaseXUnitFSharpTypesDotNetRunner" ( fun _ ->
    Directory.ensure "./_Reports"
    let unpack = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0"
    let x = Path.getFullName "./_Reports/ReleaseXUnitFSharpTypesDotNetRunner.xml"
    let o = Path.getFullName "Sample4/_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.0"
    let i = Path.getFullName "_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.0"

    Shell.cleanDir o

    // Instrument the code
    Actions.RunDotnet (fun o' -> {dotnetOptions o' with WorkingDirectory = unpack} )""
                          ("AltCover.dll -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")
                          "ReleaseXUnitFSharpTypesDotNetRunner"

    Actions.ValidateFSharpTypes x ["main"]

    printfn "Execute the instrumented tests"
    let sample4 = Path.getFullName "./Sample4/sample4.core.fsproj"
    let runner = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0/AltCover.dll"

    // Run
    Actions.RunDotnet (fun o' -> {dotnetOptions o' with WorkingDirectory = o}) ""
                          (runner +
                          " Runner -x \"dotnet\" -r \"" + o +
                          "\" -- test --no-build --configuration Debug " +
                          sample4)
                          "ReleaseXUnitFSharpTypesDotNetRunner test"
    Actions.ValidateFSharpTypesCoverage x
)

_Target "ReleaseXUnitFSharpTypesDotNetFullRunner" ( fun _ ->
    Directory.ensure "./_Reports"
    let unpack = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0"
    let x = Path.getFullName "./_Reports/ReleaseXUnitFSharpTypesDotNetFullRunner.xml"
    let o = Path.getFullName "Sample4/_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.0"
    let i = Path.getFullName "_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.0"

    Shell.cleanDir o

    // Instrument the code
    Actions.RunDotnet (fun o' -> {dotnetOptions o' with WorkingDirectory = unpack} )""
                          ("AltCover.dll --opencover -c=0 \"-c=[Fact]\" -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")
                          "ReleaseXUnitFSharpTypesDotNetFullRunner"

    Actions.CheckSample4Content x

    printfn "Execute the instrumented tests"
    let sample4 = Path.getFullName "./Sample4/sample4.core.fsproj"
    let runner = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0/AltCover.dll"

    // Run
    Actions.RunDotnet (fun o' -> {dotnetOptions o' with WorkingDirectory = o}) ""
                          (runner +
                          " Runner -x \"dotnet\" -r \"" + o +
                          "\" -- test --no-build --configuration Debug " +
                          sample4)
                          "ReleaseXUnitFSharpTypesDotNetFullRunner test"

    Actions.CheckSample4Visits x
)

_Target "MSBuildTest" ( fun _ ->
    Directory.ensure "./_Reports"
    let build = Path.getFullName "Build"
    let sample = Path.getFullName "Sample4"
    let x = Path.getFullName "./_Reports/MSBuildTest.xml"
    // Run
    Shell.cleanDir (sample @@ "_Binaries")
    Actions.RunDotnet (fun o' -> {dotnetOptions o' with WorkingDirectory = sample}) "msbuild"
                          (build @@ "msbuildtest.proj")
                          "MSBuildTest"
    printfn "Checking samples4 output"
    Actions.CheckSample4 x

    // touch-test framework
    let unpack = Path.getFullName "_Packaging/Unpack/tools/net45"
    if (unpack @@ "AltCover.exe") |> File.Exists then
        MSBuild.build (fun p ->
              { p with
                  Verbosity = Some MSBuildVerbosity.Minimal
                  Properties = [
                                 "Configuration", "Debug"
                                 "DebugSymbols", "True"
                               ]})  "./Sample4/Sample4.prepare.fsproj"
    else
      printfn "Skipping touch-test -- AltCover.exe not packaged"
)

_Target "ApiUse" (fun _ ->
  try
    Directory.ensure "./_ApiUse"
    Shell.cleanDir ("./_ApiUse")
    Directory.ensure "./_ApiUse/_DotnetTest"

    let config = XDocument.Load "./Build/NuGet.config.dotnettest"
    let repo = config.Descendants(XName.Get("add")) |> Seq.head
    repo.SetAttributeValue(XName.Get "value", Path.getFullName "./_Packaging" )
    config.Save "./_ApiUse/_DotnetTest/NuGet.config"

    let fsproj = XDocument.Load "./Sample4/sample4.core.fsproj"
    let pack = fsproj.Descendants(XName.Get("PackageReference")) |> Seq.head
    let inject = XElement(XName.Get "PackageReference",
                          XAttribute (XName.Get "Include", "altcover.api"),
                          XAttribute (XName.Get "Version", !Version) )
    pack.AddBeforeSelf inject
    fsproj.Save "./_ApiUse/_DotnetTest/dotnettest.fsproj"
    Shell.copy "./_ApiUse/_DotnetTest" (!! "./Sample4/*.fs")

    let config = """<?xml version="1.0" encoding="utf-8"?>
<configuration>
  <packageSources>
    <add key="local" value="{0}" />
    <add key="nuget.org" value="https://api.nuget.org/v3/index.json" />
  </packageSources>
</configuration>"""

    File.WriteAllText("./_ApiUse/NuGet.config", String.Format(config, Path.getFullName "./_Packaging.api"))

    let script = """#r "paket: groupref netcorebuild //"
#load ".fake/DriveApi.fsx/intellisense.fsx"

open System
open Fake.IO
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing
open Fake.IO.Globbing.Operators
open AltCover
open AltCover.Fake.DotNet

let _Target s f =
  Target.description s
  Target.create s f

_Target "DoIt" (fun _ ->
  AltCover.Api.Version ()
  |> printfn "Returned %A"

  AltCover.Fake.Api.Version ()
  |> Trace.trace

  AltCover.CSApi.Version ()
  |> printfn "Returned %A"

  let collect = { AltCover.CollectParams.Default with LcovReport = "x" }
  let prepare = { AltCover.PrepareParams.Default with TypeFilter = [| "a"; "b" |] }

  printfn "%s" (AltCover.DotNet.ToTestArguments prepare collect)

  let t = DotNet.TestOptions.Create().WithParameters prepare collect
  printfn "returned '%A'" t.Common.CustomParams

  let p2 = { AltCover.PrepareParams.Default with CallContext = [| "[Fact]"; "0" |] }
  let c2 = AltCover.CollectParams.Default

  let setBaseOptions (o:DotNet.Options) =
    { o with WorkingDirectory = Path.getFullName "./_DotnetTest"
             Verbosity = Some DotNet.Verbosity.Minimal }

  DotNet.test (fun to' -> to'.WithCommon(setBaseOptions).WithParameters p2 c2) "dotnettest.fsproj"

  let ipmo = AltCover.Api.Ipmo().Trim().Split().[1].Trim([|'\"'|])
  let command = "$ipmo = '" + ipmo + "'; Import-Module $ipmo; ConvertTo-BarChart -?"

  let pwsh = if Environment.isWindows then
                    Tools.findToolInSubPath "pwsh.exe" (Environment.environVar "ProgramFiles" @@ "PowerShell")
             else "pwsh"

  let r = Fake.Core.Process.execSimple (fun info -> { info with FileName = pwsh
                                                                WorkingDirectory = "."
                                                                Arguments = ("-NoProfile -Command \"" + command + "\"")})
                                   (TimeSpan.FromMinutes 10.0)
  if (r <> 0) then new InvalidOperationException("Non zero return code") |> raise
)

Target.runOrDefault "DoIt"
"""
    File.WriteAllText("./_ApiUse/DriveApi.fsx", script)

    let dependencies = """// [ FAKE GROUP ]
group NetcoreBuild
  source https://api.nuget.org/v3/index.json
  nuget Fake.Core >= 5.3.0
  nuget Fake.Core.Target >= 5.3.0
  nuget Fake.DotNet.Cli >= 5.3.0

  source {0}
  nuget AltCover.Api {1} """

    File.WriteAllText("./_ApiUse/paket.dependencies", String.Format(dependencies, Path.getFullName "./_Packaging.api", !Version))

    Shell.copy "./_ApiUse" (!! "./dotnet*.fsproj")

    Actions.RunDotnet (fun o' -> {dotnetOptions o' with WorkingDirectory = Path.getFullName "_ApiUse"}) "restore"
                      (String.Empty)
                      "restoring dotnet-fake returned with a non-zero exit code"

    Actions.RunDotnet (fun o' -> {dotnetOptions o' with WorkingDirectory = Path.getFullName "_ApiUse"}) "fake"
                      ("run ./DriveApi.fsx")
                      "running fake script returned with a non-zero exit code"

    let x = Path.getFullName "./_ApiUse/_DotnetTest/coverage.xml"
    Actions.CheckSample4 x

  finally
    ["altcover"; "altcover.api"]
    |> List.iter (fun f -> let folder = (nugetCache @@ f) @@ !Version
                           Shell.mkdir folder
                           Shell.deleteDir folder)
)

_Target "DotnetTestIntegration" ( fun _ ->
  try
    Directory.ensure "./_DotnetTest"
    Shell.cleanDir ("./_DotnetTest")
    let config = XDocument.Load "./Build/NuGet.config.dotnettest"
    let repo = config.Descendants(XName.Get("add")) |> Seq.head
    repo.SetAttributeValue(XName.Get "value", Path.getFullName "./_Packaging" )
    config.Save "./_DotnetTest/NuGet.config"

    let fsproj = XDocument.Load "./Sample4/sample4.core.fsproj"
    let pack = fsproj.Descendants(XName.Get("PackageReference")) |> Seq.head
    let inject = XElement(XName.Get "PackageReference",
                          XAttribute (XName.Get "Include", "altcover"),
                          XAttribute (XName.Get "Version", !Version) )
    pack.AddBeforeSelf inject
    fsproj.Save "./_DotnetTest/dotnettest.fsproj"
    Shell.copy "./_DotnetTest" (!! "./Sample4/*.fs")

    DotNet.test (fun to' -> to'.WithCommon (fun o' -> {dotnetOptions o' with WorkingDirectory = Path.getFullName "_DotnetTest"
                                                                             Verbosity = Some DotNet.Verbosity.Minimal
                                                                             CustomParams = Some "/p:AltCover=true /p:AltCoverCallContext=[Fact]|0 /p:AltCoverIpmo=true /p:AltCoverGetVersion=true"
    })

                        ) "dotnettest.fsproj"

//    Actions.RunDotnet (fun o' -> {dotnetOptions o' with WorkingDirectory = Path.getFullName "_DotnetTest"}) "test"
//                      ("-v m /p:AltCover=true /p:AltCoverCallContext=[Fact]|0 /p:AltCoverIpmo=true /p:AltCoverGetVersion=true")
//                      "sample test returned with a non-zero exit code"

    let x = Path.getFullName "./_DotnetTest/coverage.xml"
    Actions.CheckSample4 x

// optest linecover
    Directory.ensure "./_DotnetTestLineCover"
    Shell.cleanDir ("./_DotnetTestLineCover")
    let config = XDocument.Load "./Build/NuGet.config.dotnettest"
    let repo = config.Descendants(XName.Get("add")) |> Seq.head
    repo.SetAttributeValue(XName.Get "value", Path.getFullName "./_Packaging" )
    config.Save "./_DotnetTestLineCover/NuGet.config"

    let fsproj = XDocument.Load "./Sample10/sample10.core.csproj"
    let pack = fsproj.Descendants(XName.Get("PackageReference")) |> Seq.head
    let inject = XElement(XName.Get "PackageReference",
                          XAttribute (XName.Get "Include", "altcover"),
                          XAttribute (XName.Get "Version", !Version) )
    pack.AddBeforeSelf inject
    fsproj.Save "./_DotnetTestLineCover/dotnettest.csproj"
    Shell.copy "./_DotnetTestLineCover" (!! "./Sample10/*.cs")

    Actions.RunDotnet (fun o' -> {dotnetOptions o' with WorkingDirectory = Path.getFullName "_DotnetTestLineCover"}) "test"
                      ("-v m /p:AltCover=true /p:AltCoverLineCover=true")
                      "sample test returned with a non-zero exit code"

    let x = Path.getFullName "./_DotnetTestLineCover/coverage.xml"

    do
      use coverageFile = new FileStream(x, FileMode.Open, FileAccess.Read, FileShare.None, 4096, FileOptions.SequentialScan)
      let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))
      Assert.That (coverageDocument.Descendants(XName.Get("SequencePoint")) |> Seq.length, Is.EqualTo 13)
      Assert.That (coverageDocument.Descendants(XName.Get("BranchPoint")) |> Seq.length, Is.EqualTo 0)

// optest branchcover
    Directory.ensure "./_DotnetTestBranchCover"
    Shell.cleanDir ("./_DotnetTestBranchCover")
    let config = XDocument.Load "./Build/NuGet.config.dotnettest"
    let repo = config.Descendants(XName.Get("add")) |> Seq.head
    repo.SetAttributeValue(XName.Get "value", Path.getFullName "./_Packaging" )
    config.Save "./_DotnetTestLineCover/NuGet.config"

    let fsproj = XDocument.Load "./Sample10/sample10.core.csproj"
    let pack = fsproj.Descendants(XName.Get("PackageReference")) |> Seq.head
    let inject = XElement(XName.Get "PackageReference",
                          XAttribute (XName.Get "Include", "altcover"),
                          XAttribute (XName.Get "Version", !Version) )
    pack.AddBeforeSelf inject
    fsproj.Save "./_DotnetTestBranchCover/dotnettest.csproj"
    Shell.copy "./_DotnetTestBranchCover" (!! "./Sample10/*.cs")

    Actions.RunDotnet (fun o' -> {dotnetOptions o' with WorkingDirectory = Path.getFullName "_DotnetTestBranchCover"}) "test"
                      ("-v m /p:AltCover=true /p:AltCoverBranchCover=true")
                      "sample test returned with a non-zero exit code"

    let x = Path.getFullName "./_DotnetTestBranchCover/coverage.xml"
    do
      use coverageFile = new FileStream(x, FileMode.Open, FileAccess.Read, FileShare.None, 4096, FileOptions.SequentialScan)
      let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))
      Assert.That (coverageDocument.Descendants(XName.Get("SequencePoint")) |> Seq.length, Is.EqualTo 0)
      Assert.That (coverageDocument.Descendants(XName.Get("BranchPoint")) |> Seq.length, Is.EqualTo 2)

    // Regression test -- On travis : 'the reference assemblies for framework ".NETFramework,Version=v4.6.1" were not found.'
    if Environment.isWindows then
        let proj = XDocument.Load "./RegressionTesting/issue29/issue29.xml"
        let pack = proj.Descendants(XName.Get("PackageReference")) |> Seq.head
        let inject = XElement(XName.Get "PackageReference",
                              XAttribute (XName.Get "Include", "altcover"),
                              XAttribute (XName.Get "Version", !Version) )
        pack.AddBeforeSelf inject
        proj.Save "./RegressionTesting/issue29/issue29.csproj"
        Actions.RunDotnet (fun o' -> {dotnetOptions o' with WorkingDirectory = Path.getFullName "RegressionTesting/issue29"}) "test"
                          ("-v m /p:AltCover=true")
                          "issue#29 regression test returned with a non-zero exit code"

  finally
    let folder = (nugetCache @@ "altcover") @@ !Version
    Shell.mkdir folder
    Shell.deleteDir folder
)

_Target "Issue23" ( fun _ ->
  try
    Directory.ensure "./_Issue23"
    Shell.cleanDir ("./_Issue23")
    let config = XDocument.Load "./Build/NuGet.config.dotnettest"
    let repo = config.Descendants(XName.Get("add")) |> Seq.head
    repo.SetAttributeValue(XName.Get "value", Path.getFullName "./_Packaging" )
    config.Save "./_Issue23/NuGet.config"

    let csproj = XDocument.Load "./Sample9/sample9.csproj"
    let pack = csproj.Descendants(XName.Get("PackageReference")) |> Seq.head
    let inject = XElement(XName.Get "PackageReference",
                          XAttribute (XName.Get "Include", "altcover"),
                          XAttribute (XName.Get "Version", !Version) )
    pack.AddBeforeSelf inject
    csproj.Save "./_Issue23/sample9.csproj"
    Shell.copy "./_Issue23" (!! "./Sample9/*.cs")

    Actions.RunDotnet (fun o' -> {dotnetOptions o' with WorkingDirectory = Path.getFullName "_Issue23"}) "restore"
                      ("")
                      "restore returned with a non-zero exit code"

    Actions.RunDotnet (fun o' -> {dotnetOptions o' with WorkingDirectory = Path.getFullName "_Issue23"}) "test"
                      ("-v m /p:AltCover=true /p:AltCoverIpmo=true /p:AltCoverGetVersion=true")
                      "sample test returned with a non-zero exit code"
  finally
    let folder = (nugetCache @@ "altcover") @@ !Version
    Shell.mkdir folder
    Shell.deleteDir folder
)

_Target "DotnetCLIIntegration" ( fun _ ->
  try
    Directory.ensure "./_DotnetCLITest"
    Shell.cleanDir ("./_DotnetCLITest")
    let config = XDocument.Load "./Build/NuGet.config.dotnettest"
    let repo = config.Descendants(XName.Get("add")) |> Seq.head
    repo.SetAttributeValue(XName.Get "value", Path.getFullName "./_Packaging.dotnet" )
    config.Save "./_DotnetCLITest/NuGet.config"

    let fsproj = XDocument.Load "./Sample4/sample4.core.fsproj"
    let pack = fsproj.Descendants(XName.Get("PackageReference")) |> Seq.head
    let inject = XElement(XName.Get "DotNetCliToolReference",
                          XAttribute (XName.Get "Include", "altcover.dotnet"),
                          XAttribute (XName.Get "Version", !Version) )
    pack.AddBeforeSelf inject
    fsproj.Save "./_DotnetCLITest/dotnetcli.fsproj"
    Shell.copy "./_DotnetCLITest" (!! "./Sample4/*.fs")

    let working = Path.getFullName "./_DotnetCLITest"

    Actions.RunDotnet (fun o' -> {dotnetOptions o' with WorkingDirectory = working} ) "build" "" "Restored"

    let x = Path.getFullName "./_Reports/DotnetCLIIntegration.xml"
    let o = Path.getFullName "./_DotnetCLITest/_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.0"

    Actions.RunDotnet (fun o' -> {dotnetOptions o' with WorkingDirectory = working} ) "altcover"
                          ("ipmo")
                          "DotnetCLIIntegration ipmo"

    Actions.RunDotnet (fun o' -> {dotnetOptions o' with WorkingDirectory = working} ) "altcover"
                          ("version")
                          "DotnetCLIIntegration version"
    // Instrument the code
    Actions.RunDotnet (fun o' -> {dotnetOptions o' with WorkingDirectory = working} ) "altcover"
                          (" --opencover --inplace -c=0 \"-c=[Fact]\" -x \"" + x + "\" -i \"" + o + "\"")
                          "DotnetCLIIntegration"
    Actions.CheckSample4Content x

    printfn "Execute the instrumented tests"
    Actions.RunDotnet (fun o' -> {dotnetOptions o' with WorkingDirectory = working}) "altcover"
                          (" Runner -x \"dotnet\" -r \"" + o +
                          "\" -- test --no-build --configuration Debug ")
                          "DotnetCLIIntegration test"

    Actions.CheckSample4Visits x

    let command = """$ipmo = (dotnet altcover ipmo | Out-String).Trim().Split()[1].Trim(@('""')); Import-Module $ipmo; ConvertTo-BarChart -?"""

    Actions.RunRaw (fun info -> { info with
                                        FileName = pwsh
                                        WorkingDirectory = working
                                        Arguments = ("-NoProfile -Command \"" + command + "\"")})
                                   "pwsh"

    Actions.RunDotnet (fun o' -> {dotnetOptions o' with WorkingDirectory = Path.getFullName "_DotnetCLITest"}) "add"
                      ("package altcover.dotnet --version " + !Version)
                      "sample test returned with a non-zero exit code"
    Actions.RunDotnet (fun o' -> {dotnetOptions o' with WorkingDirectory = Path.getFullName "_DotnetCLITest"}) "test"
                      ("-v m /p:AltCover=true")
                      "sample test returned with a non-zero exit code"
    "./_DotnetCLITest/coverage.xml" |> Path.getFullName |> File.Exists |> Assert.That

  finally
    let folder = (nugetCache @@ "altcover.dotnet") @@ !Version
    Shell.mkdir folder
    Shell.deleteDir folder
    let folder2 = ((nugetCache @@ ".tools") @@ "altcover.dotnet") @@ !Version
    Shell.mkdir folder2
    Shell.deleteDir folder2
)

_Target "DotnetGlobalIntegration" ( fun _ ->
  let working = Path.getFullName "./_DotnetGlobalTest"
  let mutable set = false
  try
    Directory.ensure working
    Shell.cleanDir working

    let fsproj = XDocument.Load "./Sample4/sample4.core.fsproj"
    fsproj.Save "./_DotnetGlobalTest/dotnetglobal.fsproj"
    Shell.copy "./_DotnetGlobalTest" (!! "./Sample4/*.fs")

    Actions.RunDotnet (fun o' -> {dotnetOptions o' with WorkingDirectory = working} ) "tool"
                       ("install -g altcover.global --add-source " + (Path.getFullName "./_Packaging.global") + " --version " + !Version) "Installed"

    Actions.RunDotnet (fun o' -> {dotnetOptions o' with WorkingDirectory = working} ) "tool"
                       ("list -g ") "Checked"
    set <- true

    Actions.RunDotnet (fun o' -> {dotnetOptions o' with WorkingDirectory = working} ) "build" "" "Built"

    let x = Path.getFullName "./_Reports/DotnetGlobalIntegration.xml"
    let o = Path.getFullName "./_DotnetGlobalTest/_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.0"

    Actions.RunRaw (fun info -> { info with
                                        FileName = "altcover"
                                        WorkingDirectory = working
                                        Arguments = ("ipmo")})
                                 "DotnetGlobalIntegration ipmo"

    Actions.RunRaw (fun info -> { info with
                                        FileName = "altcover"
                                        WorkingDirectory = working
                                        Arguments = ("version")})
                                 "DotnetGlobalIntegration version"

    // Instrument the code
    Actions.RunRaw (fun info -> { info with
                                        FileName = "altcover"
                                        WorkingDirectory = working
                                        Arguments = (" --opencover --inplace -c=0 \"-c=[Fact]\" -x \"" + x + "\" -i \"" + o + "\"")})
                                 "DotnetGlobalIntegration"

    Actions.CheckSample4Content x

    printfn "Execute the instrumented tests"
    Actions.RunRaw (fun info -> { info with
                                        FileName = "altcover"
                                        WorkingDirectory = working
                                        Arguments = (" Runner -x \"dotnet\" -r \"" + o +
                                                       "\" -- test --no-build --configuration Debug ")})
                                 "DotnetGlobalIntegration test"

    Actions.CheckSample4Visits x
    let command = """$ipmo = (altcover ipmo | Out-String).Trim().Split()[1].Trim(@('""')); Import-Module $ipmo; ConvertTo-BarChart -?"""

    Actions.RunRaw (fun info -> { info with
                                        FileName = pwsh
                                        WorkingDirectory = working
                                        Arguments = ("-NoProfile -Command \"" + command + "\"")})
                                   "pwsh"

    // (fsproj.Descendants(XName.Get("TargetFramework")) |> Seq.head).Value <- "netcoreapp2.1"
    // fsproj.Save "./_DotnetGlobalTest/dotnetglobal.fsproj"
    // Actions.RunDotnet (fun o' -> {dotnetOptions o' with WorkingDirectory = Path.getFullName "_DotnetGlobalTest"}) "add"
    //                   ("package altcover.global")
    //                   "sample test returned with a non-zero exit code"
    // Actions.RunDotnet (fun o' -> {dotnetOptions o' with WorkingDirectory = Path.getFullName "_DotnetGlobalTest"}) "test"
    //                   ("-v m /p:AltCover=true")
    //                   "sample test returned with a non-zero exit code"
    // "./_DotnetGlobalTest/coverage.xml" |> Path.getFullName |> File.Exists |> Assert.That

  finally
    if set then Actions.RunDotnet (fun o' -> {dotnetOptions o' with WorkingDirectory = working} ) "tool"
                                             ("uninstall -g altcover.global") "uninstalled"
    let folder = (nugetCache @@ "altcover.global") @@ !Version
    Shell.mkdir folder
    Shell.deleteDir folder
)

// AOB

_Target "BulkReport" (fun _ ->
    printfn "Overall coverage reporting"
    Directory.ensure "./_Reports/_BulkReport"

    !! "./_Reports/*.xml"
    |> Seq.filter (fun f -> not <| f.EndsWith("Report.xml", StringComparison.OrdinalIgnoreCase))
    |> Seq.toList
    |> ReportGenerator.generateReports
                       (fun p -> { p with ExePath = Tools.findToolInSubPath "ReportGenerator.exe" "."
                                          ReportTypes = [ ReportGenerator.ReportType.Html ]
                                          TargetDir = "_Reports/_BulkReport"})
)

_Target "All" ignore

let resetColours = (fun _ ->
  System.Console.ResetColor()
)
Target.description "ResetConsoleColours"
Target.createFinal "ResetConsoleColours" resetColours
Target.activateFinal "ResetConsoleColours"

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
=?> ("Analysis", Environment.isWindows) // not supported

"Compilation"
==> "Gendarme"
==> "Analysis"

"Compilation"
?=> "UnitTest"

"Compilation"
==> "JustUnitTest"
==> "UnitTest"

"Compilation"
==> "BuildForUnitTestDotNet"
==> "UnitTestDotNet"
==> "UnitTest"

"Compilation"
==> "UnitTestWithOpenCover"
=?> ("UnitTest", Environment.isWindows)  // OpenCover Mono support

"Compilation"
==> "UnitTestWithAltCover"
==> "UnitTest"

"Compilation"
==> "UnitTestWithAltCoverRunner"
==> "UnitTest"

"UnitTestDotNet"
==> "UnitTestWithAltCoverCore"
// ==> "UnitTest"

"UnitTestDotNet"
==> "UnitTestWithAltCoverCoreRunner"
==> "UnitTest"

"UnitTestDotNet"
==> "BuildForCoverlet"
==> "UnitTestDotNetWithCoverlet"
==> "UnitTest"

"Compilation"
?=> "OperationalTest"

"Compilation"
==> "FSharpTypes"
==> "OperationalTest"

"Compilation"
==> "FSharpTests"
==> "OperationalTest"

"Compilation"
==> "FSharpTypesDotNet"
// ==> "OperationalTest"

"Compilation"
==> "FSharpTypesDotNetRunner"
// ==> "OperationalTest" // test is duplicated in the Pester testing

"Compilation"
==> "FSharpTypesDotNetCollecter"
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
==> "RecordResumeTest"
==> "OperationalTest"

"Compilation"
==> "RecordResumeTrackingTest"
==> "OperationalTest"

//"Compilation"
//==> "RecordResumeTestUnderMono"
//=?> ("OperationalTest", Option.isSome monoOnWindows) // System.EntryPointNotFoundException: CreateZStream

"Compilation"
==> "RecordResumeTestDotNet"
==> "OperationalTest"

"Compilation"
==> "SelfTest"
// =?> ("OperationalTest", Environment.isWindows)  // OpenCover Mono support AND Mono + F# + Fake build => no symbols

"Compilation"
?=> "Packaging"

"Compilation"
==> "PrepareFrameworkBuild"
=?> ("Packaging", Environment.isWindows)  // can't ILMerge

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
==> "Pester"
==> "UnitTestWithAltCoverRunner"

"WindowsPowerShell"
=?> ("Pester", Environment.isWindows)

"Unpack"
==> "WindowsPowerShell"
=?> ("Deployment", Environment.isWindows)

"ReleaseXUnitFSharpTypesDotNetRunner"
=?> ("WindowsPowerShell", Environment.isWindows)

"ReleaseXUnitFSharpTypesDotNetRunner"
==> "Pester"

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
//==> "Deployment"

"Unpack"
==> "ReleaseDotNetWithFramework"
==> "Deployment"

"Unpack"
==> "ReleaseFSharpTypesDotNetRunner"
// ==> "Deployment" // test is duplicated in the Pester testing

"Unpack"
==> "ReleaseFSharpTypesX86DotNetRunner"
=?> ("Deployment", Option.isSome dotnetPath86)

"Unpack"
==> "ReleaseXUnitFSharpTypesDotNet"
//==> "Deployment"

"Unpack"
==> "MSBuildTest"
==> "Deployment"

"Unpack"
==> "ApiUse"
==> "Deployment"

"Unpack"
==> "DotnetTestIntegration"
==> "Deployment"

"Unpack"
==> "Issue23"
=?> ("Deployment", Environment.isWindows)

"Unpack"
==> "DotnetCLIIntegration"
==> "Deployment"

"Unpack"
==> "DotnetGlobalIntegration"
==> "Deployment"

"Unpack"
==> "ReleaseXUnitFSharpTypesDotNetRunner"
==> "Deployment"

"Unpack"
==> "ReleaseXUnitFSharpTypesDotNetFullRunner"
==> "Deployment"

"Analysis"
==> "All"

"UnitTest"
==> "All"

"OperationalTest"
==> "All"

"Deployment"
==> "BulkReport"
==> "All"

Target.runOrDefault "All"