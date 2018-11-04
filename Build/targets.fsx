open System
open System.Diagnostics.Tracing
open System.IO
open System.Reflection
open System.Xml
open System.Xml.Linq

open Actions
open AltCover.Fake.DotNet.Testing

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

let Copyright = ref String.Empty
let Version = ref String.Empty
let consoleBefore = (Console.ForegroundColor, Console.BackgroundColor)

let OpenCoverFilter = "+[AltCove*]* -[*]Microsoft.* -[*]System.* +[*]N.*"

let AltCoverFilter(p : AltCover.PrepareParams) =
  { p with MethodFilter = "WaitForExitCustom" :: (p.MethodFilter |> Seq.toList)
           AssemblyExcludeFilter =
             [ "Adapter"; "Tests" ] @ (p.AssemblyExcludeFilter |> Seq.toList)
           AssemblyFilter =
             [ "Mono"; @"\.Recorder"; "Sample"; "nunit" ]
             @ (p.AssemblyFilter |> Seq.toList)
           TypeFilter = [ @"System\."; @"Sample3\.Class2" ] @ (p.TypeFilter |> Seq.toList) }

let AltCoverFilterX(p : AltCover.PrepareParams) =
  { p with MethodFilter = "WaitForExitCustom" :: (p.MethodFilter |> Seq.toList)
           AssemblyExcludeFilter = "Adapter" :: (p.AssemblyExcludeFilter |> Seq.toList)
           AssemblyFilter =
             [ "Mono"; @"\.Recorder"; "Sample"; "nunit" ]
             @ (p.AssemblyFilter |> Seq.toList)
           TypeFilter = [ @"System\."; @"Sample3\.Class2" ] @ (p.TypeFilter |> Seq.toList) }

let AltCoverFilterG(p : AltCover.PrepareParams) =
  { p with MethodFilter = "WaitForExitCustom" :: (p.MethodFilter |> Seq.toList)
           AssemblyExcludeFilter =
             [ "Adapter"; "Tests" ] @ (p.AssemblyExcludeFilter |> Seq.toList)
           AssemblyFilter =
             [ "Mono"; @"\.Recorder\.g"; "Sample"; "nunit" ]
             @ (p.AssemblyFilter |> Seq.toList)
           TypeFilter = [ @"System\."; @"Sample3\.Class2" ] @ (p.TypeFilter |> Seq.toList) }

let programFiles = Environment.environVar "ProgramFiles"
let programFiles86 = Environment.environVar "ProgramFiles(x86)"
let dotnetPath = "dotnet" |> Fake.Core.ProcessUtils.tryFindFileOnPath

let dotnetOptions (o : DotNet.Options) =
  match dotnetPath with
  | Some f -> { o with DotNetCliPath = f }
  | None -> o

let monoOnWindows =
  if Environment.isWindows then
    [ programFiles; programFiles86 ]
    |> List.filter (String.IsNullOrWhiteSpace >> not)
    |> List.map (fun s -> s @@ "Mono/bin/mono.exe")
    |> List.tryFind File.Exists
  else None

let dotnetPath86 =
  if Environment.isWindows then
    let perhaps =
      [ programFiles86 ]
      |> List.filter (String.IsNullOrWhiteSpace >> not)
      |> List.map (fun s -> s @@ "dotnet\dotnet.EXE")
      |> List.tryFind File.Exists
    match perhaps with
    | Some path ->
      try // detect if we have the SDK
        DotNet.info
          (fun opt ->
          { opt with Common = { dotnetOptions opt.Common with DotNetCliPath = path } })
        |> ignore
        perhaps
      with _ -> None
    | _ -> None
  else None

let nugetCache =
  Path.Combine
    (Environment.GetFolderPath Environment.SpecialFolder.UserProfile, ".nuget/packages")

let pwsh =
  if Environment.isWindows then
    Tools.findToolInSubPath "pwsh.exe" (programFiles @@ "PowerShell")
  else "pwsh"

let cliArguments =
  { MSBuild.CliArguments.Create() with ConsoleLogParameters = []
                                       DistributedLoggers = None
                                       DisableInternalBinLog = true }

let _Target s f =
  Target.description s
  Target.create s f

// Preparation

_Target "Preparation" ignore

_Target "Clean" (fun _ ->
  printfn "Cleaning the build and deploy folders"
  Actions.Clean())

_Target "SetVersion" (fun _ ->
  let appveyor = Environment.environVar "APPVEYOR_BUILD_VERSION"
  let travis = Environment.environVar "TRAVIS_JOB_NUMBER"
  let version = Actions.GetVersionFromYaml()

  let ci =
    if String.IsNullOrWhiteSpace appveyor then
      if String.IsNullOrWhiteSpace travis then String.Empty
      else version.Replace("{build}", travis + "-travis")
    else appveyor

  let (v, majmin, y) = Actions.LocalVersion ci version
  Version := v
  let copy = sprintf "© 2010-%d by Steve Gilham <SteveGilham@users.noreply.github.com>" y
  Copyright := "Copyright " + copy

  Directory.ensure "./_Generated"
  Actions.InternalsVisibleTo(!Version)
  let v' = !Version
  [ "./_Generated/AssemblyVersion.fs"; "./_Generated/AssemblyVersion.cs" ]
  |> List.iter
       (fun file ->
       AssemblyInfoFile.create file [ AssemblyInfo.Product "AltCover"
                                      AssemblyInfo.Version(majmin + ".0.0")
                                      AssemblyInfo.FileVersion v'
                                      AssemblyInfo.Company "Steve Gilham"
                                      AssemblyInfo.Trademark ""
                                      AssemblyInfo.Copyright copy ]
         (Some AssemblyInfoFileConfig.Default))
  let hack = """namespace AltCover
module SolutionRoot =
  let location = """ + "\"\"\"" + (Path.getFullName ".") + "\"\"\""
  let path = "_Generated/SolutionRoot.fs"

  // Update the file only if it would change
  let old =
    if File.Exists(path) then File.ReadAllText(path)
    else String.Empty
  if not (old.Equals(hack)) then File.WriteAllText(path, hack))

// Basic compilation

_Target "Compilation" ignore

_Target "BuildRelease" (fun _ ->
  try
    "AltCover.sln"
    |> MSBuild.build (fun p ->
         { p with Verbosity = Some MSBuildVerbosity.Normal
                  ConsoleLogParameters = []
                  DistributedLoggers = None
                  DisableInternalBinLog = true
                  Properties =
                    [ "Configuration", "Release"
                      "DebugSymbols", "True" ] })
    "./altcover.core.sln"
    |> DotNet.build (fun p ->
         { p with Configuration = DotNet.BuildConfiguration.Release
                  Common = dotnetOptions p.Common
                  MSBuildParams = cliArguments })
  with x ->
    printfn "%A" x
    reraise())

_Target "BuildDebug" (fun _ ->
  "AltCover.sln"
  |> MSBuild.build (fun p ->
       { p with Verbosity = Some MSBuildVerbosity.Normal
                ConsoleLogParameters = []
                DistributedLoggers = None
                DisableInternalBinLog = true
                Properties =
                  [ "Configuration", "Debug"
                    "DebugSymbols", "True" ] })
  "./altcover.core.sln"
  |> DotNet.build (fun p ->
       { p with Configuration = DotNet.BuildConfiguration.Debug
                Common = dotnetOptions p.Common
                MSBuildParams = cliArguments }))

_Target "BuildMonoSamples" (fun _ ->
  let mcs = "_Binaries/MCS/Release+AnyCPU/MCS.exe"
  [ ("./_Mono/Sample1",
     [ "-debug"; "-out:./_Mono/Sample1/Sample1.exe"; "./Sample1/Program.cs" ])

    ("./_Mono/Sample3",
     [ "-target:library"; "-debug"; "-out:./_Mono/Sample3/Sample3.dll";
       "-lib:./packages/Mono.Cecil.0.10.1/lib/net40"; "-r:Mono.Cecil.dll";
       "./Sample3/Class1.cs" ]) ]
  |> Seq.iter
       (fun (dir, cmd) ->
       Directory.ensure dir
       ("Mono compilation of '" + String.Join(" ", cmd) + "' failed")
       |> Actions.Run(mcs, ".", cmd))
  Actions.FixMVId [ "./_Mono/Sample1/Sample1.exe"; "./_Mono/Sample3/Sample3.dll" ])

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
  ())

_Target "Gendarme" (fun _ -> // Needs debug because release is compiled --standalone which contaminates everything
  Directory.ensure "./_Reports"

  let rules =
    if Environment.isWindows then "./Build/rules.xml"
    else "./Build/rules-mono.xml"

  [ (rules,
     [ "_Binaries/AltCover/Debug+AnyCPU/AltCover.exe";
       "_Binaries/AltCover.Shadow/Debug+AnyCPU/AltCover.Shadow.dll" ])

    ("./Build/rules-posh.xml",
     [ "_Binaries/AltCover.PowerShell/Debug+AnyCPU/AltCover.PowerShell.dll";
       "_Binaries/AltCover.FSApi/Debug+AnyCPU/AltCover.FSApi.dll" ])

    ("./Build/rules-gtk.xml",
     [ "_Binaries/AltCover.Visualizer/Debug+AnyCPU/AltCover.Visualizer.exe" ]) ]
  |> Seq.iter (fun (ruleset, files) ->
       Gendarme.run { Gendarme.Params.Create() with WorkingDirectory = "."
                                                    Severity = Gendarme.Severity.All
                                                    Confidence = Gendarme.Confidence.All
                                                    Configuration = ruleset
                                                    Console = true
                                                    Log = "./_Reports/gendarme.html"
                                                    LogKind = Gendarme.LogKind.Html
                                                    Targets = files }))

_Target "FxCop" (fun _ -> // Needs debug because release is compiled --standalone which contaminates everything
  Directory.ensure "./_Reports"

  let rules =   ["-Microsoft.Design#CA1004"
                 "-Microsoft.Design#CA1006"
                 "-Microsoft.Design#CA1011" // maybe sometimes
                 "-Microsoft.Design#CA1062" // null checks,  In F#!
                 "-Microsoft.Maintainability#CA1506"
                 "-Microsoft.Naming#CA1704"
                 "-Microsoft.Naming#CA1707"
                 "-Microsoft.Naming#CA1709"
                 "-Microsoft.Naming#CA1715" ]

  [   ([
         "_Binaries/AltCover/Debug+AnyCPU/AltCover.exe"
       ],   ["AltCover.AltCover"
             "AltCover.Api"
             "AltCover.Args"
             "AltCover.Augment"
             "AltCover.Collect"
             "AltCover.CollectParams"
             "AltCover.CommandLine"
             "AltCover.Filter"
             "AltCover.FilterClass"
             "AltCover.Fix"
             "AltCover.GetVersion"
             "AltCover.Instrument"
             "AltCover.KeyRecord"
             "AltCover.KeyStore"
             "AltCover.Logging"
             "AltCover.Main"
             "AltCover.Naming"
             "AltCover.Node"
             "AltCover.PowerShell"
             "AltCover.Prepare"
             "AltCover.PrepareParams"
             "AltCover.ProgramDatabase"
             "AltCover.Report"
             "AltCover.Runner"
             "AltCover.Visitor"], rules);
      ([
          "_Binaries/AltCover.Shadow/Debug+AnyCPU/AltCover.Shadow.dll"
       ], ["AltCover.Recorder.Assist"
           "AltCover.Recorder.Counter"
           "AltCover.Recorder.Assist"
           "AltCover.Recorder.Tracer"
           "AltCover.Recorder.Instance"], rules);
      ([
         "_Binaries/AltCover.PowerShell/Debug+AnyCPU/AltCover.PowerShell.dll"
       ], [], ["-Microsoft.Design#CA1059"
               "-Microsoft.Usage#CA2235"
               "-Microsoft.Performance#CA1819"
               "-Microsoft.Design#CA1020"
               "-Microsoft.Design#CA1004"
               "-Microsoft.Design#CA1006"
               "-Microsoft.Design#CA1011"
               "-Microsoft.Design#CA1062"
               "-Microsoft.Maintainability#CA1506"
               "-Microsoft.Naming#CA1704"
               "-Microsoft.Naming#CA1707"
               "-Microsoft.Naming#CA1709"
               "-Microsoft.Naming#CA1715"]);
      ([
         "_Binaries/AltCover.FSApi/Debug+AnyCPU/AltCover.FSApi.dll"
       ], [], ["-Microsoft.Usage#CA2235";
               "-Microsoft.Performance#CA1819"
               "-Microsoft.Design#CA1020"
               "-Microsoft.Design#CA1004"
               "-Microsoft.Design#CA1006"
               "-Microsoft.Design#CA1011"
               "-Microsoft.Design#CA1062"
               "-Microsoft.Maintainability#CA1506"
               "-Microsoft.Naming#CA1704"
               "-Microsoft.Naming#CA1707"
               "-Microsoft.Naming#CA1709"
               "-Microsoft.Naming#CA1715"]);
      ([
         "_Binaries/AltCover.Visualizer/Debug+AnyCPU/AltCover.Visualizer.exe"
       ], [
            "AltCover.Augment"
            "AltCover.Visualizer.Transformer"
            "AltCover.Visualizer.CoverageFile"
            "AltCover.Visualizer.Extensions"
            "AltCover.Visualizer.Gui"
           ], ["-Microsoft.Usage#CA2208"
               "-Microsoft.Usage#CA2235"
               "-Microsoft.Maintainability#CA1506"
               "-Microsoft.Design#CA1004"
               "-Microsoft.Design#CA1006"
               "-Microsoft.Naming#CA1707"
               "-Microsoft.Naming#CA1715"
               "-Microsoft.Naming#CA1704"
               "-Microsoft.Naming#CA1709"])
      ]
    |> Seq.iter (fun (files, types, ruleset) -> files
                                                |> FxCop.run { FxCop.Params.Create() with WorkingDirectory = "."
                                                                                          UseGAC = true
                                                                                          Verbose = false
                                                                                          ReportFileName = "_Reports/FxCopReport.xml"
                                                                                          Types = types
                                                                                          Rules = ruleset
                                                                                          FailOnError = FxCop.ErrorLevel.Warning
                                                                                          IgnoreGeneratedCode = true})

  ["_Binaries/AltCover.PowerShell/Debug+AnyCPU/AltCover.PowerShell.dll"]
  |> FxCop.run { FxCop.Params.Create() with WorkingDirectory = "."
                                            UseGAC = true
                                            Verbose = false
                                            ReportFileName = "_Reports/FxCopReport.xml"
                                            RuleLibraries =
                                              [ Path.getFullName
                                                  "ThirdParty/Microsoft.PowerShell.CodeAnalysis.15.dll" ]
                                            FailOnError = FxCop.ErrorLevel.Warning
                                            IgnoreGeneratedCode = true })

// Unit Test

_Target "UnitTest" (fun _ ->
  let numbers =
    !!(@"_Reports/_Unit*/Summary.xml")
    |> Seq.collect (fun f ->
         let xml = XDocument.Load f
         xml.Descendants(XName.Get("Linecoverage"))
         |> Seq.map (fun e ->
              let coverage = e.Value.Replace("%", String.Empty)
              match Double.TryParse coverage with
              | (false, _) ->
                Assert.Fail("Could not parse coverage " + coverage)
                0.0
              | (_, numeric) ->
                printfn "%s : %A" (f
                                   |> Path.GetDirectoryName
                                   |> Path.GetFileName) numeric
                numeric))
    |> Seq.toList
  if numbers
     |> List.tryFind (fun n -> n <= 99.0)
     |> Option.isSome
  then Assert.Fail("Coverage is too low"))

_Target "JustUnitTest" (fun _ ->
  Directory.ensure "./_Reports"
  try
    let here = Path.getFullName "."

    !!(@"_Binaries/*Tests/Debug+AnyCPU/*XTest*.dll")
    |> Fake.DotNet.Testing.XUnit2.run (fun p ->
         { p with ToolPath = Tools.findToolInSubPath "xunit.console.exe" "."
                  NUnitXmlOutputPath = Some "./_Reports/JustXUnitTestReport.xml"
                  WorkingDir = Some here
                  ShadowCopy = false })

    !!(@"_Binaries/*Tests*/Debug+AnyCPU/*Test*.dll")
    |> Seq.filter
         (fun f ->
         Path.GetFileName(f) <> "AltCover.XTests.dll"
         && Path.GetFileName(f) <> "xunit.runner.visualstudio.testadapter.dll")
    |> NUnit3.run (fun p ->
         { p with ToolPath = Tools.findToolInSubPath "nunit3-console.exe" "."
                  WorkingDir = "."
                  ResultSpecs = [ "./_Reports/JustUnitTestReport.xml" ] })
  with x ->
    printfn "%A" x
    reraise())

_Target "BuildForUnitTestDotNet" (fun _ ->
  !!(@"./*Tests/*.tests.core.fsproj")
  |> Seq.iter (DotNet.build (fun p ->
                 { p with Configuration = DotNet.BuildConfiguration.Debug
                          Common = dotnetOptions p.Common
                          MSBuildParams = cliArguments })))

_Target "UnitTestDotNet" (fun _ ->
  Directory.ensure "./_Reports"
  try
    !!(@"./*Tests/*.tests.core.fsproj")
    |> Seq.iter (DotNet.test (fun p ->
                   { p with Configuration = DotNet.BuildConfiguration.Debug
                            NoBuild = true
                            Common = dotnetOptions p.Common
                            MSBuildParams = cliArguments }))
  with x ->
    printfn "%A" x
    reraise())

_Target "BuildForCoverlet" (fun _ ->
  !!(@"./*Tests/*.tests.core.fsproj")
  |> Seq.iter (DotNet.build (fun p ->
                 { p with Configuration = DotNet.BuildConfiguration.Debug
                          Common = dotnetOptions p.Common
                          MSBuildParams = cliArguments })))

_Target "UnitTestDotNetWithCoverlet" (fun _ ->
  Directory.ensure "./_Reports"
  try
    let xml =
      !!(@"./*Tests/*.tests.core.fsproj")
      |> Seq.zip
           [ """/p:CollectCoverage=true /p:CoverletOutputFormat=opencover /p:Exclude="\"[Sample*]*,[AltCover.Record*]*,[NUnit*]*,[AltCover.Shadow.Adapter]*\""  """;
             """/p:CollectCoverage=true /p:CoverletOutputFormat=opencover /p:Exclude="\"[Sample*]*,[AltCover.Record*]*,[NUnit*]*,[AltCover.Shadow.Adapter]*\""  """;
             """/p:CollectCoverage=true /p:CoverletOutputFormat=opencover /p:Exclude="\"[Sample*]*,[AltCover.Record*]*\""  """ ]
      |> Seq.fold (fun l (p, f) ->
           try
             f
             |> DotNet.test (fun o ->
                  { o with Configuration = DotNet.BuildConfiguration.Debug
                           NoBuild = true
                           Framework = Some "netcoreapp2.1"
                           Common =
                             { (dotnetOptions o.Common) with CustomParams = Some p }
                           MSBuildParams = cliArguments })
           with x -> eprintf "%A" x
           let here = Path.GetDirectoryName f
           (here @@ "coverage.opencover.xml") :: l) []

    ReportGenerator.generateReports (fun p ->
      { p with ExePath = Tools.findToolInSubPath "ReportGenerator.exe" "."
               ReportTypes =
                 [ ReportGenerator.ReportType.Html; ReportGenerator.ReportType.XmlSummary ]
               TargetDir = "_Reports/_UnitTestWithCoverlet" }) xml
  with x ->
    printfn "%A" x
    reraise())

_Target "UnitTestWithOpenCover" (fun _ ->
  Directory.ensure "./_Reports/_UnitTestWithOpenCover"
  let testFiles =
    !!(@"_Binaries/*Tests/Debug+AnyCPU/*Test*.dll")
    |> Seq.filter
         (fun f ->
         Path.GetFileName(f) <> "AltCover.XTests.dll"
         && Path.GetFileName(f) <> "xunit.runner.visualstudio.testadapter.dll")
  let xtestFiles = !!(@"_Binaries/*Tests/Debug+AnyCPU/*XTest*.dll")
  let coverage = Path.getFullName "_Reports/UnitTestWithOpenCover.xml"
  let xcoverage = Path.getFullName "_Reports/XUnitTestWithOpenCover.xml"

  try
    OpenCover.run (fun p ->
      { p with WorkingDir = "."
               ExePath = Tools.findToolInSubPath "OpenCover.Console.exe" "."
               TestRunnerExePath = Tools.findToolInSubPath "xunit.console.exe" "."
               Filter =
                 "+[AltCover]* +[AltCover.Shadow]* +[AltCover.Runner]* +[AltCover.WeakNameTests]Alt* -[*]Microsoft.* -[*]System.* -[Sample*]*"
               MergeByHash = true
               OptionalArguments =
                 "-excludebyattribute:*ExcludeFromCodeCoverageAttribute;*ProgIdAttribute"
               Register = OpenCover.RegisterType.RegisterUser
               Output = xcoverage })
      (String.Join(" ", xtestFiles)
       + " -parallel none -noshadow -nunit _Reports/XUnitTestWithOpenCoverReport.xml")

    OpenCover.run (fun p ->
      { p with WorkingDir = "."
               ExePath = Tools.findToolInSubPath "OpenCover.Console.exe" "."
               TestRunnerExePath = Tools.findToolInSubPath "nunit3-console.exe" "."
               Filter =
                 "+[AltCover]* +[AltCover.Shadow]* +[AltCover.Runner]* +[AltCover.WeakNameTests]Alt* -[*]Microsoft.* -[*]System.* -[Sample*]*"
               MergeByHash = true
               OptionalArguments =
                 "-excludebyattribute:*ExcludeFromCodeCoverageAttribute;*ProgIdAttribute"
               Register = OpenCover.RegisterType.RegisterUser
               Output = coverage })
      (String.Join(" ", testFiles)
       + " --result=./_Reports/UnitTestWithOpenCoverReport.xml")
  with x ->
    printfn "%A" x
    reraise()

  ReportGenerator.generateReports (fun p ->
    { p with ExePath = Tools.findToolInSubPath "ReportGenerator.exe" "."
             ReportTypes =
               [ ReportGenerator.ReportType.Html; ReportGenerator.ReportType.XmlSummary ]
             TargetDir = "_Reports/_UnitTestWithOpenCover" }) [ coverage; xcoverage ])

// Hybrid (Self) Tests

_Target "UnitTestWithAltCover" (fun _ ->
  Directory.ensure "./_Reports/_UnitTestWithAltCover"
  let keyfile = Path.getFullName "Build/SelfTest.snk"
  let reports = Path.getFullName "./_Reports"
  let altcover = Tools.findToolInSubPath "AltCover.exe" "./_Binaries"
  let here = Path.getFullName "."

  let testDirectory = Path.getFullName "_Binaries/AltCover.Tests/Debug+AnyCPU"
  let xtestDirectory = Path.getFullName "_Binaries/AltCover.XTests/Debug+AnyCPU"

  if !!(testDirectory @@ "AltCov*.pdb")
     |> Seq.length > 0 then
    let xaltReport = reports @@ "XUnitTestWithAltCover.xml"
    printfn "Instrumented the code"
    let prep =
      { AltCover.PrepareParams.Create() with XmlReport = xaltReport
                                             OutputDirectory = "./__UnitTestWithAltCover"
                                             StrongNameKey = keyfile
                                             OpenCover = false
                                             InPlace = false
                                             Save = false }
      |> AltCoverFilter
      |> AltCover.Prepare
    { AltCover.Params.Create prep with ToolPath = altcover
                                       ToolType = AltCover.ToolType.Framework
                                       WorkingDirectory = xtestDirectory }
    |> AltCover.run

    printfn "Unit test the instrumented code"
    !!(@"_Binaries/*Tests/Debug+AnyCPU/__UnitTestWithAltCover/*XTest*.dll")
    |> Fake.DotNet.Testing.XUnit2.run (fun p ->
         { p with ToolPath = Tools.findToolInSubPath "xunit.console.exe" "."
                  NUnitXmlOutputPath = Some "./_Reports/XUnitTestWithAltCoverReport.xml"
                  WorkingDir = Some here
                  ShadowCopy = false })

    let altReport = reports @@ "UnitTestWithAltCover.xml"
    printfn "Instrumented the code"
    let prep =
      { AltCover.PrepareParams.Create() with XmlReport = altReport
                                             OutputDirectory = "./__UnitTestWithAltCover"
                                             StrongNameKey = keyfile
                                             OpenCover = false
                                             InPlace = false
                                             Save = false }
      |> AltCoverFilter
      |> AltCover.Prepare
    { AltCover.Params.Create prep with ToolPath = altcover
                                       ToolType = AltCover.ToolType.Framework
                                       WorkingDirectory = testDirectory }
    |> AltCover.run

    let sn = "sn" |> Fake.Core.ProcessUtils.tryFindFileOnPath
    if sn |> Option.isSome then
      Actions.Run
        (sn |> Option.get, testDirectory,
         [ "-vf"; "./__UnitTestWithAltCover/AltCover.Recorder.g.dll" ])
        "Recorder assembly strong-name verified OK"

    printfn "Unit test the instrumented code"
    try
      [ !!"_Binaries/AltCover.Tests/Debug+AnyCPU/__UnitTestWithAltCover/*.Tests.dll"
        !!"_Binaries/AltCover.Tests/Debug+AnyCPU/__UnitTestWithAltCover/*ple2.dll" ]
      |> Seq.concat
      |> Seq.distinct
      |> NUnit3.run (fun p ->
           { p with ToolPath = Tools.findToolInSubPath "nunit3-console.exe" "."
                    WorkingDir = "."
                    ResultSpecs = [ "./_Reports/UnitTestWithAltCoverReport.xml" ] })
    with x ->
      printfn "%A" x
      reraise()

    printfn "Instrument the weakname tests"
    let weakDir = Path.getFullName "_Binaries/AltCover.WeakNameTests/Debug+AnyCPU"
    let weakReport = reports @@ "WeakNameTestWithAltCover.xml"

    let prep =
      { AltCover.PrepareParams.Create() with XmlReport = weakReport
                                             OutputDirectory =
                                               "./__WeakNameTestWithAltCover"
                                             StrongNameKey = keyfile
                                             InPlace = false
                                             Save = false }
      |> AltCoverFilter
      |> AltCover.Prepare
    { AltCover.Params.Create prep with ToolPath = altcover
                                       ToolType = AltCover.ToolType.Framework
                                       WorkingDirectory = weakDir }
    |> AltCover.run

    printfn "Execute the weakname tests"
    !!("_Binaries/AltCover.WeakNameTests/Debug+AnyCPU/__WeakNameTestWithAltCover/*Test*.dll")
    |> NUnit3.run (fun p ->
         { p with ToolPath = Tools.findToolInSubPath "nunit3-console.exe" "."
                  WorkingDir = "."
                  ResultSpecs = [ "./_Reports/WeakNameTestWithAltCoverReport.xml" ] })

    printfn "Instrument the shadow tests"
    let shadowDir = Path.getFullName "_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU"
    let shadowReport = reports @@ "ShadowTestWithAltCover.xml"

    let prep =
      { AltCover.PrepareParams.Create() with XmlReport = shadowReport
                                             OutputDirectory =
                                               "./__ShadowTestWithAltCover"
                                             StrongNameKey = keyfile
                                             InPlace = false
                                             Save = false }
      |> AltCoverFilter
      |> AltCover.Prepare
    { AltCover.Params.Create prep with ToolPath = altcover
                                       ToolType = AltCover.ToolType.Framework
                                       WorkingDirectory = shadowDir }
    |> AltCover.run

    printfn "Execute the shadow tests"
    !!("_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU/__ShadowTestWithAltCover/*.Test*.dll")
    |> NUnit3.run (fun p ->
         { p with ToolPath = Tools.findToolInSubPath "nunit3-console.exe" "."
                  WorkingDir = "."
                  ResultSpecs = [ "./_Reports/ShadowTestWithAltCoverReport.xml" ] })

    ReportGenerator.generateReports (fun p ->
      { p with ExePath = Tools.findToolInSubPath "ReportGenerator.exe" "."
               ReportTypes =
                 [ ReportGenerator.ReportType.Html; ReportGenerator.ReportType.XmlSummary ]
               TargetDir = "_Reports/_UnitTestWithAltCover" })
      [ xaltReport; altReport; weakReport; shadowReport ]
  else printfn "Symbols not present; skipping")

_Target "UnitTestWithAltCoverRunner" (fun _ ->
  Directory.ensure "./_Reports/_UnitTestWithAltCover"
  let keyfile = Path.getFullName "Build/SelfTest.snk"
  let reports = Path.getFullName "./_Reports"
  let altcover =
    Tools.findToolInSubPath "AltCover.exe" "./_Binaries/AltCover/Debug+AnyCPU"
  let nunit = Tools.findToolInSubPath "nunit3-console.exe" "."
  let here = Path.getFullName "."

  let testDirectory = Path.getFullName "_Binaries/AltCover.Tests/Debug+AnyCPU"
  let xtestDirectory = Path.getFullName "_Binaries/AltCover.XTests/Debug+AnyCPU"
  if !!(testDirectory @@ "AltCov*.pdb")
     |> Seq.length > 0 then
    let xaltReport = reports @@ "XUnitTestWithAltCoverRunner.xml"

    printfn "Instrumented the code"
    let prep =
      { AltCover.PrepareParams.Create() with XmlReport = xaltReport
                                             OutputDirectory =
                                               "./__UnitTestWithAltCoverRunner"
                                             StrongNameKey = keyfile
                                             Single = true
                                             InPlace = false
                                             Save = false }
      |> AltCoverFilter
      |> AltCover.Prepare
    { AltCover.Params.Create prep with ToolPath = altcover
                                       ToolType = AltCover.ToolType.Framework
                                       WorkingDirectory = xtestDirectory }
    |> AltCover.run

    printfn "Unit test the instrumented code"
    try
      let collect =
        { AltCover.CollectParams.Create() with Executable = Tools.findToolInSubPath "xunit.console.exe" "."
                                               RecorderDirectory = xtestDirectory @@ "__UnitTestWithAltCoverRunner" }
          .withCommandLine [ Path.getFullName
                               "_Binaries/AltCover.XTests/Debug+AnyCPU/__UnitTestWithAltCoverRunner/AltCover.XTests.dll"
                             "-parallel"
                             "none"
                             "-noshadow"
                             "-nunit"
                             "./_Reports/XUnitTestWithAltCoverRunnerReport.xml" ]
        |> AltCover.Collect
      { AltCover.Params.Create collect with ToolPath = altcover
                                            ToolType = AltCover.ToolType.Framework
                                            WorkingDirectory = here }
      |> AltCover.run
    with x ->
      printfn "%A" x
      reraise()

    let altReport = reports @@ "UnitTestWithAltCoverRunner.xml"
    printfn "Instrumented the code"
    let prep =
      { AltCover.PrepareParams.Create() with XmlReport = altReport
                                             OutputDirectory =
                                               "./__UnitTestWithAltCoverRunner"
                                             StrongNameKey = keyfile
                                             Single = true
                                             InPlace = false
                                             Save = false }
      |> AltCoverFilter
      |> AltCover.Prepare
    { AltCover.Params.Create prep with ToolPath = altcover
                                       ToolType = AltCover.ToolType.Framework
                                       WorkingDirectory = testDirectory }
    |> AltCover.run

    printfn "Unit test the instrumented code"
    try
      let collect =
        { AltCover.CollectParams.Create() with Executable = nunit
                                               RecorderDirectory = testDirectory @@ "__UnitTestWithAltCoverRunner" }
          .withCommandLine [ "--noheader"
                             "--work=."
                             "--result=./_Reports/UnitTestWithAltCoverRunnerReport.xml"

                             Path.getFullName
                               "_Binaries/AltCover.Tests/Debug+AnyCPU/__UnitTestWithAltCoverRunner/AltCover.Tests.dll"

                             Path.getFullName
                               "_Binaries/AltCover.Tests/Debug+AnyCPU/__UnitTestWithAltCoverRunner/Sample2.dll" ]
        |> AltCover.Collect
      { AltCover.Params.Create collect with ToolPath = altcover
                                            ToolType = AltCover.ToolType.Framework
                                            WorkingDirectory = "." }
      |> AltCover.run
    with x ->
      printfn "%A" x
      reraise()

    printfn "Instrument the weakname tests"
    let weakDir = Path.getFullName "_Binaries/AltCover.WeakNameTests/Debug+AnyCPU"
    let weakReport = reports @@ "WeakNameTestWithAltCoverRunner.xml"

    let prep =
      { AltCover.PrepareParams.Create() with XmlReport = weakReport
                                             OutputDirectory =
                                               "./__WeakNameTestWithAltCoverRunner"
                                             TypeFilter = [ "WeakNameTest" ]
                                             StrongNameKey = keyfile
                                             Single = true
                                             InPlace = false
                                             Save = false }
      |> AltCoverFilterX
      |> AltCover.Prepare
    { AltCover.Params.Create prep with ToolPath = altcover
                                       ToolType = AltCover.ToolType.Framework
                                       WorkingDirectory = weakDir }
    |> AltCover.run

    printfn "Execute the weakname tests"
    let collect =
      { AltCover.CollectParams.Create() with Executable = nunit
                                             RecorderDirectory = weakDir @@ "__WeakNameTestWithAltCoverRunner" }
        .withCommandLine [ "--noheader"
                           "--work=."
                           "--result=./_Reports/ShadowTestWithAltCoverRunnerReport.xml"

                           Path.getFullName
                             "_Binaries/AltCover.WeakNameTests/Debug+AnyCPU/__WeakNameTestWithAltCoverRunner/AltCover.WeakNameTests.dll" ]
      |> AltCover.Collect
    { AltCover.Params.Create collect with ToolPath = altcover
                                          ToolType = AltCover.ToolType.Framework
                                          WorkingDirectory = "." }
    |> AltCover.run

    printfn "Instrument the shadow tests"
    let shadowDir = Path.getFullName "_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU"
    let shadowReport = reports @@ "ShadowTestWithAltCoverRunner.xml"

    let prep =
      { AltCover.PrepareParams.Create() with XmlReport = shadowReport
                                             OutputDirectory =
                                               "./__ShadowTestWithAltCoverRunner"
                                             StrongNameKey = keyfile
                                             InPlace = false
                                             Save = false }
      |> AltCoverFilter
      |> AltCover.Prepare
    { AltCover.Params.Create prep with ToolPath = altcover
                                       ToolType = AltCover.ToolType.Framework
                                       WorkingDirectory = shadowDir }
    |> AltCover.run

    let collect =
      { AltCover.CollectParams.Create() with Executable = nunit
                                             RecorderDirectory = shadowDir @@ "__ShadowTestWithAltCoverRunner" }
        .withCommandLine [ "--noheader"
                           "--work=."
                           "--result=./_Reports/ShadowTestWithAltCoverRunnerReport.xml"

                           Path.getFullName
                             "_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU/__ShadowTestWithAltCoverRunner/AltCover.Shadow.Tests.dll"

                           Path.getFullName
                             "_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU/__ShadowTestWithAltCoverRunner/AltCover.Shadow.Tests2.dll" ]
      |> AltCover.Collect
    { AltCover.Params.Create collect with ToolPath = altcover
                                          ToolType = AltCover.ToolType.Framework
                                          WorkingDirectory = "." }
    |> AltCover.run
    printfn "Instrument the GTK# visualizer tests"
    let gtkDir = Path.getFullName "_Binaries/AltCover.Tests.Visualizer/Debug+AnyCPU"
    let gtkReport = reports @@ "GTKVTestWithAltCoverRunner.xml"

    let prep =
      { AltCover.PrepareParams.Create() with XmlReport = gtkReport
                                             OutputDirectory =
                                               "./__GTKVTestWithAltCoverRunner"
                                             TypeFilter = [ "Gui" ]
                                             AssemblyFilter = [ "\\-sharp" ]
                                             StrongNameKey = keyfile
                                             InPlace = false
                                             Save = false }
      |> AltCoverFilter
      |> AltCover.Prepare
    { AltCover.Params.Create prep with ToolPath = altcover
                                       ToolType = AltCover.ToolType.Framework
                                       WorkingDirectory = gtkDir }
    |> AltCover.run

    printfn "Execute the the GTK# visualizer tests"
    let collect =
      { AltCover.CollectParams.Create() with Executable = nunit
                                             RecorderDirectory = gtkDir @@ "__GTKVTestWithAltCoverRunner" }
        .withCommandLine [ "--noheader"
                           "--work=."
                           "--result=./_Reports/GTKVTestWithAltCoverRunnerReport.xml"

                           Path.getFullName
                             "_Binaries/AltCover.Tests.Visualizer/Debug+AnyCPU/__GTKVTestWithAltCoverRunner/AltCover.Tests.Visualizer.dll" ]
      |> AltCover.Collect
    { AltCover.Params.Create collect with ToolPath = altcover
                                          ToolType = AltCover.ToolType.Framework
                                          WorkingDirectory = "." }
    |> AltCover.run

    let pester = Path.getFullName "_Reports/Pester.xml"
    ReportGenerator.generateReports (fun p ->
      { p with ExePath = Tools.findToolInSubPath "ReportGenerator.exe" "."
               ReportTypes =
                 [ ReportGenerator.ReportType.Html; ReportGenerator.ReportType.XmlSummary ]
               TargetDir = "_Reports/_UnitTestWithAltCoverRunner" })
      [ xaltReport; altReport; shadowReport; weakReport; pester ]

    let cover1 =
      altReport
      |> File.ReadAllLines
      |> Seq.takeWhile (fun l -> l <> "  </Modules>")

    let cover2 =
      shadowReport
      |> File.ReadAllLines
      |> Seq.skipWhile (fun l -> l.StartsWith("    <Module") |> not)
      |> Seq.takeWhile (fun l -> l <> "  </Modules>")

    let cover3 =
      weakReport
      |> File.ReadAllLines
      |> Seq.skipWhile (fun l -> l.StartsWith("    <Module") |> not)
      |> Seq.takeWhile (fun l -> l <> "  </Modules>")

    let cover3a =
      pester
      |> File.ReadAllLines
      |> Seq.skipWhile (fun l -> l.StartsWith("    <Module") |> not)
      |> Seq.takeWhile (fun l -> l <> "  </Modules>")

    let cover4 =
      xaltReport
      |> File.ReadAllLines
      |> Seq.skipWhile (fun l -> l.StartsWith("    <Module") |> not)

    let coverage = reports @@ "CombinedTestWithAltCoverRunner.coveralls"
    File.WriteAllLines
      (coverage, Seq.concat [ cover1; cover2; cover3; cover3a; cover4 ] |> Seq.toArray)
    if not <| String.IsNullOrWhiteSpace(Environment.environVar "APPVEYOR_BUILD_NUMBER") then
      Actions.Run
        (Tools.findToolInSubPath "coveralls.net.exe" nugetCache, "_Reports",
         [ "--opencover"; coverage ]) "Coveralls upload failed"
  else printfn "Symbols not present; skipping")

_Target "UnitTestWithAltCoverCore" // Obsolete
  (fun _ ->
  Directory.ensure "./_Reports/_UnitTestWithAltCover"
  let keyfile = Path.getFullName "Build/SelfTest.snk"
  let reports = Path.getFullName "./_Reports"
  let altcover = Tools.findToolInSubPath "AltCover.exe" "./_Binaries"
  let testDirectory =
    Path.getFullName "_Binaries/AltCover.Tests/Debug+AnyCPU/netcoreapp2.1"
  let output =
    Path.getFullName "Tests/_Binaries/AltCover.Tests/Debug+AnyCPU/netcoreapp2.1"
  let altReport = reports @@ "UnitTestWithAltCoverCore.xml"
  printfn "Instrumented the code"
  let prep =
    { AltCover.PrepareParams.Create() with XmlReport = altReport
                                           OutputDirectory = output
                                           StrongNameKey = keyfile
                                           OpenCover = false
                                           InPlace = false
                                           Save = false }
    |> AltCoverFilter
    |> AltCover.Prepare
  { AltCover.Params.Create prep with ToolPath = altcover
                                     ToolType = AltCover.ToolType.Framework
                                     WorkingDirectory = testDirectory }
  |> AltCover.run

  printfn "Unit test the instrumented code"
  try
    "altcover.tests.core.fsproj"
    |> DotNet.test (fun p ->
         { p with Configuration = DotNet.BuildConfiguration.Debug
                  NoBuild = true
                  Common =
                    { dotnetOptions p.Common with Verbosity =
                                                    Some DotNet.Verbosity.Minimal
                                                  WorkingDirectory =
                                                    Path.getFullName "Tests" }
                  MSBuildParams = cliArguments })
  with x ->
    printfn "%A" x
    reraise()

  printfn "Instrument the shadow tests"
  let shadowDir = "_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU/netcoreapp2.1"
  let shadowReport = reports @@ "ShadowTestWithAltCoverCore.xml"
  let shadowOut =
    Path.getFullName
      "Shadow.Tests/_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU/netcoreapp2.1"

  let prep =
    { AltCover.PrepareParams.Create() with XmlReport = shadowReport
                                           OutputDirectory = shadowOut
                                           StrongNameKey = keyfile
                                           OpenCover = false
                                           InPlace = false
                                           Save = false }
    |> AltCoverFilterG
    |> AltCover.Prepare
  { AltCover.Params.Create prep with ToolPath = altcover
                                     ToolType = AltCover.ToolType.Framework
                                     WorkingDirectory = shadowDir }
  |> AltCover.run

  printfn "Execute the shadow tests"
  "altcover.recorder.tests.core.fsproj"
  |> DotNet.test (fun p ->
       { p with Configuration = DotNet.BuildConfiguration.Debug
                NoBuild = true
                Common =
                  { dotnetOptions p.Common with Verbosity = Some DotNet.Verbosity.Minimal
                                                WorkingDirectory =
                                                  Path.getFullName "Shadow.Tests" }
                MSBuildParams = cliArguments })

  printfn "Instrument the XUnit tests"
  let xDir = "_Binaries/AltCover.XTests/Debug+AnyCPU/netcoreapp2.1"
  let xReport = reports @@ "XTestWithAltCoverCore.xml"
  let xOut =
    Path.getFullName "XTests/_Binaries/AltCover.XTests/Debug+AnyCPU/netcoreapp2.1"

  let prep =
    { AltCover.PrepareParams.Create() with XmlReport = xReport
                                           OutputDirectory = xOut
                                           StrongNameKey = keyfile
                                           OpenCover = false
                                           InPlace = false
                                           Save = false }
    |> AltCoverFilterG
    |> AltCover.Prepare
  { AltCover.Params.Create prep with ToolPath = altcover
                                     ToolType = AltCover.ToolType.Framework
                                     WorkingDirectory = xDir }
  |> AltCover.run

  printfn "Execute the XUnit tests"
  "altcover.x.tests.core.fsproj"
  |> DotNet.test (fun p ->
       { p with Configuration = DotNet.BuildConfiguration.Debug
                NoBuild = true
                Common =
                  { dotnetOptions p.Common with Verbosity = Some DotNet.Verbosity.Minimal
                                                WorkingDirectory =
                                                  Path.getFullName "XTests" }
                MSBuildParams = cliArguments })

  ReportGenerator.generateReports (fun p ->
    { p with ExePath = Tools.findToolInSubPath "ReportGenerator.exe" "."
             ReportTypes =
               [ ReportGenerator.ReportType.Html; ReportGenerator.ReportType.XmlSummary ]
             TargetDir = "_Reports/_UnitTestWithAltCoverCore" })
    [ altReport; shadowReport; xReport ])

_Target "UnitTestWithAltCoverCoreRunner" // Next target TODO modernization
  (fun _ ->
  Directory.ensure "./_Reports/_UnitTestWithAltCover"
  let reports = Path.getFullName "./_Reports"
  let altcover =
    Path.getFullName "./_Binaries/AltCover/Release+AnyCPU/netcoreapp2.0/AltCover.dll"
  let testDirectory =
    Path.getFullName "_Binaries/AltCover.Tests/Debug+AnyCPU/netcoreapp2.1"
  let output =
    Path.getFullName "Tests/_Binaries/AltCover.Tests/Debug+AnyCPU/netcoreapp2.1"

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
  let prep =
    { AltCover.PrepareParams.Create() with XmlReport = altReport
                                           OutputDirectory = output
                                           Single = true
                                           InPlace = false
                                           Save = false }
    |> AltCoverFilter
    |> AltCover.Prepare
  { AltCover.Params.Create prep with ToolPath = altcover
                                     ToolType = AltCover.ToolType.DotNet dotnetPath
                                     WorkingDirectory = testDirectory }
  |> AltCover.run
  printfn "Unit test the instrumented code"

  let testproject = Path.getFullName "./Tests/altcover.tests.core.fsproj"

  Actions.RunDotnet (fun o -> { dotnetOptions o with WorkingDirectory = output }) ""
    (altcover + " Runner -x \"dotnet\" -r \"" + output
     + "\" -- test --no-build --configuration Debug --verbosity normal " + testproject)
    "Unit test the instrumented code"

  printfn "Instrument the shadow tests"
  let shadowDir = "_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU/netcoreapp2.1"
  let shadowReport = reports @@ "ShadowTestWithAltCoverCoreRunner.xml"
  let shadowOut =
    Path.getFullName
      "Shadow.Tests/_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU/netcoreapp2.1"
  Shell.cleanDir shadowOut
  let prep =
    { AltCover.PrepareParams.Create() with XmlReport = shadowReport
                                           OutputDirectory = shadowOut
                                           Single = true
                                           InPlace = false
                                           Save = false }
    |> AltCoverFilter
    |> AltCover.Prepare
  { AltCover.Params.Create prep with ToolPath = altcover
                                     ToolType = AltCover.ToolType.DotNet dotnetPath
                                     WorkingDirectory = shadowDir }
  |> AltCover.run

  let shadowProject =
    Path.getFullName "./Shadow.Tests/altcover.recorder.tests.core.fsproj"
  Actions.RunDotnet (fun o -> { dotnetOptions o with WorkingDirectory = shadowOut }) ""
    (altcover + " Runner -x \"dotnet\" -r \"" + shadowOut
     + "\" -- test --no-build --configuration Debug --verbosity normal " + shadowProject)
    "Run the shadow tests"

  printfn "Instrument the XUnit tests"
  let xDir = "_Binaries/AltCover.XTests/Debug+AnyCPU/netcoreapp2.1"
  let xReport = reports @@ "XTestWithAltCoverCoreRunner.xml"
  let xOut =
    Path.getFullName "XTests/_Binaries/AltCover.XTests/Debug+AnyCPU/netcoreapp2.1"
  Shell.cleanDir xOut

  let prep =
    { AltCover.PrepareParams.Create() with XmlReport = xReport
                                           OutputDirectory = xOut
                                           Single = true
                                           InPlace = false
                                           Save = false }
    |> AltCoverFilter
    |> AltCover.Prepare
  { AltCover.Params.Create prep with ToolPath = altcover
                                     ToolType = AltCover.ToolType.DotNet dotnetPath
                                     WorkingDirectory = xDir }
  |> AltCover.run

  printfn "Execute the XUnit tests"
  let xProject = Path.getFullName "./XTests/altcover.x.tests.core.fsproj"
  Actions.RunDotnet (fun o -> { dotnetOptions o with WorkingDirectory = xOut }) ""
    (altcover + " Runner -x \"dotnet\" -r \"" + xOut
     + "\" -- test --no-build --configuration Debug --verbosity normal " + xProject)
    "Run the shadow tests"

  ReportGenerator.generateReports (fun p ->
    { p with ExePath = Tools.findToolInSubPath "ReportGenerator.exe" "."
             ReportTypes =
               [ ReportGenerator.ReportType.Html; ReportGenerator.ReportType.XmlSummary ]
             TargetDir = "_Reports/_UnitTestWithAltCoverCoreRunner" })
    [ altReport; shadowReport; xReport ])

// Pure OperationalTests

_Target "OperationalTest" ignore

_Target "FSharpTypes" (fun _ ->
  Directory.ensure "./_Reports"
  let simpleReport = (Path.getFullName "./_Reports") @@ ("AltCoverFSharpTypes.xml")
  let binRoot = Path.getFullName "_Binaries/AltCover/Release+AnyCPU"
  let sampleRoot = Path.getFullName "_Binaries/Sample2/Debug+AnyCPU"
  let instrumented = "__FSharpTypes"
  if sampleRoot @@ "Sample2.pdb" |> File.Exists then
    Actions.Run (binRoot @@ "AltCover.exe", sampleRoot,
                 [ "-s=Adapter"
                   "-s=nunit"
                   "-t=System\\."
                   "-t=Microsoft\\."
                   "-x=" + simpleReport
                   "/o=./" + instrumented ]) "FSharpTypes"
    Actions.ValidateFSharpTypes simpleReport []
  else printfn "Symbols not present; skipping")

_Target "FSharpTypesDotNet" (fun _ ->
  Directory.ensure "./_Reports"
  let altcover =
    Path.getFullName "./_Binaries/AltCover/Release+AnyCPU/netcoreapp2.0/AltCover.dll"
  let simpleReport = (Path.getFullName "./_Reports") @@ ("AltCoverFSharpTypesDotNet.xml")
  let sampleRoot = Path.getFullName "Sample2/_Binaries/Sample2/Debug+AnyCPU/netcoreapp2.1"

  // Test the --inplace operation
  Shell.cleanDir sampleRoot
  Actions.RunDotnet
    (fun o -> { dotnetOptions o with WorkingDirectory = Path.getFullName "Sample2" })
    "test" ("-v m --configuration Debug sample2.core.fsproj")
    "sample initial test returned with a non-zero exit code"

  Actions.RunDotnet (fun o -> { dotnetOptions o with WorkingDirectory = sampleRoot }) ""
    (altcover + " --inplace -s=Adapter -t \"System\\.\" -t \"Microsoft\\.\" -x \""
     + simpleReport + "\" ") "FSharpTypesDotNet"
  Actions.ValidateFSharpTypes simpleReport [ "main" ]
  Assert.That(Path.Combine(sampleRoot, "__Saved") |> Directory.Exists)

  printfn "Execute the instrumented tests"
  Actions.RunDotnet
    (fun o -> { dotnetOptions o with WorkingDirectory = Path.getFullName "Sample2" })
    "test" ("-v m --no-build --configuration Debug sample2.core.fsproj")
    "sample coverage test returned with a non-zero exit code"
  Actions.ValidateFSharpTypesCoverage simpleReport)

_Target "FSharpTests"
  (fun _ ->
  Directory.ensure "./_Reports"
  let altcover =
    Path.getFullName "./_Binaries/AltCover/Release+AnyCPU/netcoreapp2.0/AltCover.dll"
  let simpleReport = (Path.getFullName "./_Reports") @@ ("AltCoverFSharpTests.xml")
  let sampleRoot = Path.getFullName "Sample7/_Binaries/Sample7/Debug+AnyCPU/netcoreapp2.0"

  // Test the --inplace operation
  Shell.cleanDir sampleRoot
  Actions.RunDotnet
    (fun o -> { dotnetOptions o with WorkingDirectory = Path.getFullName "Sample7" })
    "test" ("-v m --configuration Debug sample7.core.fsproj")
    "sample initial test returned with a non-zero exit code"

  // inplace instrument
  Actions.RunDotnet (fun o -> { dotnetOptions o with WorkingDirectory = sampleRoot }) ""
    (altcover
     + " --opencover --inplace -c=[Test] -s=Adapter -t \"System\\.\" -t \"Microsoft\\.\" -x \""
     + simpleReport + "\" ") "FSharpTypesDotNet"
  printfn "Execute the instrumented tests"
  Actions.RunDotnet
    (fun o -> { dotnetOptions o with WorkingDirectory = Path.getFullName "Sample7" })
    "test" ("-v m --no-build --configuration Debug sample7.core.fsproj")
    "sample coverage test returned with a non-zero exit code")

_Target "FSharpTypesDotNetRunner" (fun _ ->
  Directory.ensure "./_Reports"
  let altcover =
    Path.getFullName "./_Binaries/AltCover/Release+AnyCPU/netcoreapp2.0/AltCover.dll"
  let simpleReport =
    (Path.getFullName "./_Reports") @@ ("AltCoverFSharpTypesDotNetRunner.xml")
  let sampleRoot = Path.getFullName "_Binaries/Sample2/Debug+AnyCPU/netcoreapp2.1"
  let instrumented =
    Path.getFullName "Sample2/_Binaries/Sample2/Debug+AnyCPU/netcoreapp2.1"

  // Instrument the code
  Actions.RunDotnet (fun o -> { dotnetOptions o with WorkingDirectory = sampleRoot }) ""
    (altcover + " -s=Adapter -t \"System\\.\" -t \"Microsoft\\.\" -x \"" + simpleReport
     + "\" /o \"" + instrumented + "\"") "FSharpTypesDotNetRunner"

  Actions.ValidateFSharpTypes simpleReport [ "main" ]

  printfn "Execute the instrumented tests"
  let sample2 = Path.getFullName "./Sample2/sample2.core.fsproj"
  Actions.RunDotnet (fun o -> { dotnetOptions o with WorkingDirectory = instrumented }) ""
    (altcover + " Runner -x \"dotnet\" -r \"" + instrumented
     + "\" -- test --no-build --configuration Debug " + sample2)
    "Execute the instrumented tests"

  Actions.ValidateFSharpTypesCoverage simpleReport)

_Target "FSharpTypesDotNetCollecter" (fun _ ->
  Directory.ensure "./_Reports"
  let altcover =
    Path.getFullName "./_Binaries/AltCover/Release+AnyCPU/netcoreapp2.0/AltCover.dll"
  let simpleReport =
    (Path.getFullName "./_Reports") @@ ("AltCoverFSharpTypesDotNetCollecter.xml")
  let sampleRoot = Path.getFullName "Sample2/_Binaries/Sample2/Debug+AnyCPU/netcoreapp2.1"

  // Test the --inplace operation
  Shell.cleanDir sampleRoot
  Actions.RunDotnet
    (fun o -> { dotnetOptions o with WorkingDirectory = Path.getFullName "Sample2" })
    "test" ("--configuration Debug sample2.core.fsproj")
    "sample initial test returned with a non-zero exit code"

  // inplace instrument and save
  Actions.RunDotnet (fun o -> { dotnetOptions o with WorkingDirectory = sampleRoot }) ""
    (altcover + " --inplace --save -s=Adapter -t \"System\\.\" -t \"Microsoft\\.\" -x \""
     + simpleReport + "\" ") "FSharpTypesDotNetCollecter"
  Actions.ValidateFSharpTypes simpleReport [ "main" ]
  Assert.That(Path.Combine(sampleRoot, "__Saved") |> Directory.Exists)

  printfn "Execute the instrumented tests"
  Actions.RunDotnet
    (fun o -> { dotnetOptions o with WorkingDirectory = Path.getFullName "Sample2" })
    "test" ("-v m --no-build --configuration Debug sample2.core.fsproj")
    "sample coverage test returned with a non-zero exit code"

  Actions.RunDotnet (fun o -> { dotnetOptions o with WorkingDirectory = sampleRoot }) ""
    (altcover + " Runner --collect -r \"" + sampleRoot + "\"")
    "Collect the instrumented test output"
  Actions.ValidateFSharpTypesCoverage simpleReport)

_Target "BasicCSharp"
  (fun _ ->
  Actions.SimpleInstrumentingRun "_Binaries/Sample1/Debug+AnyCPU"
    "_Binaries/AltCover/Debug+AnyCPU" "BasicCSharp")

_Target "BasicCSharpMono"
  (fun _ ->
  Actions.SimpleInstrumentingRun "_Mono/Sample1" "_Binaries/AltCover/Debug+AnyCPU"
    "BasicCSharpMono")

_Target "BasicCSharpUnderMono"
  (fun _ ->
  monoOnWindows
  |> Actions.SimpleInstrumentingRunUnderMono "_Binaries/Sample1/Debug+AnyCPU"
       "_Binaries/AltCover/Debug+AnyCPU" "BasicCSharpUnderMono")

_Target "BasicCSharpMonoUnderMono"
  (fun _ ->
  monoOnWindows
  |> Actions.SimpleInstrumentingRunUnderMono "_Mono/Sample1"
       "_Binaries/AltCover/Debug+AnyCPU" "BasicCSharpMono")

_Target "CSharpMonoWithDotNet" (fun _ ->
  Directory.ensure "./_Reports"
  let x = Path.getFullName "./_Reports/CSharpMonoWithDotNet.xml"
  let o = Path.getFullName "./_Mono/__Instrumented.CSharpMonoWithDotNet"
  let i = Path.getFullName "./_Mono/Sample1"
  let altcover =
    Path.getFullName "./_Binaries/AltCover/Release+AnyCPU/netcoreapp2.0/AltCover.dll"

  Actions.RunDotnet dotnetOptions ""
    (altcover + " -t \"System.\" -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")
    "CSharpMonoWithDotNet"

  Actions.Run (o @@ "/Sample1.exe", o, []) "Instrumented .exe failed"
  Actions.ValidateSample1 "./_Reports/CSharpMonoWithDotNet.xml" "CSharpMonoWithDotNet")

_Target "CSharpDotNetWithDotNet"
  (fun _ ->
  Directory.ensure "./_Reports"
  let x = Path.getFullName "./_Reports/CSharpDotNetWithDotNet.xml"
  let o = Path.getFullName "./_Binaries/Sample1/__Instrumented.CSharpDotNetWithDotNet"
  let i = Path.getFullName "./_Binaries/Sample1/Debug+AnyCPU/netcoreapp2.0"

  Actions.RunDotnet dotnetOptions
    "_Binaries/AltCover/Debug+AnyCPU/netcoreapp2.0/AltCover.dll"
    (" -t \"System.\" -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")
    "CSharpDotNetWithDotNet"
  Actions.RunDotnet dotnetOptions (o @@ "Sample1.dll") "" "CSharpDotNetWithDotNet test"
  Actions.ValidateSample1 "./_Reports/CSharpDotNetWithDotNet.xml" "CSharpDotNetWithDotNet")

_Target "CSharpDotNetWithFramework"
  (fun _ ->
  Directory.ensure "./_Reports"
  let simpleReport = (Path.getFullName "./_Reports") @@ ("CSharpDotNetWithFramework.xml")
  let binRoot = Path.getFullName "_Binaries/AltCover/Release+AnyCPU"
  let sampleRoot = Path.getFullName "_Binaries/Sample1/Debug+AnyCPU/netcoreapp2.0"
  let instrumented =
    Path.getFullName "_Binaries/Sample1/__Instrumented.CSharpDotNetWithFramework"

  Actions.Run (binRoot @@ "AltCover.exe", sampleRoot,
               [ "-t=System\\."
                 "-t=Microsoft\\."
                 "-x=" + simpleReport
                 "/o=" + instrumented ]) "CSharpDotNetWithFramework"

  Actions.RunDotnet dotnetOptions (instrumented @@ "Sample1.dll") ""
    "CSharpDotNetWithFramework test"
  Actions.ValidateSample1 "./_Reports/CSharpDotNetWithFramework.xml"
    "CSharpDotNetWithFramework")

_Target "SelfTest" (fun _ ->
  Directory.ensure "./_Reports/_Instrumented"
  let targetDir = "_Binaries/AltCover.Tests/Debug+AnyCPU"
  let reports = Path.getFullName "./_Reports"
  let report = reports @@ "OpenCoverSelfTest.xml"
  let altReport = reports @@ "AltCoverSelfTest.xml"
  let keyfile = Path.getFullName "Build/SelfTest.snk"

  printfn "Self-instrument under OpenCover"
  let prep =
    { AltCover.PrepareParams.Create() with XmlReport = altReport
                                           OutputDirectory = "__SelfTest"
                                           StrongNameKey = keyfile
                                           OpenCover = false
                                           InPlace = false
                                           Save = false }
    |> AltCover.Prepare

  let args =
    ({ AltCover.Params.Create prep with ToolPath = String.Empty
                                        ToolType = AltCover.ToolType.Global
                                        WorkingDirectory = "." }
     |> AltCover.composeCommandLine).CommandLine

  OpenCover.run (fun p ->
    { p with WorkingDir = targetDir
             ExePath = Tools.findToolInSubPath "OpenCover.Console.exe" "."
             TestRunnerExePath = Tools.findToolInSubPath "AltCover.exe" targetDir
             Filter = OpenCoverFilter
             MergeByHash = true
             OptionalArguments =
               "-excludebyattribute:*ExcludeFromCodeCoverageAttribute;*ProgIdAttribute"
             Register = OpenCover.RegisterType.RegisterUser
             Output = report }) args

  ReportGenerator.generateReports (fun p ->
    { p with ExePath = Tools.findToolInSubPath "ReportGenerator.exe" "."
             TargetDir = "_Reports/_OpenCoverSelfTest" }) [ report ]

  printfn "Re-instrument everything"
  let altReport2 = reports @@ "AltCoverSelfTestDummy.xml"

  let prep =
    { AltCover.PrepareParams.Create() with XmlReport = altReport2
                                           OutputDirectory = "./__SelfTestDummy"
                                           StrongNameKey = keyfile
                                           OpenCover = false
                                           InPlace = false
                                           Save = false }
    |> AltCoverFilter
    |> AltCover.Prepare
  { AltCover.Params.Create prep with ToolPath =
                                       "_Binaries/AltCover.Tests/Debug+AnyCPU/__SelfTest/AltCover.exe"
                                     ToolType = AltCover.ToolType.Framework
                                     WorkingDirectory =
                                       "_Binaries/AltCover.Tests/Debug+AnyCPU" }
  |> AltCover.run

  ReportGenerator.generateReports (fun p ->
    { p with ExePath = Tools.findToolInSubPath "ReportGenerator.exe" "."
             TargetDir = "_Reports/_AltCoverSelfTest" }) [ altReport ])

_Target "RecordResumeTest"
  (fun _ ->
  Directory.ensure "./_Reports"
  let simpleReport = (Path.getFullName "./_Reports") @@ ("RecordResumeTest.xml")
  let binRoot = Path.getFullName "_Binaries/AltCover/Release+AnyCPU"
  let sampleRoot = Path.getFullName "_Binaries/Sample8/Debug+AnyCPU"
  let instrumented = "__RecordResumeTest"

  Actions.Run (binRoot @@ "AltCover.exe", sampleRoot,
               [ "-s=Adapter"
                 "-s=nunit"
                 "-t=System\\."
                 "-t=Microsoft\\."
                 "-x=" + simpleReport
                 "/o=./" + instrumented ]) "RecordResumeTest 1"
  let testing = (sampleRoot @@ instrumented) @@ "Sample8.exe"
  Actions.Run (testing, sampleRoot, [ simpleReport + ".acv" ]) "RecordResumeTest 2"
  do use coverageFile =
       new FileStream(simpleReport, FileMode.Open, FileAccess.Read, FileShare.None, 4096,
                      FileOptions.SequentialScan)
     let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))

     let recorded =
       coverageDocument.Descendants(XName.Get("seqpnt"))
       |> Seq.map (fun x -> x.Attribute(XName.Get("visitcount")).Value)
       |> Seq.toList

     let expected = Array.create 20 "0"
     Assert.That
       (recorded, expected |> Is.EquivalentTo,
        sprintf "Bad visit list %A -- should be empty now" recorded)

  Actions.Run (binRoot @@ "AltCover.exe", sampleRoot,
               [ "runner"
                 "--collect"
                 "/r=./" + instrumented ]) "RecordResumeTest 3"

  do use coverageFile =
       new FileStream(simpleReport, FileMode.Open, FileAccess.Read, FileShare.None, 4096,
                      FileOptions.SequentialScan)
     let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))

     let recorded =
       coverageDocument.Descendants(XName.Get("seqpnt"))
       |> Seq.map (fun x -> x.Attribute(XName.Get("visitcount")).Value)
       |> Seq.toList

     let expected = Array.create 20 "0"
     Assert.That
       (recorded, expected |> Is.Not.EquivalentTo,
        sprintf "Bad visit list %A -- should no longer be empty now" recorded)
     Assert.That
       (recorded |> Seq.length, Is.EqualTo 20,
        sprintf "Bad visit list %A -- should no longer be empty now" recorded))

_Target "RecordResumeTrackingTest" (fun _ ->
  Directory.ensure "./_Reports"
  let simpleReport = (Path.getFullName "./_Reports") @@ ("RecordResumeTrackingTest.xml")
  let binRoot = Path.getFullName "_Binaries/AltCover/Release+AnyCPU"
  let sampleRoot = Path.getFullName "_Binaries/Sample8/Debug+AnyCPU"
  let instrumented = "__RecordResumeTrackingTest"

  Actions.Run (binRoot @@ "AltCover.exe", sampleRoot,
               [ "--opencover"
                 "-c=Main"
                 "-s=Adapter"
                 "-s=nunit"
                 "-t=System\\."
                 "-t=Microsoft\\."
                 "-x=" + simpleReport
                 "/o=./" + instrumented ]) "RecordResumeTrackingTest 1"
  let testing = (sampleRoot @@ instrumented) @@ "Sample8.exe"
  Actions.Run (testing, sampleRoot, [ simpleReport + ".acv" ])
    "RecordResumeTrackingTest 2"
  do use coverageFile =
       new FileStream(simpleReport, FileMode.Open, FileAccess.Read, FileShare.None, 4096,
                      FileOptions.SequentialScan)
     let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))

     let recorded =
       coverageDocument.Descendants(XName.Get("SequencePoint"))
       |> Seq.map (fun x -> x.Attribute(XName.Get("vc")).Value)
       |> Seq.toList

     let expected = Array.create 20 "0"
     Assert.That
       (recorded, expected |> Is.EquivalentTo,
        sprintf "Bad visit list %A -- should be empty now" recorded)

  Actions.Run (binRoot @@ "AltCover.exe", sampleRoot,
               [ "runner"
                 "--collect"
                 "/r=./" + instrumented ]) "RecordResumeTrackingTest 3"

  do use coverageFile =
       new FileStream(simpleReport, FileMode.Open, FileAccess.Read, FileShare.None, 4096,
                      FileOptions.SequentialScan)
     let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))

     let recorded =
       coverageDocument.Descendants(XName.Get("SequencePoint"))
       |> Seq.map (fun x -> x.Attribute(XName.Get("vc")).Value)
       |> Seq.toList

     let expected = Array.create 20 "0"
     Assert.That
       (recorded, expected |> Is.Not.EquivalentTo,
        sprintf "Bad visit list %A -- should no longer be empty now" recorded)
     Assert.That
       (recorded |> Seq.length, Is.EqualTo 20,
        sprintf "Bad visit list %A -- should no longer be empty now" recorded)
     let tracked =
       coverageDocument.Descendants(XName.Get("TrackedMethodRef")) |> Seq.toList
     Assert.That(tracked, Is.Not.Empty))

_Target "RecordResumeTestDotNet"
  (fun _ ->
  Directory.ensure "./_Reports"
  let simpleReport = (Path.getFullName "./_Reports") @@ ("RecordResumeTestDotNet.xml")
  let binRoot = Path.getFullName "_Binaries/AltCover/Release+AnyCPU"
  let sampleRoot = Path.getFullName "_Binaries/Sample8/Debug+AnyCPU/netcoreapp2.0"
  let instrumented = "__RecordResumeTestDotNet"

  Actions.Run (binRoot @@ "AltCover.exe", sampleRoot,
               [ "-s=Adapter"
                 "-s=nunit"
                 "-t=System\\."
                 "-t=Microsoft\\."
                 "-x=" + simpleReport
                 "/o=./" + instrumented ]) "RecordResumeTestDotNet 1"

  let testing = (sampleRoot @@ instrumented) @@ "Sample8.dll"
  Actions.RunDotnet (fun o -> { dotnetOptions o with WorkingDirectory = sampleRoot })
    (testing + " " + simpleReport + ".acv") "" "RecordResumeTestDotNet 2"

  do use coverageFile =
       new FileStream(simpleReport, FileMode.Open, FileAccess.Read, FileShare.None, 4096,
                      FileOptions.SequentialScan)
     let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))

     let recorded =
       coverageDocument.Descendants(XName.Get("seqpnt"))
       |> Seq.map (fun x -> x.Attribute(XName.Get("visitcount")).Value)
       |> Seq.toList

     let expected = Array.create 20 "0"
     Assert.That
       (recorded, expected |> Is.EquivalentTo,
        sprintf "Bad visit list %A -- should be empty now" recorded)

  Actions.Run (binRoot @@ "AltCover.exe", sampleRoot,
               [ "runner"
                 "--collect"
                 "/r=./" + instrumented ]) "RecordResumeTestDotNet 3"

  do use coverageFile =
       new FileStream(simpleReport, FileMode.Open, FileAccess.Read, FileShare.None, 4096,
                      FileOptions.SequentialScan)
     let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))

     let recorded =
       coverageDocument.Descendants(XName.Get("seqpnt"))
       |> Seq.map (fun x -> x.Attribute(XName.Get("visitcount")).Value)
       |> Seq.toList

     let expected = Array.create 20 "0"
     Assert.That
       (recorded, expected |> Is.Not.EquivalentTo,
        sprintf "Bad visit list %A -- should no longer be empty now" recorded)
     Assert.That
       (recorded |> Seq.length, Is.EqualTo 20,
        sprintf "Bad visit list %A -- should no longer be empty now" recorded))

_Target "RecordResumeTestUnderMono"
  (fun _ ->
  Directory.ensure "./_Reports"
  let simpleReport = (Path.getFullName "./_Reports") @@ ("RecordResumeTestUnderMono.xml")
  let binRoot = Path.getFullName "_Binaries/AltCover/Release+AnyCPU"
  let sampleRoot = Path.getFullName "_Binaries/Sample8/Debug+AnyCPU"
  let instrumented = "__RecordResumeTestUnderMono"

  Actions.Run (binRoot @@ "AltCover.exe", sampleRoot,
               [ "-s=Adapter"
                 "-s=nunit"
                 "-t=System\\."
                 "-t=Microsoft\\."
                 "-x=" + simpleReport
                 "/o=./" + instrumented ]) "RecordResumeTestUnderMono 1"

  match monoOnWindows with
  | Some mono ->
    let testing = (sampleRoot @@ instrumented) @@ "Sample8.exe"

    let r =
      CreateProcess.fromRawCommand mono [ testing
                                          simpleReport + ".acv" ]
      |> CreateProcess.withWorkingDirectory sampleRoot
      |> Proc.run
    Assert.That(r.ExitCode, Is.EqualTo 0, "RecordResumeTestUnderMono 2")
  | None -> ()

  do use coverageFile =
       new FileStream(simpleReport, FileMode.Open, FileAccess.Read, FileShare.None, 4096,
                      FileOptions.SequentialScan)
     let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))

     let recorded =
       coverageDocument.Descendants(XName.Get("seqpnt"))
       |> Seq.map (fun x -> x.Attribute(XName.Get("visitcount")).Value)
       |> Seq.toList

     let expected = Array.create 20 "0"
     Assert.That
       (recorded, expected |> Is.EquivalentTo,
        sprintf "Bad visit list %A -- should be empty now" recorded)

  Actions.Run (binRoot @@ "AltCover.exe", sampleRoot,
               [ "runner"
                 "--collect"
                 "/r=./" + instrumented ]) "RecordResumeTestUnderMono 3"

  do use coverageFile =
       new FileStream(simpleReport, FileMode.Open, FileAccess.Read, FileShare.None, 4096,
                      FileOptions.SequentialScan)
     let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))

     let recorded =
       coverageDocument.Descendants(XName.Get("seqpnt"))
       |> Seq.map (fun x -> x.Attribute(XName.Get("visitcount")).Value)
       |> Seq.toList

     let expected = Array.create 20 "0"
     Assert.That
       (recorded, expected |> Is.Not.EquivalentTo,
        sprintf "Bad visit list %A -- should no longer be empty now" recorded)
     Assert.That
       (recorded |> Seq.length, Is.EqualTo 20,
        sprintf "Bad visit list %A -- should no longer be empty now" recorded))

// Packaging

_Target "Packaging" (fun _ ->
  let AltCover = Path.getFullName "_Binaries/AltCover/AltCover.exe"
  let fscore = Path.getFullName "_Binaries/AltCover/Release+AnyCPU/FSharp.Core.dll"
  let options = Path.getFullName "_Binaries/AltCover/Release+AnyCPU/Mono.Options.dll"
  let recorder =
    Path.getFullName "_Binaries/AltCover/Release+AnyCPU/AltCover.Recorder.dll"
  let posh =
    Path.getFullName
      "_Binaries/AltCover.PowerShell/Release+AnyCPU/AltCover.PowerShell.dll"
  let csapi =
    Path.getFullName "_Binaries/AltCover.CSApi/Release+AnyCPU/AltCover.CSApi.dll"
  let fsapi =
    Path.getFullName "_Binaries/AltCover.FSApi/Release+AnyCPU/AltCover.FSApi.dll"
  let cake = Path.getFullName "_Binaries/AltCover.Cake/Release+AnyCPU/AltCover.Cake.dll"
  let fake = Path.getFullName "_Binaries/AltCover.Fake/Release+AnyCPU/AltCover.Fake.dll"
  let fake2 =
    Path.getFullName
      "_Binaries/AltCover.Fake.DotNet.Testing.AltCover/Release+AnyCPU/AltCover.Fake.DotNet.Testing.AltCover.dll"
  let vis =
    Path.getFullName
      "_Binaries/AltCover.Visualizer/Release+AnyCPU/AltCover.Visualizer.exe"
  let packable = Path.getFullName "./_Binaries/README.html"

  let applicationFiles =
    if File.Exists AltCover then
      [ (AltCover, Some "tools/net45", None)
        (recorder, Some "tools/net45", None)
        (posh, Some "tools/net45", None)
        (fsapi, Some "tools/net45", None)
        (vis, Some "tools/net45", None)
        (fake2, Some "lib/net45", None)
        (fscore, Some "tools/net45", None)
        (options, Some "tools/net45", None)
        (packable, Some "", None) ]
    else []

  let apiFiles =
    if File.Exists AltCover then
      [ (AltCover, Some "lib/net45", None)
        (recorder, Some "lib/net45", None)
        (posh, Some "lib/net45", None)
        (fsapi, Some "lib/net45", None)
        (csapi, Some "lib/net45", None)
        (cake, Some "lib/net45", None)
        (fake, Some "lib/net45", None)
        (fake2, Some "lib/net45", None)
        (fscore, Some "lib/net45", None)
        (options, Some "lib/net45", None)
        (packable, Some "", None) ]
    else []

  let resourceFiles path =
    if File.Exists AltCover then
      [ "_Binaries/AltCover/Release+AnyCPU";
        "_Binaries/AltCover.Visualizer/Release+AnyCPU" ]
      |> List.map (fun f ->
           Directory.GetDirectories(Path.getFullName f)
           |> Seq.map (fun d -> Directory.GetFiles(d, "*.resources.dll"))
           |> Seq.concat)
      |> Seq.concat
      |> Seq.map
           (fun x -> (x, Some(path + Path.GetFileName(Path.GetDirectoryName(x))), None))
      |> Seq.distinctBy (fun (x, y, _) -> (Option.get y) + "/" + (Path.GetFileName x))
      |> Seq.toList
    else []

  let nupkg = (Path.getFullName "./nupkg").Length

  let otherFiles =
    (!!"./nupkg/**/*.*")
    |> Seq.map
         (fun x ->
         (x, Some(Path.GetDirectoryName(x).Substring(nupkg).Replace("\\", "/")), None))
    |> Seq.toList

  Directory.ensure "./_Intermediate/dotnet"
  let otherFilesDotnet =
    otherFiles
    |> List.map (fun (a, b, c) ->
         let text =
           File.ReadAllText(a).Replace("tools/netcoreapp2.0", "lib/netcoreapp2.0")
         let name =
           (Path.getFullName "./_Intermediate/dotnet")
           @@ ("altcover.dotnet" + Path.GetExtension a)
         File.WriteAllText(name, text)
         (name, b, c))

  Directory.ensure "./_Intermediate/global"
  let otherFilesGlobal =
    otherFiles
    |> List.map (fun (a, b, c) ->
         let text =
           File.ReadAllText(a).Replace("tools/netcoreapp2.0", "tools/netcoreapp2.1/any")
         let name =
           (Path.getFullName "./_Intermediate/global")
           @@ ("altcover.global" + Path.GetExtension a)
         File.WriteAllText(name, text)
         (name, b, c))

  Directory.ensure "./_Intermediate/api"
  let otherFilesApi =
    otherFiles
    |> List.map (fun (a, b, c) ->
         let text =
           File.ReadAllText(a).Replace("tools/netcoreapp2.0", "lib/netstandard2.0")
         let name =
           (Path.getFullName "./_Intermediate/api")
           @@ ("altcover.api" + Path.GetExtension a)
         File.WriteAllText(name, text)
         (name, b, c))

  let poshFiles where =
    [ (!!"./_Binaries/AltCover.PowerShell/Release+AnyCPU/netstandard2.0/*.PowerShell.*")
      (!!"./_Binaries/AltCover.FSApi/Release+AnyCPU/netstandard2.0/*.FSApi.*") ]
    |> Seq.concat
    |> Seq.map (fun x -> (x, Some(where + Path.GetFileName x), None))
    |> Seq.toList

  let cakeFiles where =
    (!!"./_Binaries/AltCover.Cake/Release+AnyCPU/netstandard2.0/AltCover.C*.*")
    |> Seq.map (fun x -> (x, Some(where + Path.GetFileName x), None))
    |> Seq.toList

  let fakeFiles where =
    (!!"./_Binaries/AltCover.Fake/Release+AnyCPU/netstandard2.0/AltCover.Fak*.*")
    |> Seq.map (fun x -> (x, Some(where + Path.GetFileName x), None))
    |> Seq.toList

  let fake2Files where =
    (!!"./_Binaries/AltCover.Fake.DotNet.Testing.AltCover/Release+AnyCPU/netstandard2.0/AltCover.Fake.DotNet.*")
    |> Seq.map (fun x -> (x, Some(where + Path.GetFileName x), None))
    |> Seq.toList

  let publish = (Path.getFullName "./_Publish").Length

  let netcoreFiles where =
    (!!"./_Publish/**/*.*")
    |> Seq.map
         (fun x ->
         (x, Some(where + Path.GetDirectoryName(x).Substring(publish).Replace("\\", "/")),
          None))
    |> Seq.toList

  let publishapi = (Path.getFullName "./_Publish.api").Length

  let netstdFiles where =
    (!!"./_Publish.api/**/*.*")
    |> Seq.filter
         (fun f ->
         let n = f |> Path.GetFileName
         n.StartsWith("altcover.", StringComparison.OrdinalIgnoreCase)
         || n.StartsWith("Mono.", StringComparison.Ordinal)
         || n.StartsWith("FSharp.Core.", StringComparison.Ordinal))
    |> Seq.map
         (fun x ->
         (x,
          Some(where + Path.GetDirectoryName(x).Substring(publishapi).Replace("\\", "/")),
          None))
    |> Seq.toList

  let dotnetFiles =
    (!!"./_Binaries/dotnet-altcover/Release+AnyCPU/netcoreapp2.0/dotnet-altcover.*")
    |> Seq.map (fun x -> (x, Some("lib/netcoreapp2.0/" + Path.GetFileName x), None))
    |> Seq.toList

  let globalFiles =
    (!!"./_Binaries/global-altcover/Release+AnyCPU/netcoreapp2.1/global-altcover.*")
    |> Seq.map (fun x -> (x, Some("tools/netcoreapp2.1/any/" + Path.GetFileName x), None))
    |> Seq.toList

  let publishV = (Path.getFullName "./_Publish.visualizer").Length

  let vizFiles where =
    (!!"./_Publish.visualizer/**/*.*")
    |> Seq.map
         (fun x ->
         (x, Some(where + Path.GetDirectoryName(x).Substring(publishV).Replace("\\", "/")),
          None))
    |> Seq.toList

  let auxVFiles =
    (!!"./_Binaries/AltCover.Visualizer/Release+AnyCPU/netcoreapp2.1/*.xml")
    |> Seq.map (fun x -> (x, Some("tools/netcoreapp2.1/any/" + Path.GetFileName x), None))
    |> Seq.toList

  let auxFiles =
    (!!"./_Binaries/global-altcover/Release+AnyCPU/netcoreapp2.1/*.xml")
    |> Seq.map (fun x -> (x, Some("tools/netcoreapp2.1/any/" + Path.GetFileName x), None))
    |> Seq.toList

  printfn "Executing on %A" Environment.OSVersion
  [ (List.concat [ applicationFiles
                   resourceFiles "tools/net45/"
                   netcoreFiles "tools/netcoreapp2.0/"
                   fake2Files "lib/netstandard2.0/"
                   poshFiles "tools/netcoreapp2.0/"
                   vizFiles "tools/netcoreapp2.1"
                   otherFiles ], "_Packaging", "./Build/AltCover.nuspec", "altcover")

    (List.concat [ apiFiles
                   resourceFiles "lib/net45/"
                   netstdFiles "lib/netstandard2.0"
                   cakeFiles "lib/netstandard2.0/"
                   fakeFiles "lib/netstandard2.0/"
                   fake2Files "lib/netstandard2.0/"
                   poshFiles "lib/netstandard2.0/"
                   vizFiles "tools/netcoreapp2.1"
                   otherFilesApi ], "_Packaging.api", "./_Generated/altcover.api.nuspec",
     "altcover.api")

    (List.concat [ netcoreFiles "lib/netcoreapp2.0"
                   poshFiles "lib/netcoreapp2.0/"
                   fake2Files "lib/netstandard2.0/"
                   dotnetFiles
                   otherFilesDotnet ], "_Packaging.dotnet",
     "./_Generated/altcover.dotnet.nuspec", "altcover.dotnet")

    (List.concat [ globalFiles
                   netcoreFiles "tools/netcoreapp2.1/any"
                   poshFiles "tools/netcoreapp2.1/any/"
                   // fake2Files "lib/netstandard2.0/"  -- API like this is incompatible
                   auxFiles
                   otherFilesGlobal ], "_Packaging.global",
     "./_Generated/altcover.global.nuspec", "altcover.global")

    (List.concat [ vizFiles "tools/netcoreapp2.1/any"
                   auxVFiles ], "_Packaging.visualizer",
     "./_Generated/altcover.visualizer.nuspec", "altcover.visualizer") ]
  |> List.iter (fun (files, output, nuspec, project) ->
       let outputPath = "./" + output
       let workingDir = "./_Binaries/" + output
       Directory.ensure workingDir
       Directory.ensure outputPath

       NuGet (fun p ->
         { p with Authors = [ "Steve Gilham" ]
                  Project = project
                  Description =
                    "A cross-platform pre-instrumenting code coverage tool set for .net/.net core and Mono"
                  OutputPath = outputPath
                  WorkingDir = workingDir
                  Files = files
                  Version = !Version
                  Copyright = (!Copyright).Replace("©", "(c)")
                  Publish = false
                  ReleaseNotes = Path.getFullName "ReleaseNotes.md" |> File.ReadAllText
                  ToolPath =
                    if Environment.isWindows then
                      Tools.findToolInSubPath "NuGet.exe" "./packages"
                    else "/usr/bin/nuget" }) nuspec))

_Target "PrepareFrameworkBuild"
  (fun _ ->
  let toolpath = Tools.findToolInSubPath "ILMerge.exe" "./packages"
  let ver = String.Join(".", (!Version).Split('.') |> Seq.take 2) + ".0.0"
  let here = Directory.GetCurrentDirectory()
  ILMerge.run
    { ILMerge.Params.Create() with DebugInfo = true
                                   ToolPath = toolpath
                                   TargetKind = ILMerge.TargetKind.Exe
                                   KeyFile = "./Build/Infrastructure.snk"
                                   Version = Some(System.Version(ver))
                                   Internalize = ILMerge.InternalizeTypes.Internalize
                                   Libraries =
                                     Seq.concat
                                       [ !!"./_Binaries/AltCover/Release+AnyCPU/Mono.C*.dll"

                                         !!"./_Binaries/AltCover/Release+AnyCPU/Newton*.dll" ]
                                     |> Seq.map (fun f -> f.Replace(here, "."))
                                   AttributeFile =
                                     "./_Binaries/AltCover/Release+AnyCPU/AltCover.exe" }
    "./_Binaries/AltCover/AltCover.exe" "./_Binaries/AltCover/Release+AnyCPU/AltCover.exe")
_Target "PrepareDotNetBuild" (fun _ ->
  let netcoresource = Path.getFullName "./AltCover/altcover.core.fsproj"
  let publish = Path.getFullName "./_Publish"

  DotNet.publish (fun options ->
    { options with OutputPath = Some publish
                   Configuration = DotNet.BuildConfiguration.Release
                   Framework = Some "netcoreapp2.0" }) netcoresource
  DotNet.publish (fun options ->
    { options with OutputPath = Some(publish + ".api")
                   Configuration = DotNet.BuildConfiguration.Release
                   Framework = Some "netstandard2.0" }) netcoresource
  DotNet.publish (fun options ->
    { options with OutputPath = Some(publish + ".visualizer")
                   Configuration = DotNet.BuildConfiguration.Release
                   Framework = Some "netcoreapp2.1" })
    (Path.getFullName "./AltCover.Visualizer/altcover.visualizer.core.fsproj")
  [ (//  let toolpath = Tools.findToolInSubPath "ILMerge.exe" "./packages"
     //  let ver = String.Join(".", (!Version).Split('.') |> Seq.take 2) + ".0.0"
     //
     //  [ publish; publish + ".api" ]
     //  |> List.iter (fun dir ->
     //    let outDir = Path.Combine(dir, "out")
     //    Directory.ensure outDir
     //    Actions.Run (toolpath, ".",
     //                 [ "/out:" + Path.Combine(outDir, "AltCover.Recorder.dll")
     //                   "/ver:" + ver
     //                   "/attr:" + Path.Combine(dir, "AltCover.Recorder.dll")
     //                   "/keyfile:./Build/Infrastructure.snk"
     //                   "/target:library"
     //                   "/internalize"
     //                   Path.Combine(dir, "AltCover.Recorder.dll")
     //                   Path.Combine(dir, "FSharp.Core.dll") ])
     //      "ILMerge failure")

     // dotnet tooling mods
     "DotnetCliTool", "./_Generated/altcover.dotnet.nuspec",
     "AltCover (dotnet CLI tool install)", None, None)

    ("DotnetTool", "./_Generated/altcover.global.nuspec",
     "AltCover (dotnet global tool install)", None, None)

    ("DotnetTool", "./_Generated/altcover.visualizer.nuspec",
     "AltCover.Visualizer (dotnet global tool install)",
     Some "AltCover.Visualizer/logo.png", Some "codecoverage .netcore cross-platform")

    (String.Empty, "./_Generated/altcover.api.nuspec", "AltCover (API install)", None,
     None) ]
  |> List.iter (fun (ptype, path, caption, icon, tags) ->
       let x s = XName.Get(s, "http://schemas.microsoft.com/packaging/2010/07/nuspec.xsd")
       let dotnetNupkg = XDocument.Load "./Build/AltCover.nuspec"
       let title = dotnetNupkg.Descendants(x "title") |> Seq.head
       title.ReplaceNodes caption
       if ptype
          |> String.IsNullOrWhiteSpace
          |> not
       then
         let tag = dotnetNupkg.Descendants(x "tags") |> Seq.head
         let insert = XElement(x "packageTypes")
         insert.Add(XElement(x "packageType", XAttribute(XName.Get "name", ptype)))
         tag.AddAfterSelf insert
       match icon with
       | None -> ()
       | Some logo ->
         let tag = dotnetNupkg.Descendants(x "iconUrl") |> Seq.head
         let text = String.Concat(tag.Nodes()).Replace("Build/AltCover_128.png", logo)
         tag.Value <- text
       match tags with
       | None -> ()
       | Some line ->
         let tagnode = dotnetNupkg.Descendants(x "tags") |> Seq.head
         tagnode.Value <- line
       dotnetNupkg.Save path))

_Target "PrepareReadMe"
  (fun _ ->
  Actions.PrepareReadMe
    ((!Copyright).Replace("©", "&#xa9;").Replace("<", "&lt;").Replace(">", "&gt;")))

// Post-packaging deployment touch test

_Target "Deployment" ignore

_Target "Unpack" (fun _ ->
  let nugget = !!"./_Packaging/*.nupkg" |> Seq.last
  let unpack = Path.getFullName "_Packaging/Unpack"
  System.IO.Compression.ZipFile.ExtractToDirectory(nugget, unpack))

_Target "WindowsPowerShell"
  (fun _ ->
  let v = (!Version).Split([| '-' |]).[0]
  Actions.RunRaw
    ("powershell.exe", ".", [ "-NoProfile"; "./Build/powershell.ps1"; "-ACV"; v ])
    "powershell")

_Target "Pester" (fun _ ->
  Directory.ensure "./_Reports"
  let nugget = !!"./_Packaging/*.nupkg" |> Seq.last
  let ``module`` = Path.getFullName "_Packaging/Module"
  System.IO.Compression.ZipFile.ExtractToDirectory(nugget, ``module``)
  let unpack = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0"
  let report = Path.getFullName "_Reports/Pester.xml"
  let i = ``module`` @@ "tools/netcoreapp2.0"
  let v = (!Version).Split([| '-' |]).[0]
  let retro = Path.getFullName "_Binaries/AltCover/Release+AnyCPU/AltCover.exe"
  let key = Path.getFullName "Build/Infrastructure.snk"

  Actions.Run (retro, unpack,
               [ "--inplace"
                 "--save"
                 "--opencover"
                 "-t=DotNet"
                 "-t=System\\."
                 "-s=^AltCover$"
                 "-s=Recorder"
                 "-x"
                 report
                 "-i"
                 i
                 "-sn"
                 key ]) "Pester instrument"

  printfn "Execute the instrumented tests"
  Actions.RunRaw (pwsh, ".", [ "-NoProfile"; "./Build/pester.ps1"; "-ACV"; v ]) "pwsh"
  Actions.RunDotnet (fun o -> { dotnetOptions o with WorkingDirectory = unpack }) ""
    ("AltCover.dll Runner --collect -r \"" + i + "\"") "Collect the output"
  ReportGenerator.generateReports (fun p ->
    { p with ExePath = Tools.findToolInSubPath "ReportGenerator.exe" "."
             ReportTypes =
               [ ReportGenerator.ReportType.Html; ReportGenerator.ReportType.XmlSummary ]
             TargetDir = "_Reports/_Pester" }) [ report ]

  "_Reports/_Pester/Summary.xml"
  |> File.ReadAllText
  |> printfn "%s")

_Target "SimpleReleaseTest" (fun _ ->
  let unpack = Path.getFullName "_Packaging/Unpack/tools/net45"
  if (unpack @@ "AltCover.exe") |> File.Exists then
    Actions.SimpleInstrumentingRun "_Binaries/Sample1/Debug+AnyCPU" unpack
      "SimpleReleaseTest"
  else
    let file =
      Directory.GetFiles("./packages", "AltCover.exe", SearchOption.AllDirectories)
      |> Seq.tryFind (fun _ -> true)
    match file with
    | Some test ->
      Actions.SimpleInstrumentingRun "_Binaries/Sample1/Debug+AnyCPU"
        (Path.GetDirectoryName test) "SimpleReleaseTest"
    | None -> printfn "Skipping -- AltCover.exe not packaged")

_Target "SimpleMonoReleaseTest" (fun _ ->
  let unpack = Path.getFullName "_Packaging/Unpack/tools/net45"
  if (unpack @@ "AltCover.exe") |> File.Exists then
    Actions.SimpleInstrumentingRun "_Mono/Sample1" unpack "SimpleMonoReleaseTest"
  else
    let file =
      Directory.GetFiles("./packages", "AltCover.exe", SearchOption.AllDirectories)
      |> Seq.tryFind (fun _ -> true)
    match file with
    | Some test ->
      Actions.SimpleInstrumentingRun "_Binaries/Sample1/Debug+AnyCPU"
        (Path.GetDirectoryName test) "SimpleReleaseTest"
    | None -> printfn "Skipping -- AltCover.exe not packaged")

_Target "ReleaseDotNetWithFramework" (fun _ ->
  Directory.ensure "./_Reports"
  let unpack0 = Path.getFullName "_Packaging/Unpack/tools/net45/AltCover.exe"
  let unpack1 =
    Directory.GetFiles
      (Path.getFullName "./packages", "AltCover.exe", SearchOption.AllDirectories)
    |> Seq.tryFind (fun _ -> true)

  let unpack =
    if File.Exists unpack0 then Some unpack0
    else unpack1
  if Option.isSome unpack then
    let simpleReport =
      (Path.getFullName "./_Reports") @@ ("ReleaseDotNetWithFramework.xml")
    let sampleRoot = Path.getFullName "./_Binaries/Sample1/Debug+AnyCPU/netcoreapp2.0"
    let instrumented = sampleRoot @@ "__Instrumented.ReleaseDotNetWithFramework"

    Actions.Run (Option.get unpack, sampleRoot,
                 [ "-t=System\\."
                   "-t=Microsoft\\."
                   "-x=" + simpleReport
                   "/o=" + instrumented ]) "ReleaseDotNetWithFramework"

    Actions.RunDotnet (fun o -> { dotnetOptions o with WorkingDirectory = instrumented })
      "Sample1.dll" "" "ReleaseDotNetWithFramework test"

    Actions.ValidateSample1 "./_Reports/ReleaseDotNetWithFramework.xml"
      "ReleaseDotNetWithFramework"
  else printfn "Skipping -- AltCover.exe not packaged")

_Target "ReleaseMonoWithDotNet" (fun _ ->
  Directory.ensure "./_Reports"
  let unpack = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0"
  let x = Path.getFullName "./_Reports/ReleaseMonoWithDotNet.xml"
  let o = Path.getFullName "./_Mono/__Instrumented.ReleaseMonoWithDotNet"
  let i = Path.getFullName "./_Mono/Sample1"

  Actions.RunDotnet (fun o -> { dotnetOptions o with WorkingDirectory = unpack }) ""
    ("AltCover.dll -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")
    "ReleaseMonoWithDotNet"

  Actions.Run (o @@ "Sample1.exe", o, []) "Instrumented .exe failed"
  Actions.ValidateSample1 "./_Reports/ReleaseMonoWithDotNet.xml" "ReleaseMonoWithDotNet")

_Target "ReleaseDotNetWithDotNet"
  (fun _ ->
  Directory.ensure "./_Reports"
  let unpack = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0"
  let x = Path.getFullName "./_Reports/ReleaseDotNetWithDotNet.xml"
  let o = Path.getFullName "./_Binaries/Sample1/__Instrumented.ReleaseDotNetWithDotNet"
  let i = Path.getFullName "./_Binaries/Sample1/Debug+AnyCPU/netcoreapp2.0"

  Actions.RunDotnet (fun o -> { dotnetOptions o with WorkingDirectory = unpack }) "run"
    ("AltCover.dll -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")
    "ReleaseDotNetWithDotNet"
  Actions.RunDotnet dotnetOptions (o @@ "Sample1.dll") "" "ReleaseDotNetWithDotNet test"
  Actions.ValidateSample1 "./_Reports/ReleaseDotNetWithDotNet.xml"
    "ReleaseDotNetWithDotNet")

_Target "ReleaseFSharpTypesDotNetRunner" (fun _ ->
  Directory.ensure "./_Reports"
  let unpack = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0"
  let x = Path.getFullName "./_Reports/AltCoverReleaseFSharpTypesDotNetRunner.xml"
  let o = Path.getFullName "Sample2/_Binaries/Sample2/Debug+AnyCPU/netcoreapp2.1"
  let i = Path.getFullName "_Binaries/Sample2/Debug+AnyCPU/netcoreapp2.1"

  Shell.cleanDir o

  // Instrument the code
  Actions.RunDotnet (fun o' -> { dotnetOptions o' with WorkingDirectory = unpack }) ""
    ("AltCover.dll -s=Adapter -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")
    "ReleaseFSharpTypesDotNetRunner"

  Actions.ValidateFSharpTypes x [ "main" ]

  printfn "Execute the instrumented tests"
  let sample2 = Path.getFullName "./Sample2/sample2.core.fsproj"
  let runner = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0/AltCover.dll"

  // Run
  Actions.RunDotnet (fun o' -> { dotnetOptions o' with WorkingDirectory = o }) ""
    (runner + " Runner -x \"dotnet\" -r \"" + o
     + "\" -- test --no-build --configuration Debug " + sample2)
    "ReleaseFSharpTypesDotNetRunner test"
  Actions.ValidateFSharpTypesCoverage x)

_Target "ReleaseFSharpTypesX86DotNetRunner" (fun _ ->
  Directory.ensure "./_Reports"
  let unpack = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0"
  let s = Path.getFullName "."
  let x = Path.getFullName "./_Reports/AltCoverReleaseFSharpTypesX86DotNetRunner.xml"
  let o = Path.getFullName "Sample2/_Binaries/Sample2/Debug+x86/netcoreapp2.1"
  let i = Path.getFullName "_Binaries/Sample2/Debug+x86/netcoreapp2.1"

  Shell.cleanDir o
  try
    try
      Environment.SetEnvironmentVariable("platform", "x86")
      Actions.Run (dotnetPath86 |> Option.get, ".", [ "--info" ]) "dotnet-x86 failed"
      printfn "Build the sample2 code as x86"
      Actions.RunDotnet (fun o' -> { dotnetOptions o' with WorkingDirectory = s }) "build"
        (" altcover.core.sln --configuration Debug")
        "ReleaseFSharpTypesX86DotNetRunnerBuild"

      printfn "Instrument the code"
      let altcover = unpack @@ "AltCover.dll"
      Actions.RunDotnet (fun o' ->
        { dotnetOptions o' with WorkingDirectory = unpack
                                DotNetCliPath = dotnetPath86 |> Option.get }) ""
        (altcover + " -s=Adapter -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")
        "ReleaseFSharpTypesX86DotNetRunner"
      Actions.ValidateFSharpTypes x [ "main" ]
      printfn "Execute the instrumented tests"
      let sample2 = Path.getFullName "./Sample2/sample2.core.fsproj"

      // Run
      Actions.RunDotnet (fun o' ->
        { dotnetOptions o' with WorkingDirectory = o
                                DotNetCliPath = dotnetPath86 |> Option.get }) ""
        (altcover + " Runner -x \"" + (dotnetPath86 |> Option.get) + "\" -r \"" + o
         + "\" -- test --no-build --configuration Debug " + sample2)
        "ReleaseFSharpTypesX86DotNetRunner test"

      Actions.ValidateFSharpTypesCoverage x
    with x ->
      printfn "Failed with %A" x
      reraise()
  finally
    Environment.SetEnvironmentVariable("platform", ""))

_Target "ReleaseXUnitFSharpTypesDotNet" (fun _ ->
  Directory.ensure "./_Reports"
  let unpack = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0"
  let x = Path.getFullName "./_Reports/ReleaseXUnitFSharpTypesDotNet.xml"
  let o = Path.getFullName "Sample4/_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.1"
  let i = Path.getFullName "_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.1"

  Shell.cleanDir o

  // Instrument the code
  Actions.RunDotnet (fun o' -> { dotnetOptions o' with WorkingDirectory = unpack }) ""
    ("AltCover.dll -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")
    "ReleaseXUnitFSharpTypesDotNet"

  Actions.ValidateFSharpTypes x [ "main" ]

  printfn "Execute the instrumented tests"
  Actions.RunDotnet
    (fun o' -> { dotnetOptions o' with WorkingDirectory = Path.getFullName "Sample4" })
    "test" ("-v m --no-build --configuration Debug sample4.core.fsproj -v m")
    "sample test returned with a non-zero exit code"
  Actions.ValidateFSharpTypesCoverage x)

_Target "ReleaseXUnitFSharpTypesDotNetRunner" (fun _ ->
  Directory.ensure "./_Reports"
  let unpack = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0"
  let x = Path.getFullName "./_Reports/ReleaseXUnitFSharpTypesDotNetRunner.xml"
  let o = Path.getFullName "Sample4/_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.1"
  let i = Path.getFullName "_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.1"

  Shell.cleanDir o

  // Instrument the code
  Actions.RunDotnet (fun o' -> { dotnetOptions o' with WorkingDirectory = unpack }) ""
    ("AltCover.dll -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")
    "ReleaseXUnitFSharpTypesDotNetRunner"

  Actions.ValidateFSharpTypes x [ "main" ]

  printfn "Execute the instrumented tests"
  let sample4 = Path.getFullName "./Sample4/sample4.core.fsproj"
  let runner = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0/AltCover.dll"

  // Run
  Actions.RunDotnet (fun o' -> { dotnetOptions o' with WorkingDirectory = o }) ""
    (runner + " Runner -x \"dotnet\" -r \"" + o
     + "\" -- test --no-build --configuration Debug " + sample4)
    "ReleaseXUnitFSharpTypesDotNetRunner test"
  Actions.ValidateFSharpTypesCoverage x)

_Target "ReleaseXUnitFSharpTypesDotNetFullRunner" (fun _ ->
  Directory.ensure "./_Reports"
  let unpack = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0"
  let x = Path.getFullName "./_Reports/ReleaseXUnitFSharpTypesDotNetFullRunner.xml"
  let o = Path.getFullName "Sample4/_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.1"
  let i = Path.getFullName "_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.1"

  Shell.cleanDir o
  Actions.RunDotnet (fun o' -> { dotnetOptions o' with WorkingDirectory = unpack }) ""
    ("AltCover.dll --opencover -c=0 \"-c=[Fact]\" -x \"" + x + "\" -o \"" + o + "\" -i \""
     + i + "\"") "ReleaseXUnitFSharpTypesDotNetFullRunner"
  Actions.CheckSample4Content x

  printfn "Execute the instrumented tests"
  let sample4 = Path.getFullName "./Sample4/sample4.core.fsproj"
  let runner = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0/AltCover.dll"

  // Run
  Actions.RunDotnet (fun o' -> { dotnetOptions o' with WorkingDirectory = o }) ""
    (runner + " Runner -x \"dotnet\" -r \"" + o
     + "\" -- test --no-build --configuration Debug " + sample4)
    "ReleaseXUnitFSharpTypesDotNetFullRunner test"
  Actions.CheckSample4Visits x)

_Target "MSBuildTest" (fun _ ->
  Directory.ensure "./_Reports"
  let build = Path.getFullName "Build"
  let sample = Path.getFullName "Sample4"
  let x = Path.getFullName "./_Reports/MSBuildTest.xml"

  // Run
  Shell.cleanDir (sample @@ "_Binaries")
  Actions.RunDotnet (fun o' -> { dotnetOptions o' with WorkingDirectory = sample })
    "msbuild" (build @@ "msbuildtest.proj") "MSBuildTest"
  printfn "Checking samples4 output"
  Actions.CheckSample4 x

  // touch-test framework
  let unpack = Path.getFullName "_Packaging/Unpack/tools/net45"
  if (unpack @@ "AltCover.exe") |> File.Exists then
    MSBuild.build (fun p ->
      { p with Verbosity = Some MSBuildVerbosity.Minimal
               Properties =
                 [ "Configuration", "Debug"
                   "DebugSymbols", "True" ] }) "./Sample4/Sample4.prepare.fsproj"
  else printfn "Skipping touch-test -- AltCover.exe not packaged")

_Target "ApiUse" (fun _ ->
  try
    Directory.ensure "./_ApiUse"
    Shell.cleanDir ("./_ApiUse")
    Directory.ensure "./_ApiUse/_DotnetTest"

    let config = XDocument.Load "./Build/NuGet.config.dotnettest"
    let repo = config.Descendants(XName.Get("add")) |> Seq.head
    repo.SetAttributeValue(XName.Get "value", Path.getFullName "./_Packaging")
    config.Save "./_ApiUse/_DotnetTest/NuGet.config"

    let fsproj = XDocument.Load "./Sample4/sample4.core.fsproj"
    let pack = fsproj.Descendants(XName.Get("PackageReference")) |> Seq.head
    let inject =
      XElement
        (XName.Get "PackageReference", XAttribute(XName.Get "Include", "altcover.api"),
         XAttribute(XName.Get "Version", !Version))
    pack.AddBeforeSelf inject
    fsproj.Save "./_ApiUse/_DotnetTest/dotnettest.fsproj"
    Shell.copy "./_ApiUse/_DotnetTest" (!!"./Sample4/*.fs")

    let config = """<?xml version="1.0" encoding="utf-8"?>
<configuration>
  <packageSources>
    <add key="local" value="{0}" />
    <add key="nuget.org" value="https://api.nuget.org/v3/index.json" />
  </packageSources>
</configuration>"""

    File.WriteAllText
      ("./_ApiUse/NuGet.config",
       String.Format(config, Path.getFullName "./_Packaging.api"))

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

_Target "DoIt"
  (fun _ ->
  AltCover.Api.Version() |> printfn "Returned %A"
  AltCover.Fake.Api.Version() |> Trace.trace
  AltCover.CSApi.Version() |> printfn "Returned %A"

  let collect = { AltCover.CollectParams.Create() with LcovReport = "x" }
  let prepare = { AltCover.PrepareParams.Create() with TypeFilter = [| "a"; "b" |] }
  printfn "%s" (AltCover.DotNet.ToTestArguments prepare collect)

  let t = DotNet.TestOptions.Create().WithParameters prepare collect
  printfn "returned '%A'" t.Common.CustomParams

  let p2 = { AltCover.PrepareParams.Create() with CallContext = [| "[Fact]"; "0" |] }
  let c2 = AltCover.CollectParams.Create()

  let setBaseOptions (o : DotNet.Options) =
    { o with WorkingDirectory = Path.getFullName "./_DotnetTest"
             Verbosity = Some DotNet.Verbosity.Minimal }

  let cliArguments =
    { MSBuild.CliArguments.Create() with ConsoleLogParameters = []
                                         DistributedLoggers = None
                                         DisableInternalBinLog = true }

  DotNet.test
    (fun to' ->
    { to'.WithCommon(setBaseOptions).WithParameters p2 c2 with MSBuildParams =
                                                                 cliArguments })
    "dotnettest.fsproj"
  let ipmo = AltCover.Api.Ipmo().Trim().Split().[1].Trim([| '\"' |])
  let command = "$ipmo = '" + ipmo + "'; Import-Module $ipmo; ConvertTo-BarChart -?"

  let corePath = Testing.AltCover.toolPath Testing.AltCover.ToolType.Global
  printfn "corePath = %A" corePath

  let frameworkPath = Testing.AltCover.toolPath Testing.AltCover.ToolType.Framework
  printfn "frameworkPath = %A" frameworkPath
  if Environment.isWindows then // the Framework version isn't packaged on mono because ILMerge is needed
    { Testing.AltCover.Params.Create Testing.AltCover.ArgType.GetVersion with ToolPath =
                                                                                frameworkPath
                                                                              ToolType =
                                                                                Testing.AltCover.ToolType.Framework }
    |> Testing.AltCover.run

  let pwsh =
    if Environment.isWindows then
      Tools.findToolInSubPath "pwsh.exe"
        (Environment.environVar "ProgramFiles" @@ "PowerShell")
    else "pwsh"

  let r =
    CreateProcess.fromRawCommand pwsh [ "-NoProfile"; "-Command"; command ]
    |> CreateProcess.withWorkingDirectory "."
    |> Proc.run

  if (r.ExitCode <> 0) then new InvalidOperationException("Non zero return code") |> raise)
Target.runOrDefault "DoIt"
"""
    File.WriteAllText("./_ApiUse/DriveApi.fsx", script)

    let dependencies = """// [ FAKE GROUP ]
group NetcoreBuild
  source https://api.nuget.org/v3/index.json
  nuget Fake.Core >= 5.8.4
  nuget Fake.Core.Target >= 5.8.5
  nuget Fake.DotNet.Cli >= 5.8.5

  source {0}
  nuget AltCover.Api {1} """

    File.WriteAllText
      ("./_ApiUse/paket.dependencies",
       String.Format(dependencies, Path.getFullName "./_Packaging.api", !Version))

    Shell.copy "./_ApiUse" (!!"./dotnet*.fsproj")

    Actions.RunDotnet
      (fun o' -> { dotnetOptions o' with WorkingDirectory = Path.getFullName "_ApiUse" })
      "restore" (String.Empty) "restoring dotnet-fake returned with a non-zero exit code"
    Actions.RunDotnet
      (fun o' -> { dotnetOptions o' with WorkingDirectory = Path.getFullName "_ApiUse" })
      "fake" ("run ./DriveApi.fsx")
      "running fake script returned with a non-zero exit code"

    let x = Path.getFullName "./_ApiUse/_DotnetTest/coverage.xml"
    Actions.CheckSample4 x
  finally
    [ "altcover"; "altcover.api" ]
    |> List.iter (fun f ->
         let folder = (nugetCache @@ f) @@ !Version
         Shell.mkdir folder
         Shell.deleteDir folder))

_Target "DotnetTestIntegration" (fun _ ->
  try
    Directory.ensure "./_DotnetTest"
    Shell.cleanDir ("./_DotnetTest")
    let config = XDocument.Load "./Build/NuGet.config.dotnettest"
    let repo = config.Descendants(XName.Get("add")) |> Seq.head
    repo.SetAttributeValue(XName.Get "value", Path.getFullName "./_Packaging")
    config.Save "./_DotnetTest/NuGet.config"

    let fsproj = XDocument.Load "./Sample4/sample4.core.fsproj"
    let pack = fsproj.Descendants(XName.Get("PackageReference")) |> Seq.head
    let inject =
      XElement
        (XName.Get "PackageReference", XAttribute(XName.Get "Include", "altcover"),
         XAttribute(XName.Get "Version", !Version))
    pack.AddBeforeSelf inject
    fsproj.Save "./_DotnetTest/dotnettest.fsproj"
    Shell.copy "./_DotnetTest" (!!"./Sample4/*.fs")

    DotNet.test
      (fun to' ->
      to'.WithCommon
        (fun o' ->
        { dotnetOptions o' with WorkingDirectory = Path.getFullName "_DotnetTest"
                                Verbosity = Some DotNet.Verbosity.Minimal
                                CustomParams =
                                  Some
                                    "/p:AltCover=true /p:AltCoverCallContext=[Fact]|0 /p:AltCoverIpmo=true /p:AltCoverGetVersion=true" }))
      "dotnettest.fsproj"

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
    repo.SetAttributeValue(XName.Get "value", Path.getFullName "./_Packaging")
    config.Save "./_DotnetTestLineCover/NuGet.config"

    let fsproj = XDocument.Load "./Sample10/sample10.core.csproj"
    let pack = fsproj.Descendants(XName.Get("PackageReference")) |> Seq.head
    let inject =
      XElement
        (XName.Get "PackageReference", XAttribute(XName.Get "Include", "altcover"),
         XAttribute(XName.Get "Version", !Version))
    pack.AddBeforeSelf inject
    fsproj.Save "./_DotnetTestLineCover/dotnettest.csproj"
    Shell.copy "./_DotnetTestLineCover" (!!"./Sample10/*.cs")

    Actions.RunDotnet
      (fun o' ->
      { dotnetOptions o' with WorkingDirectory = Path.getFullName "_DotnetTestLineCover" })
      "test" ("-v m /p:AltCover=true /p:AltCoverLineCover=true")
      "sample test returned with a non-zero exit code"

    let x = Path.getFullName "./_DotnetTestLineCover/coverage.xml"

    do use coverageFile =
         new FileStream(x, FileMode.Open, FileAccess.Read, FileShare.None, 4096,
                        FileOptions.SequentialScan)
       let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))
       Assert.That
         (coverageDocument.Descendants(XName.Get("SequencePoint")) |> Seq.length,
          Is.EqualTo 13)
       Assert.That
         (coverageDocument.Descendants(XName.Get("BranchPoint")) |> Seq.length,
          Is.EqualTo 0)

    // optest branchcover
    Directory.ensure "./_DotnetTestBranchCover"
    Shell.cleanDir ("./_DotnetTestBranchCover")
    let config = XDocument.Load "./Build/NuGet.config.dotnettest"
    let repo = config.Descendants(XName.Get("add")) |> Seq.head
    repo.SetAttributeValue(XName.Get "value", Path.getFullName "./_Packaging")
    config.Save "./_DotnetTestLineCover/NuGet.config"

    let fsproj = XDocument.Load "./Sample10/sample10.core.csproj"
    let pack = fsproj.Descendants(XName.Get("PackageReference")) |> Seq.head
    let inject =
      XElement
        (XName.Get "PackageReference", XAttribute(XName.Get "Include", "altcover"),
         XAttribute(XName.Get "Version", !Version))
    pack.AddBeforeSelf inject
    fsproj.Save "./_DotnetTestBranchCover/dotnettest.csproj"
    Shell.copy "./_DotnetTestBranchCover" (!!"./Sample10/*.cs")

    Actions.RunDotnet
      (fun o' ->
      { dotnetOptions o' with WorkingDirectory = Path.getFullName "_DotnetTestBranchCover" })
      "test" ("-v m /p:AltCover=true /p:AltCoverBranchCover=true")
      "sample test returned with a non-zero exit code"

    let x = Path.getFullName "./_DotnetTestBranchCover/coverage.xml"

    do use coverageFile =
         new FileStream(x, FileMode.Open, FileAccess.Read, FileShare.None, 4096,
                        FileOptions.SequentialScan)
       let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))
       Assert.That
         (coverageDocument.Descendants(XName.Get("SequencePoint")) |> Seq.length,
          Is.EqualTo 0)
       Assert.That
         (coverageDocument.Descendants(XName.Get("BranchPoint")) |> Seq.length,
          Is.EqualTo 2)

    // Regression test -- On travis : 'the reference assemblies for framework ".NETFramework,Version=v4.6.1" were not found.'
    if Environment.isWindows then
      let proj = XDocument.Load "./RegressionTesting/issue29/issue29.xml"
      let pack = proj.Descendants(XName.Get("PackageReference")) |> Seq.head
      let inject =
        XElement
          (XName.Get "PackageReference", XAttribute(XName.Get "Include", "altcover"),
           XAttribute(XName.Get "Version", !Version))
      pack.AddBeforeSelf inject
      proj.Save "./RegressionTesting/issue29/issue29.csproj"
      Actions.RunDotnet
        (fun o' ->
        { dotnetOptions o' with WorkingDirectory =
                                  Path.getFullName "RegressionTesting/issue29" }) "test"
        ("-v m /p:AltCover=true")
        "issue#29 regression test returned with a non-zero exit code"

    let proj = XDocument.Load "./RegressionTesting/issue37/issue37.xml"
    let pack = proj.Descendants(XName.Get("PackageReference")) |> Seq.head
    let inject =
      XElement
        (XName.Get "PackageReference", XAttribute(XName.Get "Include", "altcover"),
         XAttribute(XName.Get "Version", !Version))
    pack.AddBeforeSelf inject
    proj.Save "./RegressionTesting/issue37/issue37.csproj"
    Actions.RunDotnet
      (fun o' ->
      { dotnetOptions o' with WorkingDirectory =
                                Path.getFullName "RegressionTesting/issue37" }) "test"
      ("-v m -c Release /p:AltCover=true /p:AltCoverAssemblyFilter=NUnit")
      "issue#37 regression test returned with a non-zero exit code"

    let cover37 = XDocument.Load "./RegressionTesting/issue37/coverage.xml"
    Assert.That(cover37.Descendants(XName.Get("BranchPoint")) |> Seq.length, Is.EqualTo 2)
  finally
    let folder = (nugetCache @@ "altcover") @@ !Version
    Shell.mkdir folder
    Shell.deleteDir folder)

_Target "Issue20" (fun _ ->
  try
    let config = XDocument.Load "./Build/NuGet.config.dotnettest"
    let repo = config.Descendants(XName.Get("add")) |> Seq.head
    repo.SetAttributeValue(XName.Get "value", Path.getFullName "./_Packaging")
    config.Save "./RegressionTesting/issue20/NuGet.config"

    let csproj = XDocument.Load "./RegressionTesting/issue20/xunit-tests/xunit-tests.xml"
    let pack = csproj.Descendants(XName.Get("PackageReference")) |> Seq.head
    let inject =
      XElement
        (XName.Get "PackageReference", XAttribute(XName.Get "Include", "altcover"),
         XAttribute(XName.Get "Version", !Version))
    pack.AddBeforeSelf inject
    csproj.Save "./RegressionTesting/issue20/xunit-tests/xunit-tests.csproj"

    DotNet.restore
      (fun o' ->
      { o' with Common =
                  { dotnetOptions o'.Common with WorkingDirectory =
                                                   Path.getFullName
                                                     "./RegressionTesting/issue20/classlib" } })
      ""
    DotNet.restore
      (fun o' ->
      { o' with Common =
                  { dotnetOptions o'.Common with WorkingDirectory =
                                                   Path.getFullName
                                                     "./RegressionTesting/issue20/xunit-tests" } })
      ""
    Actions.RunDotnet
      (fun o' ->
      { dotnetOptions o' with WorkingDirectory =
                                Path.getFullName "./RegressionTesting/issue20/xunit-tests" })
      "test" ("-v m /p:AltCover=true") "sample test returned with a non-zero exit code"

  //    if Environment.isWindows then
  //      Path.getFullName "./RegressionTesting/issue20/xunit-tests/bin"
  //      |> Shell.cleanDir
  //
  //      Actions.RunDotnet
  //        (fun o' ->
  //        { dotnetOptions o' with WorkingDirectory =
  //                                  Path.getFullName "./RegressionTesting/issue20/xunit-tests" })
  //        "test" ("""/p:AltCover=true /p:AltCoverDependencyList="%ProgramFiles%/dotnet/shared/Microsoft.AspNetCore.App/2.1.5/Microsoft.AspNetCore.Cryptography.KeyDerivation.dll" -v m""") "sample test returned with a non-zero exit code"

  finally
    let folder = (nugetCache @@ "altcover") @@ !Version
    Shell.mkdir folder
    Shell.deleteDir folder)

_Target "Issue23" (fun _ ->
  try
    Directory.ensure "./_Issue23"
    Shell.cleanDir ("./_Issue23")
    let config = XDocument.Load "./Build/NuGet.config.dotnettest"
    let repo = config.Descendants(XName.Get("add")) |> Seq.head
    repo.SetAttributeValue(XName.Get "value", Path.getFullName "./_Packaging")
    config.Save "./_Issue23/NuGet.config"

    let csproj = XDocument.Load "./Sample9/sample9.csproj"
    let pack = csproj.Descendants(XName.Get("PackageReference")) |> Seq.head
    let inject =
      XElement
        (XName.Get "PackageReference", XAttribute(XName.Get "Include", "altcover"),
         XAttribute(XName.Get "Version", !Version))
    pack.AddBeforeSelf inject
    csproj.Save "./_Issue23/sample9.csproj"
    Shell.copy "./_Issue23" (!!"./Sample9/*.cs")

    Actions.RunDotnet
      (fun o' -> { dotnetOptions o' with WorkingDirectory = Path.getFullName "_Issue23" })
      "restore" ("") "restore returned with a non-zero exit code"

    Actions.RunDotnet
      (fun o' -> { dotnetOptions o' with WorkingDirectory = Path.getFullName "_Issue23" })
      "test" ("-v m /p:AltCover=true /p:AltCoverIpmo=true /p:AltCoverGetVersion=true")
      "sample test returned with a non-zero exit code"
  finally
    let folder = (nugetCache @@ "altcover") @@ !Version
    Shell.mkdir folder
    Shell.deleteDir folder)

_Target "DotnetCLIIntegration" (fun _ ->
  try
    Directory.ensure "./_DotnetCLITest"
    Shell.cleanDir ("./_DotnetCLITest")
    let config = XDocument.Load "./Build/NuGet.config.dotnettest"
    let repo = config.Descendants(XName.Get("add")) |> Seq.head
    repo.SetAttributeValue(XName.Get "value", Path.getFullName "./_Packaging.dotnet")
    config.Save "./_DotnetCLITest/NuGet.config"

    let fsproj = XDocument.Load "./Sample4/sample4.core.fsproj"
    let pack = fsproj.Descendants(XName.Get("PackageReference")) |> Seq.head
    let inject =
      XElement
        (XName.Get "DotNetCliToolReference",
         XAttribute(XName.Get "Include", "altcover.dotnet"),
         XAttribute(XName.Get "Version", !Version))
    pack.AddBeforeSelf inject
    fsproj.Save "./_DotnetCLITest/dotnetcli.fsproj"
    Shell.copy "./_DotnetCLITest" (!!"./Sample4/*.fs")

    let working = Path.getFullName "./_DotnetCLITest"
    Actions.RunDotnet (fun o' -> { dotnetOptions o' with WorkingDirectory = working })
      "build" "" "Restored"
    let x = Path.getFullName "./_Reports/DotnetCLIIntegration.xml"
    let o =
      Path.getFullName "./_DotnetCLITest/_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.1"

    Actions.RunDotnet (fun o' -> { dotnetOptions o' with WorkingDirectory = working })
      "altcover" ("ipmo") "DotnetCLIIntegration ipmo"
    Actions.RunDotnet (fun o' -> { dotnetOptions o' with WorkingDirectory = working })
      "altcover" ("version") "DotnetCLIIntegration version"
    Actions.RunDotnet (fun o' -> { dotnetOptions o' with WorkingDirectory = working })
      "altcover"
      (" --opencover --inplace -c=0 \"-c=[Fact]\" -x \"" + x + "\" -i \"" + o + "\"")
      "DotnetCLIIntegration"

    Actions.CheckSample4Content x

    printfn "Execute the instrumented tests"
    Actions.RunDotnet (fun o' -> { dotnetOptions o' with WorkingDirectory = working })
      "altcover"
      (" Runner -x \"dotnet\" -r \"" + o + "\" -- test --no-build --configuration Debug ")
      "DotnetCLIIntegration test"

    Actions.CheckSample4Visits x

    let command =
      """$ipmo = (dotnet altcover ipmo | Out-String).Trim().Split()[1].Trim(@([char]34)); Import-Module $ipmo; ConvertTo-BarChart -?"""
    Actions.RunRaw (pwsh, working, [ "-NoProfile"; "-Command"; command ]) "pwsh"
    Actions.RunDotnet
      (fun o' ->
      { dotnetOptions o' with WorkingDirectory = Path.getFullName "_DotnetCLITest" })
      "add" ("package altcover.dotnet --version " + !Version)
      "sample test returned with a non-zero exit code"

    Actions.RunDotnet
      (fun o' ->
      { dotnetOptions o' with WorkingDirectory = Path.getFullName "_DotnetCLITest" })
      "test" ("-v m /p:AltCover=true") "sample test returned with a non-zero exit code"
    "./_DotnetCLITest/coverage.xml"
    |> Path.getFullName
    |> File.Exists
    |> Assert.That
  finally
    let folder = (nugetCache @@ "altcover.dotnet") @@ !Version
    Shell.mkdir folder
    Shell.deleteDir folder
    let folder2 = ((nugetCache @@ ".tools") @@ "altcover.dotnet") @@ !Version
    Shell.mkdir folder2
    Shell.deleteDir folder2)

_Target "DotnetGlobalIntegration" (fun _ ->
  let working = Path.getFullName "./_DotnetGlobalTest"
  let mutable set = false
  try
    Directory.ensure working
    Shell.cleanDir working

    let fsproj = XDocument.Load "./Sample4/sample4.core.fsproj"
    fsproj.Save "./_DotnetGlobalTest/dotnetglobal.fsproj"
    Shell.copy "./_DotnetGlobalTest" (!!"./Sample4/*.fs")

    Actions.RunDotnet (fun o' -> { dotnetOptions o' with WorkingDirectory = working })
      "tool"
      ("install -g altcover.global --add-source "
       + (Path.getFullName "./_Packaging.global") + " --version " + !Version) "Installed"

    Actions.RunDotnet (fun o' -> { dotnetOptions o' with WorkingDirectory = working })
      "tool" ("list -g ") "Checked"
    set <- true

    Actions.RunDotnet (fun o' -> { dotnetOptions o' with WorkingDirectory = working })
      "build" "" "Built"

    let x = Path.getFullName "./_Reports/DotnetGlobalIntegration.xml"
    let o =
      Path.getFullName "./_DotnetGlobalTest/_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.1"
    AltCover.Params.Create AltCover.ArgType.ImportModule |> AltCover.run
    AltCover.Params.Create AltCover.ArgType.GetVersion |> AltCover.run
    let prep =
      { AltCover.PrepareParams.Create() with XmlReport = x
                                             InputDirectory = o
                                             CallContext = [ "0"; "[Fact]" ]
                                             Save = false }
      |> AltCover.Prepare
    { AltCover.Params.Create prep with WorkingDirectory = working } |> AltCover.run

    Actions.CheckSample4Content x

    printfn "Execute the instrumented tests"
    let collect =
      { AltCover.CollectParams.Create() with Executable = "dotnet"
                                             RecorderDirectory = o }
        .withCommandLine [ "test"; "--no-build"; "--configuration"; "Debug" ]
      |> AltCover.Collect
    { AltCover.Params.Create collect with WorkingDirectory = working } |> AltCover.run

    Actions.CheckSample4Visits x
    let command =
      """$ipmo = (altcover ipmo | Out-String).Trim().Split()[1].Trim(@([char]34)); Import-Module $ipmo; ConvertTo-BarChart -?"""
    Actions.RunRaw (pwsh, working, [ "-NoProfile"; "-Command"; command ]) "pwsh"

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
    if set then
      Actions.RunDotnet (fun o' -> { dotnetOptions o' with WorkingDirectory = working })
        "tool" ("uninstall -g altcover.global") "uninstalled"
    let folder = (nugetCache @@ "altcover.global") @@ !Version
    Shell.mkdir folder
    Shell.deleteDir folder)

// AOB

_Target "BulkReport" (fun _ ->
  printfn "Overall coverage reporting"
  Directory.ensure "./_Reports/_BulkReport"

  !!"./_Reports/*.xml"
  |> Seq.filter
       (fun f -> not <| f.EndsWith("Report.xml", StringComparison.OrdinalIgnoreCase))
  |> Seq.toList
  |> ReportGenerator.generateReports (fun p ->
       { p with ExePath = Tools.findToolInSubPath "ReportGenerator.exe" "."
                ReportTypes = [ ReportGenerator.ReportType.Html ]
                TargetDir = "_Reports/_BulkReport" }))

_Target "All" ignore

let resetColours _ =
  Console.ForegroundColor <- consoleBefore |> fst
  Console.BackgroundColor <- consoleBefore |> snd

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
=?> ("UnitTest", Environment.isWindows)

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
=?> ("UnitTest", Environment.isWindows)

"Compilation"
?=> "OperationalTest"

"Compilation"
==> "FSharpTypes"
=?> ("OperationalTest", Environment.isWindows)

"Compilation"
==> "FSharpTests"
=?> ("OperationalTest", Environment.isWindows)

"Compilation"
==> "FSharpTypesDotNet"
// ==> "OperationalTest"

"Compilation"
==> "FSharpTypesDotNetRunner"
// ==> "OperationalTest" // test is duplicated in the Pester testing

"Compilation"
==> "FSharpTypesDotNetCollecter"
=?> ("OperationalTest", Environment.isWindows)

"Compilation"
==> "BasicCSharp"
==> "OperationalTest"

"Compilation"
==> "BasicCSharpMono"
=?> ("OperationalTest", Environment.isWindows)

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
=?> ("OperationalTest", Environment.isWindows)

"Compilation"
==> "RecordResumeTest"
==> "OperationalTest"

"Compilation"
==> "RecordResumeTrackingTest"
=?> ("OperationalTest", Environment.isWindows)

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
==> "Issue20"
==> "Deployment"
//=?> ("Deployment", Environment.isWindows)

"Unpack"
==> "DotnetCLIIntegration"
==> "Deployment"

"Unpack"
==> "DotnetGlobalIntegration"
==> "Deployment"

"Unpack"
==> "ReleaseXUnitFSharpTypesDotNetRunner"
=?> ("Deployment", Environment.isWindows)

"Unpack"
==> "ReleaseXUnitFSharpTypesDotNetFullRunner"
=?> ("Deployment", Environment.isWindows)

"Analysis"
==> "All"

"UnitTest"
==> "All"

"OperationalTest"
==> "All"

"Deployment"
==> "BulkReport"
==> "All"

let defaultTarget() =
  resetColours()
  "All"

Target.runOrDefault <| defaultTarget ()