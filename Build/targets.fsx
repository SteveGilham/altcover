open System
open System.Diagnostics.Tracing
open System.IO
open System.Reflection
open System.Xml
open System.Xml.Linq

open Actions
open AltCode.Fake.DotNet
open AltCover_Fake.DotNet.DotNet
open AltCover_Fake.DotNet.Testing

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
open FSharpLint.Framework
open NUnit.Framework
let Copyright = ref String.Empty
let Version = ref String.Empty
let consoleBefore = (Console.ForegroundColor, Console.BackgroundColor)

let OpenCoverFilter = "+[AltCove*]* -[*]Microsoft.* -[*]System.* +[*]N.*"

let AltCoverFilter(p : Primitive.PrepareParams) =
  { p with MethodFilter = "WaitForExitCustom" :: (p.MethodFilter |> Seq.toList)
           AssemblyExcludeFilter =
             [ "Adapter"; "Tests" ] @ (p.AssemblyExcludeFilter |> Seq.toList)
           AssemblyFilter =
             [ "Mono"; @"\.Recorder"; @"\.DataCollector"; "Sample"; "nunit"; "Newton"; "xunit"; "BlackFox" ]
             @ (p.AssemblyFilter |> Seq.toList)
           TypeFilter = [ @"System\."; @"Sample3\.Class2"; "Cecil11" ] @ (p.TypeFilter |> Seq.toList) }

let AltCoverFilterX(p : Primitive.PrepareParams) =
  { p with MethodFilter = "WaitForExitCustom" :: (p.MethodFilter |> Seq.toList)
           AssemblyExcludeFilter = "Adapter" :: (p.AssemblyExcludeFilter |> Seq.toList)
           AssemblyFilter =
             [ "Mono"; @"\.Recorder"; @"\.DataCollector"; "Sample"; "nunit"; "Newton"; "xunit"; "BlackFox" ]
             @ (p.AssemblyFilter |> Seq.toList)
           TypeFilter = [ @"System\."; @"Sample3\.Class2"; "Tests"; "Cecil11" ] @ (p.TypeFilter |> Seq.toList) }

let AltCoverFilterG(p : Primitive.PrepareParams) =
  { p with MethodFilter = "WaitForExitCustom" :: (p.MethodFilter |> Seq.toList)
           AssemblyExcludeFilter =
             [ "Adapter"; "Tests" ] @ (p.AssemblyExcludeFilter |> Seq.toList)
           AssemblyFilter =
             [ "Mono"; @"\.Recorder\.g"; "Sample"; "nunit"; "Newton"; "xunit"; "BlackFox" ]
             @ (p.AssemblyFilter |> Seq.toList)
           TypeFilter = [ @"System\."; @"Sample3\.Class2"; "Cecil11" ] @ (p.TypeFilter |> Seq.toList) }

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

let withWorkingDirectoryVM dir o =
  { dotnetOptions o with WorkingDirectory = Path.getFullName dir
                         Verbosity = Some DotNet.Verbosity.Minimal }
let withWorkingDirectoryOnly dir o =
  { dotnetOptions o with WorkingDirectory = Path.getFullName dir }

let withCLIArgs (o : Fake.DotNet.DotNet.TestOptions) =
  { o with MSBuildParams = cliArguments }
let withMSBuildParams (o : Fake.DotNet.DotNet.BuildOptions) =
  { o with MSBuildParams = cliArguments }

let NuGetAltCover =
  let xml = "./MCS/packages.config" |> Path.getFullName |> XDocument.Load
  xml.Descendants(XName.Get("package"))
  |> Seq.filter(fun x -> x.Attribute(XName.Get("id")).Value.ToLowerInvariant().Equals("altcover"))
  |> Seq.map(fun x -> "./packages/altcover." + x.Attribute(XName.Get("version")).Value + "/tools/net45/AltCover.exe")
  |> Seq.map Path.getFullName
  |> Seq.filter File.Exists
  |> Seq.tryHead

let ForceTrueOnly = DotNet.CLIArgs.Force true
let FailTrue = DotNet.CLIArgs.FailFast true

let GreenSummary = DotNet.CLIArgs.ShowSummary "Green"
let ForceTrue = DotNet.CLIArgs.Many [ ForceTrueOnly; GreenSummary ]

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
    |> DotNet.build
         (fun p ->
         { p.WithCommon dotnetOptions with Configuration =
                                             DotNet.BuildConfiguration.Release }
         |> withMSBuildParams)
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

  Directory.ensure "./_SourceLink"
  Shell.copyFile "./_SourceLink/Class2.cs" "./Sample14/Sample14/Class2.txt"
  if Environment.isWindows then
    let temp = Environment.environVar "TEMP"
    Shell.copyFile (temp @@ "/Sample14.SourceLink.Class3.cs") "./Sample14/Sample14/Class3.txt"
  else
    Directory.ensure "/tmp/.AltCover_SourceLink"
    Shell.copyFile "/tmp/.AltCover_SourceLink/Sample14.SourceLink.Class3.cs" "./Sample14/Sample14/Class3.txt"

  [ "./altcover.core.sln"; "./Sample14/Sample14.sln" ]
  |> Seq.iter (fun s -> s
                        |> DotNet.build
                         (fun p ->
                         { p.WithCommon dotnetOptions with Configuration = DotNet.BuildConfiguration.Debug }
                         |> withMSBuildParams))

  Shell.copy "./_SourceLink" (!!"./Sample14/Sample14/bin/Debug/netcoreapp2.1/*")
)

_Target "AvaloniaDebug" (fun _ ->
  DotNet.restore (fun o -> o.WithCommon(withWorkingDirectoryVM "AltCover.Avalonia")) ""

  "./AltCover.Visualizer/altcover.visualizer.core.sln"
  |> MSBuild.build (fun p ->
       { p with Verbosity = Some MSBuildVerbosity.Normal
                ConsoleLogParameters = []
                DistributedLoggers = None
                DisableInternalBinLog = true
                Properties =
                  [ "Configuration", "Debug"
                    "DebugSymbols", "True" ] })
)

_Target "AvaloniaRelease" (fun _ ->
  DotNet.restore (fun o -> o.WithCommon(withWorkingDirectoryVM "AltCover.Avalonia")) ""

  "./AltCover.Visualizer/altcover.visualizer.core.sln"
  |> MSBuild.build (fun p ->
       { p with Verbosity = Some MSBuildVerbosity.Normal
                ConsoleLogParameters = []
                DistributedLoggers = None
                DisableInternalBinLog = true
                Properties =
                  [ "Configuration", "Release"
                    "DebugSymbols", "True" ] })
)

_Target "BuildMonoSamples" (fun _ ->
  let mcs = "_Binaries/MCS/Release+AnyCPU/MCS.exe"
  [ ("./_Mono/Sample1",
     [ "-debug"; "-out:./_Mono/Sample1/Sample1.exe"; "./Sample1/Program.cs" ])

    ("./_Mono/Sample3",
     [ "-target:library"; "-debug"; "-out:./_Mono/Sample3/Sample3.dll";
       "-lib:./packages/Mono.Cecil.0.10.4/lib/net40"; "-r:Mono.Cecil.dll";
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
  let failOnIssuesFound (issuesFound : bool) =
    Assert.That(issuesFound, Is.False, "Lint issues were found")
  try
    let settings =
      Configuration.SettingsFileName
      |> Path.getFullName
      |> File.ReadAllText

    let lintConfig = FSharpLint.Application.ConfigurationManagement.loadConfigurationFile  settings
    let options =
      { Lint.OptionalLintParameters.Default with Configuration = Some lintConfig }

    !!"**/*.fsproj"
    |> Seq.collect (fun n -> !!(Path.GetDirectoryName n @@ "*.fs"))
    |> Seq.distinct
    |> Seq.map (fun f ->
         match Lint.lintFile options f with
         | Lint.LintResult.Failure x -> failwithf "%A" x
         | Lint.LintResult.Success w ->
           w
           |> Seq.filter (fun x ->
                match x.Fix with
                | None -> false
                | Some fix -> fix.FromText <> "AltCover_Fake")) // special case
    |> Seq.concat
    |> Seq.fold (fun _ x ->
                printfn "Info: %A\r\n Range: %A\r\n Fix: %A\r\n====" x.Info x.Range x.Fix
                true) false
    |> failOnIssuesFound
  with ex ->
    printfn "%A" ex
    reraise())

_Target "Gendarme" (fun _ -> // Needs debug because release is compiled --standalone which contaminates everything
  Directory.ensure "./_Reports"

  let toolPath = (Tools.findToolInSubPath "gendarme.exe" "./packages")
  let rules =
    if Environment.isWindows then "./Build/rules.xml"
    else "./Build/rules-mono.xml"

  let baseRules = Path.getFullName "./Build/rules-fake.xml"
  let fakerules =
    if Environment.isWindows then baseRules
    else
      // Gendarme mono doesn't into .pdb files
      let lines = baseRules
                  |> File.ReadAllLines
                  |> Seq.map (fun l -> l.Replace ("AvoidSwitchStatementsRule", "AvoidSwitchStatementsRule | AvoidLongMethodsRule"))
      let fixup = Path.getFullName  "./_Generated/rules-fake.xml"
      File.WriteAllLines(fixup, lines)
      fixup

  [ (rules,
     [ "_Binaries/AltCover/Debug+AnyCPU/AltCover.exe"
       "_Binaries/AltCover.Shadow/Debug+AnyCPU/AltCover.Shadow.dll" ])

    ("./Build/rules-posh.xml",
     [ "_Binaries/AltCover.PowerShell/Debug+AnyCPU/AltCover.PowerShell.dll"
       "_Binaries/AltCover.FSApi/Debug+AnyCPU/AltCover.FSApi.dll" ])

    ("./Build/rules-gtk.xml",
     [ "_Binaries/AltCover.Visualizer/Debug+AnyCPU/AltCover.Visualizer.exe" ])

    (fakerules,
     ["_Binaries/AltCover.Fake.DotNet.Testing.AltCover/Debug+AnyCPU/AltCover.Fake.DotNet.Testing.AltCover.dll"]) ]
  |> Seq.iter (fun (ruleset, files) ->
       Gendarme.run { Gendarme.Params.Create() with WorkingDirectory = "."
                                                    Severity = Gendarme.Severity.All
                                                    Confidence = Gendarme.Confidence.All
                                                    Configuration = ruleset
                                                    Console = true
                                                    Log = "./_Reports/gendarme.html"
                                                    LogKind = Gendarme.LogKind.Html
                                                    Targets = files
                                                    ToolPath = toolPath
                                                    FailBuildOnDefect = true }))

_Target "FxCop" (fun _ -> // Needs debug because release is compiled --standalone which contaminates everything
  Directory.ensure "./_Reports"

  let rules = [ "-Microsoft.Design#CA1004"
                "-Microsoft.Design#CA1006"
                "-Microsoft.Design#CA1011" // maybe sometimes
                "-Microsoft.Design#CA1062" // null checks,  In F#!
                "-Microsoft.Maintainability#CA1506"
                "-Microsoft.Naming#CA1704"
                "-Microsoft.Naming#CA1707"
                "-Microsoft.Naming#CA1709"
                "-Microsoft.Naming#CA1715"
                "-Microsoft.Usage#CA2208"
                 ]

  [ ([
         "_Binaries/AltCover/Debug+AnyCPU/AltCover.exe"
       ], [ "AltCover.AltCover"
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
            "AltCover.Visitor" ], rules)
    ([
          "_Binaries/AltCover.Shadow/Debug+AnyCPU/AltCover.Shadow.dll"
       ], [ "AltCover.Recorder.Assist"
            "AltCover.Recorder.Counter"
            "AltCover.Recorder.Assist"
            "AltCover.Recorder.Tracer"
            "AltCover.Recorder.Instance" ], rules)
    ([
         "_Binaries/AltCover.PowerShell/Debug+AnyCPU/AltCover.PowerShell.dll"
       ], [], [ "-Microsoft.Design#CA1059"
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
                "-Microsoft.Naming#CA1715" ])
    ([
         "_Binaries/AltCover.FSApi/Debug+AnyCPU/AltCover.FSApi.dll"
       ], [], [ "-Microsoft.Usage#CA2235"
                "-Microsoft.Performance#CA1819"
                "-Microsoft.Design#CA1020"
                "-Microsoft.Design#CA1034"
                "-Microsoft.Design#CA1004"
                "-Microsoft.Design#CA1006"
                "-Microsoft.Design#CA1011"
                "-Microsoft.Design#CA1062"
                "-Microsoft.Maintainability#CA1506"
                "-Microsoft.Naming#CA1704"
                "-Microsoft.Naming#CA1707"
                "-Microsoft.Naming#CA1709"
                "-Microsoft.Naming#CA1715" ])
    ([
         "_Binaries/AltCover.Visualizer/Debug+AnyCPU/AltCover.Visualizer.exe"
       ], [
            "AltCover.Augment"
            "AltCover.Visualizer.Transformer"
            "AltCover.Visualizer.CoverageFile"
            "AltCover.Visualizer.Extensions"
            "AltCover.Visualizer.Gui"
           ], [ "-Microsoft.Usage#CA2208"
                "-Microsoft.Usage#CA2235"
                "-Microsoft.Maintainability#CA1506"
                "-Microsoft.Design#CA1004"
                "-Microsoft.Design#CA1006"
                "-Microsoft.Naming#CA1707"
                "-Microsoft.Naming#CA1715"
                "-Microsoft.Naming#CA1704"
                "-Microsoft.Naming#CA1709" ])
    ([
         "_Binaries/AltCover.Fake.DotNet.Testing.AltCover/Debug+AnyCPU/AltCover.Fake.DotNet.Testing.AltCover.dll"
       ], [
            "AltCover_Fake.DotNet.Testing.AltCover.CollectParams"
            "AltCover_Fake.DotNet.Testing.AltCover.PrepareParams"
            "AltCover_Fake.DotNet.Testing.AltCover.Args"
            "AltCover_Fake.DotNet.Testing.AltCover.ArgType"
            "AltCover_Fake.DotNet.Testing.AltCover.ToolType"
            "AltCover_Fake.DotNet.Testing.AltCover.Params"
            "AltCover_Fake.DotNet.Testing.AltCover.PrepareParams"
            "AltCover_Fake.DotNet.Testing.AltCover"
            "AltCover.Internals.DotNet"
            "AltCover_Fake.DotNet.DotNet"
           ], [ "-Microsoft.Design#CA1006"
                "-Microsoft.Design#CA1011"
                "-Microsoft.Design#CA1020"
                "-Microsoft.Design#CA1062"
                "-Microsoft.Naming#CA1704"
                "-Microsoft.Naming#CA1707"
                "-Microsoft.Naming#CA1709"
                "-Microsoft.Naming#CA1724"
                "-Microsoft.Usage#CA2208" ])
      ]
    |> Seq.iter (fun (files, types, ruleset) -> files
                                                |> FxCop.run { FxCop.Params.Create() with WorkingDirectory = "."
                                                                                          UseGAC = true
                                                                                          Verbose = false
                                                                                          ReportFileName = "_Reports/FxCopReport.xml"
                                                                                          Types = types
                                                                                          Rules = ruleset
                                                                                          FailOnError = FxCop.ErrorLevel.Warning
                                                                                          IgnoreGeneratedCode = true })

  [ "_Binaries/AltCover.PowerShell/Debug+AnyCPU/AltCover.PowerShell.dll" ]
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

_Target "UnitTest" ignore

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
         && Path.GetFileName(f) <> "NUnit3.TestAdapter.dll"
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
  |> Seq.iter
       (DotNet.build
          (fun p ->
          { p.WithCommon dotnetOptions with Configuration =
                                              DotNet.BuildConfiguration.Debug }
          |> withMSBuildParams)))

_Target "UnitTestDotNet" (fun _ ->
  Directory.ensure "./_Reports"
  try
    !!(@"./*Tests/*.tests.core.fsproj")
    |> Seq.iter (DotNet.test (fun p ->
                   { p.WithCommon dotnetOptions with Configuration =
                                                       DotNet.BuildConfiguration.Debug
                                                     NoBuild = true }
                   |> withCLIArgs))
  with x ->
    printfn "%A" x
    reraise())

_Target "BuildForCoverlet" (fun _ ->
  !!(@"./*Tests/*.tests.core.fsproj")
  |> Seq.iter
       (DotNet.build
          (fun p ->
          { p.WithCommon dotnetOptions with Configuration =
                                              DotNet.BuildConfiguration.Debug }
          |> withMSBuildParams)))

_Target "UnitTestDotNetWithCoverlet" (fun _ ->
  Directory.ensure "./_Reports"
  try
    let xml =
      !!(@"./*Tests/*.tests.core.fsproj")
      |> Seq.zip
           [ """/p:CollectCoverage=true /p:CoverletOutputFormat=opencover /p:Exclude="\"[*.Tests]*,[*.XTests]*,[xunit*]*,[Sample*]*,[AltCover.Record*]*,[NUnit*]*,[AltCover.Shadow.Adapter]*\""  """
             """/p:CollectCoverage=true /p:CoverletOutputFormat=opencover /p:Exclude="\"[*.Tests]*,[*.XTests]*,[xunit*]*,[Sample*]*,[AltCover.Record*]*,[NUnit*]*,[AltCover.Shadow.Adapter]*\""  """
             """/p:CollectCoverage=true /p:CoverletOutputFormat=opencover /p:Exclude="\"[*.Tests]*,[*.XTests]*,[xunit*]*,[Sample*]*,[AltCover.Record*]*\""  """ ]
      |> Seq.fold (fun l (p, f) ->
           try
             f
             |> DotNet.test (fun o ->
                  { o.WithCommon(fun c -> { dotnetOptions c with CustomParams = Some p }) with Configuration =
                                                                                                 DotNet.BuildConfiguration.Debug
                                                                                               NoBuild =
                                                                                                 true
                                                                                               Framework =
                                                                                                 Some
                                                                                                   "netcoreapp2.1" }
                  |> withCLIArgs)
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
         && Path.GetFileName(f) <> "NUnit3.TestAdapter.dll"
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
  let shadowkeyfile = Path.getFullName "Build/Infrastructure.snk"
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
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = xaltReport
                                                 OutputDirectories = [| "./__UnitTestWithAltCover" |]
                                                 StrongNameKey = keyfile
                                                 OpenCover = false
                                                 InPlace = false
                                                 Save = false }
         |> AltCoverFilter)
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
    let weakDir = Path.getFullName "_Binaries/AltCover.WeakNameTests/Debug+AnyCPU"
    printfn "Instrumented the code"
    let prep =
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = altReport
                                                 InputDirectories = [| "."; weakDir|]
                                                 OutputDirectories = [| "./__UnitTestWithAltCover"; weakDir @@ "__WeakNameTestWithAltCover" |]
                                                 StrongNameKey = keyfile
                                                 OpenCover = false
                                                 InPlace = false
                                                 Save = false }
        |> AltCoverFilterX)
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
        !!"_Binaries/AltCover.WeakNameTests/Debug+AnyCPU/__WeakNameTestWithAltCover/Alt*Test*.dll"
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

    printfn "Instrument the shadow tests"
    let shadowDir = Path.getFullName "_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU"
    let shadowReport = reports @@ "ShadowTestWithAltCover.xml"

    let prep =
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = shadowReport
                                                 OutputDirectories =
                                                   [| "./__ShadowTestWithAltCover" |]
                                                 StrongNameKey = shadowkeyfile
                                                 InPlace = false
                                                 Save = false }
         |> AltCoverFilter)
      |> AltCover.Prepare
    { AltCover.Params.Create prep with ToolPath = altcover
                                       ToolType = AltCover.ToolType.Framework
                                       WorkingDirectory = shadowDir }
    |> AltCover.run

    printfn "Execute the shadow tests"
    !!("_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU/__ShadowTestWithAltCover/Alt*.Test*.dll")
    |> NUnit3.run (fun p ->
         { p with ToolPath = Tools.findToolInSubPath "nunit3-console.exe" "."
                  WorkingDir = "."
                  ResultSpecs = [ "./_Reports/ShadowTestWithAltCoverReport.xml" ] })

    ReportGenerator.generateReports (fun p ->
      { p with ExePath = Tools.findToolInSubPath "ReportGenerator.exe" "."
               ReportTypes =
                 [ ReportGenerator.ReportType.Html; ReportGenerator.ReportType.XmlSummary ]
               TargetDir = "_Reports/_UnitTestWithAltCover" })
      [ xaltReport; altReport; shadowReport ]
  else printfn "Symbols not present; skipping")

_Target "UnitTestWithAltCoverRunner" (fun _ ->
  Directory.ensure "./_Reports/_UnitTestWithAltCover"
  let keyfile = Path.getFullName "Build/SelfTest.snk"
  let shadowkeyfile = Path.getFullName "Build/Infrastructure.snk"
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
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = xaltReport
                                                 OutputDirectories =
                                                   [| "./__UnitTestWithAltCoverRunner" |]
                                                 StrongNameKey = keyfile
                                                 Single = true
                                                 InPlace = false
                                                 Save = false }
          |> AltCoverFilter)
      |> AltCover.Prepare
    { AltCover.Params.Create prep with ToolPath = altcover
                                       ToolType = AltCover.ToolType.Framework
                                       WorkingDirectory = xtestDirectory }
    |> AltCover.run

    printfn "Unit test the instrumented code"
    try
      let collect =
       AltCover.CollectParams.Primitive
        { Primitive.CollectParams.Create() with Executable = Tools.findToolInSubPath "xunit.console.exe" "."
                                                RecorderDirectory = xtestDirectory @@ "__UnitTestWithAltCoverRunner"
                                                CommandLine =
                                                 [ Path.getFullName
                                                     "_Binaries/AltCover.XTests/Debug+AnyCPU/__UnitTestWithAltCoverRunner/AltCover.XTests.dll"
                                                   "-parallel"
                                                   "none"
                                                   "-noshadow"
                                                   "-nunit"
                                                   "./_Reports/XUnitTestWithAltCoverRunnerReport.xml" ] }
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
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = altReport
                                                 OutputDirectories =
                                                   [| "./__UnitTestWithAltCoverRunner" |]
                                                 StrongNameKey = keyfile
                                                 Single = true
                                                 InPlace = false
                                                 Save = false }
          |> AltCoverFilter)
      |> AltCover.Prepare
    { AltCover.Params.Create prep with ToolPath = altcover
                                       ToolType = AltCover.ToolType.Framework
                                       WorkingDirectory = testDirectory }
    |> AltCover.run

    printfn "Unit test the instrumented code"
    try
      let collect =
       AltCover.CollectParams.Primitive
        { Primitive.CollectParams.Create() with Executable = nunit
                                                RecorderDirectory = testDirectory @@ "__UnitTestWithAltCoverRunner"
                                                CommandLine =
                                                 [ "--noheader";
                                                   "--work=.";
                                                   "--result=./_Reports/UnitTestWithAltCoverRunnerReport.xml";
                                                   Path.getFullName
                                                     "_Binaries/AltCover.Tests/Debug+AnyCPU/__UnitTestWithAltCoverRunner/AltCover.Tests.dll";
                                                   Path.getFullName
                                                    "_Binaries/AltCover.Tests/Debug+AnyCPU/__UnitTestWithAltCoverRunner/Sample2.dll" ]}
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
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = weakReport
                                                 OutputDirectories =
                                                   [| "./__WeakNameTestWithAltCoverRunner" |]
                                                 TypeFilter = [ "WeakNameTest" ]
                                                 StrongNameKey = keyfile
                                                 Single = true
                                                 InPlace = false
                                                 Save = false }
          |> AltCoverFilterX)
      |> AltCover.Prepare
    { AltCover.Params.Create prep with ToolPath = altcover
                                       ToolType = AltCover.ToolType.Framework
                                       WorkingDirectory = weakDir }
    |> AltCover.run

    printfn "Execute the weakname tests"
    let collect =
     AltCover.CollectParams.Primitive
      { Primitive.CollectParams.Create() with Executable = nunit
                                              RecorderDirectory = weakDir @@ "__WeakNameTestWithAltCoverRunner"
                                              CommandLine  =
                                               [ "--noheader"
                                                 "--work=."
                                                 "--result=./_Reports/ShadowTestWithAltCoverRunnerReport.xml"
                                                 Path.getFullName
                                                   "_Binaries/AltCover.WeakNameTests/Debug+AnyCPU/__WeakNameTestWithAltCoverRunner/AltCover.WeakNameTests.dll" ] }
     |> AltCover.Collect
    { AltCover.Params.Create collect with ToolPath = altcover
                                          ToolType = AltCover.ToolType.Framework
                                          WorkingDirectory = "." }
    |> AltCover.run

    printfn "Instrument the shadow tests"
    let shadowDir = Path.getFullName "_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU"
    let shadowReport = reports @@ "ShadowTestWithAltCoverRunner.xml"

    let prep =
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = shadowReport
                                                 OutputDirectories =
                                                   [| "./__ShadowTestWithAltCoverRunner" |]
                                                 StrongNameKey = shadowkeyfile
                                                 InPlace = false
                                                 Save = false }
          |> AltCoverFilter)
      |> AltCover.Prepare
    { AltCover.Params.Create prep with ToolPath = altcover
                                       ToolType = AltCover.ToolType.Framework
                                       WorkingDirectory = shadowDir }
    |> AltCover.run

    let collect =
     AltCover.CollectParams.Primitive
      { Primitive.CollectParams.Create() with Executable = nunit
                                              RecorderDirectory = shadowDir @@ "__ShadowTestWithAltCoverRunner"
                                              CommandLine =
                                               [ "--noheader";
                                                 "--work=.";
                                                 "--result=./_Reports/ShadowTestWithAltCoverRunnerReport.xml";
                                                 Path.getFullName
                                                    "_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU/__ShadowTestWithAltCoverRunner/AltCover.Shadow.Tests.dll";
                                                  Path.getFullName
                                                   "_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU/__ShadowTestWithAltCoverRunner/AltCover.Shadow.Tests2.dll" ]}
      |> AltCover.Collect
    { AltCover.Params.Create collect with ToolPath = altcover
                                          ToolType = AltCover.ToolType.Framework
                                          WorkingDirectory = "." }
    |> AltCover.run
    printfn "Instrument the GTK# visualizer tests"
    let gtkDir = Path.getFullName "_Binaries/AltCover.Tests.Visualizer/Debug+AnyCPU"
    let gtkReport = reports @@ "GTKVTestWithAltCoverRunner.xml"

    let prep =
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = gtkReport
                                                 OutputDirectories =
                                                   [| "./__GTKVTestWithAltCoverRunner" |]
                                                 TypeFilter = [ "Gui" ]
                                                 AssemblyFilter = [ "\\-sharp" ]
                                                 StrongNameKey = keyfile
                                                 InPlace = false
                                                 Save = false }
          |> AltCoverFilter)
      |> AltCover.Prepare
    { AltCover.Params.Create prep with ToolPath = altcover
                                       ToolType = AltCover.ToolType.Framework
                                       WorkingDirectory = gtkDir }
    |> AltCover.run

    printfn "Execute the the GTK# visualizer tests"
    let collect =
     AltCover.CollectParams.Primitive
      { Primitive.CollectParams.Create() with Executable = nunit
                                              RecorderDirectory = gtkDir @@ "__GTKVTestWithAltCoverRunner"
                                              CommandLine =
                                               [ "--noheader"
                                                 "--work=."
                                                 "--result=./_Reports/GTKVTestWithAltCoverRunnerReport.xml"
                                                 Path.getFullName
                                                   "_Binaries/AltCover.Tests.Visualizer/Debug+AnyCPU/__GTKVTestWithAltCoverRunner/AltCover.Tests.Visualizer.dll" ]}
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
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = altReport
                                                 OutputDirectories = [| output |]
                                                 StrongNameKey = keyfile
                                                 OpenCover = false
                                                 InPlace = false
                                                 Save = false }
        |> AltCoverFilter)
    |> AltCover.Prepare
  { AltCover.Params.Create prep with ToolPath = altcover
                                     ToolType = AltCover.ToolType.Framework
                                     WorkingDirectory = testDirectory }
  |> AltCover.run

  printfn "Unit test the instrumented code"
  try
    "altcover.tests.core.fsproj"
    |> DotNet.test (fun p ->
         { p.WithCommon(withWorkingDirectoryVM "Tests") with Configuration =
                                                               DotNet.BuildConfiguration.Debug
                                                             NoBuild =
                                                               true }
         |> withCLIArgs)
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
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = shadowReport
                                                 OutputDirectories = [| shadowOut |]
                                                 StrongNameKey = keyfile
                                                 OpenCover = false
                                                 InPlace = false
                                                 Save = false }
        |> AltCoverFilterG)
    |> AltCover.Prepare
  { AltCover.Params.Create prep with ToolPath = altcover
                                     ToolType = AltCover.ToolType.Framework
                                     WorkingDirectory = shadowDir }
  |> AltCover.run

  printfn "Execute the shadow tests"
  "altcover.recorder.tests.core.fsproj"
  |> DotNet.test (fun p ->
       { p.WithCommon(withWorkingDirectoryVM "Shadow.Tests") with Configuration =
                                                                    DotNet.BuildConfiguration.Debug
                                                                  NoBuild =
                                                                    true }
       |> withCLIArgs)

  printfn "Instrument the XUnit tests"
  let xDir = "_Binaries/AltCover.XTests/Debug+AnyCPU/netcoreapp2.1"
  let xReport = reports @@ "XTestWithAltCoverCore.xml"
  let xOut =
    Path.getFullName "XTests/_Binaries/AltCover.XTests/Debug+AnyCPU/netcoreapp2.1"

  let prep =
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = xReport
                                                 OutputDirectories = [| xOut |]
                                                 StrongNameKey = keyfile
                                                 OpenCover = false
                                                 InPlace = false
                                                 Save = false }
        |> AltCoverFilterG)
    |> AltCover.Prepare
  { AltCover.Params.Create prep with ToolPath = altcover
                                     ToolType = AltCover.ToolType.Framework
                                     WorkingDirectory = xDir }
  |> AltCover.run

  printfn "Execute the XUnit tests"
  "altcover.x.tests.core.fsproj"
  |> DotNet.test (fun p ->
       { p.WithCommon(withWorkingDirectoryVM "XTests") with Configuration =
                                                              DotNet.BuildConfiguration.Debug
                                                            NoBuild =
                                                              true }
       |> withCLIArgs)

  ReportGenerator.generateReports (fun p ->
    { p with ExePath = Tools.findToolInSubPath "ReportGenerator.exe" "."
             ReportTypes =
               [ ReportGenerator.ReportType.Html; ReportGenerator.ReportType.XmlSummary ]
             TargetDir = "_Reports/_UnitTestWithAltCoverCore" })
    [ altReport; shadowReport; xReport ])

_Target "UnitTestWithAltCoverCoreRunner"
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
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = altReport
                                                 OutputDirectories = [| output |]
                                                 Single = true
                                                 InPlace = false
                                                 Save = false }
        |> AltCoverFilter)
    |> AltCover.Prepare
  { AltCover.Params.Create prep with ToolPath = altcover
                                     ToolType = AltCover.ToolType.DotNet dotnetPath
                                     WorkingDirectory = testDirectory }
  |> AltCover.run
  printfn "Unit test the instrumented code"

  let testproject = Path.getFullName "./Tests/altcover.tests.core.fsproj"

  let collect =
   AltCover.CollectParams.Primitive
    { Primitive.CollectParams.Create() with Executable = "dotnet"
                                            RecorderDirectory = output
                                            CommandLine =
                                              [ "test"
                                                "--no-build"
                                                "--configuration"
                                                "Debug"
                                                "--verbosity"
                                                "normal"
                                                testproject ] }
    |> AltCover.Collect
  { AltCover.Params.Create collect with ToolPath = altcover
                                        ToolType = AltCover.ToolType.DotNet dotnetPath
                                        WorkingDirectory = output }
  |> AltCover.run

  printfn "Instrument the shadow tests"
  let shadowDir = "_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU/netcoreapp2.1"
  let shadowReport = reports @@ "ShadowTestWithAltCoverCoreRunner.xml"
  let shadowOut =
    Path.getFullName
      "Shadow.Tests/_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU/netcoreapp2.1"
  Shell.cleanDir shadowOut
  let prep =
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = shadowReport
                                                 OutputDirectories = [| shadowOut |]
                                                 Single = true
                                                 InPlace = false
                                                 Save = false }
        |> AltCoverFilter)
    |> AltCover.Prepare
  { AltCover.Params.Create prep with ToolPath = altcover
                                     ToolType = AltCover.ToolType.DotNet dotnetPath
                                     WorkingDirectory = shadowDir }
  |> AltCover.run

  let shadowProject =
    Path.getFullName "./Shadow.Tests/altcover.recorder.tests.core.fsproj"

  let collect =
   AltCover.CollectParams.Primitive
    { Primitive.CollectParams.Create() with Executable = "dotnet"
                                            RecorderDirectory = shadowOut
                                            CommandLine =
                                             [ "test"
                                               "--no-build"
                                               "--configuration"
                                               "Debug"
                                               "--verbosity"
                                               "normal"
                                               shadowProject ] }
    |> AltCover.Collect
  { AltCover.Params.Create collect with ToolPath = altcover
                                        ToolType = AltCover.ToolType.DotNet dotnetPath
                                        WorkingDirectory = shadowOut }
  |> AltCover.run

  printfn "Instrument the XUnit tests"
  let xDir = "_Binaries/AltCover.XTests/Debug+AnyCPU/netcoreapp2.1"
  let xReport = reports @@ "XTestWithAltCoverCoreRunner.xml"
  let xOut =
    Path.getFullName "XTests/_Binaries/AltCover.XTests/Debug+AnyCPU/netcoreapp2.1"
  Shell.cleanDir xOut

  let prep =
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = xReport
                                                 OutputDirectories = [| xOut |]
                                                 Single = true
                                                 InPlace = false
                                                 Save = false }
        |> AltCoverFilter)
    |> AltCover.Prepare
  { AltCover.Params.Create prep with ToolPath = altcover
                                     ToolType = AltCover.ToolType.DotNet dotnetPath
                                     WorkingDirectory = xDir }
  |> AltCover.run

  printfn "Execute the XUnit tests"
  let xProject = Path.getFullName "./XTests/altcover.x.tests.core.fsproj"

  let collect =
   AltCover.CollectParams.Primitive
    { Primitive.CollectParams.Create() with Executable = "dotnet"
                                            RecorderDirectory = xOut
                                            CommandLine =
                                             [ "test"
                                               "--no-build"
                                               "--configuration"
                                               "Debug"
                                               "--verbosity"
                                               "normal"
                                               xProject ] }
    |> AltCover.Collect
  { AltCover.Params.Create collect with ToolPath = altcover
                                        ToolType = AltCover.ToolType.DotNet dotnetPath
                                        WorkingDirectory = xOut }
  |> AltCover.run

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
    let prep =
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = simpleReport
                                                 OutputDirectories = [| "./" + instrumented |]
                                                 AssemblyFilter = [ "Adapter"; "nunit" ]
                                                 TypeFilter = [ "System\\."; "Microsoft\\." ]
                                                 InPlace = false
                                                 OpenCover = false
                                                 Save = false })
      |> AltCover.Prepare
    { AltCover.Params.Create prep with ToolPath = binRoot @@ "AltCover.exe"
                                       ToolType = AltCover.ToolType.Framework
                                       WorkingDirectory = sampleRoot }
    |> AltCover.run
    Actions.ValidateFSharpTypes simpleReport []
  else printfn "Symbols not present; skipping")

_Target "FSharpTypesDotNet" (fun _ -> // obsolete
  Directory.ensure "./_Reports"
  let altcover =
    Path.getFullName "./_Binaries/AltCover/Release+AnyCPU/netcoreapp2.0/AltCover.dll"
  let simpleReport = (Path.getFullName "./_Reports") @@ ("AltCoverFSharpTypesDotNet.xml")
  let sampleRoot = Path.getFullName "Sample2/_Binaries/Sample2/Debug+AnyCPU/netcoreapp2.1"

  // Test the --inplace operation
  Shell.cleanDir sampleRoot
  "sample2.core.fsproj"
  |> DotNet.test
       (fun o ->
       { o.WithCommon(withWorkingDirectoryVM "Sample2") with Configuration =
                                                              DotNet.BuildConfiguration.Debug }
       |> withCLIArgs)

  let prep =
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = simpleReport
                                                 AssemblyFilter = [ "Adapter" ]
                                                 TypeFilter = [ "System\\."; "Microsoft\\." ]
                                                 InPlace = true
                                                 OpenCover = false
                                                 Save = false })
    |> AltCover.Prepare
  { AltCover.Params.Create prep with ToolPath = altcover
                                     ToolType = AltCover.ToolType.DotNet dotnetPath
                                     WorkingDirectory = sampleRoot }
  |> AltCover.run

  Actions.ValidateFSharpTypes simpleReport [ "main" ]
  Assert.That(Path.Combine(sampleRoot, "__Saved") |> Directory.Exists)

  printfn "Execute the instrumented tests"
  "sample2.core.fsproj"
  |> DotNet.test (fun o ->
       { o.WithCommon(withWorkingDirectoryVM "Sample2") with Configuration =
                                                               DotNet.BuildConfiguration.Debug
                                                             NoBuild =
                                                               true }
       |> withCLIArgs)
  Actions.ValidateFSharpTypesCoverage simpleReport)

_Target "FSharpTests" (fun _ ->
  Directory.ensure "./_Reports"
  let altcover =
    Path.getFullName "./_Binaries/AltCover/Release+AnyCPU/netcoreapp2.0/AltCover.dll"
  let simpleReport = (Path.getFullName "./_Reports") @@ ("AltCoverFSharpTests.xml")
  let sampleRoot = Path.getFullName "Sample7/_Binaries/Sample7/Debug+AnyCPU/netcoreapp2.0"

  // Test the --inplace operation
  Shell.cleanDir sampleRoot
  "sample7.core.fsproj"
  |> DotNet.test
       (fun o ->
       { o.WithCommon(withWorkingDirectoryVM "Sample7") with Configuration =
                                                               DotNet.BuildConfiguration.Debug }
       |> withCLIArgs)

  // inplace instrument
  let prep =
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = simpleReport
                                                 CallContext = [ "[Test]" ]
                                                 AssemblyFilter = [ "Adapter" ]
                                                 TypeFilter = [ "System\\."; "Microsoft\\." ]
                                                 InPlace = true
                                                 OpenCover = true
                                                 Save = false })
    |> AltCover.Prepare
  { AltCover.Params.Create prep with ToolPath = altcover
                                     ToolType = AltCover.ToolType.DotNet dotnetPath
                                     WorkingDirectory = sampleRoot }
  |> AltCover.run

  printfn "Execute the instrumented tests"
  "sample7.core.fsproj"
  |> DotNet.test (fun o ->
       { o.WithCommon(withWorkingDirectoryVM "Sample7") with Configuration =
                                                               DotNet.BuildConfiguration.Debug
                                                             NoBuild =
                                                               true }
       |> withCLIArgs))

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
  let prep =
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = simpleReport
                                                 OutputDirectories = [ instrumented ]
                                                 AssemblyFilter = [ "Adapter" ]
                                                 TypeFilter = [ "System\\."; "Microsoft\\." ]
                                                 InPlace = false
                                                 OpenCover = false
                                                 Save = false })
    |> AltCover.Prepare
  { AltCover.Params.Create prep with ToolPath = altcover
                                     ToolType = AltCover.ToolType.DotNet dotnetPath
                                     WorkingDirectory = sampleRoot }
  |> AltCover.run

  Actions.ValidateFSharpTypes simpleReport [ "main" ]

  printfn "Execute the instrumented tests"
  let sample2 = Path.getFullName "./Sample2/sample2.core.fsproj"

  let collect =
   AltCover.CollectParams.Primitive
    { Primitive.CollectParams.Create() with Executable = "dotnet"
                                            RecorderDirectory = instrumented
                                            CommandLine = [ "test"; "--no-build"; "--configuration"; "Debug"; sample2 ] }
    |> AltCover.Collect
  { AltCover.Params.Create collect with ToolPath = altcover
                                        ToolType = AltCover.ToolType.DotNet dotnetPath
                                        WorkingDirectory = instrumented }
  |> AltCover.run

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
  "sample2.core.fsproj"
  |> DotNet.test
       (fun o ->
       { o.WithCommon(withWorkingDirectoryVM "Sample2") with Configuration =
                                                               DotNet.BuildConfiguration.Debug }
       |> withCLIArgs)

  // inplace instrument and save
  let prep =
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = simpleReport
                                                 AssemblyFilter = [ "Adapter" ]
                                                 TypeFilter = [ "System\\."; "Microsoft\\." ]
                                                 InPlace = true
                                                 OpenCover = false
                                                 Save = true })
    |> AltCover.Prepare
  { AltCover.Params.Create prep with ToolPath = altcover
                                     ToolType = AltCover.ToolType.DotNet dotnetPath
                                     WorkingDirectory = sampleRoot }
  |> AltCover.run
  Actions.ValidateFSharpTypes simpleReport [ "main" ]
  Assert.That(Path.Combine(sampleRoot, "__Saved") |> Directory.Exists)

  printfn "Execute the instrumented tests"
  "sample2.core.fsproj"
  |> DotNet.test (fun o ->
       { o.WithCommon(withWorkingDirectoryVM "Sample2") with Configuration =
                                                               DotNet.BuildConfiguration.Debug
                                                             NoBuild =
                                                               true }
       |> withCLIArgs)

  let collect =
   AltCover.CollectParams.Primitive
    { Primitive.CollectParams.Create() with RecorderDirectory = sampleRoot }
    |> AltCover.Collect
  { AltCover.Params.Create collect with ToolPath = altcover
                                        ToolType = AltCover.ToolType.DotNet dotnetPath
                                        WorkingDirectory = sampleRoot }
  |> AltCover.run
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

  let prep =
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = x
                                                 OutputDirectories = [ o ]
                                                 InputDirectories = [ i ]
                                                 TypeFilter = [ "System\\." ]
                                                 InPlace = false
                                                 OpenCover = false
                                                 Save = false })
    |> AltCover.Prepare
  { AltCover.Params.Create prep with ToolPath = altcover
                                     ToolType = AltCover.ToolType.DotNet dotnetPath
                                     WorkingDirectory = "." }
  |> AltCover.run

  Actions.Run (o @@ "/Sample1.exe", o, []) "Instrumented .exe failed"
  Actions.ValidateSample1 "./_Reports/CSharpMonoWithDotNet.xml" "CSharpMonoWithDotNet")

_Target "CSharpDotNetWithDotNet"
  (fun _ ->
  Directory.ensure "./_Reports"
  let x = Path.getFullName "./_Reports/CSharpDotNetWithDotNet.xml"
  let o = Path.getFullName "./_Binaries/Sample1/__Instrumented.CSharpDotNetWithDotNet"
  let i = Path.getFullName "./_Binaries/Sample1/Debug+AnyCPU/netcoreapp2.0"

  let prep =
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = x
                                                 OutputDirectories = [ o ]
                                                 InputDirectories = [ i ]
                                                 TypeFilter = [ "System\\." ]
                                                 InPlace = false
                                                 OpenCover = false
                                                 Save = false })
    |> AltCover.Prepare
  { AltCover.Params.Create prep with ToolPath =
                                       "_Binaries/AltCover/Debug+AnyCPU/netcoreapp2.0/AltCover.dll"
                                     ToolType = AltCover.ToolType.DotNet dotnetPath
                                     WorkingDirectory = "." }
  |> AltCover.run

  Actions.RunDotnet dotnetOptions (o @@ "Sample1.dll") "" "CSharpDotNetWithDotNet test"
  Actions.ValidateSample1 "./_Reports/CSharpDotNetWithDotNet.xml" "CSharpDotNetWithDotNet")

_Target "CSharpDotNetWithFramework" (fun _ ->
  Directory.ensure "./_Reports"
  let simpleReport = (Path.getFullName "./_Reports") @@ ("CSharpDotNetWithFramework.xml")
  let binRoot = Path.getFullName "_Binaries/AltCover/Release+AnyCPU"
  let sampleRoot = Path.getFullName "_Binaries/Sample1/Debug+AnyCPU/netcoreapp2.0"
  let instrumented =
    Path.getFullName "_Binaries/Sample1/__Instrumented.CSharpDotNetWithFramework"

  let prep =
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = simpleReport
                                                 OutputDirectories = [| instrumented |]
                                                 TypeFilter = [ "System\\."; "Microsoft\\." ]
                                                 InPlace = false
                                                 OpenCover = false
                                                 Save = false })
    |> AltCover.Prepare
  { AltCover.Params.Create prep with ToolPath = binRoot @@ "AltCover.exe"
                                     ToolType = AltCover.ToolType.Framework
                                     WorkingDirectory = sampleRoot }
  |> AltCover.run

  Actions.RunDotnet dotnetOptions (instrumented @@ "Sample1.dll") ""
    "CSharpDotNetWithFramework test"
  Actions.ValidateSample1 simpleReport "CSharpDotNetWithFramework")

_Target "SelfTest" (fun _ ->
  Directory.ensure "./_Reports/_Instrumented"
  let targetDir = "_Binaries/AltCover.Tests/Debug+AnyCPU"
  let reports = Path.getFullName "./_Reports"
  let report = reports @@ "OpenCoverSelfTest.xml"
  let altReport = reports @@ "AltCoverSelfTest.xml"
  let keyfile = Path.getFullName "Build/SelfTest.snk"

  printfn "Self-instrument under OpenCover"
  let prep =
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = altReport
                                                 OutputDirectories = [| "__SelfTest" |]
                                                 StrongNameKey = keyfile
                                                 OpenCover = false
                                                 InPlace = false
                                                 Save = false })
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
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = altReport2
                                                 OutputDirectories = [| "./__SelfTestDummy" |]
                                                 StrongNameKey = keyfile
                                                 OpenCover = false
                                                 InPlace = false
                                                 Save = false }
        |> AltCoverFilter)
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

  let prep =
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = simpleReport
                                                 OutputDirectories = [ instrumented ]
                                                 TypeFilter = [ "System\\."; "Microsoft\\." ]
                                                 AssemblyFilter = [ "Adapter"; "nunit" ]
                                                 InPlace = false
                                                 OpenCover = false
                                                 Save = false })
    |> AltCover.Prepare
  { AltCover.Params.Create prep with ToolPath = binRoot @@ "AltCover.exe"
                                     ToolType = AltCover.ToolType.Framework
                                     WorkingDirectory = sampleRoot }
  |> AltCover.run
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

  let collect =
   AltCover.CollectParams.Primitive
    { Primitive.CollectParams.Create() with RecorderDirectory = instrumented }
    |> AltCover.Collect
  { AltCover.Params.Create collect with ToolPath = binRoot @@ "AltCover.exe"
                                        ToolType = AltCover.ToolType.Framework
                                        WorkingDirectory = sampleRoot }
  |> AltCover.run

  do use coverageFile =
       new FileStream(simpleReport, FileMode.Open, FileAccess.Read, FileShare.None, 4096,
                      FileOptions.SequentialScan)
     let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))

     let recorded =
       coverageDocument.Descendants(XName.Get("seqpnt"))
       |> Seq.map (fun x -> x.Attribute(XName.Get("visitcount")).Value)
       |> Seq.toList

     Assert.That
       (recorded |> Seq.length, Is.EqualTo 20,
        sprintf "Bad visit list %A -- bad length" recorded)

     let hits = recorded |> Seq.filter (fun i -> i = "1") |> Seq.length
     Assert.That (hits, Is.GreaterThanOrEqualTo 6)
     Assert.That (hits, Is.LessThanOrEqualTo 8))

_Target "RecordResumeTrackingTest" (fun _ ->
  Directory.ensure "./_Reports"
  let simpleReport = (Path.getFullName "./_Reports") @@ ("RecordResumeTrackingTest.xml")
  let binRoot = Path.getFullName "_Binaries/AltCover/Release+AnyCPU"
  let sampleRoot = Path.getFullName "_Binaries/Sample8/Debug+AnyCPU"
  let instrumented = "__RecordResumeTrackingTest"

  let prep =
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = simpleReport
                                                 OutputDirectories = [ instrumented ]
                                                 CallContext = [ "Main" ]
                                                 TypeFilter = [ "System\\."; "Microsoft\\." ]
                                                 AssemblyFilter = [ "Adapter"; "nunit" ]
                                                 InPlace = false
                                                 OpenCover = true
                                                 Save = false })
    |> AltCover.Prepare
  { AltCover.Params.Create prep with ToolPath = binRoot @@ "AltCover.exe"
                                     ToolType = AltCover.ToolType.Framework
                                     WorkingDirectory = sampleRoot }
  |> AltCover.run
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

  let collect =
   AltCover.CollectParams.Primitive
    { Primitive.CollectParams.Create() with RecorderDirectory = instrumented }
    |> AltCover.Collect
  { AltCover.Params.Create collect with ToolPath = binRoot @@ "AltCover.exe"
                                        ToolType = AltCover.ToolType.Framework
                                        WorkingDirectory = sampleRoot }
  |> AltCover.run

  do use coverageFile =
       new FileStream(simpleReport, FileMode.Open, FileAccess.Read, FileShare.None, 4096,
                      FileOptions.SequentialScan)
     let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))

     let recorded =
       coverageDocument.Descendants(XName.Get("SequencePoint"))
       |> Seq.map (fun x -> x.Attribute(XName.Get("vc")).Value)
       |> Seq.toList
     Assert.That
       (recorded |> Seq.length, Is.EqualTo 20,
        sprintf "Bad visit list %A -- bad length" recorded)

     let hits = recorded |> Seq.filter (fun i -> i = "1") |> Seq.length
     Assert.That (hits, Is.GreaterThanOrEqualTo 6)
     Assert.That (hits, Is.LessThanOrEqualTo 8)
     let tracked =
       coverageDocument.Descendants(XName.Get("TrackedMethodRef")) |> Seq.toList
     Assert.That(tracked, Is.Not.Empty))

_Target "RecordResumeTestDotNet"
  (fun _ ->
  Directory.ensure "./_Reports"
  let simpleReport = (Path.getFullName "./_Reports") @@ ("RecordResumeTestDotNet.xml")
  let binRoot = Path.getFullName "_Binaries/AltCover/Release+AnyCPU/netcoreapp2.0"
  let sampleRoot = Path.getFullName "_Binaries/Sample8/Debug+AnyCPU/netcoreapp2.0"
  let instrumented = "__RecordResumeTestDotNet"

  let prep =
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = simpleReport
                                                 OutputDirectories = [ instrumented ]
                                                 TypeFilter = [ "System\\."; "Microsoft\\." ]
                                                 AssemblyFilter = [ "Adapter"; "nunit" ]
                                                 InPlace = false
                                                 OpenCover = false
                                                 Save = false })
    |> AltCover.Prepare
  { AltCover.Params.Create prep with ToolPath = binRoot @@ "AltCover.dll"
                                     ToolType = AltCover.ToolType.DotNet dotnetPath
                                     WorkingDirectory = sampleRoot }
  |> AltCover.run

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

  let collect =
   AltCover.CollectParams.Primitive
    { Primitive.CollectParams.Create() with RecorderDirectory = instrumented }
    |> AltCover.Collect
  { AltCover.Params.Create collect with ToolPath = binRoot @@ "AltCover.dll"
                                        ToolType = AltCover.ToolType.DotNet dotnetPath
                                        WorkingDirectory = sampleRoot }
  |> AltCover.run

  do use coverageFile =
       new FileStream(simpleReport, FileMode.Open, FileAccess.Read, FileShare.None, 4096,
                      FileOptions.SequentialScan)
     let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))

     let recorded =
       coverageDocument.Descendants(XName.Get("seqpnt"))
       |> Seq.map (fun x -> x.Attribute(XName.Get("visitcount")).Value)
       |> Seq.toList

     Assert.That
       (recorded |> Seq.length, Is.EqualTo 20,
        sprintf "Bad visit list %A -- bad length" recorded)

     let hits = recorded |> Seq.filter (fun i -> i = "1") |> Seq.length
     Assert.That (hits, Is.GreaterThanOrEqualTo 6)
     Assert.That (hits, Is.LessThanOrEqualTo 8))

_Target "RecordResumeTestUnderMono" // Fails : System.EntryPointNotFoundException: CreateZStream
  (fun _ ->
  Directory.ensure "./_Reports"
  let simpleReport = (Path.getFullName "./_Reports") @@ ("RecordResumeTestUnderMono.xml")
  let binRoot = Path.getFullName "_Binaries/AltCover/Release+AnyCPU"
  let sampleRoot = Path.getFullName "_Binaries/Sample8/Debug+AnyCPU"
  let instrumented = "__RecordResumeTestUnderMono"

  let prep =
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = simpleReport
                                                 OutputDirectories = [ instrumented ]
                                                 TypeFilter = [ "System\\."; "Microsoft\\." ]
                                                 AssemblyFilter = [ "Adapter"; "nunit" ]
                                                 InPlace = false
                                                 OpenCover = false
                                                 Save = false })
    |> AltCover.Prepare
  { AltCover.Params.Create prep with ToolPath = binRoot @@ "AltCover.exe"
                                     ToolType = AltCover.ToolType.Framework
                                     WorkingDirectory = sampleRoot }
  |> AltCover.run

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

  let collect =
   AltCover.CollectParams.Primitive
    { Primitive.CollectParams.Create() with RecorderDirectory = instrumented }
    |> AltCover.Collect
  { AltCover.Params.Create collect with ToolPath = binRoot @@ "AltCover.exe"
                                        ToolType = AltCover.ToolType.Framework
                                        WorkingDirectory = sampleRoot }
  |> AltCover.run

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
  let AltCover = Path.getFullName "_Binaries/AltCover/Release+AnyCPU/AltCover.exe"
  let config = AltCover + ".config"
  let fox = Path.getFullName "_Binaries/AltCover/Release+AnyCPU/BlackFox.CommandLine.dll"
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

  let libFiles path =
    Seq.concat
      [ !!"./_Binaries/AltCover/Release+AnyCPU/Mono.C*.dll"
        !!"./_Binaries/AltCover/Release+AnyCPU/Newton*.dll" ]
    |> Seq.map (fun f -> (f |> Path.getFullName, Some path, None))
    |> Seq.toList

  let applicationFiles =
      [ (AltCover, Some "tools/net45", None)
        (config, Some "tools/net45", None)
        (recorder, Some "tools/net45", None)
        (posh, Some "tools/net45", None)
        (fsapi, Some "tools/net45", None)
        (vis, Some "tools/net45", None)
        (fscore, Some "tools/net45", None)
        (fox, Some "tools/net45", None)
        (options, Some "tools/net45", None)
        (packable, Some "", None) ]

  let apiFiles =
      [ (AltCover, Some "lib/net45", None)
        (config, Some "lib/net45", None)
        (recorder, Some "lib/net45", None)
        (posh, Some "lib/net45", None)
        (fsapi, Some "lib/net45", None)
        (csapi, Some "lib/net45", None)
        (cake, Some "lib/net45", None)
        (fake, Some "lib/net45", None)
        (fscore, Some "lib/net45", None)
        (fox, Some "lib/net45", None)
        (options, Some "lib/net45", None)
        (packable, Some "", None) ]

  let resourceFiles path =
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

  let dataFiles where =
    (!!"./_Binaries/AltCover.DataCollector/Release+AnyCPU/netstandard2.0/AltCover.D*.*")
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

  let fox2Files where =
    (!!"./_Publish/BlackFox.*")
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
         || n.StartsWith("BlackFox.", StringComparison.Ordinal)
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
                   libFiles "tools/net45/"
                   netcoreFiles "tools/netcoreapp2.0/"
                   poshFiles "tools/netcoreapp2.0/"
                   vizFiles "tools/netcoreapp2.1"
                   dataFiles "tools/netcoreapp2.0/"
                   otherFiles ], "_Packaging", "./Build/AltCover.nuspec", "altcover")

    (List.concat [ apiFiles
                   resourceFiles "lib/net45/"
                   libFiles "lib/net45/"
                   netstdFiles "lib/netstandard2.0"
                   cakeFiles "lib/netstandard2.0/"
                   dataFiles "lib/netstandard2.0/"
                   fakeFiles "lib/netstandard2.0/"
                   poshFiles "lib/netstandard2.0/"
                   vizFiles "tools/netcoreapp2.1"
                   otherFilesApi ], "_Packaging.api", "./_Generated/altcover.api.nuspec",
     "altcover.api")

    (List.concat [ netcoreFiles "lib/netcoreapp2.0"
                   poshFiles "lib/netcoreapp2.0/"
                   dataFiles "lib/netcoreapp2.0/"
                   [ (packable, Some "", None) ]
                   dotnetFiles
                   otherFilesDotnet ], "_Packaging.dotnet",
     "./_Generated/altcover.dotnet.nuspec", "altcover.dotnet")

    (List.concat [ globalFiles
                   netcoreFiles "tools/netcoreapp2.1/any"
                   poshFiles "tools/netcoreapp2.1/any/"
                   dataFiles "tools/netcoreapp2.1/any/"
                   [ (packable, Some "", None) ]
                   auxFiles
                   otherFilesGlobal ], "_Packaging.global",
     "./_Generated/altcover.global.nuspec", "altcover.global")

    (List.concat [ vizFiles "tools/netcoreapp2.1/any"
                   [ (packable, Some "", None) ]
                   auxVFiles ], "_Packaging.visualizer",
     "./_Generated/altcover.visualizer.nuspec", "altcover.visualizer")

    (List.concat [ fake2Files "lib/netstandard2.0/"
                   fox2Files "lib/netstandard2.0/"
                   [ (packable, Some "", None) ]
                   [ (fake2, Some "lib/net45", None)
                     (fox, Some "lib/net45", None) ] ], "_Packaging.fake",
     "./_Generated/altcover.fake.nuspec", "altcover.fake") ]
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

_Target "PrepareFrameworkBuild" ignore

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

     // dotnet tooling mods
  [ ("DotnetCliTool", "./_Generated/altcover.dotnet.nuspec",
     "AltCover (dotnet CLI tool install)", None, None)

    ("DotnetTool", "./_Generated/altcover.global.nuspec",
     "AltCover (dotnet global tool install)", None, None)

    ("DotnetTool", "./_Generated/altcover.visualizer.nuspec",
     "AltCover.Visualizer (dotnet global tool install)",
     Some "AltCover.Visualizer/logo.png", Some "codecoverage .netcore cross-platform")

    (String.Empty, "./_Generated/altcover.api.nuspec", "AltCover (API install)", None,
     None)

    (String.Empty, "./_Generated/altcover.fake.nuspec", "AltCover (FAKE task helpers)",
     None, Some "codecoverage .net Mono .netcore cross-platform FAKE build") ]
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

_Target "WindowsPowerShell" (fun _ ->
  let v = (!Version).Split([| '-' |]).[0]
  CreateProcess.fromRawCommand "powershell.exe"
    [ "-NoProfile"; "./Build/powershell.ps1"; "-ACV"; v ]
  |> CreateProcess.withWorkingDirectory "."
  |> Proc.run
  |> (Actions.AssertResult "powershell"))

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

  let prep =
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = report
                                                 InputDirectories = [ i ]
                                                 StrongNameKey = key
                                                 TypeFilter = [ "System\\."; "DotNet" ]
                                                 AssemblyFilter = [ "^AltCover$"; "Recorder"; "DataCollector" ]
                                                 InPlace = true
                                                 OpenCover = true
                                                 Save = true })
    |> AltCover.Prepare
  { AltCover.Params.Create prep with ToolPath = retro
                                     ToolType = AltCover.ToolType.Framework
                                     WorkingDirectory = unpack }
  |> AltCover.run

  printfn "Execute the instrumented tests"
  CreateProcess.fromRawCommand pwsh [ "-NoProfile"; "./Build/pester.ps1"; "-ACV"; v ]
  |> CreateProcess.withWorkingDirectory "."
  |> Proc.run
  |> (Actions.AssertResult "pwsh")

  let collect =
   AltCover.CollectParams.Primitive
    { Primitive.CollectParams.Create() with RecorderDirectory = i } |> AltCover.Collect
  { AltCover.Params.Create collect with ToolPath = "AltCover.dll"
                                        ToolType = AltCover.ToolType.DotNet dotnetPath
                                        WorkingDirectory = unpack }
  |> AltCover.run
  ReportGenerator.generateReports (fun p ->
    { p with ExePath = Tools.findToolInSubPath "ReportGenerator.exe" "."
             ReportTypes =
               [ ReportGenerator.ReportType.Html; ReportGenerator.ReportType.XmlSummary ]
             TargetDir = "_Reports/_Pester" }) [ report ]

  "_Reports/_Pester/Summary.xml"
  |> File.ReadAllText
  |> printfn "%s")

_Target "SimpleReleaseTest" (fun _ ->
  let unpack = match NuGetAltCover with
               | Some test -> Trace.traceImportant "Using the NuGet package"
                              Path.GetDirectoryName test
               | _ -> Path.getFullName "_Packaging/Unpack/tools/net45"

  Actions.SimpleInstrumentingRun "_Binaries/Sample1/Debug+AnyCPU" unpack
    "SimpleReleaseTest")

_Target "SimpleMonoReleaseTest" (fun _ ->
  let unpack = match NuGetAltCover with
               | Some test -> Trace.traceImportant "Using the NuGet package"
                              Path.GetDirectoryName test
               | _ -> Path.getFullName "_Packaging/Unpack/tools/net45"

  Actions.SimpleInstrumentingRun "_Mono/Sample1" unpack "SimpleMonoReleaseTest")

_Target "ReleaseDotNetWithFramework" (fun _ ->
  Directory.ensure "./_Reports"
  let unpack = match NuGetAltCover with
               | Some test -> Trace.traceImportant "Using the NuGet package"
                              Path.GetDirectoryName test
               | _ -> Path.getFullName "_Packaging/Unpack/tools/net45"

  let simpleReport =
    (Path.getFullName "./_Reports") @@ ("ReleaseDotNetWithFramework.xml")
  let sampleRoot = Path.getFullName "./_Binaries/Sample1/Debug+AnyCPU/netcoreapp2.0"
  let instrumented = sampleRoot @@ "__Instrumented.ReleaseDotNetWithFramework"

  let prep =
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = simpleReport
                                                 OutputDirectories = [ instrumented ]
                                                 TypeFilter = [ "System\\."; "Microsoft\\." ]
                                                 InPlace = false
                                                 OpenCover = false
                                                 Save = false })
    |> AltCover.Prepare
  { AltCover.Params.Create prep with ToolPath = (unpack @@ "AltCover.exe")
                                     ToolType = AltCover.ToolType.Framework
                                     WorkingDirectory = sampleRoot }
  |> AltCover.run

  Actions.RunDotnet (fun o -> { dotnetOptions o with WorkingDirectory = instrumented })
    "Sample1.dll" "" "ReleaseDotNetWithFramework test"

  Actions.ValidateSample1 "./_Reports/ReleaseDotNetWithFramework.xml"
    "ReleaseDotNetWithFramework")

_Target "ReleaseMonoWithDotNet" (fun _ ->
  Directory.ensure "./_Reports"
  let unpack = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0"
  let x = Path.getFullName "./_Reports/ReleaseMonoWithDotNet.xml"
  let o = Path.getFullName "./_Mono/__Instrumented.ReleaseMonoWithDotNet"
  let i = Path.getFullName "./_Mono/Sample1"

  let prep =
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = x
                                                 OutputDirectories = [ o ]
                                                 InputDirectories = [ i ]
                                                 InPlace = false
                                                 OpenCover = false
                                                 Save = false })
    |> AltCover.Prepare
  { AltCover.Params.Create prep with ToolPath = "AltCover.dll"
                                     ToolType = AltCover.ToolType.DotNet dotnetPath
                                     WorkingDirectory = unpack }
  |> AltCover.run

  Actions.Run (o @@ "Sample1.exe", o, []) "Instrumented .exe failed"
  Actions.ValidateSample1 "./_Reports/ReleaseMonoWithDotNet.xml" "ReleaseMonoWithDotNet")

_Target "ReleaseDotNetWithDotNet"
  (fun _ ->
  Directory.ensure "./_Reports"
  let unpack = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0"
  let x = Path.getFullName "./_Reports/ReleaseDotNetWithDotNet.xml"
  let o = Path.getFullName "./_Binaries/Sample1/__Instrumented.ReleaseDotNetWithDotNet"
  let i = Path.getFullName "./_Binaries/Sample1/Debug+AnyCPU/netcoreapp2.0"

  let prep =
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = x
                                                 OutputDirectories = [ o ]
                                                 InputDirectories = [ i ]
                                                 InPlace = false
                                                 OpenCover = false
                                                 Save = false })
    |> AltCover.Prepare
  { AltCover.Params.Create prep with ToolPath = "AltCover.dll"
                                     ToolType = AltCover.ToolType.DotNet dotnetPath
                                     WorkingDirectory = unpack }
  |> AltCover.run
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
  let prep =
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = x
                                                 OutputDirectories = [ o ]
                                                 InputDirectories = [ i ]
                                                 AssemblyFilter = [ "Adapter" ]
                                                 InPlace = false
                                                 OpenCover = false
                                                 Save = false })
    |> AltCover.Prepare
  { AltCover.Params.Create prep with ToolPath = "AltCover.dll"
                                     ToolType = AltCover.ToolType.DotNet dotnetPath
                                     WorkingDirectory = unpack }
  |> AltCover.run

  Actions.ValidateFSharpTypes x [ "main" ]

  printfn "Execute the instrumented tests"
  let sample2 = Path.getFullName "./Sample2/sample2.core.fsproj"
  let runner = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0/AltCover.dll"

  // Run
  let collect =
   AltCover.CollectParams.Primitive
    { Primitive.CollectParams.Create() with Executable = "dotnet"
                                            RecorderDirectory = o
                                            CommandLine = [ "test"; "--no-build"; "--configuration"; "Debug"; sample2 ] }
    |> AltCover.Collect
  { AltCover.Params.Create collect with ToolPath = runner
                                        ToolType = AltCover.ToolType.DotNet dotnetPath
                                        WorkingDirectory = o }
  |> AltCover.run
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
      DotNet.info (fun o' ->
        { o' with Common =
                    { o'.Common with WorkingDirectory = s
                                     DotNetCliPath = dotnetPath86 |> Option.get } })
      |> printfn "%A"

      printfn "Build the sample2 code as x86"
      "./altcover.core.sln"
      |> DotNet.build (fun p ->
           { p with Configuration = DotNet.BuildConfiguration.Debug
                    Common =
                      { p.Common with WorkingDirectory = s
                                      DotNetCliPath = dotnetPath86 |> Option.get }
                    MSBuildParams = cliArguments })

      printfn "Instrument the code"
      let altcover = unpack @@ "AltCover.dll"

      let prep =
        AltCover.PrepareParams.Primitive
          ({ Primitive.PrepareParams.Create() with XmlReport = x
                                                   OutputDirectories = [ o ]
                                                   InputDirectories = [ i ]
                                                   AssemblyFilter = [ "Adapter" ]
                                                   InPlace = false
                                                   OpenCover = false
                                                   Save = false })
        |> AltCover.Prepare
      { AltCover.Params.Create prep with ToolPath = altcover
                                         ToolType = AltCover.ToolType.DotNet dotnetPath86
                                         WorkingDirectory = unpack }
      |> AltCover.run
      Actions.ValidateFSharpTypes x [ "main" ]
      printfn "Execute the instrumented tests"
      let sample2 = Path.getFullName "./Sample2/sample2.core.fsproj"

      // Run
      let collect =
       AltCover.CollectParams.Primitive
        { Primitive.CollectParams.Create() with Executable = dotnetPath86 |> Option.get
                                                RecorderDirectory = o
                                                CommandLine= [ "test"; "--no-build"; "--configuration"; "Debug"; sample2 ] }
        |> AltCover.Collect
      { AltCover.Params.Create collect with ToolPath = altcover
                                            ToolType =
                                              AltCover.ToolType.DotNet dotnetPath86
                                            WorkingDirectory = o }
      |> AltCover.run

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
  let prep =
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = x
                                                 OutputDirectories = [ o ]
                                                 InputDirectories = [ i ]
                                                 AssemblyFilter = [ "xunit" ]
                                                 InPlace = false
                                                 OpenCover = false
                                                 Save = false })
    |> AltCover.Prepare
  { AltCover.Params.Create prep with ToolPath = "AltCover.dll"
                                     ToolType = AltCover.ToolType.DotNet dotnetPath
                                     WorkingDirectory = unpack }
  |> AltCover.run

  Actions.ValidateFSharpTypes x [ "main" ]

  printfn "Execute the instrumented tests"
  "sample4.core.fsproj"
  |> DotNet.test (fun o ->
       { o.WithCommon(withWorkingDirectoryVM "Sample4") with Configuration =
                                                               DotNet.BuildConfiguration.Debug
                                                             NoBuild =
                                                               true }
       |> withCLIArgs)
  Actions.ValidateFSharpTypesCoverage x)

_Target "ReleaseXUnitFSharpTypesDotNetRunner" (fun _ ->
  Directory.ensure "./_Reports"
  let unpack = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0"
  let x = Path.getFullName "./_Reports/ReleaseXUnitFSharpTypesDotNetRunner.xml"
  let o = Path.getFullName "Sample4/_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.1"
  let i = Path.getFullName "_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.1"

  Shell.cleanDir o

  // Instrument the code
  let prep =
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = x
                                                 OutputDirectories = [ o ]
                                                 InputDirectories = [ i ]
                                                 AssemblyFilter = [ "xunit" ]
                                                 InPlace = false
                                                 OpenCover = false
                                                 Save = false })
    |> AltCover.Prepare
  { AltCover.Params.Create prep with ToolPath = "AltCover.dll"
                                     ToolType = AltCover.ToolType.DotNet dotnetPath
                                     WorkingDirectory = unpack }
  |> AltCover.run

  Actions.ValidateFSharpTypes x [ "main" ]

  printfn "Execute the instrumented tests"
  let sample4 = Path.getFullName "./Sample4/sample4.core.fsproj"
  let runner = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0/AltCover.dll"

  // Run
  let collect =
   AltCover.CollectParams.Primitive
    { Primitive.CollectParams.Create() with Executable = "dotnet"
                                            RecorderDirectory = o
                                            CommandLine = [ "test"; "--no-build"; "--configuration"; "Debug"; sample4 ] }
    |> AltCover.Collect
  { AltCover.Params.Create collect with ToolPath = runner
                                        ToolType = AltCover.ToolType.DotNet dotnetPath
                                        WorkingDirectory = o }
  |> AltCover.run
  Actions.ValidateFSharpTypesCoverage x)

_Target "ReleaseXUnitFSharpTypesDotNetFullRunner" (fun _ ->
  Directory.ensure "./_Reports"
  let unpack = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0"
  let x = Path.getFullName "./_Reports/ReleaseXUnitFSharpTypesDotNetFullRunner.xml"
  let o = Path.getFullName "Sample4/_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.1"
  let i = Path.getFullName "_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.1"

  Shell.cleanDir o
  let prep =
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = x
                                                 OutputDirectories = [ o ]
                                                 InputDirectories = [ i ]
                                                 CallContext = [ "0"; "[Fact]" ]
                                                 AssemblyFilter = [ "xunit" ]
                                                 InPlace = false
                                                 OpenCover = true
                                                 Save = false })
    |> AltCover.Prepare
  { AltCover.Params.Create prep with ToolPath = "AltCover.dll"
                                     ToolType = AltCover.ToolType.DotNet dotnetPath
                                     WorkingDirectory = unpack }
  |> AltCover.run
  Actions.CheckSample4Content x

  printfn "Execute the instrumented tests"
  let sample4 = Path.getFullName "./Sample4/sample4.core.fsproj"
  let runner = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0/AltCover.dll"

  // Run
  let collect =
   AltCover.CollectParams.Primitive
    { Primitive.CollectParams.Create() with Executable = "dotnet"
                                            RecorderDirectory = o
                                            CommandLine = [ "test"; "--no-build"; "--configuration"; "Debug"; sample4 ] }
    |> AltCover.Collect
  { AltCover.Params.Create collect with ToolPath = runner
                                        ToolType = AltCover.ToolType.DotNet dotnetPath
                                        WorkingDirectory = o }
  |> AltCover.run
  Actions.CheckSample4Visits x)

_Target "MSBuildTest" (fun _ ->
  Directory.ensure "./_Reports"
  let build = Path.getFullName "Build"
  let sample = Path.getFullName "Sample4"
  let x = Path.getFullName "./_Reports/MSBuildTest.xml"

  // Run
  Shell.cleanDir (sample @@ "_Binaries")
  DotNet.msbuild
    (fun opt ->
    opt.WithCommon(fun o' -> { dotnetOptions o' with WorkingDirectory = sample }))
    (build @@ "msbuildtest.proj")
  printfn "Checking samples4 output"
  Actions.CheckSample4 x

  // touch-test framework
  let unpack = match NuGetAltCover with
               | Some test -> Trace.traceImportant "Using the NuGet package"
                              Path.GetDirectoryName test
               | _ -> Path.getFullName "_Packaging/Unpack/tools/net45"

  MSBuild.build (fun p ->
    { p with Verbosity = Some MSBuildVerbosity.Minimal
             Properties =
               [ "Configuration", "Debug"
                 "MSBuildTest", "true"
                 "AltCoverPath", unpack.Replace('\\', '/')
                 "DebugSymbols", "True" ] }) "./Sample4/Sample4.fsproj")

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
    <add key="local2" value="{1}" />
    <add key="nuget.org" value="https://api.nuget.org/v3/index.json" />
  </packageSources>
</configuration>"""

    File.WriteAllText
      ("./_ApiUse/NuGet.config",
       String.Format
         (config, Path.getFullName "./_Packaging.api",
          Path.getFullName "./_Packaging.fake"))
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

  let collect = FSApi.CollectParams.Primitive { AltCover.Primitive.CollectParams.Create() with LcovReport = "x" }
  let prepare = FSApi.PrepareParams.Primitive { AltCover.Primitive.PrepareParams.Create() with TypeFilter = [| "a"; "b" |] }
  let ForceTrue = DotNet.CLIArgs.Force true
  printfn "%s" (DotNet.ToTestArguments prepare collect ForceTrue)

  let t = DotNet.TestOptions.Create().WithParameters prepare collect ForceTrue
  printfn "returned '%A'" t.Common.CustomParams

  let p2 = { Primitive.PrepareParams.Create() with CallContext = [| "[Fact]"; "0" |]
                                                   AssemblyFilter = [| "xunit" |] }
  let pp2 = FSApi.PrepareParams.Primitive p2
  let c2 = Primitive.CollectParams.Create()
  let cc2 = FSApi.CollectParams.Primitive c2

  let setBaseOptions (o : DotNet.Options) =
    { o with WorkingDirectory = Path.getFullName "./_DotnetTest"
             Verbosity = Some DotNet.Verbosity.Minimal }

  let cliArguments =
    { MSBuild.CliArguments.Create() with ConsoleLogParameters = []
                                         DistributedLoggers = None
                                         DisableInternalBinLog = true }

  DotNet.test
    (fun to' ->
    { to'.WithCommon(setBaseOptions).WithParameters pp2 cc2 ForceTrue with MSBuildParams =
                                                                             cliArguments })
    "dotnettest.fsproj"
  let ipmo = AltCover.Api.Ipmo().Trim().Split().[1].Trim([| '\"' |])
  let command = "$ipmo = '" + ipmo + "'; Import-Module $ipmo; ConvertTo-BarChart -?"

  let corePath = AltCover.Fake.Api.toolPath AltCover.Fake.Implementation.DotNetCore
  printfn "corePath = %A" corePath
  let frameworkPath = AltCover.Fake.Api.toolPath AltCover.Fake.Implementation.Framework
  printfn "frameworkPath = %A" frameworkPath

  { AltCover_Fake.DotNet.Testing.AltCover.Params.Create
      AltCover_Fake.DotNet.Testing.AltCover.ArgType.GetVersion with ToolPath = frameworkPath
                                                                    ToolType =
                                                                      AltCover_Fake.DotNet.Testing.AltCover.ToolType.Framework }
  |> AltCover_Fake.DotNet.Testing.AltCover.run

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

    let dependencies = """version 5.216.0
// [ FAKE GROUP ]
group NetcoreBuild
  source https://api.nuget.org/v3/index.json
  nuget Fake.Core >= 5.8.4
  nuget Fake.Core.Target >= 5.16
  nuget Fake.DotNet.Cli >= 5.16
  nuget FSharp.Core = 4.7

  source {0}
  nuget AltCover.Api {1}

  source {2}
  nuget AltCover.Fake {1} """
    File.WriteAllText
      ("./_ApiUse/paket.dependencies",
       String.Format
         (dependencies, Path.getFullName "./_Packaging.api", !Version,
          Path.getFullName "./_Packaging.fake"))
    Shell.copy "./_ApiUse" (!!"./dotnet*.fsproj")

    DotNet.restore (fun o -> o.WithCommon(withWorkingDirectoryVM "_ApiUse")) ""
    Actions.RunDotnet (withWorkingDirectoryOnly "_ApiUse")
      "fake" "run ./DriveApi.fsx"
      "running fake script returned with a non-zero exit code"

    let x = Path.getFullName "./_ApiUse/_DotnetTest/coverage.xml"
    Actions.CheckSample4 x
  finally
    [ "altcover"; "altcover.api" ; "altcover.fake" ]
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

    Directory.ensure "./_DotnetTestFail"
    Shell.cleanDir ("./_DotnetTestFail")
    let config = XDocument.Load "./Build/NuGet.config.dotnettest"
    let repo = config.Descendants(XName.Get("add")) |> Seq.head
    repo.SetAttributeValue(XName.Get "value", Path.getFullName "./_Packaging")
    config.Save "./_DotnetTestFail/NuGet.config"

    let fsproj = XDocument.Load "./Sample4/sample4.core.fsproj"
    let pack = fsproj.Descendants(XName.Get("PackageReference")) |> Seq.head
    let inject =
      XElement
        (XName.Get "PackageReference", XAttribute(XName.Get "Include", "altcover"),
         XAttribute(XName.Get "Version", !Version))
    pack.AddBeforeSelf inject
    fsproj.Save "./_DotnetTest/dotnettest.fsproj"
    Shell.copy "./_DotnetTest" (!!"./Sample4/*.fs")

    let p0 = Primitive.PrepareParams.Create()
    let pp0 = AltCover.PrepareParams.Primitive p0
    let c0 = Primitive.CollectParams.Create()
    let p1 = { p0 with CallContext = [ "[Fact]"; "0" ]
                       AssemblyFilter = [| "xunit" |] }
    let pp1 = AltCover.PrepareParams.Primitive p1
    let cc0 = AltCover.CollectParams.Primitive { c0 with SummaryFormat = "+B" }
    DotNet.test
      (fun to' ->
      (to'.WithCommon(withWorkingDirectoryVM "_DotnetTest")
          .WithGetVersion().WithImportModule()).WithParameters pp1 cc0 ForceTrue |> withCLIArgs)
      "dotnettest.fsproj"

    let x = Path.getFullName "./_DotnetTest/coverage.xml"
    Actions.CheckSample4 x

    // optest failing test
    let fsproj = XDocument.Load "./Sample13/sample13.core.fsproj"
    let pack = fsproj.Descendants(XName.Get("PackageReference")) |> Seq.head
    let inject =
      XElement
        (XName.Get "PackageReference", XAttribute(XName.Get "Include", "altcover"),
         XAttribute(XName.Get "Version", !Version))
    pack.AddBeforeSelf inject
    fsproj.Save "./_DotnetTestFail/dotnettest.fsproj"
    Shell.copy "./_DotnetTestFail" (!!"./Sample13/*.fs")

    let xx = Path.getFullName "./_DotnetTestFail/coverage.xml"
    let pf1 = { p0 with AssemblyFilter = [| "NUnit" |] } |> AltCover.PrepareParams.Primitive

    try
      DotNet.test
        (fun to' ->
        (to'.WithCommon(withWorkingDirectoryVM "_DotnetTestFail")).WithParameters pf1 cc0 ForceTrue |> withCLIArgs)
        "dotnettest.fsproj"
      Assert.Fail("Build exception should be raised")
    with
    | :? Fake.DotNet.MSBuildException -> printfn "Caught expected exception"

    do use coverageFile =
         new FileStream(xx, FileMode.Open, FileAccess.Read, FileShare.None, 4096,
                        FileOptions.SequentialScan)
       let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))
       let recorded =
         coverageDocument.Descendants(XName.Get("SequencePoint"))
         |> Seq.map (fun x -> x.Attribute(XName.Get("vc")).Value)
         |> Seq.toList

       Assert.That
         (recorded,
          Is.EquivalentTo [ "1"; "1"; "1"; "0"] )

    // optest failing fast test
    Directory.ensure "./_DotnetTestFailFast"
    Shell.cleanDir ("./_DotnetTestFailFast")
    let fsproj = XDocument.Load "./Sample13/sample13.core.fsproj"
    let pack = fsproj.Descendants(XName.Get("PackageReference")) |> Seq.head
    let inject =
      XElement
        (XName.Get "PackageReference", XAttribute(XName.Get "Include", "altcover"),
         XAttribute(XName.Get "Version", !Version))
    pack.AddBeforeSelf inject
    fsproj.Save "./_DotnetTestFailFast/dotnettest.fsproj"
    Shell.copy "./_DotnetTestFailFast" (!!"./Sample13/*.fs")

    let xx = Path.getFullName "./_DotnetTestFailFast/coverage.xml"
    let pf1 = { p0 with AssemblyFilter = [| "NUnit" |] } |> AltCover.PrepareParams.Primitive

    try
      DotNet.test
        (fun to' ->
        (to'.WithCommon(withWorkingDirectoryVM "_DotnetTestFailFast")).WithParameters pf1 cc0 FailTrue |> withCLIArgs)
        "dotnettest.fsproj"
      Assert.Fail("Build exception should be raised")
    with
    | :? Fake.DotNet.MSBuildException -> printfn "Caught expected exception"

    do use coverageFile =
         new FileStream(xx, FileMode.Open, FileAccess.Read, FileShare.None, 4096,
                        FileOptions.SequentialScan)
       let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))
       let recorded =
         coverageDocument.Descendants(XName.Get("SequencePoint"))
         |> Seq.map (fun x -> x.Attribute(XName.Get("vc")).Value)
         |> Seq.toList

       Assert.That
         (recorded,
          Is.EquivalentTo [ "0"; "0"; "0"; "0"] )

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

    let p2 = { p0 with LineCover = true
                       AssemblyFilter = [| "xunit" |] }
    let pp2 = AltCover.PrepareParams.Primitive p2
    DotNet.test
      (fun to' ->
      to'.WithCommon(withWorkingDirectoryVM "_DotnetTestLineCover").WithParameters
        pp2 cc0 ForceTrue |> withCLIArgs) ""

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

    let p3 = { p0 with BranchCover = true
                       AssemblyFilter = [| "xunit" |] }
    let pp3 = AltCover.PrepareParams.Primitive p3

    DotNet.test
      (fun to' ->
      (to'.WithCommon(withWorkingDirectoryVM "_DotnetTestBranchCover").WithParameters
         pp3 cc0 ForceTrue) |> withCLIArgs) ""

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

      DotNet.test
        (fun to' ->
        (to'.WithCommon(withWorkingDirectoryVM "RegressionTesting/issue29").WithParameters
           pp0 cc0 ForceTrue) |> withCLIArgs) ""

    let proj = XDocument.Load "./RegressionTesting/issue37/issue37.xml"
    let pack = proj.Descendants(XName.Get("PackageReference")) |> Seq.head
    let inject =
      XElement
        (XName.Get "PackageReference", XAttribute(XName.Get "Include", "altcover"),
         XAttribute(XName.Get "Version", !Version))
    pack.AddBeforeSelf inject
    proj.Save "./RegressionTesting/issue37/issue37.csproj"

    let p4 = { p0 with AssemblyFilter = [ "NUnit" ] }
    let pp4 = AltCover.PrepareParams.Primitive p4
    DotNet.test
      (fun to' ->
      { ((to'.WithCommon
            (withWorkingDirectoryVM "RegressionTesting/issue37")).WithParameters
           pp4 cc0 ForceTrue) with Configuration = DotNet.BuildConfiguration.Release } |> withCLIArgs)
      ""

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
      (fun o -> o.WithCommon(withWorkingDirectoryVM "./RegressionTesting/issue20/classlib")) ""
    DotNet.restore
      (fun o -> o.WithCommon(withWorkingDirectoryVM "./RegressionTesting/issue20/xunit-tests")) ""

    // would like to assert "succeeds with warnings"
    let p0 = { Primitive.PrepareParams.Create() with AssemblyFilter = [| "xunit" |] }
    let pp0 = AltCover.PrepareParams.Primitive p0
    let c0 = Primitive.CollectParams.Create()
    let cc0 = AltCover.CollectParams.Primitive c0
    DotNet.test (fun to' ->
      ({ to'.WithCommon(withWorkingDirectoryVM "./RegressionTesting/issue20/xunit-tests") with Configuration =
                                                                                                 DotNet.BuildConfiguration.Debug
                                                                                               NoBuild =
                                                                                                 false }).WithParameters
        pp0 cc0 ForceTrue
      |> withCLIArgs) ""

  //let shared =
  //  if Environment.isWindows then
  //    [ "%ProgramFiles%/dotnet/shared/Microsoft.AspNetCore.App/2.1.5/Microsoft.AspNetCore.Cryptography.KeyDerivation.dll" ]
  //  else
  //    [ "/usr/share/dotnet/shared/Microsoft.AspNetCore.App/2.1.5/Microsoft.AspNetCore.Cryptography.KeyDerivation.dll" ]
  //Path.getFullName "./RegressionTesting/issue20/xunit-tests/bin" |> Shell.cleanDir
  //// would like to assert "no warnings"
  //let p1 = { p0 with Dependencies = shared }
  //DotNet.test
  //  (fun to' ->
  //  { to'.WithCommon(fun c ->
  //      { c with WorkingDirectory =
  //                 Path.getFullName"./RegressionTesting/issue20/xunit-tests"
  //               Verbosity = SomeDotNet.Verbosity.Minimal }).WithParameters p1 c0 ForceTrue with Configuration =
  //                                                                                                 DotNet.BuildConfiguration.Debug
  //                                                                                               NoBuild =
  //                                                                                                 false
  //                                                                                               MSBuildParams =
  //                                                                                                 cliArguments })
  //  ""
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
    DotNet.restore (fun o -> o.WithCommon(withWorkingDirectoryVM "_Issue23")) ""

    let p0 = { Primitive.PrepareParams.Create() with AssemblyFilter = [| "xunit" |] }
    let pp0 = AltCover.PrepareParams.Primitive p0
    let c0 = Primitive.CollectParams.Create()
    let cc0 = AltCover.CollectParams.Primitive c0
    DotNet.test (fun p ->
      (({ p.WithCommon(withWorkingDirectoryVM "_Issue23") with Configuration = DotNet.BuildConfiguration.Debug
                                                               NoBuild = false }).WithParameters pp0 cc0 ForceTrue)
        .WithImportModule().WithGetVersion()
      |> withCLIArgs) ""
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
    ""
    |> DotNet.build (fun p ->
         { p with Configuration = DotNet.BuildConfiguration.Debug
                  Common = { dotnetOptions p.Common with WorkingDirectory = working }
                  MSBuildParams = cliArguments })

    let x = Path.getFullName "./_Reports/DotnetCLIIntegration.xml"
    let o =
      Path.getFullName "./_DotnetCLITest/_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.1"

    { AltCover.Params.Create AltCover.ArgType.ImportModule with ToolPath = "altcover"
                                                                ToolType =
                                                                  AltCover.ToolType.DotNet
                                                                    dotnetPath
                                                                WorkingDirectory = working }
    |> AltCover.run
    { AltCover.Params.Create AltCover.ArgType.GetVersion with ToolPath = "altcover"
                                                              ToolType =
                                                                AltCover.ToolType.DotNet
                                                                  dotnetPath
                                                              WorkingDirectory = working }
    |> AltCover.run
    let prep =
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = x
                                                 InputDirectories = [ o ]
                                                 CallContext = [ "0"; "[Fact]" ]
                                                 AssemblyFilter = [| "xunit" |]
                                                 Save = false })
      |> AltCover.Prepare
    { AltCover.Params.Create prep with ToolPath = "altcover"
                                       ToolType = AltCover.ToolType.DotNet dotnetPath
                                       WorkingDirectory = working }
    |> AltCover.run

    Actions.CheckSample4Content x

    printfn "Execute the instrumented tests"
    let collect =
     AltCover.CollectParams.Primitive
      { Primitive.CollectParams.Create() with Executable = "dotnet"
                                              RecorderDirectory = o
                                              CommandLine = [ "test"; "--no-build"; "--configuration"; "Debug" ] }
      |> AltCover.Collect
    { AltCover.Params.Create collect with ToolPath = "altcover"
                                          ToolType = AltCover.ToolType.DotNet dotnetPath
                                          WorkingDirectory = working }
    |> AltCover.run

    Actions.CheckSample4Visits x

    let command =
      """$ipmo = (dotnet altcover ipmo | Out-String).Trim().Split()[1].Trim(@([char]34)); Import-Module $ipmo; ConvertTo-BarChart -?"""
    CreateProcess.fromRawCommand pwsh [ "-NoProfile"; "-Command"; command ]
    |> CreateProcess.withWorkingDirectory working
    |> Proc.run
    |> (Actions.AssertResult "pwsh")

    Actions.RunDotnet
      (fun o' ->
      { dotnetOptions o' with WorkingDirectory = Path.getFullName "_DotnetCLITest" })
      "add" ("package altcover.dotnet --version " + !Version)
      "sample test returned with a non-zero exit code"

    // Shell.cleanDir ("./_DotnetCLITest/_Binaries")
    let p0 = { Primitive.PrepareParams.Create() with AssemblyFilter = [| "xunit" |] }
    let pp0 = AltCover.PrepareParams.Primitive p0
    let c0 = Primitive.CollectParams.Create()
    let cc0 = AltCover.CollectParams.Primitive c0
    DotNet.test (fun to' ->
      { to'.WithCommon(withWorkingDirectoryVM "_DotnetCLITest").WithParameters pp0 cc0 ForceTrue with Configuration =
                                                                                                        DotNet.BuildConfiguration.Debug
                                                                                                      NoBuild =
                                                                                                        false }
      |> withCLIArgs) ""

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

    ""
    |> DotNet.build (fun p ->
         { p with Configuration = DotNet.BuildConfiguration.Debug
                  Common = { dotnetOptions p.Common with WorkingDirectory = working }
                  MSBuildParams = cliArguments })

    let x = Path.getFullName "./_Reports/DotnetGlobalIntegration.xml"
    let o =
      Path.getFullName "./_DotnetGlobalTest/_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.1"
    AltCover.Params.Create AltCover.ArgType.ImportModule |> AltCover.run
    AltCover.Params.Create AltCover.ArgType.GetVersion |> AltCover.run
    let prep =
      AltCover.PrepareParams.Primitive
        ({ Primitive.PrepareParams.Create() with XmlReport = x
                                                 InputDirectories = [ o ]
                                                 CallContext = [ "0"; "[Fact]" ]
                                                 AssemblyFilter = [| "xunit" |]
                                                 Save = false })
      |> AltCover.Prepare
    { AltCover.Params.Create prep with WorkingDirectory = working } |> AltCover.run

    Actions.CheckSample4Content x

    printfn "Execute the instrumented tests"
    let collect =
     AltCover.CollectParams.Primitive
      { Primitive.CollectParams.Create() with Executable = "dotnet"
                                              RecorderDirectory = o
                                              CommandLine = [ "test"; "--no-build"; "--configuration"; "Debug" ] }
      |> AltCover.Collect
    { AltCover.Params.Create collect with WorkingDirectory = working } |> AltCover.run

    Actions.CheckSample4Visits x
    let command =
      """$ipmo = (altcover ipmo | Out-String).Trim().Split()[1].Trim(@([char]34)); Import-Module $ipmo; ConvertTo-BarChart -?"""
    CreateProcess.fromRawCommand pwsh [ "-NoProfile"; "-Command"; command ]
    |> CreateProcess.withWorkingDirectory working
    |> Proc.run
    |> (Actions.AssertResult "pwsh")

  // Would be nice to do this, but we can't `dotnet add` a global tool
  // Actually we would crib the modernized versions from "DotnetCLIIntegration"
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
                TargetDir = "_Reports/_BulkReport" })

  let misses = ref 0
  let numbers =
    !!(@"_Reports/_Unit*/Summary.xml")
    |> Seq.collect (fun f ->
         let xml = XDocument.Load f
         xml.Descendants(XName.Get("Linecoverage"))
         |> Seq.filter (fun x -> match String.IsNullOrWhiteSpace x.Value with
                                 | false -> true
                                 | _ -> sprintf "No coverage from '%s'" f |> Trace.traceImportant
                                        misses := 1 + !misses
                                        false)
         |> Seq.map (fun e ->
              let coverage = e.Value.Split('%').[0]
              match Double.TryParse coverage with
              | (false, _) ->
                printfn "%A" xml
                Assert.Fail("Could not parse coverage '" + e.Value + "'")
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
     || !misses > 1
  then Assert.Fail("Coverage is too low"))

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

"UnitTestWithAltCoverRunner"
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
==> "BulkReport"
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