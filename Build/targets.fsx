// Downloads/docker-machine-Windows-x86_64 create --driver virtualbox <name>

open System
open System.Diagnostics.Tracing
open System.IO
open System.Reflection
open System.Xml
open System.Xml.Linq

open Actions
open AltCode.Fake.DotNet
open AltCoverFake.DotNet.DotNet
open AltCoverFake.DotNet.Testing

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
open Fake.Tools.Git

open FSharpLint.Application
open FSharpLint.Framework
open NUnit.Framework

let Copyright = ref String.Empty
let Version = ref String.Empty
let consoleBefore = (Console.ForegroundColor, Console.BackgroundColor)

let AltCoverFilter(p : Primitive.PrepareOptions) =
  { p with
      MethodFilter = "WaitForExitCustom" :: (p.MethodFilter |> Seq.toList)
      AssemblyExcludeFilter = "Tests" :: (p.AssemblyExcludeFilter |> Seq.toList)
      AssemblyFilter = [ @"\.DataCollector"; "Sample" ] @ (p.AssemblyFilter |> Seq.toList)
      LocalSource = true
      TypeFilter =
        [ @"System\."; @"Sample3\.Class2"; "Microsoft"; "ICSharpCode" ] @ (p.TypeFilter |> Seq.toList) }

let AltCoverFilterTypeSafe(p : TypeSafe.PrepareOptions) =
  { p with
      MethodFilter = [TypeSafe.Raw "WaitForExitCustom"] |>  p.MethodFilter.Join
      AssemblyExcludeFilter = [ TypeSafe.Raw "Tests"] |> p.AssemblyExcludeFilter.Join
      AssemblyFilter = [ @"\.DataCollector"; "Sample" ]
                       |> Seq.map TypeSafe.Raw
                       |> p.AssemblyFilter.Join
      LocalSource = TypeSafe.Set
      TypeFilter =
        [ @"System\."; @"Sample3\.Class2"; "Microsoft"; "ICSharpCode" ]
        |> Seq.map TypeSafe.Raw
        |> p.TypeFilter.Join }

let AltCoverApiFilter(p : Primitive.PrepareOptions) =
  { p with
      AssemblyExcludeFilter = "Tests" :: (p.AssemblyExcludeFilter |> Seq.toList)
      AssemblyFilter = [ "?^AltCover\." ] @ (p.AssemblyFilter |> Seq.toList)
      LocalSource = true
      TypeFilter =
        [ @"System\."; @"Sample3\.Class2"; "Microsoft"; "ICSharpCode"; "<Start" ] @ (p.TypeFilter |> Seq.toList) }

let AltCoverFilterX(p : Primitive.PrepareOptions) =
  { p with
      MethodFilter = "WaitForExitCustom" :: (p.MethodFilter |> Seq.toList)
      AssemblyExcludeFilter = "Tests" :: (p.AssemblyExcludeFilter |> Seq.toList)
      AssemblyFilter = [ @"\.DataCollector"; "Sample" ] @ (p.AssemblyFilter |> Seq.toList)
      LocalSource = true
      TypeFilter =
        [ @"System\."; @"Sample3\.Class2"; "Tests"; "Microsoft"; "ICSharpCode"; "<Start"]
        @ (p.TypeFilter |> Seq.toList) }

let AltCoverFilterXTypeSafe(p : TypeSafe.PrepareOptions) =
  { p with
      MethodFilter = [TypeSafe.Raw "WaitForExitCustom"] |>  p.MethodFilter.Join
      AssemblyExcludeFilter = [ TypeSafe.Raw "Tests"] |> p.AssemblyExcludeFilter.Join
      AssemblyFilter = [ @"\.DataCollector"; "Sample" ]
                       |> Seq.map TypeSafe.Raw
                       |> p.AssemblyFilter.Join
      LocalSource = TypeSafe.Set
      TypeFilter =
        [ @"System\."; @"Sample3\.Class2"; "Tests"; "Microsoft"; "ICSharpCode" ]
        |> Seq.map TypeSafe.Raw
        |> p.TypeFilter.Join }

let AltCoverFilterG(p : Primitive.PrepareOptions) =
  { p with
      MethodFilter = "WaitForExitCustom" :: (p.MethodFilter |> Seq.toList)
      AssemblyExcludeFilter = "Tests" :: (p.AssemblyExcludeFilter |> Seq.toList)
      AssemblyFilter = [ @"\.Recorder\.g"; "Sample" ] @ (p.AssemblyFilter |> Seq.toList)
      LocalSource = true
      TypeFilter =
        [ @"System\."; @"Sample3\.Class2"; "Microsoft" ] @ (p.TypeFilter |> Seq.toList) }

let programFiles = Environment.environVar "ProgramFiles"
let programFiles86 = Environment.environVar "ProgramFiles(x86)"
let dotnetPath = "dotnet" |> Fake.Core.ProcessUtils.tryFindFileOnPath

let dotnetOptions (o : DotNet.Options) =
  match dotnetPath with
  | Some f -> { o with DotNetCliPath = f }
  | None -> o

let fxcop =
  if Environment.isWindows then
    let expect = "./packages/fxcop/FxCopCmd.exe" |> Path.getFullName
    if File.Exists expect then Some expect else None
  else
    None

let monoOnWindows =
  if Environment.isWindows then
    [ programFiles; programFiles86 ]
    |> List.filter (String.IsNullOrWhiteSpace >> not)
    |> List.map (fun s -> s @@ "Mono/bin/mono.exe")
    |> List.tryFind File.Exists
  else
    None

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
          DotNet.info (fun opt ->
            { opt with Common = { dotnetOptions opt.Common with DotNetCliPath = path } })
          |> ignore
          perhaps
        with _ -> None
    | _ -> None
  else
    None

let dotnetOptions86 (o : DotNet.Options) =
  match dotnetPath86 with
  | Some f -> { o with DotNetCliPath = f }
  | None -> o

let nugetCache =
  Path.Combine
    (Environment.GetFolderPath Environment.SpecialFolder.UserProfile, ".nuget/packages")

let pwsh =
  match "pwsh" |> Fake.Core.ProcessUtils.tryFindFileOnPath with
  | Some path -> path
  | _ -> "pwsh"

let toolPackages =
  let xml =
    "./Build/NuGet.csproj"
    |> Path.getFullName
    |> XDocument.Load
  xml.Descendants(XName.Get("PackageReference"))
  |> Seq.map
       (fun x ->
         (x.Attribute(XName.Get("Include")).Value, x.Attribute(XName.Get("version")).Value))
  |> Map.ofSeq

let packageVersion (p : string) = p.ToLowerInvariant() + "/" + (toolPackages.Item p)

// MCS packages.config
let openCoverConsole =
  ("./packages/" + (packageVersion "OpenCover") + "/tools/OpenCover.Console.exe")
  |> Path.getFullName
let nunitConsole =
  ("./packages/" + (packageVersion "NUnit.ConsoleRunner") + "/tools/nunit3-console.exe")
  |> Path.getFullName
let xmldoc2cmdletdoc =
  ("./packages/" + (packageVersion "XmlDoc2CmdletDoc") + "/tools/netcoreapp2.1/XmlDoc2CmdletDoc.dll")
  |> Path.getFullName

let cliArguments =
  { MSBuild.CliArguments.Create() with
      ConsoleLogParameters = []
      DistributedLoggers = None
      DisableInternalBinLog = true }

let withWorkingDirectoryVM dir o =
  { dotnetOptions o with
      WorkingDirectory = Path.getFullName dir
      Verbosity = Some DotNet.Verbosity.Minimal }

let withWorkingDirectoryOnly dir o =
  { dotnetOptions o with WorkingDirectory = Path.getFullName dir }

let testWithCLIArguments (o : Fake.DotNet.DotNet.TestOptions) =
  { o with MSBuildParams = cliArguments }
let buildWithCLIArguments (o : Fake.DotNet.DotNet.BuildOptions) =
  { o with MSBuildParams = cliArguments }

let NuGetAltCover =
  toolPackages
  |> Seq.filter (fun kv -> kv.Key = "altcover")
  |> Seq.map (fun _ ->
       ("./packages/" + (packageVersion "altcover") + "/tools/net472/AltCover.exe")
       |> Path.getFullName)
  |> Seq.filter File.Exists
  |> Seq.tryHead

let ForceTrueOnly = DotNet.CLIOptions.Force true
let FailTrue = DotNet.CLIOptions.Fail true

let GreenSummary = DotNet.CLIOptions.Summary "Green"
let ForceTrue = DotNet.CLIOptions.Many [ ForceTrueOnly; GreenSummary ]
let ForceTrueFast = DotNet.CLIOptions.Many [ FailTrue; ForceTrueOnly; GreenSummary ]

let dotnetAltcover =
  Fake.DotNet.ToolType.CreateFrameworkDependentDeployment dotnetOptions
let dotnetAltcover86 =
  Fake.DotNet.ToolType.CreateFrameworkDependentDeployment dotnetOptions86
let frameworkAltcover = Fake.DotNet.ToolType.CreateFullFramework()

let defaultTestOptions fwk common (o : DotNet.TestOptions) =
  { o.WithCommon
      ((fun o2 -> { o2 with Verbosity = Some DotNet.Verbosity.Normal }) >> common) with
      NoBuild = true
      Framework = fwk // Some "netcoreapp3.0"
      Configuration = DotNet.BuildConfiguration.Debug }

let defaultDotNetTestCommandLine fwk project =
  AltCoverCommand.buildDotNetTestCommandLine (defaultTestOptions fwk dotnetOptions) project

let defaultDotNetTestCommandLine86 fwk project =
  AltCoverCommand.buildDotNetTestCommandLine (defaultTestOptions fwk dotnetOptions86) project

let coverletOptions (o : DotNet.Options) =
  { dotnetOptions o with CustomParams = Some "--collect:\"XPlat Code Coverage\"" }

let coverletTestOptions (o : DotNet.TestOptions) =
  { o.WithCommon dotnetOptions with
      Configuration = DotNet.BuildConfiguration.Debug
      NoBuild = true
      Framework = Some "netcoreapp3.0"
      Settings = Some "./Build/coverletArgs.runsettings" }
  |> testWithCLIArguments

let coverletTestOptionsSample (o : DotNet.TestOptions) =
  { coverletTestOptions o with Settings = Some "./Build/coverletArgs.sample.runsettings" }

let misses = ref 0

let uncovered (path : string) =
  misses := 0
  !!path
  |> Seq.collect (fun f ->
       let xml = XDocument.Load f
       xml.Descendants(XName.Get("Uncoveredlines"))
       |> Seq.filter (fun x ->
            match String.IsNullOrWhiteSpace x.Value with
            | false -> true
            | _ ->
                sprintf "No coverage from '%s'" f |> Trace.traceImportant
                misses := 1 + !misses
                false)
       |> Seq.map (fun e ->
            let coverage = e.Value
            match Int32.TryParse coverage with
            | (false, _) ->
                printfn "%A" xml
                Assert.Fail("Could not parse uncovered line value '" + coverage + "'")
                0
            | (_, numeric) ->
                printfn "%s : %A"
                  (f
                   |> Path.GetDirectoryName
                   |> Path.GetFileName) numeric
                numeric))
  |> Seq.toList

let coverageSummary _ =
  let numbers = uncovered "_Reports/_Unit*/Summary.xml"
  if numbers
     |> List.tryFind (fun n -> n > 0)
     |> Option.isSome
     || !misses > 1
  then Assert.Fail("Coverage is too low")

let msbuildRelease proj =
  MSBuild.build (fun p ->
    { p with
        Verbosity = Some MSBuildVerbosity.Normal
        ConsoleLogParameters = []
        DistributedLoggers = None
        DisableInternalBinLog = true
        Properties =
          [ "Configuration", "Release"
            "DebugSymbols", "True" ] }) proj

let msbuildDebug proj =
  MSBuild.build (fun p ->
    { p with
        Verbosity = Some MSBuildVerbosity.Normal
        ConsoleLogParameters = []
        DistributedLoggers = None
        DisableInternalBinLog = true
        Properties =
          [ "Configuration", "Debug"
            "DebugSymbols", "True" ] }) proj

let dotnetBuildRelease proj =
  DotNet.build (fun p ->
    { p.WithCommon dotnetOptions with Configuration = DotNet.BuildConfiguration.Release }
    |> buildWithCLIArguments) (Path.GetFullPath proj)

let dotnetBuildDebug proj =
  DotNet.build (fun p ->
    { p.WithCommon dotnetOptions with Configuration = DotNet.BuildConfiguration.Debug }
    |> buildWithCLIArguments) (Path.GetFullPath proj)

// Information.getCurrentHash()
let commitHash = Information.getCurrentSHA1 (".")
let infoV = Information.showName "." commitHash

//----------------------------------------------------------------

let _Target s f =
  Target.description s
  Target.create s f

// Preparation

_Target "RebuildPaketLock" ignore

_Target "Preparation" ignore

_Target "PreClean" (fun _ ->
// dir -Recurse *ssemblyAttributes.cs | % { del -Force $_.FullName }
      !!"**/*ssemblyAttributes.cs"
      |> Seq.map Path.GetFullPath
      |> Seq.toList
      |> List.iter File.delete)

_Target "Clean" (fun _ ->
  printfn "Cleaning the build and deploy folders"
  Actions.Clean())

_Target "SetVersion" (fun _ ->
  let appveyor = Environment.environVar "APPVEYOR_BUILD_VERSION"
  let travis = Environment.environVar "TRAVIS_JOB_NUMBER"
  let version = Actions.GetVersionFromYaml()

  let ci =
    if String.IsNullOrWhiteSpace appveyor then
      if String.IsNullOrWhiteSpace travis
      then String.Empty
      else version.Replace("{build}", travis + "-travis")
    else
      appveyor

  let (v, majmin, y) = Actions.LocalVersion ci version
  Version := v
  let copy =
    sprintf "© 2010-%d by Steve Gilham <SteveGilham@users.noreply.github.com>" y
  Copyright := "Copyright " + copy

  Directory.ensure "./_Generated"
  Shell.copyFile "./AltCover.Engine/Abstract.fsi" "./AltCover.Engine/Abstract.fs"
  Actions.InternalsVisibleTo(!Version)
  let v' = !Version

  [ "./_Generated/AssemblyVersion.fs"; "./_Generated/AssemblyVersion.cs" ]
  |> List.iter (fun file ->
       AssemblyInfoFile.create file
         [ AssemblyInfo.Product "AltCover"
           AssemblyInfo.Version(majmin + ".0.0")
           AssemblyInfo.FileVersion v'
           AssemblyInfo.Company "Steve Gilham"
           AssemblyInfo.Trademark ""
           AssemblyInfo.InformationalVersion(infoV)
           AssemblyInfo.Copyright copy ] (Some AssemblyInfoFileConfig.Default))
  printfn "%A" AssemblyInfoFileConfig.Default
  let lite = AssemblyInfoFileConfig(false)
  [ "./_Generated/AssemblyVersionLite.fs"; "./_Generated/AssemblyVersionList.cs" ]
  |> List.iter (fun file ->
       AssemblyInfoFile.create file
         [ AssemblyInfo.Product "AltCover"
           AssemblyInfo.Version(majmin + ".0.0")
           AssemblyInfo.FileVersion v'
           AssemblyInfo.Company "Steve Gilham"
           AssemblyInfo.Trademark ""
           AssemblyInfo.InformationalVersion(infoV)
           AssemblyInfo.Copyright copy ] (Some lite))
  let hack = """namespace AltCover
module SolutionRoot =
  let location = """ + "\"\"\"" + (Path.getFullName ".") + "\"\"\""
  let path = "_Generated/SolutionRoot.fs"

  // Update the file only if it would change
  let old =
    if File.Exists(path) then File.ReadAllText(path) else String.Empty
  if not (old.Equals(hack)) then File.WriteAllText(path, hack)

  [ "./AltCover.Recorder/AltCover.Recorder.fsproj" // net20 resgen
    "./Recorder.Tests/AltCover.Recorder.Tests.fsproj"
    "./AltCover.Avalonia/AltCover.Avalonia.fsproj"
    "./AltCover.Avalonia.FuncUI/AltCover.Avalonia.FuncUI.fsproj"
    "./AltCover.Visualizer/AltCover.Visualizer.fsproj" // GAC
    "./Tests.Visualizer/AltCover.Visualizer.Tests.fsproj" ]
  |> Seq.iter (fun f ->
       let dir = Path.GetDirectoryName f
       let proj = Path.GetFileName f
       DotNet.restore (fun o -> o.WithCommon(withWorkingDirectoryVM dir)) proj))

// Basic compilation

_Target "Compilation" ignore

_Target "BuildRelease" (fun _ ->
  try
    [ "./AltCover.Recorder.sln"; "./AltCover.Visualizer.sln"; "MCS.sln" ] |> Seq.iter msbuildRelease // gac+net20; mono

    [ "./AltCover.sln" ] |> Seq.iter dotnetBuildRelease

    // document cmdlets ahead of packaging
    let packages =
      let xml =
        "./AltCover.PowerShell/AltCover.PowerShell.fsproj"
        |> Path.getFullName
        |> XDocument.Load
      xml.Descendants(XName.Get("PackageReference"))
      |> Seq.map
           (fun x ->
             let incl = x.Attribute(XName.Get("Include"))
             let update = x.Attribute(XName.Get("Update"))
             let version = x.Attribute(XName.Get("Version")).Value
             if incl |> isNull
             then (update.Value, version)
             else (incl.Value, version))
      |> Map.ofSeq
    let packageVersionPart (p : string) = nugetCache +
                                          "/" + p.ToLowerInvariant() + "/" + (packages.Item p) +
                                          "/lib/netstandard2.0/"

    Shell.copyFile
      ("./_Binaries/AltCover.PowerShell/Release+AnyCPU/netstandard2.0/FSharp.Core.dll")
      ((packageVersionPart "FSharp.Core") + "FSharp.Core.dll")
    Shell.copyFile
      ("./_Binaries/AltCover.PowerShell/Release+AnyCPU/netstandard2.0/System.Management.Automation.dll")
      ((packageVersionPart "PowerShellStandard.Library") + "System.Management.Automation.dll")

    let cmdlets = "./_Binaries/AltCover.PowerShell/Release+AnyCPU/netstandard2.0/AltCover.PowerShell.dll"
                  |> Path.getFullName
    if Environment.isWindows then // the Jolt comment reader library is sadly windows/fullframework bound
      Actions.RunDotnet dotnetOptions ""
        ("--roll-forward Major " + xmldoc2cmdletdoc + " -strict " + cmdlets)
        "documenting cmdlets"
  with x ->
    printfn "%A" x
    reraise())

_Target "BuildDebug" (fun _ ->
  Directory.ensure "./_SourceLink"
  Shell.copyFile "./_SourceLink/Class2.cs" "./Sample14/Sample14/Class2.txt"
  if Environment.isWindows then
    let temp = Environment.environVar "TEMP"
    Shell.copyFile (temp @@ "/Sample14.SourceLink.Class3.cs")
      "./Sample14/Sample14/Class3.txt"
  else
    Directory.ensure "/tmp/.AltCover_SourceLink"
    Shell.copyFile "/tmp/.AltCover_SourceLink/Sample14.SourceLink.Class3.cs"
      "./Sample14/Sample14/Class3.txt"

  [ "./AltCover.Recorder.sln"; "./AltCover.Visualizer.sln"; "MCS.sln" ] |> Seq.iter msbuildDebug // gac+net20; mono

  [ "./AltCover.sln"; "./Sample14/Sample14.sln" ] |> Seq.iter dotnetBuildDebug

  Shell.copy "./_SourceLink" (!!"./Sample14/Sample14/bin/Debug/netcoreapp2.1/*"))

_Target "BuildMonoSamples" (fun _ ->
  [ "./Sample8/Sample8.csproj" ] |> Seq.iter dotnetBuildDebug // build to embed on non-Windows

  let mcs = "_Binaries/MCS/Release+AnyCPU/MCS.exe"
  [ ("./_Mono/Sample1",
     [ "-debug"; "-out:./_Mono/Sample1/Sample1.exe"; "./Sample1/Program.cs" ])

    ("./_Mono/Sample3",
     [ "-target:library"
       "-debug"
       "-out:./_Mono/Sample3/Sample3.dll"
       "-lib:./packages/Mono.Cecil.0.11.1/lib/net40"
       "-r:Mono.Cecil.dll"
       "./Sample3/Class1.cs" ]) ]
  |> Seq.iter (fun (dir, cmd) ->
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
    let options =
      { Lint.OptionalLintParameters.Default with
          Configuration = FromFile(Path.getFullName "./fsharplint.json") }

    [
      !!"**/*.fsproj"
      |> Seq.collect (fun n -> !!(Path.GetDirectoryName n @@ "*.fs"))
      |> Seq.distinct;
      !!"./Build/*.fsx"
      |> Seq.map Path.GetFullPath
    ]
    |> Seq.concat
    |> Seq.collect (fun f ->
         match Lint.lintFile options f with
         | Lint.LintResult.Failure x -> failwithf "%A" x
         | Lint.LintResult.Success w ->
             w
             |> Seq.filter (fun x -> x.Details.SuggestedFix |> Option.isSome))
    |> Seq.fold (fun _ x ->
         printfn "Info: %A\r\n Range: %A\r\n Fix: %A\r\n====" x.Details.Message
           x.Details.Range x.Details.SuggestedFix
         true) false
    |> failOnIssuesFound
  with ex ->
    printfn "%A" ex
    reraise())

_Target "Gendarme" (fun _ -> // Needs debug because release is compiled --standalone which contaminates everything

  Directory.ensure "./_Reports"

  [ ("./Build/common-rules.xml",
     [ "_Binaries/AltCover.Engine/Debug+AnyCPU/netstandard2.0/AltCover.Engine.dll"
       "_Binaries/AltCover/Debug+AnyCPU/netcoreapp2.0/AltCover.dll"
       "_Binaries/AltCover.Recorder/Debug+AnyCPU/net20/AltCover.Recorder.dll"
       "_Binaries/AltCover.PowerShell/Debug+AnyCPU/netstandard2.0/AltCover.PowerShell.dll"
       "_Binaries/AltCover.Fake/Debug+AnyCPU/netstandard2.0/AltCover.Fake.dll"
       "_Binaries/AltCover.DotNet/Debug+AnyCPU/netstandard2.0/AltCover.DotNet.dll"
       "_Binaries/AltCover.Toolkit/Debug+AnyCPU/netstandard2.0/AltCover.Toolkit.dll"
       "_Binaries/AltCover.UICommon/Debug+AnyCPU/netstandard2.0/AltCover.UICommon.dll"
       "_Binaries/AltCover.Visualizer/Debug+AnyCPU/netcoreapp2.1/AltCover.Visualizer.dll" // GTK3 (obsolete)
       "_Binaries/AltCover.Fake.DotNet.Testing.AltCover/Debug+AnyCPU/netstandard2.0/AltCover.Fake.DotNet.Testing.AltCover.dll" ])
    ("./Build/common-rules.xml",
     [ "_Binaries/AltCover/Debug+AnyCPU/netcoreapp2.1/AltCover.dll" // global tool build
       "_Binaries/AltCover.Visualizer/Debug+AnyCPU/net472/AltCover.Visualizer.exe" ]) // GTK2
//    ("./Build/common-rules.xml", // Avalonia generated code breaks everything
//     [ "_Binaries/AltCover.Visualizer.Avalonia/Debug+AnyCPU/netcoreapp2.1/AltCover.Visualizer.dll" ])
//    ("./Build/common-rules.xml",
//     [ "_Binaries/AltCover.Visualizer.FuncUI/Debug+AnyCPU/netcoreapp3.0/AltCover.Visualizer.dll" ])
    ("./Build/csharp-rules.xml",
     [ "_Binaries/AltCover.DataCollector/Debug+AnyCPU/netstandard2.0/AltCover.DataCollector.dll"
       "_Binaries/AltCover.FontSupport/Debug+AnyCPU/netstandard2.0/AltCover.FontSupport.dll"
       "_Binaries/AltCover.Cake/Debug+AnyCPU/netstandard2.0/AltCover.Cake.dll" ]) ]
  |> Seq.iter (fun (ruleset, files) ->
       Gendarme.run
         { Gendarme.Params.Create() with
             WorkingDirectory = "."
             Severity = Gendarme.Severity.All
             Confidence = Gendarme.Confidence.All
             Configuration = ruleset
             Console = true
             Log = "./_Reports/gendarme.html"
             LogKind = Gendarme.LogKind.Html
             Targets = files
             ToolType = ToolType.CreateLocalTool()
             FailBuildOnDefect = true }))

_Target "FxCop" (fun _ ->
  Directory.ensure "./_Reports"

  let dumpSuppressions (report : String) =
    let x = XDocument.Load report
    let messages = x.Descendants(XName.Get "Message")
    messages
    |> Seq.iter (fun m ->
         let mpp = m.Parent.Parent
         let target = mpp.Name.LocalName
         let tname = mpp.Attribute(XName.Get "Name").Value

         let (text, fqn) =
           match target with
           | "Namespace" -> ("namespace", tname)
           | "Resource" -> ("resource", tname)
           | "File"
           | "Module" -> ("module", String.Empty)
           | "Type" ->
               let spp = mpp.Parent.Parent
               ("type", spp.Attribute(XName.Get "Name").Value + "." + tname)
           | _ ->
               let spp = mpp.Parent.Parent
               let sp4 = spp.Parent.Parent
               ("member",
                sp4.Attribute(XName.Get "Name").Value + "."
                + spp.Attribute(XName.Get "Name").Value + "." + tname)

         let text2 = "[<assembly: SuppressMessage("

         let id = m.Attribute(XName.Get "Id")

         let text3 =
           (if id
               |> isNull
               |> not then
             ", MessageId=\"" + id.Value + "\""
            else
              String.Empty)
           + ", Justification=\"\")>]"

         let category = m.Attribute(XName.Get "Category").Value
         let checkId = m.Attribute(XName.Get "CheckId").Value
         let name = m.Attribute(XName.Get "TypeName").Value

         let finish t t2 =
           let t5 = t2 + "\"" + category + "\", \"" + checkId + ":" + name + "\""
           if t
              |> isNull
              || t = "module" then
             t5 + text3
           else
             t5 + ", Scope=\"" + t + "\", Target=\"" + fqn + "\"" + text3

         printfn "%s" (finish text text2))

  let deprecatedRules = [ "-Microsoft.Usage#CA2202" ] // double dispose

  let nonFsharpRules =
    [
      "-Microsoft.Design#CA1006" // nested generics
      "-Microsoft.Design#CA1034" // nested classes being visible
      "-Microsoft.Design#CA1062" // null checks,  In F#!
      "-Microsoft.Naming#CA1709" // defer to the Gendarme casing rule for implicit 'a
      "-Microsoft.Naming#CA1715" // defer to the Gendarme naming rule for implicit 'a
      "-Microsoft.Usage#CA2235"  // closures being serializable
    ]

  let standardRules =
    [
      "-Microsoft.Design#CA1020"
      "-Microsoft.Usage#CA2243:AttributeStringLiteralsShouldParseCorrectly"
    ] // small namespaces

  let cantStrongName =
    [ "-Microsoft.Design#CA2210" ] // should strongname

  let defaultRules = List.concat [
    deprecatedRules
    standardRules
    nonFsharpRules
  ]

  let defaultCSharpRules = List.concat [
    deprecatedRules
    standardRules
  ]

  [ (
     (if String.IsNullOrEmpty(Environment.environVar "APPVEYOR_BUILD_VERSION")
      then
        [ "_Binaries/AltCover.FontSupport/Debug+AnyCPU/net472/AltCover.FontSupport.dll"
          "_Binaries/AltCover/Debug+AnyCPU/net472/AltCover.exe"
          "_Binaries/AltCover.DataCollector/Debug+AnyCPU/net472/AltCover.DataCollector.dll" ]
      else // HACK HACK HACK
        [ "_Binaries/AltCover/Debug+AnyCPU/net472/AltCover.exe"
          "_Binaries/AltCover.DataCollector/Debug+AnyCPU/net472/AltCover.DataCollector.dll" ]), // TODO netcore support
      [],
      standardRules)
    ([ "_Binaries/AltCover.Fake/Debug+AnyCPU/net472/AltCover.Fake.dll" ],
      [],
      List.concat [
        defaultRules
        cantStrongName  // can't strongname this as Fake isn't strongnamed
      ])
    ([ "_Binaries/AltCover.Fake.DotNet.Testing.AltCover/Debug+AnyCPU/net472/AltCover.Fake.DotNet.Testing.AltCover.dll" ],
     [],
     defaultRules)
    ([ "_Binaries/AltCover.Cake/Debug+AnyCPU/net472/AltCover.Cake.dll"
       ],
     [],
     List.concat [
        defaultCSharpRules
        cantStrongName // can't strongname this as Cake isn't strongnamed
      ])
    ([ "_Binaries/AltCover.Toolkit/Debug+AnyCPU/net472/AltCover.Toolkit.dll"
       "_Binaries/AltCover.DotNet/Debug+AnyCPU/net472/AltCover.DotNet.dll"],
     [],
     defaultRules)
    ([ "_Binaries/AltCover.PowerShell/Debug+AnyCPU/net472/AltCover.PowerShell.dll" ],
     [],
      defaultRules)
    ([ "_Binaries/AltCover.UICommon/Debug+AnyCPU/net472/AltCover.UICommon.dll"
       "_Binaries/AltCover.Visualizer/Debug+AnyCPU/net472/AltCover.Visualizer.exe"],
     [],
     defaultRules)
    ([ "_Binaries/AltCover.Recorder/Debug+AnyCPU/net20/AltCover.Recorder.dll" ],
     [],
       "-Microsoft.Naming#CA1703:ResourceStringsShouldBeSpelledCorrectly" :: defaultRules) // Esperanto resources in-line
    ([ "_Binaries/AltCover.Engine/Debug+AnyCPU/net472/AltCover.Engine.dll" ],
     [],
      List.concat [
        defaultRules
        [
          "-Microsoft.Naming#CA1703"   // spelling in resources
          "-Microsoft.Performance#CA1810"  // Static module initializers in $Type classes
        ]
      ])
  ]
  |> Seq.iter (fun (files, types, ruleset) ->
       try
         files
         |> FxCop.run
              { FxCop.Params.Create() with
                  WorkingDirectory = "."
                  ToolPath = Option.get fxcop
                  UseGAC = true
                  Verbose = false
                  ReportFileName = "_Reports/FxCopReport.xml"
                  Types = types
                  Rules = ruleset
                  FailOnError = FxCop.ErrorLevel.Warning
                  IgnoreGeneratedCode = true }
       with _ ->
         dumpSuppressions "_Reports/FxCopReport.xml"
         reraise())

  try
    [ "_Binaries/AltCover.PowerShell/Debug+AnyCPU/net472/AltCover.PowerShell.dll" ]
    |> FxCop.run
         { FxCop.Params.Create() with
             WorkingDirectory = "."
             ToolPath = Option.get fxcop
             UseGAC = true
             Verbose = false
             ReportFileName = "_Reports/FxCopReport.xml"
             RuleLibraries =
               [ Path.getFullName "ThirdParty/Microsoft.PowerShell.CodeAnalysis.16.dll" ]
             FailOnError = FxCop.ErrorLevel.Warning
             IgnoreGeneratedCode = true }
  with _ ->
    dumpSuppressions "_Reports/FxCopReport.xml"
    reraise())
// Unit Test

_Target "UnitTest" coverageSummary
_Target "UncoveredUnitTest" ignore

_Target "JustUnitTest" (fun _ ->
  Directory.ensure "./_Reports"
  try
    !!(@"_Binaries/*Test*/Debug+AnyCPU/net4*/AltCover*Test*.dll")
    |> Seq.filter (fun f -> Path.GetFileName(f) <> "AltCover.Fake.DotNet.Testing.AltCover.dll")
    |> Seq.filter (fun f -> Path.GetFileName(f) <> "AltCover.Recorder.Tests.dll")
    |> NUnit3.run (fun p ->
         { p with
             ToolPath = nunitConsole
             WorkingDir = "."
             ResultSpecs = [ "./_Reports/JustUnitTestReport.xml" ] })

    !!(@"_Binaries/AltCover.Recorder.Tests/Debug+AnyCPU/net472/AltCover.Recorder.Tests.dll")
    |> NUnit3.run (fun p ->
         { p with
             ToolPath = nunitConsole
             WorkingDir = "."
             ResultSpecs = [ "./_Reports/RecorderUnitTestReport.xml" ] })

    !!(@"_Binaries/AltCover.Recorder.Tests/Debug+AnyCPU/net20/AltCover.Recorder.Tests.dll")
    |> NUnit3.run (fun p ->
         { p with
             ToolPath = nunitConsole
             WorkingDir = "."
             ResultSpecs = [ "./_Reports/Recorder2UnitTestReport.xml" ] })
  with x ->
    printfn "%A" x
    reraise())

_Target "BuildForUnitTestDotNet" (fun _ ->
  msbuildDebug "./Recorder.Tests/AltCover.Recorder.Tests.fsproj"

  !!(@"./*Tests/*Tests.fsproj")
  |> Seq.filter (fun s -> s.Contains("Visualizer") |> not // incomplete
                          && s.Contains("Recorder") |> not) // net20
  |> Seq.iter
       (DotNet.build (fun p ->
         { p.WithCommon dotnetOptions with
             Configuration = DotNet.BuildConfiguration.Debug
             Framework = Some "netcoreapp3.0" }
         |> buildWithCLIArguments)))

_Target "UnitTestDotNet" (fun _ ->
  Directory.ensure "./_Reports"
  try
    !!(@"./*Tests/*Tests.fsproj")
    |> Seq.iter
         (DotNet.test (fun p ->
           { p.WithCommon dotnetOptions with
               Configuration = DotNet.BuildConfiguration.Debug
               Framework = Some "netcoreapp3.0"
               NoBuild = true }
           |> testWithCLIArguments))
  with x ->
    printfn "%A" x
    reraise())

_Target "BuildForCoverlet" (fun _ ->
  msbuildDebug "./Recorder.Tests/AltCover.Recorder.Tests.fsproj"
  let l = !!(@"./*Tests/*Tests.fsproj")
          |> Seq.filter (fun s -> s.Contains("Visualizer") |> not // incomplete
                                  && s.Contains("Recorder") |> not) // net20
          |> Seq.toList

  ("./ValidateGendarmeEmulation/AltCover.ValidateGendarmeEmulation.fsproj" :: l)
  |> Seq.iter
       (DotNet.build (fun p ->
         { p.WithCommon dotnetOptions with
             Configuration = DotNet.BuildConfiguration.Debug
             Framework = Some "netcoreapp3.0" }
         |> buildWithCLIArguments)))

_Target "UnitTestDotNetWithCoverlet" (fun _ ->
  Directory.ensure "./_Reports"
  try
    let l = !!(@"./*Tests/*Tests.fsproj")
            |> Seq.filter (fun s -> s.Contains("Visualizer") |> not) // incomplete
            |> Seq.toList

    let xml =
      ("./ValidateGendarmeEmulation/AltCover.ValidateGendarmeEmulation.fsproj" :: l)
      |> Seq.fold (fun l f ->
           let here = Path.GetDirectoryName f
           let tr = here @@ "TestResults"
           Directory.ensure tr
           Shell.cleanDir tr
           try
             f |> DotNet.test coverletTestOptions
           with x -> eprintf "%A" x

           // Can't seem to get this any other way
           let covxml =
             (!!(tr @@ "*/coverage.opencover.xml") |> Seq.head) |> Path.getFullName
           let doc = covxml |> XDocument.Load

           let key =
             doc.Descendants(XName.Get "Name")
             |> Seq.filter
                  (fun x -> x.Value = "System.Void AltCover.CommandLine/Format::.ctor()")
             |> Seq.toList
           key |> List.iter (fun x -> x.Parent.Remove())

           let target =
             (Path.getFullName "./_Reports")
             @@ ((Path.GetFileNameWithoutExtension f) + ".coverlet.xml")
           doc.Save target

           // Shell.copyFile target covxml
           target :: l) []

    ReportGenerator.generateReports (fun p ->
      { p with
          ToolType = ToolType.CreateLocalTool()
          ReportTypes =
            [ ReportGenerator.ReportType.Html; ReportGenerator.ReportType.XmlSummary ]
          TargetDir = "_Reports/_UnitTestWithCoverlet" }) xml

    uncovered @"_Reports/_UnitTestWithCoverl*/Summary.xml"
    |> printfn "%A uncovered lines"
  with x ->
    printfn "%A" x
    reraise())

_Target "UnitTestWithOpenCover" (fun _ ->
  Directory.ensure "./_Reports/_UnitTestWithOpenCover"
  let testFiles = "./_Binaries/AltCover.ValidateGendarmeEmulation/Debug+AnyCPU/net472/AltCover.ValidateGendarmeEmulation.dll" ::
                  (!!(@"_Binaries/*Test*/Debug+AnyCPU/net4*/AltCover*Test*.dll")
                   |> Seq.filter (fun f -> Path.GetFileName(f) <> "AltCover.Fake.DotNet.Testing.AltCover.dll")
                   |> Seq.filter (fun f -> Path.GetFileName(f) <> "AltCover.Recorder.Tests.dll")
                   |> Seq.filter (fun s -> s.Contains("Visualizer") |> not) // incomplete
                   |> Seq.toList)

  let Recorder4Files = !!(@"_Binaries/*Tests/Debug+AnyCPU/net472/*Recorder.Tests.dll")

  let RecorderFiles = !!(@"_Binaries/*Tests/Debug+AnyCPU/net20/AltCover*Test*.dll")
  let coverage = Path.getFullName "_Reports/UnitTestWithOpenCover.xml"
  let scoverage = Path.getFullName "_Reports/RecorderTestWithOpenCover.xml"
  let s4coverage = Path.getFullName "_Reports/Recorder4TestWithOpenCover.xml"

  try
    OpenCover.run (fun p ->
      { p with
          WorkingDir = "."
          ExePath = openCoverConsole
          TestRunnerExePath = nunitConsole
          Filter =
            "+[AltCover]* +[AltCover.Recorder]* +[AltCover.Engine]* +[AltCover.ValidateGendarmeEmulation]Alt* +[AltCover.Toolkit]* +[AltCover.DotNet]* -[*]AltCover.Tests.* -[*]AltCover.SolutionRoot -[*]Microsoft.* -[*]System.* -[Sample*]* -[*]ICSharpCode.*"
          MergeByHash = true
          ReturnTargetCode = Fake.DotNet.Testing.OpenCover.ReturnTargetCodeType.Yes
          OptionalArguments =
            "-excludebyattribute:*ExcludeFromCodeCoverageAttribute;*ProgIdAttribute"
          Register = OpenCover.RegisterType.Path64
          Output = coverage
          TimeOut = TimeSpan (0,10,0) })
      (String.Join(" ", testFiles)
       + " --result=./_Reports/UnitTestWithOpenCoverReport.xml")

    OpenCover.run (fun p ->
      { p with
          WorkingDir = "."
          ExePath = openCoverConsole
          TestRunnerExePath = nunitConsole
          Filter =
            "+[AltCover.Recorder]* -[*]ICSharpCode.* -[*]System.*"
          MergeByHash = true
          ReturnTargetCode = Fake.DotNet.Testing.OpenCover.ReturnTargetCodeType.Yes
          OptionalArguments =
            "-excludebyattribute:*ExcludeFromCodeCoverageAttribute;*ProgIdAttribute"
          Register = OpenCover.RegisterType.RegisterUser // Path64 doesn't work on my machine
          Output = scoverage })
      (String.Join(" ", RecorderFiles)
       + " --result=./_Reports/RecorderTestWithOpenCoverReport.xml")

    OpenCover.run (fun p ->
      { p with
          WorkingDir = "."
          ExePath = openCoverConsole
          TestRunnerExePath = nunitConsole
          Filter =
            "+[AltCover.Recorder]* -[*]ICSharpCode.* -[*]System.*"
          MergeByHash = true
          ReturnTargetCode = Fake.DotNet.Testing.OpenCover.ReturnTargetCodeType.Yes
          OptionalArguments =
            "-excludebyattribute:*ExcludeFromCodeCoverageAttribute;*ProgIdAttribute"
          Register = OpenCover.RegisterType.Path64
          Output = s4coverage })
      (String.Join(" ", Recorder4Files)
       + " --result=./_Reports/RecorderTest4WithOpenCoverReport.xml")

  with x ->
    printfn "%A" x
    reraise()

  ReportGenerator.generateReports (fun p ->
    { p with
        ToolType = ToolType.CreateLocalTool()
        ReportTypes =
          [ ReportGenerator.ReportType.Html; ReportGenerator.ReportType.XmlSummary ]
        TargetDir = "_Reports/_UnitTestWithOpenCover" })
    [ coverage; scoverage; s4coverage ]

  uncovered @"_Reports/_UnitTestWithOpenCove*/Summary.xml"
  |> printfn "%A uncovered lines")

// Hybrid (Self) Tests

_Target "UnitTestWithAltCover" (fun _ ->
  Directory.ensure "./_Reports/_UnitTestWithAltCover"

  // Tools
  let keyfile = Path.getFullName "Build/SelfTest.snk"
  let shadowkeyfile = Path.getFullName "Build/Infrastructure.snk"
  let reports = Path.getFullName "./_Reports"
  let altcover = Path.getFullName "./_Binaries/AltCover/Release+AnyCPU/net472/AltCover.exe"
  let sn = "sn" |> Fake.Core.ProcessUtils.tryFindFileOnPath

  // net4x tests -- TODO API
  let testDirectory = Path.getFullName "_Binaries/AltCover.Tests/Debug+AnyCPU/net472"
  let weakDir = Path.getFullName "_Binaries/AltCover.ValidateGendarmeEmulation/Debug+AnyCPU/net472"
  let Recorder4Dir =
    Path.getFullName "_Binaries/AltCover.Recorder.Tests/Debug+AnyCPU/net472"
  let apiDir = Path.getFullName "_Binaries/AltCover.Api.Tests/Debug+AnyCPU/net472"

  let altReport = reports @@ "UnitTestWithAltCover.xml"

  printfn "Instrument the net4x code"
  let prep =
    AltCover.PrepareOptions.Primitive
      ({ Primitive.PrepareOptions.Create() with
           XmlReport = altReport
           InputDirectories = [| "."; weakDir; Recorder4Dir; apiDir |]
           OutputDirectories =
             [| "./__UnitTestWithAltCover"
                weakDir @@ "__ValidateGendarmeEmulationWithAltCover"
                Recorder4Dir @@ "__RecorderTestWithAltCover"
                apiDir @@ "__ApiTestWithAltCover" |]
           StrongNameKey = keyfile
           ReportFormat = "NCover"
           InPlace = false
           Save = false }
       |> AltCoverFilterX)
    |> AltCoverCommand.Prepare
  { AltCoverCommand.Options.Create prep with
      ToolPath = altcover
      ToolType = frameworkAltcover
      WorkingDirectory = testDirectory }
  |> AltCoverCommand.run

  if sn |> Option.isSome then
    Actions.Run
      (sn |> Option.get, testDirectory,
       [ "-vf"; "./__UnitTestWithAltCover/AltCover.Recorder.g.dll" ])
      "Recorder assembly strong-name verified OK"

  printfn "Unit test the instrumented net4x code"
  try
    [ !!"_Binaries/AltCover.Tests/Debug+AnyCPU/net472/__UnitTestWithAltCover/*.Tests.dll"
      !!"_Binaries/AltCover.Api.Tests/Debug+AnyCPU/net472/__ApiTestWithAltCover/*.Tests.dll"
      !!"_Binaries/AltCover.ValidateGendarmeEmulation/Debug+AnyCPU/net472/__ValidateGendarmeEmulationWithAltCover/Alt*Valid*.dll"
      !!"_Binaries/AltCover.Recorder.Tests/Debug+AnyCPU/net472/__RecorderTestWithAltCover/Alt*Test*.dll"
      !!"_Binaries/AltCover.Tests/Debug+AnyCPU/net472/__UnitTestWithAltCover/*ple2.dll" ]
    |> Seq.concat
    |> Seq.distinct
    |> NUnit3.run (fun p ->
         { p with
             ToolPath = nunitConsole
             WorkingDir = "."
             ResultSpecs = [ "./_Reports/UnitTestWithAltCoverReport.xml" ] })
  with x ->
    printfn "%A" x
    reraise()

  printfn "Instrument the net20 Recorder tests"
  let RecorderDir =
    Path.getFullName "_Binaries/AltCover.Recorder.Tests/Debug+AnyCPU/net20"
  let RecorderReport = reports @@ "RecorderTestWithAltCover.xml"

  let prep =
    AltCover.PrepareOptions.Primitive
      ({ Primitive.PrepareOptions.Create() with
           XmlReport = RecorderReport
           OutputDirectories = [| "./__RecorderTestWithAltCover" |]
           StrongNameKey = shadowkeyfile
           ReportFormat = "NCover"
           InPlace = false
           Save = false }
       |> AltCoverFilter)
    |> AltCoverCommand.Prepare
  { AltCoverCommand.Options.Create prep with
      ToolPath = altcover
      ToolType = frameworkAltcover
      WorkingDirectory = RecorderDir }
  |> AltCoverCommand.run

  printfn "Execute the net20 Recorder tests"
  !!("_Binaries/AltCover.Recorder.Tests/Debug+AnyCPU/net20/__RecorderTestWithAltCover/Alt*.Test*.dll")
  |> NUnit3.run (fun p ->
       { p with
           ToolPath = nunitConsole
           WorkingDir = "."
           ResultSpecs = [ "./_Reports/RecorderTestWithAltCoverReport.xml" ] })

  ReportGenerator.generateReports (fun p ->
    { p with
        ToolType = ToolType.CreateLocalTool()
        ReportTypes =
          [ ReportGenerator.ReportType.Html; ReportGenerator.ReportType.XmlSummary ]
        TargetDir = "_Reports/_UnitTestWithAltCover" }) [ altReport; RecorderReport ]

  uncovered @"_Reports/_UnitTestWithAltCover/Summary.xml" |> printfn "%A uncovered lines")

_Target "UnitTestWithAltCoverRunner" (fun _ ->
  Directory.ensure "./_Reports/_UnitTestWithAltCover"

  // Tools
  let keyfile = Path.getFullName "Build/SelfTest.snk"
  let shadowkeyfile = Path.getFullName "Build/Infrastructure.snk"
  let reports = Path.getFullName "./_Reports"
  let altcover = Path.getFullName "./_Binaries/AltCover/Release+AnyCPU/net472/AltCover.exe"

  let tests =
    [
      (
        Path.getFullName "_Binaries/AltCover.Tests/Debug+AnyCPU/net472", // test directory
        "./__UnitTestWithAltCoverRunner", // relative output
        "UnitTestWithAltCoverRunner.xml", // coverage report
        "./_Reports/UnitTestWithAltCoverRunnerReport.xml", // relative nunit reporting
        [ Path.getFullName // test assemblies
            "_Binaries/AltCover.Tests/Debug+AnyCPU/net472/__UnitTestWithAltCoverRunner/AltCover.Tests.dll"
          Path.getFullName
            "_Binaries/AltCover.Tests/Debug+AnyCPU/net472/__UnitTestWithAltCoverRunner/Sample2.dll" ],
        AltCoverFilterTypeSafe,
        keyfile
      )
      (
        Path.getFullName "_Binaries/AltCover.Api.Tests/Debug+AnyCPU/net472", // test directory
        "./__ApiTestWithAltCoverRunner", // relative output
        "ApiTestWithAltCoverRunner.xml", // coverage report
        "./_Reports/ApiTestWithAltCoverRunnerReport.xml", // relative nunit reporting
        [ Path.getFullName // test assemblies
            "_Binaries/AltCover.Api.Tests/Debug+AnyCPU/net472/__ApiTestWithAltCoverRunner/AltCover.Api.Tests.dll" ],
        AltCoverFilterTypeSafe,
        keyfile
      )
      (
        Path.getFullName "_Binaries/AltCover.ValidateGendarmeEmulation/Debug+AnyCPU/net472",
        "./__ValidateGendarmeEmulationWithAltCoverRunner",
        "ValidateGendarmeEmulationWithAltCoverRunner.xml",
        "./_Reports/ValidateGendarmeEmulationWithAltCoverRunnerReport.xml",
        [ Path.getFullName
            "_Binaries/AltCover.ValidateGendarmeEmulation/Debug+AnyCPU/net472/__ValidateGendarmeEmulationWithAltCoverRunner/AltCover.ValidateGendarmeEmulation.dll" ],
        (fun x -> { x with TypeFilter = TypeSafe.Filters [ TypeSafe.Raw "Tests"; TypeSafe.Raw "SolutionRoot" ]}) >> AltCoverFilterXTypeSafe,
        keyfile
      )
      (
        Path.getFullName "_Binaries/AltCover.Recorder.Tests/Debug+AnyCPU/net472",
        "./__RecorderTestWithAltCoverRunner",
        "RecorderTestWithAltCoverRunner.xml",
        "./_Reports/RecorderTestWithAltCoverRunnerReport.xml",
        [ Path.getFullName
            "_Binaries/AltCover.Recorder.Tests/Debug+AnyCPU/net472/__RecorderTestWithAltCoverRunner/AltCover.Recorder.Tests.dll" ],
        AltCoverFilterTypeSafe,
        shadowkeyfile
      )
      (
        Path.getFullName "_Binaries/AltCover.Recorder.Tests/Debug+AnyCPU/net20",
        "./__RecorderTest2WithAltCoverRunner",
        "RecorderTest2WithAltCoverRunner.xml",
        "./_Reports/RecorderTest2WithAltCoverRunnerReport.xml",
        [ Path.getFullName
            "_Binaries/AltCover.Recorder.Tests/Debug+AnyCPU/net20/__RecorderTest2WithAltCoverRunner/AltCover.Recorder.Tests.dll" ],
        AltCoverFilterTypeSafe,
        shadowkeyfile
      )
      (
        Path.getFullName "_Binaries/AltCover.Tests.Visualizer/Debug+AnyCPU/net472",
        "./__GTKVTestWithAltCoverRunner",
        "GTKVTestWithAltCoverRunner.xml",
        "./_Reports/GTKVTestWithAltCoverRunnerReport.xml",
        [ Path.getFullName
            "_Binaries/AltCover.Tests.Visualizer/Debug+AnyCPU/net472/__GTKVTestWithAltCoverRunner/AltCover.Tests.Visualizer.dll" ],
        (fun x -> { x with TypeFilter = TypeSafe.Filters [ TypeSafe.Raw "Gui" ]
                           AssemblyFilter = TypeSafe.Filters [ TypeSafe.Raw "\\-sharp" ]}) >> AltCoverFilterTypeSafe,
        keyfile
      )
    ]

  tests
  |> List.iter (fun (testDirectory, outputDirectory, coverageReport, nunitReport, nunitAssemblies, filter, signingKey) ->
       let altReport = reports @@ coverageReport
       printfn "Instrument the code %s" testDirectory
       let prep =
         AltCover.PrepareOptions.TypeSafe
           ({ TypeSafe.PrepareOptions.Create() with
                XmlReport = TypeSafe.FilePath altReport
                OutputDirectories = TypeSafe.DirectoryPaths [| TypeSafe.DirectoryPath outputDirectory |]
                StrongNameKey = TypeSafe.FilePath signingKey
                SingleVisit = TypeSafe.Set
                InPlace = TypeSafe.Clear
                Save = TypeSafe.Clear }
            |> filter)
         |> AltCoverCommand.Prepare
       { AltCoverCommand.Options.Create prep with
           ToolPath = altcover
           ToolType = frameworkAltcover
           WorkingDirectory = testDirectory }
       |> AltCoverCommand.run

       printfn "Unit test the instrumented code %s" outputDirectory
       let nunitparams =
         { NUnit3Defaults with
             ToolPath = nunitConsole
             WorkingDir = "."
             ResultSpecs = [ nunitReport ] }

       let nunitcmd = NUnit3.buildArgs nunitparams nunitAssemblies

       try
         let collect =
           AltCover.CollectOptions.TypeSafe
             { TypeSafe.CollectOptions.Create() with
                 Executable = TypeSafe.FilePath nunitConsole
                 RecorderDirectory = TypeSafe.DirectoryPath (testDirectory @@ outputDirectory)
                 CommandLine = nunitcmd
                               |> AltCoverCommand.splitCommandLine
                               |> Seq.map TypeSafe.CommandArgument
                               |> TypeSafe.CommandArguments  }
           |> AltCoverCommand.Collect
         { AltCoverCommand.Options.Create collect with
             ToolPath = altcover
             ToolType = frameworkAltcover
             WorkingDirectory = "." }
         |> AltCoverCommand.run
       with x ->
         printfn "%A" x
         reraise())
  let pester = Path.getFullName "_Reports/Pester.xml"

  let xmlreports =
    pester :: (tests
               |> List.map (fun (_, _, report, _, _, _, _) -> reports @@ report)
               |> List.filter (fun f -> f.Contains("GTKV") |> not))

  ReportGenerator.generateReports (fun p ->
    { p with
        ToolType = ToolType.CreateLocalTool()
        ReportTypes =
          [ ReportGenerator.ReportType.Html; ReportGenerator.ReportType.XmlSummary ]
        TargetDir = "_Reports/_UnitTestWithAltCoverRunner" }) xmlreports

  uncovered @"_Reports/_UnitTestWithAltCoverRunner/Summary.xml"
  |> printfn "%A uncovered lines"

  let reportLines = xmlreports |> List.map File.ReadAllLines

  let top =
    reportLines
    |> List.head
    |> Seq.takeWhile (fun l -> l.StartsWith("    <Module") |> not)
  let tail =
    reportLines
    |> List.head
    |> Seq.skipWhile (fun l -> l <> "  </Modules>")
  let core =
    reportLines
    |> List.map (fun f ->
         f
         |> Seq.skipWhile (fun l -> l.StartsWith("    <Module") |> not)
         |> Seq.takeWhile (fun l -> l <> "  </Modules>"))

  let coverage = reports @@ "CombinedTestWithAltCoverRunner.coveralls"
  File.WriteAllLines
    (coverage,
     Seq.concat
       [ top
         Seq.concat core
         tail ]
     |> Seq.toArray)
  let coveralls =
    ("./packages/" + (packageVersion "coveralls.io") + "/tools/coveralls.net.exe")
    |> Path.getFullName

  if not <| String.IsNullOrWhiteSpace(Environment.environVar "APPVEYOR_BUILD_NUMBER") then
    Actions.Run (coveralls, "_Reports", [ "--opencover"; coverage ])
      "Coveralls upload failed")

_Target "UnitTestWithAltCoverCore" (fun _ ->
  Directory.ensure "./_Reports/_UnitTestWithAltCover"
  let keyfile = Path.getFullName "Build/SelfTest.snk"
  let reports = Path.getFullName "./_Reports"
  let altcover = Path.getFullName "./_Binaries/AltCover/Release+AnyCPU/netcoreapp2.0/AltCover.dll"

  let tests =
   [
     (
       Path.getFullName "_Binaries/AltCover.Tests/Debug+AnyCPU/netcoreapp3.0", // testDirectory
       Path.getFullName "Tests/_Binaries/AltCover.Tests/Debug+AnyCPU/netcoreapp3.0", // output
       reports @@ "UnitTestWithAltCoverCore.xml", // report
       "AltCover.Tests.fsproj", // project
       Path.getFullName "Tests", // workingDirectory
       AltCoverFilter >> (fun p -> { p with AssemblyExcludeFilter = [ "?^AltCover$" ]}) // filter
     )
     (
       Path.getFullName "_Binaries/AltCover.Recorder.Tests/Debug+AnyCPU/netcoreapp3.0",
       Path.getFullName "Recorder.Tests/_Binaries/AltCover.Recorder.Tests/Debug+AnyCPU/netcoreapp3.0",
       reports @@ "RecorderTestWithAltCoverCore.xml",
       "AltCover.Recorder.Tests.fsproj",
       Path.getFullName "Recorder.Tests",
       AltCoverFilterG
     )
     (
       Path.getFullName "_Binaries/AltCover.Api.Tests/Debug+AnyCPU/netcoreapp3.0", // testDirectory
       Path.getFullName "AltCover.Api.Tests/_Binaries/AltCover.Api.Tests/Debug+AnyCPU/netcoreapp3.0", // output
       reports @@ "ApiUnitTestWithAltCoverCore.xml", // report
       "AltCover.Api.Tests.fsproj", // project
       Path.getFullName "AltCover.Api.Tests", // workingDirectory
       AltCoverApiFilter // filter
     )
     (
       Path.getFullName "_Binaries/AltCover.ValidateGendarmeEmulation/Debug+AnyCPU/netcoreapp3.0", // testDirectory
       Path.getFullName "AltCover.ValidateGendarmeEmulation/_Binaries/AltCover.ValidateGendarmeEmulation/Debug+AnyCPU/netcoreapp3.0", // output
       reports @@ "ValidateGendarmeEmulationUnitTestWithAltCoverCore.xml", // report
       "AltCover.ValidateGendarmeEmulation.fsproj", // project
       Path.getFullName "ValidateGendarmeEmulations", // workingDirectory
       (fun p -> { p with TypeFilter = [ "<Start"; "Expecto"; "Tests" ]}) >> AltCoverFilter // filter
     )
   ]

  tests
  |> List.iter (fun (testDirectory, output, report, project, workingDirectory, filter) ->

       printfn "Instrument the code %s" testDirectory
       let prep =
         AltCover.PrepareOptions.Primitive
           ({ Primitive.PrepareOptions.Create() with
                XmlReport = report
                OutputDirectories = [| output |]
                StrongNameKey = keyfile
                ReportFormat = "NCover"
                InPlace = false
                Save = false }
            |> filter)
         |> AltCoverCommand.Prepare
       { AltCoverCommand.Options.Create prep with
           ToolPath = altcover
           ToolType = dotnetAltcover
           WorkingDirectory = testDirectory }
       |> AltCoverCommand.run

       printfn "Unit test the instrumented code %s" project
       try
         project
         |> DotNet.test (fun p ->
              { p.WithCommon(withWorkingDirectoryVM workingDirectory) with
                  Configuration = DotNet.BuildConfiguration.Debug
                  Framework = Some "netcoreapp3.0"
                  NoBuild = true }
              |> testWithCLIArguments)
       with x ->
         printfn "%A" x
         reraise())

  let xmlreports =
    tests
    |> List.map (fun (_, _, report, _, _, _) -> report)
    |> List.filter (fun f -> f.Contains("GTKV") |> not)

  ReportGenerator.generateReports (fun p ->
    { p with
        ToolType = ToolType.CreateLocalTool()
        ReportTypes =
          [ ReportGenerator.ReportType.Html; ReportGenerator.ReportType.XmlSummary ]
        TargetDir = "_Reports/_UnitTestWithAltCoverCore" }) xmlreports

  uncovered @"_Reports/_UnitTestWithAltCoverCore/Summary.xml"
  |> printfn "%A uncovered lines")

_Target "UnitTestWithAltCoverCoreRunner" (fun _ ->
  Directory.ensure "./_Reports/_UnitTestWithAltCoverCoreRunner"
  let keyfile = Path.getFullName "Build/SelfTest.snk"
  let reports = Path.getFullName "./_Reports"
  let altcover =
    Path.getFullName "./_Binaries/AltCover/Release+AnyCPU/netcoreapp2.0/AltCover.dll"

  let tests =
   [
     (
       Path.getFullName "_Binaries/AltCover.Tests/Debug+AnyCPU/netcoreapp3.0", // testDirectory
       Path.getFullName "Tests/_Binaries/AltCover.Tests/Debug+AnyCPU/netcoreapp3.0", // output
       reports @@ "UnitTestWithAltCoverCoreRunner.xml", // report
       Path.getFullName "./Tests/AltCover.Tests.fsproj"
     )
     (
       Path.getFullName "_Binaries/AltCover.Api.Tests/Debug+AnyCPU/netcoreapp3.0", // testDirectory
       Path.getFullName "AltCover.Api.Tests/_Binaries/AltCover.Api.Tests/Debug+AnyCPU/netcoreapp3.0", // output
       reports @@ "ApiTestWithAltCoverCoreRunner.xml", // report
       Path.getFullName "./AltCover.Api.Tests/AltCover.Api.Tests.fsproj"
     )
     (
       Path.getFullName "_Binaries/AltCover.Recorder.Tests/Debug+AnyCPU/netcoreapp3.0",
       Path.getFullName "Recorder.Tests/_Binaries/AltCover.Recorder.Tests/Debug+AnyCPU/netcoreapp3.0",
       reports @@ "RecorderTestWithAltCoverCoreRunner.xml",
       Path.getFullName "./Recorder.Tests/AltCover.Recorder.Tests.fsproj")
     (
       Path.getFullName "_Binaries/AltCover.ValidateGendarmeEmulation/Debug+AnyCPU/netcoreapp3.0", // testDirectory
       Path.getFullName "ValidateGendarmeEmulation/_Binaries/AltCover.ValidateGendarmeEmulation/Debug+AnyCPU/netcoreapp3.0", // output
       reports @@ "ValidateGendarmeEmulationUnitTestWithAltCoverCoreRunner.xml", // report
       Path.getFullName "ValidateGendarmeEmulation/AltCover.ValidateGendarmeEmulation.fsproj") // project
   ]

  tests
  |> List.iter (fun (testDirectory, output, report, testproject) ->

       printfn "Instrument the code %s" testDirectory
       Shell.cleanDir output
       let prep =
         AltCover.PrepareOptions.Primitive
           ({ Primitive.PrepareOptions.Create() with
                XmlReport = report
                OutputDirectories = [| output |]
                TypeFilter = [ "SolutionRoot"; "Expecto"; "Tests" ]
                VisibleBranches = true
                StrongNameKey = keyfile
                SingleVisit = true
                InPlace = false
                Save = false }
            |> AltCoverFilter)
         |> AltCoverCommand.Prepare
       { AltCoverCommand.Options.Create prep with
           ToolPath = altcover
           ToolType = dotnetAltcover
           WorkingDirectory = testDirectory }
       |> AltCoverCommand.run

       printfn "Unit test the instrumented code %s" testproject
       let (dotnetexe, args) =
         defaultDotNetTestCommandLine (Some "netcoreapp3.0") testproject

       let collect =
         AltCover.CollectOptions.Primitive
           { Primitive.CollectOptions.Create() with
               Executable = dotnetexe
               RecorderDirectory = output
               CommandLine = args }
         |> AltCoverCommand.Collect
       { AltCoverCommand.Options.Create collect with
           ToolPath = altcover
           ToolType = dotnetAltcover
           WorkingDirectory = output }
       |> AltCoverCommand.run)

  let xmlreports =
    tests
    |> List.map (fun (_, _, report, _) -> report)
    |> List.filter (fun f -> f.Contains("GTKV") |> not)

  ReportGenerator.generateReports (fun p ->
    { p with
        ToolType = ToolType.CreateLocalTool()
        ReportTypes =
          [ ReportGenerator.ReportType.Html; ReportGenerator.ReportType.XmlSummary ]
        TargetDir = "_Reports/_UnitTestWithAltCoverCoreRunner" }) xmlreports

  uncovered @"_Reports/_UnitTestWithAltCoverCoreRunner/Summary.xml"
  |> printfn "%A uncovered lines")

// Pure OperationalTests

_Target "OperationalTest" ignore

_Target "FSharpTypes" (fun _ ->
  Directory.ensure "./_Reports"
  let simpleReport = (Path.getFullName "./_Reports") @@ ("AltCoverFSharpTypes.xml")
  let binRoot = Path.getFullName "_Binaries/AltCover/Release+AnyCPU/net472"
  let sampleRoot = Path.getFullName "_Binaries/Sample2/Debug+AnyCPU/net472"
  let instrumented = "__FSharpTypes"

  let prep =
    AltCover.PrepareOptions.Primitive
      ({ Primitive.PrepareOptions.Create() with
           XmlReport = simpleReport
           OutputDirectories = [| "./" + instrumented |]
           AssemblyFilter = [ "Adapter"; "nunit"; "FSharp" ]
           TypeFilter = [ "System\\."; "Microsoft\\." ]
           InPlace = false
           ReportFormat = "NCover"
           Save = false })
    |> AltCoverCommand.Prepare
  { AltCoverCommand.Options.Create prep with
      ToolPath = binRoot @@ "AltCover.exe"
      ToolType = frameworkAltcover
      WorkingDirectory = sampleRoot }
  |> AltCoverCommand.run
  Actions.ValidateFSharpTypes simpleReport [])

_Target "FSharpTypesDotNet" (fun _ -> // obsolete

  Directory.ensure "./_Reports"
  let simpleReport = (Path.getFullName "./_Reports") @@ ("AltCoverFSharpTypesDotNet.xml")
  let sampleRoot =
    Path.getFullName "Sample2/_Binaries/Sample2/Debug+AnyCPU/netcoreapp2.1"

  // Test the --inplace operation
  Shell.cleanDir sampleRoot
  "Sample2.fsproj"
  |> DotNet.test (fun o ->
       { o.WithCommon(withWorkingDirectoryVM "Sample2") with
           Configuration = DotNet.BuildConfiguration.Debug
           Framework = Some "netcoreapp2.1" }
       |> testWithCLIArguments)

  let prep =
    AltCover.PrepareOptions.Primitive
      ({ Primitive.PrepareOptions.Create() with
           XmlReport = simpleReport
           AssemblyFilter = [ "Adapter" ]
           TypeFilter = [ "System\\."; "Microsoft\\." ]
           InPlace = true
           ReportFormat = "NCover"
           Save = false })
    |> AltCoverCommand.Prepare
  { AltCoverCommand.Options.Create prep with
      ToolPath =
        (Path.getFullName "./_Binaries/AltCover/Release+AnyCPU/netcoreapp2.0/AltCover.dll")
      ToolType = dotnetAltcover
      WorkingDirectory = sampleRoot }
  |> AltCoverCommand.run

  Actions.ValidateFSharpTypes simpleReport [ "main" ]
  Assert.That(Path.Combine(sampleRoot, "__Saved") |> Directory.Exists)

  printfn "Execute the instrumented tests"
  "Sample2.fsproj"
  |> DotNet.test (fun o ->
       { o.WithCommon(withWorkingDirectoryVM "Sample2") with
           Configuration = DotNet.BuildConfiguration.Debug
           Framework = Some "netcoreapp2.1"
           NoBuild = true }
       |> testWithCLIArguments)
  Actions.ValidateFSharpTypesCoverage simpleReport)

_Target "FSharpTests" (fun _ ->
  Directory.ensure "./_Reports"
  let altcover =
    Path.getFullName "./_Binaries/AltCover/Release+AnyCPU/netcoreapp2.0/AltCover.dll"
  let simpleReport = (Path.getFullName "./_Reports") @@ ("AltCoverFSharpTests.xml")
  let sampleRoot =
    Path.getFullName "Sample7/_Binaries/Sample7/Debug+AnyCPU/netcoreapp2.1"

  // Test the --inplace operation
  Shell.cleanDir sampleRoot
  "Sample7.fsproj"
  |> DotNet.test (fun o ->
       { o.WithCommon(withWorkingDirectoryVM "Sample7") with
           Configuration = DotNet.BuildConfiguration.Debug } |> testWithCLIArguments)

  // inplace instrument
  let prep =
    AltCover.PrepareOptions.Primitive
      ({ Primitive.PrepareOptions.Create() with
           XmlReport = simpleReport
           CallContext = [ "[Test]" ]
           AssemblyFilter = [ "Adapter" ]
           TypeFilter = [ "System\\."; "Microsoft\\." ]
           InPlace = true
           ReportFormat = "OpenCover"
           Save = false })
    |> AltCoverCommand.Prepare
  { AltCoverCommand.Options.Create prep with
      ToolPath = altcover
      ToolType = dotnetAltcover
      WorkingDirectory = sampleRoot }
  |> AltCoverCommand.run

  printfn "Execute the instrumented tests"

  let sample7 = Path.getFullName "./Sample7/Sample7.fsproj"
  let (dotnetexe, args) = defaultDotNetTestCommandLine None sample7

  let collect =
    AltCover.CollectOptions.Primitive
      { Primitive.CollectOptions.Create() with
          Executable = dotnetexe
          RecorderDirectory = sampleRoot
          CommandLine = args }
    |> AltCoverCommand.Collect
  { AltCoverCommand.Options.Create collect with
      ToolPath = altcover
      ToolType = dotnetAltcover
      WorkingDirectory = "Sample7" }
  |> AltCoverCommand.run)

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
    AltCover.PrepareOptions.Primitive
      ({ Primitive.PrepareOptions.Create() with
           XmlReport = simpleReport
           OutputDirectories = [ instrumented ]
           AssemblyFilter = [ "Adapter" ]
           TypeFilter = [ "System\\."; "Microsoft\\." ]
           InPlace = false
           ReportFormat = "NCover"
           Save = false })
    |> AltCoverCommand.Prepare
  { AltCoverCommand.Options.Create prep with
      ToolPath = altcover
      ToolType = dotnetAltcover
      WorkingDirectory = sampleRoot }
  |> AltCoverCommand.run

  Actions.ValidateFSharpTypes simpleReport [ "main" ]

  printfn "Execute the instrumented tests"
  let sample2 = Path.getFullName "./Sample2/Sample2.fsproj"
  let (dotnetexe, args) = defaultDotNetTestCommandLine (Some "netcoreapp2.1") sample2

  let collect =
    AltCover.CollectOptions.Primitive
      { Primitive.CollectOptions.Create() with
          Executable = dotnetexe
          RecorderDirectory = instrumented
          CommandLine = args }
    |> AltCoverCommand.Collect
  { AltCoverCommand.Options.Create collect with
      ToolPath = altcover
      ToolType = dotnetAltcover
      WorkingDirectory = instrumented }
  |> AltCoverCommand.run

  Actions.ValidateFSharpTypesCoverage simpleReport)

_Target "FSharpTypesDotNetCollecter" (fun _ ->
  Directory.ensure "./_Reports"
  let altcover =
    Path.getFullName "./_Binaries/AltCover/Release+AnyCPU/netcoreapp2.0/AltCover.dll"
  let simpleReport =
    (Path.getFullName "./_Reports") @@ ("AltCoverFSharpTypesDotNetCollecter.xml")
  let simpleReport2 =
    (Path.getFullName "./_Reports/unzip") @@ ("AltCoverFSharpTypesDotNetCollecter.xml")
  let simpleReport3 =
    (Path.getFullName "./_Reports/unzip2") @@ ("AltCoverFSharpTypesDotNetCollecter.xml")
  let sampleRoot =
    Path.getFullName "Sample2/_Binaries/Sample2/Debug+AnyCPU/netcoreapp2.1"

  printfn "Build and test normally"
  Shell.cleanDir sampleRoot
  "Sample2.fsproj"
  |> DotNet.test (fun o ->
       { o.WithCommon(withWorkingDirectoryVM "Sample2") with
           Configuration = DotNet.BuildConfiguration.Debug } |> testWithCLIArguments)

  printfn  "inplace instrument and save"
  let prep =
    AltCover.PrepareOptions.Primitive
      ({ Primitive.PrepareOptions.Create() with
           XmlReport = simpleReport
           AssemblyFilter = [ "Adapter" ]
           TypeFilter = [ "System\\."; "Microsoft\\." ]
           InPlace = true
           ReportFormat = "NCover"
           ZipFile = true
           Save = true })
    |> AltCoverCommand.Prepare
  { AltCoverCommand.Options.Create prep with
      ToolPath = altcover
      ToolType = dotnetAltcover
      WorkingDirectory = sampleRoot }
  |> AltCoverCommand.run

  printfn "Extract and verify the first results"
  System.IO.Compression.ZipFile.ExtractToDirectory(simpleReport + ".zip", Path.GetDirectoryName simpleReport3)
  Actions.ValidateFSharpTypes simpleReport3 [ "main" ]
  Assert.That(Path.Combine(sampleRoot, "__Saved") |> Directory.Exists)

  printfn "Execute the instrumented tests"
  "Sample2.fsproj"
  |> DotNet.test (fun o ->
       { o.WithCommon(withWorkingDirectoryVM "Sample2") with
           Configuration = DotNet.BuildConfiguration.Debug
           NoBuild = true }
       |> testWithCLIArguments)

  printfn "Collect the results"
  let collect =
    AltCover.CollectOptions.Primitive
      { Primitive.CollectOptions.Create() with RecorderDirectory = sampleRoot }
    |> AltCoverCommand.Collect
  { AltCoverCommand.Options.Create collect with
      ToolPath = altcover
      ToolType = dotnetAltcover
      WorkingDirectory = sampleRoot }
  |> AltCoverCommand.run

  printfn "Extract and verify the results"
  System.IO.Compression.ZipFile.ExtractToDirectory(simpleReport + ".zip", Path.GetDirectoryName simpleReport2)
  Actions.ValidateFSharpTypesCoverage simpleReport2)

_Target "BasicCSharp"
  (fun _ ->
    Actions.SimpleInstrumentingRun "_Binaries/Sample1/Debug+AnyCPU/net20"
      "_Binaries/AltCover/Release+AnyCPU/net472" "BasicCSharp")

_Target "BasicCSharpMono"
  (fun _ ->
    Actions.SimpleInstrumentingRun "_Mono/Sample1"
      "_Binaries/AltCover/Release+AnyCPU/net472" "BasicCSharpMono")

_Target "BasicCSharpUnderMono" (fun _ ->
  monoOnWindows
  |> Actions.SimpleInstrumentingRunUnderMono "_Binaries/Sample1/Debug+AnyCPU/net20"
       "_Binaries/AltCover/Release+AnyCPU/net472" "BasicCSharpUnderMono")

_Target "BasicCSharpMonoUnderMono" (fun _ ->
  monoOnWindows
  |> Actions.SimpleInstrumentingRunUnderMono "_Mono/Sample1"
       "_Binaries/AltCover/Release+AnyCPU/net472" "BasicCSharpMono")

_Target "CSharpMonoWithDotNet" (fun _ ->
  Directory.ensure "./_Reports"
  let x = Path.getFullName "./_Reports/CSharpMonoWithDotNet.xml"
  let o = Path.getFullName "./_Mono/__Instrumented.CSharpMonoWithDotNet"
  let i = Path.getFullName "./_Mono/Sample1"
  let altcover =
    Path.getFullName "./_Binaries/AltCover/Release+AnyCPU/netcoreapp2.0/AltCover.dll"

  let prep =
    AltCover.PrepareOptions.Primitive
      ({ Primitive.PrepareOptions.Create() with
           XmlReport = x
           OutputDirectories = [ o ]
           InputDirectories = [ i ]
           TypeFilter = [ "System\\." ]
           InPlace = false
           ReportFormat = "NCover"
           Save = false })
    |> AltCoverCommand.Prepare
  { AltCoverCommand.Options.Create prep with
      ToolPath = altcover
      ToolType = dotnetAltcover
      WorkingDirectory = "." }
  |> AltCoverCommand.run

  Actions.Run (o @@ "/Sample1.exe", o, []) "Instrumented .exe failed"
  Actions.ValidateSample1 "./_Reports/CSharpMonoWithDotNet.xml" "CSharpMonoWithDotNet")

_Target "CSharpDotNetWithDotNet" (fun _ ->
  Directory.ensure "./_Reports"
  let x = Path.getFullName "./_Reports/CSharpDotNetWithDotNet.xml"
  let o = Path.getFullName "./_Binaries/Sample1/__Instrumented.CSharpDotNetWithDotNet"
  let i = Path.getFullName "./_Binaries/Sample1/Debug+AnyCPU/netcoreapp2.0"

  let prep =
    AltCover.PrepareOptions.Primitive
      ({ Primitive.PrepareOptions.Create() with
           XmlReport = x
           OutputDirectories = [ o ]
           InputDirectories = [ i ]
           TypeFilter = [ "System\\." ]
           InPlace = false
           ReportFormat = "NCover"
           Save = false })
    |> AltCoverCommand.Prepare
  { AltCoverCommand.Options.Create prep with
      ToolPath = "_Binaries/AltCover/Release+AnyCPU/netcoreapp2.0/AltCover.dll"
      ToolType = dotnetAltcover
      WorkingDirectory = "." }
  |> AltCoverCommand.run

  Actions.RunDotnet dotnetOptions "" (o @@ "Sample1.dll") "CSharpDotNetWithDotNet test"
  Actions.ValidateSample1 "./_Reports/CSharpDotNetWithDotNet.xml"
    "CSharpDotNetWithDotNet")

_Target "CSharpDotNetWithFramework" (fun _ ->
  Directory.ensure "./_Reports"
  let simpleReport = (Path.getFullName "./_Reports") @@ ("CSharpDotNetWithFramework.xml")
  let binRoot = Path.getFullName "_Binaries/AltCover/Release+AnyCPU/net472"
  let sampleRoot = Path.getFullName "_Binaries/Sample1/Debug+AnyCPU/netcoreapp2.0"
  let instrumented =
    Path.getFullName "_Binaries/Sample1/__Instrumented.CSharpDotNetWithFramework"

  let prep =
    AltCover.PrepareOptions.Primitive
      ({ Primitive.PrepareOptions.Create() with
           XmlReport = simpleReport
           OutputDirectories = [| instrumented |]
           TypeFilter = [ "System\\."; "Microsoft\\." ]
           InPlace = false
           ReportFormat = "NCover"
           Save = false })
    |> AltCoverCommand.Prepare
  { AltCoverCommand.Options.Create prep with
      ToolPath = binRoot @@ "AltCover.exe"
      ToolType = frameworkAltcover
      WorkingDirectory = sampleRoot }
  |> AltCoverCommand.run

  Actions.RunDotnet dotnetOptions "" (instrumented @@ "Sample1.dll")
    "CSharpDotNetWithFramework test"
  Actions.ValidateSample1 simpleReport "CSharpDotNetWithFramework")

_Target "SelfTest" (fun _ ->
  Directory.ensure "./_Reports/_Instrumented"
  let targetDir = "_Binaries/AltCover.Tests/Debug+AnyCPU/net472"
  let reports = Path.getFullName "./_Reports"
  let report = reports @@ "OpenCoverSelfTest.xml"
  let altReport = reports @@ "AltCoverSelfTest.xml"
  let keyfile = Path.getFullName "Build/SelfTest.snk"

  printfn "Self-instrument under OpenCover"
  let prep =
    AltCover.PrepareOptions.Primitive
      ({ Primitive.PrepareOptions.Create() with
           XmlReport = altReport
           OutputDirectories = [| "__SelfTest" |]
           AssemblyExcludeFilter = [ "xunit"; "NUnit" ]
           StrongNameKey = keyfile
           ReportFormat = "NCover"
           InPlace = false
           Save = false })
    |> AltCoverCommand.Prepare

  let args =
    ({ AltCoverCommand.Options.Create prep with
         ToolPath = String.Empty
         WorkingDirectory = "." }
     |> AltCoverCommand.composeCommandLine).CommandLine

  let OpenCoverFilter = "+[AltCove*]* -[*]Microsoft.* -[*]System.* +[*]N.* -[*]ICSharpCode.*"

  OpenCover.run (fun p ->
    { p with
        WorkingDir = targetDir
        ExePath = openCoverConsole
        TestRunnerExePath = "./_Binaries/AltCover/Release+AnyCPU/net472/AltCover.exe"
        Filter = OpenCoverFilter
        MergeByHash = true
        OptionalArguments =
          "-excludebyattribute:*ExcludeFromCodeCoverageAttribute;*ProgIdAttribute"
        Register = OpenCover.RegisterType.Path64
        Output = report }) args

  ReportGenerator.generateReports (fun p ->
    { p with
        ToolType = ToolType.CreateLocalTool()
        TargetDir = "_Reports/_OpenCoverSelfTest" }) [ report ]

  printfn "Re-instrument everything"
  let altReport2 = reports @@ "AltCoverSelfTestDummy.xml"

  let prep =
    AltCover.PrepareOptions.Primitive
      ({ Primitive.PrepareOptions.Create() with
           XmlReport = altReport2
           OutputDirectories = [| "./__SelfTestDummy" |]
           StrongNameKey = keyfile
           ReportFormat = "NCover"
           InPlace = false
           Save = false }
       |> AltCoverFilter)
    |> AltCoverCommand.Prepare
  { AltCoverCommand.Options.Create prep with
      ToolPath = "_Binaries/AltCover.Tests/Debug+AnyCPU/net472/__SelfTest/AltCover.exe"
      ToolType = frameworkAltcover
      WorkingDirectory = "_Binaries/AltCover.Tests/Debug+AnyCPU/net472" }
  |> AltCoverCommand.run

  ReportGenerator.generateReports (fun p ->
    { p with
        ToolType = ToolType.CreateLocalTool()
        TargetDir = "_Reports/_AltCoverSelfTest" }) [ altReport ])

_Target "RecordResumeTest" (fun _ ->
  Directory.ensure "./_Reports"
  let simpleReport = (Path.getFullName "./_Reports") @@ ("RecordResumeTest.xml")
  let sampleRoot = Path.getFullName "_Binaries/Sample8/Debug+AnyCPU/net20"
  let instrumented = "__RecordResumeTest"

  let toolPath =
    match NuGetAltCover with
    | Some test ->
        Trace.traceImportant "Using the NuGet package"
        test
    | _ -> Path.getFullName "_Binaries/AltCover/Release+AnyCPU/net472/AltCover.exe"

  let prep =
    AltCover.PrepareOptions.Primitive
      ({ Primitive.PrepareOptions.Create() with
           XmlReport = simpleReport
           OutputDirectories = [ instrumented ]
           TypeFilter = [ "System\\."; "Microsoft\\." ]
           AssemblyFilter = [ "Adapter"; "nunit"; "xunit" ]
           InPlace = false
           ReportFormat = "NCover"
           Save = false })
    |> AltCoverCommand.Prepare

  { AltCoverCommand.Options.Create prep with
      ToolPath = toolPath
      ToolType = frameworkAltcover
      WorkingDirectory = sampleRoot }
  |> AltCoverCommand.run
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
    AltCover.CollectOptions.Primitive
      { Primitive.CollectOptions.Create() with RecorderDirectory = instrumented }
    |> AltCoverCommand.Collect
  { AltCoverCommand.Options.Create collect with
      ToolPath = toolPath
      ToolType = frameworkAltcover
      WorkingDirectory = sampleRoot }
  |> AltCoverCommand.run

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

     let hits =
       recorded
       |> Seq.filter (fun i -> i = "1")
       |> Seq.length
     Assert.That(hits, Is.GreaterThanOrEqualTo 6)
     Assert.That(hits, Is.LessThanOrEqualTo 8))

_Target "RecordResumeTrackingTest" (fun _ ->
  Directory.ensure "./_Reports"
  let simpleReport = (Path.getFullName "./_Reports") @@ ("RecordResumeTrackingTest.xml")
  let sampleRoot = Path.getFullName "_Binaries/Sample8/Debug+AnyCPU/net20"
  let instrumented = "__RecordResumeTrackingTest"

  let toolPath =
    match NuGetAltCover with
    | Some test ->
        Trace.traceImportant "Using the NuGet package"
        test
    | _ -> Path.getFullName "_Binaries/AltCover/Release+AnyCPU/net472/AltCover.exe"

  let prep =
    AltCover.PrepareOptions.Primitive
      ({ Primitive.PrepareOptions.Create() with
           XmlReport = simpleReport
           OutputDirectories = [ instrumented ]
           CallContext = [ "Main" ]
           TypeFilter = [ "System\\."; "Microsoft\\." ]
           AssemblyFilter = [ "Adapter"; "nunit" ]
           InPlace = false
           ReportFormat = "OpenCover"
           Save = false })
    |> AltCoverCommand.Prepare

  { AltCoverCommand.Options.Create prep with
      ToolPath = toolPath
      ToolType = frameworkAltcover
      WorkingDirectory = sampleRoot }
  |> AltCoverCommand.run
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
    AltCover.CollectOptions.Primitive
      { Primitive.CollectOptions.Create() with RecorderDirectory = instrumented }
    |> AltCoverCommand.Collect
  { AltCoverCommand.Options.Create collect with
      ToolPath = toolPath
      ToolType = frameworkAltcover
      WorkingDirectory = sampleRoot }
  |> AltCoverCommand.run

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

     let hits =
       recorded
       |> Seq.filter (fun i -> i = "1")
       |> Seq.length
     Assert.That(hits, Is.GreaterThanOrEqualTo 6)
     Assert.That(hits, Is.LessThanOrEqualTo 8)
     let tracked =
       coverageDocument.Descendants(XName.Get("TrackedMethodRef")) |> Seq.toList
     Assert.That(tracked, Is.Not.Empty))

_Target "RecordResumeTestDotNet" (fun _ ->
  Directory.ensure "./_Reports"
  let simpleReport = (Path.getFullName "./_Reports") @@ ("RecordResumeTestDotNet.xml")
  let binRoot = Path.getFullName "_Binaries/AltCover/Release+AnyCPU/netcoreapp2.0"
  let sampleRoot = Path.getFullName "_Binaries/Sample8/Debug+AnyCPU/netcoreapp2.0"
  let instrumented = "__RecordResumeTestDotNet"

  let prep =
    AltCover.PrepareOptions.Primitive
      ({ Primitive.PrepareOptions.Create() with
           XmlReport = simpleReport
           OutputDirectories = [ instrumented ]
           TypeFilter = [ "System\\."; "Microsoft\\." ]
           AssemblyFilter = [ "Adapter"; "nunit" ]
           InPlace = false
           ReportFormat = "NCover"
           Save = false })
    |> AltCoverCommand.Prepare
  { AltCoverCommand.Options.Create prep with
      ToolPath = binRoot @@ "AltCover.dll"
      ToolType = dotnetAltcover
      WorkingDirectory = sampleRoot }
  |> AltCoverCommand.run

  let testing = (sampleRoot @@ instrumented) @@ "Sample8.dll"
  Actions.RunDotnet (fun o -> { dotnetOptions o with WorkingDirectory = sampleRoot }) ""
    (testing + " " + simpleReport + ".acv") "RecordResumeTestDotNet 2"

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
    AltCover.CollectOptions.Primitive
      { Primitive.CollectOptions.Create() with RecorderDirectory = instrumented }
    |> AltCoverCommand.Collect
  { AltCoverCommand.Options.Create collect with
      ToolPath = binRoot @@ "AltCover.dll"
      ToolType = dotnetAltcover
      WorkingDirectory = sampleRoot }
  |> AltCoverCommand.run

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

     let hits =
       recorded
       |> Seq.filter (fun i -> i = "1")
       |> Seq.length
     Assert.That(hits, Is.GreaterThanOrEqualTo 6)
     Assert.That(hits, Is.LessThanOrEqualTo 8))

_Target "RecordResumeTestUnderMono" (fun _ ->  // Fails : System.EntryPointNotFoundException: CreateZStream
  Directory.ensure "./_Reports"
  let simpleReport = (Path.getFullName "./_Reports") @@ ("RecordResumeTestUnderMono.xml")
  let binRoot = Path.getFullName "_Binaries/AltCover/Release+AnyCPU/net472"
  let sampleRoot = Path.getFullName "_Binaries/Sample8/Debug+AnyCPU/net20"
  let instrumented = "__RecordResumeTestUnderMono"

  let prep =
    AltCover.PrepareOptions.Primitive
      ({ Primitive.PrepareOptions.Create() with
           XmlReport = simpleReport
           OutputDirectories = [ instrumented ]
           TypeFilter = [ "System\\."; "Microsoft\\." ]
           AssemblyFilter = [ "Adapter"; "nunit" ]
           InPlace = false
           ReportFormat = "NCover"
           Save = false })
    |> AltCoverCommand.Prepare
  { AltCoverCommand.Options.Create prep with
      ToolPath = binRoot @@ "AltCover.exe"
      ToolType = frameworkAltcover
      WorkingDirectory = sampleRoot }
  |> AltCoverCommand.run

  match monoOnWindows with
  | Some mono ->
      let testing = (sampleRoot @@ instrumented) @@ "Sample8.exe"

      let r =
        CreateProcess.fromRawCommand mono
          [ testing
            simpleReport + ".acv" ]
        |> CreateProcess.withWorkingDirectory sampleRoot
        |> Proc.run
      Assert.That(r.ExitCode, Is.EqualTo 0, "RecordResumeTestUnderMono 2")
  | None -> Trace.traceError "No mono found!!"

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
    AltCover.CollectOptions.Primitive
      { Primitive.CollectOptions.Create() with RecorderDirectory = instrumented }
    |> AltCoverCommand.Collect
  { AltCoverCommand.Options.Create collect with
      ToolPath = binRoot @@ "AltCover.exe"
      ToolType = frameworkAltcover
      WorkingDirectory = sampleRoot }
  |> AltCoverCommand.run

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
  let AltCover = Path.getFullName "_Binaries/AltCover/Release+AnyCPU/net472/AltCover.exe"
  let engine = Path.getFullName "_Binaries/AltCover/Release+AnyCPU/net472/AltCover.Engine.dll"
  let config = AltCover + ".config"
  let manatee =
    Path.getFullName "_Binaries/AltCover/Release+AnyCPU/net472/Manatee.Json.dll"
  let fox =
    Path.getFullName "_Binaries/AltCover/Release+AnyCPU/net472/BlackFox.CommandLine.dll"
  let fscore = Path.getFullName "_Binaries/AltCover/Release+AnyCPU/net472/FSharp.Core.dll"
  let options =
    Path.getFullName "_Binaries/AltCover/Release+AnyCPU/net472/Mono.Options.dll"
  let recorder =
    Path.getFullName "_Binaries/AltCover/Release+AnyCPU/net472/AltCover.Recorder.dll"
  let poshHelp =
    Path.getFullName
      "_Binaries/AltCover.PowerShell/Release+AnyCPU/netstandard2.0/AltCover.PowerShell.dll-Help.xml"
  if (poshHelp |> File.Exists |> not) && (Environment.isWindows |> not)
  then File.WriteAllText(poshHelp, "DUMMY TEXT")

  let vis =
    Path.getFullName
      "_Binaries/AltCover.Visualizer/Release+AnyCPU/net472/AltCover.Visualizer.exe"
  let uic =
    Path.getFullName
      "_Binaries/AltCover.Visualizer/Release+AnyCPU/net472/AltCover.UICommon.dll"
  let packable = Path.getFullName "./_Binaries/README.html"

  let libFiles path =
    Seq.concat
      [ !!"./_Binaries/AltCover/Release+AnyCPU/net472/Mono.C*.dll"
        !!"./_Binaries/AltCover/Release+AnyCPU/net472/Newton*.dll" ]
    |> Seq.map (fun f -> (f |> Path.getFullName, Some path, None))
    |> Seq.toList

  let housekeeping =
    [ (Path.getFullName "./LICENS*", Some "", None)
      (Path.getFullName "./Build/AltCover_128.*", Some "", None) ]

  let housekeepingVis =
    [ (Path.getFullName "./LICENS*", Some "", None)
      (Path.getFullName "./AltCover.UICommon/logo.*", Some "", None) ]

  let applicationFiles =
    [ (AltCover, Some "tools/net472", None)
      (engine, Some "tools/net472", None)
      (config, Some "tools/net472", None)
      (recorder, Some "tools/net472", None)
      (vis, Some "tools/net472", None)
      (uic, Some "tools/net472", None)
      (fscore, Some "tools/net472", None)
      (manatee, Some "tools/net472", None)
      (fox, Some "tools/net472", None)
      (options, Some "tools/net472", None)
      (packable, Some "", None) ]

  let apiFiles =
    [ (AltCover, Some "lib/net472", None)
      (engine, Some "lib/net472", None)
      (config, Some "lib/net472", None)
      (recorder, Some "lib/net472", None)
      (fscore, Some "lib/net472", None)
      (manatee, Some "lib/net472", None)
      (fox, Some "lib/net472", None)
      (options, Some "lib/net472", None)
      (packable, Some "", None) ]

  let resourceFiles path =
    [ "_Binaries/AltCover/Release+AnyCPU/net472"
      "_Binaries/AltCover.Visualizer/Release+AnyCPU/net472" ]
    |> List.map (fun f ->
         Directory.GetDirectories(Path.getFullName f)
         |> Seq.collect (fun d -> Directory.GetFiles(d, "*.resources.dll")))
    |> Seq.concat
    |> Seq.map
         (fun x -> (x, Some(path + Path.GetFileName(Path.GetDirectoryName(x))), None))
    |> Seq.distinctBy (fun (x, y, _) -> (Option.get y) + "/" + (Path.GetFileName x))
    |> Seq.toList

  let nupkg = (Path.getFullName "./nupkg").Length

  let otherFiles =
    (!!"./nupkg/**/*.*")
    |> Seq.map (fun x ->
         (x, Some(Path.GetDirectoryName(x).Substring(nupkg).Replace("\\", "/")), None))
    |> Seq.toList

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
      (!!"./_Binaries/AltCover.Toolkit/Release+AnyCPU/netstandard2.0/*.Toolkit.*") ]
    |> Seq.concat
    |> Seq.map (fun x -> (x, Some(where + Path.GetFileName x), None))
    |> Seq.toList

  let poshHelpFiles where =
    [poshHelp]
    |> Seq.map (fun x -> (x, Some(where + Path.GetFileName x), None))
    |> Seq.toList

  let cakeFiles where =
    (!!"./_Binaries/AltCover.Cake/Release+AnyCPU/netstandard2.0/AltCover.C*.*")
    |> Seq.map (fun x -> (x, Some(where + Path.GetFileName x), None))
    |> Seq.toList

  let dataFiles1 where =
    (!!"./_Binaries/AltCover.DataCollector/Release+AnyCPU/netstandard2.0/AltCover.D*.*")
    |> Seq.map (fun x -> (x, Some(where + Path.GetFileName x), None))
    |> Seq.toList

  let dataFiles2 where =
    (!!"./_Binaries/AltCover.DataCollector/Release+AnyCPU/netstandard2.0/*/AltCover.DataCollector.resources.dll")
    |> Seq.map (fun x -> let d = Path.GetDirectoryName x
                         let locale = Path.GetFileName d
                         (x, Some(where + locale + "/" + (Path.GetFileName x)), None))
    |> Seq.toList

  let dataFiles where = [dataFiles1; dataFiles2]
                        |> List.collect (fun f -> f where)

  let fakeFiles where =
    [ (!!"./_Binaries/AltCover.Fake/Release+AnyCPU/netstandard2.0/AltCover.Fak*.*")
      (!!"./_Binaries/AltCover.DotNet/Release+AnyCPU/netstandard2.0/AltCover.Dot*.*") ]
    |> Seq.concat
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
    |> Seq.map (fun x ->
         (x, Some(where + Path.GetDirectoryName(x).Substring(publish).Replace("\\", "/")),
          None))
    |> Seq.toList

  let publishapi = (Path.getFullName "./_Publish").Length

  let netstdFiles where =
    (!!"./_Publish/**/*.*")
    |> Seq.map (fun x ->
         (x,
          Some(where + Path.GetDirectoryName(x).Substring(publishapi).Replace("\\", "/")),
          None))
    |> Seq.toList

  let globalFiles =
    (!!"./_Binaries/AltCover/Release+AnyCPU/netcoreapp2.1/AltCover.*")
    |> Seq.map
         (fun x -> (x, Some("tools/netcoreapp2.1/any/" + Path.GetFileName x), None))
    |> Seq.toList

  let publishV = (Path.getFullName "./_Publish.visualizer").Length

  let vizFiles where =
    (!!"./_Publish.visualizer/**/*.*")
    |> Seq.map (fun x ->
         (x, Some(where + Path.GetDirectoryName(x).Substring(publishV).Replace("\\", "/")),
          None))
    |> Seq.toList

  let auxVFiles =
    (!!"./_Binaries/AltCover.Visualizer/Release+AnyCPU/netcoreapp2.1/*.xml")
    |> Seq.map
         (fun x -> (x, Some("tools/netcoreapp2.1/any/" + Path.GetFileName x), None))
    |> Seq.toList

  let auxFiles =
    (!!"./_Binaries/AltCover/Release+AnyCPU/netcoreapp2.1/*.xml")
    |> Seq.map
         (fun x -> (x, Some("tools/netcoreapp2.1/any/" + Path.GetFileName x), None))
    |> Seq.toList

  printfn "Executing on %A" Environment.OSVersion
  [ (List.concat
       [ applicationFiles
         resourceFiles "tools/net472/"
         libFiles "tools/net472/"
         netcoreFiles "tools/netcoreapp2.0/"
         poshFiles "tools/netcoreapp2.0/"
         poshHelpFiles "tools/netcoreapp2.0/"
         dataFiles "tools/netcoreapp2.0/"
         otherFiles
         housekeeping ], [], "_Packaging", "./Build/AltCover.nuspec", "altcover")

    (List.concat
      [ apiFiles
        resourceFiles "lib/net472/"
        libFiles "lib/net472/"
        netstdFiles "lib/netstandard2.0"
        cakeFiles "lib/netstandard2.0/"
        dataFiles "lib/netstandard2.0/"
        fakeFiles "lib/netstandard2.0/"
        poshFiles "lib/netstandard2.0/"
        poshHelpFiles "lib/netstandard2.0/"
        otherFilesApi
        housekeeping
         ],
      [ // these are and should be opt-in, depnding which if any you want
//        ("Cake.Common", "0.28")
//        ("Cake.Core", "0.28" )
//        ("Fake.Core.Trace", "5.0.0")
//        ("Fake.DotNet.Cli", "5.0.0")
        ], "_Packaging.api", "./_Generated/altcover.api.nuspec", "altcover.api")

    (List.concat
      [ globalFiles
        netcoreFiles "tools/netcoreapp2.1/any"
        poshFiles "tools/netcoreapp2.1/any/"
        poshHelpFiles "tools/netcoreapp2.1/any/"
        dataFiles "tools/netcoreapp2.1/any/"
        [ (packable, Some "", None) ]
        auxFiles
        otherFilesGlobal
        housekeeping ], [], "_Packaging.global", "./_Generated/altcover.global.nuspec",
     "altcover.global")

    (List.concat
      [ vizFiles "tools/netcoreapp2.1/any"
        [ (packable, Some "", None) ]
        auxVFiles
        housekeepingVis ], [], "_Packaging.visualizer",
     "./_Generated/altcover.visualizer.nuspec", "altcover.visualizer")

    (List.concat
      [ fake2Files "lib/netstandard2.0/"
        fox2Files "lib/netstandard2.0/"
        [ (packable, Some "", None) ]
        housekeeping ],
     [ // make these explicit, as this package implies an opt-in
       ("Fake.Core.Environment", "5.18.1")
       ("Fake.DotNet.Cli", "5.18.1")
       ("FSharp.Core", "4.7")
       ("System.Collections.Immutable", "1.6.0") ], "_Packaging.fake",
     "./_Generated/altcover.fake.nuspec", "altcover.fake") ]
  |> List.iter (fun (files, dependencies, output, nuspec, project) ->
       let outputPath = "./" + output
       let workingDir = "./_Binaries/" + output
       Directory.ensure workingDir
       Directory.ensure outputPath

       NuGet (fun p ->
         { p with
             Authors = [ "Steve Gilham" ]
             Project = project
             Description =
               "A cross-platform pre-instrumenting code coverage tool set for .net/.net core and Mono"
             OutputPath = outputPath
             WorkingDir = workingDir
             Files = files |> List.distinctBy (fun (f1,f2,_) -> let name = Path.GetFileName f1
                                                                let path = match f2 with
                                                                           | Some s -> s.Replace("\\", "/")
                                                                           | _ -> ""
                                                                if path.EndsWith(name, StringComparison.Ordinal)
                                                                then path
                                                                else path + "/" + name)
             Dependencies = dependencies
             Version = !Version
             Copyright = (!Copyright).Replace("©", "(c)")
             Publish = false
             ReleaseNotes =
               "This build from https://github.com/SteveGilham/altcover/tree/"
               + commitHash + Environment.NewLine + Environment.NewLine
               + (Path.getFullName "ReleaseNotes.md" |> File.ReadAllText)
             ToolPath =
               if Environment.isWindows then
                 ("./packages/" + (packageVersion "NuGet.CommandLine")
                  + "/tools/NuGet.exe") |> Path.getFullName
               else
                 "/usr/bin/nuget" }) nuspec))

_Target "PrepareFrameworkBuild" ignore

_Target "PrepareDotNetBuild" (fun _ ->
  let netcoresource = Path.getFullName "./AltCover/AltCover.fsproj"
  let publish = Path.getFullName "./_Publish"

  DotNet.publish (fun options ->
    { options with
        OutputPath = Some publish
        Configuration = DotNet.BuildConfiguration.Release
        Framework = Some "netcoreapp2.0" }) netcoresource
  DotNet.publish (fun options ->
    { options with
        OutputPath = Some(publish + ".visualizer")
        Configuration = DotNet.BuildConfiguration.Release
        Framework = Some "netcoreapp2.1" })
    (Path.getFullName "./AltCover.Avalonia/AltCover.Avalonia.fsproj")

  // dotnet tooling mods
  [ ("DotnetTool", "./_Generated/altcover.global.nuspec",
     "AltCover (dotnet global tool install)", None, None)

    ("DotnetTool", "./_Generated/altcover.visualizer.nuspec",
     "AltCover.Visualizer (dotnet global tool install)",
     Some "AltCover.UICommon/logo.png", Some "codecoverage .netcore cross-platform")

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
           tag.Value <- tag.Value.Replace("Build/AltCover_128.png", logo)
           let tag2 = dotnetNupkg.Descendants(x "icon") |> Seq.head
           tag2.Value <- tag2.Value.Replace("AltCover_128.png", Path.GetFileName logo)
       match tags with
       | None -> ()
       | Some line ->
           let tagnode = dotnetNupkg.Descendants(x "tags") |> Seq.head
           tagnode.Value <- line
       dotnetNupkg.Save path))

_Target "PrepareReadMe" (fun _ ->
  Actions.PrepareReadMe
    ((!Copyright).Replace("©", "&#xa9;").Replace("<", "&lt;").Replace(">", "&gt;")))

// Post-packaging deployment touch test

_Target "Deployment" ignore

_Target "Unpack" (fun _ ->
  !!"./_Pack*/*.nupkg"
  |> Seq.iter (fun nugget ->
    let packdir = Path.GetDirectoryName nugget
    let unpack = Path.getFullName (packdir @@ "Unpack")
    System.IO.Compression.ZipFile.ExtractToDirectory(nugget, unpack))

  // C# style API documentation
  let packages =
    let xml =
     [
      "./AltCover.PowerShell/AltCover.PowerShell.fsproj"
      "./AltCover.Cake/AltCover.Cake.csproj"
     ]
     |> List.map (Path.getFullName >>  XDocument.Load)
    xml
    |> List.map (fun x -> x.Descendants(XName.Get("PackageReference")))
    |> Seq.concat
    |> Seq.map
         (fun x ->
           let incl = x.Attribute(XName.Get("Include"))
           let update = x.Attribute(XName.Get("Update"))
           let version = x.Attribute(XName.Get("Version")).Value
           if incl |> isNull
           then (update.Value, version)
           else (incl.Value, version))
    |> Seq.distinctBy fst
    |> Map.ofSeq
  let packageVersionPart (p : string) = nugetCache +
                                        "/" + p.ToLowerInvariant() + "/" + (packages.Item p) +
                                        "/lib/netstandard2.0/"

  let unpacked = "./_Packaging.api/Unpack/lib/netstandard2.0/"
  Shell.copyFile (unpacked + "Cake.Core.dll")
      ((packageVersionPart "Cake.Core") + "Cake.Core.dll")
  Shell.copyFile (unpacked + "Cake.Common.dll")
      ((packageVersionPart "Cake.Common") + "Cake.Common.dll")
  Shell.copyFile (unpacked + "System.Management.Automation.dll")
      ((packageVersionPart "PowerShellStandard.Library") + "System.Management.Automation.dll")

  [
    "AltCover.Cake"
    "AltCover.DotNet"
    "AltCover.Engine" // beware static linkage -- maybe copy from debug?
    "AltCover.PowerShell"
    "AltCover.Toolkit"
  ]
  |> List.iter (fun n ->
    Shell.copyFile (unpacked + n + ".xml") ("./_Binaries/" + n + "/Release+AnyCPU/netstandard2.0/" + n + ".xml")
    Actions.RunDotnet dotnetOptions "xmldocmd"
     (unpacked + n + ".dll ./_Documentation/" + n + " --visibility public --skip-unbrowsable --clean")
     ("documenting " + n)))

_Target "WindowsPowerShell" (fun _ ->
  Directory.ensure "./_Documentation"

  let v = (!Version).Split([| '-' |]).[0]
  CreateProcess.fromRawCommand "powershell.exe"
    [ "-NoProfile"; "./Build/pester.ps1"; "-ACV"; v; "-ReportName"; "PoshReport"; "-FolderName"; "Unpack" ]
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
  let key = Path.getFullName "Build/Infrastructure.snk"

  let prep =
    AltCover.PrepareOptions.Primitive
      ({ Primitive.PrepareOptions.Create() with
           XmlReport = report
           InputDirectories = [ i ]
           StrongNameKey = key
           TypeFilter = [ "System\\."; "DotNet" ]
           AssemblyFilter = [ "AltCover.Engine"; "Recorder"; "DataCollector"; "FSharp" ]
           InPlace = true
           ReportFormat = "OpenCover"
           Save = true
           VisibleBranches = true })
    |> AltCoverCommand.Prepare
  { AltCoverCommand.Options.Create prep with
      ToolPath = "AltCover.dll"
      ToolType = dotnetAltcover
      WorkingDirectory = unpack }
  |> AltCoverCommand.run

  printfn "Execute the instrumented tests"
  CreateProcess.fromRawCommand pwsh [ "-NoProfile"; "./Build/pester.ps1"; "-ACV"; v; "-ReportName"; "PesterReport"; "-FolderName"; "Module" ]
  |> CreateProcess.withWorkingDirectory "."
  |> Proc.run
  |> (Actions.AssertResult "pwsh")

  let collect =
    AltCover.CollectOptions.Primitive
      { Primitive.CollectOptions.Create() with RecorderDirectory = i }
    |> AltCoverCommand.Collect
  { AltCoverCommand.Options.Create collect with
      ToolPath = "AltCover.dll"
      ToolType = dotnetAltcover
      WorkingDirectory = unpack }
  |> AltCoverCommand.run

  ReportGenerator.generateReports (fun p ->
    { p with
        ToolType = ToolType.CreateLocalTool()
        ReportTypes =
          [ ReportGenerator.ReportType.Html; ReportGenerator.ReportType.XmlSummary ]
        TargetDir = "_Reports/_Pester" }) [ report ]

  "_Reports/_Pester/Summary.xml"
  |> File.ReadAllText
  |> printfn "%s")

_Target "SimpleReleaseTest" (fun _ ->
  let unpack =
    match NuGetAltCover with
    | Some test ->
        Trace.traceImportant "Using the NuGet package"
        Path.GetDirectoryName test
    | _ -> Path.getFullName "_Packaging/Unpack/tools/net472"

  Actions.SimpleInstrumentingRun "_Binaries/Sample1/Debug+AnyCPU/net20" unpack
    "SimpleReleaseTest")

_Target "SimpleZipReleaseTest" (fun _ ->
  let binaryPath =
    match NuGetAltCover with
    | Some test ->
        Trace.traceImportant "Using the NuGet package"
        Path.GetDirectoryName test
    | _ -> Path.getFullName "_Packaging/Unpack/tools/net472"

  let reportSigil = "SimpleZipReleaseTest"

  printfn "Instrument and run a simple executable"
  Directory.ensure "./_Reports"
  let simpleReport = (Path.getFullName "./_Reports") @@ (reportSigil + ".xml")
  let binRoot = Path.getFullName binaryPath
  let sampleRoot = Path.getFullName "_Binaries/Sample1/Debug+AnyCPU/net20"
  let instrumented = "__Instrumented." + reportSigil
  let framework = Fake.DotNet.ToolType.CreateFullFramework()

  let prep =
    AltCover.PrepareOptions.Primitive
      { Primitive.PrepareOptions.Create() with
          TypeFilter = [ """System\.""" ]
          XmlReport = simpleReport
          OutputDirectories = [| "./" + instrumented |]
          ReportFormat = "NCover"
          ZipFile = true
          InPlace = false
          Save = false }
    |> AltCoverCommand.Prepare

  let parameters =
    { AltCoverCommand.Options.Create prep with
        ToolPath = binRoot @@ "AltCover.exe"
        ToolType = framework
        WorkingDirectory = sampleRoot }

  AltCoverCommand.run parameters
  System.Threading.Thread.Sleep(1000)

  Actions.Run (sampleRoot @@ (instrumented + "/Sample1.exe"), (sampleRoot @@ instrumented), [])
    "Instrumented .exe failed"
  System.Threading.Thread.Sleep(1000)

  printfn "Extract and verify the first results"
  System.IO.Compression.ZipFile.ExtractToDirectory(simpleReport + ".zip", (Path.GetDirectoryName simpleReport) @@ ("unzip" + reportSigil))

  Actions.ValidateSample1 (((Path.GetDirectoryName simpleReport) @@ ("unzip" + reportSigil)) @@ (Path.GetFileName simpleReport)) reportSigil)

_Target "SimpleMonoReleaseTest" (fun _ ->
  let unpack =
    match NuGetAltCover with
    | Some test ->
        Trace.traceImportant "Using the NuGet package"
        Path.GetDirectoryName test
    | _ -> Path.getFullName "_Packaging/Unpack/tools/net472"

  Actions.SimpleInstrumentingRun "_Mono/Sample1" unpack "SimpleMonoReleaseTest")

_Target "ReleaseDotNetWithFramework" (fun _ ->
  Directory.ensure "./_Reports"
  let unpack =
    match NuGetAltCover with
    | Some test ->
        Trace.traceImportant "Using the NuGet package"
        Path.GetDirectoryName test
    | _ -> Path.getFullName "_Packaging/Unpack/tools/net472"

  let simpleReport =
    (Path.getFullName "./_Reports") @@ ("ReleaseDotNetWithFramework.xml")
  let sampleRoot = Path.getFullName "./_Binaries/Sample1/Debug+AnyCPU/netcoreapp2.0"
  let instrumented = sampleRoot @@ "__Instrumented.ReleaseDotNetWithFramework"

  let prep =
    AltCover.PrepareOptions.Primitive
      ({ Primitive.PrepareOptions.Create() with
           XmlReport = simpleReport
           OutputDirectories = [ instrumented ]
           TypeFilter = [ "System\\."; "Microsoft\\." ]
           InPlace = false
           ReportFormat = "NCover"
           Save = false })
    |> AltCoverCommand.Prepare
  { AltCoverCommand.Options.Create prep with
      ToolPath = (unpack @@ "AltCover.exe")
      ToolType = frameworkAltcover
      WorkingDirectory = sampleRoot }
  |> AltCoverCommand.run

  Actions.RunDotnet (fun o -> { dotnetOptions o with WorkingDirectory = instrumented })
    "" "Sample1.dll" "ReleaseDotNetWithFramework test"

  Actions.ValidateSample1 "./_Reports/ReleaseDotNetWithFramework.xml"
    "ReleaseDotNetWithFramework")

_Target "ReleaseMonoWithDotNet" (fun _ ->
  Directory.ensure "./_Reports"
  let unpack = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0"
  let x = Path.getFullName "./_Reports/ReleaseMonoWithDotNet.xml"
  let o = Path.getFullName "./_Mono/__Instrumented.ReleaseMonoWithDotNet"
  let i = Path.getFullName "./_Mono/Sample1"

  let prep =
    AltCover.PrepareOptions.Primitive
      ({ Primitive.PrepareOptions.Create() with
           XmlReport = x
           OutputDirectories = [ o ]
           InputDirectories = [ i ]
           InPlace = false
           ReportFormat = "NCover"
           Save = false })
    |> AltCoverCommand.Prepare
  { AltCoverCommand.Options.Create prep with
      ToolPath = "AltCover.dll"
      ToolType = dotnetAltcover
      WorkingDirectory = unpack }
  |> AltCoverCommand.run

  Actions.Run (o @@ "Sample1.exe", o, []) "Instrumented .exe failed"
  Actions.ValidateSample1 "./_Reports/ReleaseMonoWithDotNet.xml" "ReleaseMonoWithDotNet")

_Target "ReleaseDotNetWithDotNet" (fun _ ->
  Directory.ensure "./_Reports"
  let unpack = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0"
  let x = Path.getFullName "./_Reports/ReleaseDotNetWithDotNet.xml"
  let o = Path.getFullName "./_Binaries/Sample1/__Instrumented.ReleaseDotNetWithDotNet"
  let i = Path.getFullName "./_Binaries/Sample1/Debug+AnyCPU/netcoreapp2.0"

  let prep =
    AltCover.PrepareOptions.Primitive
      ({ Primitive.PrepareOptions.Create() with
           XmlReport = x
           OutputDirectories = [ o ]
           InputDirectories = [ i ]
           InPlace = false
           ReportFormat = "NCover"
           Save = false })
    |> AltCoverCommand.Prepare
  { AltCoverCommand.Options.Create prep with
      ToolPath = "AltCover.dll"
      ToolType = dotnetAltcover
      WorkingDirectory = unpack }
  |> AltCoverCommand.run
  Actions.RunDotnet dotnetOptions "" (o @@ "Sample1.dll") "ReleaseDotNetWithDotNet test"
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
    AltCover.PrepareOptions.Primitive
      ({ Primitive.PrepareOptions.Create() with
           XmlReport = x
           OutputDirectories = [ o ]
           InputDirectories = [ i ]
           AssemblyFilter = [ "Adapter" ]
           InPlace = false
           ReportFormat = "NCover"
           Save = false })
    |> AltCoverCommand.Prepare
  { AltCoverCommand.Options.Create prep with
      ToolPath = "AltCover.dll"
      ToolType = dotnetAltcover
      WorkingDirectory = unpack }
  |> AltCoverCommand.run

  Actions.ValidateFSharpTypes x [ "main" ]

  printfn "Execute the instrumented tests"
  let sample2 = Path.getFullName "./Sample2/Sample2.fsproj"
  let runner = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0/AltCover.dll"
  let (dotnetexe, args) = defaultDotNetTestCommandLine (Some "netcoreapp2.1") sample2

  // Run
  let collect =
    AltCover.CollectOptions.Primitive
      { Primitive.CollectOptions.Create() with
          Executable = dotnetexe
          RecorderDirectory = o
          CommandLine = args }
    |> AltCoverCommand.Collect
  { AltCoverCommand.Options.Create collect with
      ToolPath = runner
      ToolType = dotnetAltcover
      WorkingDirectory = o }
  |> AltCoverCommand.run
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
        { o' with
            Common =
              { o'.Common with
                  WorkingDirectory = s
                  DotNetCliPath = dotnetPath86 |> Option.get } })
      |> printfn "%A"

      printfn "Build the sample2 code as x86"
      "./AltCover.sln"
      |> Path.GetFullPath
      |> DotNet.build (fun p ->
           { p with
               Configuration = DotNet.BuildConfiguration.Debug
               Common =
                 { p.Common with
                     WorkingDirectory = s
                     DotNetCliPath = dotnetPath86 |> Option.get }
               MSBuildParams = cliArguments })

      printfn "Instrument the code"
      let altcover = unpack @@ "AltCover.dll"

      let prep =
        AltCover.PrepareOptions.Primitive
          ({ Primitive.PrepareOptions.Create() with
               XmlReport = x
               OutputDirectories = [ o ]
               InputDirectories = [ i ]
               AssemblyFilter = [ "Adapter" ]
               InPlace = false
               ReportFormat = "NCover"
               Save = false })
        |> AltCoverCommand.Prepare
      { AltCoverCommand.Options.Create prep with
          ToolPath = altcover
          ToolType = dotnetAltcover86
          WorkingDirectory = unpack }
      |> AltCoverCommand.run
      Actions.ValidateFSharpTypes x [ "main" ]
      printfn "Execute the instrumented tests"
      let sample2 = Path.getFullName "./Sample2/Sample2.fsproj"

      // Run
      let (dotnetexe, args) =
        defaultDotNetTestCommandLine86 (Some "netcoreapp2.1") sample2

      let collect =
        AltCover.CollectOptions.Primitive
          { Primitive.CollectOptions.Create() with
              Executable = dotnetexe
              RecorderDirectory = o
              CommandLine = args }
        |> AltCoverCommand.Collect
      { AltCoverCommand.Options.Create collect with
          ToolPath = altcover
          ToolType = dotnetAltcover86
          WorkingDirectory = o }
      |> AltCoverCommand.run

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
    AltCover.PrepareOptions.Primitive
      ({ Primitive.PrepareOptions.Create() with
           XmlReport = x
           OutputDirectories = [ o ]
           InputDirectories = [ i ]
           AssemblyFilter = [ "xunit" ]
           InPlace = false
           ReportFormat = "NCover"
           Save = false })
    |> AltCoverCommand.Prepare
  { AltCoverCommand.Options.Create prep with
      ToolPath = "AltCover.dll"
      ToolType = dotnetAltcover
      WorkingDirectory = unpack }
  |> AltCoverCommand.run

  Actions.ValidateFSharpTypes x [ "main" ]

  printfn "Execute the instrumented tests"
  "Sample4.fsproj"
  |> DotNet.test (fun o ->
       { o.WithCommon(withWorkingDirectoryVM "Sample4") with
           Configuration = DotNet.BuildConfiguration.Debug
           Framework = Some "netcoreapp2.1"
           NoBuild = true }
       |> testWithCLIArguments)
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
    AltCover.PrepareOptions.Primitive
      ({ Primitive.PrepareOptions.Create() with
           XmlReport = x
           OutputDirectories = [ o ]
           InputDirectories = [ i ]
           AssemblyFilter = [ "xunit" ]
           InPlace = false
           ReportFormat = "NCover"
           Save = false })
    |> AltCoverCommand.Prepare
  { AltCoverCommand.Options.Create prep with
      ToolPath = "AltCover.dll"
      ToolType = dotnetAltcover
      WorkingDirectory = unpack }
  |> AltCoverCommand.run

  Actions.ValidateFSharpTypes x [ "main" ]

  printfn "Execute the instrumented tests"
  let sample4 = Path.getFullName "./Sample4/Sample4.fsproj"
  let runner = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0/AltCover.dll"
  let (dotnetexe, args) = defaultDotNetTestCommandLine (Some "netcoreapp2.1") sample4

  // Run
  let collect =
    AltCover.CollectOptions.Primitive
      { Primitive.CollectOptions.Create() with
          Executable = dotnetexe
          RecorderDirectory = o
          CommandLine = args }
    |> AltCoverCommand.Collect
  { AltCoverCommand.Options.Create collect with
      ToolPath = runner
      ToolType = dotnetAltcover
      WorkingDirectory = o }
  |> AltCoverCommand.run
  Actions.ValidateFSharpTypesCoverage x)

_Target "OpenCoverForPester" (fun _ ->
  Directory.ensure "./_Reports"
  let reportDir = Path.getFullName "./_Reports/OpenCoverForPester"
  Directory.ensure reportDir
  let unpack = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0"
  let x = Path.getFullName "./_Reports/OpenCoverForPester/OpenCoverForPester.xml"
  let o = Path.getFullName "Sample18/_Binaries/Sample18/Debug+AnyCPU/netcoreapp3.0"
  let i = Path.getFullName "_Binaries/Sample18/Debug+AnyCPU/netcoreapp3.0"

  Shell.cleanDir o

  // Instrument the code
  let prep =
    AltCover.PrepareOptions.Primitive
      ({ Primitive.PrepareOptions.Create() with
           XmlReport = x
           OutputDirectories = [ o ]
           InputDirectories = [ i ]
           AssemblyFilter = [ "xunit"; "FSharp" ]
           InPlace = false
           ReportFormat = "OpenCover"
           Save = false })
    |> AltCoverCommand.Prepare
  { AltCoverCommand.Options.Create prep with
      ToolPath = "AltCover.dll"
      ToolType = dotnetAltcover
      WorkingDirectory = unpack }
  |> AltCoverCommand.run

  printfn "Execute the instrumented tests"
  let sample = Path.getFullName "./Sample18/Sample18.fsproj"
  let runner = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0/AltCover.dll"
  let (dotnetexe, args) = defaultDotNetTestCommandLine (Some "netcoreapp3.0") sample

  // Run
  let collect =
    AltCover.CollectOptions.Primitive
      { Primitive.CollectOptions.Create() with
          Executable = dotnetexe
          RecorderDirectory = o
          CommandLine = args }
    |> AltCoverCommand.Collect
  { AltCoverCommand.Options.Create collect with
      ToolPath = runner
      ToolType = dotnetAltcover
      WorkingDirectory = o }
  |> AltCoverCommand.run

  // now do it for coverlet
  let here = Path.GetDirectoryName sample
  let tr = here @@ "TestResults"
  Directory.ensure tr
  Directory.ensure tr
  Shell.cleanDir tr
  try
    DotNet.build (fun p ->
      { p.WithCommon dotnetOptions with Configuration = DotNet.BuildConfiguration.Debug }
      |> buildWithCLIArguments) sample
    DotNet.test coverletTestOptionsSample sample
  with x -> eprintf "%A" x
  let covxml = (!!(tr @@ "*/coverage.opencover.xml") |> Seq.head) |> Path.getFullName
  let target = reportDir @@ "OpenCoverForPester.coverlet.xml"
  Shell.copyFile target covxml
  let binary = here @@ "_Binaries/Sample18/Debug+AnyCPU/netcoreapp3.0/Sample18.dll"
  let binaryTarget = reportDir @@ "Sample18.dll"
  Shell.copyFile binaryTarget binary
  let binary2 = here @@ "_Binaries/Sample18/Debug+AnyCPU/netcoreapp3.0/Sample18.pdb"
  let binary2Target = reportDir @@ "Sample18.pdb"
  Shell.copyFile binary2Target binary2)

_Target "ReleaseXUnitFSharpTypesShowVisualized" (fun _ ->
  Directory.ensure "./_Reports"
  let unpack = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0"
  let x = Path.getFullName "./_Reports/ShowStatic.xml"
  let x1 = Path.getFullName "./_Reports/ShowStaticPP.xml"
  let x2 = Path.getFullName "./_Reports/ShowGenerated.xml"
  let x3 = Path.getFullName "./_Reports/ShowGeneratedRun.xml"
  let o = Path.getFullName "Sample4/_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.1"
  let i = Path.getFullName "_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.1"

  Shell.cleanDir o

  // Instrument the code
  let prep =
    AltCover.PrepareOptions.Primitive
      ({ Primitive.PrepareOptions.Create() with
           XmlReport = x
           OutputDirectories = [ o ]
           InputDirectories = [ i ]
           AssemblyFilter = [ "xunit" ]
           InPlace = false
           ReportFormat = "NCover"
           Save = false
           ShowStatic = "+" })
    |> AltCoverCommand.Prepare
  { AltCoverCommand.Options.Create prep with
      ToolPath = "AltCover.dll"
      ToolType = dotnetAltcover
      WorkingDirectory = unpack }
  |> AltCoverCommand.run

  do use coverageFile =
       new FileStream(x, FileMode.Open, FileAccess.Read, FileShare.None, 4096,
                      FileOptions.SequentialScan)
     // Edit xml report to store new hits
     let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))

     let vcs =
       coverageDocument.Descendants(XName.Get("seqpnt"))
       |> Seq.map (fun x -> x.Attribute(XName.Get "visitcount").Value |> int)
       |> Seq.groupBy id
       |> Seq.sortBy fst
       |> Seq.toList
     printfn "%A" vcs
     printfn "%A" (vcs |> List.map (snd >> Seq.length))
     Assert.That(vcs |> List.map fst, Is.EqualTo [ -3; 0 ], "-3 or 0 only")
     Assert.That(vcs |> List.map (snd >> Seq.length), Is.EqualTo [ 10; 28 ], "10 and 28")

  let prep =
    AltCover.PrepareOptions.Primitive
      ({ Primitive.PrepareOptions.Create() with
           XmlReport = x1
           OutputDirectories = [ o ]
           InputDirectories = [ i ]
           AssemblyFilter = [ "xunit" ]
           InPlace = false
           ReportFormat = "NCover"
           Save = false
           ShowStatic = "++" })
    |> AltCoverCommand.Prepare
  { AltCoverCommand.Options.Create prep with
      ToolPath = "AltCover.dll"
      ToolType = dotnetAltcover
      WorkingDirectory = unpack }
  |> AltCoverCommand.run

  do use coverageFile =
       new FileStream(x1, FileMode.Open, FileAccess.Read, FileShare.None, 4096,
                      FileOptions.SequentialScan)
     // Edit xml report to store new hits
     let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))

     let vcs =
       coverageDocument.Descendants(XName.Get("seqpnt"))
       |> Seq.map (fun x -> x.Attribute(XName.Get "visitcount").Value |> int)
       |> Seq.groupBy id
       |> Seq.sortBy fst
       |> Seq.toList
     printfn "%A" vcs
     printfn "%A" (vcs |> List.map (snd >> Seq.length))
     Assert.That(vcs |> List.map fst, Is.EqualTo [ 0 ])
     Assert.That(vcs |> List.map (snd >> Seq.length), Is.EqualTo [ 38 ])

  let prep =
    AltCover.PrepareOptions.Primitive
      ({ Primitive.PrepareOptions.Create() with
           XmlReport = x2
           OutputDirectories = [ o ]
           InputDirectories = [ i ]
           AssemblyFilter = [ "xunit" ]
           InPlace = false
           ReportFormat = "NCover"
           Save = false
           ShowGenerated = true })
    |> AltCoverCommand.Prepare
  { AltCoverCommand.Options.Create prep with
      ToolPath = "AltCover.dll"
      ToolType = dotnetAltcover
      WorkingDirectory = unpack }
  |> AltCoverCommand.run

  do use coverageFile =
       new FileStream(x2, FileMode.Open, FileAccess.Read, FileShare.None, 4096,
                      FileOptions.SequentialScan)
     // Edit xml report to store new hits
     let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))

     let vcs =
       coverageDocument.Descendants(XName.Get("seqpnt"))
       |> Seq.map (fun x -> x.Attribute(XName.Get "visitcount").Value |> int)
       |> Seq.groupBy id
       |> Seq.sortBy fst
       |> Seq.toList
     printfn "%A" vcs
     printfn "%A" (vcs |> List.map (snd >> Seq.length))
     Assert.That(vcs |> List.map fst, Is.EqualTo [ -2; 0 ], "Expect -2, 0")
     Assert.That
       (vcs |> List.map (snd >> Seq.length), Is.EqualTo [ 6; 22 ], "Expect 6, 22")

  let prep =
    AltCover.PrepareOptions.Primitive
      ({ Primitive.PrepareOptions.Create() with
           XmlReport = x3
           OutputDirectories = [ o ]
           InputDirectories = [ i ]
           AssemblyFilter = [ "xunit" ]
           InPlace = false
           ReportFormat = "NCover"
           Save = false
           ShowGenerated = true })
    |> AltCoverCommand.Prepare
  { AltCoverCommand.Options.Create prep with
      ToolPath = "AltCover.dll"
      ToolType = dotnetAltcover
      WorkingDirectory = unpack }
  |> AltCoverCommand.run

  printfn "Execute the instrumented tests"
  let sample4 = Path.getFullName "./Sample4/Sample4.fsproj"
  let runner = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0/AltCover.dll"
  let (dotnetexe, args) = defaultDotNetTestCommandLine (Some "netcoreapp2.1") sample4

  // Run
  let collect =
    AltCover.CollectOptions.Primitive
      { Primitive.CollectOptions.Create() with
          Executable = dotnetexe
          RecorderDirectory = o
          CommandLine = args }
    |> AltCoverCommand.Collect
  { AltCoverCommand.Options.Create collect with
      ToolPath = runner
      ToolType = dotnetAltcover
      WorkingDirectory = o }
  |> AltCoverCommand.run

  do use coverageFile =
       new FileStream(x3, FileMode.Open, FileAccess.Read, FileShare.None, 4096,
                      FileOptions.SequentialScan)
     // Edit xml report to store new hits
     let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))

     let vcs =
       coverageDocument.Descendants(XName.Get("seqpnt"))
       |> Seq.map (fun x -> x.Attribute(XName.Get "visitcount").Value |> int)
       |> Seq.groupBy id
       |> Seq.sortBy fst
       |> Seq.toList
     printfn "%A" vcs
     printfn "%A" (vcs |> List.map (snd >> Seq.length))
     Assert.That
       (vcs |> List.map fst, Is.EqualTo [ -2; 0; 1; 2 ], "expect [ -2; 0; 1; 2]")
     Assert.That
       (vcs |> List.map (snd >> Seq.length), Is.EqualTo [ 3; 14; 10; 1 ],
        "expect [3 ; 10; 9; 1]"))

_Target "ReleaseXUnitFSharpTypesDotNetFullRunner" (fun _ ->
  Directory.ensure "./_Reports"
  let unpack = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0"
  let x = Path.getFullName "./_Reports/ReleaseXUnitFSharpTypesDotNetFullRunner.xml"
  let o = Path.getFullName "Sample4/_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.1"
  let i = Path.getFullName "_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.1"

  Shell.cleanDir o
  let prep =
    AltCover.PrepareOptions.Primitive
      ({ Primitive.PrepareOptions.Create() with
           XmlReport = x
           OutputDirectories = [ o ]
           InputDirectories = [ i ]
           CallContext = [ "0"; "[Fact]" ]
           AssemblyFilter = [ "xunit" ]
           InPlace = false
           Save = false })
    |> AltCoverCommand.Prepare
  { AltCoverCommand.Options.Create prep with
      ToolPath = "AltCover.dll"
      ToolType = dotnetAltcover
      WorkingDirectory = unpack }
  |> AltCoverCommand.run
  Actions.CheckSample4Content x

  printfn "Execute the instrumented tests"
  let sample4 = Path.getFullName "./Sample4/Sample4.fsproj"
  let runner = Path.getFullName "_Packaging/Unpack/tools/netcoreapp2.0/AltCover.dll"
  let (dotnetexe, args) = defaultDotNetTestCommandLine (Some "netcoreapp2.1") sample4

  // Run
  let collect =
    AltCover.CollectOptions.Primitive
      { Primitive.CollectOptions.Create() with
          Executable = dotnetexe
          RecorderDirectory = o
          CommandLine = args }
    |> AltCoverCommand.Collect
  { AltCoverCommand.Options.Create collect with
      ToolPath = runner
      ToolType = dotnetAltcover
      WorkingDirectory = o }
  |> AltCoverCommand.run
  Actions.CheckSample4Visits x)

_Target "MSBuildTest" (fun _ ->
  Directory.ensure "./_Reports"
  let build = Path.getFullName "Build"
  let sample = Path.getFullName "Sample4"
  let x = Path.getFullName "./_Reports/MSBuildTest.xml"

  // Run
  Shell.cleanDir (sample @@ "_Binaries")
  DotNet.msbuild (fun opt ->
    opt.WithCommon(fun o' -> { dotnetOptions o' with WorkingDirectory = sample }))
    (build @@ "msbuildtest.proj")
  printfn "Checking samples4 output"
  Actions.CheckSample4 x

  // touch-test framework
  let unpack =
    match NuGetAltCover with
    | Some test ->
        Trace.traceImportant "Using the NuGet package"
        Path.GetDirectoryName test
    | _ -> Path.getFullName "_Packaging/Unpack/tools/net472"

  MSBuild.build (fun p ->
    { p with
        Verbosity = Some MSBuildVerbosity.Minimal
        Properties =
          [ "Configuration", "Debug"
            "MSBuildTest", "true"
            "AltCoverPath", unpack.Replace('\\', '/')
            "DebugSymbols", "True" ] }) "./Sample4/Sample4LongForm.fsproj")

_Target "ApiUse" (fun _ ->
  try
    Directory.ensure "./_ApiUse"
    Shell.cleanDir ("./_ApiUse")
    Directory.ensure "./_ApiUse/_DotnetTest"

    let apiroot = Path.GetFullPath "./_Packaging.api"
    let fakeroot = Path.GetFullPath "./_Packaging.fake"
    if "./Build/paket.lock" |> File.Exists then
      // manage the dependencies
      let lines =
        "./Build/paket.lock"
        |> File.ReadAllLines
        |> Array.map (fun line -> String.Format(line, !Version, apiroot, fakeroot))
      File.WriteAllLines("./_ApiUse/paket.lock", lines)

    let config = XDocument.Load "./Build/NuGet.config.dotnettest"
    let repo = config.Descendants(XName.Get("add")) |> Seq.head
    repo.SetAttributeValue(XName.Get "value", Path.getFullName "./_Packaging")
    config.Save "./_ApiUse/_DotnetTest/NuGet.config"

    let fsproj = XDocument.Load "./Sample4/Sample4.fsproj"
    let targets = fsproj.Descendants(XName.Get("TargetFrameworks")) |> Seq.head
    targets.SetValue "netcoreapp2.1"
    let pack = fsproj.Descendants(XName.Get("PackageReference")) |> Seq.head
    let inject =
      XElement
        (XName.Get "PackageReference", XAttribute(XName.Get "Include", "altcover.api"),
         XAttribute(XName.Get "Version", !Version))
    pack.AddBeforeSelf inject
    fsproj.Save "./_ApiUse/_DotnetTest/dotnettest.fsproj"
    Shell.copy "./_ApiUse/_DotnetTest" (!!"./Sample4/*.fs")
    Shell.copy "./_ApiUse/_DotnetTest" (!!"./Sample4/*.json")

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

open AltCover.Fake.DotNet // extension methods

let _Target s f =
  Target.description s
  Target.create s f

_Target "DoIt"
  (fun _ ->
  let expected = {0}
  let acv = AltCover.Command.Version()
  printfn "AltCover.Command.Version - Returned %A expected %A" acv expected
  if acv.ToString() <> expected
  then failwith "AltCover.Command.Version mismatch"

  let acfv = AltCover.Command.FormattedVersion()
  printfn "AltCover.Command.FormattedVersion - Returned '%s' expected %A" acfv expected
  if acfv <> (sprintf "AltCover version %s" expected)
  then failwith "AltCover.Command.FormattedVersion mismatch"

  let afcv = AltCover.Fake.Command.Version().ToString()
  afcv |> Trace.trace
  printfn "expected %A" expected
  if afcv.ToString() <> expected
  then failwith "AltCover.Fake.Command.Version mismatch"

  let collect =
    AltCover.AltCover.CollectOptions.Primitive
      { AltCover.Primitive.CollectOptions.Create() with LcovReport = "x" }
  let prepare =
    AltCover.AltCover.PrepareOptions.Primitive
      { AltCover.Primitive.PrepareOptions.Create() with TypeFilter = [| "a"; "b" |] }
  let ForceTrue = AltCover.DotNet.CLIOptions.Force true
  printfn "Test arguments : '%s'" (AltCover.DotNet.ToTestArguments prepare collect ForceTrue)

  let t = DotNet.TestOptions.Create().WithAltCoverOptions prepare collect ForceTrue
  printfn "WithAltCoverOptions returned '%A'" t.Common.CustomParams

  let p2 =
    { AltCover.Primitive.PrepareOptions.Create() with
        CallContext = [| "[Fact]"; "0" |]
        AssemblyFilter = [| "xunit" |] }

  let pp2 = AltCover.AltCover.PrepareOptions.Primitive p2
  let c2 = AltCover.Primitive.CollectOptions.Create()
  let cc2 = AltCover.AltCover.CollectOptions.Primitive c2

  let setBaseOptions (o: DotNet.Options) =
    { o with
        WorkingDirectory = Path.getFullName "./_DotnetTest"
        Verbosity = Some DotNet.Verbosity.Minimal }

  let cliArguments =
    { MSBuild.CliArguments.Create() with
        ConsoleLogParameters = []
        DistributedLoggers = None
        DisableInternalBinLog = true }

  DotNet.test
    (fun to' ->
    { to'.WithCommon(setBaseOptions).WithAltCoverOptions pp2 cc2 ForceTrue with
        MSBuildParams = cliArguments }) "dotnettest.fsproj"
  let ImportModule =
    (AltCover.Command.ImportModule().Trim().Split()
     |> Seq.take 2
     |> Seq.skip 1
     |> Seq.head).Trim([| '"' |])

  let command = "$ImportModule = '" + ImportModule + "'; Import-Module $ImportModule; ConvertTo-BarChart -?"

  let corePath = AltCover.Fake.Command.ToolPath AltCover.Fake.Implementation.DotNetCore
  printfn "corePath = %A" corePath
  let frameworkPath = AltCover.Fake.Command.ToolPath AltCover.Fake.Implementation.Framework
  printfn "frameworkPath = %A" frameworkPath

  if frameworkPath |> String.IsNullOrEmpty |> not
  then
    let framework = Fake.DotNet.ToolType.CreateFullFramework()

    { AltCoverFake.DotNet.Testing.AltCoverCommand.Options.Create
        AltCoverFake.DotNet.Testing.AltCoverCommand.ArgumentType.GetVersion
          with
            ToolType = framework
            ToolPath = frameworkPath }
    |> AltCoverFake.DotNet.Testing.AltCoverCommand.run

  let core = Fake.DotNet.ToolType.CreateFrameworkDependentDeployment id

  { AltCoverFake.DotNet.Testing.AltCoverCommand.Options.Create
      AltCoverFake.DotNet.Testing.AltCoverCommand.ArgumentType.GetVersion
        with
          ToolType = core
          ToolPath = corePath }
  |> AltCoverFake.DotNet.Testing.AltCoverCommand.run

  let pwsh =
    if Environment.isWindows then
      Fake.Core.ProcessUtils.findLocalTool String.Empty "pwsh.exe"
        [ Environment.environVar "ProgramFiles" @@ "PowerShell" ]
    else "pwsh"

  let r =
    CreateProcess.fromRawCommand pwsh [ "-NoProfile"; "-Command"; command ]
    |> CreateProcess.withWorkingDirectory "."
    |> Proc.run

  if (r.ExitCode <> 0) then new InvalidOperationException("Non zero return code") |> raise)
Target.runOrDefault "DoIt"
"""
    let vv = !Version + "-"
    let ver = vv.Split([|'-'|]) |> Seq.head

    File.WriteAllText("./_ApiUse/DriveApi.fsx", script.Replace("{0}","\"" + ver + "\""))

    let dependencies = """version 5.249.0
// [ FAKE GROUP ]
group NetcoreBuild
  source https://api.nuget.org/v3/index.json
  nuget Fake.Core.Target >= 5.20.3
  nuget Fake.DotNet.Cli >= 5.20.3
  source {0}
  nuget AltCover.Api {1}
  source {2}
  nuget AltCover.Fake {1} """

    File.WriteAllText
      ("./_ApiUse/paket.dependencies",
       String.Format
         (dependencies, Path.getFullName "./_Packaging.api", !Version,
          Path.getFullName "./_Packaging.fake"))

    Actions.RunDotnet (withWorkingDirectoryOnly "_ApiUse") "fake" "run ./DriveApi.fsx"
      "running fake script returned with a non-zero exit code"

    let x = Path.getFullName "./_ApiUse/_DotnetTest/coverage.netcoreapp2.1.xml"
    Actions.CheckSample4 x
  finally
    [ "altcover"; "altcover.api"; "altcover.fake" ]
    |> List.iter (fun f ->
         let folder = (nugetCache @@ f) @@ !Version
         Shell.mkdir folder
         Shell.deleteDir folder))

_Target "DotnetTestIntegration" (fun _ ->
  try
    printfn "Initializing ------------------------------------------------"

    [
     ("./_DotnetTest", "Sample4", "fsproj")
     ("./_DotnetTestFail", "Sample13", "fsproj")
     ("./_DotnetTestFailFast", "Sample13", "fsproj")
     ("./_DotnetTestFailInstrumentation", "Sample13", "fsproj")
     ("./_DotnetTestLineCover", "Sample10", "csproj")
     ("./_DotnetTestBranchCover", "Sample10", "csproj")
    ]
    |> List.iter (fun (d, p, t) ->  Directory.ensure d
                                    Shell.cleanDir d
                                    let config = XDocument.Load "./Build/NuGet.config.dotnettest"
                                    let repo = config.Descendants(XName.Get("add")) |> Seq.head
                                    repo.SetAttributeValue(XName.Get "value", Path.getFullName "./_Packaging")
                                    config.Save (d @@ "NuGet.config")

                                    let projpath = "./" + p + "/" + p + "." + t
                                    printfn "%s -> %s" d projpath

                                    let fsproj = XDocument.Load projpath
                                    let targets = [
                                                    fsproj.Descendants(XName.Get("TargetFrameworks"))
                                                    fsproj.Descendants(XName.Get("TargetFramework"))
                                                  ]
                                                  |> Seq.concat
                                                  |> Seq.head
                                    targets.SetValue "netcoreapp2.1"
                                    let pack = fsproj.Descendants(XName.Get("PackageReference")) |> Seq.head
                                    let inject =
                                      XElement
                                        (XName.Get "PackageReference", XAttribute(XName.Get "Include", "altcover"),
                                         XAttribute(XName.Get "Version", !Version))
                                    pack.AddBeforeSelf inject
                                    fsproj.Save (d + "/dotnettest." + t)
                                    Shell.copy d !!("./" + p + "/*." + t.Substring(0, 2))
                                    Shell.copy d !!("./" + p + "/*.json")
    )

    printfn "Simple positive case ------------------------------------------------"
    let p0 = Primitive.PrepareOptions.Create()
    let c0 = Primitive.CollectOptions.Create()

    let p1 =
      { p0 with
          CallContext = [ "[Fact]"; "0" ]
          AssemblyFilter = [| "xunit" |] }

    let pp1 = AltCover.PrepareOptions.Primitive p1
    let cc0 = AltCover.CollectOptions.Primitive { c0 with SummaryFormat = "+B" }
    DotNet.test (fun to' ->
      (to'.WithCommon(withWorkingDirectoryVM "_DotnetTest").WithAltCoverGetVersion()
          .WithAltCoverImportModule()).WithAltCoverOptions pp1 cc0 ForceTrue
      |> testWithCLIArguments) "dotnettest.fsproj"

    let x = Path.getFullName "./_DotnetTest/coverage.netcoreapp2.1.xml"
    Actions.CheckSample4 x

    printfn "optest failing instrumentation ------------------------------------------------"

    let xx0 = Path.getFullName "./_Reports/nonesuch.xml"

    let pf0 =
      { p0 with AssemblyFilter = [| "NUnit" |]
                StrongNameKey = "./_Reports/nonesuch.junk"
                XmlReport = xx0
      } |> AltCover.PrepareOptions.Primitive

    try
      DotNet.test (fun to' ->
        (to'.WithCommon(withWorkingDirectoryVM "_DotnetTestFailInstrumentation")).WithAltCoverOptions
          pf0 cc0 ForceTrue |> testWithCLIArguments) "dotnettest.fsproj"
      Assert.Fail("Build exception should be raised")
    with :? Fake.DotNet.MSBuildException -> printfn "Caught expected exception"
    Assert.That (xx0 |> File.Exists |> not)
    Assert.That("./_DotnetTestFailInstrumentation/bin/Debug/netcoreapp2.1/dotnettest.dll.txt" |> File.Exists |> not)

    printfn "optest failing test ------------------------------------------------"

    let xx = Path.getFullName "./_DotnetTestFail/coverage.xml"
    let pf1 =
      { p0 with AssemblyFilter = [| "NUnit" |] } |> AltCover.PrepareOptions.Primitive

    try
      DotNet.test (fun to' ->
        (to'.WithCommon(withWorkingDirectoryVM "_DotnetTestFail")).WithAltCoverOptions
          pf1 cc0 ForceTrue |> testWithCLIArguments) "dotnettest.fsproj"
      Assert.Fail("Build exception should be raised")
    with :? Fake.DotNet.MSBuildException -> printfn "Caught expected exception"
    Assert.That("./_DotnetTestFail/bin/Debug/netcoreapp2.1/dotnettest.dll.txt" |> File.Exists)

    do use coverageFile =
         new FileStream(xx, FileMode.Open, FileAccess.Read, FileShare.None, 4096,
                        FileOptions.SequentialScan)
       let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))

       let recorded =
         coverageDocument.Descendants(XName.Get("SequencePoint"))
         |> Seq.map (fun x -> x.Attribute(XName.Get("vc")).Value)
         |> Seq.toList

       Assert.That(recorded, Is.EquivalentTo [ "1"; "1"; "1"; "1"; "0" ])

    printfn "optest failing test fast ------------------------------------------------"

    let xx = Path.getFullName "./_DotnetTestFailFast/coverage.xml"
    let pf1 =
      { p0 with AssemblyFilter = [| "NUnit" |] } |> AltCover.PrepareOptions.Primitive

    try
      DotNet.test (fun to' ->
        (to'.WithCommon(withWorkingDirectoryVM "_DotnetTestFailFast")).WithAltCoverOptions
          pf1 cc0 FailTrue |> testWithCLIArguments) "dotnettest.fsproj"
      Assert.Fail("Build exception should be raised")
    with :? Fake.DotNet.MSBuildException -> printfn "Caught expected exception"
    Assert.That("./_DotnetTestFailFast/bin/Debug/netcoreapp2.1/dotnettest.dll.txt" |> File.Exists)

    do use coverageFile =
         new FileStream(xx, FileMode.Open, FileAccess.Read, FileShare.None, 4096,
                        FileOptions.SequentialScan)
       let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))

       let recorded =
         coverageDocument.Descendants(XName.Get("SequencePoint"))
         |> Seq.map (fun x -> x.Attribute(XName.Get("vc")).Value)
         |> Seq.toList

       Assert.That(recorded, Is.EquivalentTo [ "0"; "0"; "0"; "0"; "0" ])

    printfn "optest line cover ------------------------------------------------"
    let p2 =
      { p0 with
          LineCover = true
          AssemblyFilter = [| "xunit" |] }

    let pp2 = AltCover.PrepareOptions.Primitive p2
    DotNet.test (fun to' ->
      to'.WithCommon(withWorkingDirectoryVM "_DotnetTestLineCover").WithAltCoverOptions
        pp2 cc0 ForceTrue |> testWithCLIArguments) ""

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

    printfn "optest branch cover ------------------------------------------------"

    let p3 =
      { p0 with
          BranchCover = true
          AssemblyFilter = [| "xunit" |] }

    let pp3 = AltCover.PrepareOptions.Primitive p3

    DotNet.test (fun to' ->
      (to'.WithCommon(withWorkingDirectoryVM "_DotnetTestBranchCover").WithAltCoverOptions
        pp3 cc0 ForceTrue) |> testWithCLIArguments) ""

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

    printfn "Regression test issue 29 ------------------------------------------------"
    let proj = XDocument.Load "./RegressionTesting/issue29/issue29.xml"
    let pack = proj.Descendants(XName.Get("PackageReference")) |> Seq.head
    let inject =
      XElement
        (XName.Get "PackageReference", XAttribute(XName.Get "Include", "altcover"),
         XAttribute(XName.Get "Version", !Version))
    pack.AddBeforeSelf inject
    proj.Save "./RegressionTesting/issue29/issue29.csproj"
    let p29 = { p0 with AssemblyFilter = [ "NUnit" ] }
    let pp29 = AltCover.PrepareOptions.Primitive p29

    DotNet.test (fun to' ->
      (to'.WithCommon(withWorkingDirectoryVM "RegressionTesting/issue29").WithAltCoverOptions
        pp29 cc0 ForceTrueFast) |> testWithCLIArguments) ""

    printfn "Regression test issue 37 ------------------------------------------------"
    let proj = XDocument.Load "./RegressionTesting/issue37/issue37.xml"
    let pack = proj.Descendants(XName.Get("PackageReference")) |> Seq.head
    let inject =
      XElement
        (XName.Get "PackageReference", XAttribute(XName.Get "Include", "altcover"),
         XAttribute(XName.Get "Version", !Version))
    pack.AddBeforeSelf inject
    proj.Save "./RegressionTesting/issue37/issue37.csproj"

    let p4 = { p0 with AssemblyFilter = [ "NUnit" ] }
    let pp4 = AltCover.PrepareOptions.Primitive p4
    DotNet.test (fun to' ->
      { ((to'.WithCommon(withWorkingDirectoryVM "RegressionTesting/issue37")).WithAltCoverOptions
          pp4 cc0 ForceTrue) with Configuration = DotNet.BuildConfiguration.Release }
      |> testWithCLIArguments) ""

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
      (fun o ->
        o.WithCommon(withWorkingDirectoryVM "./RegressionTesting/issue20/classlib")) ""
    DotNet.restore
      (fun o ->
        o.WithCommon(withWorkingDirectoryVM "./RegressionTesting/issue20/xunit-tests"))
      ""

    // would like to assert "succeeds with warnings"
    let p0 = { Primitive.PrepareOptions.Create() with AssemblyFilter = [| "xunit" |] }
    let pp0 = AltCover.PrepareOptions.Primitive p0
    let c0 = Primitive.CollectOptions.Create()
    let cc0 = AltCover.CollectOptions.Primitive c0
    DotNet.test (fun to' ->
      ({ to'.WithCommon(withWorkingDirectoryVM "./RegressionTesting/issue20/xunit-tests") with
           Configuration = DotNet.BuildConfiguration.Debug
           NoBuild = false }).WithAltCoverOptions pp0 cc0 ForceTrue
      |> testWithCLIArguments) ""

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
  //               Verbosity = SomeDotNet.Verbosity.Minimal }).WithAltCoverOptions p1 c0 ForceTrue with Configuration =
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
    Shell.copy "./_Issue23" (!!"./Sample9/*.json")
    DotNet.restore (fun o -> o.WithCommon(withWorkingDirectoryVM "_Issue23")) ""

    let p0 = { Primitive.PrepareOptions.Create() with AssemblyFilter = [| "xunit" |] }
    let pp0 = AltCover.PrepareOptions.Primitive p0
    let c0 = Primitive.CollectOptions.Create()
    let cc0 = AltCover.CollectOptions.Primitive c0
    DotNet.test (fun p ->
      (({ p.WithCommon(withWorkingDirectoryVM "_Issue23") with
            Configuration = DotNet.BuildConfiguration.Debug
            NoBuild = false }).WithAltCoverOptions pp0 cc0 ForceTrue)
        .WithAltCoverImportModule().WithAltCoverGetVersion()
      |> testWithCLIArguments) ""
  finally
    let folder = (nugetCache @@ "altcover") @@ !Version
    Shell.mkdir folder
    Shell.deleteDir folder)

_Target "Issue67" (fun _ ->
  try
    Directory.ensure "./_Issue67" // escaping the | in a regex by doubling
    Shell.cleanDir ("./_Issue67")
    let config = XDocument.Load "./Build/NuGet.config.dotnettest"
    let repo = config.Descendants(XName.Get("add")) |> Seq.head
    repo.SetAttributeValue(XName.Get "value", Path.getFullName "./_Packaging")
    config.Save "./_Issue67/NuGet.config"

    let csproj = XDocument.Load "./Sample9/sample9.csproj"
    let target = csproj.Descendants(XName.Get("TargetFramework")) |> Seq.head
    target.SetValue "netcoreapp2.1"

    let pack = csproj.Descendants(XName.Get("PackageReference")) |> Seq.head
    let inject =
      XElement
        (XName.Get "PackageReference", XAttribute(XName.Get "Include", "altcover"),
         XAttribute(XName.Get "Version", !Version))
    pack.AddBeforeSelf inject
    csproj.Save "./_Issue67/sample9.csproj"
    Shell.copy "./_Issue67" (!!"./Sample9/*.cs")
    Shell.copy "./_Issue67" (!!"./Sample9/*.json")
    DotNet.restore (fun o -> o.WithCommon(withWorkingDirectoryVM "_Issue67")) ""

    let p0 =
      { Primitive.PrepareOptions.Create() with
          AssemblyExcludeFilter = [| "?(sample9||xunit.runner.reporters.netcoreapp10)" |] }
    let pp0 = AltCover.PrepareOptions.Primitive p0
    let c0 = Primitive.CollectOptions.Create()
    let cc0 = AltCover.CollectOptions.Primitive c0
    DotNet.test (fun p ->
      (({ p.WithCommon(withWorkingDirectoryVM "_Issue67") with
            Configuration = DotNet.BuildConfiguration.Debug
            NoBuild = false }).WithAltCoverOptions pp0 cc0 ForceTrue)
        .WithAltCoverImportModule().WithAltCoverGetVersion()
      |> testWithCLIArguments) ""

    let cover = XDocument.Load "./_Issue67/coverage.xml"

    let passed =
      cover.Descendants(XName.Get("Module"))
      |> Seq.filter (fun x -> x.Attribute(XName.Get("skippedDueTo")) |> isNull)
      |> Seq.length

    Assert.That(passed, Is.EqualTo 2)
  finally
    let folder = (nugetCache @@ "altcover") @@ !Version
    Shell.mkdir folder
    Shell.deleteDir folder)

_Target "Issue72" (fun _ ->
  try
    Directory.ensure "./Sample16/Test/_Issue72"
    Shell.cleanDir ("./Sample16/Test/_Issue72")

    let config = XDocument.Load "./Build/NuGet.config.dotnettest"
    let repo = config.Descendants(XName.Get("add")) |> Seq.head
    repo.SetAttributeValue(XName.Get "value", Path.getFullName "./_Packaging")
    config.Save "./Sample16/Test/_Issue72/NuGet.config"

    Shell.copy "./Sample16/Test/_Issue72" (!!"./Sample16/Test/Test/*.cs")
    Shell.copy "./Sample16/Test/_Issue72" (!!"./Sample16/Test/Test/*.json")

    let csproj = XDocument.Load "./Sample16/Test/Test/Test.csproj"

    let pack = csproj.Descendants(XName.Get("PackageReference")) |> Seq.head
    let inject =
      XElement
        (XName.Get "PackageReference", XAttribute(XName.Get "Include", "altcover"),
         XAttribute(XName.Get "Version", !Version))
    pack.AddBeforeSelf inject
    csproj.Save "./Sample16/Test/_Issue72/Test.csproj"

    let p0 =
      { Primitive.PrepareOptions.Create() with
          LocalSource = true
          VisibleBranches = false
          TypeFilter = [ "UnitTest" ]
          XmlReport = "./original.xml" }

    let pp0 = AltCover.PrepareOptions.Primitive p0
    let c0 = Primitive.CollectOptions.Create()
    let cc0 = AltCover.CollectOptions.Primitive c0
    DotNet.test (fun p ->
      (({ p.WithCommon(withWorkingDirectoryVM "./Sample16/Test/_Issue72") with
            Configuration = DotNet.BuildConfiguration.Debug
            NoBuild = false }).WithAltCoverOptions pp0 cc0 ForceTrue)
        .WithAltCoverImportModule().WithAltCoverGetVersion()
      |> testWithCLIArguments) ""

    do use coverageFile =
         new FileStream("./Sample16/Test/_Issue72/original.xml", FileMode.Open,
                        FileAccess.Read, FileShare.None, 4096, FileOptions.SequentialScan)
       let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))

       let found =
         coverageDocument.Descendants(XName.Get("BranchPoint"))
         |> Seq.map (fun x -> x.Attribute(XName.Get("vc")).Value)
         |> Seq.toList
       Assert.That
         (found,
          Is.EquivalentTo [ "1"; "4"; "4"; "0"; "3"; "1"; "2"; "1"; "1"; "1"; "5"; "5" ],
          sprintf "original: %A" found)

    let p1 =
      { Primitive.PrepareOptions.Create() with
          LocalSource = true
          VisibleBranches = true
          TypeFilter = [ "UnitTest" ]
          XmlReport = "./combined.xml" }

    let pp1 = AltCover.PrepareOptions.Primitive p1
    let c0 = Primitive.CollectOptions.Create()
    let cc0 = AltCover.CollectOptions.Primitive c0
    DotNet.test (fun p ->
      (({ p.WithCommon(withWorkingDirectoryVM "./Sample16/Test/_Issue72") with
            Configuration = DotNet.BuildConfiguration.Debug
            NoBuild = false }).WithAltCoverOptions pp1 cc0 ForceTrue)
        .WithAltCoverImportModule().WithAltCoverGetVersion()
      |> testWithCLIArguments) ""

    do use coverageFile =
         new FileStream("./Sample16/Test/_Issue72/combined.xml", FileMode.Open,
                        FileAccess.Read, FileShare.None, 4096, FileOptions.SequentialScan)
       let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))

       let found =
         coverageDocument.Descendants(XName.Get("BranchPoint"))
         |> Seq.map (fun x -> x.Attribute(XName.Get("vc")).Value)
         |> Seq.toList
       Assert.That
         (found, Is.EquivalentTo [ "1"; "4"; "1"; "1"; "1"; "1"; "5"; "5" ],
          sprintf "combined: %A" found)

  finally
    let folder = (nugetCache @@ "altcover") @@ !Version
    Shell.mkdir folder
    Shell.deleteDir folder)

_Target "DotnetGlobalIntegration" (fun _ ->
  let working = Path.getFullName "./_DotnetGlobalTest"
  let mutable set = false
  try
    Directory.ensure working
    Shell.cleanDir working

    let fsproj = XDocument.Load "./Sample4/Sample4.fsproj"
    let targets = fsproj.Descendants(XName.Get("TargetFrameworks")) |> Seq.head
    targets.SetValue "netcoreapp2.1"
    fsproj.Save "./_DotnetGlobalTest/dotnetglobal.fsproj"
    Shell.copy "./_DotnetGlobalTest" (!!"./Sample4/*.fs")
    Shell.copy "./_DotnetGlobalTest" (!!"./Sample4/*.json")

    Actions.RunDotnet (fun o' -> { dotnetOptions o' with WorkingDirectory = working })
      "tool"
      ("install -g altcover.global --add-source "
       + (Path.getFullName "./_Packaging.global") + " --version " + !Version) "Installed"

    Actions.RunDotnet (fun o' -> { dotnetOptions o' with WorkingDirectory = working })
      "tool" ("list -g ") "Checked"
    set <- true

    ""
    |> DotNet.build (fun p ->
         { p with
             Configuration = DotNet.BuildConfiguration.Debug
             Common = { dotnetOptions p.Common with WorkingDirectory = working }
             MSBuildParams = cliArguments })

    let x = Path.getFullName "./_Reports/DotnetGlobalIntegration.xml"
    let o =
      Path.getFullName "./_DotnetGlobalTest/_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.1"
    [
      AltCoverCommand.ArgumentType.ImportModule
      AltCoverCommand.ArgumentType.GetVersion
    ]
    |> List.iter (AltCoverCommand.Options.Create
                  >> AltCoverCommand.run)

    Actions.Run("altcover", ".", ["TargetsPath"]) "altcover target"
    let prep =
      AltCover.PrepareOptions.Primitive
        ({ Primitive.PrepareOptions.Create() with
             XmlReport = x
             InputDirectories = [ o ]
             CallContext = [ "0"; "[Fact]" ]
             AssemblyFilter = [| "xunit" |]
             Save = false })
      |> AltCoverCommand.Prepare
    { AltCoverCommand.Options.Create prep with WorkingDirectory = working } |> AltCoverCommand.run

    Actions.CheckSample4Content x

    printfn "Execute the instrumented tests"
    let (dotnetexe, args) =
      defaultDotNetTestCommandLine (Some "netcoreapp2.1") String.Empty

    let collect =
      AltCover.CollectOptions.Primitive
        { Primitive.CollectOptions.Create() with
            Executable = dotnetexe
            RecorderDirectory = o
            CommandLine = args }
      |> AltCoverCommand.Collect
    { AltCoverCommand.Options.Create collect with WorkingDirectory = working } |> AltCoverCommand.run

    Actions.CheckSample4Visits x
    let command =
      """$ImportModule = (altcover ImportModule | Out-String).Trim().Split()[1].Trim(@([char]34)); Import-Module $ImportModule; ConvertTo-BarChart -?"""
    CreateProcess.fromRawCommand pwsh [ "-NoProfile"; "-Command"; command ]
    |> CreateProcess.withWorkingDirectory working
    |> Proc.run
    |> (Actions.AssertResult "pwsh")

  finally
    if set then
      Actions.RunDotnet (fun o' -> { dotnetOptions o' with WorkingDirectory = working })
        "tool" ("uninstall -g altcover.global") "uninstalled"
    let folder = (nugetCache @@ "altcover.global") @@ !Version
    Shell.mkdir folder
    Shell.deleteDir folder)

// AOB

_Target "MakeDocumentation" (fun _ ->
  let branch = Information.getBranchName(".")
  Assert.That(branch, Is.EqualTo("master").Or.StartWith("develop/docs/"), branch)
  CreateProcess.fromRawCommand "powershell.exe"
    [ "-NoProfile"; "./Build/prepareDocumentation.ps1" ]
  |> CreateProcess.withWorkingDirectory "."
  |> Proc.run
  |> (Actions.AssertResult "powershell"))

_Target "BulkReport" (fun _ ->
  printfn "Overall coverage reporting"

  // coverageSummary ()

  let issue71 = !!(@"./**/*.exn") |> Seq.toList
  match issue71 with
  | [] -> ()
  | _ ->
      issue71 |> Seq.iter (printfn "%s")
      Assert.Fail("Issue #71 experienced"))

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
==> "BuildDebug"

"BuildDebug"
==> "BuildRelease"
==> "Compilation"

"BuildRelease"
==> "BuildMonoSamples"
==> "Compilation"

"BuildDebug"
==> "Lint"
==> "Analysis"

"Compilation"
?=> "Analysis"

"BuildDebug"
==> "FxCop"
=?> ("Analysis", Environment.isWindows && fxcop |> Option.isSome) // not supported

"BuildDebug"
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
// =?> ("UnitTest", Environment.isWindows |> not)  // otherwise redundant; possibly flaky due to timeouts

"UnitTestDotNet"
==> "UnitTestWithAltCoverCoreRunner"
==> "UnitTest"

"Compilation"
==> "BuildForCoverlet"
==> "UnitTestDotNetWithCoverlet"
==> "UnitTest"

"JustUnitTest"
==> "UncoveredUnitTest"

"UnitTestDotNet"
==> "UncoveredUnitTest"

"Compilation"
?=> "OperationalTest"

"Compilation"
==> "FSharpTypes"
==> "OperationalTest"

"Compilation"
==> "FSharpTests"
==> "OperationalTest"

//"Compilation"
//==> "FSharpTypesDotNet"
//=?> ("OperationalTest", Environment.isWindows) -- timing window hits

"Compilation"
==> "FSharpTypesDotNetRunner"
==> "OperationalTest"

"Compilation"
==> "FSharpTypesDotNetCollecter"
==> "OperationalTest"

"Compilation"
==> "BasicCSharp"
==> "OperationalTest"

"Compilation"
==> "BasicCSharpMono"
=?> ("OperationalTest", Environment.isWindows) // redundant on mono

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
==> "OperationalTest" // AltCover.exe conditional

"Compilation"
==> "RecordResumeTrackingTest"
==> "OperationalTest" // AltCover.exe conditional

"Compilation"
==> "RecordResumeTestUnderMono"
//=?> ("OperationalTest", Option.isSome monoOnWindows) // Still fails, but not because entrypoint

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
==> "Packaging"

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

"FSharpTests"
==> "Pester"

"FSharpTests"
==> "WindowsPowerShell"
=?> ("Pester", Environment.isWindows)

"Unpack"
==> "WindowsPowerShell"
=?> ("Deployment", Environment.isWindows)

"ReleaseXUnitFSharpTypesDotNetRunner"
=?> ("WindowsPowerShell", Environment.isWindows)

"Unpack"
==> "OpenCoverForPester"
=?> ("WindowsPowerShell", Environment.isWindows)

"Unpack"
==> "ReleaseXUnitFSharpTypesDotNetRunner"
==> "Pester"
==> "Deployment"

"Unpack"
==> "OpenCoverForPester"
==> "Pester"

"Unpack"
==> "SimpleReleaseTest"
==> "Deployment"

"Unpack"
==> "SimpleZipReleaseTest"
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
==> "Deployment" // test is duplicated in the Pester testing

"Unpack"
==> "ReleaseFSharpTypesX86DotNetRunner"
=?> ("Deployment", Option.isSome dotnetPath86)

//"Unpack"
//==> "ReleaseXUnitFSharpTypesDotNet"
//==> "Deployment"  -- timing window hits

"Unpack"
==> "ReleaseXUnitFSharpTypesShowVisualized"
==> "Deployment"

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
==> "Issue67"
==> "Deployment"

"Unpack"
==> "Issue72"
==> "Deployment"

"Unpack"
==> "Issue20"
==> "Deployment"

"Unpack"
==> "DotnetGlobalIntegration"
==> "Deployment"

"Unpack"
==> "ReleaseXUnitFSharpTypesDotNetFullRunner"
==> "Deployment"

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

Target.runOrDefault <| defaultTarget()