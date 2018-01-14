open System
open System.IO
open System.Xml
open System.Xml.Linq

open Actions

open Fake
open Fake.AssemblyInfoFile
open Fake.Testing
open Fake.OpenCoverHelper
open Fake.ReportGeneratorHelper
open FSharpLint.Fake

let Copyright  = ref String.Empty
let Version = ref String.Empty

let OpenCoverFilter = "+[AltCove*]* -[*]Microsoft.* -[*]System.* +[*]N.*"
let AltCoverFilter= @" -s=Mono -s=\.Recorder -s=Sample -s=nunit -t=Tests -t=System. -t=Sample3\.Class2 "

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
               else version + "." + travis + "-travis"
             else appveyor
    let (v, majmin, y) = Actions.LocalVersion ci version
    Version := v
    let copy = sprintf "© 2010-%d by Steve Gilham <SteveGilham@users.noreply.github.com>" y
    Copyright := "Copyright " + copy

    ensureDirectory "./_Generated"
    Actions.InternalsVisibleTo (!Version)
    CreateFSharpAssemblyInfo "./_Generated/AssemblyVersion.fs"
        [Attribute.Version (majmin + ".0.0")
         Attribute.FileVersion (!Version)
         Attribute.Company "Steve Gilham"
         Attribute.Product "AltCover"
         Attribute.Trademark ""
         Attribute.Copyright copy
         ]
)

// Basic compilation

Target "Compilation" ignore

Target "BuildRelease" (fun _ ->
    [ "AltCover.sln" ]
     |> MSBuildRelease "" ""
     |> Log "AppBuild-Output: "

    DotNetCli.Build
        (fun p -> 
            { p with 
                Configuration = "Release"
                Project =  "./altcover.core.sln"})
)

Target "BuildDebug" (fun _ ->
   !! "**/AltCove*.sln"  // include demo projects
     |> Seq.filter (fun n -> n.IndexOf(".core.") = -1)
     |> Seq.filter (fun n -> n.IndexOf(".dotnet.") = -1)
     |> MSBuildDebug "" ""
     |> Log "AppBuild-Output: "

   DotNetCli.Build
        (fun p -> 
            { p with 
                Configuration = "Debug"
                Project =  "./altcover.core.sln"})
)

Target "BuildMonoSamples" (fun _ ->
    let mcs = findToolInSubPath "MCS.exe" ".."

    [
        ("./_Mono/Sample1", "-debug -out:./_Mono/Sample1/Sample1.exe  ./Sample1/Program.cs")
        ("./_Mono/Sample3", "-target:library -debug -out:./_Mono/Sample3/Sample3.dll  ./Sample3/Class1.cs")
    ]
    |> Seq.iter (fun (dir, cmd) -> ensureDirectory dir
                                   let result = ExecProcess (fun info -> info.FileName <- mcs
                                                                         info.WorkingDirectory <- "."
                                                                         info.Arguments <- cmd) (TimeSpan.FromMinutes 5.0)
                                   if result <> 0 then failwith ("Mono compilation of '" + cmd + "' failed"))

    Actions.FixMVId ["./_Mono/Sample1/Sample1.exe"; "./_Mono/Sample3/Sample3.dll"]
)

// Code Analysis

Target "Analysis" ignore

Target "Lint" (fun _ ->
    !! "**/*.fsproj"
        |> Seq.filter (fun n -> n.IndexOf(".core.") = -1)
        |> Seq.iter (FSharpLint (fun options -> { options with FailBuildIfAnyWarnings = true }) ))

Target "Gendarme" (fun _ -> // Needs debug because release is compiled --standalone which contaminates everything
  if String.IsNullOrWhiteSpace(environVar "TRAVIS_JOB_NUMBER") then
    ensureDirectory "./_Reports"

    let r = ExecProcess (fun info -> info.FileName <- (findToolInSubPath "gendarme.exe" "./packages")
                                     info.WorkingDirectory <- "."
                                     info.Arguments <- "--severity all --confidence all --config ./Build/rules.xml --console --html ./_Reports/gendarme.html _Binaries/AltCover/Debug+AnyCPU/AltCover.exe  _Binaries/AltCover.Shadow/Debug+AnyCPU/AltCover.Shadow.dll") (TimeSpan.FromMinutes 5.0)
    if r <> 0 then failwith  "Gendarme Errors were detected"
)

// Travis TODO
(*
------------------------------------------------------------
1. AvoidLongMethodsRule
Problem: Long methods are usually hard to understand and maintain.  This method can cause problems because it contains more code than the maximum allowed.
* Severity: High, Confidence: Normal
* Target:   AltCover.Instrument/Context AltCover.Instrument::InstrumentationVisitor(AltCover.Instrument/Context,AltCover.Node)
* Details:  Method IL Size: 324. Maximum Size: 165
Solution: You should apply an Extract Method refactoring, but there are other solutions.
More info available at: https://github.com/spouliot/gendarme/wiki/Gendarme.Rules.Smells.AvoidLongMethodsRule(2.10)
2. AvoidLongMethodsRule
Problem: Long methods are usually hard to understand and maintain.  This method can cause problems because it contains more code than the maximum allowed.
* Severity: High, Confidence: Normal
* Target:   Microsoft.FSharp.Collections.FSharpList`1<System.Xml.Linq.XElement> AltCover.Report/ReportVisitor@22::Invoke(Microsoft.FSharp.Collections.FSharpList`1<System.Xml.Linq.XElement>,AltCover.Node)
* Details:  Method IL Size: 419. Maximum Size: 165
Solution: You should apply an Extract Method refactoring, but there are other solutions.
More info available at: https://github.com/spouliot/gendarme/wiki/Gendarme.Rules.Smells.AvoidLongMethodsRule(2.10)
3. AvoidLongMethodsRule
Problem: Long methods are usually hard to understand and maintain.  This method can cause problems because it contains more code than the maximum allowed.
* Severity: High, Confidence: Normal
* Target:   System.Collections.Generic.IEnumerable`1<AltCover.Node> AltCover.Visitor::Deeper(AltCover.Node)
* Details:  Method IL Size: 182. Maximum Size: 165
Solution: You should apply an Extract Method refactoring, but there are other solutions.
More info available at: https://github.com/spouliot/gendarme/wiki/Gendarme.Rules.Smells.AvoidLongMethodsRule(2.10)
*)

Target "FxCop" (fun _ -> // Needs debug because release is compiled --standalone which contaminates everything
  if String.IsNullOrWhiteSpace(environVar "TRAVIS_JOB_NUMBER") then
    ensureDirectory "./_Reports"
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

    [ (["_Binaries/AltCover/Debug+AnyCPU/AltCover.exe"],   ["AltCover.Augment"
                                                            "AltCover.Filter"
                                                            "AltCover.Instrument"
                                                            "AltCover.KeyStore"
                                                            "AltCover.Main"
                                                            "AltCover.Naming"
                                                            "AltCover.ProgramDatabase"
                                                            "AltCover.Report"
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
                                       if fileExists "_Reports/FxCopReport.xml" then failwith "FxCop Errors were detected")
)

// Unit Test

Target "UnitTest" (fun _ ->
  !! (@"_Reports/*/Summary.xml") 
  |> Seq.iter (fun f -> let xml = XDocument.Load f
                        xml.Descendants(XName.Get("Linecoverage"))
                        |> Seq.iter (fun e -> let coverage = e.Value.Replace("%", String.Empty)
                                              match Double.TryParse coverage with
                                              | (false, _) -> failwith ("Could not parse coverage "+coverage)
                                              | (_, numeric) -> printfn "%s : %A" (f |> Path.GetDirectoryName |> Path.GetFileName) numeric
                                                                if numeric < 90.0 then failwith "Coverage is too low"
                        )
  )
)

Target "JustUnitTest" (fun _ ->
    ensureDirectory "./_Reports"
    !! (@"_Binaries/*Tests/Debug+AnyCPU/*.Test*.dll") // Need to figure out why it doesn't build in Release
    |> NUnit3 (fun p -> { p with ToolPath = findToolInSubPath "nunit3-console.exe" "."
                                 WorkingDir = "."
                                 ResultSpecs = ["./_Reports/JustUnitTestReport.xml"] })
)

Target "UnitTestDotNet" (fun _ ->
    ensureDirectory "./_Reports"
    !! (@"./*Tests/*.tests.core.fsproj")
    |> Seq.iter (fun f -> printfn "Testing %s" f
                          DotNetCli.Test
                             (fun p -> 
                                  { p with 
                                      Configuration = "Debug"
                                      Project =  f}))
)

Target "UnitTestWithOpenCover" (fun _ ->
  if String.IsNullOrWhiteSpace(environVar "TRAVIS_JOB_NUMBER") then
    ensureDirectory "./_Reports/_UnitTestWithOpenCover"
    let testFiles = !! (@"_Binaries/*Tests/Debug+AnyCPU/*.Test*.dll") 
                    //|> Seq.map (fun f -> f.FullName)
    let coverage = FullName "_Reports/UnitTestWithOpenCover.xml"

    OpenCover (fun p -> { p with ExePath = findToolInSubPath "OpenCover.Console.exe" "."
                                 WorkingDir = "."
                                 TestRunnerExePath = findToolInSubPath "nunit3-console.exe" "."
                                 Filter = "+[AltCover]* +[AltCover.Shadow]* -[*]Microsoft.* -[*]System.* -[Sample*]*"
                                 MergeByHash = true
                                 OptionalArguments = "-excludebyattribute:*ExcludeFromCodeCoverageAttribute;*ProgIdAttribute"
                                 Register = RegisterType.RegisterUser
                                 Output = coverage })
        (String.Join(" ", testFiles) + " --result=./_Reports/UnitTestWithOpenCoverReport.xml")

    ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                       ReportTypes = [ ReportGeneratorReportType.Html; ReportGeneratorReportType.Badges; ReportGeneratorReportType.XmlSummary ]
                                       TargetDir = "_Reports/_UnitTestWithOpenCover"})
        [coverage]

    if not <| String.IsNullOrWhiteSpace (environVar "APPVEYOR_BUILD_NUMBER") then
            ExecProcess (fun info -> info.FileName <- findToolInSubPath "coveralls.net.exe" "."
                                     info.WorkingDirectory <- "_Reports"
                                     info.Arguments <- ("--opencover " + coverage)) (TimeSpan.FromMinutes 5.0)
            |> ignore
)

// Hybrid (Self) Tests

Target "UnitTestWithAltCover" (fun _ ->
  if String.IsNullOrWhiteSpace(environVar "TRAVIS_JOB_NUMBER") then // Mono + F# + Fake build => no symbols
    ensureDirectory "./_Reports/_UnitTestWithAltCover"
    let keyfile = FullName "Build/SelfTest.snk"
    let reports = FullName "./_Reports"
    let altcover = findToolInSubPath "AltCover.exe" "./_Binaries"

    let altReport = reports @@ "UnitTestWithAltCover.xml"
    printfn "Instrumented the code"
    let result = ExecProcess (fun info -> info.FileName <- altcover
                                          info.WorkingDirectory <- FullName "_Binaries/AltCover.Tests/Debug+AnyCPU"
                                          info.Arguments <- ("/sn=" + keyfile + AltCoverFilter + @"/o=./__UnitTestWithAltCover -x=" + altReport)) (TimeSpan.FromMinutes 5.0)
    if result <> 0 then failwithf "Re-instrument returned with a non-zero exit code"

    printfn "Unit test the instrumented code"
    [ !! "_Binaries/AltCover.Tests/Debug+AnyCPU/__UnitTestWithAltCover/*.Tests.dll"
      !! "_Binaries/AltCover.Tests/Debug+AnyCPU/__UnitTestWithAltCover/*ple2.dll"]
    |> Seq.concat |> Seq.distinct
    |> NUnit3 (fun p -> { p with ToolPath = findToolInSubPath "nunit3-console.exe" "."
                                 WorkingDir = "."
                                 ResultSpecs = ["./_Reports/UnitTestWithAltCoverReport.xml"] })

    printfn "Instrument the shadow tests"
    let shadowDir = "_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU"
    let shadowReport = reports @@ "ShadowTestWithAltCover.xml"
    let result = ExecProcess (fun info -> info.FileName <- altcover
                                          info.WorkingDirectory <- FullName "_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU"
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
)

// Pure OperationalTests

Target "OperationalTest" ignore

Target "FSharpTypes" ( fun _ ->
  if String.IsNullOrWhiteSpace(environVar "TRAVIS_JOB_NUMBER") then // Mono + F# + Fake build => no symbols
    ensureDirectory "./_Reports"
    let simpleReport = (FullName "./_Reports") @@ ( "AltCoverFSharpTypes.xml")
    let binRoot = FullName "_Binaries/AltCover/Release+AnyCPU"
    let sampleRoot = FullName "_Binaries/Sample2/Debug+AnyCPU"
    let instrumented = "__FSharpTypes"
    let result = ExecProcess (fun info -> info.FileName <- binRoot @@ "AltCover.exe"
                                          info.WorkingDirectory <- sampleRoot
                                          info.Arguments <- ("-t=System\. -t=Microsoft\. -x=" + simpleReport + " /o=./" + instrumented)) (TimeSpan.FromMinutes 5.0)
    Actions.ValidateFSharpTypes simpleReport
)

Target "FSharpTypesDotNet" ( fun _ ->
    ensureDirectory "./_Reports"
    let project = FullName "./AltCover/altcover.core.fsproj"
    let simpleReport = (FullName "./_Reports") @@ ( "AltCoverFSharpTypesDotNet.xml")
    let sampleRoot = FullName "_Binaries/Sample2/Release+AnyCPU/netstandard2.0"
    let instrumented = "__FSharpTypesDotNet"
    DotNetCli.RunCommand (fun p -> {p with WorkingDir = sampleRoot}) 
                         ("run --project " + project + " -- -t \"System\\.\" -t \"Microsoft\\.\" -x \"" + simpleReport + "\" /o \"./" + instrumented + "\"")

    Actions.ValidateFSharpTypes simpleReport
)

Target "BasicCSharp" (fun _ ->
   Actions.SimpleInstrumentingRun "_Binaries/Sample1/Debug+AnyCPU" "_Binaries/AltCover/Debug+AnyCPU" "BasicCSharp"
)

Target "BasicCSharpMono" (fun _ ->
    Actions.SimpleInstrumentingRun "_Mono/Sample1" "_Binaries/AltCover/Debug+AnyCPU" "BasicCSharpMono"
)

Target "CSharpMonoWithDotNet" (fun _ ->
    ensureDirectory "./_Reports"
    let x = FullName "./_Reports/CSharpMonoWithDotNet.xml"
    let o = FullName "./_Mono/__Instrumented.CSharpMonoWithDotNet"
    let i = FullName "./_Mono/Sample1"
    DotNetCli.RunCommand id ("run --project ./AltCover/altcover.core.fsproj -- -t \"System.\" -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")

    let result2 = ExecProcess (fun info -> info.FileName <- o @@ "/Sample1.exe"
                                           info.WorkingDirectory <- o
                                           info.Arguments <- "") (TimeSpan.FromMinutes 5.0)
    if result2 <> 0 then failwith "Instrumented .exe failed"

    Actions.ValidateSample1 "./_Reports/CSharpMonoWithDotNet.xml" "CSharpMonoWithDotNet"
)

Target "CSharpDotNetWithDotNet" (fun _ -> // TODO
    ensureDirectory "./_Reports"
    let x = FullName "./_Reports/CSharpDotNetWithDotNet.xml"
    let o = FullName "../_Binaries/Sample1/__Instrumented.CSharpDotNetWithDotNet"
    let i = FullName "./_Binaries/Sample1/Debug+AnyCPU/netcoreapp2.0"
    DotNetCli.RunCommand id ("_Binaries/AltCover/Debug+AnyCPU/netcoreapp2.0/AltCover.dll -t \"System.\" -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")

    DotNetCli.RunCommand id (o @@ "Sample1.dll")

    Actions.ValidateSample1 "./_Reports/CSharpDotNetWithDotNet.xml" "CSharpDotNetWithDotNet"
)

Target "CSharpDotNetWithFramework" (fun _ -> // TODO
    ensureDirectory "./_Reports"
    let simpleReport = (FullName "./_Reports") @@ ( "CSharpDotNetWithFramework.xml")
    let binRoot = FullName "_Binaries/AltCover/Release+AnyCPU"
    let sampleRoot = FullName "_Binaries/Sample1/Debug+AnyCPU/netcoreapp2.0"
    let instrumented = FullName "_Binaries/Sample1/__Instrumented.CSharpDotNetWithFramework"
    let result = ExecProcess (fun info -> info.FileName <- binRoot @@ "AltCover.exe"
                                          info.WorkingDirectory <- sampleRoot
                                          info.Arguments <- ("-t=System\. -t=Microsoft\. -x=" + simpleReport + " /o=" + instrumented)) (TimeSpan.FromMinutes 5.0)

    DotNetCli.RunCommand id (instrumented @@ "Sample1.dll")

    Actions.ValidateSample1 "./_Reports/CSharpDotNetWithFramework.xml" "CSharpDotNetWithFramework"
)

Target "SelfTest" (fun _ ->
  if String.IsNullOrWhiteSpace(environVar "TRAVIS_JOB_NUMBER") then
    ensureDirectory "./_Reports/_Instrumented"
    let targetDir = "_Binaries/AltCover.Tests/Debug+AnyCPU"
    let reports = FullName "./_Reports"
    let report = reports @@ "OpenCoverSelfTest.xml"
    let altReport = reports @@ "AltCoverSelfTest.xml"
    let keyfile = FullName "Build/SelfTest.snk"


    printfn "Self-instrument under OpenCover"
    OpenCover (fun p -> { p with ExePath = findToolInSubPath "OpenCover.Console.exe" "."
                                 WorkingDir = targetDir
                                 TestRunnerExePath = findToolInSubPath "AltCover.exe" targetDir
                                 Filter = OpenCoverFilter
                                 MergeByHash = true
                                 OptionalArguments = "-excludebyattribute:*ExcludeFromCodeCoverageAttribute;*ProgIdAttribute"
                                 Register = RegisterType.RegisterUser
                                 Output = report })
        ("/sn=" + keyfile + AltCoverFilter + "-x=" + altReport + " -o __SelfTest")
    ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                       TargetDir = "_Reports/_OpenCoverSelfTest"})
        [report]

    printfn "Re-instrument everything"
    let altReport2 = reports @@ "AltCoverSelfTestDummy.xml"
    let result = ExecProcess (fun info -> info.FileName <- "_Binaries/AltCover.Tests/Debug+AnyCPU/__SelfTest/AltCover.exe"
                                          info.WorkingDirectory <- "_Binaries/AltCover.Tests/Debug+AnyCPU"
                                          info.Arguments <- ("/sn=" + keyfile + AltCoverFilter + @"/o=./__SelfTestDummy -x=" + altReport2)) (TimeSpan.FromMinutes 5.0)
    if result <> 0 then failwithf "Re-instrument returned with a non-zero exit code"

    ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                       TargetDir = "_Reports/_AltCoverSelfTest"})
        [altReport]
)

// Packaging 

Target "Packaging" (fun _ ->
    ensureDirectory "./_Binaries/Packaging"
    ensureDirectory "./_Packaging"

    let AltCover = FullName "_Binaries/AltCover/AltCover.exe"
    let recorder = FullName "_Binaries/AltCover/Release+AnyCPU/AltCover.Recorder.dll"
    let packable = FullName "./_Binaries/README.html"
    let resources = filesInDirMatchingRecursive "AltCover.resources.dll" (directoryInfo (FullName "_Binaries/AltCover/Release+AnyCPU")) 

    let applicationFiles = [
                            (AltCover, Some "tools/net45", None)
                            (recorder, Some "tools/net45", None)
                            (packable, Some "", None)
                           ]
    let resourceFiles = resources
                        |> Seq.map (fun x -> x.FullName)
                        |> Seq.map (fun x -> (x, Some ("tools/net45/" + Path.GetFileName(Path.GetDirectoryName(x))), None))
                        |> Seq.toList

    let root = (FullName ".").Length
    let netcoreFiles = [
                         [
                             FullName "./altcover.dotnet.sln"
                         ];
                         ((!! "./AltCover/*")
                          |> Seq.filter (fun n -> n.EndsWith(".fs") || n.EndsWith(".core.fsproj") || n.EndsWith(".resx"))
                          |> Seq.toList);
                         ((!! "./AltCover.Recorder/*")
                          |> Seq.filter (fun n -> n.EndsWith(".fs") || n.EndsWith(".core.fsproj"))
                          |> Seq.toList);
                         ((!! "./_Generated/*")
                          |> Seq.toList)                          
                       ]
                       |> List.concat
                       |> List.map (fun x -> (x, Some ("tools/netcoreapp2.0" + Path.GetDirectoryName(x).Substring(root).Replace("\\","/")), None))

    NuGet (fun p ->
    {p with
        Authors = ["Steve Gilham"]
        Project = "altcover"
        Description = "A pre-instrumented code coverage tool for .net and Mono"
        OutputPath = "./_Packaging"
        WorkingDir = "./_Binaries/Packaging"
        Files = List.concat [applicationFiles; resourceFiles; netcoreFiles]
        Version = !Version
        Copyright = (!Copyright).Replace("©", "(c)")
        Publish = false
        ReleaseNotes = FullName "ReleaseNotes.md"
                       |> File.ReadAllText 
        })
        "./Build/AltCover.nuspec"
)

Target "PrepareFrameworkBuild" (fun _ ->
    let toolpath = findToolInSubPath "ILMerge.exe" "./packages"

    ILMerge (fun p -> { p with DebugInfo = true
                               ToolPath = toolpath
                               TargetKind = TargetKind.Exe
                               KeyFile = "./Build/Infrastructure.snk"
                               Version = (String.Join(".", (!Version).Split('.') |> Seq.take 2) + ".0.0")
                               Internalize = InternalizeTypes.Internalize
                               Libraries = Seq.concat [!! "./_Binaries/AltCover/Release+AnyCPU/Mono.C*.dll"; !! "./_Binaries/AltCover/Release+AnyCPU/Newton*.dll"]
                               AttributeFile = "./_Binaries/AltCover/Release+AnyCPU/AltCover.exe"})
                               "./_Binaries/AltCover/AltCover.exe"
                               "./_Binaries/AltCover/Release+AnyCPU/AltCover.exe"
)

Target "PrepareDotNetBuild" (fun _ ->
    DotNetCli.Publish
      (fun p -> 
           { p with 
                WorkingDir =  FullName "./AltCover"
                Project = "altcover.core.fsproj"
                Output = FullName "./_Binaries/altcover.core"
                Configuration = "Release" })                       
)

Target "PrepareReadMe" (fun _ ->
    Actions.PrepareReadMe ((!Copyright).Replace("©", "&#xa9;").Replace("<","&lt;").Replace(">", "&gt;"))
)

// Post-packaging deployment touch test

Target "Deployment" ignore

Target "Unpack" (fun _ ->
  let nugget = !! "./_Packaging/*.nupkg" |> Seq.last
  let unpack = FullName "_Packaging/Unpack"
  System.IO.Compression.ZipFile.ExtractToDirectory (nugget, unpack)
)

Target "SimpleReleaseTest" (fun _ ->
    let unpack = FullName "_Packaging/Unpack/tools/net45"
    Actions.SimpleInstrumentingRun "_Binaries/Sample1/Debug+AnyCPU" unpack "SimpleReleaseTest"
)

Target "SimpleMonoReleaseTest" (fun _ ->
    let unpack = FullName "_Packaging/Unpack/tools/net45"
    Actions.SimpleInstrumentingRun "_Mono/Sample1" unpack "SimpleMonoReleaseTest"
)

Target "ReleaseMonoWithDotNet" (fun _ ->
    ensureDirectory "./_Reports"
    let unpack = FullName "_Packaging/Unpack/tools/netcoreapp2.0/AltCover"
    let x = FullName "./_Reports/ReleaseMonoWithDotNet.xml"
    let o = FullName "./_Mono/__Instrumented.ReleaseMonoWithDotNet"
    let i = FullName "./_Mono/Sample1"
    DotNetCli.RunCommand (fun info -> {info with WorkingDir = unpack })  
                          ("run --project altcover.core.fsproj -- -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")

    let sampleRoot = "_Mono/__Instrumented.ReleaseMonoWithDotNet"
    let result2 = ExecProcess (fun info -> info.FileName <- sampleRoot @@ "/Sample1.exe"
                                           info.WorkingDirectory <- sampleRoot
                                           info.Arguments <- "") (TimeSpan.FromMinutes 5.0)
    if result2 <> 0 then failwith "Instrumented .exe failed"

    Actions.ValidateSample1 "./_Reports/ReleaseMonoWithDotNet.xml" "ReleaseMonoWithDotNet"
)

Target "ReleaseDotNetWithDotNet" (fun _ -> // TODO
    ensureDirectory "./_Reports"
    let unpack = FullName "_Packaging/Unpack/tools/netcoreapp2.0/AltCover"
    let x = FullName "./_Reports/ReleaseDotNetWithDotNet.xml"
    let o = FullName "./_Binaries/Sample1/__Instrumented.ReleaseDotNetWithDotNet"
    let i = FullName "./_Binaries/Sample1/Debug+AnyCPU/netcoreapp2.0"
    DotNetCli.RunCommand (fun info -> {info with WorkingDir = unpack })  
                          ("run --project altcover.core.fsproj -- -x \"" + x + "\" -o \"" + o + "\" -i \"" + i + "\"")

    let sampleRoot = "_Binaries/Sample1/__Instrumented.ReleaseDotNetWithDotNet"
    DotNetCli.RunCommand id (sampleRoot @@ "Sample1.dll")

    Actions.ValidateSample1 "./_Reports/ReleaseDotNetWithDotNet.xml" "ReleaseDotNetWithDotNet"
)

Target "ReleaseDotNetWithFramework" (fun _ -> // TODO
    ensureDirectory "./_Reports"
    let unpack = FullName "_Packaging/Unpack/tools/net45"
    let simpleReport = (FullName "./_Reports") @@ ( "ReleaseDotNetWithFramework.xml")
    let sampleRoot = FullName "./_Binaries/Sample1/Debug+AnyCPU/netcoreapp2.0"
    let instrumented = sampleRoot @@ "__Instrumented.ReleaseDotNetWithFramework"
    let result = ExecProcess (fun info -> info.FileName <- unpack @@ "AltCover.exe"
                                          info.WorkingDirectory <- sampleRoot
                                          info.Arguments <- ("-t=System\. -t=Microsoft\. -x=" + simpleReport + " /o=" + instrumented)) (TimeSpan.FromMinutes 5.0)

    DotNetCli.RunCommand (fun info -> { info with WorkingDir = instrumented }) "Sample1.dll"

    Actions.ValidateSample1 "./_Reports/ReleaseDotNetWithFramework.xml" "ReleaseDotNetWithFramework"
)


// AOB

Target "BulkReport" (fun _ ->
    printfn "Overall coverage reporting"
    ensureDirectory "./_Reports/_BulkReport"

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
==> "FxCop"
==> "Analysis"

"Compilation"
==> "Gendarme"
==> "Analysis"

"Compilation"
==> "JustUnitTest"
==> "UnitTest"

"Compilation"
==> "UnitTestDotNet"
==> "UnitTest"

"Compilation"
==> "UnitTestWithOpenCover"
==> "UnitTest"

"Compilation"
==> "UnitTestWithAltCover"
==> "UnitTest"

"Compilation"
==> "FSharpTypes"
==> "OperationalTest"

"Compilation"
==> "FSharpTypesDotNet"
==> "OperationalTest"

"Compilation"
==> "BasicCSharp"
==> "OperationalTest"

"Compilation"
==> "BasicCSharpMono"
==> "OperationalTest"

"Compilation"
==> "CSharpMonoWithDotNet"
==> "OperationalTest"

// Unhandled Exception: System.IO.FileNotFoundException: Could not load file or assembly 
// 'FSharp.Core, Version=4.4.1.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a'. The system cannot find the file specified.
"Compilation"
// TODO
 ==> "CSharpDotNetWithDotNet"
==> "OperationalTest"

"Compilation"
==> "CSharpDotNetWithFramework"
==> "OperationalTest"

"Compilation"
==> "SelfTest"
==> "OperationalTest"

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

"Unpack"
==> "SimpleReleaseTest"
==> "Deployment"

"Unpack"
==> "SimpleMonoReleaseTest"
==> "Deployment"

"Unpack"
==> "ReleaseMonoWithDotNet"
==> "Deployment"

// Unhandled Exception: System.IO.FileNotFoundException: Could not load file or assembly 
// 'FSharp.Core, Version=4.4.1.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a'. The system cannot find the file specified.
"Unpack"
// 
==> "ReleaseDotNetWithDotNet"
==> "Deployment"

"Unpack"
==> "ReleaseDotNetWithFramework"
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

(*
// Debug chain
"SetVersion"
=?> ("BuildDebug", (not(File.Exists("./_Generated/AssemblyVersion.fs"))))
"SetVersion"
?=> "BuildDebug"
"Clean"
?=> "BuildDebug"
*)

RunTargetOrDefault "All"