
open System
open System.IO
open System.IO.Compression
open System.Xml
open System.Xml.Linq

open Actions

open Fake
open Fake.AssemblyInfoFile
open Fake.Testing
open Fake.OpenCoverHelper
open Fake.ReportGeneratorHelper
open FSharp.Markdown
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
    let (v, majmin, y) = Actions.LocalVersion (environVar "APPVEYOR_BUILD_VERSION") (Actions.GetVersionFromYaml ())
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
     |> MSBuildDebug "" ""
     |> Log "AppBuild-Output: "

   DotNetCli.Build
        (fun p -> 
            { p with 
                Configuration = "Debug"
                Project =  "./altcover.core.sln"})
)

Target "BuildMonoSamples" (fun _ ->
    let mcs = findToolInSubPath "mcs.exe" ".."

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
    ensureDirectory "./_Reports"

    let r = ExecProcess (fun info -> info.FileName <- (findToolInSubPath "gendarme.exe" ".\packages")
                                     info.WorkingDirectory <- "."
                                     info.Arguments <- "--severity all --confidence all --config ./Build/rules.xml --console --html ./_Reports/gendarme.html _Binaries/AltCover/Debug+AnyCPU/AltCover.exe  _Binaries/AltCover.Shadow/Debug+AnyCPU/AltCover.Shadow.dll") (TimeSpan.FromMinutes 5.0)
    if r <> 0 then failwith  "Gendarme Errors were detected"
)

Target "FxCop" (fun _ -> // Needs debug because release is compiled --standalone which contaminates everything
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
  !! (@"_Reports\*\Summary.xml") 
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
    !! (@"_Binaries\*Tests\Debug+AnyCPU\*.Test*.dll") // Need to figure out why it doesn't build in Release
    |> NUnit3 (fun p -> { p with ToolPath = findToolInSubPath "nunit3-console.exe" "."
                                 WorkingDir = "."
                                 ResultSpecs = ["./_Reports/JustUnitTestReport.xml"] })
)

Target "UnitTestDotNet" (fun _ ->
    ensureDirectory "./_Reports"
    !! (@".\*Tests\*.tests.core.fsproj")
    |> Seq.iter (fun f -> printfn "Testing %s" f
                          DotNetCli.Test
                             (fun p -> 
                                  { p with 
                                      Configuration = "Debug"
                                      Project =  f}))
)

Target "UnitTestWithOpenCover" (fun _ ->
    ensureDirectory "./_Reports/_UnitTestWithOpenCover"
    let testFiles = !! (@"_Binaries\*Tests\Debug+AnyCPU\*.Test*.dll") 
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
    ensureDirectory "./_Reports/_UnitTestWithAltCover"
    let keyfile = FullName "Build\SelfTest.snk"
    let reports = FullName "./_Reports"

    let altReport = reports @@ "UnitTestWithAltCover.xml"
    printfn "Instrumented the code"
    let result = ExecProcess (fun info -> info.FileName <- "_Binaries/AltCover/Debug+AnyCPU/AltCover.exe"
                                          info.WorkingDirectory <- "_Binaries/AltCover.Tests/Debug+AnyCPU"
                                          info.Arguments <- ("/sn=" + keyfile + AltCoverFilter + @"/o=.\__UnitTestWithAltCover -x=" + altReport)) (TimeSpan.FromMinutes 5.0)
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
    let result = ExecProcess (fun info -> info.FileName <- "_Binaries/AltCover/Debug+AnyCPU/AltCover.exe"
                                          info.WorkingDirectory <- "_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU"
                                          info.Arguments <- ("/sn=" + keyfile + AltCoverFilter + @"/o=.\__ShadowTestWithAltCover -x=" + shadowReport)) (TimeSpan.FromMinutes 5.0)

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
    ensureDirectory "./_Reports"
    let simpleReport = (FullName "./_Reports") @@ ( "AltCoverFSharpTypes.xml")
    let binRoot = FullName "_Binaries/AltCover/Release+AnyCPU"
    let sampleRoot = FullName "_Binaries/Sample2/Debug+AnyCPU"
    let instrumented = "__Framework"
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
    let instrumented = "__DotNet"
    DotNetCli.RunCommand (fun p -> {p with WorkingDir = sampleRoot}) 
                         ("run --project " + project + " -- -t \"System\\.\" -t \"Microsoft\\.\" -x \"" + simpleReport + "\" /o \"./" + instrumented + "\"")

    Actions.ValidateFSharpTypes simpleReport
)



















Target "TestDotNetOnMono" (fun _ ->
    ensureDirectory "./_Reports"
    DotNetCli.RunCommand id "run --project ./AltCover/altcover.core.fsproj -- -t \"System.\" -x \"./_Reports/TestDotNetOnMono.xml\" -o \"./_Mono/_DotNetInstrumented\" -i \"./_Mono/Sample1\""

    let sampleRoot = "./_Mono/_DotNetInstrumented"
    let result2 = ExecProcess (fun info -> info.FileName <- sampleRoot @@ "/Sample1.exe"
                                           info.WorkingDirectory <- sampleRoot
                                           info.Arguments <- "") (TimeSpan.FromMinutes 5.0)
    if result2 <> 0 then failwith "Instrumented .exe failed"

    let reportSigil = "dotnet"
    let simpleReport = "./_Reports/TestDotNetOnMono.xml"
    ensureDirectory ("./_Reports/_SimpleReport" + reportSigil)
    ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                       TargetDir = "_Reports/_SimpleReport" + reportSigil})
        [simpleReport]

    // get recorded details from here
    use coverageFile = new FileStream(simpleReport, FileMode.Open, FileAccess.Read, FileShare.None, 4096, FileOptions.SequentialScan)
    let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))
    let recorded = coverageDocument.Descendants(XName.Get("seqpnt"))
                   |> Seq.toList

    let zero = recorded
               |> Seq.filter (fun x -> x.Attribute(XName.Get("visitcount")).Value = "0")
               |> Seq.map (fun x -> x.Attribute(XName.Get("line")).Value)
               |> Seq.sort
               |> Seq.toList
    let ones = recorded
               |> Seq.filter (fun x -> x.Attribute(XName.Get("visitcount")).Value = "1")
               |> Seq.map (fun x -> x.Attribute(XName.Get("line")).Value)
               |> Seq.sort
               |> Seq.toList

    if (List.length ones) + (List.length zero) <> (List.length recorded) then failwith "unexpected visits"
    let zero' = zero |> Seq.distinct |> Seq.toList

    if ["18"; "19"; "20"] <> zero' then failwith ("wrong unvisited : " + (sprintf "%A" zero'))

    let ones' = ones |> Seq.distinct |> Seq.toList
    if ["11"; "12"; "13"; "14"; "15"; "16"; "21"] <> ones' then failwith ("wrong number of visited : " + (sprintf "%A" ones'))
)

Target "TestDotNetOnDotNet" (fun _ ->
    ensureDirectory "./_Reports"
    DotNetCli.RunCommand id "_Binaries/AltCover/Debug+AnyCPU/netcoreapp2.0/AltCover.dll -t \"System.\" -x \"./_Reports/TestDotNetOnDotNet.xml\" -o \"./_Binaries/Sample1/_DotNetInstrumented\" -i \"./_Binaries/Sample1/Debug+AnyCPU/netcoreapp2.0\""

    let sampleRoot = "./_Binaries/Sample1/_DotNetInstrumented"
    DotNetCli.RunCommand id (sampleRoot @@ "Sample1.dll")

    let reportSigil = "dotnet2"
    let simpleReport = "./_Reports/TestDotNetOnDotNet.xml"
    ensureDirectory ("./_Reports/_SimpleReport" + reportSigil)
    ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                       TargetDir = "_Reports/_SimpleReport" + reportSigil})
        [simpleReport]

    // get recorded details from here
    use coverageFile = new FileStream(simpleReport, FileMode.Open, FileAccess.Read, FileShare.None, 4096, FileOptions.SequentialScan)
    let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))
    let recorded = coverageDocument.Descendants(XName.Get("seqpnt"))
                   |> Seq.toList

    let zero = recorded
               |> Seq.filter (fun x -> x.Attribute(XName.Get("visitcount")).Value = "0")
               |> Seq.map (fun x -> x.Attribute(XName.Get("line")).Value)
               |> Seq.sort
               |> Seq.toList
    let ones = recorded
               |> Seq.filter (fun x -> x.Attribute(XName.Get("visitcount")).Value = "1")
               |> Seq.map (fun x -> x.Attribute(XName.Get("line")).Value)
               |> Seq.sort
               |> Seq.toList

    if (List.length ones) + (List.length zero) <> (List.length recorded) then failwith "unexpected visits"
    let zero' = zero |> Seq.distinct |> Seq.toList

    if ["18"; "19"; "20"] <> zero' then failwith ("wrong unvisited : " + (sprintf "%A" zero'))

    let ones' = ones |> Seq.distinct |> Seq.toList
    if ["11"; "12"; "13"; "14"; "15"; "16"; "21"] <> ones' then failwith ("wrong number of visited : " + (sprintf "%A" ones'))
)

Target "SelfTest" (fun _ ->
    let targetDir = "_Binaries/AltCover.Tests/Debug+AnyCPU"
    let reports = FullName "./_Reports"
    let altReport = reports @@ "AltCoverage.xml"
    let keyfile = FullName "Build\SelfTest.snk"

    ensureDirectory "./_Reports/_Instrumented"
    ensureDirectory (targetDir @@ "__Instrumented")

    printfn "Self-instrument under OpenCover"
    OpenCover (fun p -> { p with ExePath = findToolInSubPath "OpenCover.Console.exe" "."
                                 WorkingDir = targetDir
                                 TestRunnerExePath = findToolInSubPath "AltCover.exe" targetDir
                                 Filter = OpenCoverFilter
                                 MergeByHash = true
                                 OptionalArguments = "-excludebyattribute:*ExcludeFromCodeCoverageAttribute;*ProgIdAttribute"
                                 Register = RegisterType.RegisterUser
                                 Output = reports @@ "OpenCoverInstrumentationReport.xml" })
        ("/sn=" + keyfile + AltCoverFilter + "-x=" + altReport)
    ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                       TargetDir = "_Reports/_Instrumented"})
        ["./_Reports/OpenCoverInstrumentationReport.xml"]

    // get recorder details from here
    use coverageFile = new FileStream("./_Reports/OpenCoverInstrumentationReport.xml", FileMode.Open, FileAccess.Read, FileShare.None, 4096, FileOptions.SequentialScan)
    // Edit xml report to store new hits
    let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))
    let recorder = coverageDocument.Descendants(XName.Get("Module"))
                   |> Seq.filter (fun el -> el.Descendants(XName.Get("ModulePath")).Nodes()
                                            |> Seq.exists (fun n -> n.ToString().EndsWith("AltCover.Recorder.dll")))
                   |> Seq.head

    printfn "Re-instrument everything"
    ensureDirectory "./_Reports/_AltReport"
    let altReport2 = reports @@ "AltSelfTestCoverage.xml"
    let result = ExecProcess (fun info -> info.FileName <- "_Binaries/AltCover.Tests/Debug+AnyCPU/__Instrumented/AltCover.exe"
                                          info.WorkingDirectory <- "_Binaries/AltCover.Tests/Debug+AnyCPU"
                                          info.Arguments <- ("/sn=" + keyfile + AltCoverFilter + @"/o=.\__ReInstrument -x=" + altReport2)) (TimeSpan.FromMinutes 5.0)
    ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                       TargetDir = "_Reports/_AltReport"})
        [altReport2]

    if result <> 0 then failwithf "Re-instrument returned with a non-zero exit code"

    printfn "Unit test instrumented code"
    ensureDirectory "./_Reports"
    [ !! "_Binaries/AltCover.Tests/Debug+AnyCPU/__ReInstrument/*.Tests.dll"
      !! "_Binaries/AltCover.Tests/Debug+AnyCPU/__ReInstrument/*ple2.dll"]
    |> Seq.concat |> Seq.distinct
    |> NUnit3 (fun p -> { p with ToolPath = findToolInSubPath "nunit3-console.exe" "."
                                 WorkingDir = "."
                                 ResultSpecs = ["./_Reports/NUnit3ReportInstrumented.xml"] })
    ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                       TargetDir = "_Reports/_SelfTestReport"})
        [altReport2]

    printfn "Instrument and run the shadow tests"
    let shadowDir = "_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU"
    ensureDirectory "./_Reports/_TotalSelfTestReport"
    let altReport3 = reports @@ "ShadowSelfTestCoverage.xml"
    let result = ExecProcess (fun info -> info.FileName <- "_Binaries/AltCover/Debug+AnyCPU/AltCover.exe"
                                          info.WorkingDirectory <- "_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU"
                                          info.Arguments <- ("/sn=" + keyfile + AltCoverFilter + @"/o=.\__Instrument -x=" + altReport3)) (TimeSpan.FromMinutes 5.0)
    !! (@"_Binaries\AltCover.Shadow.Tests\Debug+AnyCPU\__Instrument\*.Test*.dll")
    |> NUnit3 (fun p -> { p with ToolPath = findToolInSubPath "nunit3-console.exe" "."
                                 WorkingDir = "."
                                 ResultSpecs = ["./_Reports/NUnit3ReportShadow.xml"] })
    ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                       TargetDir = "_Reports/_TotalSelfTestReport"})
        [altReport2; altReport3]
    
    printfn "Unit-test instrumented code under OpenCover"
    ensureDirectory "./_Reports/_UnitTestInstrumented"
    OpenCover (fun p -> { p with ExePath = findToolInSubPath "OpenCover.Console.exe" "."
                                 WorkingDir = "."
                                 TestRunnerExePath = findToolInSubPath "nunit3-console.exe" "."
                                 Filter = OpenCoverFilter
                                 MergeByHash = true
                                 OptionalArguments = "-excludebyattribute:*ExcludeFromCodeCoverageAttribute;*ProgIdAttribute"
                                 Register = RegisterType.RegisterUser
                                 Output = "_Reports/OpenCoverReportAltCovered.xml" })
        "_Binaries/AltCover.Tests/Debug+AnyCPU/__Instrumented/AltCover.Tests.dll _Binaries/AltCover.Tests/Debug+AnyCPU/__Instrumented/Sample2.dll --result=./_Reports/NUnit3ReportAltCovered.xml"

    use coverageFile2 = new FileStream("./_Reports/OpenCoverReportAltCovered.xml", FileMode.Open, FileAccess.ReadWrite, FileShare.None, 4096, FileOptions.SequentialScan)
    let coverageDocument2 = XDocument.Load(XmlReader.Create(coverageFile2))
    let recorder2 = coverageDocument2.Descendants(XName.Get("Module"))
                    |> Seq.filter (fun el -> el.Descendants(XName.Get("ModulePath")).Nodes()
                                             |> Seq.exists (fun n -> n.ToString().EndsWith("AltCover.Recorder.g.dll")))
                    |> Seq.head // at most 1
    recorder2.SetAttributeValue(XName.Get("hash"), recorder.Attribute(XName.Get("hash")).Value)

    ["ModulePath"; "ModuleTime"; "ModuleName"]
    |> Seq.iter (fun name -> let from = recorder.Descendants(XName.Get(name)).Nodes() |> Seq.head :?> XText
                             let to' = recorder2.Descendants(XName.Get(name)).Nodes() |> Seq.head :?> XText
                             to'.Value <- from.Value)

    // Save modified xml to a file
    coverageFile2.Seek(0L, SeekOrigin.Begin) |> ignore
    coverageFile2.SetLength(int64 0) // truncate it all because the rewrite ends up one line shorter for some reason and leaves a dangling tag
    use writer = System.Xml.XmlWriter.Create(coverageFile2)
    coverageDocument2.WriteTo(writer)
    writer.Flush()
    writer.Close()
    coverageFile2.Close()

    ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                       TargetDir = "_Reports/_UnitTestInstrumented"})
        ["./_Reports/OpenCoverReportAltCovered.xml"]
)


let SimpleInstrumentingRun (samplePath:string) (binaryPath:string) (reportSigil:string) =
    printfn "Instrument a simple executable"
    ensureDirectory "./_Reports"
    let simpleReport = (FullName "./_Reports") @@ ( "SimpleCoverage" + reportSigil + ".xml")
    let binRoot = FullName binaryPath
    let sampleRoot = FullName samplePath
    let instrumented = "__Instrumented" + reportSigil
    let result = ExecProcess (fun info -> info.FileName <- binRoot @@ "AltCover.exe"
                                          info.WorkingDirectory <- sampleRoot
                                          info.Arguments <- ("-t=System. -x=" + simpleReport + " /o=./" + instrumented)) (TimeSpan.FromMinutes 5.0)
    if result <> 0 then failwith "Simple instrumentation failed"
    let result2 = ExecProcess (fun info -> info.FileName <- sampleRoot @@ (instrumented + "/Sample1.exe")
                                           info.WorkingDirectory <- (sampleRoot @@ instrumented)
                                           info.Arguments <- "") (TimeSpan.FromMinutes 5.0)
    if result2 <> 0 then failwith "Instrumented .exe failed"

    ensureDirectory ("./_Reports/_SimpleReport" + reportSigil)
    ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                       TargetDir = "_Reports/_SimpleReport" + reportSigil})
        [simpleReport]

    // get recorded details from here
    use coverageFile = new FileStream(simpleReport, FileMode.Open, FileAccess.Read, FileShare.None, 4096, FileOptions.SequentialScan)
    let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))
    let recorded = coverageDocument.Descendants(XName.Get("seqpnt"))
                   |> Seq.toList

    let zero = recorded
               |> Seq.filter (fun x -> x.Attribute(XName.Get("visitcount")).Value = "0")
               |> Seq.map (fun x -> x.Attribute(XName.Get("line")).Value)
               |> Seq.sort
               |> Seq.toList
    let ones = recorded
               |> Seq.filter (fun x -> x.Attribute(XName.Get("visitcount")).Value = "1")
               |> Seq.map (fun x -> x.Attribute(XName.Get("line")).Value)
               |> Seq.sort
               |> Seq.toList

    if (List.length ones) + (List.length zero) <> (List.length recorded) then failwith "unexpected visits"
    let zero' = zero |> Seq.distinct |> Seq.toList

    if ["18"; "19"; "20"] <> zero' then failwith ("wrong unvisited : " + (sprintf "%A" zero'))

    let ones' = ones |> Seq.distinct |> Seq.toList
    if ["11"; "12"; "13"; "14"; "15"; "16"; "21"] <> ones' then failwith ("wrong number of visited : " + (sprintf "%A" ones'))

Target "SimpleInstrumentation" (fun _ ->
   SimpleInstrumentingRun "_Binaries/Sample1/Debug+AnyCPU" "_Binaries/AltCover/Debug+AnyCPU" String.Empty
)

Target "SimpleMonoTest" (fun _ ->
    SimpleInstrumentingRun "_Mono/Sample1" "_Binaries/AltCover/Debug+AnyCPU" ".M"
)

Target "BulkReport" (fun _ ->
    printfn "Overall coverage reporting"
    ensureDirectory "./_Reports/_BulkReport"
    !! "./_Reports/*cover*.xml"
    |> Seq.filter (fun f -> not <| f.Contains("NUnit"))
    |> Seq.toList
    |> ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                          ReportTypes = [ ReportGeneratorReportType.Html; ReportGeneratorReportType.Badges ]
                                          TargetDir = "_Reports/_BulkReport"})
)


//   ILMerge (fun p -> { p with DebugInfo = true
//                              TargetKind = TargetKind.Exe
//                              KeyFile = "./Build/Infrastructure.snk"
//                              Version = (String.Join(".", (!Version).Split('.') |> Seq.take 2) + ".0.0")
//                              Internalize = InternalizeTypes.Internalize
//                              Libraries = !! "./_Binaries/AltCover/Release+AnyCPU/Mono.C*.dll"
//                              AttributeFile = "./_Binaries/AltCover/Release+AnyCPU/AltCover.exe"})
//                              "./_Binaries/AltCover/AltCover.exe"
//                              "./_Binaries/AltCover/Release+AnyCPU/AltCover.exe"

//ensureDirectory "./_Binaries/netcoreapp2.0"
//    !! (@"_Binaries\AltCover\Release+AnyCPU\netcoreapp2.0\*")
//|> (Copy "./_Binaries/netcoreapp2.0")

Target "Package"  (fun _ ->
    ensureDirectory "./_Binaries/Packaging"
    ensureDirectory "./_Packaging"

    let packingCopyright = (!Copyright).Replace("©", "&#xa9;").Replace("<","&lt;").Replace(">", "&gt;")
    let AltCover = FullName "_Binaries/AltCover/AltCover.exe"
    let recorder = FullName "_Binaries/AltCover/Release+AnyCPU/AltCover.Recorder.dll"
    let resources = filesInDirMatchingRecursive "AltCover.resources.dll" (directoryInfo (FullName "_Binaries/AltCover/Release+AnyCPU")) 
    let readme = FullName "README.md"
    let document = File.ReadAllText readme
    let docHtml = """<?xml version="1.0"  encoding="utf-8"?>
<!DOCTYPE html>
<html lang="en">
<head>
<title>AltCover README</title>
</head>
<body>
"""               + (Markdown.TransformHtml document) + """
<footer><p style="text-align: center">""" + packingCopyright + """</p>
</footer>
</body>
</html>
"""
    let xmlform = XDocument.Parse docHtml
    let body = xmlform.Descendants(XName.Get "body")
    let eliminate = [ "Continuous Integration"; "Building"; "Thanks to" ]
    let keep = ref true

    let kill = body.Elements() 
               |> Seq.map (fun x -> match x.Name.LocalName with
                                    | "h2" -> keep := (List.tryFind (fun e -> e = String.Concat(x.Nodes())) eliminate) |> Option.isNone
                                    | "footer" -> keep := true
                                    | _ -> ()
                                    if !keep then None else Some x)
               |> Seq.toList
    kill |> 
    Seq.iter (fun q -> match q with 
                       | Some x -> x.Remove()
                       | _ -> ())

    let packable = FullName "./_Binaries/README.html"
    xmlform.Save packable

    let applicationFiles = [
                            (AltCover, Some "tools", None)
                            (recorder, Some "tools", None)
                            (packable, Some "", None)
                           ]
    let resourceFiles = resources
                        |> Seq.map (fun x -> x.FullName)
                        |> Seq.map (fun x -> (x, Some ("tools/" + Path.GetFileName(Path.GetDirectoryName(x))), None))
                        |> Seq.toList

    NuGet (fun p ->
    {p with
        Authors = ["Steve Gilham"]
        Project = "altcover"
        Description = "A pre-instrumented code coverage tool for .net and Mono"
        OutputPath = "./_Packaging"
        WorkingDir = "./_Binaries/Packaging"
        Files = List.concat [applicationFiles; resourceFiles]
        Version = !Version
        Copyright = (!Copyright).Replace("©", "(c)")
        Publish = false
        ReleaseNotes = FullName "ReleaseNotes.md"
                       |> File.ReadAllText 
        })
        "./Build/AltCover.nuspec"
)

Target "SimpleReleaseTest" (fun _ ->
   let nugget = !! "./_Packaging/*.nupkg" |> Seq.last
   // should work but doesn't ZipFile.ExtractToDirectory(nugget, "_Packaging/Unpack")
   // so do this
   let zip = ZipStorer.Open(nugget, FileAccess.Read)
   let unpack = FullName "_Packaging/Unpack"
   zip.ReadCentralDir()
    |> Seq.filter (fun entry -> let name = Path.GetFileName(entry.FilenameInZip)
                                name.StartsWith("AltCover.", StringComparison.OrdinalIgnoreCase) &&
                                    (Path.GetExtension(name).Length = 4))
    |> Seq.iter (fun entry -> zip.ExtractFile(entry, unpack @@ Path.GetFileName(entry.FilenameInZip)) |> ignore)

   SimpleInstrumentingRun "_Binaries/Sample1/Debug+AnyCPU" unpack ".R"
)

Target "SimpleMonoReleaseTest" (fun _ ->

    let unpack = FullName "_Packaging/Unpack"
    SimpleInstrumentingRun "_Mono/Sample1" unpack ".MR"
)

Target "dotnet" ignore

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

(*
// Debug chain
"SetVersion"
=?> ("BuildDebug", (not(File.Exists("./_Generated/AssemblyVersion.fs"))))
"SetVersion"
?=> "BuildDebug"
"Clean"
?=> "BuildDebug"

"Clean"
==> "SetVersion"
==> "BuildRelease"

"BuildRelease"
==> "BuildMonoSamples"
==> "All"

"BuildRelease"
==> "Lint"
==> "All"

==> "SimpleReleaseTest"
==> "SimpleMonoReleaseTest"

"Clean"
==> "SetVersion"
==> "dotnet"

"BuildMonoSamples"
==> "SimpleMonoReleaseTest"

"BuildMonoSamples"
==> "Test"

"BuildMonoSamples"
==> "TestDotNet"


"BuildDebug"
==> "Lint"
"BuildDebug"
==> "Test"
"BuildDebug"
==> "TestCover"
"BuildDebug"
==> "FxCop"
"BuildDebug"
==> "SelfTest"
"BuildDebug"
==> "SimpleInstrumentation"

"BuildDebug"
==> "FSharpTypes"
"BuildDebug"
==> "Gendarme"

"TestCover"
==> "BulkReport"

"SelfTest"
==> "BulkReport"

"SimpleInstrumentation"
==> "BulkReport"

"BulkReport"
==> "All"

"SimpleMonoReleaseTest"
==> "All"

"SimpleMonoTest"
==> "All"

"FSharpTypes"
==> "All"

"Lint"
==> "All"

"FxCop"
==> "All"

"Gendarme"
==> "All"

"dotnet"
==> "All"

"Lint"
?=> "FxCop"

"FxCop"
?=> "Gendarme"

"Test"
==> "TestCover"

"TestDotNet"
==> "TestDotNetOnMono"
==> "TestDotNetOnDotNet"

//"TestDotNetOnDotNet"
//==> "dotnet"

"TestDotNetOnMono"
==> "dotnet"
*)

RunTargetOrDefault "All"