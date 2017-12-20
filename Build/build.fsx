#r @"../packages/FAKE.4.64.3/tools/FakeLib.dll" // include Fake lib
#I @"../packages/FSharpLint.Fake.0.8.1/tools"
#r @"FSharpLint.Fake.dll"
#I @"../packages/ZipStorer.3.4.0/lib/net20"
#r @"ZipStorer.dll"
#r "System.Xml"
#r "System.Xml.Linq"

open System
open System.IO
open System.IO.Compression
open System.Reflection
open System.Xml
open System.Xml.Linq

open Fake
open Fake.AssemblyInfoFile
open Fake.Testing
open Fake.OpenCoverHelper
open Fake.ReportGeneratorHelper
open FSharpLint.Fake

let Copyright  = ref String.Empty
let Version = ref String.Empty

let OpenCoverFilter = "+[AltCove*]* -[*]Microsoft.* -[*]System.* +[*]N.*"
let AltCoverFilter= " -s=Mono -s=.Recorder -s=Sample -s=nunit -t=System. -t=Sample3.Class2 "

Target "Lint" (fun _ ->
    !! "**/*.fsproj"
        |> Seq.iter (FSharpLint (fun options -> { options with FailBuildIfAnyWarnings = true }) ))

// The clean target cleans the build and deploy folders
Target "Clean" (fun _ ->
    printfn "Cleaning"
    subDirectories (directoryInfo ".")
    |> Seq.filter (fun x -> x.Name.StartsWith "_" )
    |> Seq.map (fun x -> x.FullName)
    |> Seq.toList
    |> DeleteDirs
)

Target "SetVersion" (fun _ ->
    let now = DateTimeOffset.UtcNow
    let epoch = DateTimeOffset(2000, 1, 1, 0, 0, 0, TimeSpan(int64 0))
    let diff = now.Subtract(epoch)
    let fraction = diff.Subtract(TimeSpan.FromDays(float diff.Days))
    let revision= ((int fraction.TotalSeconds) / 3)
    let appveyor = environVar "APPVEYOR_BUILD_NUMBER"
    let majmin = "0.8"
    let version = if String.IsNullOrWhiteSpace appveyor then sprintf "%s.%d.%d" majmin diff.Days revision else sprintf "%s.%s.0" majmin appveyor
    Version := version
    let copy = sprintf "© 2010-%d by Steve Gilham <SteveGilham@users.noreply.github.com>" now.Year
    Copyright := "Copyright " + copy

    let stream2 = new System.IO.FileStream("./Build/SelfTest.snk", System.IO.FileMode.Open, System.IO.FileAccess.Read)
    let pair2 = StrongNameKeyPair(stream2)
    let key2 = BitConverter.ToString pair2.PublicKey

    let stream = new System.IO.FileStream("./Build/Infrastructure.snk", System.IO.FileMode.Open, System.IO.FileAccess.Read)
    let pair = StrongNameKeyPair(stream)
    let key = BitConverter.ToString pair.PublicKey

    CreateFSharpAssemblyInfo "./_Generated/AssemblyVersion.fs"
        [Attribute.Version (majmin + ".0.0")
         Attribute.FileVersion version
         Attribute.Company "Steve Gilham"
         Attribute.Product "AltCover"
         Attribute.Trademark ""
         Attribute.Copyright copy
         ]

    let template ="namespace AltCover
open System.Reflection
open System.Runtime.CompilerServices
#if DEBUG
[<assembly: AssemblyConfiguration(\"Debug {0}\")>]
#else
[<assembly: AssemblyConfiguration(\"Release {0}\")>]
#endif
[<assembly: InternalsVisibleTo(\"AltCover.Tests, PublicKey={1}\")>]
[<assembly: InternalsVisibleTo(\"AltCover.Tests, PublicKey={2}\")>]
()
"
    let file = String.Format(System.Globalization.CultureInfo.InvariantCulture,
                template, version, key.Replace("-", String.Empty), key2.Replace("-", String.Empty))
    let path = @"_Generated\VisibleToTest.fs"
    // Update the file only if it would change
    let old = if File.Exists(path) then File.ReadAllText(path) else String.Empty
    if not (old.Equals(file)) then File.WriteAllText(path, file)
)

Target "BuildRelease" (fun _ ->
   !! "*.sln"
     |> MSBuildRelease "" ""
     |> Log "AppBuild-Output: "

   ILMerge (fun p -> { p with DebugInfo = true
                              TargetKind = TargetKind.Exe
                              KeyFile = "./Build/Infrastructure.snk"
                              Version = !Version
                              Internalize = InternalizeTypes.Internalize
                              Libraries = !! "./_Binaries/AltCover/Release+AnyCPU/Mono.C*.dll"
                              AttributeFile = "./_Binaries/AltCover/Release+AnyCPU/AltCover.exe"})
                              "./_Binaries/AltCover/AltCover.exe"
                              "./_Binaries/AltCover/Release+AnyCPU/AltCover.exe"
)

Target "BuildDebug" (fun _ ->
   !! "*.sln"
     |> MSBuildDebug "" ""
     |> Log "AppBuild-Output: "
)

Target "TestCover" (fun _ ->
    ensureDirectory "./_Reports/_UnitTest"
    OpenCover (fun p -> { p with ExePath = findToolInSubPath "OpenCover.Console.exe" "."
                                 WorkingDir = "."
                                 TestRunnerExePath = findToolInSubPath "nunit3-console.exe" "."
                                 Filter = "+[AltCover]* -[*]Microsoft.* -[*]System.* -[Sample*]*"
                                 MergeByHash = true
                                 Register = RegisterType.RegisterUser
                                 Output = "_Reports/OpenCoverReport.xml" })
        "_Binaries/AltCover.Tests/Debug+AnyCPU/AltCover.Tests.dll --result=./_Reports/NUnit3ReportOpenCovered.xml"
    ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                       ReportTypes = [ ReportGeneratorReportType.Html; ReportGeneratorReportType.Badges ]
                                       TargetDir = "_Reports/_UnitTest"})
        ["./_Reports/OpenCoverReport.xml"]

    if not <| String.IsNullOrWhiteSpace (environVar "APPVEYOR_BUILD_NUMBER") then
            ExecProcess (fun info -> info.FileName <- findToolInSubPath "coveralls.net.exe" "."
                                     info.WorkingDirectory <- "_Reports"
                                     info.Arguments <- "--opencover OpenCoverReport.xml") (TimeSpan.FromMinutes 5.0)
            |> ignore
)

Target "FSharpTypes" ( fun _ ->
    ensureDirectory "./_Reports"
    let simpleReport = (FullName "./_Reports") @@ ( "FSharpTypes.xml")
    let binRoot = FullName "_Binaries/AltCover/Debug+AnyCPU"
    let sampleRoot = FullName "_Binaries/Sample2/Debug+AnyCPU"
    let instrumented = "__Instrumented"
    let result = ExecProcess (fun info -> info.FileName <- binRoot @@ "AltCover.exe"
                                          info.WorkingDirectory <- sampleRoot
                                          info.Arguments <- ("-t=System. -tMicrosoft. -x=" + simpleReport + " /o=./" + instrumented)) (TimeSpan.FromMinutes 5.0)
    use coverageFile = new FileStream(simpleReport, FileMode.Open, FileAccess.Read, FileShare.None, 4096, FileOptions.SequentialScan)
    // Edit xml report to store new hits
    let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))
    let recorded = coverageDocument.Descendants(XName.Get("method"))
                   |> Seq.map (fun x -> x.Attribute(XName.Get("name")).Value)
                   |> Seq.sort
                   |> Seq.toList
    let expected = "[\"Invoke\"; \"as_bar\"; \"bytes\"; \"get_MyBar\"; \"makeThing\"; \"returnBar\"; \"returnFoo\";\n \"testMakeThing\"; \"testMakeUnion\"]"
    if recorded.Length <> 9 then failwith (sprintf "Bad method list length %A" recorded)
    if (sprintf "%A" recorded) <> expected then failwith (sprintf "Bad method list %A" recorded)
    )

Target "Test" (fun _ ->
    ensureDirectory "./_Reports"
    !! (@"_Binaries\*Tests\Debug+AnyCPU\*.Tests.dll")
    |> NUnit3 (fun p -> { p with ToolPath = findToolInSubPath "nunit3-console.exe" "."
                                 WorkingDir = "."
                                 ResultSpecs = ["./_Reports/NUnit3Report.xml"] })
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
        [altReport]

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

    printfn "Unit-test instrumented code under OpenCover"
    ensureDirectory "./_Reports/_UnitTestInstrumented"
    OpenCover (fun p -> { p with ExePath = findToolInSubPath "OpenCover.Console.exe" "."
                                 WorkingDir = "."
                                 TestRunnerExePath = findToolInSubPath "nunit3-console.exe" "."
                                 Filter = OpenCoverFilter
                                 MergeByHash = true
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

Target "BuildMonoSamples" (fun _ ->
    ensureDirectory "./_Mono/Sample1"
    let mcs = findToolInSubPath "mcs.exe" ".."
    let result = ExecProcess (fun info -> info.FileName <- mcs
                                          info.WorkingDirectory <- "."
                                          info.Arguments <- (@"-debug -out:./_Mono/Sample1/Sample1.exe  .\Sample1\Program.cs")) (TimeSpan.FromMinutes 5.0)
    if result <> 0 then failwith "Mono compilation failed"

    // Fix up symbol file to have the MVId emitted by the System.Reflection.Emit code
    let assembly = System.Reflection.Assembly.ReflectionOnlyLoadFrom (FullName "./_Mono/Sample1/Sample1.exe")
    let mvid = assembly.ManifestModule.ModuleVersionId.ToByteArray();
    let symbols = System.IO.File.ReadAllBytes("./_Mono/Sample1/Sample1.exe.mdb")
    mvid|> Array.iteri (fun i x -> symbols.[i+16] <- x)
    System.IO.File.WriteAllBytes("./_Mono/Sample1/Sample1.exe.mdb", symbols)
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

    if ["18"; "19"; "20"] <> zero' then failwith "wrong unvisited"

    let ones' = ones |> Seq.distinct |> Seq.toList
    if ["11"; "12"; "13"; "14"; "15"; "16"; "21"] <> ones' then failwith "wrong number of visited"

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

Target "FxCop" (fun _ ->
    ensureDirectory "./_Reports"
    let fxCop = combinePaths (environVar "VS150COMNTOOLS") "../../Team Tools/Static Analysis Tools/FxCop/FxCopCmd.exe"
    ["_Binaries/AltCover/Debug+AnyCPU/AltCover.exe"; "_Binaries/AltCover.Shadow/Debug+AnyCPU/AltCover.Shadow.dll"]
    |> Seq.filter (fun n -> not (n.EndsWith(".Tests.dll")))
    |> Seq.filter (fun n -> not (n.EndsWith(".pdb")))
    |> FxCop (fun p -> { p with ToolPath = fxCop
                                WorkingDir = "."
                                UseGACSwitch = true
                                Verbose = false
                                ReportFileName = "_Reports/FxCopReport.xml"
                                TypeList = ["AltCover.Augment"
                                            "AltCover.Filter"
                                            "AltCover.Instrument"
                                            "AltCover.KeyStore"
                                            "AltCover.Main"
                                            "AltCover.Naming"
                                            "AltCover.ProgramDatabase"
                                            "AltCover.Recorder.Instance"
                                            "AltCover.Report"
                                            "AltCover.Visitor"
                                            ]
                                Rules = ["-Microsoft.Design#CA1004"
                                         "-Microsoft.Design#CA1006"
                                         "-Microsoft.Design#CA1011" // maybe sometimes
                                         "-Microsoft.Design#CA1062" // null checks,  In F#!
                                         "-Microsoft.Globalization#CA1303" // TODO
                                         "-Microsoft.Maintainability#CA1506"
                                         "-Microsoft.Naming#CA1704"
                                         "-Microsoft.Naming#CA1707"
                                         "-Microsoft.Naming#CA1709"
                                         "-Microsoft.Naming#CA1715"
                                          ]
                                IgnoreGeneratedCode  = true})
    if fileExists "_Reports/FxCopReport.xml" then failwith "FxCop Errors were detected"
)

Target "Gendarme" (fun _ ->
    let r = ExecProcess (fun info -> info.FileName <- (findToolInSubPath "gendarme.exe" ".\packages")
                                     info.WorkingDirectory <- "."
                                     info.Arguments <- "--severity all --confidence all --config ./Build/rules.xml --console --html ./_Reports/gendarme.html _Binaries/AltCover/Debug+AnyCPU/AltCover.exe  _Binaries/AltCover.Shadow/Debug+AnyCPU/AltCover.Shadow.dll") (TimeSpan.FromMinutes 5.0)
    if r <> 0 then failwith  "Gendarme Errors were detected"
)

Target "Package"  (fun _ ->
    ensureDirectory "./_Binaries/Packaging"
    ensureDirectory "./_Packaging"
    let AltCover = FullName "_Binaries/AltCover/AltCover.exe"
    let recorder = FullName "_Binaries/AltCover/Release+AnyCPU/AltCover.Recorder.dll"

    NuGet (fun p ->
    {p with
        Authors = ["Steve Gilham"]
        Project = "altcover"
        Description = "A pre-instrumented code coverage tool for .net"
        OutputPath = "./_Packaging"
        WorkingDir = "./_Binaries/Packaging"
        Files = [
                        (AltCover, Some "tools", None)
                        (recorder, Some "tools", None)
                ]
        Version = !Version
        Copyright = !Copyright
        Publish = false
        ReleaseNotes = "Functional release, usable but minimalist implementation.   Still working the wrinkles out of the deployment chain."})
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

Target "All" ignore

"Clean"
==> "SetVersion"
==> "BuildRelease"
==> "Package"
==> "SimpleReleaseTest"
==> "SimpleMonoReleaseTest"

"BuildMonoSamples"
==> "SimpleMonoReleaseTest"

"BuildMonoSamples"
==> "Test"

"SetVersion"
=?> ("BuildDebug", (not(File.Exists("./_Generated/AssemblyVersion.fs"))))

"SetVersion"
?=> "BuildDebug"

"Clean"
?=> "BuildDebug"

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
==> "BuildMonoSamples"
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

"Lint"
?=> "FxCop"

"FxCop"
?=> "Gendarme"

"Test"
==> "TestCover"

RunTargetOrDefault "All"