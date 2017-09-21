#r @"FakeLib.dll" // include Fake lib
#I @"../packages/FSharpLint.Fake.0.8.0/tools"
#r @"../packages/FSharpLint.Fake.0.8.0/tools/FSharpLint.Fake.dll"
#I @"../packages/ZipStorer.3.4.0/lib/net20"
#r @"../packages/ZipStorer.3.4.0/lib/net20/ZipStorer.dll"

open System
open System.IO
open System.IO.Compression
open System.Reflection

open Fake
open Fake.AssemblyInfoFile
open Fake.Testing // yes, really -- for NUnit3
open Fake.OpenCoverHelper
open Fake.ReportGeneratorHelper
open FSharpLint.Fake

let Copyright  = ref String.Empty
let Version = ref String.Empty

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
    let epoch = new DateTimeOffset(2000, 1, 1, 0, 0, 0, new TimeSpan(int64 0))  
    let diff = now.Subtract(epoch)  
    let fraction = diff.Subtract(TimeSpan.FromDays(float diff.Days))  
    let revision= ((int fraction.TotalSeconds) / 3)  
    let version = sprintf "0.1.%d.%d" diff.Days revision
    Version := version
    let copy = sprintf "© 2010-%d by Steve Gilham <SteveGilham@users.noreply.github.com>" now.Year
    Copyright := "Copyright " + copy

    let stream2 = new System.IO.FileStream("./Build/SelfTest.snk", System.IO.FileMode.Open, System.IO.FileAccess.Read)
    let pair2 = new StrongNameKeyPair(stream2)
    let key2 = BitConverter.ToString pair2.PublicKey

    let stream = new System.IO.FileStream("./Build/Infrastructure.snk", System.IO.FileMode.Open, System.IO.FileAccess.Read)
    let pair = new StrongNameKeyPair(stream)
    let key = BitConverter.ToString pair.PublicKey

    CreateFSharpAssemblyInfo "./_Generated/AssemblyVersion.fs"
        [Attribute.Version "0.1.0.0"
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
                                 Filter = "+[AltCove*]* -[*]Microsoft.* -[*]System.*"
                                 MergeByHash = true
                                 Register = RegisterType.RegisterUser
                                 Output = "_Reports/OpenCoverReport.xml" }) 
        "_Binaries/AltCover.Tests/Debug+AnyCPU/AltCover.Tests.dll --result=./_Reports/NUnit3ReportOpenCovered.xml"
    ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                       TargetDir = "_Reports/_UnitTest"})
        ["./_Reports/OpenCoverReport.xml"]
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
    let altReport = Path.Combine(reports, "AltCoverage.xml")
    let keyfile = FullName "Build\SelfTest.snk"

    ensureDirectory "./_Reports/_Instrumented"
    ensureDirectory <| Path.Combine(targetDir, "__Instrumented")

    printfn "Self-instrument under OpenCover"
    OpenCover (fun p -> { p with ExePath = findToolInSubPath "OpenCover.Console.exe" "."
                                 WorkingDir = targetDir
                                 TestRunnerExePath = findToolInSubPath "AltCover.exe" targetDir
                                 Filter = "+[AltCove*]* -[*]Microsoft.* -[*]System.*"
                                 MergeByHash = true
                                 Register = RegisterType.RegisterUser
                                 Output = Path.Combine(reports, "OpenCoverInstrumentationReport.xml") }) 
        ("/sn=" + keyfile + " -f=Mono. -f=.Recorder -f=Sample. -f=nunit. -t=System. -x=" + altReport)
    ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                       TargetDir = "_Reports/_Instrumented"})
        ["./_Reports/OpenCoverInstrumentationReport.xml"]

    printfn "Re-instrument everything"
    ensureDirectory "./_Reports/_AltReport"
    let altReport2 = Path.Combine(reports, "AltCoverage2.xml")
    let result = ExecProcess (fun info -> info.FileName <- "_Binaries/AltCover.Tests/Debug+AnyCPU/__Instrumented/AltCover.exe"
                                          info.WorkingDirectory <- "_Binaries/AltCover.Tests/Debug+AnyCPU"
                                          info.Arguments <- ("/sn=" + keyfile + @" -f=Mono. -f=.Recorder -f=Sample. -f=nunit. -t=System. /o=.\__ReInstrument -x=" + altReport2)) (TimeSpan.FromMinutes 5.0)
    ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                       TargetDir = "_Reports/_AltReport"})
        [altReport]

    if result <> 0 then failwithf "Re-instrument returned with a non-zero exit code"    

    printfn "Unit test instrumented code"
    ensureDirectory "./_Reports"
    !! (@"_Binaries\*Tests\Debug+AnyCPU\__Instrumented\*.Tests.dll")
    |> NUnit3 (fun p -> { p with ToolPath = findToolInSubPath "nunit3-console.exe" "."
                                 WorkingDir = "."
                                 ResultSpecs = ["./_Reports/NUnit3ReportInstrumented.xml"] })

    printfn "Unit-test instrumented code under OpenCover"
    ensureDirectory "./_Reports/_UnitTestInstrumented"
    OpenCover (fun p -> { p with ExePath = findToolInSubPath "OpenCover.Console.exe" "."
                                 WorkingDir = "."
                                 TestRunnerExePath = findToolInSubPath "nunit3-console.exe" "."
                                 Filter = "+[AltCove*]* -[*]Microsoft.* -[*]System.*"
                                 MergeByHash = true
                                 Register = RegisterType.RegisterUser
                                 Output = "_Reports/OpenCoverReportAltCovered.xml" }) 
        "_Binaries/AltCover.Tests/Debug+AnyCPU/__Instrumented/AltCover.Tests.dll --result=./_Reports/NUnit3ReportAltCovered.xml"
    ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                       TargetDir = "_Reports/_UnitTestInstrumented"})
        ["./_Reports/OpenCoverReportAltCovered.xml"]
)

let SimpleInstrumentingRun (binaryPath:string) (reportSigil:string) = 
    printfn "Instrument a simple executable"
    let simpleReport = Path.Combine(FullName "./_Reports", "SimpleCoverage" + reportSigil + ".xml")
    let binRoot = FullName binaryPath
    let sampleRoot = FullName "_Binaries/Sample1/Debug+AnyCPU"
    let instrumented = "__Instrumented" + reportSigil
    let result = ExecProcess (fun info -> info.FileName <- Path.Combine(binRoot, "AltCover.exe")
                                          info.WorkingDirectory <- sampleRoot
                                          info.Arguments <- ("-t=System. -x=" + simpleReport + " /o=./" + instrumented)) (TimeSpan.FromMinutes 5.0)
    if result <> 0 then failwith "Simple instrumentation failed"
    let result2 = ExecProcess (fun info -> info.FileName <- Path.Combine(sampleRoot, instrumented + "/Sample1.exe")
                                           info.WorkingDirectory <- Path.Combine(sampleRoot, instrumented)
                                           info.Arguments <- "") (TimeSpan.FromMinutes 5.0)
    if result2 <> 0 then failwith "Instrumented .exe failed"
    
    ensureDirectory ("./_Reports/_SimpleReport" + reportSigil)
    ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                       TargetDir = "_Reports/_SimpleReport" + reportSigil})
        [simpleReport]

Target "SimpleInstrumentation" (fun _ ->
   SimpleInstrumentingRun "_Binaries/AltCover/Debug+AnyCPU" String.Empty
)


Target "BulkReport" (fun _ ->
    printfn "Overall coverage reporting"
    ensureDirectory "./_Reports/_BulkReport"
    !! "./_Reports/*cover*.xml"
    |> Seq.filter (fun f -> not <| f.Contains("NUnit"))
    |> Seq.toList
    |> ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
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
)

Target "Gendarme" (fun _ ->
    ExecProcess (fun info -> info.FileName <- (findToolInSubPath "gendarme.exe" ".\packages")
                             info.WorkingDirectory <- "."
                             info.Arguments <- "--console --html ./_Reports/gendarme.html  _Binaries/AltCover/Debug+AnyCPU/AltCover.exe  _Binaries/AltCover.Shadow/Debug+AnyCPU/AltCover.Shadow.dll") (TimeSpan.FromMinutes 5.0)
    |> ignore
                                
)


Target "Package"  (fun _ ->
    ensureDirectory "./_Binaries/Packaging"
    ensureDirectory "./_Packaging"
    let AltCover = FullName "_Binaries/AltCover/AltCover.exe"
    let recorder = FullName "_Binaries/AltCover/Release+AnyCPU/AltCover.Recorder.dll"

    NuGet (fun p -> 
    {p with
        Authors = ["Steve Gilham"]
        Project = "AltCover"
        Description = "A pre-instrumented code coverage tool for .net"                            
        OutputPath = "./_Packaging"
        WorkingDir = "./_Binaries/Packaging"
        Files = [
                        (AltCover, Some "tools", None)
                        (recorder, Some "tools", None)
                ]
        Version = !Version
        Copyright = !Copyright
        Publish = false }) 
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
    |> Seq.iter (fun entry -> zip.ExtractFile(entry, Path.Combine(unpack, Path.GetFileName(entry.FilenameInZip))) |> ignore)

   SimpleInstrumentingRun unpack ".R"
)

"Clean"
==> "SetVersion"
==> "BuildRelease"
==> "Package"
==> "SimpleReleaseTest"

"SetVersion"
=?> ("BuildDebug", (not(File.Exists("./_Generated/AssemblyVersion.fs"))))

"BuildDebug"
==> "Lint"
==> "Test"
==> "TestCover"
==> "FxCop"
==> "SelfTest"
==> "SimpleInstrumentation"
==> "BulkReport"
==> "SimpleReleaseTest"

RunTargetOrDefault "SimpleReleaseTest"
