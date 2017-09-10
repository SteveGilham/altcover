#r @"FakeLib.dll" // include Fake lib
open System
open System.IO

open Fake
open Fake.AssemblyInfoFile

// The clean target cleans the build and deploy folders
Target "Clean" (fun _ ->
    CleanDirs ["./_Binaries/"; "./_Intermediate/"; "./_Generated/"]
)

Target "SetVersion" (fun _ ->
    let now = DateTimeOffset.UtcNow  
    let epoch = new DateTimeOffset(2000, 1, 1, 0, 0, 0, new TimeSpan(int64 0))  
    let diff = now.Subtract(epoch)  
    let fraction = diff.Subtract(TimeSpan.FromDays(float diff.Days))  
    let revision= ((int fraction.TotalSeconds) / 3)  
    let version = sprintf "0.0.%d.%d" diff.Days revision

    CreateFSharpAssemblyInfo "./_Generated/AssemblyVersion.fs"
        [Attribute.Version "0.0.0.0"
         Attribute.FileVersion version]

// [<assembly: InternalsVisibleTo(\"Tests, PublicKey={0}\")>]
    let template ="#light
namespace Altcover
open System.Runtime.CompilerServices
[<assembly: InternalsVisibleTo(\"Tests\")>]
()
"
    let file = String.Format(System.Globalization.CultureInfo.InvariantCulture, template) //, token)
    let path = @"_Generated\VisibleToTest.fs"
    // Update the file only if it would change
    let old = if File.Exists(path) then File.ReadAllText(path) else String.Empty
    if not (old.Equals(file)) then File.WriteAllText(path, file)
)         

Target "BuildRelease" (fun _ ->
   !! "*.sln"
     |> MSBuildRelease "" ""
     |> Log "AppBuild-Output: "
)

Target "BuildDebug" (fun _ ->
   !! "*.sln"
     |> MSBuildDebug "" ""
     |> Log "AppBuild-Output: "
)

Target "Test" (fun _ ->
    !! (@"_Binaries\Tests\Debug+AnyCPU/Tests.dll")
      |> NUnit (fun p ->
          {p with
             DisableShadowCopy = true;
             OutputFile = "_/Output/TestResults.xml" })
)             

"Clean"
==> "SetVersion"
==> "BuildRelease"

"SetVersion"
=?> ("BuildDebug", (not(File.Exists("./_Generated/AssemblyVersion.fs"))))

"BuildDebug"
==> "Test"

RunTargetOrDefault "Test"
