#r "paket:
nuget Fake.Core.Target 5.3.0
nuget Fake.Core.Environment 5.3.0
nuget Fake.Core.Process 5.3.0
nuget Fake.DotNet.AssemblyInfoFile 5.3.0
nuget Fake.DotNet.Cli 5.3.0
nuget Fake.DotNet.MSBuild 5.3.0
nuget Fake.DotNet.NuGet 5.3.0
nuget Fake.DotNet.Testing.NUnit 5.3.0
nuget Fake.DotNet.Testing.OpenCover 5.3.0
nuget Fake.DotNet.Testing.XUnit2 5.3.0
nuget Fake.IO.FileSystem 5.3.0 
nuget AltCover >= 3.5.581 //"

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

let _Target s f =
  Target.description s
  Target.create s f

_Target "DoIt" (fun _ ->
  AltCover.Api.Version(
      {AltCover.Logging.Default with Info = printfn "%s"}
  ) |> printfn "Returned %A"
)

Target.runOrDefault "DoIt"