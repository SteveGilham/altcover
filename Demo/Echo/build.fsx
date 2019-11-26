#r "paket:
nuget Fake.Core.Target >= 5.16.1
nuget Fake.Core.Environment >= 5.16.1
nuget Fake.Core.Process >= 5.16.1
nuget Fake.DotNet.AssemblyInfoFile >= 5.16.1
nuget Fake.DotNet.Cli >= 5.16.1
nuget Fake.DotNet.FxCop >= 5.16.1
nuget Fake.DotNet.ILMerge >= 5.16.1
nuget Fake.DotNet.MSBuild >= 5.16.1
nuget Fake.DotNet.NuGet >= 5.16.1
nuget Fake.DotNet.Testing.NUnit >= 5.16.1
nuget Fake.DotNet.Testing.OpenCover >= 5.16.1
nuget Fake.DotNet.Testing.XUnit2 >= 5.16.1
nuget Fake.IO.FileSystem >= 5.16.1
nuget Fake.Testing.ReportGenerator >= 5.16.1 //"

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

open System.IO

if Directory.Exists "./Report"
then Shell.cleanDir "./Report"

if Directory.Exists "./Echo/bin"
then Shell.cleanDir "./Echo/bin"

if Directory.Exists "./Test/bin"
then Shell.cleanDir "./Test/bin"

!!(@"./**/coverage.xm*")
|> Seq.iter File.Delete


let target = Path.Combine (Path.GetFullPath "Test", "SolutionRoot.cs")

let template = """
namespace Test
{
    internal static class SolutionRoot
    {
        public static string Location
        {
            get
            {
                return @"""
                + "\"" +  (Path.GetFullPath ".") + """\";
            }
        }
    }
}
"""
File.WriteAllText(target, template)

"Echo.sln"
|> MSBuild.build (fun p ->
   { p with Verbosity = Some MSBuildVerbosity.Normal
            ConsoleLogParameters = []
            DistributedLoggers = None
            DisableInternalBinLog = true
            Properties =
              [ "Configuration", "Debug"
                "DebugSymbols", "True" ] })

DotNet.exec id "altcover" "--save --inplace -i ./Echo/bin/Debug/netcoreapp2.2 --opencover"             

DotNet.test (fun o -> let custom = { o.Common with CustomParams = Some "/p:AltCover=true /p:AltCoverForce=true"}
                      { o with Common = custom}) "./Test/Test.csproj"

DotNet.exec id "altcover" "runner --collect -r ./Echo/bin/Debug/netcoreapp2.2"     

DotNet.exec id "reportgenerator" """-reports:"./**/coverage.xml" -targetdir:Report -reporttypes:Html"""             

