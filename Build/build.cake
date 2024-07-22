#addin "nuget:?package=Microsoft.TestPlatform.ObjectModel&Version=16.1.1"
#addin "nuget:?package=PowerShellStandard.Library&Version=5.1.0"
#addin "nuget:file://{0}?package=altcover.api&Version={1}"
#addin "nuget:file://{0}?package=altcover.cake&Version={1}"

var target = Argument("target", "Test");
var configuration = Argument("configuration", "Debug");
var cakeversion = Argument("cakeversion", "Unknown");
var dotnetVersion = Argument("dotnetVersion", "Unknown");

//////////////////////////////////////////////////////////////////////
// TASKS
//////////////////////////////////////////////////////////////////////

Task("Clean")
    .WithCriteria(c => HasArgument("rebuild"))
    .Does(() =>
{
    CleanDirectory($"../_Binaries/cake_dotnettest/{configuration}+AnyCPU");
});

Task("Build")
    .IsDependentOn("Clean")
    .Does(() =>
{
    DotNetBuild("./_DotnetTest/cake_dotnettest.fsproj", new DotNetBuildSettings
    {
        Configuration = configuration,
    });
});

  class TestPrepareOptions : AltCover.Cake.PrepareOptions
  {
    public string CakeVersion;
    public override IEnumerable<string> CallContext => new string[] {"[Fact]", "0"};
    public override IEnumerable<string> AssemblyFilter  => new string[] {"testhost"};
    public override System.Diagnostics.TraceLevel Verbosity => System.Diagnostics.TraceLevel.Verbose;
    public override string Report => "coverage.build.cake." + CakeVersion +".xml";
    public override string OutputRoot => System.IO.Path.GetFullPath($"../_Binaries/cake_dotnettest/OutputRoot");
    public override bool All => true;
  }

Task("Test")
    .IsDependentOn("Build")
    .Does(() =>
{
    using AltCover.Cake;
    using FSDotNet = AltCover.DotNet;

    var testv = Version();
    var im = ImportModule();
    Console.WriteLine("Cake Version = {1} DotnetVersion = '{3}' Import = '{2}'", null, testv, im, dotnetVersion);

    var prep = new TestPrepareOptions() {
      CakeVersion = cakeversion
    };

    var altcoverSettings = new CoverageSettings {
        PreparationPhase = prep,
        CollectionPhase = new CollectOptions(),
        Options = new TestOptions()
    };

    var testSettings = new DotNetTestSettings {
        Configuration = configuration,
        NoBuild = true,
    };

    if (dotnetVersion != "7.0.100")
    {
      testSettings.ArgumentCustomization = altcoverSettings.Concatenate(testSettings.ArgumentCustomization);
    }
    else
    {
      var args =  FSDotNet.ToTestPropertiesList(
                      altcoverSettings.PreparationPhase,
                      altcoverSettings.CollectionPhase,
                      altcoverSettings.Options).ToArray();

      Array.ForEach(args, 
        s => { testSettings.EnvironmentVariables[s.Item1] = s.Item2;});
    }

    DotNetTest("./_DotnetTest/cake_dotnettest.fsproj", testSettings);
});


//////////////////////////////////////////////////////////////////////
// EXECUTION
//////////////////////////////////////////////////////////////////////

RunTarget(target);