#addin "nuget:?package=Microsoft.TestPlatform.ObjectModel&Version=16.1.1"
#addin "nuget:?package=PowerShellStandard.Library&Version=5.1.0"
#addin "nuget:file://{0}?package=altcover.api&Version={1}"
#addin "nuget:file://{0}?package=altcover.cake&Version={1}"

var target = Argument("target", "Test");
var configuration = Argument("configuration", "Debug");
var cakeversion = Argument("cakeversion", "Unknown");

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
#if CAKE_2
    DotNetBuild("./_DotnetTest/cake_dotnettest.fsproj", new DotNetBuildSettings
    {
        Configuration = configuration,
    });
#else
    DotNetCoreBuild("./_DotnetTest/cake_dotnettest.fsproj", new DotNetCoreBuildSettings
    {
        Configuration = configuration,
    });
#endif
});

  class TestPrepareOptions : AltCover.Cake.PrepareOptions
  {
    public override IEnumerable<string> CallContext => new string[] {"[Fact]", "0"};
    public override System.Diagnostics.TraceLevel Verbosity => System.Diagnostics.TraceLevel.Verbose;
  }

Task("Test")
    .IsDependentOn("Build")
    .Does(() =>
{
    using AltCover.Cake;

    var testv = Version();
    var im = ImportModule();
    Console.WriteLine("Version = {1} Import = '{2}'", null, testv, im);

    var prep = new TestPrepareOptions();


#if CAKE_2
    prep.Report = "coverage.build2.cake." + cakeversion +".xml";
#else
    prep.Report = "coverage.build.cake." + cakeversion +".xml";
#endif

    var altcoverSettings = new CoverageSettings {
        PreparationPhase = prep,
        CollectionPhase = new CollectOptions(),
        Options = new TestOptions()
    };

#if CAKE_2
    var testSettings = new DotNetTestSettings {
        Configuration = configuration,
        NoBuild = true,
    };
    
    testSettings.ArgumentCustomization = altcoverSettings.Concatenate(testSettings.ArgumentCustomization);
    DotNetTest("./_DotnetTest/cake_dotnettest.fsproj", testSettings);

#else
    var testSettings = new DotNetCoreTestSettings {
        Configuration = configuration,
        NoBuild = true,
    };

    DotNetCoreTest("./_DotnetTest/cake_dotnettest.fsproj", 
                      testSettings, altcoverSettings);
#endif
});


//////////////////////////////////////////////////////////////////////
// EXECUTION
//////////////////////////////////////////////////////////////////////

RunTarget(target);