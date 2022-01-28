# F# Fake and Cake integration v7.x

APIs for use with build scripting tools are provided in the `AltCover.Cake.dll` and `AltCover.Fake.dll` assemblies, which are present in the `AltCover.Api` nuget package

* [Fake integration](#fake-integration)
* [Cake integration](#cake-integration)

# Fake integration 
Found in `AltCover.Fake.dll`  
Detailed API documentation is [presented here](AltCover.Fake/Fake-fsapidoc).

### Example
Driving `dotnet test` in a Fake script (based on [the AltCover build script here](https://github.com/SteveGilham/altcover/blob/master/Build/targets.fsx#L3425-L3454))
```
#r "paket:
nuget Fake.DotNet.Cli >= 5.20.3
nuget AltCover.Api >= 7.0 //"

let ForceTrue = AltCover.DotNet.CLIOptions.Force true 

let p =
  { AltCover.Primitive.PrepareOptions.Create() with
      CallContext = [| "[Fact]"; "0" |]
      AssemblyFilter = [| "xunit" |] }

let prepare = AltCover.AltCover.PrepareOptions.Primitive p
let c = AltCover.Primitive.CollectOptions.Create()
let collect = AltCover.AltCover.CollectOptions.Primitive c

open AltCover.Fake.DotNet // extension method WithAltCoverOptions
Fake.DotNet.DotNet.test
  (fun to' -> to'.WithAltCoverOptions prepare collect ForceTrue)
  "dotnettest.fsproj"

```

# Cake integration 

Found in `AltCover.Cake.dll`  
Detailed API documentation is [presented here](AltCover.Cake/AltCover.Cake-apidoc).

In your `.cake` file include

```
#addin "nuget:?package=Microsoft.TestPlatform.ObjectModel&Version=16.1.1"
#addin "nuget:?package=PowerShellStandard.Library&Version=5.1.0"
#addin "nuget:?package=altcover.api&Version=<whatever>"

```
the first two needed to silence warnings.

Implement the needed interfaces e.g. by copying and pasting this for the minimal example
```
  class TestOptions : AltCover.DotNet.ICLIOptions
  {
    public bool ForceDelete => false;
    public bool FailFast => false;
    public string ShowSummary => String.Empty;
  }

  class TestPrepareOptions : AltCover.Abstract.IPrepareOptions
  {
    public IEnumerable<string> InputDirectories => throw new NotImplementedException("InputDirectories not used");
    public IEnumerable<string> OutputDirectories => throw new NotImplementedException("OutputDirectories not used");
    public IEnumerable<string> SymbolDirectories => Array.Empty<string>();
    public IEnumerable<string> Dependencies => Array.Empty<string>();
    public IEnumerable<string> Keys => Array.Empty<string>();
    public string StrongNameKey => String.Empty;
    public string Report => String.Empty;
    public IEnumerable<string> FileFilter => Array.Empty<string>();
    public IEnumerable<string> AssemblyFilter => Array.Empty<string>();
    public IEnumerable<string> AssemblyExcludeFilter => Array.Empty<string>();
    public IEnumerable<string> TypeFilter => Array.Empty<string>();
    public IEnumerable<string> MethodFilter => Array.Empty<string>();
    public IEnumerable<string> AttributeFilter => Array.Empty<string>();
    public IEnumerable<string> PathFilter => Array.Empty<string>();
    public IEnumerable<string> AttributeTopLevel => Array.Empty<string>();
    public IEnumerable<string> TypeTopLevel => Array.Empty<string>();
    public IEnumerable<string> MethodTopLevel => Array.Empty<string>();
    public IEnumerable<string> CallContext => new string[] {"[Fact]", "0"};
    public string ReportFormat => String.Empty;
    public bool InPlace => false;
    public bool Save => false;
    public bool ZipFile => false;
    public bool MethodPoint => false;
    public bool SingleVisit => false;
    public bool LineCover => false;
    public bool BranchCover => false;
    public IEnumerable<string> CommandLine => throw new NotImplementedException("CommandLine not used");
    public bool ExposeReturnCode => throw new NotImplementedException("ExposeReturnCode not used");
    public bool SourceLink => true;
    public bool Defer => true;
    public bool LocalSource => true;
    public bool VisibleBranches => false;
    public string ShowStatic => String.Empty;
    public bool ShowGenerated => false;
    public System.Diagnostics.TraceLevel Verbosity => System.Diagnostics.TraceLevel.Verbose;
  }

  class TestCollectOptions : AltCover.Abstract.ICollectOptions
  {
    public string RecorderDirectory => throw new NotImplementedException("RecorderDirectory not used");
    public string WorkingDirectory => throw new NotImplementedException("WorkingDirectory not used");
    public string Executable => throw new NotImplementedException("Executable not used");
    public string LcovReport => String.Empty;
    public string Threshold => String.Empty;
    public string Cobertura => String.Empty;
    public string OutputFile => String.Empty;
    public IEnumerable<string> CommandLine => throw new NotImplementedException("CommandLine not used");
    public bool ExposeReturnCode => throw new NotImplementedException("ExposeReturnCode not used");
    public string SummaryFormat => String.Empty;
    public System.Diagnostics.TraceLevel Verbosity => System.Diagnostics.TraceLevel.Verbose;
  }
```
changing fixed values to `{ get; set; }` as required; then your test-with-coverage phase looks like
```
{
    // do any required customizations here
    var altcoverSettings = new AltCover.Cake.CoverageSettings {
        PreparationPhase = new TestPrepareOptions(),
        CollectionPhase = new TestCollectOptions(),
        Options = new TestOptions()
    };

    var testSettings = new DotNetCoreTestSettings {
        Configuration = configuration,
        NoBuild = true,
    };

    DotNetCoreTest(<project to test>,
                      testSettings, altcoverSettings);
});

```
