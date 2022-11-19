# F# Fake and Cake integration v7.x and later

APIs for use with build scripting tools are provided in the `AltCover.Cake.dll` and `AltCover.Fake.dll` assemblies, which are present in the `AltCover.Api` nuget package

* [Fake integration](#fake-integration)
* [Cake integration](#cake-integration)

Fake versions are normally supported for six months after release (when Fake itself deprecates old versions), but deprecation of older versions is not eager. Cake version support is based on actual semantic API versioning, with the intent of going as far back as can be sompatible with supporting the current version.   Check the AltCover release notes to see how far back support actually extends : see [here](https://github.com/SteveGilham/altcover/blob/master/ReleaseNotes.md) and [here](https://github.com/SteveGilham/altcover/blob/master/ReleaseNotes%20-%20Previously.md).  

**NOTE:** dotnet SDK v7.0.100 requires special treatment.  See [here](https://github.com/SteveGilham/altcover/wiki/dotnet-SDK-7.0.100) and [here](https://github.com/SteveGilham/altcover/wiki/Release-8.5.841).

# Fake integration 
Found in `AltCover.Fake.dll`  
Detailed API documentation is [presented here](AltCover.Fake/Fake-fsapidoc).

## To use the Fake `dotnet test` API `Fake.DotNet.DotNet.test`
Driving `dotnet test` in a Fake script 

In the project(s) to be covered, insert at least

```
    <PackageReference Include="altcover.api" Version="<whatever>">
      <IncludeAssets>build;</IncludeAssets>
    </PackageReference>
```

with the relevant script fragment (based on [the AltCover build script here](https://github.com/SteveGilham/altcover/blob/9b12b5b27f2877fcde186c1d8c08f6335108e306/Build/targets.fsx#L3425-L3454))

```
!!./docs/BuildSample_1.fsx

```

# Cake integration 

Applies to Cake 2.0 and up (with harmless warnings if used with Cake 3.0 or later)

Found in `AltCover.Cake.dll`  
Detailed API documentation is [presented here](AltCover.Cake/AltCover.Cake-apidoc).

## To use the Cake `dotnet test` API `DotNetCoreTest`

In the project(s) to be covered, insert at least

```
    <PackageReference Include="altcover.api" Version="<whatever>">
      <IncludeAssets>build;</IncludeAssets>
    </PackageReference>
```

In your `.cake` file include

```
#addin "nuget:?package=Microsoft.TestPlatform.ObjectModel&Version=16.1.1"
#addin "nuget:?package=PowerShellStandard.Library&Version=5.1.0"
#addin "nuget:?package=altcover.api&Version=<whatever>"
#addin "nuget:?package=altcover.cake&Version=<whatever>"

```
the first two needed to silence warnings.

Implement the needed interfaces ([as documented here](AltCover.Engine/AltCover/Abstract-apidoc)) e.g. by overriding the default types

* [TestOptions](AltCover.Cake/AltCover.Cake/TestOptions-apidoc)
* [PrepareOptions](AltCover.Cake/AltCover.Cake/PrepareOptions-apidoc)
* [CollectOptions](AltCover.Cake/AltCover.Cake/CollectOptions-apidoc)

which have empty or `false` values for all relevant properties, changing values as required e.g.
```csharp
  class MyPrepareOptions : AltCover.Cake.PrepareOptions
  {
    public string ReportName { get; set; }
    // here, for sake of example, reportDirectory being a script parameter
    public override string Report => Path.Combine (reportDirectory, ReportName);
    public override IEnumerable<string> CallContext => new string[] {"[Fact]", "0"};
    public override System.Diagnostics.TraceLevel Verbosity => System.Diagnostics.TraceLevel.Verbose;
  }
```
then your test-with-coverage phase looks like
```
{
    // do any required customizations here such as
    var prep = new MyPrepareOptions() {
        ReportName = "coverage.build.cake." + cakeversion +".xml"
    };

    var altcoverSettings = new AltCover.Cake.CoverageSettings {
        PreparationPhase = prep,
        CollectionPhase = new CollectOptions(),
        Options = new TestOptions()
    };

    var testSettings = new DotNetTestSettings {
        Configuration = configuration,
        NoBuild = true,
    };

    // mix-in the AltCover coverage settings explicitly
    testSettings.ArgumentCustomization = altcoverSettings.Concatenate(testSettings.ArgumentCustomization);

    // test using the default alias
    DotNetTest("./_DotnetTest/cake_dotnettest.fsproj", testSettings);

```

As the `AltCover.Cake` assembly still targets Cake 2.0 and netcoreapp3.1, when used for Cake 3.0 there will be harmless warnings like
```
The assembly 'AltCover.Cake, Version=[whatever], Culture=neutral, PublicKeyToken=null'
is referencing an older version of Cake.Core (2.0.0).
For best compatibility it should target Cake.Core version 3.0.0.
```
but there will be no warnings about obsolescent types or methods being used.