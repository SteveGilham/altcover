# altcover.cake
Contains options to run AltCover as part of `dotnet test` within a Cake build script.

## Usage

Given specifications `prepare`, `process` and `collect` (part of the API), extend a `dotnet test` invocation with coverage collection

```csharp
{
    // do any required customizations here
    var altcoverSettings = new AltCover.Cake.CoverageSettings {
        PreparationPhase = prepare,
        CollectionPhase = collect,
        Options = process
    };

    var testSettings = new DotNetCoreTestSettings {
        Configuration = configuration,
        NoBuild = true,
    };

    DotNetCoreTest(<project to test>, // or DotNetTest at v2 or v3
                      testSettings, altcoverSettings);
});
```

See the [Wiki page](https://stevegilham.github.io/altcover/Fake-and-Cake-integration#cake-integration) for full details

## What's in the box?

APIs for integration into Cake build scripts (at `netstandard2.0` for v1, and `netcoreapp3.1` for v2 and v3 Cake APIs)

## Why altcover?
As the name suggests, it's an alternative coverage approach.  Rather than working by hooking the .net profiling API at run-time, it works by weaving the same sort of extra IL into the assemblies of interest ahead of execution.  This means that it should work pretty much everywhere, whatever your platform, so long as the executing process has write access to the results file.  You can even mix-and-match between platforms used to instrument and those under test.

In particular, while instrumenting .net core assemblies "just works" with this approach, it also supports Mono, as long as suitable `.mdb` (or `.pdb`, in recent versions) symbols are available.  One major limitation here is that the `.mdb` format only stores the start location in the source of any code sequence point, and not the end; consequently any nicely coloured reports that take that information into account may show a bit strangely.  

### Why altcover? -- the back-story of why it was ever a thing

Back in 2010, the new .net version finally removed the deprecated profiling APIs that the free NCover 1.5.x series relied upon.  The first version of AltCover was written to both fill a gap in functionality, and to give me an excuse for a ground-up F# project to work on.  As such, it saw real production use for about a year and a half, until [OpenCover](https://github.com/OpenCover/opencover) reached a point where it could be used for .net4/x64 work (and I could find time to adapt everything downstream that consumed NCover format input).

Fast forwards to autumn 2017, and I get the chance to dust the project off, with the intention of saying that it worked on Mono, too -- and realise that it's _déja vu_ all over again, because .net core didn't yet have profiler based coverage tools either, and the same approach would work there as well.

## Continuous Integration

| | | |
| --- | --- | --- |
| **Build** | GitHub [![Build status](https://github.com/SteveGilham/altcover/workflows/CI/badge.svg)](https://github.com/SteveGilham/altcover/actions?query=workflow%3ACI)[![Build history](https://buildstats.info/github/chart/SteveGilham/altcover?branch=master)](https://github.com/SteveGilham/altcover/actions?query=workflow%3ACI)|AppVeyor [![Build status](https://img.shields.io/appveyor/ci/SteveGilham/altcover.svg)](https://ci.appveyor.com/project/SteveGilham/altcover)  ![Build history](https://buildstats.info/appveyor/chart/SteveGilham/altcover) | 
| **Test coverage** | Coveralls [![Coverage Status](https://coveralls.io/repos/github/SteveGilham/altcover/badge.svg)](https://coveralls.io/github/SteveGilham/altcover) | AppVeyor [![Test status](https://img.shields.io/appveyor/tests/SteveGilham/altcover.svg)](https://ci.appveyor.com/project/SteveGilham/altcover)


### Possible retirement/obsolescence of support

tl;dr -- legacy framework/Mono support is not going away any time soon.

As `net472` can consume `netstandard2.0` libraries (everything but the recorder), and .net core 2+ can consume `net20` libraries (the recorder), legacy framework/Mono support continues until such a time as it is no longer possible to retain those API levels.

## Other NuGet Packages in this suite
* [![Nuget](https://buildstats.info/nuget/AltCover) General purpose install](https://www.nuget.org/packages/AltCover) -- excludes the `dotnet test` API with FAKE and CAKE integration and the AvaloniaUI visualizer
* [![Nuget](https://buildstats.info/nuget/altcover.api) API install](https://www.nuget.org/packages/AltCover.api) -- excludes the visualizer in all forms
* [![Nuget](https://buildstats.info/nuget/altcover.global) dotnet global tool install](https://www.nuget.org/packages/AltCover.global) -- excludes the visualizer in all forms
* [![Nuget](https://buildstats.info/nuget/altcover.visualizer) Visualizer dotnet global tool](https://www.nuget.org/packages/AltCover.visualizer) -- just the .net core/Avalonia Visualizer as a global tool
* [![Nuget](https://buildstats.info/nuget/altcover.fake) FAKE build task utilities](https://www.nuget.org/packages/AltCover.Fake) -- just AltCover related helper types for FAKE scripts (v5.18.1 or later), only in this package