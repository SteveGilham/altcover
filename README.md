# altcover
Instrumenting coverage tool for .net (framework 2.0+  and core) and Mono, reimplemented and extended almost beyond recognition from [dot-net-coverage](https://github.com/SteveGilham/dot-net-coverage), plus a set of related utilities for processing the results from this and from other programs producing similar output formats.

## Never mind the fluff -- how do I get started?

Start with the [Quick Start guide](https://github.com/SteveGilham/altcover/wiki/QuickStart-Guide)

The latest releases can be downloaded from [releases](https://github.com/SteveGilham/altcover/releases), but the easiest (and most automated) way is through the [nuget packages](#nuget-packages).

## What's in the box?

For Mono, .net framework and .net core, except as noted

* `AltCover`, a command-line tool for recording code coverage (including dotnet and global tool versions)
* MSBuild tasks to drive the tool, including `dotnet test` integration
* An API for the above functionality, with Fake and Cake integration
* A PowerShell module for PowerShell 5.1 and PowerShell Core 6+ containing a cmdlet that drives the tool, and other cmdlets for manipulating coverage reports
* A coverage visualizer tool 
  * For .net framework and mono (for .net framework, needs GTK# v2.12.xx installed separately -- see https://www.mono-project.com/download/stable/#download-win )
  * For .net core : uses the cross-platform AvaloniaUI toolkit
  ![Visualizer screenshot](https://github.com/SteveGilham/altcover/raw/master/AltCover.Visualizer/Screenshot-Avalonia.png)
    
### NuGet Packages
* [![Nuget](https://buildstats.info/nuget/AltCover) General purpose install](https://www.nuget.org/packages/AltCover) -- excludes the `dotnet test` API with FAKE and CAKE integration and the AvaloniaUI visualizer
* [![Nuget](https://buildstats.info/nuget/altcover.api) API install](https://www.nuget.org/packages/AltCover.api) -- excludes the visualizer in all forms
* [![Nuget](https://buildstats.info/nuget/altcover.global) dotnet global tool install](https://www.nuget.org/packages/AltCover.global) -- excludes the visualizer in all forms
* [![Nuget](https://buildstats.info/nuget/altcover.visualizer) Visualizer dotnet global tool](https://www.nuget.org/packages/AltCover.visualizer) -- just the .net core/Avalonia Visualizer as a global tool
* [![Nuget](https://buildstats.info/nuget/altcover.fake) FAKE build task utilities](https://www.nuget.org/packages/AltCover.Fake) -- just AltCover related helper types for FAKE scripts (v5.18.1 or later), only in this package

## Why altcover?
As the name suggests, it's an alternative coverage approach.  Rather than working by hooking the .net profiling API at run-time, it works by weaving the same sort of extra IL into the assemblies of interest ahead of execution.  This means that it should work pretty much everywhere, whatever your platform, so long as the executing process has write access to the results file.  You can even mix-and-match between platforms used to instrument and those under test.

In particular, while instrumenting .net core assemblies "just works" with this approach, it also supports Mono, as long as suitable `.mdb` (or `.pdb`, in recent versions) symbols are available.  One major limitation here is that the `.mdb` format only stores the start location in the source of any code sequence point, and not the end; consequently any nicely coloured reports that take that information into account may show a bit strangely.  

### Why altcover? -- the back-story of why it was ever a thing

Back in 2010, the new .net version finally removed the deprecated profiling APIs that the free NCover 1.5.x series relied upon.  The first version of AltCover was written to both fill a gap in functionality, and to give me an excuse for a ground-up F# project to work on.  As such, it saw real production use for about a year and a half, until [OpenCover](https://github.com/OpenCover/opencover) reached a point where it could be used for .net4/x64 work (and I could find time to adapt everything downstream that consumed NCover format input).

Fast forwards to autumn 2017, and I get the chance to dust the project off, with the intention of saying that it worked on Mono, too -- and realise that it's _déja vu_ all over again, because .net core didn't yet have profiler based coverage tools either, and the same approach would work there as well.

## Continuous Integration

| | | | 
| --- | --- | --- | 
| **Build** | <sup>GitHub</sup> [![Build status](https://github.com/SteveGilham/altcover/workflows/CI/badge.svg)](https://github.com/SteveGilham/altcover/actions?query=workflow%3ACI)[![Build history](https://buildstats.info/github/chart/SteveGilham/altcover?branch=master)](https://github.com/SteveGilham/altcover/actions?query=workflow%3ACI)| <sup>AppVeyor</sup> [![Build status](https://img.shields.io/appveyor/ci/SteveGilham/altcover.svg)](https://ci.appveyor.com/project/SteveGilham/altcover)  ![Build history](https://buildstats.info/appveyor/chart/SteveGilham/altcover) |
| **Test coverage** | <sup>Coveralls</sup> [![Coverage Status](https://coveralls.io/repos/github/SteveGilham/altcover/badge.svg)](https://coveralls.io/github/SteveGilham/altcover) | <sup>AppVeyor</sup> [![Test status](https://img.shields.io/appveyor/tests/SteveGilham/altcover.svg)](https://ci.appveyor.com/project/SteveGilham/altcover)

## Usage

See the [Wiki page](https://github.com/SteveGilham/altcover/wiki/Usage) for details

## Roadmap

See the [current project](https://github.com/SteveGilham/altcover/projects/8) and [long term research items](https://github.com/SteveGilham/altcover/projects/9) for details; though _ad hoc_ items not in the projects will get added as inspiration or need arise.

All `To do` and  `On Hold` items are implicitly `up for grabs` and `Help Wanted`; most of the current project items are XML manipulation or GUI programming.

### Possible retirement/obsolescence of support

tl;dr -- legacy framework/Mono support is not going away any time soon.

Despite earlier ruminations on the subject, as .net 4.7.2 can consume `netstandard2.0` libraries (everything but the recorder), and .net core 2+ can consume `net20` libraries (the recorder), legacy framework/Mono support continues after the release of .net 5 and until such a time as it is no longer possible to retain those API levels.  Framework builds apart from the minimum (executable entry-points and the recorder) remain until I have suitable replacements for Framework-only static analysis tooling (i.e. can convince FxCop to consume `netstandard20`).

## Building

### Tooling

#### All platforms

It is assumed that the following are available

.net SDK version as per global.json, or later minor version (`dotnet`) -- try https://www.microsoft.com/net/download  
PowerShell Core 7.2.0 or later (`pwsh`) -- try https://github.com/powershell/powershell  

The build may target `netstandard2.0` or `netcoreapp2.0/2.1` for deliverables, and `net6.0` for unit tests, but does not need any pre-6.0 runtimes to be installed (roll-forward policies are in place).

**Note:** F# compiler code generation changes may cause incompatibilities due to some of the IL inspection performed by AltCover and its self-tests (e.g. by, at 5.0.201, generating non-closure function objects as static fields rather than locally instantiated objects)

#### Windows

If an IDE is desired, Visual Studio VS2022 (Community Edition) with desktop option, including F# language support

For GTK# support, the GTK# latest 2.12 install is expected -- try https://www.mono-project.com/download/stable/#download-win -- while the latest releases of the GTK#3 libraries will download the native support if the expected version is not detected.

#### *nix

It is assumed that `mono` (version 6.12.x or later) and `dotnet` are on the `PATH` already, and everything is built from the command line, with your favourite editor used for coding.

### Bootstrapping

Start by setting up with `dotnet tool restore`; this sets up local tools including `dotnet fake`.
Then `dotnet fake run ./Build/setup.fsx` to do the rest of the set-up.

### Normal builds

Running `dotnet fake run ./Build/build.fsx` performs a full build/test/package process.

Use `dotnet fake run ./Build/build.fsx --target <targetname>` to run to a specific target.

#### If the build fails

If there's a passing build on the CI servers for this commit, then it's likely to be one of the [intermittent build failures](https://github.com/SteveGilham/altcover/wiki/Intermittent-build-issues) that can arise from the tooling used. The standard remedy is to try again.

### Unit Tests

The tests in the `AltCover.Test` project are broadly ordered in the same dependency order as the code within the AltCover project (the later `Runner` tests aside).  While working on any given layer, it would make sense to comment out all the tests for later files so as to show what is and isn't being covered by explicit testing, rather than merely being cascaded through.

Note that some of the unit tests expect that the separate build of test assemblies under Mono, full .net framework and .net core has taken place; these tests will be marked `ignore` when running the unit tests under `net472` and `pass` without doing anything under `net5.0` (as Expecto has no `ignore` option) if the build is not complete and thus those expected assemblies are not found e.g. in Visual Studio from clean, or after a build to targets like `Analysis` that only build code to be analysed.

## Thanks to

* [AppVeyor](https://ci.appveyor.com/project/SteveGilham/altcover) for allowing free build CI services for Open Source projects
* [Coveralls](https://coveralls.io/r/SteveGilham/altcover) for allowing free services for Open Source projects
