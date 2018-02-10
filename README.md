# altcover
Instrumenting coverage tool for .net and Mono, reimplemented and extended from [dot-net-coverage](https://github.com/SteveGilham/dot-net-coverage)

The latest releases can be downloaded from [releases](https://github.com/SteveGilham/altcover/releases), but the easiest (and most automated) way is through the [nuget package](https://www.nuget.org/packages/AltCover).

## Why altcover?
As the name suggests, it's an alternative coverage approach.  Rather than working by hooking the .net profiling API at run-time, it works by weaving the same sort of extra IL into the assemblies of interest ahead of execution.  This means that it should work pretty much everywhere, so long as the executing process has write access to the results file.

In particular, this approach supports Mono, as long as suitable `.mdb` (or `.pdb`, in recent versions) symbols are available.  One major limitation here is that the `.mdb` format only stores the start location in the source of any code sequence point, and not the end; consequently any nicely coloured reports that take that information into account may show a bit strangely.  Another limitation with Mono, at least as experienced using FAKE to build projects under Mono on Linux in the travis-ci build, is that F# projects seem to generate no symbols, even when C# projects do -- and without symbols, such assemblies cannot be instrumented.

## Continuous Integration

| | |
| --- | --- |
| **Build** | <sup>AppVeyor</sup> [![Build status](https://img.shields.io/appveyor/ci/SteveGilham/altcover/master.svg)](https://ci.appveyor.com/project/SteveGilham/altcover) [![Test status](https://img.shields.io/appveyor/tests/SteveGilham/altcover/master.svg)](https://ci.appveyor.com/project/SteveGilham/altcover) <sup>Travis</sup> [![Build status](https://travis-ci.org/SteveGilham/altcover.svg?branch=master)](https://travis-ci.org/SteveGilham/altcover#)|
| **Unit Test coverage** | <sup>Coveralls</sup> [![Coverage Status](https://img.shields.io/coveralls/github/SteveGilham/altcover/master.svg)](https://coveralls.io/github/SteveGilham/altcover?branch=master) |
| **Nuget** | [![Nuget](https://buildstats.info/nuget/AltCover)](http://nuget.org/packages/AltCover) [![Nuget](https://img.shields.io/nuget/vpre/AltCover.svg)](http://nuget.org/packages/AltCover) |

## Usage

See the [Wiki page]( https://github.com/SteveGilham/altcover/wiki/Usage) for details

## Building

You will need Visual Studio VS2017 (Community Edition) v15.5 or later with F# language support.  The NUnit3 Test Runner will simplify the basic in-IDE development cycle.  Note that some of the unit tests expect that the separate build of an assembly under Mono has taken place; there will around a dozen file-not-found failures when running the unit tests in Visual Studio from clean.

### Bootstrapping

Running `.\Build\get-nuget.ps1` pre-installs all the NuGet packages, including, most importantly, the Fake build system.

### Normal builds

Running `.\fake.bat` performs a full build/test/package process.

### Unit Tests

The tests in the `Tests.fs` file are ordered in the same dependency order as the code within the AltCover project.  While working on any given layer, it would make sense to comment out all the tests for later files so as to show what is and isn't being covered by explicit testing, rather than merely being cascaded through.

### Other

The main executable links the still in beta (after more than a year) Mono.Cecil 0.10 version.  The recorder assembly injected into the instrumented code does not.  Rather than hold my releases on the Mono.Cecil schedule, I make this disclaimer instead.

## Thanks to

* [AppVeyor](https://ci.appveyor.com/project/SteveGilham/altcover) for allowing free build CI services for Open Source projects
* [travis-ci](https://travis-ci.org/SteveGilham/altcover) for allowing free build CI services for Open Source projects
* [Coveralls](https://coveralls.io/r/SteveGilham/altcover) for allowing free services for Open Source projects