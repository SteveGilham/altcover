# altcover
Instrumenting coverage tool for .net and Mono, reimplemented and extended from [dot-net-coverage](https://github.com/SteveGilham/dot-net-coverage)

The latest releases can be downloaded from [releases](https://github.com/SteveGilham/altcover/releases), but the easiest (and most automated) way is through the [nuget package](https://www.nuget.org/packages/AltCover).

## Why altcover?
As the name suggests, it's an alternative coverage approach.  Rather than working by hooking the .net profiling API at run-time, it works by weaving the same sort of extra IL into the assemblies of interest ahead of execution.  This means that it should work pretty much everywhere, so long as the executing process has write access to the results file.

In particular, this approach supports Mono, as long as suitable `.mdb` (or `.pdb`, in recent versions) symbols are available.  One major limitation here is that the `.mdb` format only stores the start location in the source of any code sequence point, and not the end; consequently any nicely coloured reports that take that information into account may show a bit strangely.  Another limitation with Mono, at least as experienced using FAKE to build projects under Mono on Linux in the travis-ci build, is that F# projects seem to generate no symbols, even when C# projects do -- and without symbols, such assemblies cannot be instrumented.

## Continuous Integration

| | |
| --- | --- |
| **Build** | <sup>AppVeyor</sup> [![Build status](https://img.shields.io/appveyor/ci/SteveGilham/altcover/master.svg)](https://ci.appveyor.com/project/SteveGilham/altcover) [![Test status](https://img.shields.io/appveyor/tests/SteveGilham/altcover/master.svg)](https://ci.appveyor.com/project/SteveGilham/altcover) <sup>Travis</sup> [![Build status](https://travis-ci.org/SteveGilham/altcover.svg?branch=master")](https://travis-ci.org/SteveGilham/altcover#)|
| **Unit Test coverage** | <sup>Coveralls</sup> [![Coverage Status](https://img.shields.io/coveralls/github/SteveGilham/altcover/master.svg)](https://coveralls.io/github/SteveGilham/altcover?branch=master) |
| **Nuget** | [![Nuget](https://buildstats.info/nuget/AltCover)](http://nuget.org/packages/AltCover) [![Nuget](https://img.shields.io/nuget/vpre/AltCover.svg)](http://nuget.org/packages/AltCover) |

## Usage

The full command line is 

```AltCover [/i[nputDirectory]=VALUE] [/o[utputDirectory]=VALUE] [/sn|strongNameKey=VALUE] [/x[mlReport]=VALUE] [/f[ileFilter]=VALUE] [/s|assemblyFilter=VALUE] [/t|typeFilter=VALUE] [/m|methodFilter=VALUE] [/a|attributeFilter=VALUE] [/?|h[elp]] [-- command arguments]```

In detail

*  -i, --inputDirectory=VALUE  Optional: The folder containing assemblies to instrument (default: current directory)
*  -o, --outputDirectory=VALUE Optional: The folder to receive the instrumented assemblies and their companions (default: sub-folder '__Instrumented' of the current directory)
*  -k, --key=VALUE             Optional, multiple: any other strong-name key to use
*  --sn, --strongNameKey=VALUE Optional: The default strong naming key to apply to instrumented assemblies (default: None)
*  -x, --xmlReport=VALUE       Optional: The output report template file (default: coverage.xml in the current directory)
*  -f, --fileFilter=VALUE      Optional: source file name to exclude from instrumentation (may repeat)
*  -s, --assemblyFilter=VALUE  Optional: assembly name to exclude from instrumentation (may repeat)
*  -t, --typeFilter=VALUE      Optional: type name to exclude from instrumentation (may repeat)
*  -m, --methodFilter=VALUE    Optional: method name to exclude from instrumentation (may repeat)
*  -a, --attributeFilter=VALUE Optional: attribute name to exclude from instrumentation (may repeat)
*  -?, --help, -h              Prints out the options.
* --                           the rest of the command line is treated as a command to execute after performing instrumentation

The strong-name key arguments (`-k`, `--sn`) are not available in the .net core build, as the Mono.Cecil beta on that platform (and maybe the whole platform?), does not currently support strong-naming even though it can consume strong-named assemblies.

Filter values are semi-colon separated regular expressions, applied by type in the order as they are defined in the command line; any item whose name matches the expression will be excluded from the coverage reporting.  In the simplest case, with no special regex items, this means that a name containing the filter item as a sub-string will be excluded.

Coverage statistics are written to the file nominated by the `x|xmlReport=` parameter as instrumented assemblies are unloaded from an executing AppDomain, even if this is days or weeks later.  In practice the instrumented assemblies should be deleted after the relevant testing has been run, and the report file will thus be freed up.

### Use Case : Unit tests

In the case of a single unit test assembly, then executing AltCover with `/i=<unit test output directory>` to pick up the tests and dependencies, with a strongname replacement `/sn=<my component key>` will usually be sufficient, as framework assemblies without symbols will be ignored.  The test execution can then happen in the context of the output directory.

If there are symbol-bearing third-party assemblies (e.g. from NuGet packages such as `Mono.Options.Signed` as in this project), then those can be excluded with an extra `/s=<identifying substring of third party name>`

### Use Case : Windows Service

The simplest case involves patching the service under test.  The service should be installed and working on the test machine, with the symbols co-located with the assemblies of interest.  Stop the service, run AltCover on the directory containing the service, swap that folder and the one with the AltCover output (ensuring that the coverage file is not moved in this operation and is writeable by the service), then restart the service and begin testing.

When the test is complete, stop the service, swap the uninstrumented binaries back into place so that service operations won't continue to write to the coverage file, and use the coverage data as desired.

See the instructions under `Demo/Service` for a worked example

Alternatively, the binaries could be instrumented as a build step, and included, along with the coverage XML file and the `AltCover.Recorder.g.dll` helper, in an installer.  The coverage XML file would in that case need to be installed to the exact same file path as it was created in on the build machine. 

### Other Remarks

[Known issue](https://github.com/SteveGilham/altcover/projects/3#card-6169040) with the `/a|attributeFilter=` parameter in the current codebase : there is no compensation for places where the compiler creates new methods under the covers.  This means that constructs like lambdas, `yield return` and `async`/`await` will leave some code that is apparently excluded-by-attribute in the source actually included and instrumented because it falls in a different method (maybe even in a different type) in the compiled code.

## Building

You will need Visual Studio VS2017 (Community Edition) v15.5 or later with F# language support.  The NUnit3 Test Runner will simplify the basic in-IDE development cycle.  Note that some of the unit tests expect that the separate build of an assembly under Mono has taken place; there will be half a dozen file-not-found failures when running the unit tests in Visual Studio from clean.

### Bootstrapping

Running `.\Build\get-nuget.ps1` pre-installs all the NuGet packages, including, most importantly, the Fake build system.

### Normal builds

Running `.\fake.bat` performs a full build/test/package process.

### Unit Tests

The tests in the `Tests.fs` file are ordered in the same dependency order as the code within the AltCover project.  While working on any given layer, it would make sense to comment out all the tests for later files so as to show what is and isn't being covered by explicit testing, rather than merely being cascaded through.

## Thanks to

* [AppVeyor](https://ci.appveyor.com/project/SteveGilham/altcover) for allowing free build CI services for Open Source projects
* [travis-ci](https://travis-ci.org/SteveGilham/altcover) for allowing free build CI services for Open Source projects
* [Coveralls](https://coveralls.io/r/SteveGilham/altcover) for allowing free services for Open Source projects