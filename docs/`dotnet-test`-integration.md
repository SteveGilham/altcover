Available from release 3.0.488, this parallels the facility in [coverlet](https://github.com/tonerdo/coverlet); it is equivalent to doing `AltCover --inplace --save` in the project output directory before the tests are run, then `AltCover Runner --collect` after, then deleting the instrumented files and moving the saved originals back in place.

[There is an API available](AltCover.DotNet/) for use with build scripting that composes the appropriate command line.

_Note: With `AltCover`, there is no requirement that the assembly from which coverage is gathered be distinct from the assembly that contains the tests._  Use `/p:AltCoverAssemblyExcludeFilter` if you want to exclude the unit tests from coverage.


## To Use this option
Install into your test project 
```
dotnet add package AltCover
```
(or `package altcover.api` from release 4.0.600 or `package altcover.dotnet` from release 3.5.550 to 6.8.761) and switch on to run
```
dotnet test /p:AltCover=true
```

In the default operation it will produce OpenCover format output in a file `coverage.xml` (or in the case of multiple target frameworks, and from release 4.0.600, files of the form `coverage.$(TargetFramework).xml` for each such target) in your project directory.
And if you want more control over what happens to the files, then is is still possible to use `AltCover` in its original, explicit, mode -- just don't use the `/p:AltCover=true` switch.

## Microsoft.Testing.Platform

When `global.json` selects `"test": { "runner": "Microsoft.Testing.Platform" }` (.NET 10 SDK or later), `dotnet test` does not run the `VSTest` target. Instead, for each target framework, `/p:AltCover=true` instruments the build output into `__Instrumented_$(TargetFramework)_$(ProjectName)` under `/p:AltCoverOutputRoot` (default `$(TargetDir)`) and runs the test application under `AltCover Runner`, which collects the coverage and applies `/p:AltCoverThreshold` after the application exits. This needs the `altcover` package. `altcover.api` does not include the runner, so with a supported launch it stops the run with an error.

* The project must set `IsTestProject` to `true` in the project file or in a `Directory.Build.props` file, because NuGet imports the AltCover build files before `Directory.Build.targets`. `Microsoft.NET.Test.Sdk` sets it, but some test frameworks do not when they run on Microsoft.Testing.Platform. Without it, `/p:AltCover=true` does nothing.
* Only the default launch of the test application is supported: the apphost, or `dotnet exec` of the target path when `UseAppHost` is `false`. Arguments after the target path are kept. For any other `RunCommand` or `RunArguments` (for example a .NET Framework target), AltCover writes a warning and the tests of that target framework run without coverage.
* `/p:AltCoverInPlace=true` is not supported. With a supported launch, it stops the run with an error.
* `/p:AltCoverFailFast` and `/p:AltCoverShowSummary` have no effect. `/p:AltCoverVerbosity` sets the output of `AltCover Runner`, which `dotnet test` shows when a test application fails. At the default `Info`, it includes the command line of the test application and the coverage summary.
* The return code of `AltCover Runner` is the larger of the return code of the test application and the threshold shortfall, as for `AltCover Runner --threshold`. `dotnet test` reads that number as a Microsoft.Testing.Platform exit code. So its message can name another cause, a shortfall of exactly 8 (the code for "zero tests ran") does not fail the run, and a shortfall larger than 2 replaces the code for a failed test.
* `dotnet test --list-tests` also runs under `AltCover Runner`. It overwrites the coverage report with the coverage of the discovery run, and applies `/p:AltCoverThreshold` to it.
* `dotnet test --test-modules` does not build the project, so AltCover does not run.
* `dotnet run /p:AltCover=true` on such a project also runs it under `AltCover Runner`.

## Other parameters

* `/p:AltCoverSymbolDirectories=`"pipe `'|'` separated list of paths"
* `/p:AltCoverDependencyList=`"pipe `'|'` separated *AND TERMINATED* list of paths"
* `/p:AltCoverKeys=`"pipe `'|'` separated list of paths to strong-name keys for re-signing assemblies"
* `/p:AltCoverFileFilter=`"pipe `'|'` separated list of file name regexes"
* `/p:AltCoverAssemblyFilter=`"pipe `'|'` separated list of names"
* `/p:AltCoverAssemblyExcludeFilter=`"pipe `'|'` separated list of names"
* `/p:AltCoverTypeFilter=`"pipe `'|'` separated list of names"
* `/p:AltCoverMethodFilter=`"pipe `'|'` separated list of names"
* `/p:AltCoverAttributeFilter=`"pipe `'|'` separated list of names"
* `/p:AltCoverPathFilter=`"pipe `'|'` separated list of file path regexes"
* `/p:AltCoverAttributeTopLevel=`"pipe `'|'` separated list of attribute regexs"
* `/p:AltCoverTypeTopLevel=`"pipe `'|'` separated list of typename regexs"
* `/p:AltCoverMethodTopLevel=`"pipe `'|'` separated list of method name regexs"
* `/p:AltCoverCallContext=`"pipe `'|'` separated list of names or numbers"
* `/p:AltCoverStrongNameKey=`"path to default strong-name key for assemblies"
* `/p:AltCoverReport=`"path to the report" default: `coverage.xml` or `coverage.json` in the project directory
* `/p:AltCoverReportFormat=`"Json", "NCover" or default "OpenCover"
* `/p:AltCoverShowStatic=-|+|++` to mark simple code like auto-properties in the coverage file
* `/p:AltCoverOutputRoot=`"path of root folder for saved/instrumented output for `dotnet test`, default `$(TargetDir)`"
* `/p:AltCoverZipFile="true|false"` - set "true" to store the coverage report in a `.zip` archive
* `/p:AltCoverMethodPoint="true|false"` - set "true" to record only the first point of each method
* `/p:AltCoverAll="true|false"` - set "true" to record every visit to each point
* `/p:AltCoverLineCover="true|false"` - set "true" to record only line coverage in OpenCover format
* `/p:AltCoverBranchCover="true|false"` - set "true" to record only branch coverage in OpenCover format
* `/p:AltCoverSourceLink=true|false` to opt for SourceLink document URLs for tracked files
* `/p:AltCoverLocalSource=true|false` to ignore assemblies with `.pdb`s that don't refer to local source
* `/p:AltCoverVisibleBranches=true|false` to ignore compiler generated internal `switch`/`match` branches
* `/p:AltCoverShowGenerated=true|false` to mark generated code in the coverage file
* `/p:AltCoverTrivia=true|false` to omit trivial sequence points
* `/p:AltCoverInPlace=true|false` to test in-place (meaning extra file copies)
* `/p:AltCoverLcovReport=`"path to lcov format result"
* `/p:AltCoverCobertura=`"path to cobertura format result"
* `/p:AltCoverThreshold=`"coverage threshold required"
* `/p:AltCoverSummaryFormat=[BROCN+]` one or more of TeamCity Block format/TeamCity bRanch format/Classic OpenCover/CRAP score or none at all; `+` means the same as `OC` which is also the default
* `/p:AltCoverPackages=`"pipe `'|'` separated list of source root paths"
* `/p:AltCoverVerbosity=`"Levels of output -- Verbose, Info (default), Warning, Error, or Off"
* `/p:AltCoverShowSummary=true|[ConsoleColor]` to echo the coverage summary to stdout (in the colour of choice, modulo what else your build process might be doing) if the string is a valid ConsoleColor name) N.B. if this option is present, with any non-empty value then the summary will be echoed
* `/p:AltCoverForce=true|false` to force delete any left-over `__Instrumented*` (or `__Saved*`, if `InPlace` is set) folders from previous runs
* `/p:AltCoverFailFast=true|false` to skip coverage collection if the unit tests fail
* `/p:AltCoverImportModule=true` to emit the `Import-Module` command needed to register the `pwsh` support
* `/p:AltCoverGetVersion=true|false` to emit the current AltCover version

**Note**: The pipe character `|` is used as a separator because the previous choice of `;` didn't play nice with MSBuild. In v6.0.700 or later, to introduce a `|` into a regex, escape it in by doubling (`||`); if a triplet `|||` or longer is present, doubling gets grouped from the left.  Sample use : `/p:AltCoverAssemblyExcludeFilter='^(?!(NameA||NameB)).*$'` to include only `NameA` or `NameB`.

**Note**: As MSBuild informational output is suppressed by default with `dotnet test`, and log verbosity has no fine-grained control, the `-v m` (`--verbosity minimal`) option is needed to show the progress and summary information for the instrumentation and collection process if this is desired.

**Note**: In the case of multiple target frameworks the framework identifier will be inserted ahead of the extension (if any) of the file name given in `/p:AltCoverReport` just as for the default `coverage.xml` or `coverage.json` name.

## Example
```
dotnet test /p:AltCover=true /p:AltCoverReport=".\altcover.xml" /p:AltCoverAssemblyExcludeFilter=NUnit
```
Chooses a different report name, and excludes the `NUnit3.TestAdapter` assembly that comes with its pdb files, and gets instrumented by default.
