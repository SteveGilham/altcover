# Previously

# 3.5.587 (Cerulean series release 10)
* [BUGFIX] Issue #29 -- handle strong-named dependencies properly in the .net core case (reinstating code too enthusiatically removed six months earlier)
* [BUGFIX] Issue #29 -- Allow `dotnet test` to usefully specify exclusion filters when building (did not affect `dotnet test --no-build`)
* Refactor in a few places to reduce code duplication 

# 3.5.580 (Cerulean series release 9)
* [NEW] `AltCover.Visualizer` tool for .net framework and mono (for .net framework, needs GTK#2.12.xx installed separately -- see https://www.mono-project.com/download/stable/)
* `-p|--pathFilter` (`-PathFilter`, `PathFilter="string[]"`, `/p:AltCoverPathFilter=...`) option to specify regexes to select source files to ignore on the basis of their full path; distinct from the `--fileFilter` option which works on the file name alone.  The distinction is that you'd use `-p` to exclude your folder `$(SolutionRoot)Generated`, and `-f` to exclude your `.g.cs` files wherever they are.
* [BUGFIX] Issue #28 --in OpenCover format,  don't emit empty (no `uid=` attribute)`FileRef` tags for methods that aren't being instrumented.
* [BUGFIX] Related degenerate case handling in `ConvertTo-BarChart`, `ConvertTo-LCov` and `ConvertTo-Cobertura` : these operations could fail for some legitimate corner cases found in OpenCover files (fixed in code shared with the Visualizer in the course of Visualizer testing)

# 3.5.569 (Cerulean series release 8)
* `altcover version` command line option to output the version of AltCover being used (also `Invoke-AltCover -Version`, MSBuild task `AltCover.GetVersion`, option `/p:AltCoverGetVersion`)
* `--single` (`-Single`, `Single="bool"`, `/p:AltCoverSingle=true`) option to only sample the first visit to each point (per AppDomain), reducing intermediate file size and processing time.
* `--linecover` (`-LineCover`, `LineCover="bool"`, `/p:AltCoverLineCover=true`) option to only only record line coverage in OpenCover format.
* `--branchcover` (`-BranchCover`, `BranchCover="bool"`, `/p:AltCoverBranchCover=true`) option to only record branch coverage in OpenCover format.

# 3.5.560 (Cerulean series release 7)
* [BUGFIX] -- `dotnet test` integration : don't leave the imported FSharp.Core.dll in the binaries folder
* `dotnet test` integration : Override the "VSTest" target to allow tidying after test failure
* [BUGFIX; BREAKING] -- `ConvertFrom-NCover` : fill in the `<Summary />`, `<Method />`, and `<MethodPoint />` attribute values to reflect the recorded visits.  This uses existing code, and comes at the cost of translating the output to `[xml]` instead of `XDocument`.

# 3.5.556 (Cerulean series release 6)
* [BUGFIX] -- Issue #23 : properly and completely remove strong-names when no new key is supplied ( = always in the .net core build, since Mono.Cecil there doesn't support setting strong-naming keys)

# 3.5.550 (Cerulean series release 5)
Functionality expansion release
### All NuGet packages
* All packages now include the `pwsh` module alongside the main .net core AltCover.dll
* `altcover ipmo` command line option to output the `Import-Module` invocation needed to register the PowerShell module for the version of AltCover being used.  Yes, the path for the global tool does include the name and version twice; that's just how it works.
* `AltCover.PowerShell` MSBuild task and option `/p:AltCoverIpmo=true` to expose this functionality through MSBuild and `dotnet test` integration
* [BUGFIX] messages categorized as warnings now display for the Dotnet-CLI and global tool versions.
* Better format for the throughput feedback (comma groups -- or localized equivalent -- for the big numbers)

### "classic" and dotnet CLI NuGet packages
* Support `dotnet add package ...` for `dotnet test` integration -- now added to dotnet CLI package
* Availability limited because dotnet global tools aren't compatible with `dotnet add package`

### "classic" NuGet package only
* provides the .net Framework/mono binaries, including the Windows PowerShell module

# 3.5.543 (Cerulean series release 4)
### All NuGet packages
* [BUGFIX] -- Issue #22 : Using a custom serialization gives the major speed-up I was looking for, without needing to try tricks that may be counter-productive given the use of compression in the process.
* **NOTE** this breaks compatibility between previously instrumented code and the new runner (and vice versa); in general, only same-version components are supported.
* Indicate the throughput levels in the command line output
### "classic" NuGet package only
* `ConvertTo-BarChart` report generation, based on the old NCover 1.5.8 style sheet, for both NCover and OpenCover coverage format data
* [BUGFIX] `ConvertTo-NCover` now works on `pwsh`
* `ConvertFrom-NCover` to OpenCover format
### Other
* Updated some consumed NuGet packages but not F# 4.5 (which breaks the `dotnet test` integration) : that's still being worked
* Test cmdlets in WindowsPowerShell in the build, as well as `pwsh`, plus some other refinements to the Pester-based tests

# 3.5.530 (Cerulean series release 3)
* Issue #20 related : `-d|dependency` option for all .net core versions (classic, .dotnet and .global) plus equivalents for pwsh, and MSBuild to allow the user to specify assemblies to satisfy references, anticipating resolution failures
* Issue #20 related : cache resolution failure fix-ups for some possible performance improvement
* Issue #20 related : notify the user of assembly resolution fix-up as a build warning
* minor bugfixes for `ConvertTo-Lcov` and `ConvertTo-Cobertura` for NCover format input
* minor bugfixes for `ConvertTo-XDocument` and `ConvertTo-XmlDocument` to handle XML processing instructions.
* Bugfix -- rework `ConvertTo-NCover` so it actually works on PowerShell Core
* FAKE 5.0 stable now being used
* Cross-platform unit/operational testing with coverage gathering of cmdlets in the build using Pester

# 3.5.518 (Cerulean series release 2)
* [BUGFIX] `ConvertTo-NCover` now also outputs to the object pipeline as well as to the optional file (altcover classic .nupkg)
* [BUGFIX] -- Issue #20 : on assembly resolution failure, look to the nuget package cache for a match (all .nupkg variants)

# 3.5.512 (Cerulean series release 1)
* Separate NuGet packages altcover.dotnet and altcover.global that contain the command-line tool functionality (only) for .net core as a CLI tool and as a .net core global tool respectively 
* [BREAKING] `dotnet test` integration - all names have been prefixed with `AltCover` to avoid collisions
* [BREAKING] `dotnet test` integration - `|` is used as the separator character for lists rather than `;` as the latter causes problems in the command-line context
* Extended the `ConvertTo-Cobertura` and `ConvertTo-Lcov` cmdlets
* `ConvertTo-XDocument` and `ConvertTo-XmlDocument` to interconvert in the object pipeline
* `ConvertTo-NCover` to take OpenCover format to classic NCover

# 3.5.500-pre (Cerulean series pre-release)
* Reduce the amount of unnecessary infrastructure runtime code in the .nupkg that bloated the 3.0.488-490 release
* Expose the LCov and Cobertura output formatters as simple cmdlets `ConvertTo-Cobertura` and `ConvertTo-Lcov` to take OpenCover or NCover format input.
* Minor adjustments to the `Invoke-AltCover` cmdlet based on PowerShell guidelines -- plural argument names made singular with backwards-compatible aliases
---
# 3.0.490 (Caba series release 12 re-spin)
* No functional change from 3.0.488, just removing an experimental extension that couldn't be tested without a release.

# 3.0.488 (Caba series release 12 -- deprecated)
* [Invoke-Altcover cmdlets](https://github.com/SteveGilham/altcover/wiki/PowerShell-integration) for both Windows PowerShell and PowerShell Core
* [coverlet-style `dotnet test` integration](https://github.com/SteveGilham/altcover/wiki/%60dotnet-test%60-integration)
* [BUGFIX] -- Possible branch instrumentation failure when filtering is applied.

# 3.0.475 (Caba series release 11)
* Fine tune the speed-up in data collection (marginal improvements only)
* [BUGFIX] -- some corner cases for nested functions (including uses of generics) were not being detected for purposes of exclusion by containing function
* [BUGFIX] -- Issue #17 : restore intended behaviour when no arguments supplied
* [BUGFIX] -- Issue #18 : protect against certain degenerate cases when looking for branch coverage
* other minor build process changes

# 3.0.466 (Caba series release 10)
* Support for starting/pausing/resuming coverage collection during operation -- see https://github.com/SteveGilham/altcover/wiki/Pause%E2%95%B1Resume-and-the-Control-File
* Major speed-up in data collection by reducing the amount of synchronization being done while writing data to file -- this means significant reductions in the time taken for instrumented code to execute 
* Support for excluding C#7.0 local functions by excluding the enclosing method
* Add CRAP (Change Risk Anti-Patterns) score to methods (and min/max values to summaries) in `--opencover` format and `runner`/`Collect` mode 
* [BUGFIX] -- using `dotnet AltCover.dll` to instrument a .net core project using F# could fail when rewriting the `.deps.json` file.
* [BUGFIX] -- not all branches were being tallied for certain complex F# match expressions

# 3.0.455 (Caba series release 9)
* `-o|outputFile` option in `runner` mode (parameter `OutputFile` for the `AltCover.Collect` task) to allow the default report for the current dataset to be written to a new file, rather than updating/accumulating in the file specified at instrumentation time.
* Some refactoring and adjustments to the build processes, latest FAKE RC etc.
* Support for starting/pausing/resuming coverage collection during operation -- see https://github.com/SteveGilham/altcover/wiki/Pause%E2%95%B1Resume-and-the-Control-File
* Add CRAP (Change Risk Anti-Patterns) score to methods (and min/max values to summaries) in `--opencover` format and `runner`/`Collect` mode 
* [BUGFIX] -- handle and signal exceptions in the visit recording, rather than potentially just locking up or silently failing
* [BUGFIX] -- ensure that more of the IDisposable objects generated, primarily the ones that can lock the assemblies under instrumentation, are promptly disposed.
* [BUGFIX] -- using `dotnet AltCover.dll` to instrument a .net core project using F# could fail when rewriting the `.deps.json` file.
* [BUGFIX] -- not all branches were being tallied for certain complex F# match expressions

# 3.0.444 (Caba series release 8)
* [BUGFIX] for `-c|callContext` option -- generate valid IL where the function being tracked completes with a tail call
* [BUGFIX] for `-c|callContext` option -- generate valid IL where the function being tracked contains a branch directly to a return instruction
* `-c|cobertura` option in `runner` mode (parameter `Cobertura` for the `AltCover.Collect` task) -- Cobertura format output to the given file name (more complete for OpenCover format coverage gathering than NCover, inevitably)
* Signal failure explicitly for `-t|threshold` violations, as well as through the return code

# 3.0.433 (Caba series release 7)
* `-t|threshold` option in `runner` mode (parameter `Threshold` for the `AltCover.Collect` task) to fail the build (non-zero return code or MSBuild error state) if coverage falls below the specified percentage
* [BUGFIX] -- Parameters weren't being re-initialised on the second or subsequent use of the MSBuild tasks, leading to failures about arguments being specified more than once (Issue #10)
* [BUGFIX] -- ArgumentNullException when relaying output from a launched process: filter null/empty messages
* FIXED AGAIN : reinstate earlier change [f61f951] to write .mdb out for .mdb in (and pdb out for pdb in) -- at least on .net core and full .net (long story)
* Moving some of the unit tests to XUnit for reasons related to the above

# 3.0.422 (Caba series release 6)
* [BUGFIX] -- Cecil resolves type dependencies by looking relative to the current working directory, no matter where it's actually writing the file : so work in the target directory while we are writing
* Also, just in case, ensure we process files from the depended-upon first, up the dependency chain (assumes no dependency cycles).
* Give feedback of what is written where and when.

# 3.0.416 (Caba series release 5)
* Exclude constructors on compiler generated types as being simply noise -- they will be exercised if you use any of the real code they represent, so nothing of importance is lost
* C# compiler generated types for lambdas, `async`/`await` and `yield return` are mapped to their containing methods for the purpose of filtering by method name or method level attributes
* F# compiler generated types for lambdas, nested named functions and computation expressions are mapped to their containing methods (and their containing methods, if relevant) for the purpose of filtering by method name or method level attributes so that a filter at any level will be picked up by deeply nested inner functions
* Even more feedback on error, including logging exception detail to file.
* [BUGFIX] Mono.Cecil can give `ArgumentException` when given an arbitrary file input (while detecting which files are instrumentable assemblies); handle that case

# 3.0.404 (Caba series release 4)
* Exclusion by attribute on a class also extends to all nested classes, including invisible ones like lambda expressions and relatives.
* Runner mode (and the `AltCover.Collect` task) now outputs coverage summaries (class, method, point and, where available, branch)
* `-l|lcovReport` option in `runner` mode (parameter `LcovReport` for the `AltCover.Collect` task) to also output the result in lcov format

# 3.0.400 (Caba series release 3)
* [BUGFIX] -- malformed filter regexes no longer cause a crash, but are reported as normal errors
* F# auto-properties are now omitted from coverage, just as C# ones are (and have been since before v1.0.101)
* [BUGFIX] -- errors during the instrumentation or collection phases are actually reported
* The main AltCover assembly also now publishes MSBuild tasks `AltCover.Prepare` and `AltCover.Collect`; the former is the normal mode with `--opencover --save --inplace` as default, the latter is `runner` mode with `--collect` as default.  The full parameter lists are
```
InputDirectory
OutputDirectory
SymbolDirectories†
Keys†⁋
StrongNameKey⁋
XmlReport
FileFilter†
AssemblyFilter†
AssemblyExcludeFilter†
TypeFilter†
MethodFilter†
AttributeFilter†
CallContext†
OpenCover‡
InPlace‡
Save‡
CommandLine
```
where these parameters are all optional strings (default empty) except as noted
† = optional array of strings, default empty
‡ = Boolean, default `true`
⁋ = Mono/.net Framework build only

and
```
RecorderDirectory⸸
WorkingDirectory
Executable

CommandLine
```
with all these parameters being optional strings (default empty) except as noted
⸸ denotes a `[Required]` parameter

The task parameters match the command line arguments in name and function, except that `SymbolDirectories` is pluralised, and `CommandLine` is everything after a `--`.  If `AltCover.Collect`'s `Executable` parameter is set, that switches the virtual `--collect` flag off.


# 3.0.388 (Caba series release 2)
* Improved command line error reporting
* `--inplace` option to instrument the assemblies in the input directory, having saved them to the output directory first; this avoids the manual copy-back step for `dotnet test` and the like
* `--save` option in instrumenting mode plus `--collect` in `runner` mode; `--save` sets the instrumented assemblies to record coverage to disk, just as the `runner` mode does; then after doing whatever is required with the instrumented code, `runner --collect` will process the output just as if the operations had been run from within `AltCover runner`.  Note that if `--collect` is set, any arguments after a `-- ` are ignored.

# 3.0.367-pre (Caba series release 1)
* [BREAKING CHANGE] -- packaging the .net core as binaries -- so `dotnet AltCover.dll` rather than `dotnet run altcover.core.fsproj` to invoke.  This will be needed to do the MSBuild integration that's in the pipeline, where it isn't for a simple command-line tool
* [BUGFIX] -- calculate branch exit visit count for `--opencover` (an oversight in 2.0.360)
---
# 2.0.360   (Byakko series release 7)
* Branch coverage for `--opencover`.  Note that AltCover takes a rather more conservative approach to detecting "significant" branch points than does OpenCover : it excludes all branches that are entirely internal to a reported sequence point in the source (thus eliminating the many and various sorts of compiler-surprise hidden branching that may include sequence points with no corresponding source location); additionally, if two or more exits from a branch go to the same place, they are considered to constitute only one branch-point in the report, rather than being counted separately.
* Bonus feature -- `-c|callContext` tracking also applies to branch visits
* Also take the latest VS2017 and FAKE updates for building

# 2.0.354   (Byakko series release 6)
No functional changes this release, just updates of consumed components and tooling
* Take the Cecil 0.10.0 final release, having worked around the issues with unit testing in .net core with NUnit3TestAdapter 3.10, which links the beta6 version, which has a subset of the internal API present in the full release.
* Take the latest VS2017 and FAKE updates for building

# 2.0.350  (Byakko series release 5)
* `-c|callContext` option to track (when collecting the coverage data in "runner" mode) what led to a particular line of code being visited -- either by being called from a method of a specified name or with a specified attribute (unit testing, in the main), or by time of visit (which would be more appropriate for system testing).  Methods (typically, unit tests) may be tracked even if they are in assemblies that are excluded from instrumentation via the `-e`command line parameter.

# 2.0.330  (Byakko series release 4)
* "y|symbolDirectory=" option to provide other locations to find symbols for the instrumented assemblies.  The search order goes -- location in the debug header, co-located (.pdb then .mdb), then supplied locations tested in order for .pdb then .mdb

# 2.0.324  (Byakko series release 3)
* Option `--opencover` now generates values for MethodPoint tags when collecting the coverage data in "runner" mode
* When a process is launched from altcover, the command line is now echoed to stdout
* Build changes : Visual Studio 15.6.1, and latest FAKE 5 beta releases (beta 24); modified the full-framework builds to emit portable .pdb files on Mono, so the travis-ci tests now get coverage for the framework build
* [BUGFIX] altcover.core.fsproj will now build if the Platform is set (previously only `AnyCPU`, explicitly or by default, would work)
* Other x86 related information [added to the wiki](https://github.com/SteveGilham/altcover/wiki/x86-support-for-.net-core)

# 2.0.300 (Byakko series release 2)
* [BUGFIX] OpenCover format output now works correctly when classes or methods are filtered out of the coverage
* Option `--opencover` now generates cyclomatic complexity values while generating the report file
* Option `--opencover` now generates roll-up statistics "visitedSequencePoints", "visitedMethods", "visitedClasses", "sequenceCoverage" in Summary tags, and "visited", "sequenceCoverage" in Method tags when collecting the coverage data in "runner" mode

# 2.0.273 (Byakko series release 1)
* Option `--opencover` to output coverage data in a sub-set of the OpenCover format (sufficient for use with ReportGenerator and coveralls.io, at least)
* [BUGFIX] AltCover now exits with the exit code of any process it launches.  In particular if the runner mode is used to launch unit tests, and not all tests pass, then the failure code will be propagated.  Otherwise 0 for success and 255 for any other error.
* Moved to FAKE 5 (`dotnet fake`) for build, and related streamlining of the build scripts
---
# 1.6 (Araiguma respin)
* Remove the last vestiges of the pipe-based solution, some redundant code in the non-runner case that did nothing (but waste cycles) in the RC.

# 1.6-rc (Araiguma respin release candidates)
* Moved to FAKE 5 (classic) for build and related streamlining of the build scripts
* Performance tuning for the coverage gathering (trade off of async+backlog vs strict synchronous gathering) -- observed faster than OpenCover on like-for-like tests
* Fixed an intermittent spin-wait and fail in the unit tests
* Removed obsolete code that catered to Mono.Cecil 0.9.5 limitations

# 1.6-beta (Araiguma respin preview)
* simplified concurrency handling for the data collection.
* reduced size of the intermediate files (to under 5% of the previous sizes for large runs)
* **NOTE** this breaks compatibility between previously instrumented code and the new runner (and vice versa); in general, only same-version components are supported.
* The AltCover.Runner helper program in 1.5 used named pipes, collecting data from the instrumented process as it ran; this was strongly platform dependent and brittle -- and was not compatible in the classic framework with the .net 2 support.
* Named pipes eliminated in favour of writing to a file and post-processing
* Mechanism compatible with .net 2.0 so made available everywhere
* separate .net core only runner program eliminated in favour of an alternate command-line interface for the main AltCover

# 1.5-rc (Araiguma release candidates)
* AltCover.Runner helper program for collecting coverage data for .net core runs, avoiding the need to get everything written out during the ProcessExit handling window.
* various refactorings to support this, but no user-visible changes.

# 1.4-beta (Araiguma preview releases)
* "e|assemblyExcludeFilter=" option to exclude assemblies which depend on instrumented/rewritten ones
* Speed-up in writing out of the instrumentation results which happens in the ProcessExit handling and thus has a limited processing time (mostly affects instrumented code running under `dotnet test`)
* UNFIXED : earlier [f61f951] BUGFIX Write .mdb out for .mdb in -- as Mono.Cecil 0.10 will only write `.pdb` files on .net Framework on Windows, and only `.mdb` anywhere else, including .net core on Windows
* validation of the code on Linux using travis-ci, both using Mono and the full framework, and .net core.  Note that there is an apparent limitation in that F# projects don't generate `.pdb` (or `.mdb`) files under Mono, even when C# projects do, thus they cannot yet be instrumented.
* reorganised directory structure in the .nuget package, with AltCover.exe moving from `tools/` to `tools/net45/` with
* .net core 2.0 support : both the original .net framework build being able to inject instrumentation into `dotnet`-built code, but also a .net core 2.0 tool version (delivered as source to `dotnet run` via the `altcover.core.sln` in `tools/netcoreapp2.0/`) that will also instrument both full-framework and .net core code, with the limitation that this build cannot use strong-naming of assemblies.

# 1.4-alpha (Araiguma preview releases)
* .net 2.0 support : Building the recorder assembly against .net 2.0 so that it can be used with down-version code as required
* Extended filtering : values are extended from being single substring for matching to a semi-colon separated list of regexes; the only backwards incompatibility will be '.' characters which would need to be escaped.

# 1.0
* Expanded user documentation
* Localizable user messages
* More consistent command line parsing
* [f61f951] BUGFIX Write .mdb out for .mdb in
* All times in UTC for consistency
---
# 0.8
Single functional change : redefining the -f parameter to work on source files rather than redundantly on assemblies.
Otherwise, procedural changes only:
* Integrating with real-world publishing e.g. NuGet
* Adding many assertable unit tests to the "if it all hangs together, then it passes" operational tests.
* Fixing the bugs thus revealed

# 0.1
Getting the 2010-11 timescale code from a big blob labelled "projects" and into a stand-alone deployable.    
