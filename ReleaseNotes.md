# 3.5.5xx (Cerulean series release 1)
TODO -- fix appveyor.yml in release branch

# 3.5.500-pre (Cerulean series pre-release)
* Reduce the amount of unnecessary infrastructure runtime code in the .nupkg that bloated the 3.0.488-490 release
* Expose the LCov and Cobertura output formatters as simple cmdlets `ConvertTo-Cobertura` and `ConvertTo-Lcov` to take OpenCover or NCover format input.
* Minor adjustments to the `Invoke-AltCover` cmdlet based on PowerShell guidelines -- plural argument names made singular with backwards-compatible aliases

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

For previous releases (2.x and earlier) [go here](https://github.com/SteveGilham/altcover/blob/master/ReleaseNotes%20-%20Previously.md)