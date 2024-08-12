Q. Never mind the fluff -- how do I get started?

A. Start with the Quick Start guide : https://github.com/SteveGilham/altcover/wiki/QuickStart-Guide and 
read the FAQ : https://github.com/SteveGilham/altcover/wiki/FAQ

# (Habu series release 33)
* [ADVISORY] The Fake.build related assemblies (in the `altcover.api` and `altcover.fake` packages) support Fake 6.1.0

# 8.8.173 (Habu series release 32)
* [PERFORMANCE] Issue #227 - removing the slow-down observed the new (at 8.8.165) file name processing for Cobertura 
* [PERFORMANCE] removing a surprising hot-spot in branch coverage instrumentation that was taking 60% of the whole instrumentation time

# 8.8.165 (Habu series release 31)
* [ADVISORY] the Fake.build related assemblies (in the `altcover.api` and `altcover.fake` packages), and the Avalonia 0.10-based visualizer, rely on components with known vulnerabilities. The Fake.build project appears nigh-moribund so has not released an update, whereas Avalonia 11 completely rewrites all the earlier APIs and has not documented anything to assist in the rewrite of the application.
* [BUGFIX] Issue #197 - correctly split file paths in the Cobertura output
* [NET9 preparation] Recode the recorder into C# as compiler/build target changes in F#9 make maintaining net2.0 compatibility in F# too much bother.

# 8.8.74 (Habu series release 30)
* [BUGFIX] Issue #222 - distinguish methods differing only in number of generic parameters (JSON and cobertura in particular, but with small changes for all all output formats)
* [BUGFIX] Issue #223 - handle degenerate source paths for Cobertura output

# 8.8.53 (Habu series release 29)
* [BUGFIX] Fix summary data for `--outputFile` option
* [BUGFIX] Fix interaction of `--zipFile` prepare option and `--outputFile` collect option
* [BUGFIX] Issue #220 - improve dependency resolution to the GAC

# 8.8.21 (Habu series release 28)
* [BREAKING; BUGFIX] Issue #206 : Update to net6+ for `dotnet test` integration and respect the `$(IsTestProject)` setting from the `Microsoft.NET.Test.Sdk` package.
* Simplify the use of the AltCover MSBuild tasks via the associated package-level `.targets` file by not even including the `VSTest` integration unless both `'$(AltCover)' == 'true' AND '$(IsTestProject)' == 'true'`.
* Mitigate instances of `System.IO.IOException: The process cannot access the file '[coverage report]' because it is being used by another process.`
* Explicitly add GAC locations to the paths inspected for dependency resolution

# 8.8.10 (Habu series release 27)
* [BUGFIX] Add `Json` member to the report format enumerations for the typesafe API and for the `InvokeAltCover` cmdlet.
* [BUGFIX] Issue #214 : patch Mono.Cecil to use FIPS compliant algorithm
* [Enhancement] Discussion #206, maybe also Issue #203 : Option `--portable` and equivalent APIs to place the coverage report file and related coverage data in the same folder as the recorder assembly, wherever that might be, allowing the whole instrumented folder structure to be moved into another file structure (e.g. different machine, different OS). 

# 8.7.3 (Habu series release 26)
* [Enhancement] [Discussion 202](https://github.com/SteveGilham/altcover/discussions/202) : More careful tidying of temporary `.runsettings` files, fixing long-standing errors of both commission and omission.
* [Enhancement] [Discussion 199](https://github.com/SteveGilham/altcover/discussions/199) : Add `/p:AltCoverOutputRoot=[path]` and associated APIs for `dotnet test` command line creation.  The `[path]` is a directory to be used instead of `$(TargetDir)` for the relative placing of the instrumented or saved files.  The use-case here is when `$(TargetDir)` is close to `MAX_PATH` and the generated sub-folders would overflow that limit.

⁋For previous releases (8.6.125 and earlier) go here -- [https://github.com/SteveGilham/altcover/blob/master/ReleaseNotes%20-%20Previously.md](https://github.com/SteveGilham/altcover/blob/master/ReleaseNotes%20-%20Previously.md)