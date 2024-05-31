Q. Never mind the fluff -- how do I get started?

A. Start with the Quick Start guide : https://github.com/SteveGilham/altcover/wiki/QuickStart-Guide and 
read the FAQ : https://github.com/SteveGilham/altcover/wiki/FAQ

# (Habu series release 31)

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