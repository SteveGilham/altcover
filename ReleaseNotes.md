**NOTE:** The main executable still links the Mono.Cecil 0.10-beta7 version, as the 0.10 final version causes issues with some build tools that haven't caught up with events.  The binary injected into the instrumented code does not link Cecil at all.  Until the wrinkles are ironed out or worked around, I make this disclaimer instead.

# 2.0.350
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

# 0.8
Single functional change : redefining the -f parameter to work on source files rather than redundantly on assemblies.
Otherwise, procedural changes only:
* Integrating with real-world publishing e.g. NuGet
* Adding many assertable unit tests to the "if it all hangs together, then it passes" operational tests.
* Fixing the bugs thus revealed

# 0.1
Getting the 2010-11 timescale code from a big blob labelled "projects" and into a stand-alone deployable.    
