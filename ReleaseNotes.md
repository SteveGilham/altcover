# 1.4-beta (Araiguma preview releases)
* "e|assemblyExcludeFilter=" option to exclude assemblies which depend on instrumented/rewritten ones
* Speed-up in writing out of the instrumentation results which happens in the ProcessExit handling and thus has a limited processing time (mostly affects instrumented code running under `dotnet test`)
* UNFIXED : earlier [f61f951] BUGFIX Write .mdb out for .mdb in -- as Mono 0.10 will only write `.pdb` files on .net Framework on Windows, and only `.mdb` anywhere else, including .net core on Windows
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
