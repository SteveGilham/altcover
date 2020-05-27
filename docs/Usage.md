For use cases, see [Use Cases](https://github.com/SteveGilham/altcover/wiki/Use-Cases).  
For modes of operation, see [Modes of Operation](https://github.com/SteveGilham/altcover/wiki/Modes-of-Operation).  
For driving AltCover from `dotnet test`, see [`dotnet test` integration](%60dotnet-test%60-integration).  
For driving AltCover from MSBuild, see [MSBuild Tasks](MSBuild-tasks).  
For driving AltCover and associated tools with Windows PowerShell or PowerShell Core, see [PowerShell integration](PowerShell-integration).  

The full command line is 
```
AltCover [/i[nputDirectory]=VALUE] [/o[utputDirectory]=VALUE] [/y|symbolDirectory=VALUE] [/d[ependency]=VALUE] [/sn|strongNameKey=VALUE] [/k[ey]=VALUE] [/x[mlReport]=VALUE] [/f[ileFilter]=VALUE] [/p[athFilter]=VALUE] [/s|assemblyFilter=VALUE] [/t|typeFilter=VALUE] [/m|methodFilter=VALUE] [/a|attributeFilter=VALUE] [/l[ocalSource]] [/c[allContext]=VALUE] [--reportFormat=VALUE] [--inplace] [--save] [--zipfile] [--methodpoint] [--single] [--linecover|branchcover] [--dropReturnCode] [--sourceLink] [--defer[:[+|-]]] [/v[isibleBranches]] [--showstatic[=VALUE]] [--showGenerated] [/?|h[elp]] [-- ] [...]
or
AltCover Runner [/r[ecordingDirectory]=VALUE] [/w[orkingDirectory]=VALUE] [/x|executable=VALUE] [--collect] [/l[covReport]=VALUE] [/t[hreshold]=VALUE] [/c[obertura]=VALUE] [/o[utputFile]=VALUE] [--dropReturnCode] [--teamcity[:[+][R|B]]] [/?|h[elp]] [-- ] [...]
or
AltCover ImportModule
or
AltCover Version

```

In detail
```
  -i, --inputDirectory=VALUE Optional, multiple: A folder containing assemblies
                               to instrument (default: current directory)
  -o, --outputDirectory=VALUE
                             Optional, multiple: A folder to receive the
                               instrumented assemblies and their companions (
                               default: sub-folder '__Instrumented' of the
                               current directory; or '__Saved' if '--inplace'
                               is set).
                               See also '--inplace'
  -y, --symbolDirectory=VALUE
                             Optional, multiple: Additional directory to search
                               for matching symbols for the assemblies in the
                               input directory
  -d, --dependency=VALUE     Optional, multiple: assembly path to resolve
                               missing reference.
  -k, --key=VALUE            Optional, multiple: any other strong-name key to
                               use
      --sn, --strongNameKey=VALUE
                             Optional: The default strong naming key to apply
                               to instrumented assemblies (default: None)
  -x, --xmlReport=VALUE      Optional: The output report template file (default:
                                coverage.xml in the current directory)
  -f, --fileFilter=VALUE     Optional, multiple: source file name to exclude
                               from instrumentation
  -p, --pathFilter=VALUE     Optional, multiple: source file path to exclude
                               from instrumentation
  -s, --assemblyFilter=VALUE Optional, multiple: assembly name to exclude from
                               instrumentation
  -e, --assemblyExcludeFilter=VALUE
                             Optional, multiple: assembly which links other
                               instrumented assemblies but for which internal
                               details may be excluded
  -t, --typeFilter=VALUE     Optional, multiple: type name to exclude from
                               instrumentation
  -m, --methodFilter=VALUE   Optional, multiple: method name to exclude from
                               instrumentation
  -a, --attributeFilter=VALUE
                             Optional, multiple: attribute name to exclude from
                               instrumentation
  -l, --localSource          Don't instrument code for which the source file is
                               not present.
  -c, --callContext=VALUE    Optional, multiple: Tracking either times of
                               visits in ticks or designated method calls
                               leading to the visits.
                                   A single digit 0-7 gives the number of
                               decimal places of seconds to report; everything
                               else is at the mercy of the system clock
                               information available through DateTime.UtcNow
                                   A string in brackets "[]" is interpreted as
                               an attribute type name (the trailing "Attribute"
                               is optional), so [Test] or [TestAttribute] will
                               match; if the name contains one or more ".",
                               then it will be matched against the full name of
                               the attribute type.
                                   Other strings are interpreted as method
                               names (fully qualified if the string contains
                               any "." characters).
                                   Incompatible with --single
      --reportFormat=VALUE   Optional: Generate the report in the specified
                               format (NCover or the default OpenCover)
      --inplace              Optional: Instrument the inputDirectory, rather
                               than the outputDirectory (e.g. for dotnet test)
      --save                 Optional: Write raw coverage data to file for
                               later processing
      --zipfile              Optional: Emit the XML report inside a zip archive.
      --methodpoint          Optional: record only whether a method has been
                               visited or not.  Overrides the --linecover and --
                               branchcover options.
      --single               Optional: only record the first hit at any
                               location.
                                   Incompatible with --callContext.
      --linecover            Optional: Do not record branch coverage.  Implies,
                               and is compatible with, the --reportFormat=
                               opencover option.
                                   Incompatible with --branchcover.
      --branchcover          Optional: Do not record line coverage.  Implies,
                               and is compatible with, the --reportFormat=
                               opencover option.
                                   Incompatible with --linecover.
      --dropReturnCode       Optional: Do not report any non-zero return code
                               from a launched process.
      --sourcelink           Optional: Display sourcelink URLs rather than file
                               paths if present.
      --defer[=VALUE]        Optional, defers writing runner-mode coverage data
                               until process exit.
  -v, --visibleBranches      Hide complex internal IL branching implementation
                               details in switch/match constructs, and just
                               show what the source level logic implies.
      --showstatic[=VALUE]   Optional: Instrument and show code that is by
                               default skipped as trivial.  --showstatic:- is
                               equivalent to omitting the parameter; --
                               showstatic or --showstatic:+ sets the unvisited
                               count to a negative value interpreted by the
                               visualizer (but treated as zero by
                               ReportGenerator) ; --showstatic:++ sets the
                               unvisited count to zero.
      --showGenerated        Mark generated code with a visit count of -2 (
                               Automatic) for the Visualizer if unvisited
  -?, --help, -h             Prints out the options.
      -- ...                 Anything on the command line after a free-standing "--" is considered a separate command line to be executed after the instrumentation has been done.
```
or `Runner` plus
```
  -r, --recorderDirectory=VALUE
                             The folder containing the instrumented code to
                               monitor (including the AltCover.Recorder.g.dll
                               generated by previous a use of the .net core
                               AltCover).
  -w, --workingDirectory=VALUE
                             Optional: The working directory for the
                               application launch
  -x, --executable=VALUE     The executable to run e.g. dotnet
      --collect              Optional: Process previously saved raw coverage
                               data, rather than launching a process.
  -l, --lcovReport=VALUE     Optional: File for lcov format version of the
                               collected data
  -t, --threshold=VALUE      Optional: one or more of minimum acceptable
                               statement (S), branch (B) or method (M) coverage
                               percentage (integer, 1 to 100) or maximum
                               acceptable CRAP score (C followed by integer, 1
                               to 255) e.g. M80C40B50. If the value starts with
                               a number, a leading S is assumed. If any
                               threshold is specified more than once, the last
                               instance is assumed -- so 25S50 counts as S50.
                               Zero/absent values are ignored. If a coverage
                               result is below threshold, or the CRAP score is
                               above threshold, the return code of the process
                               is the largest abs(threshold - actual) rounded
                               up to the nearest integer.
  -c, --cobertura=VALUE      Optional: File for Cobertura format version of the
                               collected data
  -o, --outputFile=VALUE     Optional: write the recorded coverage to this file
                               rather than overwriting the original report file.
      --dropReturnCode       Optional: Do not report any non-zero return code
                               from a launched process.
      --teamcity[=VALUE]     Optional: Show summary in TeamCity format as well
                               as/instead of the OpenCover summary
  -?, --help, -h             Prints out the options.
      -- ...                 Anything on the command line after a free-standing "--" is considered arguments for the executable to run.
```
or
```
ImportModule                 Prints out the PowerShell script to import the associated PowerShell module
```
or
```
Version                      Prints out the AltCover build version
```
or, for the global tool only
```
TargetsPath                  Prints out the path to the 'altcover.global.targets' file
                             (as the tool cannot be 'dotnet add'ed to the project).
			     The 'altcover.global.props' file is present in the same directory
```

### Notes

* If the `--localSource` argument is given, the process checks the first source file path that it finds in the debug symbols for each assembly, and if that file is not found on the current machine, the assembly is treated as if the symbols file did not exist.

* The `--dependency` argument will expand environment variables in the paths from release 4.0.653; %USERPROFILE%, %ProgramFiles% and %NUGET_PACKAGES% are likely to be the most useful here, e.g. `%ProgramFiles%/dotnet/shared/Microsoft.AspNetCore.App/2.1.5/Microsoft.AspNetCore.Cryptography.KeyDerivation.dll` or similar to pick up ASP.Net Core assemblies needed for type references

* The `--callContext` argument is only used with `--opencover`, otherwise it has no effect.  Tracked methods are recorded at instrumentation time, but call context information is only recorded in runner mode, including `runner --collect`, which allows for heavier processing after the process terminates than the `ProcessExit` handler.

* In runner mode, exactly one of a command to be executed (`-x`) or the `--collect` option is required.  If `--collect` is supplied then anything after a free-standing `--` is ignored.

* In the `runner -x` case, before the executable is launched AltCover creates a file in the same folder as the previously nominated XML report, and while that file is present, the raw coverage data are written there, rather than being held in memory.  After the executable terminates, the data are read and processed into the XML report.  This file is the same one written by `altcover --save`

* Before v6.0 the strong-name key arguments (`-k`, `--sn`) were not available in the .net core build.  At v6.0, AltCover includes a work-around for what was a netstandard1.3 limitation in Mono.Cecil (suitable APIs weren't there), in anticipation of this being included formally in a future Mono.Cecil 0.11 release with netstandard2.0 support.

* Filter values are semi-colon (`;`) separated regular expressions, applied by type in the order as they are defined in the command line; any item whose name matches the expression will be excluded from the coverage reporting.  In the simplest case, with no special regex items, this means that a name containing the filter item as a sub-string will be excluded.  In v6.0.700 or later, should the need ever arise to have a semi-colon in a regex, then escape it in by doubling (`;;`); if a triplet `;;;` or longer is present, doubling gets grouped from the left.

* Except where being driven by AltCover in "runner" mode, coverage statistics are written to the file nominated by the `x|xmlReport=` parameter as instrumented assemblies are unloaded from an executing AppDomain, even if this is days or weeks later.  In practice the instrumented assemblies should be deleted after the relevant testing has been run, and the report file will thus be freed up.

* valid arguments for `--teamcity` are `B`, `R`, `+B`, `+R` or nothing at all (same as `B`).  The letter indicates which symbol to use in the TeamCity format for branch coverage (`B` is for `Block`, which by experiment did show in the build report, and `R` is for `bRanch` which is documented, but did not show when I tried it), the optional `+` indicates that the OpenCover summary should also be emitted.

* valid arguments for `--defer` are `+`, `-` or nothing at all (same as `+`).  `+` keeps coverage data in memory until process exit, `-` writes promptly to file in runner mode, i.e. acts as in previous releases since 1.6).

* `ipmo` will print a string on Mono/non-Windows, but Windows PowerShell won't be there to make use of it.

#### `-e` vs `-s` : what gives?

In the case where you have Unit Tests => Code Under Test => Libraries, and the coverage of the unit tests is not of interest, then exclude those assemblies with `-e`, so their dependencies get rewritten, but their contents are not instrumented or added to the coverage file.  The stable libraries consumed by the code under test (and stable libraries for the unit test framework) should be marked as `-s` to be left untouched.

#### `-p` vs `-f` : what gives?

The distinction is that a single `-p` can exclude a folder `$(SolutionRoot)Generated` (though it has to be specified with manual expansion of the MSBuild variable) and everything in it, while a single `-f` can be used to exclude all `.g.cs` files wherever they are. 

#### `runner` mode : what gives?

There are three stages to the coverage operation -- instrumenting the code, exercising it, then presenting the results.  Classic `altcover` does the first, and as part of the process, injects the result presentation code into the system under test, to be run when the test process exits (as an exit handler).

This essentially doesn't work with `dotnet test` in .net core (the exit handler doesn't have enough time to work), so the `runner` mode was introduced to wrap the test processing, catching coverage data as it was generated, through a named pipe connection, and then doing the presentation work after the tests completed (release 1.5).  The named pipe implementation proved unreliable as a cross-platform mechanism, so a file-based mechanism was adopted instead (release 1.6) that does work reliably in all places.

With the data being buffered to disk, rather than being stored entirely in memory in one or other process, the `--save` and `runner --collect` options were introduced (release 3.0) to do, respectively, the signalling to the system under test that it should dump live to an intermediate file, and the translation of the intermediate information into the final report, with the testing to be done at some point between, rather than having to run the tests through `altcover runner`.

In release 5.3, the writing the collected data has been offloaded to an in-process data handler, which removes this constraint; the data collector is automatically hooked into the MSBuild process.