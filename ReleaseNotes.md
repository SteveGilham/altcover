**Expect slower/bugfix-only releases for the Summer**

Q. Never mind the fluff -- how do I get started?

A. Start with the Quick Start guide : https://github.com/SteveGilham/altcover/wiki/QuickStart-Guide

# 3.5.xxx (Cerulean series release 9)
* `AltCover.Visualizer` tool for .net framework and mono (for .net framework, needs GTK#2.12.xx installed separately -- see https://www.mono-project.com/download/stable/)

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

For previous releases (3.0.x and earlier) [go here](https://github.com/SteveGilham/altcover/blob/master/ReleaseNotes%20-%20Previously.md)