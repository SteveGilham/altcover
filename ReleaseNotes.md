Q. Never mind the fluff -- how do I get started?

A. Start with the Quick Start guide : https://github.com/SteveGilham/altcover/wiki/QuickStart-Guide

# 6.1.7xx (Fukurou series release 5)
* [HACK] mitigate Issue #71 by simply ignoring null module identifiers.
* Use a leading `?` as a negator for filter matches e.g. `?(a|b)` means "exclude anything that doesn't match a or match b", or `?MyApp` means exclude anything that doesn't contain `MyApp`; no valid .net regex begins with this so it's backwards compatible.  Between this and the previous release's `--localSource` option, the need to resort to cumbersome constructs involving negative lookahead regexes should be reduced.

# 6.1.708 (Fukurou series release 4)
* [BUGFIX] reinstate the PowerShell Core (`pwsh`) `Invoke-AltCover` support for strongnaming.
* [API] `-l|--localSource` option (bool `LocalSource` default false in API, `-LocalSource` PowerShell flag) to ignore .pdb files that refer to source files not present on the current computer (test is if the first file found exists or not, and assumes that this is all-or-nothing, and assume no coincidences in naming).
---
# 6.0.705 (Fukurou series release 3)
* [BUGFIX] in the case of multiple output folders, properly weave the AltCover recorder assembly dependency into all `dotnet` projects, not just the first.
* [BUGFIX] when using the `dotnet` version of the tools, and when a suitable FSharp.Core package is present in the nuget cache, it is not necessary to copy one from the AltCover deployment to the output folder for a `dotnet` project
* [BUGFIX] create the directory to hold the report file if it does not already exists
* Use Mono.Cecil 0.11 for strongnaming in `dotnet`, removing the local reimplementation of assembly writing with strongnaming.

# 6.0.700 (Fukurou series release 2)
* [BUGFIX] in `dotnet test` the pipe character `|` is used as a separator because the previous choice of `;` didn't play nice with MSBuild.  To escape pipe characters inside regular expressions, double them up `||`.  See the [Usage](https://github.com/SteveGilham/altcover/wiki/Usage) and [`dotnet test`](https://github.com/SteveGilham/altcover/wiki/%60dotnet-test%60-integration) wiki pages for more detail.

# 6.0.698 (Fukurou series release 1)
* [BREAKING] Allow multiple input and output directories for instrumentation into a single report.  This changes the types in API structures from `string` to `string seq` (F#) or `string[]` (C#) where appropriate.  Use case : [instrument multiple unit test assemblies in one go](https://github.com/SteveGilham/altcover/blob/9f6ed07e9d5d1c35d8c99a589fb77fe1868cecab/Build/targets.fsx#L702-L703) for running as [a single test step](https://github.com/SteveGilham/altcover/blob/9f6ed07e9d5d1c35d8c99a589fb77fe1868cecab/Build/targets.fsx#L724-L726) to collect coverage.
* Enable the `--sn` and `-k` arguments in the `dotnet` build.  This doesn't change any APIs, but it does mean that these arguments are no longer ignored for the .net core platform.  This work borrows from changes recently made in Mono.Cecil but not yet in a release build; in the fullness of time, my roll-my-own support will be replaced with the real deal.
* For what it's worth, enable the `-d` argument in the .net framework/Mono build (as the APIs already exposed this, the change is behavioural -- any values supplied are used rather than silently dropped)


For previous releases (5.x.x and earlier) [go here](https://github.com/SteveGilham/altcover/blob/master/ReleaseNotes%20-%20Previously.md)