Q. Never mind the fluff -- how do I get started?

A. Start with the Quick Start guide : https://github.com/SteveGilham/altcover/wiki/QuickStart-Guide

# 7.0.7xx (Genbu series release 2)
* [BUGFIX] Set the `visited` attribute and CRAP score accordingly for methods with no sequence points, if the method is recorded as having been visted at all.
* [BUGFIX] Exclude CRAP score for methods with no sequence points from the min/max summary values for the containing type and above (just as such methods aren't counted for the other roll-up quantities)

# 7.0.773 (Genbu series release 2)
* [BUGFIX] Compute CRAP score for unvisited methods too (and add it to the higher level summary items)
* Add `TypeSafe.Filters.Join` to compute the concatenation of a `TypeSafe.FilterItems seq` to an existing `TypeSafe.Filters` instance.  Strict SemVer would bump the minor version at this point, but given the short time since the initial 7.0 release, I doubt the dust has settled fully yet.

# 7.0.770 (Genbu series release 1)
* There are functional changes (fixes and enhancements from the previous pre-release)
* [BUGFIX] Don't automatically exclude C# inner functions (and other `[CompilerGenerated]`) from coverage.
* [BREAKING; Command Line] Option `--defer` now takes no arguments; there is no use-case for the `--defer:-` so that and the redundant `--defer:+` are gone
* [BREAKING; MSBuild tasks] `AltCover.Prepare.Defer` defaults to `true`; in `dotnet test`, as the value is overridden (effectively forced true), this has no effect.
* [BREAKING; PowerShell] rename and generalise `Format-FromCoverletOpenCover` as `Write-OpenCoverDerivedState`
* [BREAKING; API] Rationalise and rename across most parts of the API, above and beyond the pre-release; remove `CSApi` as a separate assembly, with the main `AltCover` assembly publishing that part of the C# callable API, and split `FSApi` into `Toolkit` for PowerShell support and `DotNet` for `dotnet test` support.
* Add `--attributetoplevel`, `--typetoplevel`  and `--methodtoplevel` command line options (and equivalents) to allow inner classes or functions to be included in coverage independently their containing class or function.
* Revise/update/fix the Wiki API pages, help-text and the like
* Autogenerate more of the documentation

# 7.0.766-pre (Genbu series release pre-1)

* There should not be any functional changes before a full 7.0 release; the pre-release is just to ensure I'm happy with the API before freezing it for 7.x
* [BREAKING] Replace the `--opencover` command line argument and its equivalents with `--reportFormat=...` defaulting to `OpenCover` and currently also accepting `NCover`
* [BREAKING; dotnet tool] Remove the deprecated `altcover.dotnet` package with the old-style `DotNetCliToolReference` Tool
* [BREAKING; API] Rename the `AltCover_Fake` namespace to `AltCoverFake`, because it's simpler that way
* [BREAKING; API] Remove obsolete APIs previously marked as `[Obsolete]`
* [BREAKING; API] Rationalise and rename across most parts of the API
* [BREAKING; API, PowerShell] PowerShell cmdlets and the supporting API are now all `XDocument` based
* [BREAKING; PowerShell] Make the `Invoke-AltCover -ShowStatic` parameter take only the typesafe `enum`
* [Documentation] Completely revise the API documentation in [the wiki](https://github.com/SteveGilham/altcover/wiki)
* [PowerShell]`Add-Accelerator` and `Get-Accelerator` cmdlets to write and read new type accelerators e.g. `[xdoc]` for `System.Xml.Linq.XDocument`
*  `--zipfile` command line option (and equivalents) to put the coverage report into a `.zip` archive
* `--methodpoint` command line option (and equivalents) to restrict visit reporting to just the method level
* extend `--threshold` command line option (and equivalents) to allow minimum branch or method coverage, and maximum CRAP score
* [Global tool] `TargetsPath` command line option for the global `altcover` tool to report where the associated `.targets` file is located (as it can't be `dotnet add`ed to a project)


For previous releases (6.x.x and earlier) [go here](https://github.com/SteveGilham/altcover/blob/master/ReleaseNotes%20-%20Previously.md)