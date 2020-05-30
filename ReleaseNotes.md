Q. Never mind the fluff -- how do I get started?

A. Start with the Quick Start guide : https://github.com/SteveGilham/altcover/wiki/QuickStart-Guide

# 7.0.xxx (Genbu series release 1)
* There are functional changes (fixes and enhancements from the previous pre-release)
* [BUGFIX] Don't automatically exclude C# inner functions (and other `[CompilerGenerated]`) from coverage.
* [BREAKING; Command Line] Option `--defer` now takes no arguments; there is no use-case for the `--defer:-` so that and the redundant `--defer:+` are gone
* [BREAKING; MSBuild tasks] `AltCover.Prepare.Defer` defaults to `true`; in `dotnet test`, as the value is overridden (effectively forced true), this has no effect.
* [BREAKING; PowerShell] rename and generalise `Format-FromCoverletOpenCover` as `Write-OpenCoverDerivedState`
* [BREAKING; API] Rationalise and rename across most parts of the API, above and beyond the pre-release; remove `CSApi` as a separate assembly, with the main `AltCover` assembly publishing that part of the C# callable API, and split `FSApi` into `Toolkit` for PowerShell support and `DotNet` for `dotnet test` support.
* Add `--attributetoplevel`, `--typetoplevel`  and `--methodtoplevel`  to allow inner classes or functions to be included in coverage independently their containing class or function.
* Revise/update/fix the Wiki API pages, help-text and the like

# 7.0.766-pre (Genbu series release pre-1)

* There should not be any functional changes before a full 7.0 release; the pre-release is just to ensure I'm happy with the API before freezing it for 7.x
* [BREAKING] Replace the `--opencover` command line argument and its equivalents with `--reportFormat=...` defaulting to `OpenCover` and currently also accepting `NCover`
* [BREAKING; dotnet tool] Remove the deprecated `altcover.dotnet` package with the old-style `DotNetCliToolReference` Tool
* [BREAKING; API] Rename the `AltCover_Fake` namespace to `AltCoverFake`, because it's simpler that way
* [BREAKING; API] Remove obsolete APIs previously marked as obsolete
* [BREAKING; API] Rationalise and rename across most parts of the API
* [BREAKING; API, PowerShell] PowerShell cmdlets and the supporting API are now all `XDocument` based
* [BREAKING; PowerShell] Make the `Invoke-AltCover -ShowStatic` parameter take pnly the typesafe `enum`
* [Documentation] Completely revise the API documentation in [the wiki](https://github.com/SteveGilham/altcover/wiki)
* [PowerShell]`Add-Accelerator` and `Get-Accelerator` cmdlets to write and read new type accelerators e.g. `[xdoc]` for `System.Xml.Linq.XDocument`
*  `--zipfile` command line option (and equivalents) to put the coverage report into a `.zip` archive
* `--methodpoint` command line option (and equivalents) to restrict visit reporting to just the method level
* extend `--threshold` to allow minimum branch or method coverage, and maximum CRAP score
* [Global tool] `TargetsPath` command line option for the global `altcover` tool to report where the associated `.targets` file is located (as it can't be `dotnet add`ed to a project)


For previous releases (6.x.x and earlier) [go here](https://github.com/SteveGilham/altcover/blob/master/ReleaseNotes%20-%20Previously.md)