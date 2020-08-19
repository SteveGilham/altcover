Q. Never mind the fluff -- how do I get started?

A. Start with the Quick Start guide : https://github.com/SteveGilham/altcover/wiki/QuickStart-Guide

# 7.1.xxx (Genbu series release 7)
* As well as interfaces, hide other types with no non-abstract methods (e.g. plain enums) in the coverage
Also
* Rationalise .net versions to help speed up the build and ease the net5.0 transition
  * Clear out some corner case differences between .net core and .net framework builds based on old work-arounds for symbol writing for the instrumented files
  * Build the recorder at `net20` only and use the same assembly everywhere
  * Move all the core logic from `AltCover.exe/.dll` to `AltCover.Engine.dll`
  * Unify the three different entry-point assembly instances into the now shim-like `AltCover.exe/.dll`
  * Build everything against `netstandard2.0` except executable shims and unit tests
  * Build `AltCover.exe/.dll` against `net472` for framework support, `netcoreapp2.1` for the global tool and `netcoreapp2.0` for everywhere else
  * Build the GTK2 visualizer against `net472` for consistency
  * `net472` debug builds for published libraries are retained purely for FxCop consumption
* Collect coverage from unit tests at build time too

# 7.1.783  (Genbu series release 6a)
* [Visualizer-global-tool] 
  * [BUGFIX] Don't NRE when cancelling a File Open dialog when Avalonia uses its GTK binding (Linux)
  * Support font selection on Windows natively (monospace fonts only)
  * On non-Windows platforms, if Tcl/Tk `wish` is present, use that to perform font selection (choose wisely)

# 7.1.782  (Genbu series release 6)
* [BUGFIX] Don't throw NRE when encountering an interface with a default method implementation
* Omit interfaces without default method implementations from coverage recording, to match the behaviour of OpenCover (Issue #91)
* [Visualizer-global-tool] 
  * Fix the display of branch information on the second and subsequent coverage file loaded in a session
  * Fix the loading of the most recently accessed files list to prune ones that don't now exist
  * other minor tidyings
* [Visualizer-GTK] 
  * Make this look more like the Avalonia version
  * Tentative fix for the `About` dialog link-button on non-Windows platforms based partly on the Avalonia code.
  * Keep the GTK3 build in step, even though it's not packaged for release

# 7.1.780  (Genbu series release 5a)
* [Visualizer] Rewritten global tool based on the cross-platform AvaloniaUI toolkit (so no need for all the GTK3 set-up, including the `--schemadir[=path]` command-line parameter)
  * There's no font selection support yet as AvaloniaUI doesn't offer a cross-platform one
  * The colour scheme differs as there's not yet support for selecting a different background brush for text ranges -- covered = blue text
  * line numbers are shown, and are coloured according to any sequence point starting that line

# 7.1.778 (Genbu series release 5)
* [BUGFIX] Address problems revealed in issue #87
  * The collection process now fails gracefully if the XML report is missing or broken
  * `dotnet test` will halt after instrumentation fails, should it do so, rather than continuing to test and process the missing or broken XML report
  * Work round F# issue 9255 by replacing Newtonsoft as the JSON processor with one that can static link

# 7.1.777 (Genbu series release 4)
* [BUGFIX] Fix the GTK2/Framework Visualizer (broken since v6.8.761)
* Multi-monitor support for the Visualizer in all versions; the window will restore to its previous location even when placed on non-primary monitors
* Use integrated MSBuild error reporting in the data collector [used by the `dotnet test` integration](https://github.com/SteveGilham/altcover/wiki/The-AltCover-data-collector-and-%60dotnet-test%60).

# 7.1.776 (Genbu series release 3)
* [BUGFIX] Set the `visited` attribute and CRAP score accordingly for methods with no sequence points, if the method is recorded as having been visted at all.
* [BUGFIX] Exclude CRAP score for methods with no sequence points from the min/max summary values for the containing type and above (just as such methods aren't counted for the other roll-up quantities)
* Emit CRAP score values in the OpenCover style summary
* Distinguish between methods with source and no source in the threshold computations, the option to select the alternative measures (AM, AC) that include the no-source methods.  Extend the `TypeSafe.Thresholds` record type with the coresponding  extra fields.
* On a threshold violation, always report that via return code by preference to any non-zero process value.

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