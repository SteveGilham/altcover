Q. Never mind the fluff -- how do I get started?

A. Start with the Quick Start guide : https://github.com/SteveGilham/altcover/wiki/QuickStart-Guide

# 5.3.675 (Ezoguma series release 6)
* In the `Invoke-AltCover -Runner` cmdlet, send the summary text (OpenCover and/or TeamCity style) to the object pipeline as well as to the `Information` channel.
* Option `--defer[:[+|-]]` (default `--defer:-`) (`-Defer` for PowerShell; Defer property on MSBuild task; API field) to keep all the visit data in memory and only write the total out during process exit
* Automatic defer, with in-process data collection, for `dotnet test` to work around the 100ms limitation on `ProcessExit` handling imposed by the VSTest system that led to the "write everything to disk" runner mode in the first place
* Other throughput improvements for data collection

# 5.2.667 (Ezoguma series release 5)
* [BUGFIX] Expose the sourcelink functionality through `dotnet test` as well (`/p:AltCoverSourceLink=true`)
* [BUGFIX] Update help text to current
* [BUGFIX] Fix the long-standing case where two or more branches of a switch go to the same location so that all of them go through the instrumentation
* `/p:AltCoverShowSummary=colour` to echo summary data to stdout in the colour of choice (default: current foreground if `colour` is not a valid `ConsoleColor` case-insensitive name e.g. `"true"`), and equivalent API extensions
* `--teamcity[]:[+][R|B]]` to put an additional (`+`) or replacement teamcity summary, with branches as teamcity branches with `R` or blocks `B` (which. last I tried, was the value to actually show up in the build report); if no optionas are attached, `B` is understood.  Exposed via API field `SummaryFormat`, PowerShell `-SummaryFormat`, `dotnet test` argument `/p:AltCoverSummaryFormat=...`

# 5.1.666 (Ezoguma series release 4)
* `--dropReturnCode` ( `-DropReturnCode` in PowerShell, `ExposeReturnCode` with default value `true` in the Fake API) to not pass the return code of any nested process
* `--sourcelink` to give the source link URL for tracked files rather than the file path (untracked files still have the local file path)
* Visualizer support for sourcelink URLs
* Generate Cobertura to the v4 DTD [http://cobertura.sourceforge.net/xml/coverage-04.dtd](https://github.com/cobertura/cobertura/blob/master/cobertura/src/site/htdocs/xml/coverage-04.dtd)

# 5.0.665 (Ezoguma series release 3)
* [BUGFIX] Restore visualizer support for OpenCover format (internal consistency check failure)
* [BUGFIX] Issue #52 -- fix OpenCover, LCov and Cobertura format output in the case of exclusion by path
* [BUGFIX] Issue #50 -- take Cecil 0.10.3 with the actual fix in it
* Some improvements to the throughput of coverage data, reducing time taken by the instrumented self-tests.

# 5.0.664 (Ezoguma series release 2)
* [BUGFIX] Issue #49 -- `dotnet test` integration : internally, escape the '\' character, which is  is helpfully treated by MSBuild as a path separator and flipped to be '/' on non-Windows platforms when introduced through `/p:AltCover*Filter` arguments.
* [BUGFIX] Issue #48 -- fix embedded-PDB detection to avoid false positives
* Updating consumed libraries and related changes.

# 5.0.663 (Ezoguma series release 1)
Bringing gifts, as is appropriate for the season
* [BUGFIX] Issue #46 -- handle the case of a left-behind `__Saved` directories by failing in a more obvious fashion (and offering a `/p:AltCoverForce=true` option to force-delete such a directory)
* Support instrumenting assemblies with embedded PDBs
  * [BREAKING] the `XUnit` assemblies have embedded PDBs, so will suddenly be caught up in instrumentation without a `-e xunit` or equivalent to exclude them
* [BREAKING] Complete API overhaul to properly address known problems and to try to future-proof everything against any similar issues -- see the Wiki [here for in-process execution](https://github.com/SteveGilham/altcover/wiki/The-AltCover-API,-plus-Fake-and-Cake-integration) and [here for FAKE scripting](https://github.com/SteveGilham/altcover/wiki/The-AltCover.Fake-package)

For previous releases (4.0.x and earlier) [go here](https://github.com/SteveGilham/altcover/blob/master/ReleaseNotes%20-%20Previously.md)