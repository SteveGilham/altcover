Q. Never mind the fluff -- how do I get started?

A. Start with the Quick Start guide : https://github.com/SteveGilham/altcover/wiki/QuickStart-Guide

# 7.6.8xx (Genbu series release 16)
* "Native" JSON ...

* JSON output shake down (some changes possibly breaking)
  * Reduce allocations
  * Escape strings more rigorously
  * Method metadata tokens are kept numeric
  * Because int64 doesn't fit as a double, tracking-related timestamps represented as Base64Encoded strings `Convert.ToBase64String(BitConverter.GetBytes(ticks))`

# 7.6.812 (Genbu series release 15)
* [VISUALIZER] Move the global tool to the new 0.10 AvaloniaUI release
* Monitor API
  * [BUGFIX] Harden the monitor API `TryGetVisitTotals` against race conditions in multi-threaded tests
  * Publish the AltCover.Monitor API as API (i.e. under `lib/`) in the main package `altcover` as well as in `altcover.api` (but not in `altcover.global`; global tools aren't library compatible to be accessed through a `package add` reference).  It's there next to the PowerShell assembly (per `altcover ImportModule`) if you want to manually link to it, though
  * Support writing unit tests involving the API back to `net20` as well as `netstandard2.0`
* Add `--jsonReport` option (and equivalents) to output the NCover or OpenCover data in a minified JSON format, like the existing `--lcovReport` option does for that format.  The JSON is a direct map of the XML, with values appropriately typed.
* Add a `ConvertTo-CoverageJson` cmdlet and a `ConvertToJson` toolkit API to post-precess existing NCover/OpenCover reports 

# 7.5.809 (Genbu series release 14)
* [NEW] AltCover.Monitor API to track current coverage from running unit tests.  Current implementation requires `dotnet test`, or other command-line testing with `--defer` set, in which the cumulative visit numbers are available, rather than everything having been dumped to file instead.
* [BUGFIX] In OpenCover format output, only emit `<File />` records relevant to the respective module, not for all source files encountered so far.

# 7.4.808 (Genbu series release 13)
* [BUGFIX] In some use cases, the error `The "AltCover.ContingentCopy" task was not given a value for the required parameter "FileName".` could be provoked by `dotnet test` (Issue #113)
* Extend  to other Build Action types (at least all those that my VS2019 Community Edition was prepared to mention) the "If `/p:AltCoverInPlace` is not `true`, then copy all files in the project included as `<[Action] Include="./[some subdirectory]/..."` with `CopyToOutputDirectory` of `Always` or `PreserveNewest` to the appropriate relative location wrt the intrumented files" behaviour added for the `None` action in the previous release.  File an issue report if you have yet another build action type that you need copying for a not-in-place test scenario.

# 7.4.807 (Genbu series release 12)
* [MAYBE BREAKING] Set `InPlace` default to `false` uniformly across the API
  * Add `dotnet test` command line option `/p:AltCoverInPlace=true|false` (default false)
  * If  `/p:AltCoverInPlace=true` then `/p:AltCoverForce=true` has its pre-v7.3.805 meaning
  * Wire up "InPlace" to the Fake `DotNet.test` driver for the above
  * If `/p:AltCoverInPlace` is not `true`, then copy all files in the project included as `<None Include="./[some subdirectory]/..."` with `CopyToOutputDirectory` of `Always` or `PreserveNewest` to the appropriate relative location wrt the intrumented files
  * **NB** `/p:AltCoverInPlace=true` will not play well with the concurrent instrument-and-test behaviour of `dotnet test [multipletestprojects].sln /p:AltCover="true" --output [commonArtifactsFolder]`
* Allow `--callContext` and `--single` together, which will log at most one visit _per context_ per location, not just one visit per location

For previous releases (7.3.x and earlier) [go here](https://github.com/SteveGilham/altcover/blob/master/ReleaseNotes%20-%20Previously.md)