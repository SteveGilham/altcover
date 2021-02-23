Q. Never mind the fluff -- how do I get started?

A. Start with the Quick Start guide : https://github.com/SteveGilham/altcover/wiki/QuickStart-Guide

# 8.1.8xx (Habu series release 2)
* `Merge-OpenCover` cmdlet and `OpenCover.Merge` API.  It should handle both strict (`OpenCover`, `AltCover --reportFormat=OpenCover`) and more relaxed (`coverlet`, `ConvertFrom-CoverageJson`, `Write-OpenCoverDerivedState -Coverlet`) interpretations

# 8.0.816 (Habu series release 2)
* Move to Cake 1.0 as baseline for Cake support
* [VISUALIZER] Support for LCov and Cobertura format reports
* [VISUALIZER] For formats with only line-level information (e.g. LCov, Covertura or from coverlet), colour the whole line, and not just the line number gutter

# 8.0.815 (Habu series release 1)
* [BUGFIX] Issue 122 -- rework the method name tokenization for extracting the `returnType (argumentList)` signature values in the Cobertura output, fixing an off-by-one error that generated `returnType argumentList)` without the `(` as well as the headline exception.
* [NEW] Native JSON report formatting (`--reportFormat=Json` or equivalents), a superset of coverlet's JSON
  * AltCover `classic` mode -- just running the instrumented code and collecting results in the `ProcessExit` handler -- is not supported with `--reportFormat=Json`
  * `ConvertFrom-CoverageJson` cmdlet to convert from coverlet or AltCover JSON to a miminal OpenCover format
  * Preparing as Native JSON, to generate an LCov or Cobertura report at collection is supported
* [VISUALIZER] Both versions will now consume and display from coverlet and AltCover JSON output
* [BREAKING] the `-x, --xmlReport` argument or equivalent becomes just `-r, --report` since not all reports are XML
* [BREAKING] the stop-gap `--jsonReport` collection option from v7.6 is withdrawn, and the related `ConvertTo-CoverageJson` cmdlet now produces the AltCover native JSON format
* For both LCov and Cobertura output, coalesce cases of multiple sequence points per line into one entry per line
* Extensions to coverlet's JSON format are as follows
  * `Method` has optional fields
    * `SeqPnts` (array of `SeqPnt`) 
    * `TId` (integer tracking ID) 
    * `Entry` and
    * `Exit` (arrays of timestamps)
  * `BranchInfo` has optional fields
   * `Id` (integer unique ID)
   * `Times` (array of timestamps) and
   * `Tracks` (array of tracking IDs)
  * `SeqPnt` is `VC` (visit count), `SL` (start line), `SC` (start column), `EL`, `EC` (ditto for end), `Offset`, `Id`, all integers, and optional `Times` and `Tracks` as for `BranchInfo`
  * Because int64 doesn't fit as a double, tracking-related timestamps are represented as Base64Encoded strings of the ticks count as a network byte order quantity `Convert.ToBase64String(BitConverter.GetBytes(IPAddresss.HostToNetworkOrder(ticks)))`

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

For previous releases (7.4.x and earlier) [go here](https://github.com/SteveGilham/altcover/blob/master/ReleaseNotes%20-%20Previously.md)