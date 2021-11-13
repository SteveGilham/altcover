Q. Never mind the fluff -- how do I get started?

A. Start with the Quick Start guide : https://github.com/SteveGilham/altcover/wiki/QuickStart-Guide

# 8.2.828 (Habu series release 8)
* Absorb .net 6 release into build tooling (awaiting a non-preview updated release of (`fake.build`)[https://fake.build/] to be able to upgrade to FSharp.Core 6.x -- still have to target the v5.x library even if the build uses the F#6 compiler)
* Where the debug symbols used in the instrumentation contain embedded source (e.g. from source generators), represent that within the generated report --
  * for OpenCover format, as an `altcover.embed` attribute added to the `File` element
  * for NCover classic, `altcover.file` elements are added after the `method` records in a `module`, with attributes `document` and `embed`
  * and for the extended coverlet JSON format, as a type `«AltCover.embed»` with an empty method with name being the embedded text.
* [VISUALIZER] Support the extended formats for display, using the embedded source in preference to the file system whenever present
* Support these extensions in conversions between formats -- conversion to Lcov and Cobertura currently lose this information
* Other fixes/enhancements to these conversions, esp. in the cases of partial classes and inlined code
* Compute summary data correctly in the output from Json to OpenCover
* Carry both recorder versions (net20 and net46) as resources to simplify self-test behaviour
* Move baseline Cake support to v1.1.0 and Fake to v5.20.4

# 8.2.825 (Habu series release 7)
* Next release will be post .net 6 release to accomodate its impact, barring show-stoppers
* [BUGFIX] As noted in [Q&A discussion (#107)](https://github.com/SteveGilham/altcover/discussions/107), satellite assemblies, and in [issue #47](https://github.com/SteveGilham/altcover/issues/47#issuecomment-461838463)  platform specific library subfolders, were not being copied appropriately relative to the instrumented location; this is now resolved.
* [BUGFIX] Prevent `--localSource` possibly excluding locally built assemblies using source generators.
* [VISUALIZER] Fixes and updates
  * On the global tool, don't put expander icons (˃) on leaf nodes, i.e. most methods, all source -- such icons are an automatic feature in GTK, but are manual in Avalonia.
  * Don't throw while trying to determine if a file that doesn't exist is outdated

# 8.2.824 (Habu series release 6c)
* [VISUALIZER] Fixes and updates
  * [REGRESSION] Fix where multiple source file support broke JSON coverage support
  * Not all coverage reports are XML -- fix root node icon
  * Add more icons indicating non-default states (files missing, changed, via sourcelink &c)
  * Replace most of the annoying pop-ups with tool-tips

# 8.2.823 (Habu series release 6b)
* [VISUALIZER] Fixes and updates
  * [REGRESSION] Fix where multiple source file support broke methods with no source file
  * [BUGFIX] Fix where some types were erroneously shown as functions (ƒₓ)
  * Allow for TAB characters (which occupy 1 column only in the .PDB), and display → rather than ◻ in the global tool (TAB expands to 8 spaces in the GTK# build)
  * Update icons to VS2019 from VS2017 (except where taken from GTK in the GTK# build), which means slightly more colours in the tree view, and a small change to the branch indicator.

# 8.2.822 (Habu series release 6a)
* [VISUALIZER] Support OpenCover's output from C++/CLI assemblies compiled `/Zi` (line information only, zero column values)
  * account for (& simplify) the C++/CLI attribute decorations in method names
  * allow for (& simplify) `gcroot<type::with::Cpp::namespacing ^>` types in method names
  * allow source file selection for methods with code inlined from multiple source files

# 8.2.821 (Habu series release 6)
* Support deterministic builds `/p:ContinuousIntegrationBuild=true`, with or without `--sourcelink`/`/p:AltCoverSourceLink=true`.  Note that assemblies created by deterministic builds will be excluded by `-l`/`/p:AltCoverLocalSource=true`.
* Experiment with the ReadMe feature recently added to NuGet
* Internal refactoring of the JSON processing following the replacement of `System.Text.Encodings.Web` in the previous release.

# 8.2.820 (Habu series release 5)
* Replace `System.Text.Encodings.Web` for JSON-escaping module, class and method names 
* [BUGFIX] issue #125 -- prevent an NullReferenceException is some cases of computing cyclomatic complexity (a failure to exactly copy the algorithm from Mono.Gendarme)
* [ENHANCEMENT; API] issue #126 -- further generalise the relative-directory support for `CopyAlways`/`CopyIfNewer` from v7.4; extends the `ContingentCopy` MSBuild task

# 8.1.819 (Habu series release 4)
* Adapt to recent F# compiler optimizations that make function objects static if they don't close over their environment -- properly detect their owner functions for exclusion and for JSON format output
* If the report format is JSON, ensure that the coverage file doesn't end `.xml`, and if not JSON, that it doesn't end `.json` (case-blind comparison)

# 8.1.817 (Habu series release 3)
* `Merge-OpenCover` cmdlet and `OpenCover.Merge` API.  It should handle both strict (`OpenCover`, `AltCover --reportFormat=OpenCover`) and more relaxed (`coverlet`, `ConvertFrom-CoverageJson`, `Write-OpenCoverDerivedState -Coverlet`) interpretations
* When `--callContext` indicates a method returning an F# `async` computation, then track all calls within the same async flow, just as with C# `async` methods from v7.2.800

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

For previous releases (7.4.x and earlier) go here -- [https://github.com/SteveGilham/altcover/blob/master/ReleaseNotes%20-%20Previously.md](https://github.com/SteveGilham/altcover/blob/master/ReleaseNotes%20-%20Previously.md)