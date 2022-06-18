Q. Never mind the fluff -- how do I get started?

A. Start with the Quick Start guide : https://github.com/SteveGilham/altcover/wiki/QuickStart-Guide

# 8.2.xxx (Habu series release 14)
* [BUGFIX] -- Fake API fix for ZipFile, MethodPoint, SingleVisit, SourceLink to enable their activation
* [BUGFIX] -- work around the behaviour of `dotnet test` with an argument ending `.dll` or `.exe`, directly through API and in docs for direct use.

# 8.2.837 (Habu series release 13)
* [VISUALIZER] Critical bug fixes for GTK and Avalonia : mismatched new icon names causing crashes
* [VISUALIZER] Use new coverage data icon for the coverage report file, not a generic report icon

# 8.2.836 (Habu series release 12)
* [BUGFIX] Fix project file path handling in Cake support
* [BUGFIX] Improve heuristic to recognise (and ignore)  state-machine injected branches in `async` methods; this may cause subtle changes, up or down, in branch detection for other generated code ("hidden" sequence points) contexts with branches
* Use VS2022 icons in the Visualizer

# 8.2.835 (Habu series release 11)
* [BUGFIX] Fix tracked methods in the `Merge-OpenCover` cmdlet
* [BUGFIX] Fix blocking issue in Cake support; and add operational test
* Move Fake.Build support to 5.21 as minimum; updating other supporting libraries

# 8.2.833 (Habu series release 10)
* [BUGFIX] Work-round more (problems with ill-formed debug data)[https://github.com/jbevain/cecil/issues/816] (issue #135)
* [BUGFIX] An infinite loop while instrumenting when faced with an inner function that is also a closure (also issue #135)
* [BUGFIX] Address issue #71 by pre-allocating storage for each instrumented assembly; rather than allocating on demand, with any timing related issues not adequately dealt with subject to catch-and-ignore
* For instrumented assemblies, write embedded debug symbols, independent of the input choice.
* Improved release note formatting
* Some overhaul and updating of neglected parts of the build and test script
* Reduce to a minimum the differences in the source between the net20 and net46-for-async versions of the recorder; then just rewrite the net20 version with the delta on demand.
* Some minor improvements to the data collector for `dotnet test` use, with example of how to employ explicitly in AltCover "classic" mode in the "`UnitTestWithAltCoverCore`" fake build target

# 8.2.831 (Habu series release 9)
* [BUGFIX] Make the static-linked parts of the recorder assembly internal, so only the AltCover instrumentation API is exposed -- removes type duplications from the environment that may confuse run-time assembly creation e.g. by Marten (issue #133)
* For `CallContext`, add async-aware tracking for all methods returning `Task` or `Task<T>` not just ones with the C# `async` shape.  This includes functions returning the new F#6 `task{}` computation expression. 
  * **Note** there is a support gap : running this under .net Framework less than v4.6 (i.e. a test machine with a pre-2015-JUL-20 environment) will not work, even though `Task` is a .net 4.0 feature, as the tracking support relies on .net 4.6 `async` features.
* Emit (a warning)[https://stevegilham.github.io/altcover/AltCover.Engine/AltCover/AltCover.LoggingOptions/Warn-apidoc] if any of the files input to the instrumentations have previously been instrumented, leaving those files untouched (apart from any Recorder assembly, which will still be overwritten).  For `dotnet test` this is an MSBuild warning, for powershell, it is delivered via `Cmdlet.WriteWarning`.
  * This behaviour is subject to change in future releases
* Rework of the build process to keep artifacts out of the tracked directories

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
* [BUGFIX] issue #125 -- prevent an NullReferenceException in some cases of computing cyclomatic complexity (a failure to exactly copy the algorithm from Mono.Gendarme)
* [ENHANCEMENT; API] issue #126 -- further generalise the relative-directory support for `CopyAlways`/`CopyIfNewer` from v7.4; extends the `ContingentCopy` MSBuild task

⁋For previous releases (8.1.x and earlier) go here -- [https://github.com/SteveGilham/altcover/blob/master/ReleaseNotes%20-%20Previously.md](https://github.com/SteveGilham/altcover/blob/master/ReleaseNotes%20-%20Previously.md)