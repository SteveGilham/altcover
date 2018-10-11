Q. Never mind the fluff -- how do I get started?

A. Start with the Quick Start guide : https://github.com/SteveGilham/altcover/wiki/QuickStart-Guide

# 4.0.649 (Doruka series release 6)
* [BUGFIX] Issue #37 -- handle release builds of C# `return <ternary>;` expressions which don't look like ternaries in their IL.  Fix some corner cases of NPath complexity, branch exit counts and branch visit counts revealed by these cases.
**NOTE** This form of ternary expression is the first case of a significant user defined branch within a sequence point; the `Compress-Branching -WithinSequencePoint` cmdlet at this release _will_ flatten these constructs.
* [BUGFIX] Accept coverlet's idiosyncratic OpenCover-style output into cmdlet and Visualizer operations -- it uses Boolean.ToString() at points, which generates capitalized `True` and `False` attribute values, which are not valid XSD `boolean`s
* [BUGFIX] Issue #38 -- in some .net framework cases, the strong-naming of the recorder assembly was broken; an approach that testing shows to be more reliable has been used.
* [API] Update to FAKE 5.8.5 and validate support from 5.0 up to current

# 4.0.644 (Doruka series release 5)
* [API] - FAKE 5.7.2 support -- FAKE integration now spans from v5.0 up to current (and should be reasonably future-proofed), and is now also offered on an experimental basis for the .net framework
* various refactorings and rearrangements, some on purely aesthetic grounds, some to improve the reliability of the travis-ci build, but with no functional impact

# 4.0.630 (Doruka series release 4)
* Add SourceLink to the build process (and the .nuspec)
* [BUGFIX] Work around an apparent change in behaviour in mono 5.14 that loses some recorded coverage in runner mode; the problem does not affect .net framework or .net core, but does impact those travis build self-tests that aren't on .net core
* [BUGFIX] Fix localization packaging in .netcore 2.1.4xx
* [BUGFIX] Source scrolling in the global tool version of the visualizer
* [API] Expose all cmdlet functionality as APIs and run the PowerShell cmdlets through the public API
* [Visualizer] add the XML document as a root of the tree view, and the current source file (if any) to the application title bar; clear source pane when new coverage file is selected.

# 4.0.618 (Doruka series release 3)
* [NEW PACKAGE] `altcover.visualizer` containing .net core/GTK3 Visualizer global tool implementation
  * .net core/GTK3 Visualizer also contained in the general-purpose and the API packages for direct `dotnet path/to/AltCover.Visualizer.dll` use
  * needs GTK+3 installed separately -- for Windows, see e.g. https://github.com/GtkSharp/GtkSharp/wiki/Installing-Gtk-on-Windows
* Improved error messages for the GTK# Visualizer
* Improved font handling for the GTK# Visualizer (now it updates immediately)
* [API] strong-name keys can now be meaningfully validated from .net core
* Minor improvements to reliability on mono

# 4.0.603 (Doruka series release 2)
* [BUGFIX] PowerShell and GetVersion tasks might produce empty output in 4.0.600 : now fixed
* [BUGFIX] pack the pwsh support into the API module (omission in 4.0.600)
* [API] Defaults provided for CSApi types `CollectArgs` and `PrepareArgs` equivalent to the F# defaults
* [API] `PrepareParams.Vaildate : unit -> string array`; and `CollectParams.Validate : bool -> string array` to do read-only parameter validation
* [API] The equivalent `public string[] PrepareArgs()` and `public string[] CollectArgs(bool afterPreparation)` for the CSApi types

# 4.0.600 (Doruka series release 1)
* [NEW PACKAGE] AltCover.Api exposing the shared API used by both the MSBuild tasks and the PowerShell `Invoke-AltCover` cmdlet, in native F# and with a C#-friendly adapter layer
  * Also included, integrations with Fake ( >= 5.2.0) and Cake ( >= 0.28.0), each in their separate assembly, to avoid any need to drag in unwanted extra dependencies
  * The PowerShell module and the MSBuild tasks
  * And the `dotnet test` integration
* [ALL PACKAGES] `Compress-Branching` cmdlet to transform OpenCover's liberal idea of significant branches towards AltCover's more restricted approach -- chose either or both of `-SameSpan` to unify branches that go from the same start, and take the same trajectory to the same end (OpenCover issue #786 being one instance of this) and `-WithinSequencePoint` to remove branches interior to a statement (compiler generated things like stashing of lambdas, the hidden conditional `Dispose()` after a `using`, or inside F# inlines -- OpenCover issues #657, #807 being instances of this)
* [BUGFIX] Issue # 31 -- In the case of `dotnet test` with multiple target frameworks make the coverage file `name.extension` go to `name.framework.extension`, be it supplied or be it defaulted.

For previous releases (3.5.x and earlier) [go here](https://github.com/SteveGilham/altcover/blob/master/ReleaseNotes%20-%20Previously.md)