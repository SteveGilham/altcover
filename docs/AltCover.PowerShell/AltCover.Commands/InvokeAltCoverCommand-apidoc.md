# InvokeAltCoverCommand class

The equivalent of the `AltCover` command or `altcover` global tool.

The arguments parallel the command-line executable; the main difference is that `-Runner` is a switch parameter that selects the runner-mode parameter set if present.

The "working directory" used where mentioned in the parameter descriptions is the current set location in PowerShell, **_not_** the underlying current directory.

**Note**: As Powershell informational output is suppressed by default in PowerShell 5+, the `-InformationAction Continue` option is needed to show the progress and summary information for the process if this is desired.

Summary information is also written to the object pipeline.

**Note**: `-WhatIf` includes validation for the command line arguments. It is ignored for the purely read-only `-Version` option

```csharp
Invoke-AltCover -XmlReport $x -OutputDirectory  $o -InputDirectory $i -AssemblyFilter "Adapter" -ReportFormat NCover -InformationAction Continue
```

```csharp
public class InvokeAltCoverCommand : PSCmdlet
```

## Public Members

| name | description |
| --- | --- |
| [InvokeAltCoverCommand](InvokeAltCoverCommand/InvokeAltCoverCommand-apidoc)() | The default constructor. |
| [AssemblyExcludeFilter](InvokeAltCoverCommand/AssemblyExcludeFilter-apidoc) { get; set; } | Assembly names to exclude from instrumentation (linked to instrumented assemblies) |
| [AssemblyFilter](InvokeAltCoverCommand/AssemblyFilter-apidoc) { get; set; } | Assembly names to exclude from instrumentation (linked by instrumented assemblies) |
| [AttributeFilter](InvokeAltCoverCommand/AttributeFilter-apidoc) { get; set; } | Attribute names to exclude from instrumentation |
| [AttributeTopLevel](InvokeAltCoverCommand/AttributeTopLevel-apidoc) { get; set; } | Attributes to mark a type as "top level" |
| [BranchCover](InvokeAltCoverCommand/BranchCover-apidoc) { get; set; } | Do not record line coverage. Implies, and is compatible with, the -ReportFormat "opencover" option. Incompatible with `-LineCover`. |
| [CallContext](InvokeAltCoverCommand/CallContext-apidoc) { get; set; } | Tracking either times of visits in ticks or designated method calls leading to the visits. |
| [Cobertura](InvokeAltCoverCommand/Cobertura-apidoc) { get; set; } | File path for Cobertura format version of the collected data |
| [CommandLine](InvokeAltCoverCommand/CommandLine-apidoc) { get; set; } | Arguments for a launched process |
| [Defer](InvokeAltCoverCommand/Defer-apidoc) { get; set; } | Defers writing runner-mode coverage data until process exit. |
| [Dependency](InvokeAltCoverCommand/Dependency-apidoc) { get; set; } | Assembly paths to resolve missing references. |
| [DropReturnCode](InvokeAltCoverCommand/DropReturnCode-apidoc) { get; set; } | Do not report any non-zero return code from a launched process. |
| [Executable](InvokeAltCoverCommand/Executable-apidoc) { get; set; } | The executable to run e.g. `dotnet` |
| [FileFilter](InvokeAltCoverCommand/FileFilter-apidoc) { get; set; } | Source file names to exclude from instrumentation |
| [InPlace](InvokeAltCoverCommand/InPlace-apidoc) { get; set; } | Instrument the inputDirectory, rather than the outputDirectory (e.g. for `dotnet test`) |
| [InputDirectory](InvokeAltCoverCommand/InputDirectory-apidoc) { get; set; } | Folder or folders containing assemblies to instrument (default: current directory) |
| [JsonReport](InvokeAltCoverCommand/JsonReport-apidoc) { get; set; } | File path for JSON format version of the collected data |
| [Key](InvokeAltCoverCommand/Key-apidoc) { get; set; } | Strong name key or keys that were used to sign the inputs |
| [LcovReport](InvokeAltCoverCommand/LcovReport-apidoc) { get; set; } | File path for lcov format version of the collected data |
| [LineCover](InvokeAltCoverCommand/LineCover-apidoc) { get; set; } | Do not record branch coverage. Implies, and is compatible with, the -ReportFormat "opencover" option. Incompatible with `-BranchCover`. |
| [LocalSource](InvokeAltCoverCommand/LocalSource-apidoc) { get; set; } | Don't instrument code for which the source file is not present. |
| [MethodFilter](InvokeAltCoverCommand/MethodFilter-apidoc) { get; set; } | Method names to exclude from instrumentation |
| [MethodPoint](InvokeAltCoverCommand/MethodPoint-apidoc) { get; set; } | Record only whether a method has been visited or not. Overrides the `-LineCover` and `-BranchCover` options. |
| [MethodTopLevel](InvokeAltCoverCommand/MethodTopLevel-apidoc) { get; set; } | Names to mark a function as "top level" |
| [OutputDirectory](InvokeAltCoverCommand/OutputDirectory-apidoc) { get; set; } | Folder or folders to receive the instrumented assemblies and their companions (default: sub-folder `__Instrumented` of the current directory; or `__Saved` if `-Inplace` is set). |
| [OutputFile](InvokeAltCoverCommand/OutputFile-apidoc) { get; set; } | Write the recorded coverage to this file rather than overwriting the original report file. |
| [PathFilter](InvokeAltCoverCommand/PathFilter-apidoc) { get; set; } | Source file paths to exclude from instrumentation |
| [RecorderDirectory](InvokeAltCoverCommand/RecorderDirectory-apidoc) { get; set; } | The folder containing the instrumented code to monitor (including the `AltCover.Recorder.g.dll` generated by previous a use of the .net core `AltCover`). |
| [ReportFormat](InvokeAltCoverCommand/ReportFormat-apidoc) { get; set; } | Generate the report in the specified format (NCover or the default OpenCover) |
| [Runner](InvokeAltCoverCommand/Runner-apidoc) { get; set; } | Selects `Runner` mode |
| [Save](InvokeAltCoverCommand/Save-apidoc) { get; set; } | Write raw coverage data to file for later processing |
| [ShowGenerated](InvokeAltCoverCommand/ShowGenerated-apidoc) { get; set; } | Mark generated code with a visit count of -2 (Automatic) for the Visualizer if unvisited |
| [ShowStatic](InvokeAltCoverCommand/ShowStatic-apidoc) { get; set; } | Instrument and show code that is by default skipped as trivial. |
| [Single](InvokeAltCoverCommand/Single-apidoc) { get; set; } | only record the first hit at any location (or first for that context if `-CallContext` is operating). |
| [SourceLink](InvokeAltCoverCommand/SourceLink-apidoc) { get; set; } | Display sourcelink URLs rather than file paths if present. |
| [StrongNameKey](InvokeAltCoverCommand/StrongNameKey-apidoc) { get; set; } | The default strong naming key to apply to instrumented assemblies |
| [SummaryFormat](InvokeAltCoverCommand/SummaryFormat-apidoc) { get; set; } | Selects summary format |
| [SymbolDirectory](InvokeAltCoverCommand/SymbolDirectory-apidoc) { get; set; } | Additional folder or folders to search for matching symbols for the assemblies in the input directory |
| [Threshold](InvokeAltCoverCommand/Threshold-apidoc) { get; set; } | One or more of minimum acceptable statement (S), branch (B) or method (M)/alternative method (AM) coverage percentage (integer, 1 to 100) or maximum acceptable CRAP/alternative CRAP score (C/AC followed by integer, 1 to 255) e.g. M80AM70C40AC100B50. If the value starts with a number, a leading S is assumed. If any threshold is specified more than once, the last instance is assumed -- so 25S50 counts as S50. Zero/absent values are ignored. If a coverage result is below threshold, or the CRAP score is above threshold, the return code of the process is the largest abs(threshold - actual) rounded up to the nearest integer. |
| [TypeFilter](InvokeAltCoverCommand/TypeFilter-apidoc) { get; set; } | Type names to exclude from instrumentation |
| [TypeTopLevel](InvokeAltCoverCommand/TypeTopLevel-apidoc) { get; set; } | Names to mark a type as "top level" |
| [Verbosity](InvokeAltCoverCommand/Verbosity-apidoc) { get; set; } | Selects output level of the command |
| [Version](InvokeAltCoverCommand/Version-apidoc) { get; set; } | Selects `Version` mode |
| [VisibleBranches](InvokeAltCoverCommand/VisibleBranches-apidoc) { get; set; } | Hide complex internal IL branching implementation details in switch/match constructs, and just show what the source level logic implies. |
| [WorkingDirectory](InvokeAltCoverCommand/WorkingDirectory-apidoc) { get; set; } | The working directory for the application launch |
| [XmlReport](InvokeAltCoverCommand/XmlReport-apidoc) { get; set; } | The output report template file (default: coverage.xml in the current directory) |
| [ZipFile](InvokeAltCoverCommand/ZipFile-apidoc) { get; set; } | Emit the XML report inside a zip archive. |
| override [ProcessRecord](InvokeAltCoverCommand/ProcessRecord-apidoc)() | Perform the `AltCover` operation |

## See Also

* namespace [AltCover.Commands](../AltCover.PowerShell-apidoc)

<!-- DO NOT EDIT: generated by xmldocmd for AltCover.PowerShell.dll -->
