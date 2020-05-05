# CSApi.IPrepareOptions interface

This type defines the Prepare (instrumentation) behaviour. The properties map on to the command line arguments for `altcover`

```csharp
public interface IPrepareOptions
```

## Members

| name | description |
| --- | --- |
| [AssemblyExcludeFilter](CSApi.IPrepareOptions/AssemblyExcludeFilter-apidoc) { get; } | Assembly names to exclude from instrumentation (linked to instrumented assemblies) |
| [AssemblyFilter](CSApi.IPrepareOptions/AssemblyFilter-apidoc) { get; } | Assembly names to exclude from instrumentation (linked by instrumented assemblies) |
| [AttributeFilter](CSApi.IPrepareOptions/AttributeFilter-apidoc) { get; } | Attribute names to exclude from instrumentation |
| [BranchCover](CSApi.IPrepareOptions/BranchCover-apidoc) { get; } | Indicate whether to omit ine coverage. Implies, and is compatible with, the -ReportFormat "opencover" option. Incompatible with `-LineCover`. |
| [CallContext](CSApi.IPrepareOptions/CallContext-apidoc) { get; } | Tracking either times of visits in ticks or designated method calls leading to the visits. |
| [CommandLine](CSApi.IPrepareOptions/CommandLine-apidoc) { get; } | Command and arguments for a launched process |
| [Defer](CSApi.IPrepareOptions/Defer-apidoc) { get; } | Indicate whether to defer writing runner-mode coverage data until process exit. |
| [Dependencies](CSApi.IPrepareOptions/Dependencies-apidoc) { get; } | Assembly paths to resolve missing references. |
| [ExposeReturnCode](CSApi.IPrepareOptions/ExposeReturnCode-apidoc) { get; } | Gets whether to report any non-zero return code from a launched process. |
| [FileFilter](CSApi.IPrepareOptions/FileFilter-apidoc) { get; } | Source file names to exclude from instrumentation |
| [InPlace](CSApi.IPrepareOptions/InPlace-apidoc) { get; } | Indicate whether to instrument the inputDirectory, rather than the outputDirectory (e.g. for `dotnet test`) |
| [InputDirectories](CSApi.IPrepareOptions/InputDirectories-apidoc) { get; } | Folder or folders containing assemblies to instrument (default: current directory) |
| [Keys](CSApi.IPrepareOptions/Keys-apidoc) { get; } | Strong name key or keys that were used to sign the inputs |
| [LineCover](CSApi.IPrepareOptions/LineCover-apidoc) { get; } | Indicate whether to omit branch coverage. Implies, and is compatible with, the -ReportFormat "opencover" option. Incompatible with `-BranchCover`. |
| [LocalSource](CSApi.IPrepareOptions/LocalSource-apidoc) { get; } | Indicate whether to exclude for instrumentation code for which the source file is not present. |
| [MethodFilter](CSApi.IPrepareOptions/MethodFilter-apidoc) { get; } | Method names to exclude from instrumentation |
| [MethodPoint](CSApi.IPrepareOptions/MethodPoint-apidoc) { get; } | Indicate whether to record only whether a method has been visited or not. Overrides the `LineCover` and `BranchCover` options. |
| [OutputDirectories](CSApi.IPrepareOptions/OutputDirectories-apidoc) { get; } | Folder or folders to receive the instrumented assemblies and their companions (default: sub-folder `__Instrumented` of the current directory; or `__Saved` if `-Inplace` is set). |
| [PathFilter](CSApi.IPrepareOptions/PathFilter-apidoc) { get; } | Source file paths to exclude from instrumentation |
| [ReportFormat](CSApi.IPrepareOptions/ReportFormat-apidoc) { get; } | Generate the report in the specified format (NCover or the default OpenCover) |
| [Save](CSApi.IPrepareOptions/Save-apidoc) { get; } | Indicate whether to write raw coverage data to file for later processing |
| [ShowGenerated](CSApi.IPrepareOptions/ShowGenerated-apidoc) { get; } | Indicate whether to mark generated code with a visit count of -2 (Automatic) for the Visualizer if unvisited |
| [ShowStatic](CSApi.IPrepareOptions/ShowStatic-apidoc) { get; } | Indicate whether to instrument and show code that is by default skipped as trivial. |
| [SingleVisit](CSApi.IPrepareOptions/SingleVisit-apidoc) { get; } | Indicate whether to record only the first hit at any location. Incompatible with `-CallContext`. |
| [SourceLink](CSApi.IPrepareOptions/SourceLink-apidoc) { get; } | Indicate whether to display sourcelink URLs rather than file paths if present. |
| [StrongNameKey](CSApi.IPrepareOptions/StrongNameKey-apidoc) { get; } | The default strong naming key to apply to instrumented assemblies |
| [SymbolDirectories](CSApi.IPrepareOptions/SymbolDirectories-apidoc) { get; } | Additional folder or folders to search for matching symbols for the assemblies in the input directory |
| [TypeFilter](CSApi.IPrepareOptions/TypeFilter-apidoc) { get; } | Type names to exclude from instrumentation |
| [VisibleBranches](CSApi.IPrepareOptions/VisibleBranches-apidoc) { get; } | Indicate whether to hide complex internal IL branching implementation details in switch/match constructs, and just show what the source level logic implies. |
| [XmlReport](CSApi.IPrepareOptions/XmlReport-apidoc) { get; } | The output report template file (default: coverage.xml in the current directory) |
| [ZipFile](CSApi.IPrepareOptions/ZipFile-apidoc) { get; } | Indicate whether to emit the XML report inside a zip archive. |
| [ToOptions](CSApi.IPrepareOptions/ToOptions-apidoc)() | Returns the F#-defined equivalent type |
| [Validate](CSApi.IPrepareOptions/Validate-apidoc)() | Check whether the parameters are sensible |
| [WhatIf](CSApi.IPrepareOptions/WhatIf-apidoc)() | Performs validation (implies `Validate`) suitable for a PowerShell `-WhatIf` query; the return type has form |

## See Also

* class [CSApi](CSApi-apidoc)
* namespace [AltCover](../AltCover.CSApi-apidoc)

<!-- DO NOT EDIT: generated by xmldocmd for AltCover.CSApi.dll -->
