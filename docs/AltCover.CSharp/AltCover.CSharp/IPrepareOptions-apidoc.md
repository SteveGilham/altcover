# IPrepareOptions interface

This type defines the Prepare (instrumentation) behaviour. The properties map on to the command line arguments for `altcover`

```csharp
public interface IPrepareOptions
```

## Members

| name | description |
| --- | --- |
| [AssemblyExcludeFilter](IPrepareOptions/AssemblyExcludeFilter-apidoc) { get; } | Assembly names to exclude from instrumentation (linked to instrumented assemblies) |
| [AssemblyFilter](IPrepareOptions/AssemblyFilter-apidoc) { get; } | Assembly names to exclude from instrumentation (linked by instrumented assemblies) |
| [AttributeFilter](IPrepareOptions/AttributeFilter-apidoc) { get; } | Attribute names to exclude from instrumentation |
| [BranchCover](IPrepareOptions/BranchCover-apidoc) { get; } | Indicate whether to omit ine coverage. Implies, and is compatible with, the -ReportFormat "opencover" option. Incompatible with `-LineCover`. |
| [CallContext](IPrepareOptions/CallContext-apidoc) { get; } | Tracking either times of visits in ticks or designated method calls leading to the visits. |
| [CommandLine](IPrepareOptions/CommandLine-apidoc) { get; } | Command and arguments for a launched process |
| [Defer](IPrepareOptions/Defer-apidoc) { get; } | Indicate whether to defer writing runner-mode coverage data until process exit. |
| [Dependencies](IPrepareOptions/Dependencies-apidoc) { get; } | Assembly paths to resolve missing references. |
| [ExposeReturnCode](IPrepareOptions/ExposeReturnCode-apidoc) { get; } | Gets whether to report any non-zero return code from a launched process. |
| [FileFilter](IPrepareOptions/FileFilter-apidoc) { get; } | Source file names to exclude from instrumentation |
| [InPlace](IPrepareOptions/InPlace-apidoc) { get; } | Indicate whether to instrument the inputDirectory, rather than the outputDirectory (e.g. for `dotnet test`) |
| [InputDirectories](IPrepareOptions/InputDirectories-apidoc) { get; } | Folder or folders containing assemblies to instrument (default: current directory) |
| [Keys](IPrepareOptions/Keys-apidoc) { get; } | Strong name key or keys that were used to sign the inputs |
| [LineCover](IPrepareOptions/LineCover-apidoc) { get; } | Indicate whether to omit branch coverage. Implies, and is compatible with, the -ReportFormat "opencover" option. Incompatible with `-BranchCover`. |
| [LocalSource](IPrepareOptions/LocalSource-apidoc) { get; } | Indicate whether to exclude for instrumentation code for which the source file is not present. |
| [MethodFilter](IPrepareOptions/MethodFilter-apidoc) { get; } | Method names to exclude from instrumentation |
| [MethodPoint](IPrepareOptions/MethodPoint-apidoc) { get; } | Indicate whether to record only whether a method has been visited or not. Overrides the `LineCover` and `BranchCover` options. |
| [OutputDirectories](IPrepareOptions/OutputDirectories-apidoc) { get; } | Folder or folders to receive the instrumented assemblies and their companions (default: sub-folder `__Instrumented` of the current directory; or `__Saved` if `-Inplace` is set). |
| [PathFilter](IPrepareOptions/PathFilter-apidoc) { get; } | Source file paths to exclude from instrumentation |
| [ReportFormat](IPrepareOptions/ReportFormat-apidoc) { get; } | Generate the report in the specified format (NCover or the default OpenCover) |
| [Save](IPrepareOptions/Save-apidoc) { get; } | Indicate whether to write raw coverage data to file for later processing |
| [ShowGenerated](IPrepareOptions/ShowGenerated-apidoc) { get; } | Indicate whether to mark generated code with a visit count of -2 (Automatic) for the Visualizer if unvisited |
| [ShowStatic](IPrepareOptions/ShowStatic-apidoc) { get; } | Indicate whether to instrument and show code that is by default skipped as trivial. |
| [SingleVisit](IPrepareOptions/SingleVisit-apidoc) { get; } | Indicate whether to record only the first hit at any location. Incompatible with `-CallContext`. |
| [SourceLink](IPrepareOptions/SourceLink-apidoc) { get; } | Indicate whether to display sourcelink URLs rather than file paths if present. |
| [StrongNameKey](IPrepareOptions/StrongNameKey-apidoc) { get; } | The default strong naming key to apply to instrumented assemblies |
| [SymbolDirectories](IPrepareOptions/SymbolDirectories-apidoc) { get; } | Additional folder or folders to search for matching symbols for the assemblies in the input directory |
| [TypeFilter](IPrepareOptions/TypeFilter-apidoc) { get; } | Type names to exclude from instrumentation |
| [VisibleBranches](IPrepareOptions/VisibleBranches-apidoc) { get; } | Indicate whether to hide complex internal IL branching implementation details in switch/match constructs, and just show what the source level logic implies. |
| [XmlReport](IPrepareOptions/XmlReport-apidoc) { get; } | The output report template file (default: coverage.xml in the current directory) |
| [ZipFile](IPrepareOptions/ZipFile-apidoc) { get; } | Indicate whether to emit the XML report inside a zip archive. |
| [ToOptions](IPrepareOptions/ToOptions-apidoc)() | Returns the F#-defined equivalent type |
| [Validate](IPrepareOptions/Validate-apidoc)() | Check whether the parameters are sensible |
| [WhatIf](IPrepareOptions/WhatIf-apidoc)() | Performs validation (implies `Validate`) suitable for a PowerShell `-WhatIf` query; the return type has form |

## See Also

* namespace [AltCover.CSharp](../AltCover.CSharp-apidoc)

<!-- DO NOT EDIT: generated by xmldocmd for AltCover.CSharp.dll -->
