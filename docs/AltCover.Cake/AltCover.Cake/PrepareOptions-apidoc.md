# PrepareOptions class

Preparation stage options; override defaults as required

```csharp
public class PrepareOptions
```

## Public Members

| name | description |
| --- | --- |
| [PrepareOptions](PrepareOptions/PrepareOptions-apidoc)() | The default constructor. |
| virtual [AssemblyExcludeFilter](PrepareOptions/AssemblyExcludeFilter-apidoc) { get; } | Corresponds to command line option `-e, --assemblyExcludeFilter=VALUE` |
| virtual [AssemblyFilter](PrepareOptions/AssemblyFilter-apidoc) { get; } | Corresponds to command line option `-s, --assemblyFilter=VALUE` |
| virtual [AttributeFilter](PrepareOptions/AttributeFilter-apidoc) { get; } | Corresponds to command line option `-a, --attributeFilter=VALUE` |
| virtual [AttributeTopLevel](PrepareOptions/AttributeTopLevel-apidoc) { get; } | Corresponds to command line option `--attributetoplevel=VALUE` |
| virtual [BranchCover](PrepareOptions/BranchCover-apidoc) { get; } | Corresponds to command line option `--branchcover` |
| virtual [CallContext](PrepareOptions/CallContext-apidoc) { get; } | Corresponds to command line option `-c, --callContext=VALUE` |
| [CommandLine](PrepareOptions/CommandLine-apidoc) { get; } | Gets the command line (overridden by the MSBuild integration) |
| [Defer](PrepareOptions/Defer-apidoc) { get; } | Gets whether to defer coverage reporting (overridden by the MSBuild integration) |
| virtual [Dependencies](PrepareOptions/Dependencies-apidoc) { get; } | Corresponds to command line option `-d, --dependency=VALUE` |
| [ExposeReturnCode](PrepareOptions/ExposeReturnCode-apidoc) { get; } | Gets whether to expose a program return code (overridden by the MSBuild integration) |
| virtual [FileFilter](PrepareOptions/FileFilter-apidoc) { get; } | Corresponds to command line option `-f, --fileFilter=VALUE` |
| virtual [InPlace](PrepareOptions/InPlace-apidoc) { get; } | Corresponds to command line option `--inplace` |
| [InputDirectories](PrepareOptions/InputDirectories-apidoc) { get; } | Gets the input directories (overridden by the MSBuild integration) |
| virtual [Keys](PrepareOptions/Keys-apidoc) { get; } | Corresponds to command line option `-k, --key=VALUE` |
| virtual [LineCover](PrepareOptions/LineCover-apidoc) { get; } | Corresponds to command line option `--linecover` |
| virtual [LocalSource](PrepareOptions/LocalSource-apidoc) { get; } | Corresponds to command line option `-l, --localSource` |
| virtual [MethodFilter](PrepareOptions/MethodFilter-apidoc) { get; } | Corresponds to command line option `-m, --methodFilter=VALUE` |
| virtual [MethodPoint](PrepareOptions/MethodPoint-apidoc) { get; } | Corresponds to command line option `--methodpoint` |
| virtual [MethodTopLevel](PrepareOptions/MethodTopLevel-apidoc) { get; } | Corresponds to command line option `--methodtoplevel=VALUE` |
| [OutputDirectories](PrepareOptions/OutputDirectories-apidoc) { get; } | Gets the output directories (overridden by the MSBuild integration) |
| virtual [OutputRoot](PrepareOptions/OutputRoot-apidoc) { get; } | Corresponds to dotnet test option `/p:AltCoverOutputRoot` |
| virtual [PathFilter](PrepareOptions/PathFilter-apidoc) { get; } | Corresponds to command line option `-p, --pathFilter=VALUE` |
| virtual [Portable](PrepareOptions/Portable-apidoc) { get; } | Corresponds to command line option `--portable` |
| virtual [Report](PrepareOptions/Report-apidoc) { get; } | Gets or sets the value that corresponds to command line option `-r, --report=VALUE` |
| virtual [ReportFormat](PrepareOptions/ReportFormat-apidoc) { get; } | Corresponds to command line option `--reportFormat=VALUE` |
| virtual [Save](PrepareOptions/Save-apidoc) { get; } | Corresponds to command line option `--save` |
| virtual [ShowGenerated](PrepareOptions/ShowGenerated-apidoc) { get; } | Corresponds to command line option `--showGenerated` |
| virtual [ShowStatic](PrepareOptions/ShowStatic-apidoc) { get; } | Corresponds to command line option `--showstatic[=VALUE]` |
| virtual [SingleVisit](PrepareOptions/SingleVisit-apidoc) { get; } | Corresponds to command line option `--single` |
| virtual [SourceLink](PrepareOptions/SourceLink-apidoc) { get; } | Corresponds to command line option `--sourcelink` |
| virtual [StrongNameKey](PrepareOptions/StrongNameKey-apidoc) { get; } | Corresponds to command line option `--sn, --strongNameKey=VALUE` |
| virtual [SymbolDirectories](PrepareOptions/SymbolDirectories-apidoc) { get; } | Corresponds to command line option `-y, --symbolDirectory=VALUE` |
| virtual [Trivia](PrepareOptions/Trivia-apidoc) { get; } | Corresponds to command line option `--trivia` |
| virtual [TypeFilter](PrepareOptions/TypeFilter-apidoc) { get; } | Corresponds to command line option `-t, --typeFilter=VALUE` |
| virtual [TypeTopLevel](PrepareOptions/TypeTopLevel-apidoc) { get; } | Corresponds to command line option `--typetoplevel=VALUE` |
| virtual [Verbosity](PrepareOptions/Verbosity-apidoc) { get; } | Corresponds to command line option `-q` |
| virtual [VisibleBranches](PrepareOptions/VisibleBranches-apidoc) { get; } | Corresponds to command line option `-v, --visibleBranches` |
| virtual [ZipFile](PrepareOptions/ZipFile-apidoc) { get; } | Corresponds to command line option `--zipfile` |

## See Also

* namespace [AltCover.Cake](../AltCover.Cake-apidoc)

<!-- DO NOT EDIT: generated by xmldocmd for AltCover.Cake.dll -->
