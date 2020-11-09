# TypeSafe.PrepareOptions class

Command line options for `AltCover`

```csharp
public sealed class PrepareOptions : IEquatable<PrepareOptions>, IStructuralEquatable
```

## Public Members

| name | description |
| --- | --- |
| [PrepareOptions](TypeSafe.PrepareOptions/PrepareOptions-apidoc)(…) |  |
| static [Create](TypeSafe.PrepareOptions/Create-apidoc)() | returns an instance that has all fields unset/default except `ExposeReturnCode`, `InPlace` and `Save` are `Set` |
| [AssemblyExcludeFilter](TypeSafe.PrepareOptions/AssemblyExcludeFilter-apidoc) { get; } | Corresponds to command line option `-e, --assemblyExcludeFilter=VALUE` |
| [AssemblyFilter](TypeSafe.PrepareOptions/AssemblyFilter-apidoc) { get; } | Corresponds to command line option `-s, --assemblyFilter=VALUE` |
| [AttributeFilter](TypeSafe.PrepareOptions/AttributeFilter-apidoc) { get; } | Corresponds to command line option `-a, --attributeFilter=VALUE` |
| [AttributeTopLevel](TypeSafe.PrepareOptions/AttributeTopLevel-apidoc) { get; } | Corresponds to command line option -`-attributetoplevel=VALUE` |
| [BranchCover](TypeSafe.PrepareOptions/BranchCover-apidoc) { get; } | Corresponds to command line option `--branchcover` |
| [CallContext](TypeSafe.PrepareOptions/CallContext-apidoc) { get; } | Corresponds to command line option `-c, --callContext=VALUE` |
| [CommandLine](TypeSafe.PrepareOptions/CommandLine-apidoc) { get; } | Corresponds to the command line to run, given after a `-- ` |
| [Defer](TypeSafe.PrepareOptions/Defer-apidoc) { get; } | Corresponds to command line option `--defer` |
| [Dependencies](TypeSafe.PrepareOptions/Dependencies-apidoc) { get; } | Corresponds to command line option `-d, --dependency=VALUE` |
| [ExposeReturnCode](TypeSafe.PrepareOptions/ExposeReturnCode-apidoc) { get; } | Corresponds to the converse of command line option `--dropReturnCode ` |
| [FileFilter](TypeSafe.PrepareOptions/FileFilter-apidoc) { get; } | Corresponds to command line option `-f, --fileFilter=VALUE` |
| [InPlace](TypeSafe.PrepareOptions/InPlace-apidoc) { get; } | Corresponds to command line option `--inplace` |
| [InputDirectories](TypeSafe.PrepareOptions/InputDirectories-apidoc) { get; } | Corresponds to command line option ` -i, --inputDirectory=VALUE` |
| [Keys](TypeSafe.PrepareOptions/Keys-apidoc) { get; } | Corresponds to command line option ` -k, --key=VALUE` |
| [LineCover](TypeSafe.PrepareOptions/LineCover-apidoc) { get; } | Corresponds to command line option `--linecover` |
| [LocalSource](TypeSafe.PrepareOptions/LocalSource-apidoc) { get; } | Corresponds to command line option `-l, --localSource` |
| [MethodFilter](TypeSafe.PrepareOptions/MethodFilter-apidoc) { get; } | Corresponds to command line option ` -m, --methodFilter=VALUE` |
| [MethodPoint](TypeSafe.PrepareOptions/MethodPoint-apidoc) { get; } | Corresponds to command line option `--methodpoint` |
| [MethodTopLevel](TypeSafe.PrepareOptions/MethodTopLevel-apidoc) { get; } | Corresponds to command line option `--methodtoplevel=VALUE` |
| [OutputDirectories](TypeSafe.PrepareOptions/OutputDirectories-apidoc) { get; } | Corresponds to command line option `-o, --outputDirectory=VALUE` |
| [PathFilter](TypeSafe.PrepareOptions/PathFilter-apidoc) { get; } | Corresponds to command line option `-p, --pathFilter=VALUE` |
| [ReportFormat](TypeSafe.PrepareOptions/ReportFormat-apidoc) { get; } | Corresponds to command line option `--reportFormat=VALUE` |
| [Save](TypeSafe.PrepareOptions/Save-apidoc) { get; } | Corresponds to command line option `--save` |
| [ShowGenerated](TypeSafe.PrepareOptions/ShowGenerated-apidoc) { get; } | Corresponds to command line option ` --showGenerated` |
| [ShowStatic](TypeSafe.PrepareOptions/ShowStatic-apidoc) { get; } | Corresponds to command line option `--showstatic[=VALUE]` |
| [SingleVisit](TypeSafe.PrepareOptions/SingleVisit-apidoc) { get; } | Corresponds to command line option `--single` |
| [SourceLink](TypeSafe.PrepareOptions/SourceLink-apidoc) { get; } | Corresponds to command line option `--sourcelink` |
| [StrongNameKey](TypeSafe.PrepareOptions/StrongNameKey-apidoc) { get; } | Corresponds to command line option `--sn, --strongNameKey=VALUE` |
| [SymbolDirectories](TypeSafe.PrepareOptions/SymbolDirectories-apidoc) { get; } | Corresponds to command line option `-y, --symbolDirectory=VALUE` |
| [TypeFilter](TypeSafe.PrepareOptions/TypeFilter-apidoc) { get; } | Corresponds to command line option `-t, --typeFilter=VALUE` |
| [TypeTopLevel](TypeSafe.PrepareOptions/TypeTopLevel-apidoc) { get; } | Corresponds to command line option `--typetoplevel=VALUE` |
| [VisibleBranches](TypeSafe.PrepareOptions/VisibleBranches-apidoc) { get; } | Corresponds to command line option ` -v, --visibleBranches` |
| [XmlReport](TypeSafe.PrepareOptions/XmlReport-apidoc) { get; } | Corresponds to command line option `-x, --xmlReport=VALUE` |
| [ZipFile](TypeSafe.PrepareOptions/ZipFile-apidoc) { get; } | Corresponds to command line option `--zipfile` |

## See Also

* class [TypeSafe](TypeSafe-apidoc)
* namespace [AltCover](../AltCover.Engine-apidoc)

<!-- DO NOT EDIT: generated by xmldocmd for AltCover.Engine.dll -->
