# Primitive.PrepareOptions class

Command line options for `AltCover`

```csharp
public sealed class PrepareOptions : IEquatable<PrepareOptions>, IStructuralEquatable
```

## Public Members

| name | description |
| --- | --- |
| [PrepareOptions](Primitive.PrepareOptions/PrepareOptions-apidoc)(…) |  |
| static [Create](Primitive.PrepareOptions/Create-apidoc)() | Returns an instance with all fields empty that has all empty or `false` fields except `ExposeReturnCode`, `OpenCover`, `InPlace` and `Save` are `true`, and `ShowStatic` is `-` |
| [AssemblyExcludeFilter](Primitive.PrepareOptions/AssemblyExcludeFilter-apidoc) { get; } | Corresponds to command line option `-e, --assemblyExcludeFilter=VALUE` |
| [AssemblyFilter](Primitive.PrepareOptions/AssemblyFilter-apidoc) { get; } | Corresponds to command line option `-s, --assemblyFilter=VALUE` |
| [AttributeFilter](Primitive.PrepareOptions/AttributeFilter-apidoc) { get; } | Corresponds to command line option `-a, --attributeFilter=VALUE` |
| [AttributeTopLevel](Primitive.PrepareOptions/AttributeTopLevel-apidoc) { get; } | Corresponds to command line option `--attributetoplevel=VALUE` |
| [BranchCover](Primitive.PrepareOptions/BranchCover-apidoc) { get; } | Corresponds to command line option `--branchcover` |
| [CallContext](Primitive.PrepareOptions/CallContext-apidoc) { get; } | Corresponds to command line option `-c, --callContext=VALUE` |
| [CommandLine](Primitive.PrepareOptions/CommandLine-apidoc) { get; } | Corresponds to the command line to run, given after a `-- ` |
| [Defer](Primitive.PrepareOptions/Defer-apidoc) { get; } | Corresponds to command line option `--defer` |
| [Dependencies](Primitive.PrepareOptions/Dependencies-apidoc) { get; } | Corresponds to command line option `-d, --dependency=VALUE` |
| [ExposeReturnCode](Primitive.PrepareOptions/ExposeReturnCode-apidoc) { get; } | Corresponds to the converse of command line option `--dropReturnCode ` |
| [FileFilter](Primitive.PrepareOptions/FileFilter-apidoc) { get; } | Corresponds to command line option `-f, --fileFilter=VALUE` |
| [InPlace](Primitive.PrepareOptions/InPlace-apidoc) { get; } | Corresponds to command line option `--inplace` |
| [InputDirectories](Primitive.PrepareOptions/InputDirectories-apidoc) { get; } | Corresponds to command line option ` -i, --inputDirectory=VALUE` |
| [Keys](Primitive.PrepareOptions/Keys-apidoc) { get; } | Corresponds to command line option ` -k, --key=VALUE` |
| [LineCover](Primitive.PrepareOptions/LineCover-apidoc) { get; } | Corresponds to command line option `--linecover` |
| [LocalSource](Primitive.PrepareOptions/LocalSource-apidoc) { get; } | Corresponds to command line option `-l, --localSource` |
| [MethodFilter](Primitive.PrepareOptions/MethodFilter-apidoc) { get; } | Corresponds to command line option ` -m, --methodFilter=VALUE` |
| [MethodPoint](Primitive.PrepareOptions/MethodPoint-apidoc) { get; } | Corresponds to command line option `--methodpoint` |
| [MethodTopLevel](Primitive.PrepareOptions/MethodTopLevel-apidoc) { get; } | Corresponds to command line option `--methodtoplevel=VALUE` |
| [OutputDirectories](Primitive.PrepareOptions/OutputDirectories-apidoc) { get; } | Corresponds to command line option `-o, --outputDirectory=VALUE` |
| [PathFilter](Primitive.PrepareOptions/PathFilter-apidoc) { get; } | Corresponds to command line option `-p, --pathFilter=VALUE` |
| [ReportFormat](Primitive.PrepareOptions/ReportFormat-apidoc) { get; } | Corresponds to command line option `--reportFormat=VALUE` |
| [Save](Primitive.PrepareOptions/Save-apidoc) { get; } | Corresponds to command line option `--save` |
| [ShowGenerated](Primitive.PrepareOptions/ShowGenerated-apidoc) { get; } | Corresponds to command line option ` --showGenerated` |
| [ShowStatic](Primitive.PrepareOptions/ShowStatic-apidoc) { get; } | Corresponds to command line option `--showstatic[=VALUE]` |
| [SingleVisit](Primitive.PrepareOptions/SingleVisit-apidoc) { get; } | Corresponds to command line option `--single` |
| [SourceLink](Primitive.PrepareOptions/SourceLink-apidoc) { get; } | Corresponds to command line option `--sourcelink` |
| [StrongNameKey](Primitive.PrepareOptions/StrongNameKey-apidoc) { get; } | Corresponds to command line option `--sn, --strongNameKey=VALUE` |
| [SymbolDirectories](Primitive.PrepareOptions/SymbolDirectories-apidoc) { get; } | Corresponds to command line option `-y, --symbolDirectory=VALUE` |
| [TypeFilter](Primitive.PrepareOptions/TypeFilter-apidoc) { get; } | Corresponds to command line option `-t, --typeFilter=VALUE` |
| [TypeTopLevel](Primitive.PrepareOptions/TypeTopLevel-apidoc) { get; } | Corresponds to command line option `--typetoplevel=VALUE` |
| [VisibleBranches](Primitive.PrepareOptions/VisibleBranches-apidoc) { get; } | Corresponds to command line option ` -v, --visibleBranches` |
| [XmlReport](Primitive.PrepareOptions/XmlReport-apidoc) { get; } | Corresponds to command line option `-x, --xmlReport=VALUE` |
| [ZipFile](Primitive.PrepareOptions/ZipFile-apidoc) { get; } | Corresponds to command line option `--zipfile` |

## See Also

* class [Primitive](Primitive-apidoc)
* namespace [AltCover](../AltCover.Engine-apidoc)

<!-- DO NOT EDIT: generated by xmldocmd for AltCover.Engine.dll -->
