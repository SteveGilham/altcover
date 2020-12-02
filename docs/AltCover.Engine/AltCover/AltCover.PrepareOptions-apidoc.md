# AltCover.PrepareOptions class

Command line options for `AltCover`

```csharp
public abstract class PrepareOptions : IEquatable<PrepareOptions>, IStructuralEquatable
```

## Public Members

| name | description |
| --- | --- |
| static [NewAbstract](AltCover.PrepareOptions/NewAbstract-apidoc)(…) |  |
| static [NewPrimitive](AltCover.PrepareOptions/NewPrimitive-apidoc)(…) |  |
| static [NewTypeSafe](AltCover.PrepareOptions/NewTypeSafe-apidoc)(…) |  |
| [AssemblyExcludeFilter](AltCover.PrepareOptions/AssemblyExcludeFilter-apidoc) { get; } | Corresponds to command line option `-e, --assemblyExcludeFilter=VALUE` |
| [AssemblyFilter](AltCover.PrepareOptions/AssemblyFilter-apidoc) { get; } | Corresponds to command line option `-s, --assemblyFilter=VALUE` |
| [AttributeFilter](AltCover.PrepareOptions/AttributeFilter-apidoc) { get; } | Corresponds to command line option `-a, --attributeFilter=VALUE` |
| [AttributeTopLevel](AltCover.PrepareOptions/AttributeTopLevel-apidoc) { get; } | Corresponds to command line option --attributetoplevel=VALUE` |
| [BranchCover](AltCover.PrepareOptions/BranchCover-apidoc) { get; } | Corresponds to command line option `--branchcover` |
| [CallContext](AltCover.PrepareOptions/CallContext-apidoc) { get; } | Corresponds to command line option `-c, --callContext=VALUE` |
| [CommandLine](AltCover.PrepareOptions/CommandLine-apidoc) { get; } | Corresponds to the command line to run, given after a `-- ` |
| [Defer](AltCover.PrepareOptions/Defer-apidoc) { get; } | Corresponds to command line option `--defer` |
| [Dependencies](AltCover.PrepareOptions/Dependencies-apidoc) { get; } | Corresponds to command line option `-d, --dependency=VALUE` |
| [ExposeReturnCode](AltCover.PrepareOptions/ExposeReturnCode-apidoc) { get; } | Corresponds to the converse of command line option `--dropReturnCode ` |
| [FileFilter](AltCover.PrepareOptions/FileFilter-apidoc) { get; } | Corresponds to command line option `-f, --fileFilter=VALUE` |
| [InPlace](AltCover.PrepareOptions/InPlace-apidoc) { get; } | Corresponds to command line option `--inplace` |
| [InputDirectories](AltCover.PrepareOptions/InputDirectories-apidoc) { get; } | Corresponds to command line option ` -i, --inputDirectory=VALUE` |
| [Keys](AltCover.PrepareOptions/Keys-apidoc) { get; } | Corresponds to command line option ` -k, --key=VALUE` |
| [LineCover](AltCover.PrepareOptions/LineCover-apidoc) { get; } | Corresponds to command line option `--linecover` |
| [LocalSource](AltCover.PrepareOptions/LocalSource-apidoc) { get; } | Corresponds to command line option `-l, --localSource` |
| [MethodFilter](AltCover.PrepareOptions/MethodFilter-apidoc) { get; } | Corresponds to command line option ` -m, --methodFilter=VALUE` |
| [MethodPoint](AltCover.PrepareOptions/MethodPoint-apidoc) { get; } | Corresponds to command line option `--methodpoint` |
| [MethodTopLevel](AltCover.PrepareOptions/MethodTopLevel-apidoc) { get; } | Corresponds to command line option `--methodtoplevel=VALUE` |
| [OutputDirectories](AltCover.PrepareOptions/OutputDirectories-apidoc) { get; } | Corresponds to command line option `-o, --outputDirectory=VALUE` |
| [PathFilter](AltCover.PrepareOptions/PathFilter-apidoc) { get; } | Corresponds to command line option `-p, --pathFilter=VALUE` |
| [ReportFormat](AltCover.PrepareOptions/ReportFormat-apidoc) { get; } | Corresponds to command line option `--reportFormat=VALUE` |
| [Save](AltCover.PrepareOptions/Save-apidoc) { get; } | Corresponds to command line option `--save` |
| [ShowGenerated](AltCover.PrepareOptions/ShowGenerated-apidoc) { get; } | Corresponds to command line option ` --showGenerated` |
| [ShowStatic](AltCover.PrepareOptions/ShowStatic-apidoc) { get; } | Corresponds to command line option `--showstatic[=VALUE]` |
| [SingleVisit](AltCover.PrepareOptions/SingleVisit-apidoc) { get; } | Corresponds to command line option `--single` |
| [SourceLink](AltCover.PrepareOptions/SourceLink-apidoc) { get; } | Corresponds to command line option `--sourcelink` |
| [StrongNameKey](AltCover.PrepareOptions/StrongNameKey-apidoc) { get; } | Corresponds to command line option `--sn, --strongNameKey=VALUE` |
| [SymbolDirectories](AltCover.PrepareOptions/SymbolDirectories-apidoc) { get; } | Corresponds to command line option `-y, --symbolDirectory=VALUE` |
| [TypeFilter](AltCover.PrepareOptions/TypeFilter-apidoc) { get; } | Corresponds to command line option `-t, --typeFilter=VALUE` |
| [TypeTopLevel](AltCover.PrepareOptions/TypeTopLevel-apidoc) { get; } | Corresponds to command line option `--typetoplevel=VALUE` |
| [VisibleBranches](AltCover.PrepareOptions/VisibleBranches-apidoc) { get; } | Corresponds to command line option ` -v, --visibleBranches` |
| [XmlReport](AltCover.PrepareOptions/XmlReport-apidoc) { get; } | Corresponds to command line option `-x, --xmlReport=VALUE` |
| [ZipFile](AltCover.PrepareOptions/ZipFile-apidoc) { get; } | Corresponds to command line option `--zipfile` |
| [Validate](AltCover.PrepareOptions/Validate-apidoc)() | Does simple checking of the arguments without causing any changes to the system |
| class [Abstract](AltCover.PrepareOptions.Abstract-apidoc) | Options expressed as an interface |
| class [Primitive](AltCover.PrepareOptions.Primitive-apidoc) | Options expressed as an F# "stringly" typed record |
| static class [Tags](AltCover.PrepareOptions.Tags-apidoc) |  |
| class [TypeSafe](AltCover.PrepareOptions.TypeSafe-apidoc) | Options expressed as an F# strongly-typed record |

## See Also

* class [AltCover](AltCover-apidoc)
* namespace [AltCover](../AltCover.Engine-apidoc)

<!-- DO NOT EDIT: generated by xmldocmd for AltCover.Engine.dll -->
