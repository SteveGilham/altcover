# Primitive.CollectOptions class

Command line options for `AltCover Runner`

```csharp
public sealed class CollectOptions : IEquatable<CollectOptions>, IStructuralEquatable
```

## Public Members

| name | description |
| --- | --- |
| [CollectOptions](Primitive.CollectOptions/CollectOptions-apidoc)(…) |  |
| static [Create](Primitive.CollectOptions/Create-apidoc)() | Returns an instance with all fields empty save `ExposeReturnCode` being `true` |
| [Cobertura](Primitive.CollectOptions/Cobertura-apidoc) { get; } | Corresponds to command line option `-c, --cobertura=VALUE` |
| [CommandLine](Primitive.CollectOptions/CommandLine-apidoc) { get; } | Corresponds to the command line arguments for the executable, given after a `-- ` |
| [Executable](Primitive.CollectOptions/Executable-apidoc) { get; } | Corresponds to command line option `-x, --executable=VALUE` |
| [ExposeReturnCode](Primitive.CollectOptions/ExposeReturnCode-apidoc) { get; } | Corresponds to the converse of command line option `--dropReturnCode ` |
| [LcovReport](Primitive.CollectOptions/LcovReport-apidoc) { get; } | Corresponds to command line option `-l, --lcovReport=VALUE` |
| [OutputFile](Primitive.CollectOptions/OutputFile-apidoc) { get; } | Corresponds to command line option `-o, --outputFile=VALUE` |
| [Packages](Primitive.CollectOptions/Packages-apidoc) { get; } | Corresponds to command line option `-p, --package=VALUE` |
| [RecorderDirectory](Primitive.CollectOptions/RecorderDirectory-apidoc) { get; } | Corresponds to command line option `-r, --recorderDirectory=VALUE` |
| [SummaryFormat](Primitive.CollectOptions/SummaryFormat-apidoc) { get; } | Corresponds to command line option `--teamcity[=VALUE]` |
| [Threshold](Primitive.CollectOptions/Threshold-apidoc) { get; } | Corresponds to command line option `-t, --threshold=VALUE` |
| [Verbosity](Primitive.CollectOptions/Verbosity-apidoc) { get; } | Corresponds to command line options `-q` and `--verbose` |
| [WorkingDirectory](Primitive.CollectOptions/WorkingDirectory-apidoc) { get; } | Corresponds to command line option `-w, --workingDirectory=VALUE` |

## See Also

* class [Primitive](./Primitive-apidoc)
* namespace [AltCover](../AltCover.Engine-apidoc)

<!-- DO NOT EDIT: generated by xmldocmd for AltCover.Engine.dll -->
