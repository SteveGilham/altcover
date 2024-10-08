# Abstract.ICollectOptions interface

Command line options for `AltCover Runner`

Usage

```csharp
 using Altcover;
 Abstract.ICollectOptions collect = ...
```

or

```csharp
 using static Altcover.Abstract;
 ICollectOptions collect = ...
```

```csharp
public interface ICollectOptions
```

## Members

| name | description |
| --- | --- |
| [Cobertura](Abstract.ICollectOptions/Cobertura-apidoc) { get; } | Corresponds to command line option `-c, --cobertura=VALUE` |
| [CommandLine](Abstract.ICollectOptions/CommandLine-apidoc) { get; } | Corresponds to the command line arguments for the executable, given after a `-- ` |
| [Executable](Abstract.ICollectOptions/Executable-apidoc) { get; } | Corresponds to command line option `-x, --executable=VALUE` |
| [ExposeReturnCode](Abstract.ICollectOptions/ExposeReturnCode-apidoc) { get; } | Corresponds to the converse of command line option `--dropReturnCode ` |
| [LcovReport](Abstract.ICollectOptions/LcovReport-apidoc) { get; } | Corresponds to command line option `-l, --lcovReport=VALUE` |
| [OutputFile](Abstract.ICollectOptions/OutputFile-apidoc) { get; } | Corresponds to command line option `-o, --outputFile=VALUE` |
| [Packages](Abstract.ICollectOptions/Packages-apidoc) { get; } | Corresponds to command line option `-p, --package=VALUE` |
| [RecorderDirectory](Abstract.ICollectOptions/RecorderDirectory-apidoc) { get; } | Corresponds to command line option `-r, --recorderDirectory=VALUE` |
| [SummaryFormat](Abstract.ICollectOptions/SummaryFormat-apidoc) { get; } | Corresponds to command line option `--teamcity[=VALUE]` |
| [Threshold](Abstract.ICollectOptions/Threshold-apidoc) { get; } | Corresponds to command line option `-t, --threshold=VALUE` |
| [Verbosity](Abstract.ICollectOptions/Verbosity-apidoc) { get; } | Corresponds to command line option `-q` |
| [WorkingDirectory](Abstract.ICollectOptions/WorkingDirectory-apidoc) { get; } | Corresponds to command line option `-w, --workingDirectory=VALUE` |

## See Also

* class [Abstract](./Abstract-apidoc)
* namespace [AltCover](../AltCover.Engine-apidoc)

<!-- DO NOT EDIT: generated by xmldocmd for AltCover.Engine.dll -->
