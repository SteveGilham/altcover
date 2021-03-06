# WriteOpenCoverDerivedStateCommand class

Fills other values based on recorded visit count numbers.

Adds or updates summary data and other computed items in the OpenCover format report.

In `-Coverlet` mode, also fills in some of the gaps left by `coverlet`'s OpenCover dialect, particularly giving somewhat meaningful start and end column values for its line-based paradigm, as well as npath coverage and branch exits.

```csharp
    $xml = Write-OpenCoverDerivedState -InputFile "./_Reports/OpenCoverForPester/OpenCoverForPester.coverlet.xml" -Coverlet -Assembly $Assemblies -OutputFile "./_Packaging/OpenCoverForPester.coverlet.xml"
```

```csharp
    $xml = Write-OpenCoverDerivedState -InputFile "./_Reports/OpenCoverForPester/OpenCoverForPester.xml"
```

```csharp
public class WriteOpenCoverDerivedStateCommand : PSCmdlet
```

## Public Members

| name | description |
| --- | --- |
| [WriteOpenCoverDerivedStateCommand](WriteOpenCoverDerivedStateCommand/WriteOpenCoverDerivedStateCommand-apidoc)() | The default constructor. |
| [Assembly](WriteOpenCoverDerivedStateCommand/Assembly-apidoc) { get; set; } | Assemblies to use for generating the output |
| [BranchOrdinal](WriteOpenCoverDerivedStateCommand/BranchOrdinal-apidoc) { get; set; } | The data source was generated by `coverlet`, so needs more work doing. |
| [Coverlet](WriteOpenCoverDerivedStateCommand/Coverlet-apidoc) { get; set; } | The data source was generated by `coverlet`, so needs more work doing. |
| [InputFile](WriteOpenCoverDerivedStateCommand/InputFile-apidoc) { get; set; } | Input as file path |
| [OutputFile](WriteOpenCoverDerivedStateCommand/OutputFile-apidoc) { get; set; } | Output as file path |
| [XDocument](WriteOpenCoverDerivedStateCommand/XDocument-apidoc) { get; set; } | Input as `XDocument` value |
| override [ProcessRecord](WriteOpenCoverDerivedStateCommand/ProcessRecord-apidoc)() | Create transformed document |

## See Also

* namespace [AltCover.Commands](../AltCover.PowerShell-apidoc)

<!-- DO NOT EDIT: generated by xmldocmd for AltCover.PowerShell.dll -->
