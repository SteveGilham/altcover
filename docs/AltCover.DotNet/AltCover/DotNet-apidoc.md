# DotNet class

Construction of `dotnet test` command line options.

```csharp
public static class DotNet
```

## Public Members

| name | description |
| --- | --- |
| static [GetVersionProperties](DotNet/GetVersionProperties-apidoc) { get; } | Command line properties for `dotnet test` GetVersion option |
| static [ImportModuleProperties](DotNet/ImportModuleProperties-apidoc) { get; } | Command line properties for `dotnet test` ImportModule option |
| static [ToTestArgumentList](DotNet/ToTestArgumentList-apidoc)(…) | Converts the input into the command line for `dotnet test` |
| static [ToTestArguments](DotNet/ToTestArguments-apidoc)(…) | Converts the input into the command line for `dotnet test` |
| static [ToTestPropertiesList](DotNet/ToTestPropertiesList-apidoc)(…) | Converts the input into the command line properties for `dotnet test` |
| abstract class [CLIOptions](DotNet.CLIOptions-apidoc) | Union type defining general command line arguments for `dotnet test` use. |
| interface [ICLIOptions](DotNet.ICLIOptions-apidoc) | Interface defining general command line arguments for `dotnet test` use.. |

## See Also

* namespace [AltCover](../AltCover.DotNet-apidoc)

<!-- DO NOT EDIT: generated by xmldocmd for AltCover.DotNet.dll -->
