# DotNet.DotNetTest method

Hooks into the Cake wrapper for `dotnet test` and injects the AltCover command line arguments as specified.

Equivalent to

```csharp
settings.ArgumentCustomization = altcover.Concatenate(settings.ArgumentCustomization);
context.DotNetTest(project.FullPath, settings);
```

This method is a `[CakeMethodAlias]` extension method on `ICakeContext`, and `[CakeAliasCategory("Test")]`.

```csharp
public static void DotNetTest(this ICakeContext context, FilePath project, 
    DotNetCoreTestSettings testSettings, CoverageSettings coverageSettings)
```

| parameter | description |
| --- | --- |
| context | The Cake build script `ICakeContext`; a `this` parameter |
| project | The project to test as a `FilePath` |
| testSettings | The `DotNetTestSettings` for the test |
| coverageSettings | The `CoverageSettings` for the test instrumentation |

## See Also

* class [CoverageSettings](../CoverageSettings-apidoc)
* class [DotNet](../DotNet-apidoc)
* namespace [AltCover.Cake](../../AltCover.Cake-apidoc)

<!-- DO NOT EDIT: generated by xmldocmd for AltCover.Cake.dll -->