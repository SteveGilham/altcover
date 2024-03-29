# TypeSafe.Thresholds class

Corresponds to the set of options available for coverage thresholds; zero values are ignored

```csharp
public sealed class Thresholds : IEquatable<Thresholds>, IStructuralEquatable
```

## Public Members

| name | description |
| --- | --- |
| [Thresholds](TypeSafe.Thresholds/Thresholds-apidoc)(…) |  |
| static [Create](TypeSafe.Thresholds/Create-apidoc)() | Returns an all-zero value |
| [AltMaxCrap](TypeSafe.Thresholds/AltMaxCrap-apidoc) { get; } | Maximum acceptable CRAP score including methods with no source |
| [AltMethods](TypeSafe.Thresholds/AltMethods-apidoc) { get; } | Minimum method coverage %age including methods with no source |
| [Branches](TypeSafe.Thresholds/Branches-apidoc) { get; } | Minimum branch coverage %age |
| [MaxCrap](TypeSafe.Thresholds/MaxCrap-apidoc) { get; } | Maximum acceptable CRAP score |
| [Methods](TypeSafe.Thresholds/Methods-apidoc) { get; } | Minimum method coverage %age |
| [Statements](TypeSafe.Thresholds/Statements-apidoc) { get; } | Minimum statement (sequence point) coverage %age |

## See Also

* class [TypeSafe](./TypeSafe-apidoc)
* namespace [AltCover](../AltCover.Engine-apidoc)

<!-- DO NOT EDIT: generated by xmldocmd for AltCover.Engine.dll -->
