# InvokeAltCoverCommand.Threshold property

One or more of minimum acceptable statement (S), branch (B) or method (M)/alternative method (AM) coverage percentage (integer, 1 to 100) or maximum acceptable CRAP/alternative CRAP score (C/AC followed by integer, 1 to 255) e.g. M80AM70C40AC100B50. If the value starts with a number, a leading S is assumed. If any threshold is specified more than once, the last instance is assumed -- so 25S50 counts as S50. Zero/absent values are ignored. If a coverage result is below threshold, or the CRAP score is above threshold, the return code of the process is the largest abs(threshold - actual) rounded up to the nearest integer.

```csharp
public string Threshold { get; set; }
```

## See Also

* class [InvokeAltCoverCommand](../InvokeAltCoverCommand-apidoc)
* namespace [AltCover.Commands](../../AltCover.PowerShell-apidoc)

<!-- DO NOT EDIT: generated by xmldocmd for AltCover.PowerShell.dll -->
