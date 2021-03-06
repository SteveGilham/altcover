# ContingentCopy class

Used by the .net core implementation to copy files copied relative to the output directory to the same locations relative to the instrumented files folder.

Not intended for general use, but see the `AltCover.targets` file for how it is used around the test stage.

```csharp
public class ContingentCopy : Task
```

## Public Members

| name | description |
| --- | --- |
| [ContingentCopy](ContingentCopy/ContingentCopy-apidoc)() | The default constructor |
| [BuildOutputDirectory](ContingentCopy/BuildOutputDirectory-apidoc) { get; set; } | The base of the relative from directory |
| [CopyToOutputDirectory](ContingentCopy/CopyToOutputDirectory-apidoc) { get; set; } | The file copying property (if empty, then no-op) |
| [FileName](ContingentCopy/FileName-apidoc) { get; set; } | The name of the file |
| [InstrumentDirectory](ContingentCopy/InstrumentDirectory-apidoc) { get; set; } | The base of the relative to directory |
| [ProjectDir](ContingentCopy/ProjectDir-apidoc) { get; set; } | The project directory for basing relative paths from (possibly empty; ignored if not an absolute path) |
| [RelativeDir](ContingentCopy/RelativeDir-apidoc) { get; set; } | The file relative location (if empty, then no-op) |
| override [Execute](ContingentCopy/Execute-apidoc)() | Perform the operation |

## See Also

* namespace [AltCover](../AltCover.Engine-apidoc)

<!-- DO NOT EDIT: generated by xmldocmd for AltCover.Engine.dll -->
