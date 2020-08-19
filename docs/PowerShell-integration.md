This is the PowerShell Help/scripting use version of the `AltCover.PowerShell.dll` API; the .net programmable API documentation is [here](https://stevegilham.github.io/altcover/AltCover.PowerShell/AltCover.PowerShell-apidoc).  This functionality is present in all NuGet packages except the `altcover.visualizer` global tool package.

Use `Import-Module` either by specific path using the command given by `AltCover.exe ImportModule` (or `dotnet AltCover.dll ImportModule` or `altcover ImportModule`), or add the appropriate directory to your `PSModulePath` and do a simple import.

The string output from `AltCover ImportModule` is intended for convenient cutting and pasting; it can be used in a script like
```
$ipmo = (AltCover ImportModule | Out-String).Trim().Split()[1].Trim(@('""'))
Import-Module $ipmo
```
which unpeels the wrapper around the file path.  Just substitute in the appropriate invocation for the `AltCover`.

## Cmdlets
* [Add-Accelerator](#add-accelerator)
* [Compress-Branching](#compress-branching)
* [ConvertFrom-NCover](#convertfrom-ncover)
* [ConvertTo-BarChart](#convertto-barchart)
* [ConvertTo-Cobertura](#convertto-cobertura)
* [ConvertTo-Lcov](#convertto-lcov)
* [ConvertTo-NCover](#convertto-ncover)
* [ConvertTo-XDocument](#convertto-xdocument)
* [ConvertTo-XmlDocument](#convertto-xmldocument)
* [Get-Accelerator](#get-accelerator)
* [Invoke-AltCover](#invoke-altcover)
* [Write-OpenCoverDerivedState](#write-opencoverderivedstate)
 
###    Add-Accelerator

NAME

```
Add-Accelerator

```
SYNOPSIS

Add one or more type abbreviations, like the built-in `[xml]` for `System.Xml.XmlDocument`.


SYNTAX

```
Add-Accelerator [-Accelerator <SwitchParameter>] [-Mapping <Hashtable>] [-XDocument <SwitchParameter>]
[<CommonParameters>]


```
DESCRIPTION

Extends the built-in set of type abbreviations with user declared ones. Two common abbreviations are supplied as
switch parameters, and then others can be added free-form.


PARAMETERS
#### `-Mapping <Hashtable>` 
Mapping of name to type

`[Key.ToString()]` is the accelerator for `Value.GetType()` (or just `Value` if that is a `System.Type`
already.

```
Required?                    false
Position?                    named
Default value
Accept pipeline input?       true (ByValue, ByPropertyName)
Accept wildcard characters?  false
```

#### `-Accelerator <SwitchParameter>` 
Add [accelerators] for the accelerator type

```
Required?                    false
Position?                    named
Default value                False
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-XDocument <SwitchParameter>` 
Add [xdoc] for the `System.Xml.Linq.XDocument` type

```
Required?                    false
Position?                    named
Default value                False
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `<CommonParameters>` 
This cmdlet supports the common parameters: Verbose, Debug,
ErrorAction, ErrorVariable, WarningAction, WarningVariable,
OutBuffer, PipelineVariable, and OutVariable. For more information, see
about_CommonParameters (https:/go.microsoft.com/fwlink/?LinkID=113216).

INPUTS
```
System.Collections.Hashtable
```
Mapping of name to type

`[Key.ToString()]` is the accelerator for `Value.GetType()` (or just `Value` if that is a `System.Type`
already.


OUTPUTS
```
None
```


----------  EXAMPLE 1  ----------


```
Add-Accelerator -XDocument
```

Add `[xdoc]` the easy way

----------  EXAMPLE 2  ----------


```
Add-Accelerator -Mapping @{ "xdoc" = [type]::gettype("System.Xml.Linq.XDocument")
```

Add `[xdoc]` by the long way round

###    Compress-Branching

NAME

```
Compress-Branching

```
SYNOPSIS

Removes compiler-generated hidden branches from OpenCover.


SYNTAX

```
Compress-Branching [-XDocument] <XDocument> [[-OutputFile] <string>] [-SameSpan] <SwitchParameter>
[[-WithinSequencePoint] <SwitchParameter>] [<CommonParameters>]

Compress-Branching [-XDocument] <XDocument> [[-OutputFile] <string>] [-WithinSequencePoint] <SwitchParameter>
[<CommonParameters>]

Compress-Branching [-InputFile] <string> [[-OutputFile] <string>] [-SameSpan] <SwitchParameter>
[[-WithinSequencePoint] <SwitchParameter>] [<CommonParameters>]

Compress-Branching [-InputFile] <string> [[-OutputFile] <string>] [-WithinSequencePoint] <SwitchParameter>
[<CommonParameters>]


```
DESCRIPTION

Takes output from the OpenCover program, and adjust from OpenCover's liberal idea of significant branches towards
AltCover's more restricted approach -- chose either or both of `-SameSpan` to unify branches that go from the
same start, and take the same trajectory to the same end (OpenCover issue #786 being one instance of this) and
`-WithinSequencePoint` to remove branches interior to a statement (compiler generated things like stashing of
lambdas, the hidden conditional `Dispose()` after a `using`, or inside F# inlines -- OpenCover issues #657, #807
being instances of this).

Either takes an `XDocument` from the pipeline or from a file; emits the result as an `XDocument` to the pipeline
and optionally to a file.


PARAMETERS
#### `-XDocument <XDocument>` 
Input as `XDocument` value

```
Required?                    true
Position?                    1
Default value
Accept pipeline input?       true (ByValue)
Accept wildcard characters?  false
```

#### `-InputFile <string>` 
Input as file path

```
Required?                    true
Position?                    1
Default value
Accept pipeline input?       true (ByValue)
Accept wildcard characters?  false
```

#### `-OutputFile <string>` 
Output as file path

```
Required?                    false
Position?                    3
Default value
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-SameSpan <SwitchParameter>` 
Merge branches when start and end at the same place

```
Required?                    true
Position?                    4
Default value                False
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-WithinSequencePoint <SwitchParameter>` 
Discard branches within a sequence point

```
Required?                    true
Position?                    4
Default value                False
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `<CommonParameters>` 
This cmdlet supports the common parameters: Verbose, Debug,
ErrorAction, ErrorVariable, WarningAction, WarningVariable,
OutBuffer, PipelineVariable, and OutVariable. For more information, see
about_CommonParameters (https:/go.microsoft.com/fwlink/?LinkID=113216).

INPUTS
```
System.Xml.Linq.XDocument
```
Input as `XDocument` value

```
System.String
```
Input as file path


OUTPUTS
```
System.Xml.Linq.XDocument
```


----------  EXAMPLE 1  ----------


```
$xml = Compress-Branching -WithinSequencePoint -InputFile "./Tests/Compressible.xml" -OutputFile "./_Packaging/CompressInterior.xml"
```


###    ConvertFrom-NCover

NAME

```
ConvertFrom-NCover

```
SYNOPSIS

Converts classic NCover format and returns OpenCover format.


SYNTAX

```
ConvertFrom-NCover [-XDocument] <XDocument> [-Assembly] <string[]> [[-OutputFile] <string>] [<CommonParameters>]

ConvertFrom-NCover [-InputFile] <string> [-Assembly] <string[]> [[-OutputFile] <string>] [<CommonParameters>]


```
DESCRIPTION

The classic NCover format input may be either may be as an `XDocument` from the object pipeline or from a file.

Writes the OpenCover format report to the pipeline as an `XDocument`, and, optionally, to a file. The report will
contain data for the assemblies listed as the `-Assembly` argument and that are in the NCover input.


PARAMETERS
#### `-XDocument <XDocument>` 
Input as `XDocument` value

```
Required?                    true
Position?                    1
Default value
Accept pipeline input?       true (ByValue)
Accept wildcard characters?  false
```

#### `-InputFile <string>` 
Input as file path

```
Required?                    true
Position?                    1
Default value
Accept pipeline input?       true (ByValue)
Accept wildcard characters?  false
```

#### `-Assembly <string[]>` 
Assemblies to use for generating the output

```
Required?                    true
Position?                    2
Default value
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-OutputFile <string>` 
Output as file path

```
Required?                    false
Position?                    3
Default value
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `<CommonParameters>` 
This cmdlet supports the common parameters: Verbose, Debug,
ErrorAction, ErrorVariable, WarningAction, WarningVariable,
OutBuffer, PipelineVariable, and OutVariable. For more information, see
about_CommonParameters (https:/go.microsoft.com/fwlink/?LinkID=113216).

INPUTS
```
System.Xml.Linq.XDocument
```
Input as `XDocument` value

```
System.String
```
Input as file path


OUTPUTS
```
System.Xml.Linq.XDocument
```


----------  EXAMPLE 1  ----------


```
$xml = ConvertFrom-NCover -InputFile "./_Reports/ReleaseXUnitFSharpTypesDotNetRunner.xml" -Assembly $Assemblies -OutputFile "./_Packaging/AltCoverFSharpTypes.xml"
```


###    ConvertTo-BarChart

NAME

```
ConvertTo-BarChart

```
SYNOPSIS

Generates a simple HTML report from coverage data.


SYNTAX

```
ConvertTo-BarChart [-XDocument] <XDocument> [[-OutputFile] <string>] [<CommonParameters>]

ConvertTo-BarChart [-InputFile] <string> [[-OutputFile] <string>] [<CommonParameters>]


```
DESCRIPTION

The report produced is based on the old NCover 1.5.8 XSLT, for both NCover and OpenCover coverage format data.
The input is as a file name or an `XDocument` from the pipeline, the output is to the pipeline as an `XDocument`,
and, optionally, to a file.


PARAMETERS
#### `-XDocument <XDocument>` 
Input as `XDocument` value

```
Required?                    true
Position?                    1
Default value
Accept pipeline input?       true (ByValue)
Accept wildcard characters?  false
```

#### `-InputFile <string>` 
Input as file path

```
Required?                    true
Position?                    1
Default value
Accept pipeline input?       true (ByValue)
Accept wildcard characters?  false
```

#### `-OutputFile <string>` 
Output as file path

```
Required?                    false
Position?                    2
Default value
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `<CommonParameters>` 
This cmdlet supports the common parameters: Verbose, Debug,
ErrorAction, ErrorVariable, WarningAction, WarningVariable,
OutBuffer, PipelineVariable, and OutVariable. For more information, see
about_CommonParameters (https:/go.microsoft.com/fwlink/?LinkID=113216).

INPUTS
```
System.Xml.Linq.XDocument
```
Input as `XDocument` value

```
System.String
```
Input as file path


OUTPUTS
```
System.Xml.Linq.XDocument
```


----------  EXAMPLE 1  ----------


```
$xml = ConvertTo-BarChart -InputFile "./Tests/HandRolledMonoCoverage.xml" -OutputFile "./_Packaging/HandRolledMonoCoverage.html"
```


###    ConvertTo-Cobertura

NAME

```
ConvertTo-Cobertura

```
SYNOPSIS

Creates a Cobertura format report from other report formats.


SYNTAX

```
ConvertTo-Cobertura [-XDocument] <XDocument> [[-OutputFile] <string>] [<CommonParameters>]

ConvertTo-Cobertura [-InputFile] <string> [[-OutputFile] <string>] [<CommonParameters>]


```
DESCRIPTION

Takes either OpenCover or classic NCover format input as an `XDocument`, as an argument or from the object
pipeline.

Writes the Cobertura report to the object pipeline as an `XDocument`, and optionally to a file.


PARAMETERS
#### `-XDocument <XDocument>` 
Input as `XDocument` value

```
Required?                    true
Position?                    1
Default value
Accept pipeline input?       true (ByValue)
Accept wildcard characters?  false
```

#### `-InputFile <string>` 
Input as file path

```
Required?                    true
Position?                    1
Default value
Accept pipeline input?       true (ByValue)
Accept wildcard characters?  false
```

#### `-OutputFile <string>` 
Output as file path

```
Required?                    false
Position?                    2
Default value
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `<CommonParameters>` 
This cmdlet supports the common parameters: Verbose, Debug,
ErrorAction, ErrorVariable, WarningAction, WarningVariable,
OutBuffer, PipelineVariable, and OutVariable. For more information, see
about_CommonParameters (https:/go.microsoft.com/fwlink/?LinkID=113216).

INPUTS
```
System.Xml.Linq.XDocument
```
Input as `XDocument` value

```
System.String
```
Input as file path


OUTPUTS
```
System.Xml.Linq.XDocument
```


----------  EXAMPLE 1  ----------


```
ConvertTo-Cobertura -InputFile "./Tests/HandRolledMonoCoverage.xml" -OutputFile "./_Packaging/OpenCover.lcov"
```


###    ConvertTo-Lcov

NAME

```
ConvertTo-Lcov

```
SYNOPSIS

Creates an Lcov format report from other report formats.


SYNTAX

```
ConvertTo-Lcov [-XDocument] <XDocument> [-OutputFile] <string> [<CommonParameters>]

ConvertTo-Lcov [-InputFile] <string> [-OutputFile] <string> [<CommonParameters>]


```
DESCRIPTION

Takes either OpenCover or classic NCover format input as an `XDocument`, as an argument or from the object
pipeline. Writes the Lcov report to a file.


PARAMETERS
#### `-XDocument <XDocument>` 
Input as `XDocument` value

```
Required?                    true
Position?                    1
Default value
Accept pipeline input?       true (ByValue)
Accept wildcard characters?  false
```

#### `-InputFile <string>` 
Input as file path

```
Required?                    true
Position?                    1
Default value
Accept pipeline input?       true (ByValue)
Accept wildcard characters?  false
```

#### `-OutputFile <string>` 
Output as file path

```
Required?                    true
Position?                    2
Default value
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `<CommonParameters>` 
This cmdlet supports the common parameters: Verbose, Debug,
ErrorAction, ErrorVariable, WarningAction, WarningVariable,
OutBuffer, PipelineVariable, and OutVariable. For more information, see
about_CommonParameters (https:/go.microsoft.com/fwlink/?LinkID=113216).

INPUTS
```
System.Xml.Linq.XDocument
```
Input as `XDocument` value

```
System.String
```
Input as file path


OUTPUTS
```
None
```


----------  EXAMPLE 1  ----------


```
ConvertTo-Lcov -InputFile "./Tests/HandRolledMonoCoverage.xml" -OutputFile "./_Packaging/OpenCover.lcov"
```


###    ConvertTo-NCover

NAME

```
ConvertTo-NCover

```
SYNOPSIS

Converts OpenCover format to NCover format.


SYNTAX

```
ConvertTo-NCover [-XDocument] <XDocument> [[-OutputFile] <string>] [<CommonParameters>]

ConvertTo-NCover [-InputFile] <string> [[-OutputFile] <string>] [<CommonParameters>]


```
DESCRIPTION

Takes the OpenCover input either as an ``XDocument`` from the object pipeline or from a file.

Writes the classic NCover report to the pipeline as an ``XDocument``, and, optionally, to a file.


PARAMETERS
#### `-XDocument <XDocument>` 
Input as `XDocument` value

```
Required?                    true
Position?                    1
Default value
Accept pipeline input?       true (ByValue)
Accept wildcard characters?  false
```

#### `-InputFile <string>` 
Input as file path

```
Required?                    true
Position?                    1
Default value
Accept pipeline input?       true (ByValue)
Accept wildcard characters?  false
```

#### `-OutputFile <string>` 
Output as file path

```
Required?                    false
Position?                    2
Default value
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `<CommonParameters>` 
This cmdlet supports the common parameters: Verbose, Debug,
ErrorAction, ErrorVariable, WarningAction, WarningVariable,
OutBuffer, PipelineVariable, and OutVariable. For more information, see
about_CommonParameters (https:/go.microsoft.com/fwlink/?LinkID=113216).

INPUTS
```
System.Xml.Linq.XDocument
```
Input as `XDocument` value

```
System.String
```
Input as file path


OUTPUTS
```
System.Xml.Linq.XDocument
```


----------  EXAMPLE 1  ----------


```
$xml = ConvertTo-NCover -InputFile "./Tests/HandRolledMonoCoverage.xml" -OutputFile "./_Packaging/HandRolledMonoNCover.xml"
```


###    ConvertTo-XDocument

NAME

```
ConvertTo-XDocument

```
SYNOPSIS

Converts `[xml]` to `XDocument`.


SYNTAX

```
ConvertTo-XDocument [-XmlDocument] <XmlDocument> [<CommonParameters>]


```
DESCRIPTION

Takes an `[xml]` in and puts an `XDocument` to the object pipeline.


PARAMETERS
#### `-XmlDocument <XmlDocument>` 
Input as `[xml]` value

```
Required?                    true
Position?                    1
Default value
Accept pipeline input?       true (ByValue)
Accept wildcard characters?  false
```

#### `<CommonParameters>` 
This cmdlet supports the common parameters: Verbose, Debug,
ErrorAction, ErrorVariable, WarningAction, WarningVariable,
OutBuffer, PipelineVariable, and OutVariable. For more information, see
about_CommonParameters (https:/go.microsoft.com/fwlink/?LinkID=113216).

INPUTS
```
System.Xml.XmlDocument
```
Input as `[xml]` value


OUTPUTS
```
System.Xml.Linq.XDocument
```


----------  EXAMPLE 1  ----------


```
$xd = [xml]"<Document />" | ConvertTo-XDocument
```


###    ConvertTo-XmlDocument

NAME

```
ConvertTo-XmlDocument

```
SYNOPSIS

Converts `XDocument` to `[xml]`.


SYNTAX

```
ConvertTo-XmlDocument [-XDocument] <XDocument> [<CommonParameters>]


```
DESCRIPTION

Takes an `XDocument` in and puts an `[xml]` to the object pipeline.


PARAMETERS
#### `-XDocument <XDocument>` 
Input as `XDocument` value

```
Required?                    true
Position?                    1
Default value
Accept pipeline input?       true (ByValue)
Accept wildcard characters?  false
```

#### `<CommonParameters>` 
This cmdlet supports the common parameters: Verbose, Debug,
ErrorAction, ErrorVariable, WarningAction, WarningVariable,
OutBuffer, PipelineVariable, and OutVariable. For more information, see
about_CommonParameters (https:/go.microsoft.com/fwlink/?LinkID=113216).

INPUTS
```
System.Xml.Linq.XDocument
```
Input as `XDocument` value


OUTPUTS
```
System.Xml.XmlDocument
```


----------  EXAMPLE 1  ----------


```
Add-Accelerator -XDocument; $xd = [xdoc]::Load("./Tests/Sample1WithNCover.xml");  $xml = $xd | ConvertTo-XmlDocument

```

Adds `[xdoc]` the easy way first, then does the conversion

###    Get-Accelerator

NAME

```
Get-Accelerator

```
SYNOPSIS

List all type abbreviations, like the built-in `[xml]` for `System.Xml.XmlDocument`.


SYNTAX

```
Get-Accelerator [<CommonParameters>]


```
DESCRIPTION

Reports all currently available type abbreviations, both system- and user- defined.


PARAMETERS
#### `<CommonParameters>` 
This cmdlet supports the common parameters: Verbose, Debug,
ErrorAction, ErrorVariable, WarningAction, WarningVariable,
OutBuffer, PipelineVariable, and OutVariable. For more information, see
about_CommonParameters (https:/go.microsoft.com/fwlink/?LinkID=113216).

INPUTS

OUTPUTS
```
System.Collections.Hashtable
```


----------  EXAMPLE 1  ----------


```
$a = Get-Accelerator
```


###    Invoke-AltCover

NAME

```
Invoke-AltCover

```
SYNOPSIS

The equivalent of the `AltCover` command or `altcover` global tool.


SYNTAX

```
Invoke-AltCover [-Runner] <SwitchParameter> -RecorderDirectory <string> [-Cobertura <string>] [-CommandLine
<string[]>] [-DropReturnCode <SwitchParameter>] [-Executable <string>] [-LcovReport <string>] [-OutputFile
<string>] [-SummaryFormat {Default | R | B | RPlus | BPlus}] [-Threshold <string>] [-WorkingDirectory <string>]
[<CommonParameters>]

Invoke-AltCover [-AssemblyExcludeFilter <string[]>] [-AssemblyFilter <string[]>] [-AttributeFilter <string[]>]
[-AttributeTopLevel <string[]>] [-BranchCover <SwitchParameter>] [-CallContext <string[]>] [-CommandLine
<string[]>] [-Defer <SwitchParameter>] [-Dependency <string[]>] [-DropReturnCode <SwitchParameter>] [-FileFilter
<string[]>] [-InPlace <SwitchParameter>] [-InputDirectory <string[]>] [-Key <string[]>] [-LineCover
<SwitchParameter>] [-LocalSource <SwitchParameter>] [-MethodFilter <string[]>] [-MethodPoint <SwitchParameter>]
[-MethodTopLevel <string[]>] [-OutputDirectory <string[]>] [-PathFilter <string[]>] [-ReportFormat {NCover |
OpenCover}] [-Save <SwitchParameter>] [-ShowGenerated <SwitchParameter>] [-ShowStatic {KeepHidden | Mark |
Reveal}] [-Single <SwitchParameter>] [-SourceLink <SwitchParameter>] [-StrongNameKey <string>] [-SymbolDirectory
<string[]>] [-TypeFilter <string[]>] [-TypeTopLevel <string[]>] [-VisibleBranches <SwitchParameter>] [-XmlReport
<string>] [-ZipFile <SwitchParameter>] [<CommonParameters>]

Invoke-AltCover [-Version] <SwitchParameter> [<CommonParameters>]


```
DESCRIPTION

The arguments parallel the command-line executable; the main difference is that `-Runner` is a switch parameter
that selects the runner-mode parameter set if present.

The "working directory" used where mentioned in the parameter descriptions is the current set location in
PowerShell, **_not_** the underlying current directory.

**Note**: As Powershell informational output is suppressed by default in PowerShell 5+, the `-InformationAction
Continue` option is needed to show the progress and summary information for the process if this is desired.

Summary information is also written to the object pipeline.

**Note**: `-WhatIf` includes validation for the command line arguments. It is ignored for the purely read-only
`-Version` option


PARAMETERS
#### `-Runner <SwitchParameter>` 
Selects `Runner` mode

```
Required?                    true
Position?                    1
Default value                False
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-RecorderDirectory <string>` 
The folder containing the instrumented code to monitor (including the `AltCover.Recorder.g.dll` generated by
previous a use of the .net core `AltCover`).

```
Required?                    true
Position?                    named
Default value
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-WorkingDirectory <string>` 
The working directory for the application launch

```
Required?                    false
Position?                    named
Default value
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-Executable <string>` 
The executable to run e.g. `dotnet`

```
Required?                    false
Position?                    named
Default value
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-LcovReport <string>` 
File path for lcov format version of the collected data

```
Required?                    false
Position?                    named
Default value
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-Threshold <string>` 
One or more of minimum acceptable statement (S), branch (B) or method (M)/alternative method (AM) coverage
percentage (integer, 1 to 100) or maximum acceptable CRAP/alternative CRAP score (C/AC followed by integer, 1
to 255) e.g. M80AM70C40AC100B50. If the value starts with a number, a leading S is assumed. If any threshold
is specified more than once, the last instance is assumed -- so 25S50 counts as S50. Zero/absent values are
ignored. If a coverage result is below threshold, or the CRAP score is above threshold, the return code of
the process is the largest abs(threshold - actual) rounded up to the nearest integer.

```
Required?                    false
Position?                    named
Default value
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-Cobertura <string>` 
File path for Cobertura format version of the collected data

```
Required?                    false
Position?                    named
Default value
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-OutputFile <string>` 
Write the recorded coverage to this file rather than overwriting the original report file.

```
Required?                    false
Position?                    named
Default value
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-CommandLine <string[]>` 
Arguments for a launched process

```
Required?                    false
Position?                    named
Default value
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-InputDirectory <string[]>` 
Folder or folders containing assemblies to instrument (default: current directory)

```
Required?                    false
Position?                    named
Default value
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-OutputDirectory <string[]>` 
Folder or folders to receive the instrumented assemblies and their companions (default: sub-folder
`__Instrumented` of the current directory; or `__Saved` if `-Inplace` is set).

```
Required?                    false
Position?                    named
Default value
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-SymbolDirectory <string[]>` 
Additional folder or folders to search for matching symbols for the assemblies in the input directory

```
Required?                    false
Position?                    named
Default value
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-Dependency <string[]>` 
Assembly paths to resolve missing references.

```
Required?                    false
Position?                    named
Default value
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-Key <string[]>` 
Strong name key or keys that were used to sign the inputs

```
Required?                    false
Position?                    named
Default value
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-StrongNameKey <string>` 
The default strong naming key to apply to instrumented assemblies

```
Required?                    false
Position?                    named
Default value
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-XmlReport <string>` 
The output report template file (default: coverage.xml in the current directory)

```
Required?                    false
Position?                    named
Default value
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-FileFilter <string[]>` 
Source file names to exclude from instrumentation

```
Required?                    false
Position?                    named
Default value
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-PathFilter <string[]>` 
Source file paths to exclude from instrumentation

```
Required?                    false
Position?                    named
Default value
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-AssemblyFilter <string[]>` 
Assembly names to exclude from instrumentation (linked by instrumented assemblies)

```
Required?                    false
Position?                    named
Default value
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-AssemblyExcludeFilter <string[]>` 
Assembly names to exclude from instrumentation (linked to instrumented assemblies)

```
Required?                    false
Position?                    named
Default value
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-TypeFilter <string[]>` 
Type names to exclude from instrumentation

```
Required?                    false
Position?                    named
Default value
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-MethodFilter <string[]>` 
Method names to exclude from instrumentation

```
Required?                    false
Position?                    named
Default value
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-AttributeFilter <string[]>` 
Attribute names to exclude from instrumentation

```
Required?                    false
Position?                    named
Default value
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-AttributeTopLevel <string[]>` 
Attributes to mark a type as "top level"

```
Required?                    false
Position?                    named
Default value
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-TypeTopLevel <string[]>` 
Names to mark a type as "top level"

```
Required?                    false
Position?                    named
Default value
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-MethodTopLevel <string[]>` 
Names to mark a function as "top level"

```
Required?                    false
Position?                    named
Default value
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-CallContext <string[]>` 
Tracking either times of visits in ticks or designated method calls leading to the visits.

A single digit 0-7 gives the number of decimal places of seconds to report; everything else is at the mercy
of the system clock information available through DateTime.UtcNow

A string in brackets "[]" is interpreted as an attribute type name (the trailing "Attribute" is optional), so
[Test] or [TestAttribute] will match; if the name contains one or more ".", then it will be matched against
the full name of the attribute type.

Other strings are interpreted as method names (fully qualified if the string contains any "." characters).

Incompatible with -Single

```
Required?                    false
Position?                    named
Default value
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-ReportFormat <ReportFormat>` 
Generate the report in the specified format (NCover or the default OpenCover)

Possible values: NCover, OpenCover

```
Required?                    false
Position?                    named
Default value                OpenCover
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-InPlace <SwitchParameter>` 
Instrument the inputDirectory, rather than the outputDirectory (e.g. for `dotnet test`)

```
Required?                    false
Position?                    named
Default value                False
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-Save <SwitchParameter>` 
Write raw coverage data to file for later processing

```
Required?                    false
Position?                    named
Default value                False
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-ZipFile <SwitchParameter>` 
Emit the XML report inside a zip archive.

```
Required?                    false
Position?                    named
Default value                False
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-MethodPoint <SwitchParameter>` 
Record only whether a method has been visited or not. Overrides the `-LineCover` and `-BranchCover` options.

```
Required?                    false
Position?                    named
Default value                False
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-Single <SwitchParameter>` 
only record the first hit at any location. Incompatible with `-CallContext`.

```
Required?                    false
Position?                    named
Default value                False
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-LineCover <SwitchParameter>` 
Do not record branch coverage. Implies, and is compatible with, the -ReportFormat "opencover" option.
Incompatible with `-BranchCover`.

```
Required?                    false
Position?                    named
Default value                False
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-BranchCover <SwitchParameter>` 
Do not record line coverage. Implies, and is compatible with, the -ReportFormat "opencover" option.
Incompatible with `-LineCover`.

```
Required?                    false
Position?                    named
Default value                False
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-Version <SwitchParameter>` 
Selects `Version` mode

```
Required?                    true
Position?                    1
Default value                False
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-DropReturnCode <SwitchParameter>` 
Do not report any non-zero return code from a launched process.

```
Required?                    false
Position?                    named
Default value                False
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-SourceLink <SwitchParameter>` 
Display sourcelink URLs rather than file paths if present.

```
Required?                    false
Position?                    named
Default value                False
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-Defer <SwitchParameter>` 
Defers writing runner-mode coverage data until process exit.

```
Required?                    false
Position?                    named
Default value                False
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-LocalSource <SwitchParameter>` 
Don't instrument code for which the source file is not present.

```
Required?                    false
Position?                    named
Default value                False
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-VisibleBranches <SwitchParameter>` 
Hide complex internal IL branching implementation details in switch/match constructs, and just show what the
source level logic implies.

```
Required?                    false
Position?                    named
Default value                False
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-ShowGenerated <SwitchParameter>` 
Mark generated code with a visit count of -2 (Automatic) for the Visualizer if unvisited

```
Required?                    false
Position?                    named
Default value                False
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-ShowStatic <ShowHidden>` 
Instrument and show code that is by default skipped as trivial.

Possible values: KeepHidden, Mark, Reveal

```
Required?                    false
Position?                    named
Default value                KeepHidden
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-SummaryFormat <Summary>` 
Selects summary format

Possible values: Default, R, B, RPlus, BPlus

```
Required?                    false
Position?                    named
Default value                Default
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `<CommonParameters>` 
This cmdlet supports the common parameters: Verbose, Debug,
ErrorAction, ErrorVariable, WarningAction, WarningVariable,
OutBuffer, PipelineVariable, and OutVariable. For more information, see
about_CommonParameters (https:/go.microsoft.com/fwlink/?LinkID=113216).

INPUTS

OUTPUTS
```
System.String
```

```
None
```


----------  EXAMPLE 1  ----------


```
Invoke-AltCover -XmlReport $x -OutputDirectory  $o -InputDirectory $i -AssemblyFilter "Adapter" -ReportFormat NCover -InformationAction Continue
```


###    Write-OpenCoverDerivedState

NAME

```
Write-OpenCoverDerivedState

```
SYNOPSIS

Fills other values based on recorded visit count numbers.


SYNTAX

```
Write-OpenCoverDerivedState [-XDocument] <XDocument> [-Coverlet] <SwitchParameter> [-Assembly] <string[]>
[[-OutputFile] <string>] [<CommonParameters>]

Write-OpenCoverDerivedState [-XDocument] <XDocument> [[-BranchOrdinal] {Offset | SL}] [[-OutputFile] <string>]
[<CommonParameters>]

Write-OpenCoverDerivedState [-InputFile] <string> [-Coverlet] <SwitchParameter> [-Assembly] <string[]>
[[-OutputFile] <string>] [<CommonParameters>]

Write-OpenCoverDerivedState [-InputFile] <string> [[-BranchOrdinal] {Offset | SL}] [[-OutputFile] <string>]
[<CommonParameters>]


```
DESCRIPTION

Adds or updates summary data and other computed items in the OpenCover format report.

In `-Coverlet` mode, also fills in some of the gaps left by `coverlet`'s OpenCover dialect, particularly giving
somewhat meaningful start and end column values for its line-based paradigm, as well as npath coverage and branch
exits.


PARAMETERS
#### `-XDocument <XDocument>` 
Input as `XDocument` value

```
Required?                    true
Position?                    1
Default value
Accept pipeline input?       true (ByValue)
Accept wildcard characters?  false
```

#### `-InputFile <string>` 
Input as file path

```
Required?                    true
Position?                    1
Default value
Accept pipeline input?       true (ByValue)
Accept wildcard characters?  false
```

#### `-Coverlet <SwitchParameter>` 
The data source was generated by `coverlet`, so needs more work doing.

```
Required?                    true
Position?                    2
Default value                False
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-BranchOrdinal <BranchOrdinal>` 
The data source was generated by `coverlet`, so needs more work doing.

Possible values: Offset, SL

```
Required?                    false
Position?                    2
Default value                Offset
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-Assembly <string[]>` 
Assemblies to use for generating the output

```
Required?                    true
Position?                    3
Default value
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `-OutputFile <string>` 
Output as file path

```
Required?                    false
Position?                    3
Default value
Accept pipeline input?       false
Accept wildcard characters?  false
```

#### `<CommonParameters>` 
This cmdlet supports the common parameters: Verbose, Debug,
ErrorAction, ErrorVariable, WarningAction, WarningVariable,
OutBuffer, PipelineVariable, and OutVariable. For more information, see
about_CommonParameters (https:/go.microsoft.com/fwlink/?LinkID=113216).

INPUTS
```
System.Xml.Linq.XDocument
```
Input as `XDocument` value

```
System.String
```
Input as file path


OUTPUTS
```
System.Xml.Linq.XDocument
```


----------  EXAMPLE 1  ----------

```
$xml = Write-OpenCoverComputedValues -InputFile "./_Reports/OpenCoverForPester/OpenCoverForPester.coverlet.xml" -Coverlet -Assembly $Assemblies -OutputFile "./_Packaging/OpenCoverForPester.coverlet.xml"
```

----------  EXAMPLE 2  ----------

```
$xml = Write-OpenCoverComputedValues -InputFile "./_Reports/OpenCoverForPester/OpenCoverForPester.xml"
```


