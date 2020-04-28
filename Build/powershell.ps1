param([string]$ACV="0.0.0.0")

Import-Module "./_Packaging/Unpack/tools/net45/AltCover.PowerShell.dll"
Import-Module "./packages/pester/4.10.1/tools/Pester.psm1"

Invoke-Altcover -?
Invoke-Pester -Script @{ Path='.\Build'; Parameters = @{ ACV = $ACV}} -EnableExit -OutputFormat NUnitXml -OutputFile "./_Reports/PoshReport.xml"

$m = Get-Module -Name "AltCover.PowerShell"

$preamble = @"
Use ``Import-Module`` either by specific path using the command given by ``AltCover.exe ImportModule`` (or ``dotnet AltCover.dll ImportModule`` or ``altcover ImportModule``), or add the appropriate directory to your ``PSModulePath`` and do a simple import.

The string output from ``AltCover ImportModule`` is intended for convenient cutting and pasting; it can be used in a script like
``````
`$ipmo = (AltCover ImportModule | Out-String).Trim().Split()[1].Trim(@('""'))
Import-Module `$ipmo
``````
which unpeels the wrapper around the file path.  Just substitute in the appropriate invocation for the ``AltCover``.

## Cmdlets
"@

$text

$comments = @{
"Add-Accelerator" = @"
Add one or more type abbreviations, like the built-in ``[xml]`` for ``System.Xml.XmlDocument``.  Two common abbreviations are supplied as switch parameters, and then others can be added free-form
"@;
"Get-Accelerator" = @"
List all type abbreviations, like the built-in ``[xml]`` for ``System.Xml.XmlDocument``.
"@;
"Compress-Branching" = @"
Takes output from the OpenCover program, and adjust from OpenCover's liberal idea of significant branches towards AltCover's more restricted approach -- chose either or both of ``-SameSpan`` to unify branches that go from the same start, and take the same trajectory to the same end (OpenCover issue #786 being one instance of this) and ``-WithinSequencePoint`` to remove branches interior to a statement (compiler generated things like stashing of lambdas, the hidden conditional ``Dispose()`` after a ``using``, or inside F# inlines -- OpenCover issues #657, #807 being instances of this).

Either takes an ``XDocument`` from the pipeline or from a file; emits the result as an ``XDocument`` to the pipeline and optionally to a file.
"@;
"ConvertTo-XmlDocument" = @"
Takes an ``XDocument`` in and puts an ``[xml]`` to the object pipeline
"@;
"ConvertTo-XDocument" = @"
Takes an ``[xml]`` in and puts an ``XDocument`` to the object pipeline
"@;
"ConvertTo-Lcov" = @"
Takes either OpenCover or classic NCover format input as an ``XDocument``, as an argument or from the object pipeline.
Writes the Lcov report to a file
"@;
"ConvertTo-Cobertura" = @"
Takes either OpenCover or classic NCover format input as an ``XDocument``, as an argument or from the object pipeline.
Writes the Cobertura report to the object pipeline as an ``XDocument``, and optionally to a file
"@;
"ConvertTo-NCover" = @"
Takes OpenCover format input either as an ``XDocument`` from the object pipeline or from a file.
Writes the classic NCover format report to the pipeline as an ``XDocument``, and, optionally, to a file.
"@;
"ConvertFrom-NCover" = @"
Takes classic NCover format input either as an ``XDocument`` from the object pipeline or from a file.
Writes the OpenCover format report to the pipeline as an ``XDocument``, and, optionally, to a file.  The report will contain data for the assemblies listed as the ``-Assembly`` argument and that are in the NCover input.
"@;
"Format-FromCoverletOpenCover" = @"
Takes a coverage report in ``coverlet``'s OpenCover dialect, and fills in most of the gaps, particularly giving somewhat meaningful start and end column values for its line-based paradigm, as well as summaries, npath coverage, branch exits and other such details.
"@;
"ConvertTo-BarChart" = @"
Generates an HTML report, based on the old NCover 1.5.8 XSLT, for both NCover and OpenCover coverage format data.  The input is as a file name or an ``XDocument`` from the pipeline, the output is to the pipeline as an ``XDocument``, and, optionally, to a file. 
"@;
"Invoke-AltCover" = @"
The arguments parallel the command-line executable; the main difference is that ``-Runner`` is a switch parameter that selects the runner-mode parameter set if present.

The "working directory" used where mentioned in the parameter descriptions is the current set location in PowerShell, **_not_** the underlying current directory.

The ``Summary`` enumeration values are
* ``Default`` -- OpenCover format
* ``R`` -- TeamCity with R for bRanch
* ``B`` -- TeamCity with B for Block representing branch coverage
* ``RPlus`` -- ``R`` + ``Default``
* ``BPlus`` -- ``B`` + ``Default``

The ``ShowStatic`` enumeration values are
* ``KeepHidden`` -- default, don't show code ignored in static analysis
* ``Mark`` -- show code ignored in static analysis with a special negative visit count if not visited
* ``Reveal`` -- treat the code as normal

**Note**: As Powershell informational output is suppressed by default in PowerShell 5+, the ``-InformationAction Continue`` option is needed to show the progress and summary information for the process if this is desired.

Summary information is also written to the object pipeline.

**Note**: ``-WhatIf`` includes validation for the command line arguments.  It is ignored for the purely read-only ``-Version`` option 
"@;
}

$preamble | Out-File "./_Documentation/PowerShell-integration.md"

$m.ExportedCmdlets.Keys | % {
    "[$_](#$_)" | Out-File -Append "./_Documentation/PowerShell-integration.md"
}

" " | Out-File -Append "./_Documentation/PowerShell-integration.md"

$m.ExportedCmdlets.Keys | % { 
  "###    $_"  | Out-File -Append "./_Documentation/PowerShell-integration.md"
  '```' | Out-File -Append "./_Documentation/PowerShell-integration.md"
  Invoke-Expression ($_ + " -?") | Out-File -Append "./_Documentation/PowerShell-integration.md"
  '```' | Out-File -Append "./_Documentation/PowerShell-integration.md"
  " " | Out-File -Append "./_Documentation/PowerShell-integration.md"
  if (-not $comments[$_]) { throw "No documentation for $_" }
  $comments[$_]  | Out-File -Append "./_Documentation/PowerShell-integration.md"
  " " | Out-File -Append "./_Documentation/PowerShell-integration.md"
}