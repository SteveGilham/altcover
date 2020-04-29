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

$preamble | Out-File -Encoding UTF8 "./_Documentation/PowerShell-integration.md"

$m.ExportedCmdlets.Keys | % {
    "[$_](#$_)" | Out-File -Encoding UTF8 -Append "./_Documentation/PowerShell-integration.md"
}

" " | Out-File -Encoding UTF8 -Append "./_Documentation/PowerShell-integration.md"

$m.ExportedCmdlets.Keys | % { 
  "###    $_"  | Out-File -Encoding UTF8 -Append "./_Documentation/PowerShell-integration.md"
  Invoke-Expression ("Get-Help " + $_ + " -full") | Out-File -Encoding UTF8 -Append "./_Documentation/PowerShell-integration.md"
  " " | Out-File -Encoding UTF8 -Append "./_Documentation/PowerShell-integration.md"
}