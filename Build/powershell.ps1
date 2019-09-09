param([string]$ACV="0.0.0.0")

Import-Module "./_Packaging/Unpack/tools/net45/AltCover.PowerShell.dll"
Import-Module "./packages/Pester.4.9.0/tools/Pester.psm1"

Invoke-Altcover -?
Invoke-Pester -Script @{ Path='.\Build'; Parameters = @{ ACV = $ACV}} -EnableExit -OutputFormat NUnitXml -OutputFile "./_Reports/PoshReport.xml"