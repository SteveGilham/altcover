param([string]$ACV="0.0.0.0")
Import-Module "./_Packaging/Module/tools/netcoreapp2.0/AltCover.PowerShell.dll"
Import-Module "./packages/Pester.4.4.1/tools/Pester.psm1"

Invoke-Altcover -?
Invoke-Pester -Script @{ Path='.\Build'; Parameters = @{ ACV = $ACV}} -EnableExit -OutputFormat NUnitXml -OutputFile "./_Reports/PesterReport.xml"