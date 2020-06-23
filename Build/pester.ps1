param([string]$ACV="0.0.0.0")

Import-Module "./_Packaging/Module/tools/netcoreapp2.0/AltCover.PowerShell.dll"
Import-Module "./packages/pester/5.0.2/Pester.psm1"

Invoke-Altcover -?
# Invoke-Pester -Script @{ Path='.\Build'; Parameters = @{ ACV = $ACV}} -EnableExit -OutputFormat NUnitXml -OutputFile "./_Reports/PesterReport.xml"

$configuration = [PesterConfiguration]::Default
$configuration.Run.Path = '.\Build'
$configuration.Run.Exit = $true

$configuration.TestResult.Enabled = $true
# $configuration.TestResult.OutputFormat = "NUnit2.5"
$configuration.TestResult.OutputPath = "./_Reports/PesterReport.xml"

Invoke-Pester -Configuration $configuration