param([string]$ACV="0.0.0.0", [string]$ReportName="!:!", [string]$FolderName="!:!")

Import-Module "./_Packaging/$($FolderName)/tools/netcoreapp2.0/AltCover.PowerShell.dll"
Import-Module "./packages/pester/5.0.4/tools/Pester.psm1"

Invoke-Altcover -?
# Invoke-Pester -Script @{ Path='.\Build'; Parameters = @{ ACV = $ACV}} -EnableExit -OutputFormat NUnitXml -OutputFile "./_Reports/$($ReportName).xml"

$configuration = [PesterConfiguration]::Default
$configuration.Run.Path = '.\Build'
$configuration.Run.Exit = $true

$configuration.TestResult.Enabled = $true
# $configuration.TestResult.OutputFormat = "NUnit2.5"
$configuration.TestResult.OutputPath = "./_Reports/$($ReportName).xml"

Invoke-Pester -Configuration $configuration