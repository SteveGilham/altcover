param([string]$ACV="0.0.0.0", [string]$ReportName="!:!", [string]$FolderName="!:!", [string]$PesterVersion="0.0.0")

Import-Module (Join-Path $($FolderName) "AltCover.PowerShell.dll")
Invoke-Altcover -?

Import-Module "./packages/pester/$($PesterVersion)/tools/Pester.psm1"
$configuration = [PesterConfiguration]::Default
$configuration.Run.Path = '.\Build'
$configuration.Run.Exit = $true
$configuration.TestResult.Enabled = $true
$configuration.TestResult.OutputPath = "./_Reports/$($ReportName).xml"
Invoke-Pester -Configuration $configuration