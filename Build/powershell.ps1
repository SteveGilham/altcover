Import-Module "./_Packaging/Unpack/tools/net45/AltCover.PowerShell.dll"
Import-Module "./packages/Pester.4.3.1/tools/Pester.psm1"

Invoke-Altcover -?
Invoke-Pester -Script .\Build -EnableExit -OutputFormat NUnitXml -OutputFile "./_Reports/PoshReport.xml"