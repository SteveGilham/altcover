Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
Set-Location -Path $PSScriptRoot

dotnet tool restore
dotnet run --project (Join-Path $PSScriptRoot 'Build/Setup.fsproj')
dotnet run --project (Join-Path $PSScriptRoot 'Build/Build.fsproj')