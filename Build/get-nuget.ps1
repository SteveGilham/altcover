$nugetDir = Join-Path $PSScriptRoot "..\ThirdParty\nuget"
$nugetPath = Join-Path $nugetDir "nuget.exe"
if (-not (Test-Path $nugetPath)) {
    mkdir $nugetDir  | Out-Null
    $sourceNugetExe = "https://dist.nuget.org/win-x86-commandline/latest/nuget.exe"
    Invoke-WebRequest $sourceNugetExe -OutFile $nugetPath
}
$solutionRoot = Resolve-Path (Join-Path $PSScriptRoot "..")
& $nugetPath restore $solutionRoot
$fake = (dir -recurse "$solutionRoot\*ake.exe") | % { $_.FullName } | Select-Object -First 1
$bat = @"
@echo off
SET PATH="$($env:path);$(Split-Path -Parent $fake)"
"$fake" "%1" "%2"
exit /b %errorlevel%
"@
Set-Content -Value $bat -Path (Join-Path $SolutionRoot "fake.bat")