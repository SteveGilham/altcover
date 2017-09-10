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

## establish vsvars
$key = "HKLM:\SOFTWARE\Microsoft\VisualStudio\SxS\VS7"
if (-not (Test-Path $Key))
{
    $key = "HKLM:\SOFTWARE\Wow6432Node\Microsoft\VisualStudio\SxS\VS7"  
}
$VsKey = get-ItemProperty $key
$version = "15.0"
$VsInstallPath = $VsKey.$version
$VcTools = Join-Path $VsInstallPath "Common7\Tools"
$BatchFile = Resolve-Path (Join-Path $VcTools VsDevCmd.bat)
##$path = (($env:path).Split(";") | Select-Object -Unique) -join ";"
$bat = @"
@echo off
IF '"%VS150COMNTOOLS%"' == '""' CALL "$batchFile"
SET PATH="$($env:path);$(Split-Path -Parent $fake)"
"$fake" ".\build\build.fsx" "%1"
exit /b %errorlevel%
"@
Set-Content -Value $bat -Path (Join-Path $SolutionRoot "fake.bat")