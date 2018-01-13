$nugetDir = Join-Path $PSScriptRoot "..\ThirdParty\nuget"
$nugetPath = Join-Path $nugetDir "nuget.exe"
if (-not (Test-Path $nugetPath)) {
    mkdir $nugetDir  | Out-Null
    $sourceNugetExe = "https://dist.nuget.org/win-x86-commandline/latest/nuget.exe"
    Invoke-WebRequest $sourceNugetExe -OutFile $nugetPath
}
$solutionRoot = Resolve-Path (Join-Path $PSScriptRoot "..")
$solution = Join-Path $SolutionRoot "AltCover.sln"
& $nugetPath restore $solution
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

$fakelib = (dir -recurse "$solutionRoot/*akeLib.dll") | % { $_.FullName } | Select-Object -First 1
$lintlib = (dir -recurse "$solutionRoot/*SharpLint.Fake.dll") | % { $_.FullName } | Select-Object -First 1
$mdlib = (dir -recurse "$solutionRoot/*Sharp.Markdown.dll") | % { $_.FullName } | ? { $_ -like "*net40*" } | Select-Object -First 1
$ylib = (dir -recurse "$solutionRoot/*amlDotNet.dll") | % { $_.FullName } | ? { $_ -like "*net35*" } | Select-Object -First 1

$build = @"
#I "$((Split-Path -Parent $fakelib).Replace('\', '/'))" // include Fake lib
#r "FakeLib.dll"
#I "$((Split-Path -Parent $lintlib).Replace('\', '/'))"
#r "FSharpLint.Fake.dll"
#I "$((Split-Path -Parent $mdlib).Replace('\', '/'))"
#r "FSharp.Markdown.dll"
#I "$((Split-Path -Parent $ylib).Replace('\', '/'))"
#r "YamlDotNet.dll"
#r "System.IO.Compression.FileSystem.dll"
#r "System.Xml"
#r "System.Xml.Linq"

#load "actions.fsx"
#load "targets.fsx"
"@
Set-Content -Value $build -Path (Join-Path $SolutionRoot ".\build\build.fsx")
