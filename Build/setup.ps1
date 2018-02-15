$nugetDir = Join-Path $PSScriptRoot "..\packages"
$project = Join-Path $PSScriptRoot dotnet-nuget.csproj
& dotnet restore --packages $nugetDir $project
& dotnet fake run ".\Build\prebuild.fsx"

$nugetPath = dir -recurse (Join-Path $nugetDir "nuget.exe") | % { $_.FullName } | Select-Object -First 1

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
"$fake" ".\Build\build.fsx" "%1"
exit /b %errorlevel%
"@
Set-Content -Value $bat -Path (Join-Path $SolutionRoot "fake.bat")

$lines = Get-Content .travis.yml
$root = Split-Path $PSScriptRoot -Parent
$fpath = $fake.Substring(1 + $root.Length).Replace("\","/")

$lines | % { 
    if ($_ -like "*packages/FAKE*") {
        $bits = $_.Split()
        $newbits = $bits | % {
                if ($_ -like "packages/FAKE*") { 
                    $fpath
                }
                else { $_ }
            }
        [string]::Join(" ", $newbits)
    }
    else { $_ }
} | Set-Content .travis.yml