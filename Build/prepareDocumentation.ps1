Import-Module "./_Binaries/AltCover.PowerShell/Release+AnyCPU/net47/AltCover.PowerShell.dll"

## Documentation

$mdfiles = dir -Recurse "./_Documentation/AltCov*/*.md"

$mdfiles | % {
  $fromFile = $_.FullName
  Write-Host "Processing $fromFile"

  $toFile = $fromFile.Replace(".md", "-apidoc.md").Replace("`\_Documentation",".\docs")
  Write-Host "`tto $toFile"

  $wikidir = Split-Path $toFile
  mkdir -Force $wikidir | Out-Null 

  $lines = Get-Content $fromFile
  $lines | % { $_.Replace(".md)", "-apidoc)") } | Set-Content $toFile
}

$m = Get-Module -Name "AltCover.PowerShell"

$preamble = @"
This is the PowerShell Help/scripting use version of the ``AltCover.PowerShell.dll`` API; the .net programmable API documentation is [here](https://stevegilham.github.io/altcover/AltCover.PowerShell/AltCover.PowerShell-apidoc).  This functionality is present in all NuGet packages except the ``altcover.visualizer`` global tool package.

Use ``Import-Module`` either by specific path using the command given by ``AltCover.exe ImportModule`` (or ``dotnet AltCover.dll ImportModule`` or ``altcover ImportModule``), or add the appropriate directory to your ``PSModulePath`` and do a simple import.

The string output from ``AltCover ImportModule`` is intended for convenient cutting and pasting; it can be used in a script like
``````
`$ipmo = (AltCover ImportModule | Out-String).Trim().Split()[1].Trim(@('""'))
Import-Module `$ipmo
``````
which unpeels the wrapper around the file path.  Just substitute in the appropriate invocation for the ``AltCover``.

## Cmdlets
"@

$mdfile = "../altcover.wiki/PowerShell-integration.md" ## yes wiki

$preamble | Out-File -Encoding UTF8 $mdfile

$m.ExportedCmdlets.Keys | % {
    "* [$_](#$_)" | Out-File -Encoding UTF8 -Append $mdfile
}

" " | Out-File -Encoding UTF8 -Append $mdfile

$m.ExportedCmdlets.Keys | % { 
  $cmdletname = $_
  Write-Host "processing $_"
  $cmdlet = "./_Documentation/$($_).txt"
  Invoke-Expression ("Get-Help " + $_ + " -full") | Out-File -Encoding UTF8 $cmdlet
  $lines = Get-Content $cmdlet

  "###    $_"  | Out-File -Encoding UTF8 -Append $mdfile
  $cmdletname = $_

  $state="start"
  $nl = $false
  $closeBlock = $false
  $openBlock = $false

  $lines | % {
    $line = $_.Trim()

# state machine

  $header = ($line -eq "NAME") -or ($line -eq "SYNOPSIS") -or ($line -eq "SYNTAX") -or ($line -eq "DESCRIPTION")
  $header1 = ($line -eq "SYNOPSIS") -or ($line -eq "DESCRIPTION")
  $header2 = ($line -eq "NAME") -or ($line -eq "SYNTAX")

    if ($state -eq "start") {
      $nl = $header
      $closeBlock = $header1
      $openBlock = $header2
    }

    if (($line -eq "DESCRIPTION") -and ($state -eq "start")) {
      $state="description"
    }

    if ($line -eq "RELATED LINKS") {
      $state="related"
    }

    if (($line -eq "PARAMETERS") -and ($state -eq "description")) {
      $state="parameters"
    }

    if (($line -eq "INPUTS") -and ($state -eq "parameters")) {
      $state="io"
    }

    if ($line.StartsWith("----------  EXAMPLE")  -and ($state -eq "io")) {
      $state="example"
    }

# prefix    
    # use the change of state immediately above to our advantage
    if ($line.StartsWith("----------  EXAMPLE")  -and ($state -eq "example")) {
      $nl = $true
    }

    $closeBlock = $closeBlock -or 
      ($line.Contains($cmdletname)  -and ($state -eq "example")) -or  ## actually opens
      (($_ -match "^    \S") -and ($state -eq "io"))  -or
      (($line.StartsWith("Required?")) -and ($state -eq "parameters"))

    if ($closeBlock) {
      '```' | Out-File -Encoding UTF8 -Append  $mdfile
      $closeBlock = $false
    }

# echo    
    
    $decorate = (($line -like "-*") -and ($state -eq "parameters"))  -or
                (($line -like "<CommonParameters>") -and ($state -eq "parameters")) 

    if ($decorate) { $line = "#### ``$line`` "}

    if ($state -ne "related") {
      $line | Out-File -Encoding UTF8 -Append $mdfile
    }

# postfix    

    if ($nl) {
      '' | Out-File -Encoding UTF8 -Append  $mdfile
      $nl = $false
    }

    $openBlock = $openBlock -or
      ($line.Contains($cmdletname)  -and ($state -eq "example")) -or   ## actually closes
      (($_ -match "^    \S") -and ($state -eq "io")) -or
      (($line.StartsWith("Accept wildcard characters?")) -and ($state -eq "parameters"))

    if ($openBlock) {
      '```' | Out-File -Encoding UTF8 -Append  $mdfile
      $openBlock = $false
    }

  }
}

dir -recurse *.fsproj | % {
  $x = [xml](Get-Content $_.FullName) 
  $projectDir = Split-Path $_.FullName
  $name = "junk"

  $signatures = $x.project.ItemGroup.Compile.Include | ? { $_ -like "*.fsi" }
  if ($signatures) {  
    $name = $x.project.propertygroup.assemblyname | Select-Object -First 1
    mkdir -Force "./_Documentation/$name" | Out-Null 
    mkdir -Force "./docs/$name" | Out-Null 
    }

  $globals = $x.project.propertygroup.GlobalDefineConstants.'#text'
  if (-not $globals) { $globals = @() + $x.project.propertygroup.GlobalDefineConstants  | ? { $_ }}
  
  if ($globals) {
    $globals = [string]::join(";", $globals).Split(";") | Select-Object -Unique | ? { $_ }
    $globals = $globals | % { "/D " + $_ + "=1" }
    $globals = [string]::join(" ", $globals) + " /DDOCUMENTATION=1"
  }
  else {
    $globals = ""
  }

  $signatures | % {
    $sigpath = Join-Path $projectDir $_
    Write-Host "Processing $sigpath"
    $signame = (Split-Path $sigpath -Leaf).replace(".fsi", ".i")
    $iFile = Join-Path (Join-Path "./_Documentation" $name) $signame
    Write-Host "`tto $ifile"
    Write-Host "`twith $globals"
    $command = "& cl $globals /C /EP $sigpath > $ifile"
    Write-Host $command
    Invoke-Expression $command
    $lines = Get-Content $ifile | ?  { -not $_.Contains("///") } # skip XML doc comments
    $append = "<!-- DO NOT EDIT: generated by ./Build/prepareDocumentation.ps1 for $(Resolve-Path $sigpath -Relative) -->"
    $lines = ,$append + $lines
    $docfile = (Resolve-Path $iFile).Path.Replace(".i", "-fsapidoc.md").Replace("`\_Documentation","\docs")
    $lines | % { $_.Replace("// ", "").Replace("//", "")} | Set-Content $docFile
  }
}

# dotnet test integration

$header = @"
Available from release 3.0.488, this parallels the facility in [coverlet](https://github.com/tonerdo/coverlet); it is equivalent to doing ``AltCover --inplace --save`` in the project output directory before the tests are run, then ``AltCover Runner --collect`` after, then deleting the instrumented files and moving the saved originals back in place.

[There is an API available](https://stevegilham.github.io/altcover/AltCover.DotNet/DotNet-apidoc) for use with build scripting that composes the appropriate command line.

_Note: With ``AltCover``, there is no requirement that the assembly from which coverage is gathered be distinct from the assembly that contains the tests._  Use ``/p:AltCoverAssemblyExcludeFilter`` if you want to exclude the unit tests from coverage.


## To Use this option
Install into your test project 
``````
dotnet add package AltCover
``````
(or ``package altcover.api`` from release 4.0.600 or ``package altcover.dotnet`` from release 3.5.550 to 6.8.761) and switch on to run
``````
dotnet test /p:AltCover=true
``````

In the default operation it will produce OpenCover format output in a file ``coverage.xml`` (or in the case of multiple target frameworks, and from release 4.0.600, files of the form ``coverage.`$(TargetFramework).xml`` for each such target) in your project directory.
And if you want more control over what happens to the files, then is is still possible to use ``AltCover`` in its original, explicit, mode -- just don't use the ``/p:AltCover=true`` switch.

## Other parameters

"@

$footer = @"

**Note**: The pipe character ``|`` is used as a separator because the previous choice of ``;`` didn't play nice with MSBuild. In v6.0.700 or later, to introduce a ``|`` into a regex, escape it in by doubling (``||``); if a triplet ``|||`` or longer is present, doubling gets grouped from the left.  Sample use : ``/p:AltCoverAssemblyExcludeFilter='^(?!(NameA||NameB)).*`$'`` to include only ``NameA`` or ``NameB``.

**Note**: As MSBuild informational output is suppressed by default with ``dotnet test``, and log verbosity has no fine-grained control, the ``-v m`` (``--verbosity minimal``) option is needed to show the progress and summary information for the instrumentation and collection process if this is desired.

**Note**: In the case of multiple target frameworks the framework identifier will be inserted ahead of the extension (if any) of the file name given in ``/p:AltCoverXmlReport`` just as for the default ``coverage.xml`` name.

## Example
``````
dotnet test /p:AltCover=true /p:AltCoverXmlreport=".\altcover.xml" /p:AltCoverAssemblyFilter=NUnit
``````
Chooses a different report name, and excludes the ``NUnit3.TestAdapter`` assembly that comes with its pdb files, and gets instrumented by default.
"@

$mdfile = "../altcover.wiki/``dotnet-test``-integration.md" ## yes wiki

$header | Out-File -Encoding UTF8 $mdfile

$lines = (Get-Content  ".\AltCover.DotNet\DotNet.fs") | ? { $_.Contains("//=") }
$lines | % {
  $line = $_
  if ($line.StartsWith("//")) { $line = $line.Substring(2) }
  $parts = $line.Split("//")
  $parts2 = $parts[0].Split("`"")
  
  $comment = $parts | Select-Object -Last 1
  
  $compose = "* ``/p:AltCover" + $parts2[1] + $comment ##$parts[1]
  $compose | Out-File -Encoding UTF8 -Append $mdfile
}

$footer | Out-File -Encoding UTF8 -Append $mdfile




