Import-Module "./_Binaries/AltCover.PowerShell/Release+AnyCPU/net47/AltCover.PowerShell.dll"

## Documentation

$mdfiles = dir -Recurse "./_Documentation/AltCov*/*.md"

$mdfiles | % {
  $fromFile = $_.FullName
  Write-Host "Processing $fromFile"

  $toFile = $fromFile.Replace(".md", "-apidoc.md").Replace("`\_Documentation",".wiki")
  Write-Host "`tto $toFile"

  $lines = Get-Content $fromFile
  $lines | % { $_.Replace(".md)", "-apidoc)") } | Set-Content $toFile
}

$m = Get-Module -Name "AltCover.PowerShell"

$preamble = @"
This is the PowerShell Help/scripting use version of the ``AltCover.PowerShell.dll`` API; the .net programmable API documentation is [here](AltCover.PowerShell/AltCover.PowerShell-apidoc).  This functionality is present in all NuGet packages except the ``altcover.visualizer`` global tool package.

Use ``Import-Module`` either by specific path using the command given by ``AltCover.exe ImportModule`` (or ``dotnet AltCover.dll ImportModule`` or ``altcover ImportModule``), or add the appropriate directory to your ``PSModulePath`` and do a simple import.

The string output from ``AltCover ImportModule`` is intended for convenient cutting and pasting; it can be used in a script like
``````
`$ipmo = (AltCover ImportModule | Out-String).Trim().Split()[1].Trim(@('""'))
Import-Module `$ipmo
``````
which unpeels the wrapper around the file path.  Just substitute in the appropriate invocation for the ``AltCover``.

## Cmdlets
"@

$mdfile = "../altcover.wiki/PowerShell-integration.md"

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