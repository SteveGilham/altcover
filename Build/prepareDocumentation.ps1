## TODO -- AltCover => AltCover.Engine

Import-Module "./_Binaries/AltCover.PowerShell/Release+AnyCPU/netstandard2.0/AltCover.PowerShell.dll"

## clear first
dir -recurse "./docs/*apidoc.md" | del -force
dir -recurse "./docs/*.fsx.lock" | del -force

## Documentation

### C# generated

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

### Cmdlet help

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

$mdfile = "./docs/PowerShell-integration.md"

$preamble | Out-File -Encoding UTF8 $mdfile

$m.ExportedCmdlets.Keys | Sort-Object | % {
    "* [$_](#$($_.ToLowerInvariant()))" | Out-File -Encoding UTF8 -Append $mdfile
}

" " | Out-File -Encoding UTF8 -Append $mdfile

$m.ExportedCmdlets.Keys | Sort-Object | % { 
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

### F# generated

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

### dotnet test integration

$header = @"
Available from release 3.0.488, this parallels the facility in [coverlet](https://github.com/tonerdo/coverlet); it is equivalent to doing ``AltCover --inplace --save`` in the project output directory before the tests are run, then ``AltCover Runner --collect`` after, then deleting the instrumented files and moving the saved originals back in place.

[There is an API available](AltCover.DotNet/) for use with build scripting that composes the appropriate command line.

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

$mdfile = "./docs/``dotnet-test``-integration.md"

$header | Out-File -Encoding UTF8 $mdfile

$lines = (Get-Content  ".\AltCover.DotNet\DotNet.fs") | ? { $_.Contains("//=") }
$lines | % {
  $line = $_
  if ($line.StartsWith("//")) { $line = $line.Substring(2) }
  
  $strike = $line.IndexOf("//")
  $comment = $line.Substring($strike + 2)
  $parts = $line.Substring(0, $strike)
  $parts2 = $parts.Split("`"")
  
  
  $compose = "* ``/p:AltCover" + $parts2[1] + $comment 
  $compose | Out-File -Encoding UTF8 -Append $mdfile
}

$footer | Out-File -Encoding UTF8 -Append $mdfile

### Usage

$mdfile = "./docs/Usage.md"

$header = @"
This is the command-line usage

* For use cases, see [Use Cases](https://github.com/SteveGilham/altcover/wiki/Use-Cases).  
* For modes of operation, see [Modes of Operation](https://github.com/SteveGilham/altcover/wiki/Modes-of-Operation).  
* For driving AltCover from `dotnet test`, see [`dotnet test` integration](%60dotnet-test%60-integration).  
* For driving AltCover from MSBuild, see [MSBuild Tasks](MSBuild-tasks).  
* For driving AltCover and associated tools with Windows PowerShell or PowerShell Core, see [PowerShell integration](PowerShell-integration).  

The full command line is 
``````
"@

$header | Out-File -Encoding UTF8 $mdfile

$resources = [xml](Get-Content "./AltCover.Engine/Strings.resx")

$helptext = $resources.root.data | ? { $_.name -eq "HelpText" } | % { $_.value }

$helptext = $helptext.Split("`r`n`r`n") | ? { 
  $_.Trim().Length } | ? {
    -not( $_.contains("http") ) 
  } | % { $_ | Out-File -Encoding UTF8 -Append $mdfile }

$glue = @"
``````

In detail
``````
"@

$glue | Out-File -Encoding UTF8 -Append $mdfile

Get-Content "./Tests/AltCover.Usage.txt" | % { $_ | Out-File -Encoding UTF8 -Append $mdfile }

$glue = @"
-- ...                 Anything on the command line after a free-standing "--" is considered a separate command line to be executed after the instrumentation has been done.
``````
or
``````
"@

$glue | Out-File -Encoding UTF8 -Append $mdfile

Get-Content "./Tests/AltCover.Runner.Usage.txt" | % { $_  | Out-File -Encoding UTF8 -Append $mdfile }


$footer = @"
-- ...                 Anything on the command line after a free-standing "--" is considered arguments for the executable to run.
``````
or
``````
  ImportModule               Prints out the PowerShell script to import the associated PowerShell module
``````
or
``````
  Version                    Prints out the AltCover build version
``````
or, for the global tool only
``````
  TargetsPath                Prints out the path to the 'altcover.global.targets' file
                               (as the tool cannot be 'dotnet add'ed to the project).
                               The 'altcover.global.props' file is present in the same directory
``````

### Notes

* If the ``--localSource`` argument is given, the process checks the first source file path that it finds in the debug symbols for each assembly, and if that file is not found on the current machine, the assembly is treated as if the symbols file did not exist.

* The ``--dependency`` argument will expand environment variables in the paths from release 4.0.653; %USERPROFILE%, %ProgramFiles% and %NUGET_PACKAGES% are likely to be the most useful here, e.g. ``%ProgramFiles%/dotnet/shared/Microsoft.AspNetCore.App/2.1.5/Microsoft.AspNetCore.Cryptography.KeyDerivation.dll`` or similar to pick up ASP.Net Core assemblies needed for type references

* The ``--callContext`` argument is only used with ``--opencover``, otherwise it has no effect.  Tracked methods are recorded at instrumentation time, but call context information is only recorded in runner mode, including ``runner --collect``, which allows for heavier processing after the process terminates than the ``ProcessExit`` handler.

* In runner mode, exactly one of a command to be executed (``-x``) or the ``--collect`` option is required.  If ``--collect`` is supplied then anything after a free-standing ``--`` is ignored.

* In the ``runner -x`` case, before the executable is launched AltCover creates a file in the same folder as the previously nominated XML report, and while that file is present, the raw coverage data are written there, rather than being held in memory.  After the executable terminates, the data are read and processed into the XML report.  This file is the same one written by ``altcover --save``

* Before v6.0 the strong-name key arguments (``-k``, ``--sn``) were not available in the .net core build.  At v6.0, AltCover includes a work-around for what was a netstandard1.3 limitation in Mono.Cecil (suitable APIs weren't there), in anticipation of this being included formally in a future Mono.Cecil 0.11 release with netstandard2.0 support.

* Filter values are semi-colon (``;``) separated regular expressions, applied by type in the order as they are defined in the command line; any item whose name matches the expression will be excluded from the coverage reporting.  In the simplest case, with no special regex items, this means that a name containing the filter item as a sub-string will be excluded.  In v6.0.700 or later, should the need ever arise to have a semi-colon in a regex, then escape it in by doubling (``;;``); if a triplet ``;;;`` or longer is present, doubling gets grouped from the left.

* Except where being driven by AltCover in "runner" mode, coverage statistics are written to the file nominated by the ``x|xmlReport=`` parameter as instrumented assemblies are unloaded from an executing AppDomain, even if this is days or weeks later.  In practice the instrumented assemblies should be deleted after the relevant testing has been run, and the report file will thus be freed up.

* valid arguments for ``--teamcity`` are ``B``, ``R``, ``+B``, ``+R`` or nothing at all (same as ``B``).  The letter indicates which symbol to use in the TeamCity format for branch coverage (``B`` is for ``Block``, which by experiment did show in the build report, and ``R`` is for ``bRanch`` which is documented, but did not show when I tried it), the optional ``+`` indicates that the OpenCover summary should also be emitted.

* valid arguments for ``--defer`` are ``+``, ``-`` or nothing at all (same as ``+``).  ``+`` keeps coverage data in memory until process exit, ``-`` writes promptly to file in runner mode, i.e. acts as in previous releases since 1.6).

* ``ImportModule`` will print a string on Mono/non-Windows, but Windows PowerShell won't be there to make use of it.

#### ``-e`` vs ``-s`` : what gives?

In the case where you have Unit Tests => Code Under Test => Libraries, and the coverage of the unit tests is not of interest, then exclude those assemblies with ``-e``, so their dependencies get rewritten, but their contents are not instrumented or added to the coverage file.  The stable libraries consumed by the code under test (and stable libraries for the unit test framework) should be marked as ``-s`` to be left untouched.

#### ``-p`` vs ``-f`` : what gives?

The distinction is that a single ``-p`` can exclude a folder ```$(SolutionRoot)Generated`` (though it has to be specified with manual expansion of the MSBuild variable) and everything in it, while a single ``-f`` can be used to exclude all ``.g.cs`` files wherever they are. 

#### ``Runner`` mode : what gives?

There are three stages to the coverage operation -- instrumenting the code, exercising it, then presenting the results.  Classic ``altcover`` does the first, and as part of the process, injects the result presentation code into the system under test, to be run when the test process exits (as an exit handler).

This essentially doesn't work with ``dotnet test`` in .net core (the exit handler doesn't have enough time to work), so the ``runner`` mode was introduced to wrap the test processing, catching coverage data as it was generated, through a named pipe connection, and then doing the presentation work after the tests completed (release 1.5).  The named pipe implementation proved unreliable as a cross-platform mechanism, so a file-based mechanism was adopted instead (release 1.6) that does work reliably in all places.

With the data being buffered to disk, rather than being stored entirely in memory in one or other process, the ``--save`` and ``runner --collect`` options were introduced (release 3.0) to do, respectively, the signalling to the system under test that it should dump live to an intermediate file, and the translation of the intermediate information into the final report, with the testing to be done at some point between, rather than having to run the tests through ``altcover runner``.

In release 5.3, the writing the collected data has been offloaded to an in-process data handler, which removes this constraint; the data collector is automatically hooked into the MSBuild process.
"@

$footer | Out-File -Encoding UTF8 -Append $mdfile

### docs/AltCover.Fake.DotNet.Testing.AltCover/index.md

$lines = Get-Content "./Build/AltCover.Fake.DotNet.Testing.AltCover.md"
$lines | % {
  if ($_ -like "!!*") {
    Get-Content ($_.Substring(2))
  }
  else { $_ }
} | Set-Content "./docs/AltCover.Fake.DotNet.Testing.AltCover/index.md"

### docs/Fake-and-Cake-integration.md

$lines = Get-Content "./Build/Fake-and-Cake-integration.md"
$lines | % {
  if ($_ -like "!!*") {
    Get-Content ($_.Substring(2))
  }
  else { $_ }
} | Set-Content "./docs/Fake-and-Cake-integration.md"


##-----------------------------------------

Write-Host "In node.js prompt, 'harp server C:\Users\steve\Documents\GitHub\altcover\docs'"
Write-Host ""
Write-Host "Touch test examples like"
Write-Host "dotnet fake run .\docs\AltCover.Fake.DotNet.Testing.AltCover\BuildSample_1.fsx"
Write-Host "dotnet fake run .\docs\AltCover.Fake.DotNet.Testing.AltCover\BuildSample_2.fsx"
Write-Host "dotnet fake run .\docs\BuildSample_1.fsx"








