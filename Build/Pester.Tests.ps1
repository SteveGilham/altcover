Get-ChildItem "./_Packaging/*.*" | % { if ( -not($_.Name -like "*.nupkg")) { Remove-Item -force $_.FullName }}

$accel = @{ "minfo" = [type]::gettype("System.Reflection.MethodInfo"); "pinfo" =  [type]::gettype("System.Type").GetProperty("FullName")}
Add-Accelerator -Accelerator -Xdocument -Mapping $accel

Describe "Get-Accelerator" {
  It "Has the expected values" {
    $a = Get-Accelerator
    $a["xdoc"].FullName | Should -Be "System.Xml.Linq.XDocument"
    $a["accelerators"].FullName | Should -Be "System.Management.Automation.TypeAccelerators"
    $a["minfo"].FullName | Should -Be "System.Reflection.MethodInfo"
    $a["pinfo"].FullName | Should -Be "System.Reflection.RuntimePropertyInfo"
    $a["xml"].FullName | Should -Be "System.Xml.XmlDocument"
    $a.Count | Should -BeGreaterThan 3
  }
}

Describe "Add-Accelerator" {
  It "Accepts WhatIf" {

    Start-Transcript -Path "./_Packaging/AccelWhatIf.txt"
    try
    {
      $accel = @{ "minfo" = [type]::gettype("System.Reflection.MethodInfo"); "pinfo" =  [type]::gettype("System.Type").GetProperty("FullName")}
      Add-Accelerator -WhatIf
      Add-Accelerator -Accelerator -Xdocument -Mapping $accel -WhatIf
    }
    catch
    {
        Write-Host $_.Exception
    }
    Stop-Transcript
    $expected = [string]::Join([System.Environment]::NewLine, 
                ('What if: Performing the operation "Add-Accelerator" on target "Command Line : ".',
                 'What if: Performing the operation "Add-Accelerator" on target "Command Line :  -Mapping @{"minfo" = "System.Reflection.MethodInfo"; "pinfo" = "System.Reflection.RuntimePropertyInfo"} -XDocument -Accelerator".'))

    $lines = Get-Content "./_Packaging/AccelWhatIf.txt"
    $ll = $lines | ? { $_ -like "What if: *" }
    [string]::Join([System.Environment]::NewLine, $ll) | Should -Be $expected
  }
}

Describe "Invoke-Altcover" {
    It "instruments and collects" {
        $o = "./Samples/Sample2/_Binaries/Sample2/Debug+AnyCPU/net6.0"
        $x = "./_Reports/PesterFSharpTypesDotNetRunner.xml"
        $i = "./_Binaries/Sample2/Debug+AnyCPU/net6.0"
        if (Test-Path $o) {
            Remove-Item -Force -Recurse $o
        }
        if (Test-Path $x) { Remove-Item -force $x }

        Invoke-AltCover -Report $x -OutputDirectory  $o -InputDirectory $i -AssemblyFilter ("Adapter", "FSharp", "nunit") -ReportFormat NCover -InformationAction Continue
        $o | Should -Exist
        $x | Should -Exist
        $xm = [xml](Get-Content $x)
        [string]::Join(" ", $xm.coverage.module.method.name) | Should -Be "main returnFoo returnBar testMakeUnion as_bar get_MyBar Invoke .ctor makeThing testMakeThing bytes"
        $result = [string]::Join(" ", $xm.coverage.module.method.seqpnt.visitcount)
        $result | Should -Be "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
        $w = ""
        $format = @([AltCover.Commands.Summary]::O, [AltCover.Commands.Summary]::C)

        Invoke-AltCover -Runner -RecorderDirectory $o -SummaryFormat $format -WarningVariable w
        $xm = [xml](Get-Content $x)

        [string]::Join(" ", $xm.coverage.module.method.name) | Should -Be "main returnFoo returnBar testMakeUnion as_bar get_MyBar Invoke .ctor makeThing testMakeThing bytes"
        $result = [string]::Join(" ", $xm.coverage.module.method.seqpnt.visitcount)
        $result | Should -Be "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
        $w | Should -Be "A total of 0 visits recorded"

        $summary = Invoke-AltCover  -InformationAction Continue -Runner -RecorderDirectory $o -WorkingDirectory "./Samples/Sample2" -Executable "dotnet" -CommandLine @("test", "--no-build", "--configuration", "Debug", "--framework", "net6.0", "Sample2.fsproj")
        $xm2 = [xml](Get-Content $x)
        $result = [string]::Join(" ", $xm2.coverage.module.method.seqpnt.visitcount)
        $result | Should -Be "0 1 1 1 0 1 0 1 0 1 1 1 0 0 0 0 0 0 0 0 0 2 1 0 1 0 1"
        $result = $summary.Replace("`r", [String]::Empty).Replace("`n", "|") 
        $result | Should -Be "Visited Classes 4 of 7 (57.14)|Visited Methods 7 of 11 (63.64)|Visited Points 12 of 27 (44.44)|"
    }

    It "Fails on garbage" {
        $o = "./Samples/Sample2/_Binaries/Sample2/Debug+AnyCPU/net6.0"
        $x = "./_Reports/PesterFSharpTypesDotNetRunner.xml"
        try 
        {
          $ev = ""
          Invoke-AltCover -Report $x -OutputDirectory  $o -InputDirectory "./NoneSuch/xunit-dotnet/bin/Debug/netcoreapp2.0" -InPlace -ErrorVariable ev -ErrorAction SilentlyContinue
        }
        finally
        {
          $ev | Should -Be ("--inputDirectory : Directory ./NoneSuch/xunit-dotnet/bin/Debug/netcoreapp2.0 not found" +
                            [Environment]::NewLine +"255")
        }
    }

    It "Reports the version" {
        $version = Invoke-AltCover -Version -InformationAction Continue 6>&1
        $version.ToString().Trim() | Should -Be ("AltCover version " + $ACV)
    }

    It "Shows WhatIf" {
        $m = [AltCover.Commands.ShowHidden]::Reveal
        Start-Transcript -Path "./_Packaging/WhatIf.txt"
        Invoke-AltCover -WhatIf -ShowStatic "mark"
        Invoke-AltCover -WhatIf -ShowStatic $m
        Invoke-AltCover -Runner -RecorderDirectory "./Samples/Sample2" -WhatIf
        Stop-Transcript
        $expected = [string]::Join([System.Environment]::NewLine, 
                    ('What if: Performing the operation "Invoke-AltCover" on target "Command Line : altcover --reportFormat OpenCover --showstatic:+".',
                     'What if: Performing the operation "Invoke-AltCover" on target "Command Line : altcover --reportFormat OpenCover --showstatic:++ ".',
                     'What if: Performing the operation "Invoke-AltCover" on target "Command Line : altcover Runner -r ./Samples/Sample2 --collect".'))

        $lines = Get-Content "./_Packaging/WhatIf.txt"
        $ll = $lines | ? { $_ -like "What if: *" }
        [string]::Join([System.Environment]::NewLine, $ll) | Should -Be $expected
    }
}

Describe "ConvertTo-XDocument" {
    It "converts" {
        $xml = [xml](Get-Content "./AltCover.Tests/Sample1WithNCover.xml")
        $xd = $xml | ConvertTo-XDocument
        $xd | Should -BeOfType [xdoc]
        $header = $xd.Declaration.ToString().Replace(" standalone=`"`"", "") + "`n" 
        $sw = new-object System.IO.StringWriter @()
        $settings = new-object System.Xml.XmlWriterSettings @()
        $settings.Indent = $true
        $settings.IndentChars = "  "
        $xw = [System.Xml.XmlWriter]::Create($sw, $settings)
        $xml.WriteTo($xw)
        $xw.Close()
        ($header + $xd.ToString()).Replace("`r", "") | Should -Be $sw.ToString().Replace("`r", "")
    }

    It "Round Trips" {
        $xml = [xml]"<document/>"
        $xd = $xml | ConvertTo-XDocument
        $xd.ToString() | Should -Be $xml.OuterXml
        $x2 = $xd | ConvertTo-XmlDocument
        $x2.OuterXml | Should -Be $xml.OuterXml
    }
}

Describe "ConvertTo-XmlDocument" {
    It "converts" {
        $xd = [xdoc]::Load("./AltCover.Tests/Sample1WithNCover.xml")
        $xml = $xd | ConvertTo-XmlDocument
        $xml | Should -BeOfType "System.Xml.XmlDocument"
        $sw = new-object System.IO.StringWriter @()
        $settings = new-object System.Xml.XmlWriterSettings @()
        $settings.Indent = $true
        $settings.IndentChars = "  "
        $xw = [System.Xml.XmlWriter]::Create($sw, $settings)
        $xml.WriteTo($xw)
        $xw.Close()
        $header = $xd.Declaration.ToString() + "`n"
        $sw.ToString().Replace("`r", "") | Should -Be ($header + $xd.ToString()).Replace("`r", "")
    }
}

Describe "ConvertTo-Lcov" {
    It "Converts OpenCover Data" {
        ConvertTo-LCov -InputFile "./AltCover.Tests/HandRolledMonoCoverage.xml" -OutputFile "./_Packaging/OpenCover.lcov"
        $expected = [String]::Join("`n", (Get-Content "./AltCover.Tests/HandRolledMonoCoverage.lcov"))
        $got = [String]::Join("`n", (Get-Content "./_Packaging/OpenCover.lcov"))
        $got | Should -Be $expected.Replace("`r", "")
    }

  It "Converts NCover Data" {
      $lines = (Get-Content "./AltCover.Tests/Sample1WithNCover.xml") | % { $_.Replace('excluded="true"', 'excluded="false"')}
      $lines | Set-Content "./_Packaging/NCover.lcov.xml"
      ConvertTo-LCov -InputFile "./_Packaging/NCover.lcov.xml" -OutputFile "./_Packaging/NCover.lcov"
      $expected = [String]::Join("`n", (Get-Content "./AltCover.Tests/NCoverBugFix.lcov"))
      $got = [String]::Join("`n", (Get-Content "./_Packaging/NCover.lcov"))
      $got | Should -Be $expected.Replace("`r", "")
  }

  It "Converts Real NCover Data" {
    $ev = ""
    [xdoc]::Load("./AltCover.Tests/GenuineNCover158.Xml") | ConvertTo-LCov -OutputFile "./_Packaging/NCover158.lcov" -ErrorVariable ev
    $ev | Should -BeFalse
  }
}

Describe "ConvertTo-Cobertura" {
  It "Converts OpenCover Data" {
    $x = ConvertTo-Cobertura -InputFile "./AltCover.Tests/HandRolledMonoCoverage.xml" -OutputFile "./_Packaging/OpenCover.ConvertTo.cobertura"
    $coverage = $x.Descendants("coverage")
    $v = $coverage.Attribute("version").Value
    $t = $coverage.Attribute("timestamp").Value

    $expected = ([String]::Join("`n", (Get-Content "./AltCover.Tests/OpenCover.ConvertTo.cobertura"))).Replace("`$v", $v).Replace("`$t", $t)

    $got = [String]::Join("`n", (Get-Content "./_Packaging/OpenCover.ConvertTo.cobertura"))
    $got | Should -Be $expected.Replace("`r", "").Replace("\", [System.IO.Path]::DirectorySeparatorChar)

    $header = $x.Declaration.ToString() + "`n"
    ($header + $x.ToString()).Replace("`r", "") | Should -Be $expected.Replace("`r", "").Replace("\", [System.IO.Path]::DirectorySeparatorChar)
  }

  It "Converts NCover Data" {
    $lines = (Get-Content "./AltCover.Tests/Sample1WithNCover.xml") | % { $_.Replace('excluded="true"', 'excluded="false"')}
    $lines | Set-Content "./_Packaging/NCover.cob.xml"
    $x = ConvertTo-Cobertura -InputFile "./_Packaging/NCover.cob.xml" -OutputFile "./_Packaging/NCover.ConvertTo.cobertura"
    $coverage = $x.Descendants("coverage")
    $v = $coverage.Attribute("version").Value
    $t = $coverage.Attribute("timestamp").Value

    $expected = ([String]::Join("`n", (Get-Content "./AltCover.Tests/NCover.ConvertTo.cobertura"))).Replace("`$v", $v).Replace("`$t", $t)
    $got = [String]::Join("`n", (Get-Content "./_Packaging/NCover.ConvertTo.cobertura"))
    $got | Should -Be $expected.Replace("`r", "")

    $header = $x.Declaration.ToString() + "`n"
    ($header + $x.ToString()).Replace("`r", "") | Should -Be $expected.Replace("`r", "")
  }

  It "Converts With the pipeline" {
    $raw = [xdoc]::Load("./AltCover.Tests/Sample1WithNCover.xml")
    $raw.Descendants() | % {
      if ($_.Attribute("excluded")) { $_.Attribute("excluded").Value = "false" }
    }
    $x = $raw | ConvertTo-Cobertura
    $coverage = $x.Descendants("coverage")
    $v = $coverage.Attribute("version").Value
    $t = $coverage.Attribute("timestamp").Value

    $expected = [System.IO.File]::ReadAllText("./AltCover.Tests/Sample1WithNCover.cob.xml")
    $expected = $expected.Replace("{0}", $v).Replace("{1}", $t)
    $header = $x.Declaration.ToString() + "`n"
    ($header + $x.ToString()).Replace("`r", "") | Should -Be $expected.Replace("`r", "")
  }

  It "Converts Real NCover Data" {
    $ev = ""
    ConvertTo-Cobertura -InputFile "./AltCover.Tests/GenuineNCover158.Xml" -OutputFile "./_Packaging/NCover158.cobertura" -ErrorVariable ev
    $ev | Should -BeFalse
  }
}

Describe "ConvertTo-NCover" {
  It "converts" {
      $xml = ConvertTo-NCover -InputFile "./AltCover.Tests/HandRolledMonoCoverage.xml" -OutputFile "./_Packaging/HandRolledMonoNCover.xml"
      $xml | Should -BeOfType [xdoc]

      $sw = new-object System.IO.StringWriter @()
      $settings = new-object System.Xml.XmlWriterSettings @()
      $settings.Indent = $true
      $settings.IndentChars = "  "
      $xw = [System.Xml.XmlWriter]::Create($sw, $settings)
      $xml.WriteTo($xw)
      $xw.Close()
      $written = [System.IO.File]::ReadAllText("./_Packaging/HandRolledMonoNCover.xml")
      $result = [xml](Get-Content "./_Packaging/HandRolledMonoNCover.xml")
      $time = $result.coverage.startTime

      $expected = @"
<?xml version="1.0" encoding="utf-16"?>
<coverage profilerVersion="OpenCover" driverVersion="OpenCover" startTime="$time" measureTime="$time" xmlns:msxsl="urn:schemas-microsoft-com:xslt" xmlns:user="urn:my-scripts">
  <module moduleId="6A-33-AA-93-82-ED-22-9D-F8-68-2C-39-5B-93-9F-74-01-76-00-9F" name="Sample1.exe" assembly="Sample1" assemblyIdentity="Sample1">
    <method excluded="false" instrumented="true" name=".ctor" class="TouchTest.Program" fullname="System.Void TouchTest.Program::.ctor()" />
    <method excluded="false" instrumented="true" name="Main" class="TouchTest.Program" fullname="System.Void TouchTest.Program::Main(System.String[])">
      <seqpnt visitcount="1" line="11" column="3" endline="11" endcolumn="4" offset="0" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="1" line="12" column="23" endline="12" endcolumn="24" offset="1" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="1" line="13" column="4" endline="13" endcolumn="5" offset="7" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="1" line="13" column="12" endline="13" endcolumn="13" offset="9" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="1" line="14" column="4" endline="14" endcolumn="5" offset="24" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="1" line="15" column="5" endline="15" endcolumn="6" offset="25" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="1" line="15" column="60" endline="15" endcolumn="61" offset="36" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="1" line="15" column="13" endline="15" endcolumn="14" offset="46" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="1" line="16" column="4" endline="16" endcolumn="5" offset="51" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="0" line="18" column="4" endline="18" endcolumn="5" offset="57" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="0" line="19" column="5" endline="19" endcolumn="6" offset="58" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="0" line="19" column="13" endline="19" endcolumn="14" offset="63" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="0" line="20" column="4" endline="20" endcolumn="5" offset="68" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="1" line="21" column="3" endline="21" endcolumn="4" offset="69" excluded="false" document="altcover/Sample1/Program.cs" />
    </method>
    <altcover.file document="altcover/Sample1/Program.cs" embed="hU9NS8NAED0nkP/wzKnVkgavRT3Uk6gIDRQRD2ucJkuTnTC7SQjS/+5mq+DNwwzDvI9501ttKuwm66jdJHESr9dwZB064UpUGxYHFgh1LA4VGRLlNJuZbFRLtlMloeC+rAsvTOKvJI60cSRGNSgbZS1efs2iGYw60YNyBOu8U4mB9SeelDYL68THeXuHksouZ2rgR4MSGB5xg3uvK3RL2TOPm4DpAxYey17Jk25xned5UP5Ioy0byw1le9GOHrWhRbqvSQjaop0gXB7Jv6vK4x1SXGEXMmQP7POkl+nqnGV5PnYKnRpL/5woRjJOk6ux9UMv0wofPF2kf33m5uv0DQ==" />
  </module>
</coverage>
"@
    $sw.ToString().Replace("`r", "") | Should -Be $expected.Replace("`r", "")
    $sw.ToString().Replace("`r", "") | Should -Be $written.Replace("`r", "").Replace("utf-8", "utf-16")
  }

  It "converts with the pipeline" {
    $xml = [xdoc]::Load("./AltCover.Tests/HandRolledMonoCoverage.xml") | ConvertTo-NCover
    $xml | Should -BeOfType [xdoc]

    $sw = new-object System.IO.StringWriter @()
    $settings = new-object System.Xml.XmlWriterSettings @()
    $settings.Indent = $true
    $settings.IndentChars = "  "
    $xw = [System.Xml.XmlWriter]::Create($sw, $settings)
    $xml.WriteTo($xw)
    $xw.Close()
    $time = $xml.Descendants("coverage") | Select-Object -First 1 
    $time = $time.Attribute("startTime").Value
    $expected = [System.IO.File]::ReadAllText("./AltCover.Tests/HandRolledToNCover.xml")
    $sw.ToString().Replace("`r", "") | Should -Be $expected.Replace("`r", "").Replace("{0}", $time)
  }
}

Describe "ConvertTo-BarChart" {
  It "converts NCover" {
    $xml = ConvertTo-BarChart -InputFile "./AltCover.Tests/GenuineNCover158.Xml" -OutputFile "./_Packaging/GenuineNCover158Chart.html"
    $xml | Should -BeOfType [xdoc]

    $sw = new-object System.IO.StringWriter @()
    $settings = new-object System.Xml.XmlWriterSettings @()
    $settings.Indent = $true
    $settings.IndentChars = "  "
    $xw = [System.Xml.XmlWriter]::Create($sw, $settings)
    $xml.WriteTo($xw)
    $xw.Close()
    $written = [System.IO.File]::ReadAllText("./_Packaging/GenuineNCover158Chart.html")
    $expected = [System.IO.File]::ReadAllText("./AltCover.Tests/GenuineNCover158Chart.html")

    $result = $sw.ToString().Replace("`r", "").Replace("html >", "html>") 
    $result | Should -Be $expected.Replace("`r", "").Replace("`"utf-8`"?>", "`"utf-16`"?>")
    $written.Replace("`r", "").Replace("html >", "html>")  | Should -Be $expected.Replace("`r", "")
  }

  It "converts NCover through the pipeline" {
    $xml = [xdoc]::Load("./AltCover.Tests/HandRolledVisualized.xml") | ConvertTo-BarChart ## -OutputFile "./_Packaging/HandRolledVisualized.html"
    $xml | Should -BeOfType [xdoc]

    $sw = new-object System.IO.StringWriter @()
    $settings = new-object System.Xml.XmlWriterSettings @()
    $settings.Indent = $true
    $settings.IndentChars = "  "
    $xw = [System.Xml.XmlWriter]::Create($sw, $settings)
    $xml.WriteTo($xw)
    $xw.Close()
    $expected = [System.IO.File]::ReadAllText("./AltCover.Tests/HandRolledVisualized.html")

    # swap out unique identifiers
    $result = $sw.ToString().Replace("`r", "").Replace("html >", "html>")
    $result = $result -replace "href\=`"\#[A-Z0-9]{7}`"","href=`"xxx`"" 
    $result = $result -replace "name\=`"\#[A-Z0-9]{7}`"","name=`"xxx`"" 
    $result = $result -replace "id\=`"[A-Z0-9]+class","id=`"xxxclass" 
    $result = $result -replace "id\=`"[A-Z0-9]+`"","id=`"xxx`"" 
    $result = $result -replace "toggle\([A-Z0-9]+class","toggle(xxxclass" 
    $result = $result -replace "toggle\([A-Z0-9]+\)","toggle(xxx)" 

    $expected = $expected -replace "href\=`"\#[A-Z0-9]{7}`"","href=`"xxx`"" 
    $expected = $expected -replace "name\=`"\#[A-Z0-9]{7}`"","name=`"xxx`"" 
    $expected = $expected -replace "id\=`"[A-Z0-9]+class","id=`"xxxclass" 
    $expected = $expected -replace "id\=`"[A-Z0-9]+`"","id=`"xxx`"" 
    $expected = $expected -replace "toggle\([A-Z0-9]+class","toggle(xxxclass" 
    $expected = $expected -replace "toggle\([A-Z0-9]+\)","toggle(xxx)" 

    $result | Should -Be $expected.Replace("`r", "").Replace("`"utf-8`"?>", "`"utf-16`"?>")
  }

  It "converts OpenCover" {
    $xml = ConvertTo-BarChart -InputFile "./AltCover.Tests/HandRolledMonoCoverage.xml" -OutputFile "./_Packaging/HandRolledMonoCoverage.html"
    $xml | Should -BeOfType [xdoc]

    $sw = new-object System.IO.StringWriter @()
    $settings = new-object System.Xml.XmlWriterSettings @()
    $settings.Indent = $true
    $settings.IndentChars = "  "
    $xw = [System.Xml.XmlWriter]::Create($sw, $settings)
    $xml.WriteTo($xw)
    $xw.Close()
    $written = [System.IO.File]::ReadAllText("./_Packaging/HandRolledMonoCoverage.html")
    $expected = [System.IO.File]::ReadAllText("./AltCover.Tests/HandRolledMonoCoverage.html")

    $result = $sw.ToString().Replace("`r", "").Replace("ID0ES", "ID0ET") # flakiness in label autogenerator
    $result | Should -Be $expected.Replace("`r", "").Replace("&#x2442;", ([char]0x2442).ToString()).Replace("`"utf-8`"?>", "`"utf-16`"?>")
    $written.Replace("`r", "").Replace("ID0ES", "ID0ET")  | Should -Be $expected.Replace("`r", "")
  }
}

Describe "ConvertFrom-NCover" {
  It "converts" {
    $assemblies = @()
    $assemblies += "./_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.1/Sample4.dll"
    $xml = ConvertFrom-NCover -InputFile "./_Reports/ReleaseXUnitFSharpTypesDotNetRunner.xml" -Assembly $Assemblies -OutputFile "./_Packaging/AltCoverFSharpTypes.xml"
    $xml | Should -BeOfType [xdoc]

    $sw = new-object System.IO.StringWriter @()
    $settings = new-object System.Xml.XmlWriterSettings @()
    $settings.Indent = $true
    $settings.IndentChars = "  "
    $xw = [System.Xml.XmlWriter]::Create($sw, $settings)
    $xml.WriteTo($xw)
    $xw.Close()
    $written = [System.IO.File]::ReadAllText("./_Packaging/AltCoverFSharpTypes.xml")
    $expected = [System.IO.File]::ReadAllText("./AltCover.Tests/AltCoverFSharpTypes.n3.xml")

    $xml = ConvertTo-XDocument $xml
    $hash = $xml.Descendants("Module").Attribute("hash").Value
    $time = $xml.Descendants("ModuleTime").Value
    $file = $xml.Descendants("File") | Select-Object -First 1
    $fullpath = [System.io.path]::GetDirectoryName($file.Attribute("fullPath").Value)

    $expected = $expected.Replace("09-23-DC-B3-65-CE-96-5D-B4-56-2A-3A-0D-5A-1B-09-3E-38-2B-22", $hash)
    $expected = $expected.Replace("2018-06-13T15:08:24.8840000Z", $time)
    $expected = $expected.Replace("Sample4|Program.fs", (Join-Path $fullpath "Program.fs"))
    $expected = $expected.Replace("Sample4|Tests.fs", (Join-Path $fullpath "Tests.fs"))
    $expected = $expected.Replace("<ModulePath>./_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.1/Sample4.dll",
                                  "<ModulePath>" + [System.IO.Path]::GetFullPath("./_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.1/Sample4.dll"))

    $result = $sw.ToString().Replace("`r", "").Replace("utf-16", "utf-8")
    $result = $result.Replace("rapScore=`"13.12", "rapScore=`"13.13").Replace("rapScore=`"8.12", "rapScore=`"8.13")
    $result | Should -Be $expected.Replace("`r", "")

    $written = $written.Replace("rapScore=`"13.12", "rapScore=`"13.13").Replace("rapScore=`"8.12", "rapScore=`"8.13").Replace("`r", "")
    $result | Should -Be $expected.Replace("`r", "")
  }

  It "converts from the pipeline" {
    $assemblies = @()
    $assemblies += "./_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.1/Sample4.dll"
    $xml = [xdoc]::Load("./_Reports/ReleaseXUnitFSharpTypesDotNetRunner.xml") | ConvertFrom-NCover -Assembly $Assemblies
    $xml | Should -BeOfType [xdoc]

    $sw = new-object System.IO.StringWriter @()
    $settings = new-object System.Xml.XmlWriterSettings @()
    $settings.Indent = $true
    $settings.IndentChars = "  "
    $xw = [System.Xml.XmlWriter]::Create($sw, $settings)
    $xml.WriteTo($xw)
    $xw.Close()

    $expected = [System.IO.File]::ReadAllText("./AltCover.Tests/AltCoverFSharpTypes.n3.xml")

    $xml = ConvertTo-XDocument $xml
    $hash = $xml.Descendants("Module").Attribute("hash").Value
    $time = $xml.Descendants("ModuleTime").Value
    $file = $xml.Descendants("File") | Select-Object -First 1
    $fullpath = [System.io.path]::GetDirectoryName($file.Attribute("fullPath").Value)

    $expected = $expected.Replace("09-23-DC-B3-65-CE-96-5D-B4-56-2A-3A-0D-5A-1B-09-3E-38-2B-22", $hash)
    $expected = $expected.Replace("2018-06-13T15:08:24.8840000Z", $time)
    $expected = $expected.Replace("Sample4|Program.fs", (Join-Path $fullpath "Program.fs"))
    $expected = $expected.Replace("Sample4|Tests.fs", (Join-Path $fullpath "Tests.fs"))
    $expected = $expected.Replace("<ModulePath>./_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.1/Sample4.dll",
                                  "<ModulePath>" + [System.IO.Path]::GetFullPath("./_Binaries/Sample4/Debug+AnyCPU/netcoreapp2.1/Sample4.dll"))

    $result = $sw.ToString().Replace("`r", "").Replace("utf-16", "utf-8")
    $result = $result.Replace("rapScore=`"13.12", "rapScore=`"13.13").Replace("rapScore=`"8.12", "rapScore=`"8.13")

    $result | Should -Be $expected.Replace("`r", "")
  }
}

Describe "Compress-Branching" {
  It "Removes interior branches" {
    $xml = Compress-Branching -WithinSequencePoint -InputFile "./AltCover.Tests/Compressible.xml" -OutputFile "./_Packaging/CompressInterior.xml"
    $xml | Should -BeOfType [xdoc]

    $sw = new-object System.IO.StringWriter @()
    $settings = new-object System.Xml.XmlWriterSettings @()
    $settings.Indent = $true
    $settings.IndentChars = "  "
    $xw = [System.Xml.XmlWriter]::Create($sw, $settings)
    $xml.WriteTo($xw)
    $xw.Close()
    $written = [System.IO.File]::ReadAllText("./_Packaging/CompressInterior.xml")
    $expected = [System.IO.File]::ReadAllText("./AltCover.Tests/CompressInterior.xml").Replace("`r", "")

    $result = $sw.ToString().Replace("`r", "").Replace("utf-16", "utf-8")  
    $result | Should -Be $expected
    $written.Replace("`r", "") | Should -Be  $expected
  }
  It "Unifies equivalent branches" {
    $xml = Compress-Branching -SameSpan -InputFile "./AltCover.Tests/Compressible.xml" -OutputFile "./_Packaging/SameSpan.xml"
    $xml | Should -BeOfType [xdoc]

    $sw = new-object System.IO.StringWriter @()
    $settings = new-object System.Xml.XmlWriterSettings @()
    $settings.Indent = $true
    $settings.IndentChars = "  "
    $xw = [System.Xml.XmlWriter]::Create($sw, $settings)
    $xml.WriteTo($xw)
    $xw.Close()
    $written = [System.IO.File]::ReadAllText("./_Packaging/SameSpan.xml")
    $expected = [System.IO.File]::ReadAllText("./AltCover.Tests/SameSpan.xml").Replace("`r", "")

    $result = $sw.ToString().Replace("`r", "").Replace("utf-16", "utf-8")  
    $result | Should -Be $expected
    $written.Replace("`r", "") | Should -Be  $expected
  }
  It "DoesBoth" {
    $xml = [xdoc]::Load("./AltCover.Tests/Compressible.xml") | Compress-Branching -SameSpan -WithinSequencePoint -OutputFile "./_Packaging/CompressBoth.xml"
    $xml | Should -BeOfType [xdoc]

    $sw = new-object System.IO.StringWriter @()
    $settings = new-object System.Xml.XmlWriterSettings @()
    $settings.Indent = $true
    $settings.IndentChars = "  "
    $xw = [System.Xml.XmlWriter]::Create($sw, $settings)
    $xml.WriteTo($xw)
    $xw.Close()
    $written = [System.IO.File]::ReadAllText("./_Packaging/CompressBoth.xml")
    $expected = ([System.IO.File]::ReadAllText("./AltCover.Tests/CompressBoth.xml")).Replace("`r", "")

    $result = $sw.ToString().Replace("`r", "").Replace("utf-16", "utf-8") 
    $result | Should -Be $expected
    $written.Replace("`r", "") | Should -Be  $expected
  }
  It "DoesAtLeastOne" {
    $fail = $false
    try {
      Compress-Branching -InputFile "./AltCover.Tests/HandRolledMonoCoverage.xml" -OutputFile "./_Packaging/CompressBoth.xml"
      $fail = $true
    }
    catch {
      $_.Exception.Message | Should -BeLikeExactly 'Parameter set cannot be resolved using the specified named parameters.*'
    }

    $fail | Should -BeFalse
  }
}

#Describe "ConvertTo-SourceMap" {
#  It "finds files from NCover" {
#    $result = "./_Reports/ReleaseXUnitFSharpTypesDotNetRunner.xml" | ConvertTo-SourceMap -OutputFolder "./_Packaging/NCoverSourceMap"
#
#    $result.Count | Should -Be 1
#    $result.Keys | Should -Be @('Tests.fs')
#
#    dir "./_Packaging/NCoverSourceMap" | % { $_.Name }  | Should -Be @('Tests.fs.html')
#  }
#
#  It "finds files from OpenCover" {
#    $result = [xdoc]::Load("./_Reports/AltCoverFSharpTests.xml") | ConvertTo-SourceMap
#
#    $result.Count | Should -Be 1
#    $result.Keys | Should -Be 'Library.fs'
#  }
#}

Describe "Write-OpenCoverDerivedState" {
  It "Outputs a document from a file" {
    $assembly = (Resolve-Path "./_Reports/OpenCoverForPester/Sample18.dll").Path
    $assemblies = @()
    $assemblies += $assembly
    # $hash = Get-FileHash -Algorithm SHA1 $assembly
    # $hexpected= (0..19 | % { $hash.hash.Substring( 2 * $_, 2) }) -join "-"
    $sha1 = new-object "System.Security.Cryptography.SHA1Managed"
    $fs = [System.IO.File]::OpenRead($assembly)
    $bytes = $sha1.ComputeHash($fs)
    $fs.Close()
    $sha1.Dispose()
    $hexpected = [System.BitConverter]::ToString($bytes)

    $inputFile = "./_Reports/OpenCoverForPester/OpenCoverForPester.coverlet.xml"

    $check = [xml](Get-Content "./_Reports/OpenCoverForPester/OpenCoverForPester.coverlet.xml")
    if ($check.CoverageSession.Summary.visitedSequencePoints -ne "11")
    {
      $inputFile = "./AltCover.Tests/OpenCoverForPester.coverlet.expected.xml"
      [System.Console]::WriteLine("Substituting coverlet file")
    }
    $xml = Write-OpenCoverDerivedState -InputFile "./_Reports/OpenCoverForPester/OpenCoverForPester.coverlet.xml" -Coverlet -Assembly $Assemblies -OutputFile "./_Packaging/OpenCoverForPester.coverlet.xml"
    $xml | Should -BeOfType [xdoc]

    $doc = [xml](Get-Content "./_Packaging/OpenCoverForPester.coverlet.xml")
    $hactual = $doc.CoverageSession.Modules.Module.hash
    $hactual | Should -BeExactly $hexpected

    $expected = [xml](Get-Content "./AltCover.Tests/OpenCoverForPester.coverlet.xml")
    $expected.CoverageSession.Modules.Module.hash = $hactual
    $expected.CoverageSession.Modules.Module.ModulePath = $assembly
    $expected.CoverageSession.Modules.Module.ModuleTime = $doc.CoverageSession.Modules.Module.ModuleTime
    $expected.CoverageSession.Modules.Module.Files.File.fullPath = (Resolve-Path "./Samples/Sample18/Tests.fs").Path

    $sw = new-object System.IO.StringWriter @()
    $settings = new-object System.Xml.XmlWriterSettings @()
    $settings.Indent = $true
    $settings.IndentChars = "  "
    $xw = [System.Xml.XmlWriter]::Create($sw, $settings)
    $expected.WriteTo($xw)
    $xw.Close()
    $expect = $sw.ToString().Replace("`r", "").Replace("utf-16", "utf-8") 

    $written = [System.IO.File]::ReadAllText("./_Packaging/OpenCoverForPester.coverlet.xml").Replace("`r", "").Replace("utf-16", "utf-8") 

    $written | Should -BeExactly $expect
  }

  It "Works the pipeline" {
    
    $xmlIn = [xdoc]::Load("./AltCover.Tests/BasicCSharp.xml")

    $xml = $xmlIn | Write-OpenCoverDerivedState -BranchOrdinal SL
    $xml | Should -BeOfType [xdoc]

    $xml.Save("./_Packaging/BasicCSharp.xml")

    $sw = new-object System.IO.StringWriter @()
    $settings = new-object System.Xml.XmlWriterSettings @()
    $settings.Indent = $true
    $settings.IndentChars = "  "
    $xw = [System.Xml.XmlWriter]::Create($sw, $settings)
    $xml.WriteTo($xw)
    $xw.Close()
    $result = $sw.ToString().Replace("`r", "").Replace("utf-16", "utf-8") 

    $expected = [System.IO.File]::ReadAllText("./AltCover.Tests/BasicCSharp.postprocessed.xml").Replace("`r", "")
    $result | Should -BeExactly $expected
  }
}

Describe "ConvertTo-CoverageJson" {
    It "converts from a document" {
        $xd = [xdoc]::Load("./AltCover.Tests/GenuineNCover158.Xml")

        ## fix up file path
        $exe = [System.IO.Path]::Combine("./Samples/Sample19", "ConsoleApplication1.exe")
        $xd.Root.Descendants("module") | % {
          $_.Attribute("name").Value = [System.IO.Path]::GetFullPath($exe)
        }

        $json = ConvertTo-CoverageJson $xd
        $json.Trim() | Set-Content "./_Packaging/GenuineNCover158.json"
        $expect = Get-Content "./AltCover.Tests/GenuineNCover158.json" 
        $result = Get-Content "./_Packaging/GenuineNCover158.json"
        $result | Should -BeExactly $expect
    }

    It "Converts from a file" {
        $path = "./AltCover.Tests/Sample4FullTracking.xml"
        $json = ConvertTo-CoverageJson $path
        $json.Trim() | Set-Content("./_Packaging/OpenCover.json")
        $expect = Get-Content "./AltCover.Tests/OpenCover.json" 
        $result = Get-Content "./_Packaging/OpenCover.json"
        $result | Should -BeExactly $expect
    }

    It "Converts Coverlet near-OpenCover format" {
        $path = "./AltCover.Tests/Sample4.coverlet.xml"
        $json = ConvertTo-CoverageJson $path
        $json.Trim() | Set-Content("./_Packaging/Coverlet.FromXml.json")
        $expect = Get-Content "./AltCover.Tests/Coverlet.FromXml.json" 
        $result = Get-Content "./_Packaging/Coverlet.FromXml.json"
        $result | Should -BeExactly $expect
    }
}

Describe "ConvertFrom-CoverageJson" {
  It "Converts coverlet Data" {
    $x = ConvertFrom-CoverageJson -InputFile "./AltCover.Tests/Sample4.coverlet.json" -OutputFile "./_Packaging/Sample4.fromcoverletjson.xml"

    $expected = ([String]::Join("`n", (Get-Content "./AltCover.Tests/Sample4.fromcoverletjson.xml")))

    $got = [String]::Join("`n", (Get-Content "./_Packaging/Sample4.fromcoverletjson.xml"))
    $got | Should -Be $expected
  }

  It "Converts With the pipeline" {
    $raw = Get-Content "./AltCover.Tests/Sample5.native.json"
    $raw = [String]::Join("`n", $raw)
    $x = $raw | ConvertFrom-CoverageJson

    $x.Save("./_Packaging/Sample5.native.xml")

    $expected = ([String]::Join("`n", (Get-Content "./AltCover.Tests/Sample5.native.xml")))
    $actual = ([String]::Join("`n", (Get-Content "./_Packaging/Sample5.native.xml")))
    $actual | Should -Be $expected
  }
}

Describe "MergeOpenCover" {
  It "Absorbs Unsupported Xml Files" {
    $xml = dir -recurse "./AltCover.Tests/*.cobertura" | Merge-OpenCover
    $xml | Should -BeOfType [xdoc]

    $expected = @"
<CoverageSession xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
  <Summary numSequencePoints="?" visitedSequencePoints="0" numBranchPoints="?" visitedBranchPoints="0" sequenceCoverage="0" branchCoverage="0" maxCyclomaticComplexity="0" minCyclomaticComplexity="0" visitedClasses="0" numClasses="?" visitedMethods="0" numMethods="?" minCrapScore="0" maxCrapScore="0" />
  <Modules />
</CoverageSession>
"@

    $xml.ToString().Replace("`r", "") | Should -BeExactly $expected.Trim().Replace("`r", "")
  }

  It "Selects Single OpenCover Files" {
    $files = ("./AltCover.Tests/HandRolledMonoCoverage.xml", "./AltCover.Tests/Sample1WithNCover.xml","./AltCover.Tests/OpenCover.cobertura")
    $xml = $files | Merge-OpenCover
       $xml | Should -BeOfType [xdoc]
       $expected = [xdoc]::Load("./AltCover.Tests/HandRolledMonoCoverage.xml")
       $xml.ToString() | Should -BeExactly $expected.ToString().Replace(' standalone="yes"', '')
  }

  It "Combines Top Level Summaries" {
    $files = ("./AltCover.Tests/HandRolledMonoCoverage.xml", "./AltCover.Tests/Sample4FullTracking.xml")
    $xml = $files | Merge-OpenCover -OutputFile "./_Packaging/OpenCoverCombination-1.xml"
    $xml | Should -BeOfType [xdoc]
    $summary = ($xml.Descendants("Summary") | Select-Object -First 1).ToString()
    $summary | Should -BeExactly '<Summary numSequencePoints="35" visitedSequencePoints="21" numBranchPoints="5" visitedBranchPoints="5" sequenceCoverage="60.00" branchCoverage="100.00" maxCyclomaticComplexity="7" minCyclomaticComplexity="1" visitedClasses="5" numClasses="8" visitedMethods="9" numMethods="13" minCrapScore="1.00" maxCrapScore="14.11" />'
  }
}
