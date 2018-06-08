$x = "./_Reports/PesterFSharpTypesDotNetRunner.xml"
$o = "./Sample2/_Binaries/Sample2/Debug+AnyCPU/netcoreapp2.0"
$i = "./_Binaries/Sample2/Debug+AnyCPU/netcoreapp2.0"

Describe "Invoke-Altcover" {
    It "instruments and collects" {
        if (Test-Path $o) {
            Remove-Item -Force -Recurse $o
        }
        Invoke-AltCover -XmlReport $x -OutputDirectory  $o -InputDirectory $i -AssemblyFilter "Adapter" -InformationAction Continue
        $o | Should -Exist
        $x | Should -Exist
        $xm = [xml](Get-Content $x)
        [string]::Join(" ", $xm.coverage.module.method.name) | Should -Be "main returnFoo returnBar testMakeUnion as_bar get_MyBar Invoke .ctor makeThing testMakeThing bytes"
        [string]::Join(" ", $xm.coverage.module.method.seqpnt.visitcount) | Should -Be "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0"

        $w = ""
        Invoke-AltCover -Runner -RecorderDirectory $o -WarningVariable w
        $xm = [xml](Get-Content $x)
        [string]::Join(" ", $xm.coverage.module.method.name) | Should -Be "main returnFoo returnBar testMakeUnion as_bar get_MyBar Invoke .ctor makeThing testMakeThing bytes"
        [string]::Join(" ", $xm.coverage.module.method.seqpnt.visitcount) | Should -Be "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
        $w | Should -Be "A total of 0 visits recorded"

        Invoke-AltCover  -InformationAction Continue -Runner -RecorderDirectory $o -WorkingDirectory "./Sample2" -Executable "dotnet" -CommandLine @("test", "--no-build", "--configuration", "Debug",  "sample2.core.fsproj")
        $xm2 = [xml](Get-Content $x)
        [string]::Join(" ", $xm2.coverage.module.method.seqpnt.visitcount) | Should -Be "0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 2 1 1 1"
        Remove-Item -Force -Recurse $o
    }

    It "Fails on garbage" {
        $saved = [System.Console]::Error
        $stderr = new-object System.IO.StringWriter @()
        [System.Console]::SetError($stderr)
        try 
        {
          $ev = ""
          Invoke-AltCover -XmlReport $x -OutputDirectory  $o -InputDirectory "./NoneSuch/xunit-dotnet/bin/Debug/netcoreapp2.0" -InPlace -ErrorVariable ev
          $ev | Should -BeTrue
          $stderr.ToString()  | Should -BeTrue
        }
        finally
        {
            [System.Console]::SetError($saved)     
        }
    }
}

Describe "ConvertTo-XDocument" {
    It "converts" {
        $xml = [xml](Get-Content "./Tests/Sample1WithNCover.xml")
        $xd = $xml | ConvertTo-XDocument
        $xd | Should -BeOfType "System.Xml.Linq.XDocument"
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
        $xd = [System.Xml.Linq.XDocument]::Load("./Tests/Sample1WithNCover.xml")
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
        ConvertTo-LCov -InputFile "./Tests/HandRolledMonoCoverage.xml" -OutputFile "./_Packaging/OpenCover.lcov"
        $expected = @"
TN:
SF:altcover/Sample1/Program.cs
FN:11,System.Void TouchTest.Program::Main(System.String[])
FNDA:1,System.Void TouchTest.Program::Main(System.String[])
FNF:1
FNH:1
BRDA:13,0,0,1
BRDA:13,0,1,-
BRF:2
BRH:1
DA:11,1
DA:12,1
DA:13,1
DA:13,1
DA:14,1
DA:15,1
DA:15,1
DA:15,1
DA:16,1
DA:18,0
DA:19,0
DA:19,0
DA:20,0
DA:21,1
LH:10
LF:14
end_of_record
"@
        $got = [String]::Join("`n", (Get-Content "./_Packaging/OpenCover.lcov"))
        $got | Should -Be $expected.Replace("`r", "")
    }

    It "Converts NCover Data" {
        ConvertTo-LCov -InputFile "./Tests/Sample1WithNCover.xml" -OutputFile "./_Packaging/NCover.lcov"
        $expected = [String]::Join("`n", (Get-Content "./Tests/NCoverBugFix.lcov"))
        $got = [String]::Join("`n", (Get-Content "./_Packaging/NCover.lcov"))
        $got | Should -Be $expected.Replace("`r", "")
    }

  It "Converts Real NCover Data" {
    $ev = ""
    ConvertTo-LCov -InputFile "./Tests/GenuineNCover158.Xml" -OutputFile "./_Packaging/NCover158.lcov" -ErrorVariable ev
    $ev | Should -BeFalse
  }
}

Describe "ConvertTo-Cobertura" {
    It "Converts OpenCover Data" {
        $x = ConvertTo-Cobertura -InputFile "./Tests/HandRolledMonoCoverage.xml" -OutputFile "./_Packaging/OpenCover.cobertura"
        $coverage = $x.Descendants("coverage")
        $v = $coverage.Attribute("version").Value
        $t = $coverage.Attribute("timestamp").Value

        $expected = @"
<?xml version="1.0" encoding="utf-8" standalone="yes"?>
<coverage line-rate="0.7142857142857143" branch-rate="0.66666666666666663" version="$v" timestamp="$t">
  <sources>
    <source>altcover\Sample1</source>
  </sources>
  <packages>
    <package name="Sample1" line-rate="0.7142857142857143" branch-rate="0.66666666666666663" complexity="2">
      <classes>
        <class name="TouchTest.Program" filename="altcover/Sample1/Program.cs" line-rate="0.7142857142857143" branch-rate="0.66666666666666663" complexity="2">
          <methods>
            <method name="Main" signature="System.Void System.String[])" line-rate="0.7142857142857143" branch-rate="0.66666666666666663">
              <lines>
                <line number="11" hits="1" branch="false" />
                <line number="12" hits="1" branch="false" />
                <line number="13" hits="1" branch="false" />
                <line number="13" hits="1" branch="true" condition-coverage="50% (1/2)">
                  <conditions>
                    <condition number="0" type="jump" coverage="50%" />
                  </conditions>
                </line>
                <line number="14" hits="1" branch="false" />
                <line number="15" hits="1" branch="false" />
                <line number="15" hits="1" branch="false" />
                <line number="15" hits="1" branch="false" />
                <line number="16" hits="1" branch="false" />
                <line number="18" hits="0" branch="false" />
                <line number="19" hits="0" branch="false" />
                <line number="19" hits="0" branch="false" />
                <line number="20" hits="0" branch="false" />
                <line number="21" hits="1" branch="false" />
              </lines>
            </method>
          </methods>
          <lines>
            <line number="11" hits="1" branch="false" />
            <line number="12" hits="1" branch="false" />
            <line number="13" hits="1" branch="false" />
            <line number="13" hits="1" branch="true" condition-coverage="50% (1/2)">
              <conditions>
                <condition number="0" type="jump" coverage="50%" />
              </conditions>
            </line>
            <line number="14" hits="1" branch="false" />
            <line number="15" hits="1" branch="false" />
            <line number="15" hits="1" branch="false" />
            <line number="15" hits="1" branch="false" />
            <line number="16" hits="1" branch="false" />
            <line number="18" hits="0" branch="false" />
            <line number="19" hits="0" branch="false" />
            <line number="19" hits="0" branch="false" />
            <line number="20" hits="0" branch="false" />
            <line number="21" hits="1" branch="false" />
          </lines>
        </class>
      </classes>
    </package>
  </packages>
</coverage>
"@
        $got = [String]::Join("`n", (Get-Content "./_Packaging/OpenCover.cobertura"))
        $got | Should -Be $expected.Replace("`r", "").Replace("\", [System.IO.Path]::DirectorySeparatorChar)

        $header = $x.Declaration.ToString() + "`n"
        ($header + $x.ToString()).Replace("`r", "") | Should -Be $expected.Replace("`r", "").Replace("\", [System.IO.Path]::DirectorySeparatorChar)
    }

    It "Converts NCover Data" {
        $x = ConvertTo-Cobertura -InputFile "./Tests/Sample1WithNCover.xml" -OutputFile "./_Packaging/NCover.cobertura"
        $coverage = $x.Descendants("coverage")
        $v = $coverage.Attribute("version").Value
        $t = $coverage.Attribute("timestamp").Value

        $expected = @"
<?xml version="1.0" encoding="utf-8" standalone="yes"?>
<coverage line-rate="0.7" branch-rate="1" version="$v" timestamp="$t">
  <sources>
    <source>Sample1</source>
  </sources>
  <packages>
    <package name="Sample1.exe" line-rate="0.7" branch-rate="1" complexity="1">
      <classes>
        <class name="TouchTest.Program" filename="Sample1/Program.cs" line-rate="0.7" branch-rate="1" complexity="1">
          <methods>
            <method name="TouchTest.Program.Main" signature="" line-rate="0.7" branch-rate="1">
              <lines>
                <line number="11" hits="1" branch="false" />
                <line number="12" hits="1" branch="false" />
                <line number="13" hits="1" branch="false" />
                <line number="14" hits="1" branch="false" />
                <line number="15" hits="1" branch="false" />
                <line number="16" hits="1" branch="false" />
                <line number="18" hits="0" branch="false" />
                <line number="19" hits="0" branch="false" />
                <line number="20" hits="0" branch="false" />
                <line number="21" hits="1" branch="false" />
              </lines>
            </method>
          </methods>
          <lines>
            <line number="11" hits="1" branch="false" />
            <line number="12" hits="1" branch="false" />
            <line number="13" hits="1" branch="false" />
            <line number="14" hits="1" branch="false" />
            <line number="15" hits="1" branch="false" />
            <line number="16" hits="1" branch="false" />
            <line number="18" hits="0" branch="false" />
            <line number="19" hits="0" branch="false" />
            <line number="20" hits="0" branch="false" />
            <line number="21" hits="1" branch="false" />
          </lines>
        </class>
      </classes>
    </package>
  </packages>
</coverage>
"@
        $got = [String]::Join("`n", (Get-Content "./_Packaging/NCover.cobertura"))
        $got | Should -Be $expected.Replace("`r", "")

        $header = $x.Declaration.ToString() + "`n"
        ($header + $x.ToString()).Replace("`r", "") | Should -Be $expected.Replace("`r", "")
    }

    It "Converts Real NCover Data" {
      $ev = ""
      ConvertTo-Cobertura -InputFile "./Tests/GenuineNCover158.Xml" -OutputFile "./_Packaging/NCover158.cobertura" -ErrorVariable ev
      $ev | Should -BeFalse
    }
  }
}

Describe "ConvertTo-NCover" {
  It "converts" {
      $xml = ConvertTo-NCover -InputFile "./Tests/HandRolledMonoCoverage.xml" -OutputFile "./_Packaging/HandRolledMonoNCover.xml"
      $xml | Should -BeOfType "System.Xml.XmlDocument"

      $sw = new-object System.IO.StringWriter @()
      $settings = new-object System.Xml.XmlWriterSettings @()
      $settings.Indent = $true
      $settings.IndentChars = "  "
      $xw = [System.Xml.XmlWriter]::Create($sw, $settings)
      $xml.WriteTo($xw)
      $xw.Close()
      $header = $xd.Declaration.ToString() + "`n"
      $written = [System.IO.File]::ReadAllText("./_Packaging/HandRolledMonoNCover.xml")
      $result = [xml](Get-Content "./_Packaging/HandRolledMonoNCover.xml")
      $time = $result.coverage.startTime

      $expected = @"
<?xml version="1.0" encoding="utf-8"?>
<coverage profilerVersion="OpenCover" driverVersion="OpenCover" startTime="$time" measureTime="$time" xmlns:msxsl="urn:schemas-microsoft-com:xslt" xmlns:user="urn:my-scripts">
  <module moduleId="6A-33-AA-93-82-ED-22-9D-F8-68-2C-39-5B-93-9F-74-01-76-00-9F" name="Sample1.exe" assembly="Sample1" assemblyIdentity="Sample1">
    <method excluded="false" instrumented="true" name=".ctor" class="TouchTest.Program" fullname="System.Void TouchTest.Program::.ctor()" />
    <method excluded="false" instrumented="true" name="Main" class="TouchTest.Program" fullname="System.Void TouchTest.Program::Main(System.String[])">
      <seqpnt visitcount="1" line="11" column="9" endline="11" endcolumn="10" offset="0" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="1" line="12" column="32" endline="12" endcolumn="33" offset="1" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="1" line="13" column="13" endline="13" endcolumn="14" offset="7" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="1" line="13" column="21" endline="13" endcolumn="22" offset="9" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="1" line="14" column="13" endline="14" endcolumn="14" offset="24" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="1" line="15" column="17" endline="15" endcolumn="18" offset="25" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="1" line="15" column="72" endline="15" endcolumn="73" offset="36" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="1" line="15" column="25" endline="15" endcolumn="26" offset="46" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="1" line="16" column="13" endline="16" endcolumn="14" offset="51" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="0" line="18" column="13" endline="18" endcolumn="14" offset="57" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="0" line="19" column="17" endline="19" endcolumn="18" offset="58" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="0" line="19" column="25" endline="19" endcolumn="26" offset="63" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="0" line="20" column="13" endline="20" endcolumn="14" offset="68" excluded="false" document="altcover/Sample1/Program.cs" />
      <seqpnt visitcount="1" line="21" column="9" endline="21" endcolumn="10" offset="69" excluded="false" document="altcover/Sample1/Program.cs" />
    </method>
  </module>
</coverage>
"@
    $sw.ToString().Replace("`r", "") | Should -Be $expected.Replace("`r", "")
    $sw.ToString().Replace("`r", "") | Should -Be $written.Replace("`r", "")
  }
}