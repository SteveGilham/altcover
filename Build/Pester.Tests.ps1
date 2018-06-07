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
        $xd.GetType().FullName | Should -Be "System.Xml.Linq.XDocument"
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
        $xml.GetType().FullName | Should -Be "System.Xml.XmlDocument"
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
SF:altcover\Sample1\Program.cs
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
}

Describe "ConvertTo-Cobertura" {
    It "Converts OpenCover Data" {
        $x = ConvertTo-Cobertura -InputFile "./Tests/HandRolledMonoCoverage.xml" -OutputFile "./_Packaging/OpenCover.cobertura"
        $expected = @"
        <?xml version="1.0" encoding="utf-8" standalone="yes"?>
        <coverage line-rate="0.7142857142857143" branch-rate="0.66666666666666663" version="3.5.0.0" timestamp="1528396285">
          <sources>
            <source>altcover\Sample1</source>
          </sources>
          <packages>
            <package name="Sample1" line-rate="0.7142857142857143" branch-rate="0.66666666666666663" complexity="2">
              <classes>
                <class name="TouchTest.Program" filename="altcover\Sample1\Program.cs" line-rate="0.7142857142857143" branch-rate="0.66666666666666663" complexity="2">
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
        $got | Should -Be $expected.Replace("`r", "")

        $header = $x.Declaration.ToString() + "`n"
        ($header + $x.ToString()).Replace("`r", "") | Should -Be $sexpected.Replace("`r", "")
    }

    It "Converts NCover Data" {
        $x = ConvertTo-Cobertura -InputFile "./Tests/Sample1WithNCover.xml" -OutputFile "./_Packaging/NCover.cobertura"
        $expected = @"
        <?xml version="1.0" encoding="utf-8" standalone="yes"?>
        <coverage line-rate="0.7142857142857143" branch-rate="0.66666666666666663" version="3.5.0.0" timestamp="1528396285">
          <sources>
            <source>altcover\Sample1</source>
          </sources>
          <packages>
            <package name="Sample1" line-rate="0.7142857142857143" branch-rate="0.66666666666666663" complexity="2">
              <classes>
                <class name="TouchTest.Program" filename="altcover\Sample1\Program.cs" line-rate="0.7142857142857143" branch-rate="0.66666666666666663" complexity="2">
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
        $got = [String]::Join("`n", (Get-Content "./_Packaging/NCover.cobertura"))
        $got | Should -Be $expected.Replace("`r", "")

        $header = $x.Declaration.ToString() + "`n"
        ($header + $x.ToString()).Replace("`r", "") | Should -Be $sexpected.Replace("`r", "")
    }
}