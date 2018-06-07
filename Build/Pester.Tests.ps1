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