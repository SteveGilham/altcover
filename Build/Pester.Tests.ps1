$x = "./_Reports/PesterFSharpTypesDotNetRunner.xml"
$o = "./Sample2/_Binaries/Sample2/Debug+AnyCPU/netcoreapp2.0"
$i = "./_Binaries/Sample2/Debug+AnyCPU/netcoreapp2.0"

Describe "Invoke-Altcover" {
    It "instruments and collects" {
        Invoke-AltCover -XmlReport $x -OutputDirectory  $o -InputDirectory $i -InPlace
        $o | Should -Exist
        $x | Should -Exist
        Invoke-AltCover -Runner -RecorderDirectory $i -WorkingDirectory "./Sample2" -Executable "dotnet" -CommandLine @("test", "--no-build", "--configuration", "Debug",  "sample2.core.fsproj")
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