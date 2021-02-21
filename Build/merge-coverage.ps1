Import-Module "./_Packaging/Unpack/tools/netcoreapp2.0/AltCover.PowerShell.dll"


$files = ("_Reports/UnitTestWithAltCoverRunner.xml",
          "_Reports/ApiTestWithAltCoverRunner.xml",
          "_Reports/ValidateGendarmeEmulationWithAltCoverRunner.xml",
          "_Reports/RecorderTestWithAltCoverRunner.xml",
          "_Reports/RecorderTest2WithAltCoverRunner.xml",
          "_Reports/Pester.xml",
          "_Reports/MonitorTestWithAltCoverCoreRunner.net5.0.xml")

$xml = $files | Merge-OpenCover -OutputFile "_Reports/CombinedTestWithAltCoverRunner.coveralls"