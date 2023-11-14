Import-Module "./_Packaging/Unpack/tools/netcoreapp2.0/AltCover.PowerShell.dll"


$files = ("_Reports/UnitTestWithAltCoverCoreRunner.xml",
          "_Reports/ApiTestWithAltCoverCoreRunner.net8.0.xml",
          "_Reports/MonitorTestWithAltCoverCoreRunner.net8.0.xml",
          "_Reports/RecorderTestWithAltCoverCoreRunner.net8.0.xml",
          "_Reports/Recorder2TestWithAltCoverCoreRunner.xml",
          "_Reports/ValidateGendarmeEmulationUnitTestWithAltCoverCoreRunner.net8.0.xml",
          "_Reports/Pester.xml" )

$xml = $files | Merge-OpenCover -OutputFile "_Reports/CombinedTestWithAltCoverRunner.coveralls"