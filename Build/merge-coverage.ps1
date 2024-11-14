Import-Module "./_Packaging/Unpack/tools/net8.0/AltCover.PowerShell.dll"


$files = ("_Reports/UnitTestWithAltCoverCoreRunner.xml",
          "_Reports/Recorder2TestWithAltCoverCoreRunner.xml",
          "_Reports/ApiTestWithAltCoverCoreRunner.net9.0.xml",
          "_Reports/MonitorTestWithAltCoverCoreRunner.net9.0.xml",
          "_Reports/RecorderTestWithAltCoverCoreRunner.net9.0.xml",
          "_Reports/ValidateGendarmeEmulationUnitTestWithAltCoverCoreRunner.net9.0.xml",
          "_Reports/Pester.xml" )

$xml = $files | Merge-OpenCover -OutputFile "_Reports/CombinedTestWithAltCoverRunner.coveralls"