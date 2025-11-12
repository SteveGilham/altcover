Import-Module "./_Packaging/Unpack/tools/net8.0/AltCover.PowerShell.dll"


$files = ("_Reports/EngineTestWithAltCoverCoreRunner.net10.0.xml",
          "_Reports/Recorder2TestWithAltCoverCoreRunner.xml",
          "_Reports/ApiTestWithAltCoverCoreRunner.net10.0.xml",
          "_Reports/MonitorTestWithAltCoverCoreRunner.net10.0.xml",
          "_Reports/RecorderTestWithAltCoverCoreRunner.net10.0.xml",
          "_Reports/ValidateGendarmeEmulationUnitTestWithAltCoverCoreRunner.net10.0.xml",
          "_Reports/Pester.xml" )

$xml = $files | Merge-OpenCover -OutputFile "_Reports/CombinedTestWithAltCoverRunner.coveralls"