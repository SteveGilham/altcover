namespace Tests

#if !NET472

open Expecto

module ExpectoMain =
  let regular = [
          Tests.TestCommonTests.ExerciseItAll, "TestCommonTests.ExerciseItAll"
          Tests.TestCommonTests.SelfTest, "TestCommonTests.SelfTest"
          Tests.MonitorTests.ShouldRecordPointTotals, "MonitorTests.ShouldRecordPointTotals"
          Tests.MonitorTests.ShouldRecordVisitTotals, "MonitorTests.ShouldRecordVisitTotals"
        ]

  let specials =
   []

  let consistencyCheck() =
    ExpectoTestCommon.consistencyCheck regular []

  [<Tests>]
  let tests =
    ExpectoTestCommon.makeTests "AltCoverMonitorTests" consistencyCheck regular specials ignore

module UnitTestStub =
  [<EntryPoint; System.Runtime.CompilerServices.CompilerGenerated>]
  let unitTestStub argv =
    let writeResults = TestResults.writeNUnitSummary ("AltCover.Monitor.TestResults.xml", "AltCover.Monitor.Tests")
    let config = defaultConfig.appendSummaryHandler writeResults
    runTestsWithArgs config argv ExpectoMain.tests
#endif