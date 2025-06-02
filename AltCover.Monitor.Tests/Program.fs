namespace Tests

#if !NET472

open Expecto

module ExpectoMain =
  let regular =
    [ Tests.TestCommonTests.TestMultiple, "TestCommon.TestMultiple"
      Tests.TestCommonTests.TestIgnoredTests, "TestCommon.TestIgnoredTests"
      Tests.TestCommonTests.ExerciseItAll, "TestCommon.ExerciseItAll"
      Tests.TestCommonTests.SelfTest, "TestCommon.SelfTest"
      Tests.MonitorTests.ShouldCountOpenCoverTotals,
      "MonitorTests.ShouldCountOpenCoverTotals"
      Tests.MonitorTests.ShouldRecordPointTotals, "MonitorTests.ShouldRecordPointTotals"
      Tests.MonitorTests.ShouldRecordVisitTotals, "MonitorTests.ShouldRecordVisitTotals" ]

  let specials = []

  let consistencyCheck () =
    ExpectoTestCommon.consistencyCheck regular []

  [<Tests>]
  let tests =
    ExpectoTestCommon.makeTests
      "AltCoverMonitorTests"
      consistencyCheck
      regular
      specials
      ignore

module UnitTestStub =
  [<EntryPoint; System.Runtime.CompilerServices.CompilerGenerated>]
  let unitTestStub argv =
    runTestsWithCLIArgs Seq.empty<CLIArguments> argv ExpectoMain.tests
#endif