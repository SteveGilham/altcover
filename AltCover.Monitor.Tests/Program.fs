namespace Tests

#if !NET472

open Expecto

module ExpectoMain =
  let regular =
    [ Tests.TestCommonTests.TestMultiple, "Tests.TestCommonTests.TestMultiple"
      Tests.TestCommonTests.TestIgnoredTests, "TestCommonTests.TestIgnoredTests"
      Tests.TestCommonTests.ExerciseItAll, "TestCommonTests.ExerciseItAll"
      Tests.TestCommonTests.SelfTest, "TestCommonTests.SelfTest"
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