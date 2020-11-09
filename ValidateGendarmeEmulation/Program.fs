namespace Tests

#if NETCOREAPP3_0

open Expecto

module ExpectoMain =

  let regular = [
          Tests.TestCommonTests.ExerciseItAll, "TestCommonTests.ExerciseItAll"
          Tests.TestCommonTests.SelfTest, "TestCommonTests.SelfTest"
          Tests.ValidateGendarmeEmulation.DoSelfTest, "ValidateGendarmeEmulation.DoSelfTest"
          Tests.ValidateGendarmeEmulation.ShouldMatchGendarmeComplexityInAltCover, "ValidateGendarmeEmulation.ShouldMatchGendarmeComplexityInAltCover"
          Tests.ValidateGendarmeEmulation.ShouldMatchGendarmeComplexityInSamples, "ValidateGendarmeEmulation.ShouldMatchGendarmeComplexityInSamples"
          Tests.ValidateGendarmeEmulation.ShouldMatchGendarmeComplexityInMonoSamples, "ValidateGendarmeEmulation.ShouldMatchGendarmeComplexityInMonoSamples"
          Tests.ValidateGendarmeEmulation.GratuitousCoverage, "ValidateGendarmeEmulation.GratuitousCoverage"
        ]

  let specials =
   []

  let consistencyCheck() =
    ExpectoTestCommon.consistencyCheck regular []

  [<Tests>]
  let tests =
    ExpectoTestCommon.makeTests "AltCover.ValidateGendarmeEmulation"
                                   consistencyCheck regular specials
                                   ignore

module UnitTestStub =
  [<EntryPoint; System.Runtime.CompilerServices.CompilerGenerated>]
  let unitTestStub argv =
    let writeResults = TestResults.writeNUnitSummary ("AltCover.ValidateGendarmeEmulation.TestResults.xml", "AltCover.ValidateGendarmeEmulation")
    let config = defaultConfig.appendSummaryHandler writeResults
    runTestsWithArgs config argv ExpectoMain.tests
#endif