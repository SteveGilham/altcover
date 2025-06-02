namespace Tests

#if !NET472

open Expecto

module ExpectoMain =

  let regular =
    [ Tests.TestCommonTests.TestMultiple, "TestCommon.TestMultiple"
      Tests.TestCommonTests.TestIgnoredTests, "TestCommon.TestIgnoredTests"
      Tests.TestCommonTests.ExerciseItAll, "TestCommon.ExerciseItAll"
      Tests.TestCommonTests.SelfTest, "TestCommon.SelfTest"
      Tests.ValidateGendarmeEmulation.DoSelfTest, "ValidateGendarmeEmulation.DoSelfTest"
      Tests.ValidateGendarmeEmulation.ShouldMatchGendarmeComplexityInAltCover,
      "ValidateGendarmeEmulation.ShouldMatchGendarmeComplexityInAltCover"
      Tests.ValidateGendarmeEmulation.ShouldMatchGendarmeComplexityInSamples,
      "ValidateGendarmeEmulation.ShouldMatchGendarmeComplexityInSamples"
      Tests.ValidateGendarmeEmulation.ShouldMatchGendarmeComplexityInMonoSamples,
      "ValidateGendarmeEmulation.ShouldMatchGendarmeComplexityInMonoSamples"
      Tests.ValidateGendarmeEmulation.GratuitousCoverage,
      "ValidateGendarmeEmulation.GratuitousCoverage" ]

  let specials = []

  let consistencyCheck () =
    ExpectoTestCommon.consistencyCheck regular []

  [<Tests>]
  let tests =
    ExpectoTestCommon.makeTests
      "AltCover.ValidateGendarmeEmulation"
      consistencyCheck
      regular
      specials
      ignore

module UnitTestStub =
  [<EntryPoint; System.Runtime.CompilerServices.CompilerGenerated>]
  let unitTestStub argv =

    runTestsWithCLIArgs Seq.empty<CLIArguments> argv ExpectoMain.tests
#endif