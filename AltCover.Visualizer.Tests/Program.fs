namespace Tests

#if !NET472

open Expecto

module TestMain =
  let regular =
    [ Tests.TestCommonTests.TestMultiple, "TestCommon.TestMultiple"
      Tests.TestCommonTests.ExerciseItAll, "TestCommon.ExerciseItAll"
      Tests.TestCommonTests.SelfTest, "TestCommon.SelfTest"
      Tests.TestCommonTests.TestIgnoredTests, "TestCommon.TestIgnoredTests"
      Tests.VisualizerTests.AugmentNullableDetectNulls,
      "VisualizerTests.AugmentNullableDetectNulls"
      Tests.VisualizerTests.AugmentNonNullableDetectNoNulls,
      "VisualizerTests.AugmentNonNullableDetectNoNulls"
      Tests.VisualizerTests.DefaultHelperPassesThrough,
      "VisualizerTests.DefaultHelperPassesThrough"
      Tests.VisualizerTests.CoberturaToNCoverIsOK, "VisualizerTests.CoberturaToNCoverIsOK" ]

  let specials = []

  let consistencyCheck () =
    ExpectoTestCommon.consistencyCheck regular []

  [<Tests>]
  let tests =
    ExpectoTestCommon.makeTests
      "AltCover.Visualizer"
      consistencyCheck
      regular
      specials
      ignore

module UnitTestStub =
  [<EntryPoint; System.Runtime.CompilerServices.CompilerGenerated>]
  let unitTestStub argv =

    runTestsWithCLIArgs Seq.empty<CLIArguments> argv TestMain.tests
#endif