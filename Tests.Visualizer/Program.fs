namespace Tests

#if NETCOREAPP3_0

open Expecto

module TestMain =
  let regular = [
          Tests.TestCommonTests.ExerciseItAll, "TestCommonTests.ExerciseItAll"
          Tests.TestCommonTests.SelfTest, "TestCommonTests.SelfTest"
          Tests.VisualizerTests.AugmentNullableDetectNulls, "VisualizerTests.AugmentNullableDetectNulls"
          Tests.VisualizerTests.AugmentNonNullableDetectNoNulls, "VisualizerTests.AugmentNonNullableDetectNoNulls"
          Tests.VisualizerTests.DefaultHelperPassesThrough, "VisualizerTests.DefaultHelperPassesThrough"
        ]

  let specials =
   []

  let consistencyCheck() =
    ExpectoTestCommon.consistencyCheck regular []

  [<Tests>]
  let tests =
    ExpectoTestCommon.makeTests "AltCover.Visualizer"
                                   consistencyCheck regular specials
                                   ignore

module UnitTestStub =
  [<EntryPoint; System.Runtime.CompilerServices.CompilerGenerated>]
  let unitTestStub argv =
    let writeResults = TestResults.writeNUnitSummary ("AltCover.Visualizer.TestResults.xml", "AltCover.Visualizer.Tests")
    let config = defaultConfig.appendSummaryHandler writeResults
    runTestsWithArgs config argv TestMain.tests
#endif