namespace Tests

#if NETCOREAPP3_0

open Expecto

module ExpectoMain =
  let regular = [
          Tests.TestCommonTests.ExerciseItAll, "TestCommonTests.ExerciseItAll"
          Tests.TestCommonTests.SelfTest, "TestCommonTests.SelfTest"
          Tests.FSApiTests.FormatFromCoverletMeetsSpec, "FSApiTests.FormatFromCoverlet"
          Tests.FSApiTests.PostprocessShouldRestoreBranchOnlyOpenCoverState, "FSApiTests.PostprocessShouldRestoreBranchOnlyOpenCoverState"
          Tests.FSApiTests.OpenCoverToLcov, "FSApiTests.OpenCoverToLcov"
          Tests.FSApiTests.OpenCoverToBarChart, "FSApiTests.OpenCoverToBarChart"
          Tests.FSApiTests.OpenCoverToNCover, "FSApiTests.OpenCoverToNCover"
          Tests.FSApiTests.OpenCoverFromNCover, "FSApiTests.OpenCoverFromNCover"
          Tests.FSApiTests.FormatsConvertToXmlDocument, "FSApiTests.FormatsConvertToXmlDocument"
          Tests.FSApiTests.FormatsConvertToXDocument, "FSApiTests.FormatsConvertToXDocument"
          Tests.FSApiTests.FormatsRoundTripSimply, "FSApiTests.FormatsRoundTripSimply"
          Tests.FSApiTests.NCoverToCobertura, "FSApiTests.NCoverToCobertura"
          Tests.FSApiTests.NCoverToBarChart, "FSApiTests.NCoverToBarChart"
          Tests.FSApiTests.OpenCoverBranchCompression, "FSApiTests.OpenCoverBranchCompression"
          Tests.FSApiTests.ArgumentsBuilt, "FSApiTests.ArgumentsBuilt"
#if SOURCEMAP
          Tests.FSApiTests.NCoverFindsFiles, "FSApiTests.NCoverFindsFiles"
          Tests.FSApiTests.OpenCoverFindsFiles, "FSApiTests.OpenCoverFindsFiles"
#endif
          Tests.FSApiTests.ArgumentsConsistent, "FSApiTests.ArgumentsConsistent"
        ]

  let specials =
   []

  let consistencyCheck() =
    ExpectoTestCommon.consistencyCheck regular []

  [<Tests>]
  let tests =
    ExpectoTestCommon.makeTests "AltCoverApiTests" consistencyCheck regular specials ignore

module UnitTestStub =
  [<EntryPoint; System.Runtime.CompilerServices.CompilerGenerated>]
  let unitTestStub argv =
    let writeResults = TestResults.writeNUnitSummary ("AltCover.Api.TestResults.xml", "AltCover.Api.Tests")
    let config = defaultConfig.appendSummaryHandler writeResults
    runTestsWithArgs config argv ExpectoMain.tests
#endif