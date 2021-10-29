namespace Tests

#if !NET472

open Expecto

module ExpectoMain =
  let regular =
    [ Tests.TestCommonTests.TestIgnoredTests, "TestCommonTests.TestIgnoredTests"
      Tests.TestCommonTests.ExerciseItAll, "TestCommonTests.ExerciseItAll"
      Tests.TestCommonTests.SelfTest, "TestCommonTests.SelfTest"
      Tests.FSApiTests.FormatFromCoverletMeetsSpec, "FSApiTests.FormatFromCoverlet"
      Tests.FSApiTests.PostprocessShouldRestoreBranchOnlyOpenCoverState,
      "FSApiTests.PostprocessShouldRestoreBranchOnlyOpenCoverState"
      Tests.FSApiTests.JsonToOpenCover, "FSApiTests.JsonToOpenCover"
      Tests.FSApiTests.OpenCoverToJson, "FSApiTests.OpenCoverToJson"
      Tests.FSApiTests.OpenCoverToLcov, "FSApiTests.OpenCoverToLcov"
      Tests.FSApiTests.OpenCoverToBarChart, "FSApiTests.OpenCoverToBarChart"
      Tests.FSApiTests.OpenCoverToNCover, "FSApiTests.OpenCoverToNCover"
      Tests.FSApiTests.OpenCoverFromNCover, "FSApiTests.OpenCoverFromNCover"
      Tests.FSApiTests.FormatsConvertToXmlDocument,
      "FSApiTests.FormatsConvertToXmlDocument"
      Tests.FSApiTests.FormatsConvertToXDocument, "FSApiTests.FormatsConvertToXDocument"
      Tests.FSApiTests.FormatsRoundTripSimply, "FSApiTests.FormatsRoundTripSimply"
      Tests.FSApiTests.NCoverToCobertura, "FSApiTests.NCoverToCobertura"
      Tests.FSApiTests.NCoverToJson, "FSApiTests.NCoverToJson"
      Tests.FSApiTests.NCoverToJsonWithEmbeds, "FSApiTests.NCoverToJsonWithEmbeds"
      Tests.FSApiTests.NCoverToBarChart, "FSApiTests.NCoverToBarChart"
      Tests.FSApiTests.OpenCoverBranchCompression, "FSApiTests.OpenCoverBranchCompression"
      Tests.FSApiTests.ArgumentsBuilt, "FSApiTests.ArgumentsBuilt"
      Tests.FSApiTests.ArgumentsConsistent, "FSApiTests.ArgumentsConsistent"
      Tests.FSApiTests.MergeRejectsNonCoverage, "FSApiTests.MergeRejectsNonCoverage"
      Tests.FSApiTests.MergePassesSingleOpenCover, "FSApiTests.MergePassesSingleOpenCover"
      Tests.FSApiTests.MergeCombinesSummaryCoverage,
      "FSApiTests.MergeCombinesSummaryCoverage"
      Tests.FSApiTests.MergeCombinesRepeatCoverage,
      "FSApiTests.MergeCombinesRepeatCoverage"
#if SOURCEMAP
      Tests.FSApiTests.NCoverFindsFiles, "FSApiTests.NCoverFindsFiles"
      Tests.FSApiTests.OpenCoverFindsFiles, "FSApiTests.OpenCoverFindsFiles"
#endif
      ]

  let specials = []

  let consistencyCheck () =
    ExpectoTestCommon.consistencyCheck regular []

  [<Tests>]
  let tests =
    ExpectoTestCommon.makeTests
      "AltCoverApiTests"
      consistencyCheck
      regular
      specials
      ignore

module UnitTestStub =
  [<EntryPoint; System.Runtime.CompilerServices.CompilerGenerated>]
  let unitTestStub argv =
    let writeResults =
      TestResults.writeNUnitSummary ("AltCover.Api.TestResults.xml", "AltCover.Api.Tests")

    let config =
      defaultConfig.appendSummaryHandler writeResults

    runTestsWithArgs config argv ExpectoMain.tests
#endif