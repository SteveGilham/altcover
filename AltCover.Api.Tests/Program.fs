﻿namespace Tests

#if !NET472

open Expecto

module ExpectoMain =
  let regular =
    [ Tests.TestCommonTests.TestMultiple, "TestCommon.TestMultiple"
      Tests.TestCommonTests.TestIgnoredTests, "TestCommon.TestIgnoredTests"
      Tests.TestCommonTests.ExerciseItAll, "TestCommon.ExerciseItAll"
      Tests.TestCommonTests.SelfTest, "TestCommon.SelfTest"
      Tests.FSApiTests.FormatFromCoverletMeetsSpec, "FSApiTests.FormatFromCoverlet"
      Tests.FSApiTests.PostprocessShouldRestoreBranchOnlyOpenCoverState,
      "FSApiTests.PostprocessShouldRestoreBranchOnlyOpenCoverState"
      Tests.FSApiTests.JsonToOpenCover, "FSApiTests.JsonToOpenCover"
      Tests.FSApiTests.JsonWithPartialsToOpenCover,
      "FSApiTests.JsonWithPartialsToOpenCover"
      Tests.FSApiTests.JsonFromCoverletShouldHaveBranchExitValuesOK,
      "FSApiTests.JsonFromCoverletShouldHaveBranchExitValuesOK"
      Tests.FSApiTests.OpenCoverToJson, "FSApiTests.OpenCoverToJson"
      Tests.FSApiTests.OpenCoverWithPartialsToJson,
      "FSApiTests.OpenCoverWithPartialsToJson"
      Tests.FSApiTests.OpenCoverToLcov, "FSApiTests.OpenCoverToLcov"
      Tests.FSApiTests.OpenCoverWithPartialsToLcov,
      "FSApiTests.OpenCoverWithPartialsToLcov"
      Tests.FSApiTests.OpenCoverToBarChart, "FSApiTests.OpenCoverToBarChart"
      Tests.FSApiTests.OpenCoverToNCover, "FSApiTests.OpenCoverToNCover"
      Tests.FSApiTests.OpenCoverWithPartialsToNCover,
      "FSApiTests.OpenCoverWithPartialsToNCover"
      Tests.FSApiTests.OpenCoverFromNCover, "FSApiTests.OpenCoverFromNCover"
      Tests.FSApiTests.OpenCoverFromNCoverWithPartials,
      "FSApiTests.OpenCoverFromNCoverWithPartials"
      Tests.FSApiTests.FormatsConvertToXmlDocument,
      "FSApiTests.FormatsConvertToXmlDocument"
      Tests.FSApiTests.FormatsConvertToXDocument, "FSApiTests.FormatsConvertToXDocument"
      Tests.FSApiTests.FormatsRoundTripSimply, "FSApiTests.FormatsRoundTripSimply"
      Tests.FSApiTests.NCoverToCobertura, "FSApiTests.NCoverToCobertura"
      Tests.FSApiTests.NCoverWithPartialsToCobertura,
      "FSApiTests.NCoverWithPartialsToCobertura"
      Tests.FSApiTests.NCoverToJson, "FSApiTests.NCoverToJson"
      Tests.FSApiTests.NCoverWithPartialsToJson, "FSApiTests.NCoverWithPartialsToJson"
      Tests.FSApiTests.NCoverToJsonWithEmbeds, "FSApiTests.NCoverToJsonWithEmbeds"
      Tests.FSApiTests.NCoverToBarChart, "FSApiTests.NCoverToBarChart"
      Tests.FSApiTests.OpenCoverBranchCompression, "FSApiTests.OpenCoverBranchCompression"
      Tests.FSApiTests.ArgumentsBuilt, "FSApiTests.ArgumentsBuilt"
      Tests.FSApiTests.ArgumentsConsistent, "FSApiTests.ArgumentsConsistent"
      Tests.FSApiTests.LoggingCanBeExercised, "FSApiTests.LoggingCanBeExercised"
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

    runTestsWithCLIArgs Seq.empty<CLIArguments> argv ExpectoMain.tests
#endif