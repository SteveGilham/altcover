namespace Tests
#if NET472
does not build using the framework version
#endif

#if EXPECTO_MAIN
module Manifest =
  let regular =
    [|
#else
module ExpectoTestManifest =
  let simpleTests () =
    [|
#endif
       Tests.TestCommonTests.TestMultiple, "Tests.TestCommonTests.TestMultiple"
       Tests.TestCommonTests.TestIgnoredTests, "TestCommonTests.TestIgnoredTests"
       Tests.TestCommonTests.ExerciseItAll, "TestCommonTests.ExerciseItAll"
       Tests.TestCommonTests.SelfTest, "TestCommonTests.SelfTest"
       Tests.AltCoverTests3.ShouldLaunchWithExpectedOutput,
       "Tests3.ShouldLaunchWithExpectedOutput"
       Tests.AltCoverTests3.ShouldHaveExpectedOptions, "Tests3.ShouldHaveExpectedOptions"
       Tests.AltCoverTests3.ParsingJunkIsAnError, "Tests3.ParsingJunkIsAnError"
       Tests.AltCoverTests3.ParsingJunkBeforeSeparatorIsAnError,
       "Tests3.ParsingJunkBeforeSeparatorIsAnError"
       Tests.AltCoverTests3.ParsingJunkAfterSeparatorIsExpected,
       "Tests3.ParsingJunkAfterSeparatorIsExpected"
       Tests.AltCoverTests3.ParsingHelpGivesHelp, "Tests3.ParsingHelpGivesHelp"
       Tests.AltCoverTests3.ParsingErrorHelpGivesHelp, "Tests3.ParsingErrorHelpGivesHelp"
       Tests.AltCoverTests3.ParsingAttributesGivesAttributes,
       "Tests3.ParsingAttributesGivesAttributes"
       Tests.AltCoverTests3.ParsingTopLevelGivesTopLevel,
       "Tests3.ParsingTopLevelGivesTopLevel"
       Tests.AltCoverTests3.ParsingMethodsGivesMethods,
       "Tests3.ParsingMethodsGivesMethods"
       Tests.AltCoverTests3.ParsingTypesGivesTypes, "Tests3.ParsingTypesGivesTypes"
       Tests.AltCoverTests3.ParsingAssembliesGivesAssemblies,
       "Tests3.ParsingAssembliesGivesAssemblies"
       Tests.AltCoverTests3.ParsingEscapeCasesWork, "Tests3.ParsingEscapeCasesWork"
       Tests.AltCoverTests3.ParsingModulesGivesModules,
       "Tests3.ParsingModulesGivesModules"
       Tests.AltCoverTests3.ParsingFilesGivesFiles, "Tests3.ParsingFilesGivesFiles"
       Tests.AltCoverTests3.ParsingPathsGivesPaths, "Tests3.ParsingPathsGivesPaths"
       Tests.AltCoverTests3.ParsingReportGivesReport, "Tests3.ParsingReportGivesReport"
       Tests.AltCoverTests3.ParsingMultipleReportGivesFailure,
       "Tests3.ParsingMultipleReportGivesFailure"
       Tests.AltCoverTests3.ParsingBadReportGivesFailure,
       "Tests3.ParsingBadReportGivesFailure"
       Tests.AltCoverTests3.ParsingNoReportGivesFailure,
       "Tests3.ParsingNoReportGivesFailure"
       Tests.AltCoverTests3.ParsingEmptyReportGivesFailure,
       "Tests3.ParsingEmptyReportGivesFailure"
       Tests.AltCoverTests3.ParsingInputGivesInput, "Tests3.ParsingInputGivesInput"
       Tests.AltCoverTests3.ParsingMultipleInputIsOKToo,
       "Tests3.ParsingMultipleInputIsOKToo"
       Tests.AltCoverTests3.ParsingDuplicateInputGivesFailure,
       "Tests3.ParsingDuplicateInputGivesFailure"
       Tests.AltCoverTests3.ParsingBadInputGivesFailure,
       "Tests3.ParsingBadInputGivesFailure"
       Tests.AltCoverTests3.ParsingNoInputGivesFailure,
       "Tests3.ParsingNoInputGivesFailure"
       Tests.AltCoverTests3.ParsingOutputGivesOutput, "Tests3.ParsingOutputGivesOutput"
       Tests.AltCoverTests3.ParsingDuplicateOutputGivesFailure,
       "Tests3.ParsingDuplicateOutputGivesFailure"
       Tests.AltCoverTests3.ParsingMultipleOutputIsOK, "Tests3.ParsingMultipleOutputIsOK"
       Tests.AltCoverTests3.ParsingBadOutputGivesFailure,
       "Tests3.ParsingBadOutputGivesFailure"
       Tests.AltCoverTests3.ParsingNoOutputGivesFailure,
       "Tests3.ParsingNoOutputGivesFailure"
       Tests.AltCoverTests3.ParsingEmptyOutputGivesFailure,
       "Tests3.ParsingEmptyOutputGivesFailure"
       Tests.AltCoverTests3.ParsingSymbolGivesSymbol, "Tests3.ParsingSymbolGivesSymbol"
       Tests.AltCoverTests3.ParsingMultipleSymbolGivesOK,
       "Tests3.ParsingMultipleSymbolGivesOK"
       Tests.AltCoverTests3.ParsingBadSymbolGivesFailure,
       "Tests3.ParsingBadSymbolGivesFailure"
       Tests.AltCoverTests3.ParsingNoSymbolGivesFailure,
       "Tests3.ParsingNoSymbolGivesFailure"
       Tests.AltCoverTests3.ParsingMultipleDependencyIsOk,
       "Tests3.ParsingMultipleDependencyIsOk"
       Tests.AltCoverTests3.ParsingBadDependencyGivesFailure,
       "Tests3.ParsingBadDependencyGivesFailure"
       Tests.AltCoverTests3.ParsingNonDependencyGivesFailure,
       "Tests3.ParsingNonDependencyGivesFailure"
       Tests.AltCoverTests3.ParsingStrongNameGivesStrongName,
       "Tests3.ParsingStrongNameGivesStrongName"
       Tests.AltCoverTests3.ParsingMultipleStrongNameGivesFailure,
       "Tests3.ParsingMultipleStrongNameGivesFailure"
       Tests.AltCoverTests3.ParsingBadStrongNameGivesFailure,
       "Tests3.ParsingBadStrongNameGivesFailure"
       Tests.AltCoverTests3.ParsingNonStrongNameGivesFailure,
       "Tests3.ParsingNonStrongNameGivesFailure"
       Tests.AltCoverTests3.ParsingNoStrongNameGivesFailure,
       "Tests3.ParsingNoStrongNameGivesFailure"
       Tests.AltCoverTests3.ParsingMultipleAltStrongNameIsOk,
       "Tests3.ParsingMultipleAltStrongNameIsOk"
       Tests.AltCoverTests3.ParsingNoAltStrongNameGivesFailure,
       "Tests3.ParsingNoAltStrongNameGivesFailure"
       Tests.AltCoverTests3.ParsingBadAltStrongNameGivesFailure,
       "Tests3.ParsingBadAltStrongNameGivesFailure"
       Tests.AltCoverTests3.ParsingNonAltsStrongNameGivesFailure,
       "Tests3.ParsingNonAltsStrongNameGivesFailure"
       Tests.AltCoverTests3.ParsingLocalGivesLocal, "Tests3.ParsingLocalGivesLocal"
       Tests.AltCoverTests3.ParsingMultipleLocalGivesFailure,
       "Tests3.ParsingMultipleLocalGivesFailure"
       Tests.AltCoverTests3.ParsingVisibleGivesVisible,
       "Tests3.ParsingVisibleGivesVisible"
       Tests.AltCoverTests3.ParsingMultipleVisibleGivesFailure,
       "Tests3.ParsingMultipleVisibleGivesFailure"
       Tests.AltCoverTests3.ParsingTimeGivesTime, "Tests3.ParsingTimeGivesTime"
       Tests.AltCoverTests3.ParsingOnlyArabicNumeralsNotThatSortofArabicNumeralsGivesTime,
       "Tests3.ParsingOnlyArabicNumeralsNotThatSortofArabicNumeralsGivesTime"
       Tests.AltCoverTests3.ParsingMultipleTimesGivesFailure,
       "Tests3.ParsingMultipleTimesGivesFailure"
       Tests.AltCoverTests3.ParsingTimeAndNamesGivesOK,
       "Tests3.ParsingTimeAndNamesGivesOK"
       Tests.AltCoverTests3.ParsingBadTimeGivesNoOp, "Tests3.ParsingBadTimeGivesNoOp"
       Tests.AltCoverTests3.ParsingNonTimeGivesFailure,
       "Tests3.ParsingNonTimeGivesFailure"
       Tests.AltCoverTests3.ParsingNoTimeGivesFailure, "Tests3.ParsingNoTimeGivesFailure"
       Tests.AltCoverTests3.ParsingJsonFormatGivesJson,
       "Tests3.ParsingJsonFormatGivesJson"
       Tests.AltCoverTests3.ParsingNCoverFormatGivesNCover,
       "Tests3.ParsingNCoverFormatGivesNCover"
       Tests.AltCoverTests3.ParsingOpenCoverFormatGivesOpenCover,
       "Tests3.ParsingOpenCoverFormatGivesOpenCover"
       Tests.AltCoverTests3.ParsingMultipleReportFormatGivesFailure,
       "Tests3.ParsingMultipleReportFormatGivesFailure"
       Tests.AltCoverTests3.ParsingInPlaceGivesInPlace,
       "Tests3.ParsingInPlaceGivesInPlace"
       Tests.AltCoverTests3.ParsingMultipleInPlaceGivesFailure,
       "Tests3.ParsingMultipleInPlaceGivesFailure"
       Tests.AltCoverTests3.ParsingSaveGivesSave, "Tests3.ParsingSaveGivesSave"
       Tests.AltCoverTests3.ParsingMultipleSaveGivesFailure,
       "Tests3.ParsingMultipleSaveGivesFailure"
       Tests.AltCoverTests3.ParsingAllGivesAll, "Tests3.ParsingAllGivesAll"
       Tests.AltCoverTests3.ParsingMultipleAllGivesFailure,
       "Tests3.ParsingMultipleAllGivesFailure"
       Tests.AltCoverTests3.ParsingLineCoverGivesLineCover,
       "Tests3.ParsingLineCoverGivesLineCover"
       Tests.AltCoverTests3.OpenCoverIsCompatibleWithLineCover,
       "Tests3.OpenCoverIsCompatibleWithLineCover"
       Tests.AltCoverTests3.LineCoverIsCompatibleWithOpenCover,
       "Tests3.LineCoverIsCompatibleWithOpenCover"
       Tests.AltCoverTests3.ParsingMultipleLineCoverGivesFailure,
       "Tests3.ParsingMultipleLineCoverGivesFailure"
       Tests.AltCoverTests3.LineCoverIsNotCompatibleWithBranchCover,
       "Tests3.LineCoverIsNotCompatibleWithBranchCover"
       Tests.AltCoverTests3.ParsingBranchCoverGivesBranchCover,
       "Tests3.ParsingBranchCoverGivesBranchCover"
       Tests.AltCoverTests3.OpenCoverIsCompatibleWithBranchCover,
       "Tests3.OpenCoverIsCompatibleWithBranchCover"
       Tests.AltCoverTests3.BranchCoverIsCompatibleWithOpenCover,
       "Tests3.BranchCoverIsCompatibleWithOpenCover"
       Tests.AltCoverTests3.ParsingMultipleBranchCoverGivesFailure,
       "Tests3.ParsingMultipleBranchCoverGivesFailure"
       Tests.AltCoverTests3.BranchCoverIsNotCompatibleWithLineCover,
       "Tests3.BranchCoverIsNotCompatibleWithLineCover"
       Tests.AltCoverTests3.ParsingDropGivesDrop, "Tests3.ParsingDropGivesDrop"
       Tests.AltCoverTests3.ParsingMultipleDropGivesFailure,
       "Tests3.ParsingMultipleDropGivesFailure"
       Tests.AltCoverTests3.ParsingEagerWorks, "Tests3.ParsingEagerWorks"
       Tests.AltCoverTests3.ParsingMultipleEagerGivesFailure,
       "Tests3.ParsingMultipleEagerGivesFailure"
       Tests.AltCoverTests3.ParsingStaticGivesStatic, "Tests3.ParsingStaticGivesStatic"
       Tests.AltCoverTests3.ParsingStaticPlusGivesStatic,
       "Tests3.ParsingStaticPlusGivesStatic"
       Tests.AltCoverTests3.ParsingStaticPlusPlusGivesStaticPlus,
       "Tests3.ParsingStaticPlusPlusGivesStaticPlus"
       Tests.AltCoverTests3.ParsingStaticMinusGivesNoStatic,
       "Tests3.ParsingStaticMinusGivesNoStatic"
       Tests.AltCoverTests3.ParsingMultipleStaticGivesFailure,
       "Tests3.ParsingMultipleStaticGivesFailure"
       Tests.AltCoverTests3.ParsingJunkStaticGivesFailure,
       "Tests3.ParsingJunkStaticGivesFailure"
       Tests.AltCoverTests3.ParsingQuietWorks, "Tests3.ParsingQuietWorks"
       Tests.AltCoverTests3.ParsingMultiQuietWorks, "Tests3.ParsingMultiQuietWorks"
       Tests.AltCoverTests3.ParsingBatchMultiQuietWorks,
       "Tests3.ParsingBatchMultiQuietWorks"
       Tests.AltCoverTests3.ParsingVerboseWorks, "Tests3.ParsingVerboseWorks"
       Tests.AltCoverTests3.ParsingMixedQuietWorks, "Tests3.ParsingMixedQuietWorks"
       Tests.AltCoverTests3.OutputLeftPassesThrough, "Tests3.OutputLeftPassesThrough"
       Tests.AltCoverTests3.OutputInPlaceFails, "Tests3.OutputInPlaceFails"
       Tests.AltCoverTests3.PortableFailsOnMultiInputs,
       "Tests3.PortableFailsOnMultiInputs"
       Tests.AltCoverTests3.ScreeningFilesShouldRejectTheInstrumentedOnes,
       "Tests3.ScreeningFilesShouldRejectTheInstrumentedOnes"
       Tests.AltCoverTests3.OutputToNewPlaceIsOK, "Tests3.OutputToNewPlaceIsOK"
       Tests.AltCoverTests3.OutputToReallyNewPlaceIsOK,
       "Tests3.OutputToReallyNewPlaceIsOK"
       Tests.AltCoverTests3.InPlaceToExistingPlaceFails,
       "Tests3.InPlaceToExistingPlaceFails"
       Tests.AltCoverTests3.InPlaceOperationIsAsExpected,
       "Tests3.InPlaceOperationIsAsExpected"
       Tests.AltCoverTests3.ImageLoadResilientPassesThrough,
       "Tests3.ImageLoadResilientPassesThrough"
       Tests.AltCoverTests3.ResilientHandlesIOException,
       "Tests3.ResilientHandlesIOException"
       Tests.AltCoverTests3.ResilientHandlesSymbolReadException,
       "Tests3.ResilientHandlesSymbolReadException"
       Tests.AltCoverTests3.ResilientHandlesBadImageFormatException,
       "Tests3.ResilientHandlesBadImageFormatException"
       Tests.AltCoverTests3.ResilientHandlesArgumentException,
       "Tests3.ResilientHandlesArgumentException"
       Tests.AltCoverTests3.FolderNestingIsDetectedCorrectly,
       "Tests3.FolderNestingIsDetectedCorrectly"
       Tests.AltCoverTests3.PreparingNewPlaceShouldCopyEverything,
       "Tests3.PreparingNewPlaceShouldCopyEverything"
       Tests.AltCoverTests3.ShouldProcessTrailingArguments,
       "Tests3.ShouldProcessTrailingArguments"
       Tests.AltCoverTests3.StoresAsExpected, "Tests3.StoresAsExpected"
       Tests.AltCoverTests3.ImportModuleIsAsExpected, "Tests3.ImportModuleIsAsExpected"
       Tests.AltCoverTests3.VersionIsAsExpected, "Tests3.VersionIsAsExpected"
       Tests.AltCoverTests3.TargetsPathIsAsExpected, "Tests3.TargetsPathIsAsExpected"
       Tests.AltCoverTests3.UsageIsAsExpected, "Tests3.UsageIsAsExpected"
       Tests.AltCoverTests3.ErrorResponseIsAsExpected, "Tests3.ErrorResponseIsAsExpected"
       Tests.AltCoverTests3.LoggingCanBeExercised, "Tests3.LoggingCanBeExercised"
       Tests.AltCoverTests3.EmptyInstrumentIsJustTheDefaults,
       "Tests3.EmptyInstrumentIsJustTheDefaults"
       Tests.AltCoverTests3.InstrumentLevelsCanBeSet, "Tests3.InstrumentLevelsCanBeSet"
       Tests.AltCoverTests3.NonDefaultInstrumentObsoleteIsOK,
       "Tests3.NonDefaultInstrumentObsoleteIsOK"
       Tests.AltCoverTests3.NonDefaultInstrumentIsOK, "Tests3.NonDefaultInstrumentIsOK"
       Tests.AltCoverTests3.EmptyCollectIsJustTheDefaults,
       "Tests3.EmptyCollectIsJustTheDefaults"
       Tests.AltCoverTests3.CollectLevelsCanBeSet, "Tests3.CollectLevelsCanBeSet"
       Tests.AltCoverTests3.CollectWithExeIsNotCollecting,
       "Tests3.CollectWithExeIsNotCollecting"
       Tests.AltCoverTests3.EmptyPowerShellIsJustTheDefaults,
       "Tests3.EmptyPowerShellIsJustTheDefaults"
       Tests.AltCoverTests3.EmptyVersionIsJustTheDefaults,
       "Tests3.EmptyVersionIsJustTheDefaults"
       Tests.AltCoverTests3.EchoWorks, "Tests3.EchoWorks"
       Tests.AltCoverTests3.EchoFallsSilent, "Tests3.EchoFallsSilent"
       Tests.AltCoverTests3.RunSettingsFailsIfCollectorNotFound,
       "Tests3.RunSettingsFailsIfCollectorNotFound"
       Tests.AltCoverTests3.RunSettingsWorksIfOK, "Tests3.RunSettingsWorksIfOK"
       Tests.AltCoverTests3.RunSettingsExtendsOK, "Tests3.RunSettingsExtendsOK"
       Tests.AltCoverTests3.RunSettingsRecoversOK, "Tests3.RunSettingsRecoversOK"
       Tests.AltCoverTests3.RunSettingsThrowsIfUninitialized,
       "Tests3.RunSettingsThrowsIfUninitialized"
       Tests.AltCoverTests3.ContingentCopyTest, "Tests3.ContingentCopyTest"
       Tests.AltCoverTests3.RetryDeleteTest, "Tests3.RetryDeleteTest"
       Tests.AltCoverXTests.CollectOptionsCanBeValidated,
       "XTests.CollectOptionsCanBeValidated"
       Tests.AltCoverXTests.TypeSafeEmptyThresholdCanBeValidated,
       "XTests.TypeSafeEmptyThresholdCanBeValidated"
       Tests.AltCoverXTests.TypeSafeCollectOptionsCanBeValidated,
       "XTests.TypeSafeCollectOptionsCanBeValidated"
       Tests.AltCoverXTests.TypeSafeCollectSummaryCanBeValidated,
       "XTests.TypeSafeCollectSummaryCanBeValidated"
       Tests.AltCoverXTests.CollectOptionsCanBeValidatedWithErrors,
       "XTests.CollectOptionsCanBeValidatedWithErrors"
       Tests.AltCoverXTests.TypeSafeCollectOptionsCanBeValidatedWithErrors,
       "XTests.TypeSafeCollectOptionsCanBeValidatedWithErrors"
       Tests.AltCoverXTests.CollectOptionsCanBePositivelyValidatedWithErrors,
       "XTests.CollectOptionsCanBePositivelyValidatedWithErrors"
       Tests.AltCoverXTests.TypeSafeCollectOptionsCanBePositivelyValidatedWithErrors,
       "XTests.TypeSafeCollectOptionsCanBePositivelyValidatedWithErrors"
       Tests.AltCoverXTests.PrepareOptionsCanBeValidated,
       "XTests.PrepareOptionsCanBeValidated"
       Tests.AltCoverXTests.TypeSafePrepareOptionsCanBeValidated,
       "XTests.TypeSafePrepareOptionsCanBeValidated"
       Tests.AltCoverXTests.TypeSafePrepareOptionsCanBeValidatedAgain,
       "XTests.TypeSafePrepareOptionsCanBeValidatedAgain"
       Tests.AltCoverXTests.PrepareOptionsStrongNamesCanBeValidated,
       "XTests.PrepareOptionsStrongNamesCanBeValidated"
       Tests.AltCoverXTests.TypeSafePrepareOptionsStrongNamesCanBeValidated,
       "XTests.TypeSafePrepareOptionsStrongNamesCanBeValidated"
       Tests.AltCoverXTests.PrepareOptionsCanBeValidatedWithNulls,
       "XTests.PrepareOptionsCanBeValidatedWithNulls"
       Tests.AltCoverXTests.PrepareOptionsCanBeValidatedAndDetectInconsistency,
       "XTests.PrepareOptionsCanBeValidatedAndDetectInconsistency"
       Tests.AltCoverXTests.TypeSafePrepareStaticCanBeValidated,
       "XTests.TypeSafePrepareStaticCanBeValidated"
       Tests.AltCoverXTests.TypeSafePrepareOptionsCanBeValidatedAndDetectInconsistency,
       "XTests.TypeSafePrepareOptionsCanBeValidatedAndDetectInconsistency"
       Tests.AltCoverXTests.PrepareOptionsCanBeValidatedWithErrors,
       "XTests.PrepareOptionsCanBeValidatedWithErrors"
       Tests.AltCoverXTests.NullListsAreEmpty, "XTests.NullListsAreEmpty"
       Tests.AltCoverXTests.ValidateAssemblyOption, "XTests.ValidateAssemblyOption"
       Tests.AltCoverXTests.OutputVerbose, "XTests.OutputVerbose"
       Tests.AltCoverXTests.ADotNetDryRunLooksAsExpected,
       "XTests.ADotNetDryRunLooksAsExpected"
       Tests.AltCoverXTests.ADryRunLooksAsExpected, "XTests.ADryRunLooksAsExpected"
       Tests.AltCoverXTests.AfterAssemblyCommitsThatAssembly,
       "XTests.AfterAssemblyCommitsThatAssembly"
       Tests.AltCoverXTests.AfterAssemblyCommitsThatAssemblyForMono,
       "XTests.AfterAssemblyCommitsThatAssemblyForMono"
       Tests.AltCoverXTests.FinishCommitsTheRecordingAssembly,
       "XTests.FinishCommitsTheRecordingAssembly"
       Tests.AltCoverXTests.FinishCommitsTheAsyncRecordingAssembly,
       "XTests.FinishCommitsTheAsyncRecordingAssembly"
       Tests.AltCoverXTests.ShouldDoCoverage, "XTests.ShouldDoCoverage"
       Tests.AltCoverXTests.ShouldGenerateExpectedXmlReportFromMono,
       "XTests.ShouldGenerateExpectedXmlReportFromMono"
       Tests.AltCoverXTests.ShouldGenerateExpectedXmlReportFromMonoOpenCoverStyle,
       "XTests.ShouldGenerateExpectedXmlReportFromMonoOpenCoverStyle" |]
    |> Array.toList

#if !EXPECTO_MAIN
  let consistencyCheck specials =
    ExpectoTestCommon.consistencyCheck (simpleTests ()) specials //["Tests.AltCoverTests2::ShouldUpdateHandlerOK"]
#endif