namespace Tests
#if NET472  // remove for fantomas

junk goes here  // remove for fantomas

#else // remove for fantomas

#if EXPECTO_MAIN
module Manifest =
  let regular =
    [|
#else
module ExpectoTestManifest =
  let simpleTests () =
    [|
#endif
      Tests.TestCommonTests.TestIgnoredTests, "TestCommonTests.TestIgnoredTests"
      Tests.TestCommonTests.ExerciseItAll, "TestCommonTests.ExerciseItAll"
      Tests.TestCommonTests.SelfTest, "TestCommonTests.SelfTest"
      Tests.AltCoverRunnerTests.ShouldFailXmlDataForNativeJson,
      "Runner.ShouldFailXmlDataForNativeJson"
      Tests.AltCoverRunnerTests.MaxTimeFirst, "Runner.MaxTimeFirst"
      Tests.AltCoverRunnerTests.MaxTimeLast, "Runner.MaxTimeLast"
      Tests.AltCoverRunnerTests.MinTimeFirst, "Runner.MinTimeFirst"
      Tests.AltCoverRunnerTests.MinTimeLast, "Runner.MinTimeLast"
      Tests.AltCoverRunnerTests.JunkUspidGivesNegativeIndex,
      "Runner.JunkUspidGivesNegativeIndex"
      Tests.AltCoverRunnerTests.RealIdShouldIncrementCount,
      "Runner.RealIdShouldIncrementCount"
      Tests.AltCoverRunnerTests.RealIdShouldIncrementList,
      "Runner.RealIdShouldIncrementList"
      Tests.AltCoverRunnerTests.DistinctIdShouldBeDistinct,
      "Runner.DistinctIdShouldBeDistinct"
      Tests.AltCoverRunnerTests.DistinctLineShouldBeDistinct,
      "Runner.DistinctLineShouldBeDistinct"
      Tests.AltCoverRunnerTests.RepeatVisitsShouldIncrementCount,
      "Runner.RepeatVisitsShouldIncrementCount"
      Tests.AltCoverRunnerTests.RepeatVisitsShouldIncrementTotal,
      "Runner.RepeatVisitsShouldIncrementTotal"
      Tests.AltCoverRunnerTests.KnownModuleWithPayloadMakesExpectedChangeInOpenCover,
      "Runner.KnownModuleWithPayloadMakesExpectedChangeInOpenCover"
      Tests.AltCoverRunnerTests.FlushLeavesExpectedTraces,
      "Runner.FlushLeavesExpectedTraces"
      Tests.AltCoverRunnerTests.FlushLeavesExpectedTracesWhenDiverted,
      "Runner.FlushLeavesExpectedTracesWhenDiverted"
      Tests.AltCoverRunnerTests.NCoverShouldGeneratePlausibleJson,
      "Runner.NCoverShouldGeneratePlausibleJson"
      Tests.AltCoverRunnerTests.OpenCoverShouldGeneratePlausibleJson,
      "Runner.OpenCoverShouldGeneratePlausibleJson"
      Tests.AltCoverRunnerTests.UsageIsAsExpected, "Runner.UsageIsAsExpected"
      Tests.AltCoverRunnerTests.ShouldLaunchWithExpectedOutput,
      "Runner.ShouldLaunchWithExpectedOutput"
      Tests.AltCoverRunnerTests.ShouldHaveExpectedOptions,
      "Runner.ShouldHaveExpectedOptions"
      Tests.AltCoverRunnerTests.ParsingJunkIsAnError, "Runner.ParsingJunkIsAnError"
      Tests.AltCoverRunnerTests.ParsingJunkAfterSeparatorIsExpected,
      "Runner.ParsingJunkAfterSeparatorIsExpected"
      Tests.AltCoverRunnerTests.ParsingHelpGivesHelp, "Runner.ParsingHelpGivesHelp"
      Tests.AltCoverRunnerTests.ParsingErrorHelpGivesHelp,
      "Runner.ParsingErrorHelpGivesHelp"
      Tests.AltCoverRunnerTests.ParsingExeGivesExe, "Runner.ParsingExeGivesExe"
      Tests.AltCoverRunnerTests.ParsingMultipleExeGivesFailure,
      "Runner.ParsingMultipleExeGivesFailure"
      Tests.AltCoverRunnerTests.ParsingNoExeGivesFailure,
      "Runner.ParsingNoExeGivesFailure"
      Tests.AltCoverRunnerTests.ParsingWorkerGivesWorker,
      "Runner.ParsingWorkerGivesWorker"
      Tests.AltCoverRunnerTests.ParsingMultipleWorkerGivesFailure,
      "Runner.ParsingMultipleWorkerGivesFailure"
      Tests.AltCoverRunnerTests.ParsingBadWorkerGivesFailure,
      "Runner.ParsingBadWorkerGivesFailure"
      Tests.AltCoverRunnerTests.ParsingNoWorkerGivesFailure,
      "Runner.ParsingNoWorkerGivesFailure"
      Tests.AltCoverRunnerTests.ParsingRecorderGivesRecorder,
      "Runner.ParsingRecorderGivesRecorder"
      Tests.AltCoverRunnerTests.ParsingMultipleRecorderGivesFailure,
      "Runner.ParsingMultipleRecorderGivesFailure"
      Tests.AltCoverRunnerTests.ParsingBadRecorderGivesFailure,
      "Runner.ParsingBadRecorderGivesFailure"
      Tests.AltCoverRunnerTests.ParsingNoRecorderGivesFailure,
      "Runner.ParsingNoRecorderGivesFailure"
      Tests.AltCoverRunnerTests.ParsingCollectGivesCollect,
      "Runner.ParsingCollectGivesCollect"
      Tests.AltCoverRunnerTests.ParsingMultipleCollectGivesFailure,
      "Runner.ParsingMultipleCollectGivesFailure"
      Tests.AltCoverRunnerTests.ParsingLcovGivesLcov, "Runner.ParsingLcovGivesLcov"
      Tests.AltCoverRunnerTests.ParsingMultipleLcovGivesFailure,
      "Runner.ParsingMultipleLcovGivesFailure"
      Tests.AltCoverRunnerTests.ParsingNoLcovGivesFailure,
      "Runner.ParsingNoLcovGivesFailure"
      Tests.AltCoverRunnerTests.ParsingThresholdGivesThreshold,
      "Runner.ParsingThresholdGivesThreshold"
      Tests.AltCoverRunnerTests.ParsingTopThresholdGivesThreshold,
      "Runner.ParsingTopThresholdGivesThreshold"
      Tests.AltCoverRunnerTests.ParsingLowThresholdGivesThreshold,
      "Runner.ParsingLowThresholdGivesThreshold"
      Tests.AltCoverRunnerTests.ParsingComplexThresholdGivesThreshold,
      "Runner.ParsingComplexThresholdGivesThreshold"
      Tests.AltCoverRunnerTests.ParsingMultipleThresholdGivesFailure,
      "Runner.ParsingMultipleThresholdGivesFailure"
      Tests.AltCoverRunnerTests.ParsingBadThresholdGivesFailure,
      "Runner.ParsingBadThresholdGivesFailure"
      Tests.AltCoverRunnerTests.ParsingBadThreshold2GivesFailure,
      "Runner.ParsingBadThreshold2GivesFailure"
      Tests.AltCoverRunnerTests.ParsingBadThreshold3GivesFailure,
      "Runner.ParsingBadThreshold3GivesFailure"
      Tests.AltCoverRunnerTests.ParsingBadThreshold4GivesFailure,
      "Runner.ParsingBadThreshold4GivesFailure"
      Tests.AltCoverRunnerTests.ParsingBadThreshold5GivesFailure,
      "Runner.ParsingBadThreshold5GivesFailure"
      Tests.AltCoverRunnerTests.ParsingBadThreshold6GivesFailure,
      "Runner.ParsingBadThreshold6GivesFailure"
      Tests.AltCoverRunnerTests.ParsingBadThreshold7GivesFailure,
      "Runner.ParsingBadThreshold7GivesFailure"
      Tests.AltCoverRunnerTests.ParsingEmptyThresholdGivesFailure,
      "Runner.ParsingEmptyThresholdGivesFailure"
      Tests.AltCoverRunnerTests.ParsingNoThresholdGivesFailure,
      "Runner.ParsingNoThresholdGivesFailure"
      Tests.AltCoverRunnerTests.ParsingCoberturaGivesCobertura,
      "Runner.ParsingCoberturaGivesCobertura"
      Tests.AltCoverRunnerTests.ParsingMultipleCoberturaGivesFailure,
      "Runner.ParsingMultipleCoberturaGivesFailure"
      Tests.AltCoverRunnerTests.ParsingNoCoberturaGivesFailure,
      "Runner.ParsingNoCoberturaGivesFailure"
      Tests.AltCoverRunnerTests.ParsingOutputGivesOutput,
      "Runner.ParsingOutputGivesOutput"
      Tests.AltCoverRunnerTests.ParsingMultipleOutputGivesFailure,
      "Runner.ParsingMultipleOutputGivesFailure"
      Tests.AltCoverRunnerTests.ParsingNoOutputGivesFailure,
      "Runner.ParsingNoOutputGivesFailure"
      Tests.AltCoverRunnerTests.ParsingDropGivesDrop, "Runner.ParsingDropGivesDrop"
      Tests.AltCoverRunnerTests.ParsingMultipleDropGivesFailure,
      "Runner.ParsingMultipleDropGivesFailure"
      Tests.AltCoverRunnerTests.ParsingTCString, "Runner.ParsingTCString"
      Tests.AltCoverRunnerTests.ParsingTCGivesTC, "Runner.ParsingTCGivesTC"
      Tests.AltCoverRunnerTests.ParsingMultipleTCGivesFailure,
      "Runner.ParsingMultipleTCGivesFailure"
      Tests.AltCoverRunnerTests.ParsingBadTCGivesFailure,
      "Runner.ParsingBadTCGivesFailure"
      Tests.AltCoverRunnerTests.ParsingQuietWorks, "Runner.ParsingQuietWorks"
      Tests.AltCoverRunnerTests.ParsingMultiQuietWorks, "Runner.ParsingMultiQuietWorks"
      Tests.AltCoverRunnerTests.ParsingBatchMultiQuietWorks,
      "Runner.ParsingBatchMultiQuietWorks"
      Tests.AltCoverRunnerTests.ShouldRequireExe, "Runner.ShouldRequireExe"
      Tests.AltCoverRunnerTests.ShouldAcceptExe, "Runner.ShouldAcceptExe"
      Tests.AltCoverRunnerTests.ShouldRequireCollectIfNotExe,
      "Runner.ShouldRequireCollectIfNotExe"
      Tests.AltCoverRunnerTests.ShouldRejectExeIfCollect,
      "Runner.ShouldRejectExeIfCollect"
      Tests.AltCoverRunnerTests.ShouldRequireWorker, "Runner.ShouldRequireWorker"
      Tests.AltCoverRunnerTests.ShouldAcceptWorker, "Runner.ShouldAcceptWorker"
      Tests.AltCoverRunnerTests.ShouldRequireRecorder, "Runner.ShouldRequireRecorder"
      Tests.AltCoverRunnerTests.ShouldRequireRecorderDll,
      "Runner.ShouldRequireRecorderDll"
      Tests.AltCoverRunnerTests.ShouldAcceptRecorder, "Runner.ShouldAcceptRecorder"
      Tests.AltCoverRunnerTests.ShouldHandleReturnCodes, "Runner.ShouldHandleReturnCodes"
      Tests.AltCoverRunnerTests.ShouldProcessTrailingArguments,
      "Runner.ShouldProcessTrailingArguments"
      Tests.AltCoverRunnerTests.ShouldNoOp, "Runner.ShouldNoOp"
      Tests.AltCoverRunnerTests.ErrorResponseIsAsExpected,
      "Runner.ErrorResponseIsAsExpected"
      Tests.AltCoverRunnerTests.ShouldGetStringConstants,
      "Runner.ShouldGetStringConstants"
      Tests.AltCoverRunnerTests.ShouldProcessPayload, "Runner.ShouldProcessPayload"
      Tests.AltCoverRunnerTests.WriteJsonLeavesExpectedTraces,
      "Runner.WriteJsonLeavesExpectedTraces"
      Tests.AltCoverRunnerTests.ZipWriteJsonLeavesExpectedTraces,
      "Runner.ZipWriteJsonLeavesExpectedTraces"
      Tests.AltCoverRunnerTests.NullPayloadShouldReportNothing,
      "Runner.NullPayloadShouldReportNothing"
      Tests.AltCoverRunnerTests.WriteLeavesExpectedTraces,
      "Runner.WriteLeavesExpectedTraces"
      Tests.AltCoverRunnerTests.ZipWriteLeavesExpectedTraces,
      "Runner.ZipWriteLeavesExpectedTraces"
      Tests.AltCoverRunnerTests.ActivePayloadShouldReportAsExpected,
      "Runner.ActivePayloadShouldReportAsExpected"
      Tests.AltCoverRunnerTests.CollectShouldReportAsExpected,
      "Runner.CollectShouldReportAsExpected"
      Tests.AltCoverRunnerTests.JunkPayloadShouldReportAsExpected,
      "Runner.JunkPayloadShouldReportAsExpected"
      Tests.AltCoverRunnerTests.TrackingPayloadShouldReportAsExpected,
      "Runner.TrackingPayloadShouldReportAsExpected"
      Tests.AltCoverRunnerTests.PointProcessShouldCaptureTimes,
      "Runner.PointProcessShouldCaptureTimes"
      Tests.AltCoverRunnerTests.PostprocessShouldHandleNullCase,
      "Runner.PostprocessShouldHandleNullCase"
      Tests.AltCoverRunnerTests.PostprocessShouldHandleEntryAndExitTimes,
      "Runner.PostprocessShouldHandleEntryAndExitTimes"
      Tests.AltCoverRunnerTests.PostprocessShouldRestoreKnownOpenCoverState,
      "Runner.PostprocessShouldRestoreKnownOpenCoverState"
      Tests.AltCoverRunnerTests.PostprocessShouldRestoreKnownOpenCoverStateFromMono,
      "Runner.PostprocessShouldRestoreKnownOpenCoverStateFromMono"
      Tests.AltCoverRunnerTests.PostprocessShouldRestoreDegenerateOpenCoverState,
      "Runner.PostprocessShouldRestoreDegenerateOpenCoverState"
      Tests.AltCoverRunnerTests.PostprocessShouldRestoreBranchOnlyOpenCoverState,
      "Runner.PostprocessShouldRestoreBranchOnlyOpenCoverState"
      Tests.AltCoverRunnerTests.PostprocessShouldRestoreBranchOnlyOpenCoverStateXDoc,
      "Runner.PostprocessShouldRestoreBranchOnlyOpenCoverStateXDoc"
      Tests.AltCoverRunnerTests.JunkTokenShouldDefaultZero,
      "Runner.JunkTokenShouldDefaultZero"
      Tests.AltCoverRunnerTests.EmptyJsonGeneratesExpectedSummary,
      "Runner.EmptyJsonGeneratesExpectedSummary"
      Tests.AltCoverRunnerTests.EmptyJsonGeneratesExpectedTCSummary,
      "Runner.EmptyJsonGeneratesExpectedTCSummary"
      Tests.AltCoverRunnerTests.EmptyJsonGeneratesExpectedSummaries,
      "Runner.EmptyJsonGeneratesExpectedSummaries"
      Tests.AltCoverRunnerTests.SimpleJsonShouldGeneratePlausibleSummary,
      "Runner.SimpleJsonShouldGeneratePlausibleSummary"
      Tests.AltCoverRunnerTests.ComplexJsonShouldGeneratePlausibleSummary,
      "Runner.ComplexJsonShouldGeneratePlausibleSummary"
      Tests.AltCoverRunnerTests.EmptyNCoverGeneratesExpectedSummary,
      "Runner.EmptyNCoverGeneratesExpectedSummary"
      Tests.AltCoverRunnerTests.EmptyNCoverGeneratesExpectedTCSummary,
      "Runner.EmptyNCoverGeneratesExpectedTCSummary"
      Tests.AltCoverRunnerTests.EmptyNCoverGeneratesExpectedSummaries,
      "Runner.EmptyNCoverGeneratesExpectedSummaries"
      Tests.AltCoverRunnerTests.NCoverShouldGeneratePlausibleSummary,
      "Runner.NCoverShouldGeneratePlausibleSummary"
      Tests.AltCoverRunnerTests.EmptyOpenCoverGeneratesExpectedSummary,
      "Runner.EmptyOpenCoverGeneratesExpectedSummary"
      Tests.AltCoverRunnerTests.EmptyOpenCoverGeneratesExpectedTCSummary,
      "Runner.EmptyOpenCoverGeneratesExpectedTCSummary"
      Tests.AltCoverRunnerTests.EmptyOpenCoverGeneratesExpectedSummaries,
      "Runner.EmptyOpenCoverGeneratesExpectedSummaries"
      Tests.AltCoverRunnerTests.OpenCoverShouldGeneratePlausibleSummary,
      "Runner.OpenCoverShouldGeneratePlausibleSummary"
      Tests.AltCoverRunnerTests.OpenCoverShouldGeneratePlausiblePartialSummary,
      "Runner.OpenCoverShouldGeneratePlausiblePartialSummary"
      Tests.AltCoverRunnerTests.DegenerateCasesShouldNotGenerateLcov,
      "Runner.DegenerateCasesShouldNotGenerateLcov"
      Tests.AltCoverRunnerTests.OpenCoverShouldGeneratePlausibleLcov,
      "Runner.OpenCoverShouldGeneratePlausibleLcov"
      Tests.AltCoverRunnerTests.JsonShouldGeneratePlausibleLcov,
      "Runner.JsonShouldGeneratePlausibleLcov"
      Tests.AltCoverRunnerTests.NCoverShouldGeneratePlausibleLcov,
      "Runner.NCoverShouldGeneratePlausibleLcov"
      Tests.AltCoverRunnerTests.NCoverShouldGenerateMorePlausibleLcov,
      "Runner.NCoverShouldGenerateMorePlausibleLcov"
      Tests.AltCoverRunnerTests.NCoverShouldGeneratePlausibleLcovWithMissingFullName,
      "Runner.NCoverShouldGeneratePlausibleLcovWithMissingFullName"
      Tests.AltCoverRunnerTests.MultiSortDoesItsThing, "Runner.MultiSortDoesItsThing"
      Tests.AltCoverRunnerTests.JsonShouldGeneratePlausibleXml,
      "Runner.JsonShouldGeneratePlausibleXml"
      Tests.AltCoverRunnerTests.JsonShouldGeneratePlausibleCobertura,
      "Runner.JsonShouldGeneratePlausibleCobertura"
      Tests.AltCoverRunnerTests.JsonFromComplexNestingShouldGeneratePlausibleCobertura,
      "Runner.JsonFromComplexNestingShouldGeneratePlausibleCobertura"
      Tests.AltCoverRunnerTests.NCoverShouldGeneratePlausibleCobertura,
      "Runner.NCoverShouldGeneratePlausibleCobertura"
      Tests.AltCoverRunnerTests.NCoverShouldGenerateMorePlausibleCobertura,
      "Runner.NCoverShouldGenerateMorePlausibleCobertura"
      Tests.AltCoverRunnerTests.DegenerateCasesShouldNotGenerateCobertura,
      "Runner.DegenerateCasesShouldNotGenerateCobertura"
      Tests.AltCoverRunnerTests.NCoverShouldGeneratePlausibleCoberturaWithMissingFullName,
      "Runner.NCoverShouldGeneratePlausibleCoberturaWithMissingFullName"
      Tests.AltCoverRunnerTests.OpenCoverShouldGeneratePlausibleCobertura,
      "Runner.OpenCoverShouldGeneratePlausibleCobertura"
      Tests.AltCoverRunnerTests.ThresholdViolationShouldBeReported,
      "Runner.ThresholdViolationShouldBeReported"
      Tests.AltCoverRunnerTests.TryGetValueHandlesNull, "Runner.TryGetValueHandlesNull"
      Tests.AltCoverTests.ReportFileShouldBeCorrectlySuffixed, "Tests.ReportFileShouldBeCorrectlySuffixed"
      Tests.AltCoverTests.CanSwitchSampling, "Tests.CanSwitchSampling"
      Tests.AltCoverTests.ShouldGetPdbFromImage, "Tests.ShouldGetPdbFromImage"
      Tests.AltCoverTests.ShouldGetEmbeddedPdbFromImage,
      "Tests.ShouldGetEmbeddedPdbFromImage"
      Tests.AltCoverTests.ShouldGetNoMdbFromMonoImage, "Tests.ShouldGetNoMdbFromMonoImage"
      Tests.AltCoverTests.ShouldGetPdbWithFallback, "Tests.ShouldGetPdbWithFallback"
      Tests.AltCoverTests.ShouldGetForeignPdbWithFallback,
      "Tests.ShouldGetForeignPdbWithFallback"
      Tests.AltCoverTests.ShouldGetForeignPdbWithFallbackWhenNotColocated,
      "Tests.ShouldGetForeignPdbWithFallbackWhenNotColocated"
      Tests.AltCoverTests.ShouldGetMdbWithFallback, "Tests.ShouldGetMdbWithFallback"
      Tests.AltCoverTests.ShouldGetSymbolsFromPdb, "Tests.ShouldGetSymbolsFromPdb"
      Tests.AltCoverTests.ShouldGetSymbolsFromEmbeddedPdb,
      "Tests.ShouldGetSymbolsFromEmbeddedPdb"
      Tests.AltCoverTests.ShouldNotGetSymbolsWhenNoPdb,
      "Tests.ShouldNotGetSymbolsWhenNoPdb"
      Tests.AltCoverTests.ShouldGetSymbolsFromMdb, "Tests.ShouldGetSymbolsFromMdb"
      Tests.AltCoverTests.NoneOfTheAboveMatchesNoType, "Tests.NoneOfTheAboveMatchesNoType"
      Tests.AltCoverTests.NoneOfTheAboveMatchesNoAttribute,
      "Tests.NoneOfTheAboveMatchesNoAttribute"
      Tests.AltCoverTests.NoneOfTheAboveMatchesNoAssembly,
      "Tests.NoneOfTheAboveMatchesNoAssembly"
      Tests.AltCoverTests.NoneOfTheAboveMatchesNoModule,
      "Tests.NoneOfTheAboveMatchesNoModule"
      Tests.AltCoverTests.NoneOfTheAboveMatchesNoFile, "Tests.NoneOfTheAboveMatchesNoFile"
      Tests.AltCoverTests.NoneOfTheAboveMatchesNoPath, "Tests.NoneOfTheAboveMatchesNoPath"
      Tests.AltCoverTests.NoneOfTheAboveMatchesNoMethod,
      "Tests.NoneOfTheAboveMatchesNoMethod"
      Tests.AltCoverTests.FileDoesNotMatchNonFileClass,
      "Tests.FileDoesNotMatchNonFileClass"
      Tests.AltCoverTests.FileDoesMatchFileClass, "Tests.FileDoesMatchFileClass"
      Tests.AltCoverTests.PathDoesNotMatchNonPathClass,
      "Tests.PathDoesNotMatchNonPathClass"
      Tests.AltCoverTests.PathDoesMatchPathClass, "Tests.PathDoesMatchPathClass"
      Tests.AltCoverTests.AssemblyDoesNotMatchNonAssemblyClass,
      "Tests.AssemblyDoesNotMatchNonAssemblyClass"
      Tests.AltCoverTests.AssemblyDoesMatchAssemblyClass,
      "Tests.AssemblyDoesMatchAssemblyClass"
      Tests.AltCoverTests.ModuleDoesNotMatchNonModuleClass,
      "Tests.ModuleDoesNotMatchNonModuleClass"
      Tests.AltCoverTests.ModuleDoesMatchModuleClass, "Tests.ModuleDoesMatchModuleClass"
      Tests.AltCoverTests.TypeDoesNotMatchNonTypeClass,
      "Tests.TypeDoesNotMatchNonTypeClass"
      Tests.AltCoverTests.TypeDoesMatchTypeClass, "Tests.TypeDoesMatchTypeClass"
      Tests.AltCoverTests.MethodDoesNotMatchNonMethodClass,
      "Tests.MethodDoesNotMatchNonMethodClass"
      Tests.AltCoverTests.MethodDoesMatchMethodClass, "Tests.MethodDoesMatchMethodClass"
      Tests.AltCoverTests.AttributeDoesNotMatchNonAttributeClass,
      "Tests.AttributeDoesNotMatchNonAttributeClass"
      Tests.AltCoverTests.AttributeDoesMatchAttributeClass,
      "Tests.AttributeDoesMatchAttributeClass"
      Tests.AltCoverTests.CanExcludeCSharpPropertiesByAttribute,
      "Tests.CanExcludeCSharpPropertiesByAttribute"
      Tests.AltCoverTests.Sample3Class1IsCSharpAutoproperty,
      "Tests.Sample3Class1IsCSharpAutoproperty"
      Tests.AltCoverTests.Sample3Class2IsNotCSharpAutoproperty,
      "Tests.Sample3Class2IsNotCSharpAutoproperty"
      Tests.AltCoverTests.CanIdentifyExcludedFSharpMethods,
      "Tests.CanIdentifyExcludedFSharpMethods"
      Tests.AltCoverTests.CanIdentifyExcludedCSharpAutoProperties,
      "Tests.CanIdentifyExcludedCSharpAutoProperties"
      Tests.AltCoverTests.CanIdentifyIncludedCSharpProperties,
      "Tests.CanIdentifyIncludedCSharpProperties"
      Tests.AltCoverTests.ValidateStaticExemption, "Tests.ValidateStaticExemption"
      Tests.AltCoverTests.ValidateStaticClass, "Tests.ValidateStaticClass"
      Tests.AltCoverTests.ValidateAutomaticExemption, "Tests.ValidateAutomaticExemption"
      Tests.AltCoverTests.DetectLocalSource, "Tests.DetectLocalSource"
      Tests.AltCoverTests.LocateMatchShouldChooseLongerWildCardPath,
      "Tests.LocateMatchShouldChooseLongerWildCardPath"
      Tests.AltCoverTests.LocateMatchFallsBackOK, "Tests.LocateMatchFallsBackOK"
      Tests.AltCoverTests.AsyncTestInContext, "Tests.AsyncTestInContext"
      Tests.AltCoverTests.AnotherAsyncTestInContext, "Tests.AnotherAsyncTestInContext"
      Tests.AltCoverTests.DebugBuildTernaryTestInContext,
      "Tests.DebugBuildTernaryTestInContext"
      Tests.AltCoverTests.ReleaseBuildTernaryTest, "Tests.ReleaseBuildTernaryTest"
      Tests.AltCoverTests.ReleaseBuildTernaryTestInContext,
      "Tests.ReleaseBuildTernaryTestInContext"
      Tests.AltCoverTests.ReleaseBuildTernaryTestInContextWithCoalescence,
      "Tests.ReleaseBuildTernaryTestInContextWithCoalescence"
      Tests.AltCoverTests.CSharpNestedMethods, "Tests.CSharpNestedMethods"
      Tests.AltCoverTests.FSharpNestedMethodsClassic, "Tests.FSharpNestedMethodsClassic"
      Tests.AltCoverTests.FSharpNestedMethods5x0x201, "Tests.FSharpNestedMethods_5_0_201"
      Tests.AltCoverTests.ValidateSeqPntFixUp, "Tests.ValidateSeqPntFixUp" // HACK HACK HACK
      Tests.AltCoverTests.EmptyArrayHasExpectedHash, "Tests.EmptyArrayHasExpectedHash"
      Tests.AltCoverTests.KeyHasExpectedToken, "Tests.KeyHasExpectedToken"
      Tests.AltCoverTests.TokenGeneratesExpectedULong, "Tests.TokenGeneratesExpectedULong"
      Tests.AltCoverTests.KeyHasExpectedIndex, "Tests.KeyHasExpectedIndex"
      Tests.AltCoverTests.EmptyArrayHasExpectedIndex, "Tests.EmptyArrayHasExpectedIndex"
      Tests.AltCoverTests.KeyHasExpectedRecord, "Tests.KeyHasExpectedRecord"
      Tests.AltCoverTests.KeyHasExpectedPlaceInIndex, "Tests.KeyHasExpectedPlaceInIndex"
      Tests.AltCoverTests.EmptyFiltersPassAll, "Tests.EmptyFiltersPassAll"
      Tests.AltCoverTests.NonEmptyFiltersCatchAnExpectedValue,
      "Tests.NonEmptyFiltersCatchAnExpectedValue"
      Tests.AltCoverTests.NonEmptyFiltersPassAnExpectedValue,
      "Tests.NonEmptyFiltersPassAnExpectedValue"
      Tests.AltCoverTests.AfterProcessingYieldsAnExpectedValue,
      "Tests.AfterProcessingYieldsAnExpectedValue"
      Tests.AltCoverTests.Sample3Class1PropertyIsNotSignificant,
      "Tests.Sample3Class1PropertyIsNotSignificant"
      Tests.AltCoverTests.Sample3Class2IPropertyIsSignificant,
      "Tests.Sample3Class2IPropertyIsSignificant"
      Tests.AltCoverTests.TerminalCasesGoNoDeeper, "Tests.TerminalCasesGoNoDeeper"
      Tests.AltCoverTests.MethodPointsAreDeeperThanMethods,
      "Tests.MethodPointsAreDeeperThanMethods"
      Tests.AltCoverTests.BranchPointsAreComputedForSwitch,
      "Tests.BranchPointsAreComputedForSwitch"
      Tests.AltCoverTests.BranchPointsAreComputedForMatch,
      "Tests.BranchPointsAreComputedForMatch"
      Tests.AltCoverTests.MethodsAreDeeperThanTypes, "Tests.MethodsAreDeeperThanTypes"
      Tests.AltCoverTests.TypesAreDeeperThanModules, "Tests.TypesAreDeeperThanModules"
      Tests.AltCoverTests.ModulesAreDeeperThanAssemblies,
      "Tests.ModulesAreDeeperThanAssemblies"
      Tests.AltCoverTests.AssembliesAreDeeperThanPaths,
      "Tests.AssembliesAreDeeperThanPaths"
      Tests.AltCoverTests.FilteredAssembliesDoNotHaveSequencePoints,
      "Tests.FilteredAssembliesDoNotHaveSequencePoints"
      Tests.AltCoverTests.TestFixPointInvoke, "Tests.TestFixPointInvoke"
      Tests.AltCoverTests.TestFixPointApply, "Tests.TestFixPointApply"
      Tests.AltCoverTests.PathsAreDeeperThanAVisit, "Tests.PathsAreDeeperThanAVisit"
      Tests.AltCoverTests.TrackingDetectsTests, "Tests.TrackingDetectsTests"
      Tests.AltCoverTests.TrackingDetectsExpectedTests,
      "Tests.TrackingDetectsExpectedTests"
      Tests.AltCoverTests.TrackingDetectsTestsByFullType,
      "Tests.TrackingDetectsTestsByFullType"
      Tests.AltCoverTests.TrackingDetectsMethods, "Tests.TrackingDetectsMethods"
      Tests.AltCoverTests.NamingDetectEmpties, "Tests.NamingDetectEmpties"
      Tests.AltCoverTests.NamingSuffixDetectEmpties, "Tests.NamingSuffixDetectEmpties"
      Tests.AltCoverTests.TypeNamesAreExtracted, "Tests.TypeNamesAreExtracted"
      Tests.AltCoverTests.FullTypeNamesAreExtracted, "Tests.FullTypeNamesAreExtracted"
      Tests.AltCoverTests.TypeRefNamesAreExtracted, "Tests.TypeRefNamesAreExtracted"
      Tests.AltCoverTests.FullTypeRefNamesAreExtracted,
      "Tests.FullTypeRefNamesAreExtracted"
      Tests.AltCoverTests.MethodNamesAreExtracted, "Tests.MethodNamesAreExtracted"
      Tests.AltCoverTests.FullMethodNamesAreExtracted, "Tests.FullMethodNamesAreExtracted"
      Tests.AltCoverTests.ShouldGenerateExpectedXmlReportFromDotNet,
      "Tests.ShouldGenerateExpectedXmlReportFromDotNet"
      Tests.AltCoverTests.ShouldGenerateExpectedJsonReportFromDotNet,
      "Tests.ShouldGenerateExpectedJsonReportFromDotNet"
      Tests.AltCoverTests.ShouldGenerateExpectedXmlReportForNCoverWithMethodPointOnly,
      "Tests.ShouldGenerateExpectedXmlReportForNCoverWithMethodPointOnly"
      Tests.AltCoverTests.ShouldGenerateExpectedXmlReportForNCoverWithTopLevel,
      "Tests.ShouldGenerateExpectedXmlReportForNCoverWithTopLevel"
      Tests.AltCoverTests.ShouldGenerateExpectedXmlReportForOpenCoverWithMethodPointOnly,
      "Tests.ShouldGenerateExpectedXmlReportForOpenCoverWithMethodPointOnly"
      Tests.AltCoverTests.ShouldGenerateExpectedXmlReportWithSourceLink,
      "Tests.ShouldGenerateExpectedXmlReportWithSourceLink"
      Tests.AltCoverTests.ShouldGenerateExpectedXmlReportFromDotNetWithPathFilter,
      "Tests.ShouldGenerateExpectedXmlReportFromDotNetWithPathFilter"
      Tests.AltCoverTests.ShouldGenerateExpectedXmlReportFromDotNetWhenExcluded,
      "Tests.ShouldGenerateExpectedXmlReportFromDotNetWhenExcluded"
      Tests.AltCoverTests.ShouldGenerateExpectedXmlReportFromDotNetWhenExcludedEvenIfTracked,
      "Tests.ShouldGenerateExpectedXmlReportFromDotNetWhenExcludedEvenIfTracked"
      Tests.AltCoverTests.ShouldDetectTernary, "Tests.ShouldDetectTernary"
      Tests.AltCoverTests.ShouldDetectSwitchNesting, "Tests.ShouldDetectSwitchNesting"
      Tests.AltCoverTests.SafeMultiplyIsSafe, "Tests.SafeMultiplyIsSafe"
      Tests.AltCoverTests.EmptyMethodHasComplexity1, "Tests.EmptyMethodHasComplexity1"
      Tests.AltCoverTests.BranchChainsSerialize, "Tests.BranchChainsSerialize"
      Tests.AltCoverTests.BranchChainsTerminate, "Tests.BranchChainsTerminate"
      Tests.AltCoverTests.ShouldGenerateExpectedXmlReportWithSourceLinkOpenCoverStyle,
      "Tests.ShouldGenerateExpectedXmlReportWithSourceLinkOpenCoverStyle"
      Tests.AltCoverTests.ShouldGenerateExpectedXmlReportFromDotNetOpenCoverStyle,
      "Tests.ShouldGenerateExpectedXmlReportFromDotNetOpenCoverStyle"
      Tests.AltCoverTests.ShouldGenerateExpectedXmlReportFromDotNetLineCoverStyle,
      "Tests.ShouldGenerateExpectedXmlReportFromDotNetLineCoverStyle"
      Tests.AltCoverTests.ShouldGenerateExpectedXmlReportFromDotNetBranchCoverStyle,
      "Tests.ShouldGenerateExpectedXmlReportFromDotNetBranchCoverStyle"
      Tests.AltCoverTests.ShouldGenerateExpectedXmlReportFromDotNetOpenCoverStyleWithTracking,
      "Tests.ShouldGenerateExpectedXmlReportFromDotNetOpenCoverStyleWithTracking"
      Tests.AltCoverTests.ShouldGenerateExpectedXmlReportWithModuleExclusionOpenCoverStyle,
      "Tests.ShouldGenerateExpectedXmlReportWithModuleExclusionOpenCoverStyle"
      Tests.AltCoverTests.ShouldGenerateExpectedTrackingXmlReportWithModuleExclusionOpenCoverStyle,
      "Tests.ShouldGenerateExpectedTrackingXmlReportWithModuleExclusionOpenCoverStyle"
      Tests.AltCoverTests.ShouldGenerateExpectedXmlReportWithClassExclusionOpenCoverStyle,
      "Tests.ShouldGenerateExpectedXmlReportWithClassExclusionOpenCoverStyle"
      Tests.AltCoverTests.ShouldGenerateExpectedTrackingXmlReportWithClassExclusionOpenCoverStyle,
      "Tests.ShouldGenerateExpectedTrackingXmlReportWithClassExclusionOpenCoverStyle"
      Tests.AltCoverTests.ShouldGenerateExpectedXmlReportWithMethodExclusionOpenCoverStyle,
      "Tests.ShouldGenerateExpectedXmlReportWithMethodExclusionOpenCoverStyle"
      Tests.AltCoverTests.ShouldGenerateExpectedXmlReportWithFileExclusionOpenCoverStyle,
      "Tests.ShouldGenerateExpectedXmlReportWithFileExclusionOpenCoverStyle"
      Tests.AltCoverTests.ShouldGenerateExpectedTrackingXmlReportWithMethodExclusionOpenCoverStyle,
      "Tests.ShouldGenerateExpectedTrackingXmlReportWithMethodExclusionOpenCoverStyle"
      Tests.AltCoverTests.ShouldGenerateExpectedXmlReportWithTraditionalInterfacesOpenCoverStyle,
      "Tests.ShouldGenerateExpectedXmlReportWithTraditionalInterfacesOpenCoverStyle"
      Tests.AltCoverTests.ShouldGenerateExpectedXmlReportWithModernInterfacesOpenCoverStyle,
      "Tests.ShouldGenerateExpectedXmlReportWithModernInterfacesOpenCoverStyle"
      Tests.AltCoverTests.ShouldSortFileIds, "Tests.ShouldSortFileIds"
      Tests.AltCoverTests.ZeroIsNotVisited, "Tests.ZeroIsNotVisited"
      Tests.AltCoverTests.PositiveIsVisited, "Tests.PositiveIsVisited"
      Tests.AltCoverTests.NegativesSpray, "Tests.NegativesSpray"
      Tests.AltCoverTests2.ShouldBeAbleToGetTheVisitReportMethod,
      "Tests2.ShouldBeAbleToGetTheVisitReportMethod"
      Tests.AltCoverTests2.ShouldBeAbleToClearTheStrongNameKey,
      "Tests2.ShouldBeAbleToClearTheStrongNameKey"
      Tests.AltCoverTests2.ShouldBeAbleToUpdateTheStrongNameKeyWherePossible,
      "Tests2.ShouldBeAbleToUpdateTheStrongNameKeyWherePossible"
      Tests.AltCoverTests2.NoKnownKeyInEmptyIndex, "Tests2.NoKnownKeyInEmptyIndex"
      Tests.AltCoverTests2.KnownKeyMatchedInIndex, "Tests2.KnownKeyMatchedInIndex"
      Tests.AltCoverTests2.ThirdPartyKeyNotMatchedInIndex,
      "Tests2.ThirdPartyKeyNotMatchedInIndex"
      Tests.AltCoverTests2.FakedUpKeyIsMatchedInIndex, "Tests2.FakedUpKeyIsMatchedInIndex"
      Tests.AltCoverTests2.NoKnownKeyIfAssemblyHasNone,
      "Tests2.NoKnownKeyIfAssemblyHasNone"
      Tests.AltCoverTests2.NoKnownTokenInEmptyIndex, "Tests2.NoKnownTokenInEmptyIndex"
      Tests.AltCoverTests2.KnownTokenMatchedInIndex, "Tests2.KnownTokenMatchedInIndex"
      Tests.AltCoverTests2.NoKnownTokenIfAssemblyHasNone,
      "Tests2.NoKnownTokenIfAssemblyHasNone"
      Tests.AltCoverTests2.ForeignTokenIsNotMatchedInIndex,
      "Tests2.ForeignTokenIsNotMatchedInIndex"
      Tests.AltCoverTests2.FakedUpTokenIsMatchedInIndex,
      "Tests2.FakedUpTokenIsMatchedInIndex"
      Tests.AltCoverTests2.MonoCombinationCanBeExercisedOnNetCore,
      "Tests2.MonoCombinationCanBeExercisedOnNetCore"
      Tests.AltCoverTests2.GuardShouldDisposeRecordingAssemblyOnException,
      "Tests2.GuardShouldDisposeRecordingAssemblyOnException"
      Tests.AltCoverTests2.ShouldBeAbleToTellAnAssembly,
      "Tests2.ShouldBeAbleToTellAnAssembly"
      Tests.AltCoverTests2.ShouldBeAbleToValidateAnAssembly,
      "Tests2.ShouldBeAbleToValidateAnAssembly"
      Tests.AltCoverTests2.ShouldBeAbleToLocateAReference,
      "Tests2.ShouldBeAbleToLocateAReference"
      Tests.AltCoverTests2.ShouldBeAbleToPrepareTheAssembly,
      "Tests2.ShouldBeAbleToPrepareTheAssembly"
      Tests.AltCoverTests2.ShouldGetTrackingStyleIfSet,
      "Tests2.ShouldGetTrackingStyleIfSet"
      Tests.AltCoverTests2.ShouldSymbolWriterAsExpected,
      "Tests2.ShouldSymbolWriterAsExpected"
      Tests.AltCoverTests2.ShouldGetNewFilePathFromPreparedAssembly,
      "Tests2.ShouldGetNewFilePathFromPreparedAssembly"
      Tests.AltCoverTests2.ShouldWriteMonoAssemblyOK, "Tests2.ShouldWriteMonoAssemblyOK"
      Tests.AltCoverTests2.ShouldGetVisitFromWrittenAssembly,
      "Tests2.ShouldGetVisitFromWrittenAssembly"
      //Tests.AltCoverTests2.ShouldUpdateHandlerOK([<Range(0, 31)>] selection)
      Tests.AltCoverTests2.ShouldSubstituteInstructionOperand,
      "Tests2.ShouldSubstituteInstructionOperand"
      Tests.AltCoverTests2.ShouldNotSubstituteDifferentInstructionOperand,
      "Tests2.ShouldNotSubstituteDifferentInstructionOperand"
      Tests.AltCoverTests2.ShouldSubstituteIntoInstructionOperandArray,
      "Tests2.ShouldSubstituteIntoInstructionOperandArray"
      Tests.AltCoverTests2.ShouldNotSubstituteOutsideInstructionOperandArray,
      "Tests2.ShouldNotSubstituteOutsideInstructionOperandArray"
      Tests.AltCoverTests2.ShouldNotSubstituteOtherOperand,
      "Tests2.ShouldNotSubstituteOtherOperand"
      Tests.AltCoverTests2.ShouldBeAbleToTrackAMethod, "Tests2.ShouldBeAbleToTrackAMethod"
      Tests.AltCoverTests2.ShouldBeAbleToTrackAMethodWithTailCalls,
      "Tests2.ShouldBeAbleToTrackAMethodWithTailCalls"
      Tests.AltCoverTests2.ShouldBeAbleToTrackAMethodWithNonVoidReturn,
      "Tests2.ShouldBeAbleToTrackAMethodWithNonVoidReturn"
      Tests.AltCoverTests2.ShouldBeAbleToTrackAnAsyncMethod,
      "Tests2.ShouldBeAbleToTrackAnAsyncMethod"
      Tests.AltCoverTests2.ShouldBeAbleToTrackAnFSAsyncMethod,
      "Tests2.ShouldBeAbleToTrackAnFSAsyncMethod"
      Tests.AltCoverTests2.ShouldBeAbleToInstrumentASwitchForNCover,
      "Tests2.ShouldBeAbleToInstrumentASwitchForNCover"
      Tests.AltCoverTests2.ShouldNotChangeAnUntrackedMethod,
      "Tests2.ShouldNotChangeAnUntrackedMethod"
      Tests.AltCoverTests2.SwitchBranchesShouldInstrumentByPushingDown,
      "Tests2.SwitchBranchesShouldInstrumentByPushingDown"
      Tests.AltCoverTests2.PseudoSwitchVisibleBranchesShouldSkipNonRepresentativeCases,
      "Tests2.PseudoSwitchVisibleBranchesShouldSkipNonRepresentativeCases"
      Tests.AltCoverTests2.SimpleBranchShouldInstrumentByPushingDown,
      "Tests2.SimpleBranchShouldInstrumentByPushingDown"
      Tests.AltCoverTests2.StartShouldLoadRecordingAssembly,
      "Tests2.StartShouldLoadRecordingAssembly"
      Tests.AltCoverTests2.TypeShouldNotChangeState, "Tests2.TypeShouldNotChangeState"
      Tests.AltCoverTests2.ExcludedMethodShouldNotChangeState,
      "Tests2.ExcludedMethodShouldNotChangeState"
      Tests.AltCoverTests2.IncludedMethodShouldChangeState,
      "Tests2.IncludedMethodShouldChangeState"
      Tests.AltCoverTests2.ExcludedAfterMethodShouldNotChangeState,
      "Tests2.ExcludedAfterMethodShouldNotChangeState"
      Tests.AltCoverTests2.IncludedAfterMethodShouldRewriteMethod,
      "Tests2.IncludedAfterMethodShouldRewriteMethod"
      Tests.AltCoverTests2.UpdateStrongReferencesShouldChangeSigningKeyWherePossible,
      "Tests2.UpdateStrongReferencesShouldChangeSigningKeyWherePossible"
      Tests.AltCoverTests2.UpdateStrongReferencesShouldChangeSigningKeyWherePossible2,
      "Tests2.UpdateStrongReferencesShouldChangeSigningKeyWherePossible2"
      Tests.AltCoverTests2.UpdateStrongReferencesShouldRemoveSigningKeyIfRequired,
      "Tests2.UpdateStrongReferencesShouldRemoveSigningKeyIfRequired"
      Tests.AltCoverTests2.UpdateStrongReferencesShouldNotAddASigningKey,
      "Tests2.UpdateStrongReferencesShouldNotAddASigningKey"
      Tests.AltCoverTests2.UpdateStrongReferencesShouldTrackReferences,
      "Tests2.UpdateStrongReferencesShouldTrackReferences"
      Tests.AltCoverTests2.UpdateStrongReferencesShouldTrackReferencesEvenFakes,
      "Tests2.UpdateStrongReferencesShouldTrackReferencesEvenFakes"
      Tests.AltCoverTests2.ExcludedAssemblyRefsAreNotUpdated,
      "Tests2.ExcludedAssemblyRefsAreNotUpdated"
      Tests.AltCoverTests2.IncludedAssemblyRefsAreUpdated,
      "Tests2.IncludedAssemblyRefsAreUpdated"
      Tests.AltCoverTests2.ExcludedModuleJustRecordsMVid,
      "Tests2.ExcludedModuleJustRecordsMVid"
      Tests.AltCoverTests2.ExcludedModuleJustRecordsNameForJson,
      "Tests2.ExcludedModuleJustRecordsNameForJson"
      Tests.AltCoverTests2.ExcludedModuleJustRecordsHashForOpenCover,
      "Tests2.ExcludedModuleJustRecordsHashForOpenCover"
      Tests.AltCoverTests2.IncludedModuleEnsuresRecorder,
      "Tests2.IncludedModuleEnsuresRecorder"
      Tests.AltCoverTests2.ExcludedMethodPointIsPassThrough,
      "Tests2.ExcludedMethodPointIsPassThrough"
      Tests.AltCoverTests2.IncludedMethodPointInsertsVisit,
      "Tests2.IncludedMethodPointInsertsVisit"
      Tests.AltCoverTests2.IncludedModuleDoesNotChangeRecorderJustTheReference,
      "Tests2.IncludedModuleDoesNotChangeRecorderJustTheReference"
      Tests.AltCoverTests2.AfterModuleShouldNotChangeState,
      "Tests2.AfterModuleShouldNotChangeState"
      Tests.AltCoverTests2.JSONInjectionTransformsSimpleFileAsExpected,
      "Tests2.JSONInjectionTransformsSimpleFileAsExpected"
      Tests.AltCoverTests2.JSONInjectionTransformsStandaloneFileAsExpected,
      "Tests2.JSONInjectionTransformsStandaloneFileAsExpected"
      Tests.AltCoverTests2.JSONInjectionTransformsDependencyFileAsExpected,
      "Tests2.JSONInjectionTransformsDependencyFileAsExpected"
      Tests.AltCoverTests2.JSONInjectionIsIdempotent, "Tests2.JSONInjectionIsIdempotent"
      Tests.AltCoverTests2.NonFinishShouldDisposeRecordingAssembly,
      "Tests2.NonFinishShouldDisposeRecordingAssembly"
      Tests.AltCoverTests2.NonFinishShouldDisposeThreadingAssembly,
      "Tests2.NonFinishShouldDisposeThreadingAssembly"
      Tests.AltCoverTests2.NonFinishShouldNotDisposeNullRecordingAssembly,
      "Tests2.NonFinishShouldNotDisposeNullRecordingAssembly"
      Tests.AltCoverTests2.FinishShouldLeaveRecordingAssembly,
      "Tests2.FinishShouldLeaveRecordingAssembly"
      Tests.AltCoverTests2.StrongNameKeyCanBeValidated,
      "Tests2.StrongNameKeyCanBeValidated"
      Tests.AltCoverTests2.VerbosityShouldBeHonoured, "Tests2.VerbosityShouldBeHonoured"
      Tests.AltCoverTests2.CryptographicExceptionIsTransformed,
      "Tests2.CryptographicExceptionIsTransformed"
      Tests.AltCoverTests2.OutputCanBeExercised, "Tests2.OutputCanBeExercised"
      Tests.AltCoverTests2.NoThrowNoErrorLeavesAllOK, "Tests2.NoThrowNoErrorLeavesAllOK"
      Tests.AltCoverTests2.NoThrowWithErrorIsSignalled,
      "Tests2.NoThrowWithErrorIsSignalled"
      Tests.AltCoverTests2.ArgumentExceptionWrites, "Tests2.ArgumentExceptionWrites"
      Tests.AltCoverTests2.ArgumentExceptionWritesEx, "Tests2.ArgumentExceptionWritesEx"
      Tests.AltCoverTests2.IOExceptionWrites, "Tests2.IOExceptionWrites"
      Tests.AltCoverTests2.NotSupportedExceptionWrites,
      "Tests2.NotSupportedExceptionWrites"
      Tests.AltCoverTests2.SecurityExceptionWrites, "Tests2.SecurityExceptionWrites"
      Tests.AltCoverTests2.UnauthorizedAccessExceptionWrites,
      "Tests2.UnauthorizedAccessExceptionWrites"
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
      Tests.AltCoverTests3.ParsingMethodsGivesMethods, "Tests3.ParsingMethodsGivesMethods"
      Tests.AltCoverTests3.ParsingTypesGivesTypes, "Tests3.ParsingTypesGivesTypes"
      Tests.AltCoverTests3.ParsingAssembliesGivesAssemblies,
      "Tests3.ParsingAssembliesGivesAssemblies"
      Tests.AltCoverTests3.ParsingEscapeCasesWork, "Tests3.ParsingEscapeCasesWork"
      Tests.AltCoverTests3.ParsingModulesGivesModules, "Tests3.ParsingModulesGivesModules"
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
      Tests.AltCoverTests3.ParsingNoInputGivesFailure, "Tests3.ParsingNoInputGivesFailure"
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
      Tests.AltCoverTests3.ParsingVisibleGivesVisible, "Tests3.ParsingVisibleGivesVisible"
      Tests.AltCoverTests3.ParsingMultipleVisibleGivesFailure,
      "Tests3.ParsingMultipleVisibleGivesFailure"
      Tests.AltCoverTests3.ParsingTimeGivesTime, "Tests3.ParsingTimeGivesTime"
      Tests.AltCoverTests3.ParsingOnlyArabicNumeralsNotThatSortofArabicNumeralsGivesTime,
      "Tests3.ParsingOnlyArabicNumeralsNotThatSortofArabicNumeralsGivesTime"
      Tests.AltCoverTests3.ParsingMultipleTimesGivesFailure,
      "Tests3.ParsingMultipleTimesGivesFailure"
      Tests.AltCoverTests3.ParsingTimeAndNamesGivesOK, "Tests3.ParsingTimeAndNamesGivesOK"
      Tests.AltCoverTests3.ParsingBadTimeGivesNoOp, "Tests3.ParsingBadTimeGivesNoOp"
      Tests.AltCoverTests3.ParsingNonTimeGivesFailure, "Tests3.ParsingNonTimeGivesFailure"
      Tests.AltCoverTests3.ParsingNoTimeGivesFailure, "Tests3.ParsingNoTimeGivesFailure"
      Tests.AltCoverTests3.ParsingJsonFormatGivesJson, "Tests3.ParsingJsonFormatGivesJson"
      Tests.AltCoverTests3.ParsingNCoverFormatGivesNCover,
      "Tests3.ParsingNCoverFormatGivesNCover"
      Tests.AltCoverTests3.ParsingOpenCoverFormatGivesOpenCover,
      "Tests3.ParsingOpenCoverFormatGivesOpenCover"
      Tests.AltCoverTests3.ParsingMultipleReportFormatGivesFailure,
      "Tests3.ParsingMultipleReportFormatGivesFailure"
      Tests.AltCoverTests3.ParsingInPlaceGivesInPlace, "Tests3.ParsingInPlaceGivesInPlace"
      Tests.AltCoverTests3.ParsingMultipleInPlaceGivesFailure,
      "Tests3.ParsingMultipleInPlaceGivesFailure"
      Tests.AltCoverTests3.ParsingSaveGivesSave, "Tests3.ParsingSaveGivesSave"
      Tests.AltCoverTests3.ParsingMultipleSaveGivesFailure,
      "Tests3.ParsingMultipleSaveGivesFailure"
      Tests.AltCoverTests3.ParsingSingleGivesSingle, "Tests3.ParsingSingleGivesSingle"
      Tests.AltCoverTests3.ParsingMultipleSingleGivesFailure,
      "Tests3.ParsingMultipleSingleGivesFailure"
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
      Tests.AltCoverTests3.ParsingDeferWorks, "Tests3.ParsingDeferWorks"
      Tests.AltCoverTests3.ParsingMultipleDeferGivesFailure,
      "Tests3.ParsingMultipleDeferGivesFailure"
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
      Tests.AltCoverTests3.OutputLeftPassesThrough, "Tests3.OutputLeftPassesThrough"
      Tests.AltCoverTests3.OutputInPlaceFails, "Tests3.OutputInPlaceFails"
      Tests.AltCoverTests3.OutputToNewPlaceIsOK, "Tests3.OutputToNewPlaceIsOK"
      Tests.AltCoverTests3.OutputToReallyNewPlaceIsOK, "Tests3.OutputToReallyNewPlaceIsOK"
      Tests.AltCoverTests3.InPlaceToExistingPlaceFails,
      "Tests3.InPlaceToExistingPlaceFails"
      Tests.AltCoverTests3.InPlaceOperationIsAsExpected,
      "Tests3.InPlaceOperationIsAsExpected"
      Tests.AltCoverTests3.ImageLoadResilientPassesThrough,
      "Tests3.ImageLoadResilientPassesThrough"
      Tests.AltCoverTests3.ResilientHandlesIOException,
      "Tests3.ResilientHandlesIOException"
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
      "XTests.ShouldGenerateExpectedXmlReportFromMonoOpenCoverStyle" |] |> Array.toList

#if !EXPECTO_MAIN
  let consistencyCheck specials =
    ExpectoTestCommon.consistencyCheck (simpleTests ()) specials //["Tests.AltCoverTests2::ShouldUpdateHandlerOK"]
#endif
#endif // remove for fantomas