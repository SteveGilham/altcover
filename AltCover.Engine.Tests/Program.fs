namespace Tests

#if !NET472

open Expecto

module ExpectoMain =
  let regular =
    [ Tests.TestCommonTests.TestMultiple, "Tests.TestCommonTests.TestMultiple"
      Tests.TestCommonTests.TestIgnoredTests, "TestCommonTests.TestIgnoredTests"
      Tests.TestCommonTests.ExerciseItAll, "TestCommonTests.ExerciseItAll"
      Tests.TestCommonTests.SelfTest, "TestCommonTests.SelfTest"
      // Augment.fs
      Tests.Augment.ZeroIsNotVisited, "Tests.ZeroIsNotVisited"
      Tests.Augment.PositiveIsVisited, "Tests.PositiveIsVisited"
      Tests.Augment.NegativesSpray, "Tests.NegativesSpray"
      // Base.fs
      Tests.BaseTests.ExerciseBoth, "BaseTests.ExerciseBoth"
      Tests.BaseTests.ExerciseTime, "BaseTests.ExerciseTime"
      Tests.BaseTests.ExerciseCall, "BaseTests.ExerciseCall"
      Tests.BaseTests.ExerciseNull, "BaseTests.ExerciseNull"
      Tests.BaseTests.ExercisePointVisit, "BaseTests.ExercisePointVisit"
      // ProgramDatabase.fs
      Tests.ProgramDatabase.ShouldTrapIndexOutOfRangeException,
      "Tests.ShouldTrapIndexOutOfRangeException"
      Tests.ProgramDatabase.ShouldGetPdbFromImage, "Tests.ShouldGetPdbFromImage"
      Tests.ProgramDatabase.ShouldGetEmbeddedPdbFromImage,
      "Tests.ShouldGetEmbeddedPdbFromImage"
      Tests.ProgramDatabase.ShouldGetNoMdbFromMonoImage,
      "Tests.ShouldGetNoMdbFromMonoImage"
      Tests.ProgramDatabase.ShouldGetGUIDfromNativePdb, "Tests.ShouldGetGUIDfromNativePdb"
      Tests.ProgramDatabase.ShouldGetPdbWithFallback, "Tests.ShouldGetPdbWithFallback"
      Tests.ProgramDatabase.ShouldGetForeignPdbWithFallback,
      "Tests.ShouldGetForeignPdbWithFallback"
      Tests.ProgramDatabase.ShouldGetForeignPdbWithFallbackWhenNotColocated,
      "Tests.ShouldGetForeignPdbWithFallbackWhenNotColocated"
      Tests.ProgramDatabase.ShouldGetMdbWithFallback, "Tests.ShouldGetMdbWithFallback"
      Tests.ProgramDatabase.ShouldGetSymbolsFromPdb, "Tests.ShouldGetSymbolsFromPdb"
      Tests.ProgramDatabase.ShouldGetSymbolsFromEmbeddedPdb,
      "Tests.ShouldGetSymbolsFromEmbeddedPdb"
      Tests.ProgramDatabase.ShouldNotGetSymbolsWhenNoPdb,
      "Tests.ShouldNotGetSymbolsWhenNoPdb"
      Tests.ProgramDatabase.ShouldGetSymbolsFromMdb, "Tests.ShouldGetSymbolsFromMdb"
      // Filter.fs
      Tests.ACFilter.NoneOfTheAboveMatchesNoType, "Tests.NoneOfTheAboveMatchesNoType"
      Tests.ACFilter.NoneOfTheAboveMatchesNoAttribute,
      "Tests.NoneOfTheAboveMatchesNoAttribute"
      Tests.ACFilter.NoneOfTheAboveMatchesNoAssembly,
      "Tests.NoneOfTheAboveMatchesNoAssembly"
      Tests.ACFilter.NoneOfTheAboveMatchesNoModule, "Tests.NoneOfTheAboveMatchesNoModule"
      Tests.ACFilter.NoneOfTheAboveMatchesNoFile, "Tests.NoneOfTheAboveMatchesNoFile"
      Tests.ACFilter.NoneOfTheAboveMatchesNoPath, "Tests.NoneOfTheAboveMatchesNoPath"
      Tests.ACFilter.NoneOfTheAboveMatchesNoMethod, "Tests.NoneOfTheAboveMatchesNoMethod"
      Tests.ACFilter.FileDoesNotMatchNonFileClass, "Tests.FileDoesNotMatchNonFileClass"
      Tests.ACFilter.FileDoesMatchFileClass, "Tests.FileDoesMatchFileClass"
      Tests.ACFilter.PathDoesNotMatchNonPathClass, "Tests.PathDoesNotMatchNonPathClass"
      Tests.ACFilter.PathDoesMatchPathClass, "Tests.PathDoesMatchPathClass"
      Tests.ACFilter.AssemblyDoesNotMatchNonAssemblyClass,
      "Tests.AssemblyDoesNotMatchNonAssemblyClass"
      Tests.ACFilter.AssemblyDoesMatchAssemblyClass,
      "Tests.AssemblyDoesMatchAssemblyClass"
      Tests.ACFilter.ModuleDoesNotMatchNonModuleClass,
      "Tests.ModuleDoesNotMatchNonModuleClass"
      Tests.ACFilter.ModuleDoesMatchModuleClass, "Tests.ModuleDoesMatchModuleClass"
      Tests.ACFilter.TypeDoesNotMatchNonTypeClass, "Tests.TypeDoesNotMatchNonTypeClass"
      Tests.ACFilter.TypeDoesMatchTypeClass, "Tests.TypeDoesMatchTypeClass"
      Tests.ACFilter.MethodDoesNotMatchNonMethodClass,
      "Tests.MethodDoesNotMatchNonMethodClass"
      Tests.ACFilter.MethodDoesMatchMethodClass, "Tests.MethodDoesMatchMethodClass"
      Tests.ACFilter.AttributeDoesNotMatchNonAttributeClass,
      "Tests.AttributeDoesNotMatchNonAttributeClass"
      Tests.ACFilter.AttributeDoesMatchAttributeClass,
      "Tests.AttributeDoesMatchAttributeClass"
      Tests.ACFilter.CanExcludeCSharpPropertiesByAttribute,
      "Tests.CanExcludeCSharpPropertiesByAttribute"
      Tests.ACFilter.RefStructsAreNotObsolete, "Tests.RefStructsAreNotObsolete"
      Tests.ACFilter.Sample3Class1IsCSharpAutoproperty,
      "Tests.Sample3Class1IsCSharpAutoproperty"
      Tests.ACFilter.Sample3Class2IsNotCSharpAutoproperty,
      "Tests.Sample3Class2IsNotCSharpAutoproperty"
      Tests.ACFilter.CanIdentifyExcludedFSharpMethods,
      "Tests.CanIdentifyExcludedFSharpMethods"
      Tests.ACFilter.CanIdentifyExcludedCSharpAutoProperties,
      "Tests.CanIdentifyExcludedCSharpAutoProperties"
      Tests.ACFilter.CanIdentifyIncludedCSharpProperties,
      "Tests.CanIdentifyIncludedCSharpProperties"
      // Visitor.fs
      Tests.Visitor.ReportFileShouldBeCorrectlySuffixed,
      "Tests.ReportFileShouldBeCorrectlySuffixed"
      Tests.Visitor.ReportFileShouldBeCorrectlyExtended,
      "Tests.ReportFileShouldBeCorrectlyExtended"
      Tests.Visitor.CanSwitchSampling, "Tests.CanSwitchSampling"
      Tests.Visitor.ValidateStaticExemption, "Tests.ValidateStaticExemption"
      Tests.Visitor.ValidateStaticClass, "Tests.ValidateStaticClass"
      Tests.Visitor.ValidateAutomaticExemption, "Tests.ValidateAutomaticExemption"
      Tests.Visitor.DetectLocalSource, "Tests.DetectLocalSource"
      Tests.Visitor.LocateMatchShouldChooseLongerWildCardPath,
      "Tests.LocateMatchShouldChooseLongerWildCardPath"
      Tests.Visitor.LocateMatchFallsBackOK, "Tests.LocateMatchFallsBackOK"
      Tests.Visitor.AsyncTestInContext, "Tests.AsyncTestInContext"
      Tests.Visitor.AnotherAsyncTestInContext, "Tests.AnotherAsyncTestInContext"
      Tests.Visitor.DebugBuildTernaryTestInContext, "Tests.DebugBuildTernaryTestInContext"
      Tests.Visitor.ReleaseBuildTernaryTest, "Tests.ReleaseBuildTernaryTest"
      Tests.Visitor.ReleaseBuildTernaryTestInContext,
      "Tests.ReleaseBuildTernaryTestInContext"
      Tests.Visitor.ReleaseBuildTernaryTestInContextWithCoalescence,
      "Tests.ReleaseBuildTernaryTestInContextWithCoalescence"
      Tests.Visitor.CSharpNestedMethods, "Tests.CSharpNestedMethods"
      Tests.Visitor.FSharpNestedMethodsClassic, "Tests.FSharpNestedMethodsClassic"
      Tests.Visitor.FSharpNestedMethods5x0x201, "Tests.FSharpNestedMethods_5_0_201"
      Tests.Visitor.ValidateSeqPntFixUp, "Tests.ValidateSeqPntFixUp" // HACK HACK HACK
      Tests.Visitor.EmptyArrayHasExpectedHash, "Tests.EmptyArrayHasExpectedHash"
      Tests.Visitor.KeyHasExpectedToken, "Tests.KeyHasExpectedToken"
      Tests.Visitor.TokenGeneratesExpectedULong, "Tests.TokenGeneratesExpectedULong"
      Tests.Visitor.KeyHasExpectedIndex, "Tests.KeyHasExpectedIndex"
      Tests.Visitor.EmptyArrayHasExpectedIndex, "Tests.EmptyArrayHasExpectedIndex"
      Tests.Visitor.KeyHasExpectedRecord, "Tests.KeyHasExpectedRecord"
      Tests.Visitor.KeyHasExpectedPlaceInIndex, "Tests.KeyHasExpectedPlaceInIndex"
      Tests.Visitor.EmptyFiltersPassAll, "Tests.EmptyFiltersPassAll"
      Tests.Visitor.NonEmptyFiltersCatchAnExpectedValue,
      "Tests.NonEmptyFiltersCatchAnExpectedValue"
      Tests.Visitor.NonEmptyFiltersPassAnExpectedValue,
      "Tests.NonEmptyFiltersPassAnExpectedValue"
      Tests.Visitor.AfterProcessingYieldsAnExpectedValue,
      "Tests.AfterProcessingYieldsAnExpectedValue"
      Tests.Visitor.Sample3Class1PropertyIsNotSignificant,
      "Tests.Sample3Class1PropertyIsNotSignificant"
      Tests.Visitor.Sample3Class2IPropertyIsSignificant,
      "Tests.Sample3Class2IPropertyIsSignificant"
      Tests.Visitor.TerminalCasesGoNoDeeper, "Tests.TerminalCasesGoNoDeeper"
      Tests.Visitor.MethodPointsAreDeeperThanMethods,
      "Tests.MethodPointsAreDeeperThanMethods"
      Tests.Visitor.BranchPointsAreComputedForSwitch,
      "Tests.BranchPointsAreComputedForSwitch"
      Tests.Visitor.BranchPointsAreComputedForMatch,
      "Tests.BranchPointsAreComputedForMatch"
      Tests.Visitor.MethodsAreDeeperThanTypes, "Tests.MethodsAreDeeperThanTypes"
      Tests.Visitor.TypesAreDeeperThanModules, "Tests.TypesAreDeeperThanModules"
      Tests.Visitor.ModulesAreDeeperThanAssemblies, "Tests.ModulesAreDeeperThanAssemblies"
      Tests.Visitor.AssembliesAreDeeperThanPaths, "Tests.AssembliesAreDeeperThanPaths"
      Tests.Visitor.FilteredAssembliesDoNotHaveSequencePoints,
      "Tests.FilteredAssembliesDoNotHaveSequencePoints"
      Tests.Visitor.TestExceptionWrapping, "Tests.TestExceptionWrapping"
      Tests.Visitor.TestFixPointInvoke, "Tests.TestFixPointInvoke"
      Tests.Visitor.TestFixPointApply, "Tests.TestFixPointApply"
      Tests.Visitor.PathsAreDeeperThanAVisit, "Tests.PathsAreDeeperThanAVisit"
      Tests.Visitor.TrackingDetectsTests, "Tests.TrackingDetectsTests"
      Tests.Visitor.TrackingDetectsExpectedTests, "Tests.TrackingDetectsExpectedTests"
      Tests.Visitor.TrackingDetectsTestsByFullType, "Tests.TrackingDetectsTestsByFullType"
      Tests.Visitor.TrackingDetectsMethods, "Tests.TrackingDetectsMethods"
      Tests.Visitor.NamingDetectEmpties, "Tests.NamingDetectEmpties"
      Tests.Visitor.NamingSuffixDetectEmpties, "Tests.NamingSuffixDetectEmpties"
      Tests.Visitor.TypeNamesAreExtracted, "Tests.TypeNamesAreExtracted"
      Tests.Visitor.FullTypeNamesAreExtracted, "Tests.FullTypeNamesAreExtracted"
      Tests.Visitor.TypeRefNamesAreExtracted, "Tests.TypeRefNamesAreExtracted"
      Tests.Visitor.FullTypeRefNamesAreExtracted, "Tests.FullTypeRefNamesAreExtracted"
      Tests.Visitor.MethodNamesAreExtracted, "Tests.MethodNamesAreExtracted"
      Tests.Visitor.FullMethodNamesAreExtracted, "Tests.FullMethodNamesAreExtracted"
      Tests.Visitor.ShouldGenerateExpectedNCoverReportWithOverloads,
      "Tests.ShouldGenerateExpectedNCoverReportWithOverloads"
      Tests.Visitor.ShouldGenerateExpectedXmlReportFromDotNet,
      "Tests.ShouldGenerateExpectedXmlReportFromDotNet"
      Tests.Visitor.ShouldGenerateExpectedXmlReportWithoutTriviaFromDotNet,
      "Tests.ShouldGenerateExpectedXmlReportWithoutTriviaFromDotNet"
      Tests.Visitor.ShouldGenerateExpectedXmlReportWithEmbeds,
      "Tests.ShouldGenerateExpectedXmlReportFromWithEmbeds"
      Tests.Visitor.ShouldGenerateExpectedXmlReportWithOverloads,
      "Tests.ShouldGenerateExpectedXmlReportWithOverloads"
      Tests.Visitor.ShouldGenerateExpectedXmlReportWithPartials,
      "Tests.ShouldGenerateExpectedXmlReportFromWithPartials"
      Tests.Visitor.ShouldGenerateExpectedJsonReportFromDotNet,
      "Tests.ShouldGenerateExpectedJsonReportFromDotNet"
      Tests.Visitor.ShouldGenerateExpectedJsonReportWithEmbeds,
      "Tests.ShouldGenerateExpectedJsonReportWithEmbeds"
      Tests.Visitor.ShouldGenerateExpectedJsonReportWithOverloads,
      "Tests.ShouldGenerateExpectedJsonReportWithOverloads"
      Tests.Visitor.ShouldGenerateExpectedJsonReportWithPartials,
      "Tests.ShouldGenerateExpectedJsonReportWithPartials"
      Tests.Visitor.ShouldGenerateExpectedXmlReportForNCoverWithMethodPointOnly,
      "Tests.ShouldGenerateExpectedXmlReportForNCoverWithMethodPointOnly"
      Tests.Visitor.ShouldGenerateExpectedXmlReportForNCoverWithTopLevel,
      "Tests.ShouldGenerateExpectedXmlReportForNCoverWithTopLevel"
      Tests.Visitor.ShouldGenerateExpectedXmlReportForOpenCoverWithMethodPointOnly,
      "Tests.ShouldGenerateExpectedXmlReportForOpenCoverWithMethodPointOnly"
      Tests.Visitor.ShouldGenerateExpectedXmlReportWithSourceLink,
      "Tests.ShouldGenerateExpectedXmlReportWithSourceLink"
      Tests.Visitor.ShouldGenerateExpectedXmlReportFromDotNetWithPathFilter,
      "Tests.ShouldGenerateExpectedXmlReportFromDotNetWithPathFilter"
      Tests.Visitor.ShouldGenerateExpectedXmlReportFromDotNetWhenExcluded,
      "Tests.ShouldGenerateExpectedXmlReportFromDotNetWhenExcluded"
      Tests.Visitor.ShouldGenerateExpectedXmlReportFromDotNetWhenExcludedEvenIfTracked,
      "Tests.ShouldGenerateExpectedXmlReportFromDotNetWhenExcludedEvenIfTracked"
      Tests.Visitor.ShouldDetectTernary, "Tests.ShouldDetectTernary"
      Tests.Visitor.ShouldDetectSwitchNesting, "Tests.ShouldDetectSwitchNesting"
      Tests.Visitor.SafeMultiplyIsSafe, "Tests.SafeMultiplyIsSafe"
      Tests.Visitor.EmptyMethodHasComplexity1, "Tests.EmptyMethodHasComplexity1"
      Tests.Visitor.BranchChainsSerialize, "Tests.BranchChainsSerialize"
      Tests.Visitor.BranchChainsTerminate, "Tests.BranchChainsTerminate"
      Tests.Visitor.ShouldGenerateExpectedXmlReportWithSourceLinkOpenCoverStyle,
      "Tests.ShouldGenerateExpectedXmlReportWithSourceLinkOpenCoverStyle"
      Tests.Visitor.ShouldGenerateExpectedXmlReportFromDotNetOpenCoverStyle,
      "Tests.ShouldGenerateExpectedXmlReportFromDotNetOpenCoverStyle"
      Tests.Visitor.ShouldGenerateExpectedXmlReportWithEmbedsOpenCoverStyle,
      "Tests.ShouldGenerateExpectedXmlReportWithEmbedsOpenCoverStyle"
      Tests.Visitor.ShouldGenerateExpectedXmlReportWithPartialsOpenCoverStyle,
      "Tests.ShouldGenerateExpectedXmlReportWithPartialsOpenCoverStyle"
      Tests.Visitor.ShouldGenerateExpectedXmlReportFromDotNetLineCoverStyle,
      "Tests.ShouldGenerateExpectedXmlReportFromDotNetLineCoverStyle"
      Tests.Visitor.ShouldGenerateExpectedXmlReportFromDotNetBranchCoverStyle,
      "Tests.ShouldGenerateExpectedXmlReportFromDotNetBranchCoverStyle"
      Tests.Visitor.ShouldGenerateExpectedXmlReportFromDotNetOpenCoverStyleWithTracking,
      "Tests.ShouldGenerateExpectedXmlReportFromDotNetOpenCoverStyleWithTracking"
      Tests.Visitor.ShouldGenerateExpectedXmlReportWithModuleExclusionOpenCoverStyle,
      "Tests.ShouldGenerateExpectedXmlReportWithModuleExclusionOpenCoverStyle"
      Tests.Visitor.ShouldGenerateExpectedTrackingXmlReportWithModuleExclusionOpenCoverStyle,
      "Tests.ShouldGenerateExpectedTrackingXmlReportWithModuleExclusionOpenCoverStyle"
      Tests.Visitor.ShouldGenerateExpectedXmlReportWithClassExclusionOpenCoverStyle,
      "Tests.ShouldGenerateExpectedXmlReportWithClassExclusionOpenCoverStyle"
      Tests.Visitor.ShouldGenerateExpectedTrackingXmlReportWithClassExclusionOpenCoverStyle,
      "Tests.ShouldGenerateExpectedTrackingXmlReportWithClassExclusionOpenCoverStyle"
      Tests.Visitor.ShouldGenerateExpectedXmlReportWithMethodExclusionOpenCoverStyle,
      "Tests.ShouldGenerateExpectedXmlReportWithMethodExclusionOpenCoverStyle"
      Tests.Visitor.ShouldGenerateExpectedXmlReportWithFileExclusionOpenCoverStyle,
      "Tests.ShouldGenerateExpectedXmlReportWithFileExclusionOpenCoverStyle"
      Tests.Visitor.ShouldGenerateExpectedTrackingXmlReportWithMethodExclusionOpenCoverStyle,
      "Tests.ShouldGenerateExpectedTrackingXmlReportWithMethodExclusionOpenCoverStyle"
      Tests.Visitor.ShouldGenerateExpectedXmlReportWithTraditionalInterfacesOpenCoverStyle,
      "Tests.ShouldGenerateExpectedXmlReportWithTraditionalInterfacesOpenCoverStyle"
      Tests.Visitor.ShouldGenerateExpectedXmlReportWithModernInterfacesOpenCoverStyle,
      "Tests.ShouldGenerateExpectedXmlReportWithModernInterfacesOpenCoverStyle"
      Tests.Visitor.ShouldSortFileIds, "Tests.ShouldSortFileIds"
      // CommandLine.fs
      Tests.CommandLine.StrongNameKeyCanBeValidated, "Tests.StrongNameKeyCanBeValidated"
      Tests.CommandLine.VerbosityShouldBeHonoured, "Tests.VerbosityShouldBeHonoured"
      Tests.CommandLine.CryptographicExceptionIsTransformed,
      "Tests.CryptographicExceptionIsTransformed"
      Tests.CommandLine.OutputCanBeExercised, "Tests.OutputCanBeExercised"
      Tests.CommandLine.NoThrowNoErrorLeavesAllOK, "Tests.NoThrowNoErrorLeavesAllOK"
      Tests.CommandLine.NoThrowWithErrorIsSignalled, "Tests.NoThrowWithErrorIsSignalled"
      Tests.CommandLine.ArgumentExceptionWrites, "Tests.ArgumentExceptionWrites"
      Tests.CommandLine.ArgumentExceptionWritesEx, "Tests.ArgumentExceptionWritesEx"
      Tests.CommandLine.IOExceptionWrites, "Tests.IOExceptionWrites"
      Tests.CommandLine.NotSupportedExceptionWrites, "Tests.NotSupportedExceptionWrites"
      Tests.CommandLine.SecurityExceptionWrites, "Tests.SecurityExceptionWrites"
      Tests.CommandLine.UnauthorizedAccessExceptionWrites,
      "Tests.UnauthorizedAccessExceptionWrites"
      // Instrument.fs
      Tests.Instrument.ShouldBeAbleToGetTheVisitReportMethod,
      "Tests.ShouldBeAbleToGetTheVisitReportMethod"
      Tests.Instrument.ShouldBeAbleToClearTheStrongNameKey,
      "Tests.ShouldBeAbleToClearTheStrongNameKey"
      Tests.Instrument.ShouldBeAbleToUpdateTheStrongNameKeyWherePossible,
      "Tests.ShouldBeAbleToUpdateTheStrongNameKeyWherePossible"
      Tests.Instrument.NoKnownKeyInEmptyIndex, "Tests.NoKnownKeyInEmptyIndex"
      Tests.Instrument.KnownKeyMatchedInIndex, "Tests.KnownKeyMatchedInIndex"
      Tests.Instrument.ThirdPartyKeyNotMatchedInIndex,
      "Tests.ThirdPartyKeyNotMatchedInIndex"
      Tests.Instrument.FakedUpKeyIsMatchedInIndex, "Tests.FakedUpKeyIsMatchedInIndex"
      Tests.Instrument.NoKnownKeyIfAssemblyHasNone, "Tests.NoKnownKeyIfAssemblyHasNone"
      Tests.Instrument.NoKnownTokenInEmptyIndex, "Tests.NoKnownTokenInEmptyIndex"
      Tests.Instrument.KnownTokenMatchedInIndex, "Tests.KnownTokenMatchedInIndex"
      Tests.Instrument.NoKnownTokenIfAssemblyHasNone,
      "Tests.NoKnownTokenIfAssemblyHasNone"
      Tests.Instrument.ForeignTokenIsNotMatchedInIndex,
      "Tests.ForeignTokenIsNotMatchedInIndex"
      Tests.Instrument.FakedUpTokenIsMatchedInIndex, "Tests.FakedUpTokenIsMatchedInIndex"
      Tests.Instrument.GuardShouldDisposeRecordingAssemblyOnException,
      "Tests.GuardShouldDisposeRecordingAssemblyOnException"
      Tests.Instrument.ShouldBeAbleToTellAnAssembly, "Tests.ShouldBeAbleToTellAnAssembly"
      Tests.Instrument.ShouldBeAbleToValidateAnAssembly,
      "Tests.ShouldBeAbleToValidateAnAssembly"
      Tests.Instrument.ShouldBeAbleToLocateAReference,
      "Tests.ShouldBeAbleToLocateAReference"
      Tests.Instrument.ShouldBeAbleToPrepareTheAssembly,
      "Tests.ShouldBeAbleToPrepareTheAssembly"
      Tests.Instrument.ShouldGetTrackingStyleIfSet, "Tests.ShouldGetTrackingStyleIfSet"
      Tests.Instrument.ShouldGetNewFilePathFromPreparedAssembly,
      "Tests.ShouldGetNewFilePathFromPreparedAssembly"
      Tests.Instrument.ShouldHandleNullConstantsOK, "Tests.ShouldHandleNullConstantsOK"
      Tests.Instrument.ShouldRescopeMonoMethodOK, "Tests.ShouldRescopeMonoMethodOK"
      Tests.Instrument.ShouldWriteMonoAssemblyOK, "Tests.ShouldWriteMonoAssemblyOK"
      Tests.Instrument.ShouldGetVisitFromWrittenAssembly,
      "Tests.ShouldGetVisitFromWrittenAssembly"
      //Tests.Instrument.ShouldUpdateHandlerOK([<Range(0, 31)>] selection)
      Tests.Instrument.ShouldSubstituteInstructionOperand,
      "Tests.ShouldSubstituteInstructionOperand"
      Tests.Instrument.ShouldNotSubstituteDifferentInstructionOperand,
      "Tests.ShouldNotSubstituteDifferentInstructionOperand"
      Tests.Instrument.ShouldSubstituteIntoInstructionOperandArray,
      "Tests.ShouldSubstituteIntoInstructionOperandArray"
      Tests.Instrument.ShouldNotSubstituteOutsideInstructionOperandArray,
      "Tests.ShouldNotSubstituteOutsideInstructionOperandArray"
      Tests.Instrument.ShouldNotSubstituteOtherOperand,
      "Tests.ShouldNotSubstituteOtherOperand"
      Tests.Instrument.ShouldBeAbleToTrackAMethod, "Tests.ShouldBeAbleToTrackAMethod"
      Tests.Instrument.ShouldBeAbleToTrackAMethodWithTailCalls,
      "Tests.ShouldBeAbleToTrackAMethodWithTailCalls"
      Tests.Instrument.ShouldBeAbleToTrackAMethodWithNonVoidReturn,
      "Tests.ShouldBeAbleToTrackAMethodWithNonVoidReturn"
      Tests.Instrument.ShouldBeAbleToTrackAnAsyncMethod,
      "Tests.ShouldBeAbleToTrackAnAsyncMethod"
      Tests.Instrument.ShouldBeAbleToTrackAnFSAsyncMethod,
      "Tests.ShouldBeAbleToTrackAnFSAsyncMethod"
      Tests.Instrument.ShouldBeAbleToTrackAnFSTaskMethod,
      "Tests.ShouldBeAbleToTrackAnFSTaskMethod"
      Tests.Instrument.ShouldBeAbleToInstrumentASwitchForNCover,
      "Tests.ShouldBeAbleToInstrumentASwitchForNCover"
      Tests.Instrument.ShouldNotChangeAnUntrackedMethod,
      "Tests.ShouldNotChangeAnUntrackedMethod"
      Tests.Instrument.SwitchBranchesShouldInstrumentByPushingDown,
      "Tests.SwitchBranchesShouldInstrumentByPushingDown"
      Tests.Instrument.PseudoSwitchVisibleBranchesShouldSkipNonRepresentativeCases,
      "Tests.PseudoSwitchVisibleBranchesShouldSkipNonRepresentativeCases"
      Tests.Instrument.SimpleBranchShouldInstrumentByPushingDown,
      "Tests.SimpleBranchShouldInstrumentByPushingDown"
      Tests.Instrument.StartShouldLoadRecordingAssembly,
      "Tests.StartShouldLoadRecordingAssembly"
      Tests.Instrument.TypeShouldNotChangeState, "Tests.TypeShouldNotChangeState"
      Tests.Instrument.ExcludedMethodShouldNotChangeState,
      "Tests.ExcludedMethodShouldNotChangeState"
      Tests.Instrument.IncludedMethodShouldChangeState,
      "Tests.IncludedMethodShouldChangeState"
      Tests.Instrument.ExcludedAfterMethodShouldNotChangeState,
      "Tests.ExcludedAfterMethodShouldNotChangeState"
      Tests.Instrument.IncludedAfterMethodShouldRewriteMethod,
      "Tests.IncludedAfterMethodShouldRewriteMethod"
      Tests.Instrument.NoStrongNameShouldUpdateVisibleTo,
      "Tests.NoStrongNameShouldUpdateVisibleTo"
      Tests.Instrument.NewStrongNameShouldUpdateVisibleTo,
      "Tests.NewStrongNameShouldUpdateVisibleTo"
      Tests.Instrument.UpdateStrongReferencesShouldChangeSigningKeyWherePossible,
      "Tests.UpdateStrongReferencesShouldChangeSigningKeyWherePossible"
      Tests.Instrument.UpdateStrongReferencesShouldChangeSigningKeyWherePossible2,
      "Tests.UpdateStrongReferencesShouldChangeSigningKeyWherePossible2"
      Tests.Instrument.UpdateStrongReferencesShouldRemoveSigningKeyIfRequired,
      "Tests.UpdateStrongReferencesShouldRemoveSigningKeyIfRequired"
      Tests.Instrument.UpdateStrongReferencesShouldNotAddASigningKey,
      "Tests.UpdateStrongReferencesShouldNotAddASigningKey"
      Tests.Instrument.UpdateStrongReferencesShouldTrackReferences,
      "Tests.UpdateStrongReferencesShouldTrackReferences"
      Tests.Instrument.UpdateStrongReferencesShouldTrackReferencesEvenFakes,
      "Tests.UpdateStrongReferencesShouldTrackReferencesEvenFakes"
      Tests.Instrument.ExcludedAssemblyRefsAreNotUpdated,
      "Tests.ExcludedAssemblyRefsAreNotUpdated"
      Tests.Instrument.IncludedAssemblyRefsAreUpdated,
      "Tests.IncludedAssemblyRefsAreUpdated"
      Tests.Instrument.ExcludedModuleJustRecordsMVid,
      "Tests.ExcludedModuleJustRecordsMVid"
      Tests.Instrument.ExcludedModuleJustRecordsNameForJson,
      "Tests.ExcludedModuleJustRecordsNameForJson"
      Tests.Instrument.ExcludedModuleJustRecordsHashForOpenCover,
      "Tests.ExcludedModuleJustRecordsHashForOpenCover"
      Tests.Instrument.IncludedModuleEnsuresRecorder,
      "Tests.IncludedModuleEnsuresRecorder"
      Tests.Instrument.ExcludedMethodPointIsPassThrough,
      "Tests.ExcludedMethodPointIsPassThrough"
      Tests.Instrument.IncludedMethodPointInsertsVisit,
      "Tests.IncludedMethodPointInsertsVisit"
      Tests.Instrument.IncludedModuleDoesNotChangeRecorderJustTheReference,
      "Tests.IncludedModuleDoesNotChangeRecorderJustTheReference"
      Tests.Instrument.AfterModuleShouldNotChangeState,
      "Tests.AfterModuleShouldNotChangeState"
      Tests.Instrument.JSONInjectionTransformsSimpleFileAsExpected,
      "Tests.JSONInjectionTransformsSimpleFileAsExpected"
      Tests.Instrument.JSONInjectionTransformsStandaloneFileAsExpected,
      "Tests.JSONInjectionTransformsStandaloneFileAsExpected"
      Tests.Instrument.JSONInjectionTransformsDependencyFileAsExpected,
      "Tests.JSONInjectionTransformsDependencyFileAsExpected"
      Tests.Instrument.JSONInjectionIsIdempotent, "Tests.JSONInjectionIsIdempotent"
      Tests.Instrument.NonFinishShouldDisposeRecordingAssembly,
      "Tests.NonFinishShouldDisposeRecordingAssembly"
      Tests.Instrument.NonFinishShouldDisposeThreadingAssembly,
      "Tests.NonFinishShouldDisposeThreadingAssembly"
      Tests.Instrument.NonFinishShouldNotDisposeNullRecordingAssembly,
      "Tests.NonFinishShouldNotDisposeNullRecordingAssembly"
      Tests.Instrument.FinishShouldLeaveRecordingAssembly,
      "Tests.FinishShouldLeaveRecordingAssembly"

      ]

  let specials =
    { 0..31 }
    |> Seq.map (fun i ->
      testCase (sprintf "Tests.ShouldUpdateHandlerOK(%d)" i)
      <| (fun () ->
        lock ExpectoTestCommon.sync (fun () ->
          AltCover.Main.init ()
          Tests.Instrument.ShouldUpdateHandlerOK i)))
    |> Seq.toList

  let consistencyCheck () =
    ExpectoTestCommon.consistencyCheck
      regular
      [ "Tests.Instrument::ShouldUpdateHandlerOK" ]

  [<Tests>]
  let tests =
    ExpectoTestCommon.makeTests
      "AltCoverEngineTests"
      consistencyCheck
      regular
      specials
      ignore

module UnitTestStub =
  [<EntryPoint; System.Runtime.CompilerServices.CompilerGenerated>]
  let unitTestStub argv =

    runTestsWithCLIArgs Seq.empty<CLIArguments> argv ExpectoMain.tests
#endif