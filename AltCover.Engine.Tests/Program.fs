namespace Tests

#if !NET472

open Expecto

module ExpectoMain =
  let regular =
    [ Tests.TestCommonTests.TestMultiple, "TestCommon.TestMultiple"
      Tests.TestCommonTests.TestIgnoredTests, "TestCommon.TestIgnoredTests"
      Tests.TestCommonTests.ExerciseItAll, "TestCommon.ExerciseItAll"
      Tests.TestCommonTests.SelfTest, "TestCommon.SelfTest"
      // Augment.fs
      Tests.Augment.ZeroIsNotVisited, "Augment.ZeroIsNotVisited"
      Tests.Augment.PositiveIsVisited, "Augment.PositiveIsVisited"
      Tests.Augment.NegativesSpray, "Augment.NegativesSpray"
      // Base.fs
      Tests.BaseTests.ExerciseBoth, "BaseTests.ExerciseBoth"
      Tests.BaseTests.ExerciseTime, "BaseTests.ExerciseTime"
      Tests.BaseTests.ExerciseCall, "BaseTests.ExerciseCall"
      Tests.BaseTests.ExerciseNull, "BaseTests.ExerciseNull"
      Tests.BaseTests.ExercisePointVisit, "BaseTests.ExercisePointVisit"
      // ProgramDatabase.fs
      Tests.ProgramDatabase.ShouldTrapIndexOutOfRangeException,
      "ProgramDatabase.ShouldTrapIndexOutOfRangeException"
      Tests.ProgramDatabase.ShouldGetPdbFromImage, "ProgramDatabase.ShouldGetPdbFromImage"
      Tests.ProgramDatabase.ShouldGetEmbeddedPdbFromImage,
      "ProgramDatabase.ShouldGetEmbeddedPdbFromImage"
      Tests.ProgramDatabase.ShouldGetNoMdbFromMonoImage,
      "ProgramDatabase.ShouldGetNoMdbFromMonoImage"
      Tests.ProgramDatabase.ShouldGetGUIDfromNativePdb,
      "ProgramDatabase.ShouldGetGUIDfromNativePdb"
      Tests.ProgramDatabase.ShouldGetPdbWithFallback,
      "ProgramDatabase.ShouldGetPdbWithFallback"
      Tests.ProgramDatabase.ShouldGetForeignPdbWithFallback,
      "ProgramDatabase.ShouldGetForeignPdbWithFallback"
      Tests.ProgramDatabase.ShouldGetForeignPdbWithFallbackWhenNotColocated,
      "ProgramDatabase.ShouldGetForeignPdbWithFallbackWhenNotColocated"
      Tests.ProgramDatabase.ShouldGetMdbWithFallback,
      "ProgramDatabase.ShouldGetMdbWithFallback"
      Tests.ProgramDatabase.ShouldGetSymbolsFromPdb,
      "ProgramDatabase.ShouldGetSymbolsFromPdb"
      Tests.ProgramDatabase.ShouldGetSymbolsFromEmbeddedPdb,
      "ProgramDatabase.ShouldGetSymbolsFromEmbeddedPdb"
      Tests.ProgramDatabase.ShouldNotGetSymbolsWhenNoPdb,
      "ProgramDatabase.ShouldNotGetSymbolsWhenNoPdb"
      Tests.ProgramDatabase.ShouldGetSymbolsFromMdb,
      "ProgramDatabase.ShouldGetSymbolsFromMdb"
      // Filter.fs
      Tests.ACFilter.NoneOfTheAboveMatchesNoType, "Filter.NoneOfTheAboveMatchesNoType"
      Tests.ACFilter.NoneOfTheAboveMatchesNoAttribute,
      "Filter.NoneOfTheAboveMatchesNoAttribute"
      Tests.ACFilter.NoneOfTheAboveMatchesNoAssembly,
      "Filter.NoneOfTheAboveMatchesNoAssembly"
      Tests.ACFilter.NoneOfTheAboveMatchesNoModule, "Filter.NoneOfTheAboveMatchesNoModule"
      Tests.ACFilter.NoneOfTheAboveMatchesNoFile, "Filter.NoneOfTheAboveMatchesNoFile"
      Tests.ACFilter.NoneOfTheAboveMatchesNoPath, "Filter.NoneOfTheAboveMatchesNoPath"
      Tests.ACFilter.NoneOfTheAboveMatchesNoMethod, "Filter.NoneOfTheAboveMatchesNoMethod"
      Tests.ACFilter.FileDoesNotMatchNonFileClass, "Filter.FileDoesNotMatchNonFileClass"
      Tests.ACFilter.FileDoesMatchFileClass, "Filter.FileDoesMatchFileClass"
      Tests.ACFilter.PathDoesNotMatchNonPathClass, "Filter.PathDoesNotMatchNonPathClass"
      Tests.ACFilter.PathDoesMatchPathClass, "Filter.PathDoesMatchPathClass"
      Tests.ACFilter.AssemblyDoesNotMatchNonAssemblyClass,
      "Filter.AssemblyDoesNotMatchNonAssemblyClass"
      Tests.ACFilter.AssemblyDoesMatchAssemblyClass,
      "Filter.AssemblyDoesMatchAssemblyClass"
      Tests.ACFilter.ModuleDoesNotMatchNonModuleClass,
      "Filter.ModuleDoesNotMatchNonModuleClass"
      Tests.ACFilter.ModuleDoesMatchModuleClass, "Filter.ModuleDoesMatchModuleClass"
      Tests.ACFilter.TypeDoesNotMatchNonTypeClass, "Filter.TypeDoesNotMatchNonTypeClass"
      Tests.ACFilter.TypeDoesMatchTypeClass, "Filter.TypeDoesMatchTypeClass"
      Tests.ACFilter.MethodDoesNotMatchNonMethodClass,
      "Filter.MethodDoesNotMatchNonMethodClass"
      Tests.ACFilter.MethodDoesMatchMethodClass, "Filter.MethodDoesMatchMethodClass"
      Tests.ACFilter.AttributeDoesNotMatchNonAttributeClass,
      "Filter.AttributeDoesNotMatchNonAttributeClass"
      Tests.ACFilter.AttributeDoesMatchAttributeClass,
      "Filter.AttributeDoesMatchAttributeClass"
      Tests.ACFilter.CanExcludeCSharpPropertiesByAttribute,
      "Filter.CanExcludeCSharpPropertiesByAttribute"
      Tests.ACFilter.RefStructsAreNotObsolete, "Filter.RefStructsAreNotObsolete"
      Tests.ACFilter.Sample3Class1IsCSharpAutoproperty,
      "Filter.Sample3Class1IsCSharpAutoproperty"
      Tests.ACFilter.Sample3Class2IsNotCSharpAutoproperty,
      "Filter.Sample3Class2IsNotCSharpAutoproperty"
      Tests.ACFilter.CanIdentifyExcludedFSharpMethods,
      "Filter.CanIdentifyExcludedFSharpMethods"
      Tests.ACFilter.CanIdentifyExcludedCSharpAutoProperties,
      "Filter.CanIdentifyExcludedCSharpAutoProperties"
      Tests.ACFilter.CanIdentifyIncludedCSharpProperties,
      "Filter.CanIdentifyIncludedCSharpProperties"
      // Visitor.fs
      Tests.Visitor.ReportFileShouldBeCorrectlySuffixed,
      "Visitor.ReportFileShouldBeCorrectlySuffixed"
      Tests.Visitor.ReportFileShouldBeCorrectlyExtended,
      "Visitor.ReportFileShouldBeCorrectlyExtended"
      Tests.Visitor.CanSwitchSampling, "Visitor.CanSwitchSampling"
      Tests.Visitor.ValidateStaticExemption, "Visitor.ValidateStaticExemption"
      Tests.Visitor.ValidateStaticClass, "Visitor.ValidateStaticClass"
      Tests.Visitor.ValidateAutomaticExemption, "Visitor.ValidateAutomaticExemption"
      Tests.Visitor.DetectLocalSource, "Visitor.DetectLocalSource"
      Tests.Visitor.LocateMatchShouldChooseLongerWildCardPath,
      "Visitor.LocateMatchShouldChooseLongerWildCardPath"
      Tests.Visitor.LocateMatchFallsBackOK, "Visitor.LocateMatchFallsBackOK"
      Tests.Visitor.AsyncTestInContext, "Visitor.AsyncTestInContext"
      Tests.Visitor.AnotherAsyncTestInContext, "Visitor.AnotherAsyncTestInContext"
      Tests.Visitor.DebugBuildTernaryTestInContext,
      "Visitor.DebugBuildTernaryTestInContext"
      Tests.Visitor.ReleaseBuildTernaryTest, "Visitor.ReleaseBuildTernaryTest"
      Tests.Visitor.ReleaseBuildTernaryTestInContext,
      "Visitor.ReleaseBuildTernaryTestInContext"
      Tests.Visitor.ReleaseBuildTernaryTestInContextWithCoalescence,
      "Visitor.ReleaseBuildTernaryTestInContextWithCoalescence"
      Tests.Visitor.CSharpNestedMethods, "Visitor.CSharpNestedMethods"
      Tests.Visitor.FSharpNestedMethodsClassic, "Visitor.FSharpNestedMethodsClassic"
      Tests.Visitor.FSharpNestedMethods5x0x201, "Visitor.FSharpNestedMethods_5_0_201"
      Tests.Visitor.ValidateSeqPntFixUp, "Visitor.ValidateSeqPntFixUp" // HACK HACK HACK
      Tests.Visitor.EmptyArrayHasExpectedHash, "Visitor.EmptyArrayHasExpectedHash"
      Tests.Visitor.KeyHasExpectedToken, "Visitor.KeyHasExpectedToken"
      Tests.Visitor.TokenGeneratesExpectedULong, "Visitor.TokenGeneratesExpectedULong"
      Tests.Visitor.KeyHasExpectedIndex, "Visitor.KeyHasExpectedIndex"
      Tests.Visitor.EmptyArrayHasExpectedIndex, "Visitor.EmptyArrayHasExpectedIndex"
      Tests.Visitor.KeyHasExpectedRecord, "Visitor.KeyHasExpectedRecord"
      Tests.Visitor.KeyHasExpectedPlaceInIndex, "Visitor.KeyHasExpectedPlaceInIndex"
      Tests.Visitor.EmptyFiltersPassAll, "Visitor.EmptyFiltersPassAll"
      Tests.Visitor.NonEmptyFiltersCatchAnExpectedValue,
      "Visitor.NonEmptyFiltersCatchAnExpectedValue"
      Tests.Visitor.NonEmptyFiltersPassAnExpectedValue,
      "Visitor.NonEmptyFiltersPassAnExpectedValue"
      Tests.Visitor.AfterProcessingYieldsAnExpectedValue,
      "Visitor.AfterProcessingYieldsAnExpectedValue"
      Tests.Visitor.Sample3Class1PropertyIsNotSignificant,
      "Visitor.Sample3Class1PropertyIsNotSignificant"
      Tests.Visitor.Sample3Class2IPropertyIsSignificant,
      "Visitor.Sample3Class2IPropertyIsSignificant"
      Tests.Visitor.TerminalCasesGoNoDeeper, "Visitor.TerminalCasesGoNoDeeper"
      Tests.Visitor.MethodPointsAreDeeperThanMethods,
      "Visitor.MethodPointsAreDeeperThanMethods"
      Tests.Visitor.BranchPointsAreComputedForSwitch,
      "Visitor.BranchPointsAreComputedForSwitch"
      Tests.Visitor.BranchPointsAreComputedForMatch,
      "Visitor.BranchPointsAreComputedForMatch"
      Tests.Visitor.MethodsAreDeeperThanTypes, "Visitor.MethodsAreDeeperThanTypes"
      Tests.Visitor.TypesAreDeeperThanModules, "Visitor.TypesAreDeeperThanModules"
      Tests.Visitor.ModulesAreDeeperThanAssemblies,
      "Visitor.ModulesAreDeeperThanAssemblies"
      Tests.Visitor.AssembliesAreDeeperThanPaths, "Visitor.AssembliesAreDeeperThanPaths"
      Tests.Visitor.FilteredAssembliesDoNotHaveSequencePoints,
      "Visitor.FilteredAssembliesDoNotHaveSequencePoints"
      Tests.Visitor.TestExceptionWrapping, "Visitor.TestExceptionWrapping"
      Tests.Visitor.TestFixPointInvoke, "Visitor.TestFixPointInvoke"
      Tests.Visitor.TestFixPointApply, "Visitor.TestFixPointApply"
      Tests.Visitor.PathsAreDeeperThanAVisit, "Visitor.PathsAreDeeperThanAVisit"
      Tests.Visitor.TrackingDetectsTests, "Visitor.TrackingDetectsTests"
      Tests.Visitor.TrackingDetectsExpectedTests, "Visitor.TrackingDetectsExpectedTests"
      Tests.Visitor.TrackingDetectsTestsByFullType,
      "Visitor.TrackingDetectsTestsByFullType"
      Tests.Visitor.TrackingDetectsMethods, "Visitor.TrackingDetectsMethods"
      Tests.Visitor.NamingDetectEmpties, "Visitor.NamingDetectEmpties"
      Tests.Visitor.NamingSuffixDetectEmpties, "Visitor.NamingSuffixDetectEmpties"
      Tests.Visitor.TypeNamesAreExtracted, "Visitor.TypeNamesAreExtracted"
      Tests.Visitor.FullTypeNamesAreExtracted, "Visitor.FullTypeNamesAreExtracted"
      Tests.Visitor.TypeRefNamesAreExtracted, "Visitor.TypeRefNamesAreExtracted"
      Tests.Visitor.FullTypeRefNamesAreExtracted, "Visitor.FullTypeRefNamesAreExtracted"
      Tests.Visitor.MethodNamesAreExtracted, "Visitor.MethodNamesAreExtracted"
      Tests.Visitor.FullMethodNamesAreExtracted, "Visitor.FullMethodNamesAreExtracted"
      Tests.Visitor.ShouldGenerateExpectedNCoverReportWithOverloads,
      "Visitor.ShouldGenerateExpectedNCoverReportWithOverloads"
      Tests.Visitor.ShouldGenerateExpectedXmlReportFromDotNet,
      "Visitor.ShouldGenerateExpectedXmlReportFromDotNet"
      Tests.Visitor.ShouldGenerateExpectedXmlReportWithoutTriviaFromDotNet,
      "Visitor.ShouldGenerateExpectedXmlReportWithoutTriviaFromDotNet"
      Tests.Visitor.ShouldGenerateExpectedXmlReportWithEmbeds,
      "Visitor.ShouldGenerateExpectedXmlReportFromWithEmbeds"
      Tests.Visitor.ShouldGenerateExpectedXmlReportWithOverloads,
      "Visitor.ShouldGenerateExpectedXmlReportWithOverloads"
      Tests.Visitor.ShouldGenerateExpectedXmlReportWithPartials,
      "Visitor.ShouldGenerateExpectedXmlReportFromWithPartials"
      Tests.Visitor.ShouldGenerateExpectedJsonReportFromDotNet,
      "Visitor.ShouldGenerateExpectedJsonReportFromDotNet"
      Tests.Visitor.ShouldGenerateExpectedJsonReportWithEmbeds,
      "Visitor.ShouldGenerateExpectedJsonReportWithEmbeds"
      Tests.Visitor.ShouldGenerateExpectedJsonReportWithOverloads,
      "Visitor.ShouldGenerateExpectedJsonReportWithOverloads"
      Tests.Visitor.ShouldGenerateExpectedJsonReportWithPartials,
      "Visitor.ShouldGenerateExpectedJsonReportWithPartials"
      Tests.Visitor.ShouldGenerateExpectedXmlReportForNCoverWithMethodPointOnly,
      "Visitor.ShouldGenerateExpectedXmlReportForNCoverWithMethodPointOnly"
      Tests.Visitor.ShouldGenerateExpectedXmlReportForNCoverWithTopLevel,
      "Visitor.ShouldGenerateExpectedXmlReportForNCoverWithTopLevel"
      Tests.Visitor.ShouldGenerateExpectedXmlReportForOpenCoverWithMethodPointOnly,
      "Visitor.ShouldGenerateExpectedXmlReportForOpenCoverWithMethodPointOnly"
      Tests.Visitor.ShouldGenerateExpectedXmlReportWithSourceLink,
      "Visitor.ShouldGenerateExpectedXmlReportWithSourceLink"
      Tests.Visitor.ShouldGenerateExpectedXmlReportFromDotNetWithPathFilter,
      "Visitor.ShouldGenerateExpectedXmlReportFromDotNetWithPathFilter"
      Tests.Visitor.ShouldGenerateExpectedXmlReportFromDotNetWhenExcluded,
      "Visitor.ShouldGenerateExpectedXmlReportFromDotNetWhenExcluded"
      Tests.Visitor.ShouldGenerateExpectedXmlReportFromDotNetWhenExcludedEvenIfTracked,
      "Visitor.ShouldGenerateExpectedXmlReportFromDotNetWhenExcludedEvenIfTracked"
      Tests.Visitor.ShouldDetectTernary, "Visitor.ShouldDetectTernary"
      Tests.Visitor.ShouldDetectSwitchNesting, "Visitor.ShouldDetectSwitchNesting"
      Tests.Visitor.SafeMultiplyIsSafe, "Visitor.SafeMultiplyIsSafe"
      Tests.Visitor.EmptyMethodHasComplexity1, "Visitor.EmptyMethodHasComplexity1"
      Tests.Visitor.BranchChainsSerialize, "Visitor.BranchChainsSerialize"
      Tests.Visitor.BranchChainsTerminate, "Visitor.BranchChainsTerminate"
      Tests.Visitor.ShouldGenerateExpectedXmlReportWithSourceLinkOpenCoverStyle,
      "Visitor.ShouldGenerateExpectedXmlReportWithSourceLinkOpenCoverStyle"
      Tests.Visitor.ShouldGenerateExpectedXmlReportFromDotNetOpenCoverStyle,
      "Visitor.ShouldGenerateExpectedXmlReportFromDotNetOpenCoverStyle"
      Tests.Visitor.ShouldGenerateExpectedXmlReportWithEmbedsOpenCoverStyle,
      "Visitor.ShouldGenerateExpectedXmlReportWithEmbedsOpenCoverStyle"
      Tests.Visitor.ShouldGenerateExpectedXmlReportWithPartialsOpenCoverStyle,
      "Visitor.ShouldGenerateExpectedXmlReportWithPartialsOpenCoverStyle"
      Tests.Visitor.ShouldGenerateExpectedXmlReportFromDotNetLineCoverStyle,
      "Visitor.ShouldGenerateExpectedXmlReportFromDotNetLineCoverStyle"
      Tests.Visitor.ShouldGenerateExpectedXmlReportFromDotNetBranchCoverStyle,
      "Visitor.ShouldGenerateExpectedXmlReportFromDotNetBranchCoverStyle"
      Tests.Visitor.ShouldGenerateExpectedXmlReportFromDotNetOpenCoverStyleWithTracking,
      "Visitor.ShouldGenerateExpectedXmlReportFromDotNetOpenCoverStyleWithTracking"
      Tests.Visitor.ShouldGenerateExpectedXmlReportWithModuleExclusionOpenCoverStyle,
      "Visitor.ShouldGenerateExpectedXmlReportWithModuleExclusionOpenCoverStyle"
      Tests.Visitor.ShouldGenerateExpectedTrackingXmlReportWithModuleExclusionOpenCoverStyle,
      "Visitor.ShouldGenerateExpectedTrackingXmlReportWithModuleExclusionOpenCoverStyle"
      Tests.Visitor.ShouldGenerateExpectedXmlReportWithClassExclusionOpenCoverStyle,
      "Visitor.ShouldGenerateExpectedXmlReportWithClassExclusionOpenCoverStyle"
      Tests.Visitor.ShouldGenerateExpectedTrackingXmlReportWithClassExclusionOpenCoverStyle,
      "Visitor.ShouldGenerateExpectedTrackingXmlReportWithClassExclusionOpenCoverStyle"
      Tests.Visitor.ShouldGenerateExpectedXmlReportWithMethodExclusionOpenCoverStyle,
      "Visitor.ShouldGenerateExpectedXmlReportWithMethodExclusionOpenCoverStyle"
      Tests.Visitor.ShouldGenerateExpectedXmlReportWithFileExclusionOpenCoverStyle,
      "Visitor.ShouldGenerateExpectedXmlReportWithFileExclusionOpenCoverStyle"
      Tests.Visitor.ShouldGenerateExpectedTrackingXmlReportWithMethodExclusionOpenCoverStyle,
      "Visitor.ShouldGenerateExpectedTrackingXmlReportWithMethodExclusionOpenCoverStyle"
      Tests.Visitor.ShouldGenerateExpectedXmlReportWithTraditionalInterfacesOpenCoverStyle,
      "Visitor.ShouldGenerateExpectedXmlReportWithTraditionalInterfacesOpenCoverStyle"
      Tests.Visitor.ShouldGenerateExpectedXmlReportWithModernInterfacesOpenCoverStyle,
      "Visitor.ShouldGenerateExpectedXmlReportWithModernInterfacesOpenCoverStyle"
      Tests.Visitor.ShouldSortFileIds, "Visitor.ShouldSortFileIds"
      Tests.Visitor.ShouldGenerateExpectedXmlReportFromMono,
      "Visitor.ShouldGenerateExpectedXmlReportFromMono"
      Tests.Visitor.ShouldGenerateExpectedXmlReportFromMonoOpenCoverStyle,
      "Visitor.ShouldGenerateExpectedXmlReportFromMonoOpenCoverStyle"

      // CommandLine.fs
      Tests.CommandLine.StrongNameKeyCanBeValidated,
      "CommandLine.StrongNameKeyCanBeValidated"
      Tests.CommandLine.VerbosityShouldBeHonoured, "CommandLine.VerbosityShouldBeHonoured"
      Tests.CommandLine.CryptographicExceptionIsTransformed,
      "CommandLine.CryptographicExceptionIsTransformed"
      Tests.CommandLine.OutputCanBeExercised, "CommandLine.OutputCanBeExercised"
      Tests.CommandLine.NoThrowNoErrorLeavesAllOK, "CommandLine.NoThrowNoErrorLeavesAllOK"
      Tests.CommandLine.NoThrowWithErrorIsSignalled,
      "CommandLine.NoThrowWithErrorIsSignalled"
      Tests.CommandLine.ArgumentExceptionWrites, "CommandLine.ArgumentExceptionWrites"
      Tests.CommandLine.ArgumentExceptionWritesEx, "CommandLine.ArgumentExceptionWritesEx"
      Tests.CommandLine.IOExceptionWrites, "CommandLine.IOExceptionWrites"
      Tests.CommandLine.NotSupportedExceptionWrites,
      "CommandLine.NotSupportedExceptionWrites"
      Tests.CommandLine.SecurityExceptionWrites, "CommandLine.SecurityExceptionWrites"
      Tests.CommandLine.UnauthorizedAccessExceptionWrites,
      "CommandLine.UnauthorizedAccessExceptionWrites"
      Tests.CommandLine.ShouldLaunchWithExpectedOutput,
      "CommandLine.ShouldLaunchWithExpectedOutput"
      Tests.CommandLine.OutputVerbose, "CommandLine.OutputVerbose"

      // Instrument.fs
      Tests.Instrument.ShouldBeAbleToGetTheVisitReportMethod,
      "Instrument.ShouldBeAbleToGetTheVisitReportMethod"
      Tests.Instrument.ShouldBeAbleToClearTheStrongNameKey,
      "Instrument.ShouldBeAbleToClearTheStrongNameKey"
      Tests.Instrument.ShouldBeAbleToUpdateTheStrongNameKeyWherePossible,
      "Instrument.ShouldBeAbleToUpdateTheStrongNameKeyWherePossible"
      Tests.Instrument.NoKnownKeyInEmptyIndex, "Instrument.NoKnownKeyInEmptyIndex"
      Tests.Instrument.KnownKeyMatchedInIndex, "Instrument.KnownKeyMatchedInIndex"
      Tests.Instrument.ThirdPartyKeyNotMatchedInIndex,
      "Instrument.ThirdPartyKeyNotMatchedInIndex"
      Tests.Instrument.FakedUpKeyIsMatchedInIndex, "Instrument.FakedUpKeyIsMatchedInIndex"
      Tests.Instrument.NoKnownKeyIfAssemblyHasNone,
      "Instrument.NoKnownKeyIfAssemblyHasNone"
      Tests.Instrument.NoKnownTokenInEmptyIndex, "Instrument.NoKnownTokenInEmptyIndex"
      Tests.Instrument.KnownTokenMatchedInIndex, "Instrument.KnownTokenMatchedInIndex"
      Tests.Instrument.NoKnownTokenIfAssemblyHasNone,
      "Instrument.NoKnownTokenIfAssemblyHasNone"
      Tests.Instrument.ForeignTokenIsNotMatchedInIndex,
      "Instrument.ForeignTokenIsNotMatchedInIndex"
      Tests.Instrument.FakedUpTokenIsMatchedInIndex,
      "Instrument.FakedUpTokenIsMatchedInIndex"
      Tests.Instrument.GuardShouldDisposeRecordingAssemblyOnException,
      "Instrument.GuardShouldDisposeRecordingAssemblyOnException"
      Tests.Instrument.ShouldBeAbleToTellAnAssembly,
      "Instrument.ShouldBeAbleToTellAnAssembly"
      Tests.Instrument.ShouldBeAbleToValidateAnAssembly,
      "Instrument.ShouldBeAbleToValidateAnAssembly"
      Tests.Instrument.ShouldBeAbleToLocateAReference,
      "Instrument.ShouldBeAbleToLocateAReference"
      Tests.Instrument.ShouldBeAbleToPrepareTheAssembly,
      "Instrument.ShouldBeAbleToPrepareTheAssembly"
      Tests.Instrument.ShouldGetTrackingStyleIfSet,
      "Instrument.ShouldGetTrackingStyleIfSet"
      Tests.Instrument.ShouldGetNewFilePathFromPreparedAssembly,
      "Instrument.ShouldGetNewFilePathFromPreparedAssembly"
      Tests.Instrument.ShouldHandleNullConstantsOK,
      "Instrument.ShouldHandleNullConstantsOK"
      Tests.Instrument.ShouldRescopeMonoMethodOK, "Instrument.ShouldRescopeMonoMethodOK"
      Tests.Instrument.ShouldWriteMonoAssemblyOK, "Instrument.ShouldWriteMonoAssemblyOK"
      Tests.Instrument.ShouldGetVisitFromWrittenAssembly,
      "Instrument.ShouldGetVisitFromWrittenAssembly"
      //Tests.Instrument.ShouldUpdateHandlerOK([<Range(0, 31)>] selection)
      Tests.Instrument.ShouldSubstituteInstructionOperand,
      "Instrument.ShouldSubstituteInstructionOperand"
      Tests.Instrument.ShouldNotSubstituteDifferentInstructionOperand,
      "Instrument.ShouldNotSubstituteDifferentInstructionOperand"
      Tests.Instrument.ShouldSubstituteIntoInstructionOperandArray,
      "Instrument.ShouldSubstituteIntoInstructionOperandArray"
      Tests.Instrument.ShouldNotSubstituteOutsideInstructionOperandArray,
      "Instrument.ShouldNotSubstituteOutsideInstructionOperandArray"
      Tests.Instrument.ShouldNotSubstituteOtherOperand,
      "Instrument.ShouldNotSubstituteOtherOperand"
      Tests.Instrument.ShouldBeAbleToTrackAMethod, "Instrument.ShouldBeAbleToTrackAMethod"
      Tests.Instrument.ShouldBeAbleToTrackAMethodWithTailCalls,
      "Instrument.ShouldBeAbleToTrackAMethodWithTailCalls"
      Tests.Instrument.ShouldBeAbleToTrackAMethodWithNonVoidReturn,
      "Instrument.ShouldBeAbleToTrackAMethodWithNonVoidReturn"
      Tests.Instrument.ShouldBeAbleToTrackAnAsyncMethod,
      "Instrument.ShouldBeAbleToTrackAnAsyncMethod"
      Tests.Instrument.ShouldBeAbleToTrackAnFSAsyncMethod,
      "Instrument.ShouldBeAbleToTrackAnFSAsyncMethod"
      Tests.Instrument.ShouldBeAbleToTrackAnFSTaskMethod,
      "Instrument.ShouldBeAbleToTrackAnFSTaskMethod"
      Tests.Instrument.ShouldBeAbleToInstrumentASwitchForNCover,
      "Instrument.ShouldBeAbleToInstrumentASwitchForNCover"
      Tests.Instrument.ShouldNotChangeAnUntrackedMethod,
      "Instrument.ShouldNotChangeAnUntrackedMethod"
      Tests.Instrument.SwitchBranchesShouldInstrumentByPushingDown,
      "Instrument.SwitchBranchesShouldInstrumentByPushingDown"
      Tests.Instrument.PseudoSwitchVisibleBranchesShouldSkipNonRepresentativeCases,
      "Instrument.PseudoSwitchVisibleBranchesShouldSkipNonRepresentativeCases"
      Tests.Instrument.SimpleBranchShouldInstrumentByPushingDown,
      "Instrument.SimpleBranchShouldInstrumentByPushingDown"
      Tests.Instrument.StartShouldLoadRecordingAssembly,
      "Instrument.StartShouldLoadRecordingAssembly"
      Tests.Instrument.TypeShouldNotChangeState, "Instrument.TypeShouldNotChangeState"
      Tests.Instrument.ExcludedMethodShouldNotChangeState,
      "Instrument.ExcludedMethodShouldNotChangeState"
      Tests.Instrument.IncludedMethodShouldChangeState,
      "Instrument.IncludedMethodShouldChangeState"
      Tests.Instrument.ExcludedAfterMethodShouldNotChangeState,
      "Instrument.ExcludedAfterMethodShouldNotChangeState"
      Tests.Instrument.IncludedAfterMethodShouldRewriteMethod,
      "Instrument.IncludedAfterMethodShouldRewriteMethod"
      Tests.Instrument.NoStrongNameShouldUpdateVisibleTo,
      "Instrument.NoStrongNameShouldUpdateVisibleTo"
      Tests.Instrument.NewStrongNameShouldUpdateVisibleTo,
      "Instrument.NewStrongNameShouldUpdateVisibleTo"
      Tests.Instrument.UpdateStrongReferencesShouldChangeSigningKeyWherePossible,
      "Instrument.UpdateStrongReferencesShouldChangeSigningKeyWherePossible"
      Tests.Instrument.UpdateStrongReferencesShouldChangeSigningKeyWherePossible2,
      "Instrument.UpdateStrongReferencesShouldChangeSigningKeyWherePossible2"
      Tests.Instrument.UpdateStrongReferencesShouldRemoveSigningKeyIfRequired,
      "Instrument.UpdateStrongReferencesShouldRemoveSigningKeyIfRequired"
      Tests.Instrument.UpdateStrongReferencesShouldNotAddASigningKey,
      "Instrument.UpdateStrongReferencesShouldNotAddASigningKey"
      Tests.Instrument.UpdateStrongReferencesShouldTrackReferences,
      "Instrument.UpdateStrongReferencesShouldTrackReferences"
      Tests.Instrument.UpdateStrongReferencesShouldTrackReferencesEvenFakes,
      "Instrument.UpdateStrongReferencesShouldTrackReferencesEvenFakes"
      Tests.Instrument.ExcludedAssemblyRefsAreNotUpdated,
      "Instrument.ExcludedAssemblyRefsAreNotUpdated"
      Tests.Instrument.IncludedAssemblyRefsAreUpdated,
      "Instrument.IncludedAssemblyRefsAreUpdated"
      Tests.Instrument.ExcludedModuleJustRecordsMVid,
      "Instrument.ExcludedModuleJustRecordsMVid"
      Tests.Instrument.ExcludedModuleJustRecordsNameForJson,
      "Instrument.ExcludedModuleJustRecordsNameForJson"
      Tests.Instrument.ExcludedModuleJustRecordsHashForOpenCover,
      "Instrument.ExcludedModuleJustRecordsHashForOpenCover"
      Tests.Instrument.IncludedModuleEnsuresRecorder,
      "Instrument.IncludedModuleEnsuresRecorder"
      Tests.Instrument.ExcludedMethodPointIsPassThrough,
      "Instrument.ExcludedMethodPointIsPassThrough"
      Tests.Instrument.IncludedMethodPointInsertsVisit,
      "Instrument.IncludedMethodPointInsertsVisit"
      Tests.Instrument.IncludedModuleDoesNotChangeRecorderJustTheReference,
      "Instrument.IncludedModuleDoesNotChangeRecorderJustTheReference"
      Tests.Instrument.AfterModuleShouldNotChangeState,
      "Instrument.AfterModuleShouldNotChangeState"
      Tests.Instrument.JSONInjectionTransformsSimpleFileAsExpected,
      "Instrument.JSONInjectionTransformsSimpleFileAsExpected"
      Tests.Instrument.JSONInjectionTransformsStandaloneFileAsExpected,
      "Instrument.JSONInjectionTransformsStandaloneFileAsExpected"
      Tests.Instrument.JSONInjectionTransformsDependencyFileAsExpected,
      "Instrument.JSONInjectionTransformsDependencyFileAsExpected"
      Tests.Instrument.JSONInjectionIsIdempotent, "Instrument.JSONInjectionIsIdempotent"
      Tests.Instrument.NonFinishShouldDisposeRecordingAssembly,
      "Instrument.NonFinishShouldDisposeRecordingAssembly"
      Tests.Instrument.NonFinishShouldDisposeThreadingAssembly,
      "Instrument.NonFinishShouldDisposeThreadingAssembly"
      Tests.Instrument.NonFinishShouldNotDisposeNullRecordingAssembly,
      "Instrument.NonFinishShouldNotDisposeNullRecordingAssembly"
      Tests.Instrument.FinishShouldLeaveRecordingAssembly,
      "Instrument.FinishShouldLeaveRecordingAssembly"
      Tests.Instrument.AfterAssemblyCommitsThatAssembly,
      "Instrument.AfterAssemblyCommitsThatAssembly"
      Tests.Instrument.AfterAssemblyCommitsThatAssemblyForMono,
      "Instrument.AfterAssemblyCommitsThatAssemblyForMono"
      Tests.Instrument.FinishCommitsTheRecordingAssembly,
      "Instrument.FinishCommitsTheRecordingAssembly"
      Tests.Instrument.FinishCommitsTheAsyncRecordingAssembly,
      "Instrument.FinishCommitsTheAsyncRecordingAssembly"

      // Json.fs
      Tests.Json.NCoverShouldGeneratePlausibleJson,
      "Json.NCoverShouldGeneratePlausibleJson"
      Tests.Json.OpenCoverShouldGeneratePlausibleJson,
      "Json.OpenCoverShouldGeneratePlausibleJson"
      // Runner.fs
      Tests.Runner.ShouldFailXmlDataForNativeJson, "Runner.ShouldFailXmlDataForNativeJson"
      Tests.Runner.MaxTimeFirst, "Runner.MaxTimeFirst"
      Tests.Runner.MaxTimeLast, "Runner.MaxTimeLast"
      Tests.Runner.MinTimeFirst, "Runner.MinTimeFirst"
      Tests.Runner.MinTimeLast, "Runner.MinTimeLast"
      Tests.Runner.JunkUspidGivesNegativeIndex, "Runner.JunkUspidGivesNegativeIndex"
      Tests.Runner.RealIdShouldIncrementCount, "Runner.RealIdShouldIncrementCount"
      Tests.Runner.RealIdShouldIncrementList, "Runner.RealIdShouldIncrementList"
      Tests.Runner.DistinctIdShouldBeDistinct, "Runner.DistinctIdShouldBeDistinct"
      Tests.Runner.DistinctLineShouldBeDistinct, "Runner.DistinctLineShouldBeDistinct"
      Tests.Runner.RepeatVisitsShouldIncrementCount,
      "Runner.RepeatVisitsShouldIncrementCount"
      Tests.Runner.RepeatVisitsShouldIncrementTotal,
      "Runner.RepeatVisitsShouldIncrementTotal"
      Tests.Runner.KnownModuleWithPayloadMakesExpectedChangeInOpenCover,
      "Runner.KnownModuleWithPayloadMakesExpectedChangeInOpenCover"
      Tests.Runner.DivertedWriteLeavesExpectedTraces,
      "Runner.DivertedWriteLeavesExpectedTraces"
      Tests.Runner.DivertedWriteJsonLeavesExpectedTraces,
      "Runner.DivertedWriteJsonLeavesExpectedTraces"
      Tests.Runner.DivertedZipWriteLeavesExpectedTraces,
      "Runner.DivertedZipWriteLeavesExpectedTraces"
      Tests.Runner.DivertedZipWriteJsonLeavesExpectedTraces,
      "Runner.DivertedZipWriteJsonLeavesExpectedTraces"
      Tests.Runner.UsageIsAsExpected, "Runner.UsageIsAsExpected"
      Tests.Runner.ShouldLaunchWithExpectedOutput,
      "Runner.ShouldLaunchWithExpectedOutput"
      Tests.Runner.ShouldHaveExpectedOptions, "Runner.ShouldHaveExpectedOptions"
      Tests.Runner.ParsingJunkIsAnError, "Runner.ParsingJunkIsAnError"
      Tests.Runner.ParsingJunkAfterSeparatorIsExpected,
      "Runner.ParsingJunkAfterSeparatorIsExpected"
      Tests.Runner.ParsingHelpGivesHelp, "Runner.ParsingHelpGivesHelp"
      Tests.Runner.ParsingErrorHelpGivesHelp, "Runner.ParsingErrorHelpGivesHelp"
      Tests.Runner.ParsingExeGivesExe, "Runner.ParsingExeGivesExe"
      Tests.Runner.ParsingMultipleExeGivesFailure, "Runner.ParsingMultipleExeGivesFailure"
      Tests.Runner.ParsingNoExeGivesFailure, "Runner.ParsingNoExeGivesFailure"
      Tests.Runner.ParsingWorkerGivesWorker, "Runner.ParsingWorkerGivesWorker"
      Tests.Runner.ParsingMultipleWorkerGivesFailure,
      "Runner.ParsingMultipleWorkerGivesFailure"
      Tests.Runner.ParsingBadWorkerGivesFailure, "Runner.ParsingBadWorkerGivesFailure"
      Tests.Runner.ParsingNoWorkerGivesFailure, "Runner.ParsingNoWorkerGivesFailure"
      Tests.Runner.ParsingRecorderGivesRecorder, "Runner.ParsingRecorderGivesRecorder"
      Tests.Runner.ParsingMultipleRecorderGivesFailure,
      "Runner.ParsingMultipleRecorderGivesFailure"
      Tests.Runner.ParsingBadRecorderGivesFailure, "Runner.ParsingBadRecorderGivesFailure"
      Tests.Runner.ParsingNoRecorderGivesFailure, "Runner.ParsingNoRecorderGivesFailure"
      Tests.Runner.ParsingCollectGivesCollect, "Runner.ParsingCollectGivesCollect"
      Tests.Runner.ParsingMultipleCollectGivesFailure,
      "Runner.ParsingMultipleCollectGivesFailure"
      Tests.Runner.ParsingLcovGivesLcov, "Runner.ParsingLcovGivesLcov"
      Tests.Runner.ParsingMultipleLcovGivesFailure,
      "Runner.ParsingMultipleLcovGivesFailure"
      Tests.Runner.ParsingNoLcovGivesFailure, "Runner.ParsingNoLcovGivesFailure"
      Tests.Runner.ParsingThresholdGivesThreshold, "Runner.ParsingThresholdGivesThreshold"
      Tests.Runner.ParsingTopThresholdGivesThreshold,
      "Runner.ParsingTopThresholdGivesThreshold"
      Tests.Runner.ParsingLowThresholdGivesThreshold,
      "Runner.ParsingLowThresholdGivesThreshold"
      Tests.Runner.ParsingComplexThresholdGivesThreshold,
      "Runner.ParsingComplexThresholdGivesThreshold"
      Tests.Runner.ParsingMultipleThresholdGivesFailure,
      "Runner.ParsingMultipleThresholdGivesFailure"
      Tests.Runner.ParsingBadThresholdGivesFailure,
      "Runner.ParsingBadThresholdGivesFailure"
      Tests.Runner.ParsingBadThreshold2GivesFailure,
      "Runner.ParsingBadThreshold2GivesFailure"
      Tests.Runner.ParsingBadThreshold3GivesFailure,
      "Runner.ParsingBadThreshold3GivesFailure"
      Tests.Runner.ParsingBadThreshold4GivesFailure,
      "Runner.ParsingBadThreshold4GivesFailure"
      Tests.Runner.ParsingBadThreshold5GivesFailure,
      "Runner.ParsingBadThreshold5GivesFailure"
      Tests.Runner.ParsingBadThreshold6GivesFailure,
      "Runner.ParsingBadThreshold6GivesFailure"
      Tests.Runner.ParsingBadThreshold7GivesFailure,
      "Runner.ParsingBadThreshold7GivesFailure"
      Tests.Runner.ParsingEmptyThresholdGivesFailure,
      "Runner.ParsingEmptyThresholdGivesFailure"
      Tests.Runner.ParsingNoThresholdGivesFailure, "Runner.ParsingNoThresholdGivesFailure"
      Tests.Runner.ParsingCoberturaGivesCobertura, "Runner.ParsingCoberturaGivesCobertura"
      Tests.Runner.ParsingMultipleCoberturaGivesFailure,
      "Runner.ParsingMultipleCoberturaGivesFailure"
      Tests.Runner.ParsingNoCoberturaGivesFailure, "Runner.ParsingNoCoberturaGivesFailure"
      Tests.Runner.ParsingNoPackagesGivesFailure, "Runner.ParsingPackagesGivesPackages"
      Tests.Runner.ParsingPackagesGivesPackages, "Runner.ParsingNoPackagesGivesFailure"
      Tests.Runner.ParsingOutputGivesOutput, "Runner.ParsingOutputGivesOutput"
      Tests.Runner.ParsingMultipleOutputGivesFailure,
      "Runner.ParsingMultipleOutputGivesFailure"
      Tests.Runner.ParsingNoOutputGivesFailure, "Runner.ParsingNoOutputGivesFailure"
      Tests.Runner.ParsingDropGivesDrop, "Runner.ParsingDropGivesDrop"
      Tests.Runner.ParsingMultipleDropGivesFailure,
      "Runner.ParsingMultipleDropGivesFailure"
      Tests.Runner.ParsingTCString, "Runner.ParsingTCString"
      Tests.Runner.ParsingTCGivesTC, "Runner.ParsingTCGivesTC"
      Tests.Runner.ParsingMultipleTCGivesFailure, "Runner.ParsingMultipleTCGivesFailure"
      Tests.Runner.ParsingBadTCGivesFailure, "Runner.ParsingBadTCGivesFailure"
      Tests.Runner.ParsingQuietWorks, "Runner.ParsingQuietWorks"
      Tests.Runner.ParsingMultiQuietWorks, "Runner.ParsingMultiQuietWorks"
      Tests.Runner.ParsingBatchMultiQuietWorks, "Runner.ParsingBatchMultiQuietWorks"
      Tests.Runner.ParsingVerboseWorks, "Runner.ParsingVerboseWorks"
      Tests.Runner.ParsingMixedQuietWorks, "Runner.ParsingMixedQuietWorks"
      Tests.Runner.ShouldRequireExe, "Runner.ShouldRequireExe"
      Tests.Runner.ShouldAcceptExe, "Runner.ShouldAcceptExe"
      Tests.Runner.ShouldRequireCollectIfNotExe, "Runner.ShouldRequireCollectIfNotExe"
      Tests.Runner.ShouldRejectExeIfCollect, "Runner.ShouldRejectExeIfCollect"
      Tests.Runner.ShouldRequireWorker, "Runner.ShouldRequireWorker"
      Tests.Runner.ShouldAcceptWorker, "Runner.ShouldAcceptWorker"
      Tests.Runner.ShouldRequireRecorder, "Runner.ShouldRequireRecorder"
      Tests.Runner.ShouldRequireRecorderDll, "Runner.ShouldRequireRecorderDll"
      Tests.Runner.ShouldAcceptRecorder, "Runner.ShouldAcceptRecorder"
      Tests.Runner.ShouldHandleReturnCodes, "Runner.ShouldHandleReturnCodes"
      Tests.Runner.ShouldProcessTrailingArguments, "Runner.ShouldProcessTrailingArguments"
      Tests.Runner.ShouldNoOp, "Runner.ShouldNoOp"
      Tests.Runner.ErrorResponseIsAsExpected, "Runner.ErrorResponseIsAsExpected"
      Tests.Runner.ShouldGetStringConstants, "Runner.ShouldGetStringConstants"
      Tests.Runner.ShouldProcessPayload, "Runner.ShouldProcessPayload"
      Tests.Runner.WriteJsonLeavesExpectedTraces, "Runner.WriteJsonLeavesExpectedTraces"
      Tests.Runner.ZipWriteJsonLeavesExpectedTraces,
      "Runner.ZipWriteJsonLeavesExpectedTraces"
      Tests.Runner.NullPayloadShouldReportNothing, "Runner.NullPayloadShouldReportNothing"
      Tests.Runner.WriteLeavesExpectedTraces, "Runner.WriteLeavesExpectedTraces"
      Tests.Runner.ZipWriteLeavesExpectedTraces, "Runner.ZipWriteLeavesExpectedTraces"
      Tests.Runner.ActivePayloadShouldReportAsExpected,
      "Runner.ActivePayloadShouldReportAsExpected"
      Tests.Runner.CollectShouldReportAsExpected, "Runner.CollectShouldReportAsExpected"
      Tests.Runner.JunkPayloadShouldReportAsExpected,
      "Runner.JunkPayloadShouldReportAsExpected"
      Tests.Runner.TrackingPayloadShouldReportAsExpected,
      "Runner.TrackingPayloadShouldReportAsExpected"
      Tests.Runner.PointProcessShouldCaptureTimes, "Runner.PointProcessShouldCaptureTimes"
      Tests.Runner.PostprocessShouldHandleNullCase,
      "Runner.PostprocessShouldHandleNullCase"
      Tests.Runner.PostprocessShouldHandleEntryAndExitTimes,
      "Runner.PostprocessShouldHandleEntryAndExitTimes"
      Tests.Runner.PostprocessShouldRestoreKnownOpenCoverState,
      "Runner.PostprocessShouldRestoreKnownOpenCoverState"
      Tests.Runner.PostprocessShouldRestoreKnownOpenCoverStateFromMono,
      "Runner.PostprocessShouldRestoreKnownOpenCoverStateFromMono"
      Tests.Runner.PostprocessShouldRestoreDegenerateOpenCoverState,
      "Runner.PostprocessShouldRestoreDegenerateOpenCoverState"
      Tests.Runner.PostprocessShouldRestoreBranchOnlyOpenCoverState,
      "Runner.PostprocessShouldRestoreBranchOnlyOpenCoverState"
      Tests.Runner.PostprocessShouldRestoreBranchOnlyOpenCoverStateXDoc,
      "Runner.PostprocessShouldRestoreBranchOnlyOpenCoverStateXDoc"
      Tests.Runner.JunkTokenShouldDefaultZero, "Runner.JunkTokenShouldDefaultZero"
      Tests.Runner.EmptyJsonGeneratesExpectedSummary,
      "Runner.EmptyJsonGeneratesExpectedSummary"
      Tests.Runner.EmptyJsonGeneratesExpectedTCSummary,
      "Runner.EmptyJsonGeneratesExpectedTCSummary"
      Tests.Runner.EmptyJsonGeneratesExpectedSummaries,
      "Runner.EmptyJsonGeneratesExpectedSummaries"
      Tests.Runner.SimpleJsonShouldGeneratePlausibleSummary,
      "Runner.SimpleJsonShouldGeneratePlausibleSummary"
      Tests.Runner.ComplexJsonShouldGeneratePlausibleSummary,
      "Runner.ComplexJsonShouldGeneratePlausibleSummary"
      Tests.Runner.UnknownGeneratesExpectedSummary,
      "Runner.UnknownGeneratesExpectedSummary"
      Tests.Runner.EmptyNCoverGeneratesExpectedSummary,
      "Runner.EmptyNCoverGeneratesExpectedSummary"
      Tests.Runner.EmptyNCoverGeneratesExpectedTCSummary,
      "Runner.EmptyNCoverGeneratesExpectedTCSummary"
      Tests.Runner.EmptyNCoverGeneratesExpectedSummaries,
      "Runner.EmptyNCoverGeneratesExpectedSummaries"
      Tests.Runner.NCoverShouldGeneratePlausibleSummary,
      "Runner.NCoverShouldGeneratePlausibleSummary"
      Tests.Runner.EmptyOpenCoverGeneratesExpectedSummary,
      "Runner.EmptyOpenCoverGeneratesExpectedSummary"
      Tests.Runner.EmptyOpenCoverGeneratesExpectedTCSummary,
      "Runner.EmptyOpenCoverGeneratesExpectedTCSummary"
      Tests.Runner.EmptyOpenCoverGeneratesExpectedSummaries,
      "Runner.EmptyOpenCoverGeneratesExpectedSummaries"
      Tests.Runner.OpenCoverShouldGeneratePlausibleSummary,
      "Runner.OpenCoverShouldGeneratePlausibleSummary"
      Tests.Runner.OpenCoverShouldGeneratePlausiblePartialSummary,
      "Runner.OpenCoverShouldGeneratePlausiblePartialSummary"
      Tests.Runner.DegenerateCasesShouldNotGenerateLcov,
      "Runner.DegenerateCasesShouldNotGenerateLcov"
      Tests.Runner.OpenCoverShouldGeneratePlausibleLcov,
      "Runner.OpenCoverShouldGeneratePlausibleLcov"
      Tests.Runner.OpenCoverWithPartialsShouldGeneratePlausibleLcov,
      "Runner.OpenCoverWithPartialsShouldGeneratePlausibleLcov"
      Tests.Runner.JsonShouldGeneratePlausibleLcov,
      "Runner.JsonShouldGeneratePlausibleLcov"
      Tests.Runner.JsonWithPartialsShouldGeneratePlausibleLcov,
      "Runner.JsonWithPartialsShouldGeneratePlausibleLcov"
      Tests.Runner.JsonWithOverloadsShouldGeneratePlausibleLcov,
      "Runner.JsonWithOverloadsShouldGeneratePlausibleLcov"
      Tests.Runner.NCoverShouldGeneratePlausibleLcov,
      "Runner.NCoverShouldGeneratePlausibleLcov"
      Tests.Runner.NCoverWithOverloadsShouldGeneratePlausibleLcov,
      "Runner.NCoverWithOverloadsShouldGeneratePlausibleLcov"
      Tests.Runner.NCoverWithPartialsShouldGeneratePlausibleLcov,
      "Runner.NCoverWithPartialsShouldGeneratePlausibleLcov"
      Tests.Runner.NCoverShouldGenerateMorePlausibleLcov,
      "Runner.NCoverShouldGenerateMorePlausibleLcov"
      Tests.Runner.NCoverShouldGeneratePlausibleLcovWithMissingFullName,
      "Runner.NCoverShouldGeneratePlausibleLcovWithMissingFullName"
      Tests.Runner.MultiSortDoesItsThing, "Runner.MultiSortDoesItsThing"
      Tests.Runner.JsonShouldGeneratePlausibleXml, "Runner.JsonShouldGeneratePlausibleXml"
      Tests.Runner.JsonWithPartialsShouldGeneratePlausibleXml,
      "Runner.JsonWithPartialsShouldGeneratePlausibleXml"
      Tests.Runner.JsonShouldGeneratePlausibleCobertura,
      "Runner.JsonShouldGeneratePlausibleCobertura"
      Tests.Runner.JsonWithPartialsShouldGeneratePlausibleCobertura,
      "Runner.JsonWithPartialsShouldGeneratePlausibleCobertura"
      Tests.Runner.JsonFromComplexNestingShouldGeneratePlausibleCobertura,
      "Runner.JsonFromComplexNestingShouldGeneratePlausibleCobertura"
      Tests.Runner.NCoverShouldGeneratePlausibleCobertura,
      "Runner.NCoverShouldGeneratePlausibleCobertura"
      Tests.Runner.NCoverWithPartialsShouldGeneratePlausibleCobertura,
      "Runner.NCoverWithPartialsShouldGeneratePlausibleCobertura"
      Tests.Runner.NCoverWithOverloadsShouldGeneratePlausibleCobertura,
      "Runner.NCoverWithOverloadsShouldGeneratePlausibleCobertura"
      Tests.Runner.NCoverShouldGenerateMorePlausibleCobertura,
      "Runner.NCoverShouldGenerateMorePlausibleCobertura"
      Tests.Runner.PathsSplitOK, "Runner.PathsSplitOK"
      Tests.Runner.PathsGroupOK, "Runner.PathsGroupOK"
      Tests.Runner.ExtractSourcesOK, "Runner.ExtractSourcesOK"
      Tests.Runner.DegenerateCasesShouldNotGenerateCobertura,
      "Runner.DegenerateCasesShouldNotGenerateCobertura"
      Tests.Runner.NCoverShouldGeneratePlausibleCoberturaWithMissingFullName,
      "Runner.NCoverShouldGeneratePlausibleCoberturaWithMissingFullName"
      Tests.Runner.OpenCoverShouldGeneratePlausibleCobertura,
      "Runner.OpenCoverShouldGeneratePlausibleCobertura"
      Tests.Runner.OpenCoverWithOverloadsShouldGeneratePlausibleCobertura,
      "Runner.OpenCoverWithOverloadsShouldGeneratePlausibleCobertura"
      Tests.Runner.OpenCoverWithPartialsShouldGeneratePlausibleCobertura,
      "Runner.OpenCoverWithPartialsShouldGeneratePlausibleCobertura"
      Tests.Runner.ThresholdViolationShouldBeReported,
      "Runner.ThresholdViolationShouldBeReported"
      Tests.Runner.TryGetValueHandlesNull, "Runner.TryGetValueHandlesNull"
      Tests.Runner.ShouldDoCoverage, "Runner.ShouldDoCoverage"

      // Arguments.fs
      Tests.Arguments.CollectOptionsCanBeValidated,
      "Arguments.CollectOptionsCanBeValidated"
      Tests.Arguments.TypeSafeEmptyThresholdCanBeValidated,
      "Arguments.TypeSafeEmptyThresholdCanBeValidated"
      Tests.Arguments.TypeSafeCollectOptionsCanBeValidated,
      "Arguments.TypeSafeCollectOptionsCanBeValidated"
      Tests.Arguments.TypeSafeCollectSummaryCanBeValidated,
      "Arguments.TypeSafeCollectSummaryCanBeValidated"
      Tests.Arguments.CollectOptionsCanBeValidatedWithErrors,
      "Arguments.CollectOptionsCanBeValidatedWithErrors"
      Tests.Arguments.TypeSafeCollectOptionsCanBeValidatedWithErrors,
      "Arguments.TypeSafeCollectOptionsCanBeValidatedWithErrors"
      Tests.Arguments.CollectOptionsCanBePositivelyValidatedWithErrors,
      "Arguments.CollectOptionsCanBePositivelyValidatedWithErrors"
      Tests.Arguments.TypeSafeCollectOptionsCanBePositivelyValidatedWithErrors,
      "Arguments.TypeSafeCollectOptionsCanBePositivelyValidatedWithErrors"
      Tests.Arguments.PrepareOptionsCanBeValidated,
      "Arguments.PrepareOptionsCanBeValidated"
      Tests.Arguments.TypeSafePrepareOptionsCanBeValidated,
      "Arguments.TypeSafePrepareOptionsCanBeValidated"
      Tests.Arguments.TypeSafePrepareOptionsCanBeValidatedAgain,
      "Arguments.TypeSafePrepareOptionsCanBeValidatedAgain"
      Tests.Arguments.PrepareOptionsStrongNamesCanBeValidated,
      "Arguments.PrepareOptionsStrongNamesCanBeValidated"
      Tests.Arguments.TypeSafePrepareOptionsStrongNamesCanBeValidated,
      "Arguments.TypeSafePrepareOptionsStrongNamesCanBeValidated"
      Tests.Arguments.PrepareOptionsCanBeValidatedWithNulls,
      "Arguments.PrepareOptionsCanBeValidatedWithNulls"
      Tests.Arguments.PrepareOptionsCanBeValidatedAndDetectInconsistency,
      "Arguments.PrepareOptionsCanBeValidatedAndDetectInconsistency"
      Tests.Arguments.TypeSafePrepareStaticCanBeValidated,
      "Arguments.TypeSafePrepareStaticCanBeValidated"
      Tests.Arguments.TypeSafePrepareOptionsCanBeValidatedAndDetectInconsistency,
      "Arguments.TypeSafePrepareOptionsCanBeValidatedAndDetectInconsistency"
      Tests.Arguments.PrepareOptionsCanBeValidatedWithErrors,
      "Arguments.PrepareOptionsCanBeValidatedWithErrors"
      Tests.Arguments.NullListsAreEmpty, "Arguments.NullListsAreEmpty"

      // Main.fs
      Tests.Main.ShouldHaveExpectedOptions, "Main.ShouldHaveExpectedOptions"
      Tests.Main.ParsingJunkIsAnError, "Main.ParsingJunkIsAnError"
      Tests.Main.ParsingJunkBeforeSeparatorIsAnError,
      "Main.ParsingJunkBeforeSeparatorIsAnError"
      Tests.Main.ParsingJunkAfterSeparatorIsExpected,
      "Main.ParsingJunkAfterSeparatorIsExpected"
      Tests.Main.ParsingHelpGivesHelp, "Main.ParsingHelpGivesHelp"
      Tests.Main.ParsingErrorHelpGivesHelp, "Main.ParsingErrorHelpGivesHelp"
      Tests.Main.ParsingAttributesGivesAttributes, "Main.ParsingAttributesGivesAttributes"
      Tests.Main.ParsingTopLevelGivesTopLevel, "Main.ParsingTopLevelGivesTopLevel"
      Tests.Main.ParsingMethodsGivesMethods, "Main.ParsingMethodsGivesMethods"
      Tests.Main.ParsingTypesGivesTypes, "Main.ParsingTypesGivesTypes"
      Tests.Main.ParsingAssembliesGivesAssemblies, "Main.ParsingAssembliesGivesAssemblies"
      Tests.Main.ParsingEscapeCasesWork, "Main.ParsingEscapeCasesWork"
      Tests.Main.ParsingModulesGivesModules, "Main.ParsingModulesGivesModules"
      Tests.Main.ParsingFilesGivesFiles, "Main.ParsingFilesGivesFiles"
      Tests.Main.ParsingPathsGivesPaths, "Main.ParsingPathsGivesPaths"
      Tests.Main.ParsingReportGivesReport, "Main.ParsingReportGivesReport"
      Tests.Main.ParsingMultipleReportGivesFailure,
      "Main.ParsingMultipleReportGivesFailure"
      Tests.Main.ParsingBadReportGivesFailure, "Main.ParsingBadReportGivesFailure"
      Tests.Main.ParsingNoReportGivesFailure, "Main.ParsingNoReportGivesFailure"
      Tests.Main.ParsingEmptyReportGivesFailure, "Main.ParsingEmptyReportGivesFailure"
      Tests.Main.ParsingInputGivesInput, "Main.ParsingInputGivesInput"
      Tests.Main.ParsingMultipleInputIsOKToo, "Main.ParsingMultipleInputIsOKToo"
      Tests.Main.ParsingDuplicateInputGivesFailure,
      "Main.ParsingDuplicateInputGivesFailure"
      Tests.Main.ParsingBadInputGivesFailure, "Main.ParsingBadInputGivesFailure"
      Tests.Main.ParsingNoInputGivesFailure, "Main.ParsingNoInputGivesFailure"
      Tests.Main.ParsingOutputGivesOutput, "Main.ParsingOutputGivesOutput"
      Tests.Main.ParsingDuplicateOutputGivesFailure,
      "Main.ParsingDuplicateOutputGivesFailure"
      Tests.Main.ParsingMultipleOutputIsOK, "Main.ParsingMultipleOutputIsOK"
      Tests.Main.ParsingBadOutputGivesFailure, "Main.ParsingBadOutputGivesFailure"
      Tests.Main.ParsingNoOutputGivesFailure, "Main.ParsingNoOutputGivesFailure"
      Tests.Main.ParsingEmptyOutputGivesFailure, "Main.ParsingEmptyOutputGivesFailure"
      Tests.Main.ParsingSymbolGivesSymbol, "Main.ParsingSymbolGivesSymbol"
      Tests.Main.ParsingMultipleSymbolGivesOK, "Main.ParsingMultipleSymbolGivesOK"
      Tests.Main.ParsingBadSymbolGivesFailure, "Main.ParsingBadSymbolGivesFailure"
      Tests.Main.ParsingNoSymbolGivesFailure, "Main.ParsingNoSymbolGivesFailure"
      Tests.Main.ParsingMultipleDependencyIsOk, "Main.ParsingMultipleDependencyIsOk"
      Tests.Main.ParsingBadDependencyGivesFailure, "Main.ParsingBadDependencyGivesFailure"
      Tests.Main.ParsingNonDependencyGivesFailure, "Main.ParsingNonDependencyGivesFailure"
      Tests.Main.ParsingStrongNameGivesStrongName, "Main.ParsingStrongNameGivesStrongName"
      Tests.Main.ParsingMultipleStrongNameGivesFailure,
      "Main.ParsingMultipleStrongNameGivesFailure"
      Tests.Main.ParsingBadStrongNameGivesFailure, "Main.ParsingBadStrongNameGivesFailure"
      Tests.Main.ParsingNonStrongNameGivesFailure, "Main.ParsingNonStrongNameGivesFailure"
      Tests.Main.ParsingNoStrongNameGivesFailure, "Main.ParsingNoStrongNameGivesFailure"
      Tests.Main.ParsingMultipleAltStrongNameIsOk, "Main.ParsingMultipleAltStrongNameIsOk"
      Tests.Main.ParsingNoAltStrongNameGivesFailure,
      "Main.ParsingNoAltStrongNameGivesFailure"
      Tests.Main.ParsingBadAltStrongNameGivesFailure,
      "Main.ParsingBadAltStrongNameGivesFailure"
      Tests.Main.ParsingNonAltsStrongNameGivesFailure,
      "Main.ParsingNonAltsStrongNameGivesFailure"
      Tests.Main.ParsingLocalGivesLocal, "Main.ParsingLocalGivesLocal"
      Tests.Main.ParsingMultipleLocalGivesFailure, "Main.ParsingMultipleLocalGivesFailure"
      Tests.Main.ParsingVisibleGivesVisible, "Main.ParsingVisibleGivesVisible"
      Tests.Main.ParsingMultipleVisibleGivesFailure,
      "Main.ParsingMultipleVisibleGivesFailure"
      Tests.Main.ParsingTimeGivesTime, "Main.ParsingTimeGivesTime"
      Tests.Main.ParsingOnlyArabicNumeralsNotThatSortofArabicNumeralsGivesTime,
      "Main.ParsingOnlyArabicNumeralsNotThatSortofArabicNumeralsGivesTime"
      Tests.Main.ParsingMultipleTimesGivesFailure, "Main.ParsingMultipleTimesGivesFailure"
      Tests.Main.ParsingTimeAndNamesGivesOK, "Main.ParsingTimeAndNamesGivesOK"
      Tests.Main.ParsingBadTimeGivesNoOp, "Main.ParsingBadTimeGivesNoOp"
      Tests.Main.ParsingNonTimeGivesFailure, "Main.ParsingNonTimeGivesFailure"
      Tests.Main.ParsingNoTimeGivesFailure, "Main.ParsingNoTimeGivesFailure"
      Tests.Main.ParsingJsonFormatGivesJson, "Main.ParsingJsonFormatGivesJson"
      Tests.Main.ParsingNCoverFormatGivesNCover, "Main.ParsingNCoverFormatGivesNCover"
      Tests.Main.ParsingOpenCoverFormatGivesOpenCover,
      "Main.ParsingOpenCoverFormatGivesOpenCover"
      Tests.Main.ParsingMultipleReportFormatGivesFailure,
      "Main.ParsingMultipleReportFormatGivesFailure"
      Tests.Main.ParsingInPlaceGivesInPlace, "Main.ParsingInPlaceGivesInPlace"
      Tests.Main.ParsingMultipleInPlaceGivesFailure,
      "Main.ParsingMultipleInPlaceGivesFailure"
      Tests.Main.ParsingSaveGivesSave, "Main.ParsingSaveGivesSave"
      Tests.Main.ParsingMultipleSaveGivesFailure, "Main.ParsingMultipleSaveGivesFailure"
      Tests.Main.ParsingAllGivesAll, "Main.ParsingAllGivesAll"
      Tests.Main.ParsingMultipleAllGivesFailure, "Main.ParsingMultipleAllGivesFailure"
      Tests.Main.ParsingLineCoverGivesLineCover, "Main.ParsingLineCoverGivesLineCover"
      Tests.Main.OpenCoverIsCompatibleWithLineCover,
      "Main.OpenCoverIsCompatibleWithLineCover"
      Tests.Main.LineCoverIsCompatibleWithOpenCover,
      "Main.LineCoverIsCompatibleWithOpenCover"
      Tests.Main.ParsingMultipleLineCoverGivesFailure,
      "Main.ParsingMultipleLineCoverGivesFailure"
      Tests.Main.LineCoverIsNotCompatibleWithBranchCover,
      "Main.LineCoverIsNotCompatibleWithBranchCover"
      Tests.Main.ParsingBranchCoverGivesBranchCover,
      "Main.ParsingBranchCoverGivesBranchCover"
      Tests.Main.OpenCoverIsCompatibleWithBranchCover,
      "Main.OpenCoverIsCompatibleWithBranchCover"
      Tests.Main.BranchCoverIsCompatibleWithOpenCover,
      "Main.BranchCoverIsCompatibleWithOpenCover"
      Tests.Main.ParsingMultipleBranchCoverGivesFailure,
      "Main.ParsingMultipleBranchCoverGivesFailure"
      Tests.Main.BranchCoverIsNotCompatibleWithLineCover,
      "Main.BranchCoverIsNotCompatibleWithLineCover"
      Tests.Main.ParsingDropGivesDrop, "Main.ParsingDropGivesDrop"
      Tests.Main.ParsingMultipleDropGivesFailure, "Main.ParsingMultipleDropGivesFailure"
      Tests.Main.ParsingEagerWorks, "Main.ParsingEagerWorks"
      Tests.Main.ParsingMultipleEagerGivesFailure, "Main.ParsingMultipleEagerGivesFailure"
      Tests.Main.ParsingStaticGivesStatic, "Main.ParsingStaticGivesStatic"
      Tests.Main.ParsingStaticPlusGivesStatic, "Main.ParsingStaticPlusGivesStatic"
      Tests.Main.ParsingStaticPlusPlusGivesStaticPlus,
      "Main.ParsingStaticPlusPlusGivesStaticPlus"
      Tests.Main.ParsingStaticMinusGivesNoStatic, "Main.ParsingStaticMinusGivesNoStatic"
      Tests.Main.ParsingMultipleStaticGivesFailure,
      "Main.ParsingMultipleStaticGivesFailure"
      Tests.Main.ParsingJunkStaticGivesFailure, "Main.ParsingJunkStaticGivesFailure"
      Tests.Main.ParsingQuietWorks, "Main.ParsingQuietWorks"
      Tests.Main.ParsingMultiQuietWorks, "Main.ParsingMultiQuietWorks"
      Tests.Main.ParsingBatchMultiQuietWorks, "Main.ParsingBatchMultiQuietWorks"
      Tests.Main.ParsingVerboseWorks, "Main.ParsingVerboseWorks"
      Tests.Main.ParsingMixedQuietWorks, "Main.ParsingMixedQuietWorks"
      Tests.Main.OutputLeftPassesThrough, "Main.OutputLeftPassesThrough"
      Tests.Main.OutputInPlaceFails, "Main.OutputInPlaceFails"
      Tests.Main.PortableFailsOnMultiInputs, "Main.PortableFailsOnMultiInputs"
      Tests.Main.ScreeningFilesShouldRejectTheInstrumentedOnes,
      "Main.ScreeningFilesShouldRejectTheInstrumentedOnes"
      Tests.Main.OutputToNewPlaceIsOK, "Main.OutputToNewPlaceIsOK"
      Tests.Main.OutputToReallyNewPlaceIsOK, "Main.OutputToReallyNewPlaceIsOK"
      Tests.Main.InPlaceToExistingPlaceFails, "Main.InPlaceToExistingPlaceFails"
      Tests.Main.InPlaceOperationIsAsExpected, "Main.InPlaceOperationIsAsExpected"
      Tests.Main.ImageLoadResilientPassesThrough, "Main.ImageLoadResilientPassesThrough"
      Tests.Main.ResilientHandlesIOException, "Main.ResilientHandlesIOException"
      Tests.Main.ResilientHandlesSymbolReadException,
      "Main.ResilientHandlesSymbolReadException"
      Tests.Main.ResilientHandlesBadImageFormatException,
      "Main.ResilientHandlesBadImageFormatException"
      Tests.Main.ResilientHandlesArgumentException,
      "Main.ResilientHandlesArgumentException"
      Tests.Main.FolderNestingIsDetectedCorrectly, "Main.FolderNestingIsDetectedCorrectly"
      Tests.Main.PreparingNewPlaceShouldCopyEverything,
      "Main.PreparingNewPlaceShouldCopyEverything"
      Tests.Main.ShouldProcessTrailingArguments, "Main.ShouldProcessTrailingArguments"
      Tests.Main.StoresAsExpected, "Main.StoresAsExpected"
      Tests.Main.ImportModuleIsAsExpected, "Main.ImportModuleIsAsExpected"
      Tests.Main.VersionIsAsExpected, "Main.VersionIsAsExpected"
      Tests.Main.TargetsPathIsAsExpected, "Main.TargetsPathIsAsExpected"
      Tests.Main.UsageIsAsExpected, "Main.UsageIsAsExpected"
      Tests.Main.ErrorResponseIsAsExpected, "Main.ErrorResponseIsAsExpected"
      Tests.Main.ADotNetDryRunLooksAsExpected, "Main.ADotNetDryRunLooksAsExpected"
      Tests.Main.ADryRunLooksAsExpected, "Main.ADryRunLooksAsExpected"
      Tests.Main.ValidateAssemblyOption, "Main.ValidateAssemblyOption"

      // Tasks.fs
      Tests.Tasks.LoggingCanBeExercised, "Tasks.LoggingCanBeExercised"
      Tests.Tasks.EmptyInstrumentIsJustTheDefaults,
      "Tasks.EmptyInstrumentIsJustTheDefaults"
      Tests.Tasks.InstrumentLevelsCanBeSet, "Tasks.InstrumentLevelsCanBeSet"
      Tests.Tasks.NonDefaultInstrumentObsoleteIsOK,
      "Tasks.NonDefaultInstrumentObsoleteIsOK"
      Tests.Tasks.NonDefaultInstrumentIsOK, "Tasks.NonDefaultInstrumentIsOK"
      Tests.Tasks.EmptyCollectIsJustTheDefaults, "Tasks.EmptyCollectIsJustTheDefaults"
      Tests.Tasks.CollectLevelsCanBeSet, "Tasks.CollectLevelsCanBeSet"
      Tests.Tasks.CollectWithExeIsNotCollecting, "Tasks.CollectWithExeIsNotCollecting"
      Tests.Tasks.EmptyPowerShellIsJustTheDefaults,
      "Tasks.EmptyPowerShellIsJustTheDefaults"
      Tests.Tasks.EmptyVersionIsJustTheDefaults, "Tasks.EmptyVersionIsJustTheDefaults"
      Tests.Tasks.EchoWorks, "Tasks.EchoWorks"
      Tests.Tasks.EchoFallsSilent, "Tasks.EchoFallsSilent"
      Tests.Tasks.RunSettingsFailsIfCollectorNotFound,
      "Tasks.RunSettingsFailsIfCollectorNotFound"
      Tests.Tasks.RunSettingsWorksIfOK, "Tasks.RunSettingsWorksIfOK"
      Tests.Tasks.RunSettingsExtendsOK, "Tasks.RunSettingsExtendsOK"
      Tests.Tasks.RunSettingsRecoversOK, "Tasks.RunSettingsRecoversOK"
      Tests.Tasks.RunSettingsThrowsIfUninitialized,
      "Tasks.RunSettingsThrowsIfUninitialized"
      Tests.Tasks.ContingentCopyTest, "Tasks.ContingentCopyTest"
      Tests.Tasks.RetryDeleteTest, "Tasks.RetryDeleteTest" ]

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