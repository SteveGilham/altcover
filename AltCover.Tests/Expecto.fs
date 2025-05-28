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