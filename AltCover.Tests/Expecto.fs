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
       Tests.TestCommonTests.SelfTest, "TestCommonTests.SelfTest" |]
    |> Array.toList

#if !EXPECTO_MAIN
  let consistencyCheck specials =
    ExpectoTestCommon.consistencyCheck (simpleTests ()) specials //["Tests.AltCoverTests2::ShouldUpdateHandlerOK"]
#endif