namespace Tests
#if !NET472
//does not build using the framework version

open Expecto

module ExpectoMain =

  let specials =
    { 0..31 }
    |> Seq.map (fun i ->
      testCase (sprintf "Tests2.ShouldUpdateHandlerOK(%d)" i)
      <| (fun () ->
        lock ExpectoTestCommon.sync (fun () ->
          AltCoverRunnerTests.mainInit ()
          Tests.AltCoverTests2.ShouldUpdateHandlerOK i)))
    |> Seq.toList

  let consistencyCheck () =
    ExpectoTestManifest.consistencyCheck [ "Tests.AltCoverTests2::ShouldUpdateHandlerOK" ]

  [<Tests>]
  let tests =
    ExpectoTestCommon.makeTests
      "AltCoverTests"
      consistencyCheck
      ExpectoTestManifest.regular
      specials
      (fun () ->
        AltCoverRunnerTests.mainInit ()
        AltCoverRunnerTests.runnerInit ())

module UnitTestStub =
  [<EntryPoint; System.Runtime.CompilerServices.CompilerGenerated>]
  let unitTestStub argv =

    runTestsWithCLIArgs Seq.empty<CLIArguments> argv ExpectoMain.tests
#endif