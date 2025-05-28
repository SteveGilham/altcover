namespace Tests

open Expecto

module ExpectoMain =
  let specials = []

  let consistencyCheck () = ExpectoTestManifest.consistencyCheck []

  [<Tests>]
  let tests =
    ExpectoTestCommon.makeTests
      "AltCoverTests"
      consistencyCheck
      Manifest.regular
      specials
      (fun () ->
        AltCoverXTests.mainInit ()
        AltCoverXTests.runnerInit ())

module UnitTestStub =
  [<EntryPoint; System.Runtime.CompilerServices.CompilerGenerated>]
  let unitTestStub argv =

    runTestsWithCLIArgs Seq.empty<CLIArguments> argv ExpectoMain.tests