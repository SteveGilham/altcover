namespace Tests

open AltCover
open Expecto

module ExpectoMain =

  let specials =
    { 0 .. 31 }
    |> Seq.map (fun i ->
         testCase (sprintf "Tests2.ShouldUpdateHandlerOK(%d)" i) <| (fun () ->
         lock ExpectoTestCommon.sync (fun () ->
           AltCoverRunnerTests.mainInit()
           Tests.AltCoverTests2.ShouldUpdateHandlerOK i)))
    |> Seq.toList

  let consistencyCheck() =
    ExpectoTestManifest.consistencyCheck ["Tests.AltCoverTests2::ShouldUpdateHandlerOK"]

  [<Tests>]
  let tests =
    ExpectoTestCommon.makeTests  "AltCoverTests" consistencyCheck Manifest.regular specials
     (fun () -> AltCoverRunnerTests.mainInit()
                AltCoverRunnerTests.runnerInit())

module UnitTestStub =
  [<EntryPoint; System.Runtime.CompilerServices.CompilerGenerated>]
  let unitTestStub argv =
    let writeResults = TestResults.writeNUnitSummary ("AltCover.Expecto.TestResults.xml", "AltCover.Expecto.Tests")
    let config = defaultConfig.appendSummaryHandler writeResults
    runTestsWithArgs config argv ExpectoMain.tests