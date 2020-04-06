namespace AltCover.Expecto.Tests

#if NETCOREAPP3_0

open Expecto
open Mono.Cecil
open Mono.Cecil.Cil
open Mono.Cecil.Rocks
open Swensen.Unquote

module TestMain =
  let sync = System.Object()

  let regular = [
          Tests.FSApiTests.FormatFromCoverletMeetsSpec, "FSApiTests.FormatFromCoverlet"
          Tests.FSApiTests.OpenCoverToLcov, "FSApiTests.OpenCoverToLcov"
          Tests.FSApiTests.OpenCoverToBarChart, "FSApiTests.OpenCoverToBarChart"
          Tests.FSApiTests.OpenCoverToNCover, "FSApiTests.OpenCoverToNCover"
          Tests.FSApiTests.OpenCoverFromNCover, "FSApiTests.OpenCoverFromNCover"
          Tests.FSApiTests.FormatsConvertToXmlDocument, "FSApiTests.FormatsConvertToXmlDocument"
          Tests.FSApiTests.FormatsConvertToXDocument, "FSApiTests.FormatsConvertToXDocument"
          Tests.FSApiTests.FormatsRoundTripSimply, "FSApiTests.FormatsRoundTripSimply"
          Tests.FSApiTests.NCoverToCobertura, "FSApiTests.NCoverToCobertura"
          Tests.FSApiTests.NCoverToBarChart, "FSApiTests.NCoverToBarChart"
          Tests.FSApiTests.OpenCoverBranchCompression, "FSApiTests.OpenCoverBranchCompression"
          Tests.FSApiTests.ArgumentsBuilt, "FSApiTests.ArgumentsBuilt"
        ]

  let specials =
   []

  let consistencyCheck() =
    let here = System.Reflection.Assembly.GetExecutingAssembly().Location
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly(here)

    let testMethods = def.MainModule.GetTypes()
                      |> Seq.collect (fun t -> t.Methods)
                      |> Seq.filter (fun m -> m.CustomAttributes |> isNull |> not)
                      |> Seq.filter (fun m -> m.CustomAttributes |> Seq.exists (fun a -> a.AttributeType.Name = "TestAttribute"))
                      |> Seq.map (fun m -> m.DeclaringType.FullName + "::" + m.Name)

    let lookup = def.MainModule.GetAllTypes()
                 |> Seq.filter (fun t -> t.Methods |> Seq.exists(fun m -> m.Name = "Invoke"))
                 |> Seq.map (fun t -> (t.FullName.Replace("/","+"), t.Methods |> Seq.find(fun m -> m.Name = "Invoke")))
                 |> Map.ofSeq

    let calls = regular
                |> List.map (fst
                            >> (fun f -> f.GetType().FullName.Replace("/","+"))
                            >> (fun f -> Map.find f lookup)
                            >> (fun f -> f.Body.Instructions |> Seq.find (fun i -> i.OpCode = OpCodes.Call))
                            >> (fun i -> let m = (i.Operand :?> MethodDefinition)
                                         m.DeclaringType.FullName + "::" + m.Name))
                |> Set.ofList

    let omitted = testMethods
                  |> Seq.filter (fun t -> (Set.contains t calls) |> not)
                  |> Seq.toList

    // cover all but the special cases
    test <@ omitted = [] @>

  [<Tests>]
  let tests =
    testList "AltCover.Api.Tests"
    <| ((((consistencyCheck, "ConsistencyCheck") :: regular)
        |> List.map (fun (f,name) -> testCase name f))
        @ specials)

module Program =
  [<EntryPoint>]
  let main argv =
    let writeResults = TestResults.writeNUnitSummary ("AltCover.Api.TestResults.xml", "AltCover.Api.Tests")
    let config = defaultConfig.appendSummaryHandler writeResults
    runTestsWithArgs config argv TestMain.tests
#endif