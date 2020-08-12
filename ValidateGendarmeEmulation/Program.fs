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
          AltCover.Tests.ValidateGendarmeEmulation.ShouldMatchGendarmeComplexityInAltCover, "ValidateGendarmeEmulation.ShouldMatchGendarmeComplexityInAltCover"
          AltCover.Tests.ValidateGendarmeEmulation.ShouldMatchGendarmeComplexityInSamples, "ValidateGendarmeEmulation.ShouldMatchGendarmeComplexityInSamples"
          AltCover.Tests.ValidateGendarmeEmulation.ShouldMatchGendarmeComplexityInMonoSamples, "ValidateGendarmeEmulation.ShouldMatchGendarmeComplexityInMonoSamples"
          AltCover.Tests.ValidateGendarmeEmulation.GratuitousCoverage, "ValidateGendarmeEmulation.GratuitousCoverage"
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

    // This is a special case where the test assembly is itself instrumented
    // so have to allow for calls to AltCover.Recorder.Instance::Visit
    // having been injected into the local function reference
    let calls = regular
                |> List.map (fst
                            >> (fun f -> f.GetType().FullName.Replace("/","+"))
                            >> (fun f -> Map.find f lookup)
                            >> (fun f -> f.Body.Instructions
                                         |> Seq.find (fun i -> if i.OpCode = OpCodes.Call
                                                               then match i.Operand with
                                                                    | :? MethodDefinition -> true
                                                                    | _ -> false
                                                               else false))
                            >> (fun i -> let m = (i.Operand :?> MethodDefinition)
                                         m.DeclaringType.FullName + "::" + m.Name))
                |> Set.ofList

    let omitted = testMethods
                  |> Seq.filter (fun t -> (Set.contains t calls) |> not)
                  |> Seq.toList

    // cover all but the special cases
    try
      test <@ omitted |> List.isEmpty @>
    with
    | x -> let t = sprintf "testMethods = %A" testMethods
           let l = sprintf "calls = %A" calls
           let defname = def.MainModule.Name
           System.String.Join(System.Environment.NewLine, [t; l; defname; x.Message])
           |> NUnit.Framework.AssertionException
           |> raise

  [<Tests>]
  let tests =
    testList "AltCover.ValidateGendarmeEmulation"
    <| ((((consistencyCheck, "ConsistencyCheck") :: regular)
        |> List.map (fun (f,name) -> testCase name (fun () -> lock sync f)))
        @ specials)

module Program =
  [<EntryPoint>]
  let main argv =
    let writeResults = TestResults.writeNUnitSummary ("AltCover.ValidateGendarmeEmulation.TestResults.xml", "AltCover.ValidateGendarmeEmulation")
    let config = defaultConfig.appendSummaryHandler writeResults
    runTestsWithArgs config argv TestMain.tests
#endif