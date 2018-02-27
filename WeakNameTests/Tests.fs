namespace WeakNameTests

open System
open System.IO
open System.Reflection

open AltCover
open Mono.Cecil
open NUnit.Framework

module Rocks =
  let rec GetAllTypes (t:TypeDefinition) =
      t ::
      (t.NestedTypes
       |> Seq.cast<TypeDefinition>
       |> Seq.toList
       |> List.map GetAllTypes
       |> List.concat)

[<TestFixture>]
type AltCoverTests() = class
  [<Test>]
  member self.ShouldMatchGendarmeComplexity() =
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(where.Substring(0, where.IndexOf("_Binaries")) + "_Binaries/AltCover/Debug+AnyCPU/netcoreapp2.0", "AltCover.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
    let failures = def.MainModule.Types
                    |> Seq.cast<TypeDefinition>
                    |> Seq.map Rocks.GetAllTypes
                    |> List.concat
                    |> List.map (fun t -> t.Methods |> Seq.cast<MethodDefinition>)
                    |> Seq.concat
                    |> Seq.sortBy (fun m -> m.FullName)
                    |> Seq.map (fun m -> (m.FullName,
                                           Gendarme.Rules.Maintainability.AvoidComplexMethodsRule.GetCyclomaticComplexity m,
                                           OpenCover.CyclomaticComplexity m))
                    |> Seq.map (fun (n,e,r) -> if r <> e then Some (n,e,r) else None)
                    |> Seq.filter (fun x -> Option.isSome x)
                    |> Seq.map (fun x -> let n,e,r = Option.get x
                                         sprintf "Expected %d got %d in %s" e r n)
                    |> Seq.toList

    Assert.That(failures, Is.Empty, String.Join(Environment.NewLine, failures))
end