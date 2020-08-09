namespace AltCover.Tests

open System
open System.IO
open System.Reflection

open AltCover
open Mono.Cecil
open Swensen.Unquote

#if NETCOREAPP3_0
[<AttributeUsage(AttributeTargets.Method)>]
type TestAttribute() = class
    inherit Attribute()
end
#else
type TestAttribute = NUnit.Framework.TestAttribute
#endif

module Rocks =
  let rec GetAllTypes(t : TypeDefinition) =
    t :: (List.collect GetAllTypes
            (t.NestedTypes
             |> Seq.cast<TypeDefinition>
             |> Seq.toList))

module ValidateGendarmeEmulation =

  [<Test>]
  let ShouldMatchGendarmeComplexityInAltCover() =
    let path =
      Path.Combine(
        SolutionRoot.location,
        "_Binaries/AltCover/Debug+AnyCPU/netcoreapp2.0/AltCover.Engine.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path

    let failures =
      (Seq.map
        ((fun (m : MethodDefinition) ->
          (m.FullName,
            Gendarme.Rules.Maintainability.AvoidComplexMethodsRule.GetCyclomaticComplexity
              m, Gendarme.cyclomaticComplexity m))
          >> (fun (n, e, r) ->
          if r <> e then Some(n, e, r) else None))
          (def.MainModule.Types
          |> Seq.cast<TypeDefinition>
          |> Seq.map Rocks.GetAllTypes
          |> List.concat
          |> List.map (fun t -> t.Methods |> Seq.cast<MethodDefinition>)
          |> Seq.concat
          |> Seq.sortBy (fun m -> m.FullName)))
      |> Seq.filter Option.isSome
      |> Seq.map (fun x ->
            let n, e, r = Option.get x
            sprintf "Expected %d got %d in %s" e r n)
      |> Seq.toList
    test <@ List.isEmpty failures @>

  [<Test>]
  let ShouldMatchGendarmeComplexityInSamples() =
    let path =
      Path.Combine(
        SolutionRoot.location,
          "_Binaries/Sample3/Debug+AnyCPU/netstandard2.0/Sample3.dll")
    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path

    let failures =
      (Seq.map
        ((fun (m : MethodDefinition) ->
          (m.FullName,
            Gendarme.Rules.Maintainability.AvoidComplexMethodsRule.GetCyclomaticComplexity
              m, Gendarme.cyclomaticComplexity m))
          >> (fun (n, e, r) ->
          if r <> e then Some(n, e, r) else None))
          (def.MainModule.Types
          |> Seq.cast<TypeDefinition>
          |> Seq.map Rocks.GetAllTypes
          |> List.concat
          |> List.map (fun t -> t.Methods |> Seq.cast<MethodDefinition>)
          |> Seq.concat
          |> Seq.sortBy (fun m -> m.FullName)))
      |> Seq.filter Option.isSome
      |> Seq.map (fun x ->
            let n, e, r = Option.get x
            sprintf "Expected %d got %d in %s" e r n)
      |> Seq.toList
    test <@ List.isEmpty failures @>

  [<Test>]
  let ShouldMatchGendarmeComplexityInMonoSamples() =
    let path =
      Path.Combine(
        SolutionRoot.location,
        "_Mono/Sample3/Sample3.dll")

    let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path

    let failures =
      (Seq.map
        ((fun (m : MethodDefinition) ->
          (m.FullName,
            Gendarme.Rules.Maintainability.AvoidComplexMethodsRule.GetCyclomaticComplexity
              m, Gendarme.cyclomaticComplexity m))
          >> (fun (n, e, r) ->
          if r <> e then Some(n, e, r) else None))
          (def.MainModule.Types
          |> Seq.cast<TypeDefinition>
          |> Seq.map Rocks.GetAllTypes
          |> List.concat
          |> List.map (fun t -> t.Methods |> Seq.cast<MethodDefinition>)
          |> Seq.concat
          |> Seq.sortBy (fun m -> m.FullName)))
      |> Seq.filter Option.isSome
      |> Seq.map (fun x ->
            let n, e, r = Option.get x
            sprintf "Expected %d got %d in %s" e r n)
      |> Seq.toList
    test <@ List.isEmpty failures @>

  [<Test>]
  let GratuitousCoverage() =
    let l = Left 23
    let r = Right true
    let mutable x = Object()
    x <- null
    test <@ (23).IsNotNull @>
    test <@ x.IsNotNull |> not @>

    let output =
      ([ r; l ]
        |> List.map (fun e ->
            match e with
            | Right b -> b.ToString().ToUpperInvariant()
            | Left i ->
                String
                  (i.ToString()
                    |> Seq.rev
                    |> Seq.toArray))).Split
    test <@ output = ("TRUE", [ "32" ]) @>