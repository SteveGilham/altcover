namespace Tests

open System
open System.IO

open AltCover
open Mono.Cecil

module Rocks =
  let rec GetAllTypes (t: TypeDefinition) =
    t
    :: (List.collect
          GetAllTypes
          (t.NestedTypes
           |> Seq.cast<TypeDefinition>
           |> Seq.toList))

module ValidateGendarmeEmulation =
  let importantItems (n, e, r) = Maybe (r <> e) (Some(n, e, r)) None

  let stringify x =
    match x with
    | Some (n, e, r) -> Some(sprintf "Expected %d got %d in %s" e r n)
    | None -> None

  let seqStringify x = x |> Seq.map stringify

  [<Test>]
  let DoSelfTest () =
    test <@ importantItems (1, 2, 3) = Some(1, 2, 3) @>
    test <@ importantItems (1, 1, 1) |> Option.isNone @>
    test <@ Some("1", 2, 3) |> stringify = Some "Expected 2 got 3 in 1" @>
    test <@ None |> stringify |> Option.isNone @>

  [<Test>]
  let ShouldMatchGendarmeComplexityInAltCover () =
    let path =
      Path.Combine(
        SolutionRoot.location,
        "_Binaries/AltCover/Debug+AnyCPU/netcoreapp2.0/AltCover.Engine.dll"
      )

    let def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly path

    let failures =
      (Seq.map
        ((fun (m: MethodDefinition) ->
          (m.FullName,
           Gendarme.Rules.Maintainability.AvoidComplexMethodsRule.GetCyclomaticComplexity
             m,
           Gendarme.cyclomaticComplexity m))
         >> (fun (n, e, r) -> importantItems (n, e, r)))
        (def.MainModule.Types
         |> Seq.cast<TypeDefinition>
         |> Seq.map Rocks.GetAllTypes
         |> List.concat
         |> List.map (fun t -> t.Methods |> Seq.cast<MethodDefinition>)
         |> Seq.concat
         |> Seq.sortBy (fun m -> m.FullName)))
      |> seqStringify
      |> Seq.choose id
      |> Seq.toList

    test <@ List.isEmpty failures @>

  [<Test>]
  let ShouldMatchGendarmeComplexityInSamples () =
    let path =
      Path.Combine(
        SolutionRoot.location,
        "_Binaries/Sample3/Debug+AnyCPU/netstandard2.0/Sample3.dll"
      )

    let def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly path

    let failures =
      (Seq.map
        ((fun (m: MethodDefinition) ->
          (m.FullName,
           Gendarme.Rules.Maintainability.AvoidComplexMethodsRule.GetCyclomaticComplexity
             m,
           Gendarme.cyclomaticComplexity m))
         >> (fun (n, e, r) -> importantItems (n, e, r)))
        (def.MainModule.Types
         |> Seq.cast<TypeDefinition>
         |> Seq.map Rocks.GetAllTypes
         |> List.concat
         |> List.map (fun t -> t.Methods |> Seq.cast<MethodDefinition>)
         |> Seq.concat
         |> Seq.sortBy (fun m -> m.FullName)))
      |> seqStringify
      |> Seq.choose id
      |> Seq.toList

    test <@ List.isEmpty failures @>

  [<Test>]
  let ShouldMatchGendarmeComplexityInMonoSamples () =
    let path =
      Path.Combine(SolutionRoot.location, "_Mono/Sample3/Sample3.dll")

    maybeIgnore (fun () -> path |> File.Exists |> not)

    let def =
      Mono.Cecil.AssemblyDefinition.ReadAssembly path

    let failures =
      (Seq.map
        ((fun (m: MethodDefinition) ->
          (m.FullName,
           Gendarme.Rules.Maintainability.AvoidComplexMethodsRule.GetCyclomaticComplexity
             m,
           Gendarme.cyclomaticComplexity m))
         >> (fun (n, e, r) -> importantItems (n, e, r)))
        (def.MainModule.Types
         |> Seq.cast<TypeDefinition>
         |> Seq.map Rocks.GetAllTypes
         |> List.concat
         |> List.map (fun t -> t.Methods |> Seq.cast<MethodDefinition>)
         |> Seq.concat
         |> Seq.sortBy (fun m -> m.FullName)))
      |> seqStringify
      |> Seq.choose id
      |> Seq.toList

    test <@ List.isEmpty failures @>

  [<Test>]
  let GratuitousCoverage () =
    let l = Left 23
    let r = Right true
    let mutable x = Object()
    x <- null
    test <@ (23).IsNotNull @>
    test <@ x.IsNotNull |> not @>

    let output =
      ([ r; l ]
       |> List.map
            (fun e ->
              match e with
              | Right b -> b.ToString().ToUpperInvariant()
              | Left i -> String(i.ToString() |> Seq.rev |> Seq.toArray)))
        .Split

    test <@ output = ("TRUE", [ "32" ]) @>