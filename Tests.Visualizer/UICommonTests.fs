namespace Tests

open System
open System.IO
open System.Reflection

open AltCover
open Swensen.Unquote

#if NETCOREAPP3_0
[<AttributeUsage(AttributeTargets.Method)>]
type TestAttribute() = class
    inherit Attribute()
end
#else
open NUnit.Framework
#endif

module VisualizerTests =

    // Augment.fs

    [<Test>]
    let AugmentNullableDetectNulls() =
      let input = [ "string"; null; "another string" ]
      let nulls = input |> List.map (fun x -> x.IsNotNull |> not)
      test <@ nulls = [ false; true; false ] @>

    [<Test>]
    let AugmentNonNullableDetectNoNulls() =
      let input = [ 1; 2 ;3 ]
      test <@ input |> List.forall (fun x -> x.IsNotNull) @>

    // CoverageFile.fs

    [<Test>]
    let DefaultHelperPassesThrough() =
      test <@ Transformer.defaultHelper null null |> isNull @>