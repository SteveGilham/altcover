namespace Tests.Visualizer

open System
open System.IO
open System.Reflection

open AltCover.Augment
open NUnit.Framework
open Swensen.Unquote

[<TestFixture>]
type AltCoverTests() =
  class

    // Augment.fs

    [<Test>]
    member self.AugmentNullableDetectNulls() =
      let input = [ "string"; null; "another string" ]
      let nulls = input |> List.map (fun x -> x.IsNotNull |> not)
      test <@ nulls = [ false; true; false ] @>

    [<Test>]
    member self.AugmentNonNullableDetectNoNulls() =
      let input = [ 1; 2 ;3 ]
      test <@ input |> List.forall (fun x -> x.IsNotNull) @>
  end