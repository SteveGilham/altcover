namespace Tests.Visualizer

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
      let nulls = input |> List.map (Option.nullable >> Option.isNone)
      test <@ nulls = [ false; true; false ] @>

    [<Test>]
    member self.AugmentGetOrElseFillsInNone() =
      let input = [ "string"; null; "another string" ]
      let strings = input |> List.map (Option.nullable >> (Option.getOrElse "fallback"))
      test <@ strings =[ "string"; "fallback"; "another string" ] @>

  end