namespace Tests.Visualizer

open AltCover.Augment
open NUnit.Framework

[<TestFixture>]
type AltCoverTests() =
  class

    // Augment.fs

    [<Test>]
    member self.AugmentNullableDetectNulls() =
      let input = [ "string"; null; "another string" ]
      let nulls = input |> Seq.map (Option.nullable >> Option.isNone)
      Assert.That(nulls, Is.EquivalentTo([ false; true; false ]))

    [<Test>]
    member self.AugmentGetOrElseFillsInNone() =
      let input = [ "string"; null; "another string" ]
      let strings = input |> Seq.map (Option.nullable >> (Option.getOrElse "fallback"))
      Assert.That(strings, Is.EquivalentTo([ "string"; "fallback"; "another string" ]))

  end