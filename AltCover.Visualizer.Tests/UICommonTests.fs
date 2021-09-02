namespace Tests

open System
open System.IO
open System.Reflection
open System.Xml.Linq

open AltCover

module VisualizerTests =

  // Augment.fs

  [<Test>]
  let AugmentNullableDetectNulls () =
    let input = [ "string"; null; "another string" ]

    let nulls =
      input |> List.map (fun x -> x.IsNotNull |> not)

    test <@ nulls = [ false; true; false ] @>

  [<Test>]
  let AugmentNonNullableDetectNoNulls () =
    let input = [ 1; 2; 3 ]
    test <@ input |> List.forall (fun x -> x.IsNotNull) @>

  // CoverageFile.fs

  [<Test>]
  let DefaultHelperPassesThrough () =
    test <@ Transformer.defaultHelper null null |> isNull @>

  [<Test>]
  let CoberturaToNCoverIsOK () =
    let root =
      Path.Combine(SolutionRoot.location, "Sample20")
      |> Path.GetFullPath

    use sr1 =
      new StreamReader(
        Assembly
          .GetExecutingAssembly()
          .GetManifestResourceStream(
            "AltCover.Visualizer.Tests.Reports.Cobertura_coverlet.xml"
          )
      )

    let before = XDocument.Load sr1

    before.Descendants(XName.Get "source")
    |> Seq.iter (fun x -> x.Value <- root)

    let after =
      Transformer.transformFromCobertura before

    use sr2 =
      new StreamReader(
        Assembly
          .GetExecutingAssembly()
          .GetManifestResourceStream(
            "AltCover.Visualizer.Tests.Results.Cobertura_coverlet.ncover.xml"
          )
      )

    let expect = XDocument.Load sr2

    //printfn "%A" after
    //Assert.That(after.ToString().Replace("\r", String.Empty),
    //            Is.EqualTo <| expect.ToString().Replace("\r", String.Empty))
    test
      <@ after.ToString().Replace("\r", String.Empty) = expect
        .ToString()
        .Replace("\r", String.Empty) @>