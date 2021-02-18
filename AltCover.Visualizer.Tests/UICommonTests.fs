namespace Tests

open System.IO
open System.Reflection
open System.Xml.Linq

open AltCover

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

    [<Test>]
    let CoberturaToNCoverIsOK() =
      use sr1 = new StreamReader(Assembly.GetExecutingAssembly()
                                         .GetManifestResourceStream("AltCover.Visualizer.Tests.Reports.Cobertura_altcover.xml"))
      let before = XDocument.Load sr1
      let after = Transformer.transformFromCobertura before
      printfn "%s" <| after.ToString()
      test <@ false @>