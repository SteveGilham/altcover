module Sample22

open System.Xml.Linq
open NUnit.Framework
open Tests

[<SetUp>]
let Setup () =
    ()

let rec RecursiveValidateOpenCover result expected' depth zero expectSkipped =
  let x name = XName.Get(name)
  let rcount = result |> Seq.length

  let expected =
    expected'
    |> Seq.filter (fun (el : XElement) ->
          el.Name.LocalName <> "Module" || expectSkipped || "skippedDueTo"
                                                            |> x
                                                            |> el.Attributes
                                                            |> Seq.isEmpty)
    |> Seq.toList

  let ecount = expected |> Seq.length
  test' <@ rcount = ecount @>
    ("Mismatch at depth " + depth.ToString() + " : " + expected.ToString() + " but got"
      + (result |> Seq.toList).ToString())
  Seq.zip result expected
  |> Seq.iter
        (fun (r : XElement, e : XElement) ->
        test <@ r.Name = e.Name @>
        let ra = r.Attributes()
        let ea = e.Attributes()
        Seq.zip ra ea
        |> Seq.iter
            (fun (a1 : XAttribute, a2 : XAttribute) ->
            test <@ a1.Name = a2.Name @>
            match a1.Name.ToString() with
            | "bev"
            | "visited"
            | "visitedSequencePoints"
            | "visitedBranchPoints"
            | "visitedClasses"
            | "visitedMethods"
            | "sequenceCoverage"
            | "branchCoverage"
            | "uspid"
            | "minCrapScore"
            | "maxCrapScore"
            | "crapScore"
            | "hash" -> ()
            | "fullPath" ->
              test'
                <@ a1.Value.Replace("\\", "/").Replace("altcover", "AltCover").
                            EndsWith(a2.Value.Replace("\\", "/").Replace("altcover", "AltCover")) @>
                (a1.Name.ToString() + " : " + r.ToString() + " -> document")
            | "vc" ->
              let expected =
                maybe zero "0" a2.Value
              test' <@ expected = a1.Value @> (r.ToString() + " -> visitcount")
            | _ ->
              test' <@ a1.Value = a2.Value @>
                (r.ToString() + " -> " + a1.Name.ToString()))
        RecursiveValidateOpenCover (r.Elements()) (e.Elements()) (depth + 1) zero
          expectSkipped)

[<Test>]
let Test1 () =
    Assert.Pass()