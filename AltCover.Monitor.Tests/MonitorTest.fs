namespace Tests

open System
open System.IO
open System.Xml.Linq

open AltCover.Local

module MonitorTests =

  let coverageXml () =
    [ Path.Combine(
        AltCover.SolutionRoot.location,
        "_Reports/MonitorTestWithAltCoverCore.xml"
      ),
      [ (261, 0) ] // 0 because NCover format
      Path.Combine(
        AltCover.SolutionRoot.location,
        "_Reports/MonitorTestWithAltCoverCoreRunner.net7.0.xml"
      ),
      [ (260, 37); (260, 36) ] ]
    |> List.filter (fst >> File.Exists)
    |> List.sortBy (fst >> File.GetCreationTimeUtc)
    |> List.last

  [<Test>]
  let ShouldCountOpenCoverTotals () =
    use stream =
      System.Reflection.Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Monitor.Tests.HandRolledMonoCoverage.xml")

    let doc = System.Xml.XmlDocument()
    doc.Load(stream)
    let b = Monitor.CountVisitPoints(doc)
    let code = b.Code
    let branch = b.Branch

    test <@ (code, branch) = (15, 2) @>
    ()

  [<Test>]
  let ShouldRecordPointTotals () =
    let (a, b) = Monitor.TryGetPointTotals()
    maybeIgnore (fun () -> not a)

    let code = b.Code
    let branch = b.Branch

    let doc =
      XDocument.Load(() |> coverageXml |> fst)

    let seqpnt =
      doc.Descendants(XName.Get("seqpnt")) |> Seq.length

    let sp2 =
      (doc.Descendants(XName.Get("SequencePoint"))
       |> Seq.length)
      + (doc.Descendants(XName.Get("Method"))
         |> Seq.filter (fun x ->
           Seq.isEmpty
           <| x.Descendants(XName.Get("SequencePoint")))
         |> Seq.length)

    let eBranch =
      doc.Descendants(XName.Get("BranchPoint"))
      |> Seq.length

    let eCode = Math.Max(seqpnt, sp2)
    test <@ (code, branch) = (eCode, eBranch) @>

  [<Test>]
  let ShouldRecordVisitTotals () =
    let (a0, _) = Monitor.TryGetPointTotals()
    let (a, b) = Monitor.TryGetVisitTotals()
    maybeIgnore (fun () -> not (a && a0))
    let code = b.Code
    let branch = b.Branch

    let xml, expect = coverageXml ()
    let text = xml |> File.ReadAllText
    let result = (code, branch)
    let t2 = sprintf "%A" result

    test' <@ List.exists (fun x -> x = result) expect @> (text + t2)