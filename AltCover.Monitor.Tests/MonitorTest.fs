namespace Tests

open System
open System.IO
open System.Xml.Linq

open AltCover

module MonitorTests =

  [<Test>]
  let ShouldRecordPointTotals () =
    let (a, b) = AltCover.Monitor.TryGetPointTotals()
    maybeIgnore (fun () -> not a)

    let code = b.Code
    let branch = b.Branch

    let xml =
      Path.Combine(
        SolutionRoot.location,
        "_Reports/MonitorTestWithAltCoverCoreRunner.net6.0.xml"
      )

    let doc = XDocument.Load(xml)

    let seqpnt =
      doc.Descendants(XName.Get("seqpnt")) |> Seq.length

    let sp2 =
      (doc.Descendants(XName.Get("SequencePoint"))
       |> Seq.length)
      + (doc.Descendants(XName.Get("Method"))
         |> Seq.filter
              (fun x ->
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
    let (a0, _) = AltCover.Monitor.TryGetPointTotals()
    let (a, b) = AltCover.Monitor.TryGetVisitTotals()
    maybeIgnore (fun () -> not (a && a0))
    let code = b.Code
    let branch = b.Branch
    let text =
      Path.Combine(
        SolutionRoot.location,
        "_Reports/MonitorTestWithAltCoverCoreRunner.net6.0.xml"
      )
      |> File.ReadAllText
    test' <@ (code, branch) = (109, 14) @> text
