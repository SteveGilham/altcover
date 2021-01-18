namespace Tests
open System
open System.IO
open System.Xml.Linq

open AltCover

module MonitorTests =

  [<Test>]
  let ShouldRecordPointTotals() =
    let (a, b) = AltCover.Monitor.TryGetPointTotals()
    test'<@ a @> "should be running under AltCover"
    let code = b.Code
    let branch = b.Branch

    let xml = Path.Combine(SolutionRoot.location,
                           "_Reports/MonitorTestWithAltCoverCoreRunner.net5.0.xml")
    let doc = XDocument.Load(xml)
    let seqpnt = doc.Descendants(XName.Get("seqpnt")) |> Seq.length
    let sp2 = (doc.Descendants(XName.Get("SequencePoint")) |> Seq.length) +
              (doc.Descendants(XName.Get("Method"))
               |> Seq.filter(fun x -> Seq.isEmpty <| x.Descendants(XName.Get("SequencePoint")) )
               |> Seq.length)
    let eBranch = doc.Descendants(XName.Get("BranchPoint")) |> Seq.length
    let eCode = Math.Max(seqpnt, sp2); // known good
// $lines = Get-Content C:\Users\steve\Documents\GitHub\altcover\_Reports\MonitorTestWithAltCoverCoreRunner.net5.0.xml
// $lines | ? { $_ -like "*BranchPoint *" } | Measure-Object
// $lines | ? { $_ -like "* uspid=*" } | ? { -not ($_ -like "*xsi:type*") } | Measure-Object
// Current (169, 41) = (140, 41)

    test <@ (code, branch) = (eCode, eBranch) @>

  [<Test>]
  let ShouldRecordVisitTotals() =
    let (a0, _) = AltCover.Monitor.TryGetPointTotals()
    let (a, b) = AltCover.Monitor.TryGetVisitTotals()

    test'<@ a && a0 @> "should be running under AltCover"
    let code = b.Code
    let branch = b.Branch
    test <@ (code, branch) = (131, 33) @>