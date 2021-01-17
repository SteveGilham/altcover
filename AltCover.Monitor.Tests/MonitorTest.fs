namespace Tests

open AltCover

module MonitorTests =

  [<Test>]
  let ShouldRecordPointTotals() =
    let (a, b) = AltCover.Monitor.TryGetPointTotals()
    test'<@ a @> "should be running under AltCover"
    let code = b.Code
    let branch = b.Branch
    test <@ (code, branch) = (122, 23) @>

  [<Test>]
  let ShouldRecordVisitTotals() =
    let (a0, b0) = AltCover.Monitor.TryGetPointTotals()
    let (a, b) = AltCover.Monitor.TryGetVisitTotals()

    test'<@ a && a0 @> "should be running under AltCover"
    let code = b.Code
    let branch = b.Branch
    test <@ (code, branch) = (107, 18) @>