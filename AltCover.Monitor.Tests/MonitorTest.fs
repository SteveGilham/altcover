namespace Tests

open AltCover
open NUnit.Framework

module MonitorTests =

  [<Test>]
  let ShouldRecordPointTotals() =
    let (a, b) = AltCover.Monitor.TryGetPointTotals()
    test'<@ a @> "should be running under AltCover"
    let code = b.Code
    let branch = b.Branch
    test' <@ code > 0 @> "Code"
    test' <@ branch >= 0 @> "Branch"

  [<Test>]
  let ShouldRecordVisitTotals() =
    let (a0, b0) = AltCover.Monitor.TryGetPointTotals()
    let (a, b) = AltCover.Monitor.TryGetVisitTotals()
    test'<@ a && a0 @> "should be running under AltCover"
    let code = b.Code
    let code0 = b0.Code
    let branch = b.Branch
    let branch0 = b0.Branch 
    test <@ (code > 0) && (code <= code0)  @>
    test <@ (branch >= 0) && (branch <= branch0)  @>