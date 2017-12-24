namespace Shadow.Tests

open System
open System.Reflection

open AltCover.Recorder
open NUnit.Framework

[<TestFixture>]
type AltCoverTests() = class

  [<Test>]
  member self.ShouldBeExecutingTheCorrectCopyOfThisCode() =
    let locker = { Tracer = String.Empty }
    let mutable where = ""
    Locking.WithLockerLocked locker (fun () -> where <- Assembly.GetCallingAssembly().GetName().Name)
    Assert.That(locker.GetType().Assembly.GetName().Name, Is.EqualTo "AltCover.Shadow")
    Assert.That(where, Is.EqualTo "AltCover.Shadow")

  [<Test>]
  member self.NullIdShouldNotGiveACount() =
    try
      Instance.Visits.Clear()
      Instance.Visit null 23
      Assert.That (Instance.Visits, Is.Empty)
    finally
      Instance.Visits.Clear()

  [<Test>]
  member self.EmptyIdShouldNotGiveACount() =
    try
      Instance.Visits.Clear()
      Instance.Visit String.Empty 23
      Assert.That (Instance.Visits, Is.Empty)
    finally
      Instance.Visits.Clear()

  [<Test>]
  member self.RealIdShouldIncrementCount() =
    try
      Instance.Visits.Clear()
      let key = " "
      Instance.Visit key 23
      Assert.That (Instance.Visits.Count, Is.EqualTo 1)
      Assert.That (Instance.Visits.[key].Count, Is.EqualTo 1)
      Assert.That (Instance.Visits.[key].[23], Is.EqualTo 1)
    finally
      Instance.Visits.Clear()

  [<Test>]
  member self.DistinctIdShouldBeDistinct() =
    try
      Instance.Visits.Clear()
      let key = " "
      Instance.Visit key 23
      Instance.Visit "key" 42
      Assert.That (Instance.Visits.Count, Is.EqualTo 2)
    finally
      Instance.Visits.Clear()

  [<Test>]
  member self.DistinctLineShouldBeDistinct() =
    try
      Instance.Visits.Clear()
      let key = " "
      Instance.Visit key 23
      Instance.Visit key 42
      Assert.That (Instance.Visits.Count, Is.EqualTo 1)
      Assert.That (Instance.Visits.[key].Count, Is.EqualTo 2)
    finally
      Instance.Visits.Clear()

  [<Test>]
  member self.RepeatVisitsShouldIncrementCount() =
    try
      Instance.Visits.Clear()
      let key = " "
      Instance.Visit key 23
      Instance.Visit key 23
      Assert.That (Instance.Visits.[key].[23], Is.EqualTo 2)
    finally
      Instance.Visits.Clear()


end