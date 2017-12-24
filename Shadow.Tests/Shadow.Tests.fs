namespace Shadow.Tests

open System
open System.Reflection

open AltCover.Recorder
open NUnit.Framework

[<TestFixture>]
type AltCoverTests() = class
  [<Test>]
  member self.Placeholder() =
    let locker = { Tracer = String.Empty }
    let mutable where = ""
    Locking.WithLockerLocked locker (fun () -> where <- Assembly.GetCallingAssembly().GetName().Name)
    Assert.That(locker.GetType().Assembly.GetName().Name, Is.EqualTo "AltCover.Shadow")
    Assert.That(where, Is.EqualTo "AltCover.Shadow")
end