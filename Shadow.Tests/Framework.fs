#if NET4
namespace Tests.Shadow.Clr4
#else
#if NET2
namespace Tests.Shadow.Clr2
#else
#if MONO
namespace Tests.Shadow.Mono
#else
namespace Tests.Shadow.Unknown
#endif
#endif
#endif

open System
open System.Collections.Generic
open System.Reflection

open AltCover.Recorder
open NUnit.Framework

[<TestFixture>]
type AltCoverFrameworkTests() = class

  [<Test>]
  member self.TracerStubsAreNoOps() =
    let t = { Tracer = "dummy" }
    t.OnFinish false
    t.CatchUp ()
    Instance.TraceVisit 1 2
    Assert.Pass()

#if MONO
  // The hack doesn't work in Mono (or .net core)
#else
  [<Test>]
  member self.FlushShouldBeRegisteredForUnload() =
    Instance.Visits.Clear()
    let d = AppDomain.CurrentDomain
    let unloaded = d.GetType().GetField(
                     "_domainUnload", BindingFlags.NonPublic ||| BindingFlags.Instance
                     ).GetValue(d) :?> MulticastDelegate
    Assert.That (unloaded, Is.Not.Null)
    let targets = unloaded.GetInvocationList()
                  |> Seq.map (fun x -> string x.Target)
                  |> Seq.filter (fun t -> t.StartsWith("AltCover.Recorder.Instance", StringComparison.Ordinal))
                  |> Seq.toArray
    Assert.That(targets, Is.Not.Empty)

  [<Test>]
  member self.FlushShouldBeRegisteredForExit() =
    Instance.Visits.Clear()
    let d = AppDomain.CurrentDomain
    let exit = d.GetType().GetField(
                     "_processExit", BindingFlags.NonPublic ||| BindingFlags.Instance
                     ).GetValue(d) :?> MulticastDelegate
    let targets = exit.GetInvocationList()
                  |> Seq.map (fun x -> string x.Target)
                  |> Seq.filter (fun t -> t.StartsWith("AltCover.Recorder.Instance", StringComparison.Ordinal))
                  |> Seq.toArray
    Assert.That(targets, Is.Not.Empty)
#endif

end