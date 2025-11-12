#if RUNNER_TESTS
namespace Tests
#else
#if !NET472
#if NET20
namespace Tests.Recorder.Clr2
#else
namespace Tests.Recorder.Core
#endif
#else
namespace Tests.Recorder.Clr4
#endif
#endif

open System

#if RUNNER_TESTS
open AltCover
#else
open AltCover.Recorder
open NUnit.Framework
#endif

module BaseTests =
  [<Test>]
  let ExerciseBoth () =
    let b = Both(Pair.Create(0L, 0))
    let b1 = Both(Pair.Create(0L, 0))
    let b2 = Both(Pair.Create(1L, 2))
    Assert.That(b1.Equals(b))
    Assert.That(not <| b2.Equals(b))
    Assert.That(not <| b2.Equals(String.Empty))
    Assert.That(not <| b2.Value.Equals(String.Empty))
    Assert.That(b.GetHashCode(), Is.EqualTo(b1.GetHashCode()))
    Assert.That(b.ToString(), Is.EqualTo("AltCover.Both : AltCover.Pair(Time=0, Call=0)"))

  [<Test>]
  let ExerciseCall () =
    let b = Call(0)
    let b1 = Call(0)
    let b2 = Call(2)
    Assert.That(b1.Equals(b))
    Assert.That(not <| b2.Equals(b))
    Assert.That(not <| b2.Equals(String.Empty))
    Assert.That(b.GetHashCode(), Is.EqualTo(b1.GetHashCode()))
    Assert.That(b.ToString(), Is.EqualTo("AltCover.Call : 0"))

  [<Test>]
  let ExerciseTime () =
    let b = Time(0l)
    let b1 = Time(0l)
    let b2 = Time(2l)
    Assert.That(b1.Equals(b))
    Assert.That(not <| b2.Equals(b))
    Assert.That(not <| b2.Equals(String.Empty))
    Assert.That(b.GetHashCode(), Is.EqualTo(b1.GetHashCode()))
    Assert.That(b.ToString(), Is.EqualTo("AltCover.Time : 0"))

  [<Test>]
  let ExerciseNull () =
    let b = Null()
    let b1 = Null()
    Assert.That(b1.Equals(b))
    Assert.That(not <| b.Equals(String.Empty))
    Assert.That(b.GetHashCode(), Is.EqualTo(b1.GetHashCode()))
    Assert.That(b.ToString(), Is.EqualTo("AltCover.Null"))

  [<Test>]
  let ExercisePointVisit () =
    let p = PointVisit.Create()
    let p1 = PointVisit.Create()
    Assert.That(p1.Equals(p))
    Assert.That(not <| p.Equals(String.Empty))
    Assert.That(p.GetHashCode(), Is.EqualTo(p1.GetHashCode()))
    Assert.That(p.ToString(), Is.EqualTo("AltCover.PointVisit : Count = 0 Tracks = ''"))
    p.Track(Null())
    p1.Track(Null())
    Assert.That(p1.Equals(p))
    Assert.That(p.GetHashCode(), Is.EqualTo(p1.GetHashCode()))

    Assert.That(
      p.ToString(),
      Is.EqualTo("AltCover.PointVisit : Count = 0 Tracks = 'AltCover.Null'")
    )

    p.Step()
    Assert.That(p1.Equals(p) |> not)
    p1.Step()
    Assert.That(p1.Equals(p))
    Assert.That(p.GetHashCode(), Is.EqualTo(p1.GetHashCode()))
    p.Track(Both(Pair.Create(1L, 2)))
    Assert.That(p1.Equals(p) |> not)

    Assert.That(
      p.ToString(),
      Is.EqualTo(
        "AltCover.PointVisit : Count = 1 Tracks = 'AltCover.Null; AltCover.Both : AltCover.Pair(Time=1, Call=2)'"
      )
    )

    p1.Track(Time(2l))
    Assert.That(p1.Equals(p) |> not)