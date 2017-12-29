namespace Shadow.Tests

open System
open System.IO
open System.Reflection
open System.Xml.Linq

open AltCover.Recorder
open NUnit.Framework
open System.Collections.Generic

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

  member private self.UpdateReport a b =
    Instance.UpdateReport a b
    |> ignore

  [<Test>]
  member self.OldDocumentStartIsNotUpdated() =
    let epoch = DateTime.UtcNow
    Instance.startTime <- epoch
    use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream("Shadow.Tests.SimpleCoverage.xml")
    use worker = new MemoryStream()
    stream.CopyTo worker
    worker.Position <- 0L
    let before = XDocument.Load (Assembly.GetExecutingAssembly().GetManifestResourceStream("Shadow.Tests.SimpleCoverage.xml"))
    self.UpdateReport [] worker
    worker.Position <- 0L
    let after = XDocument.Load worker
    let startTimeAttr = after.Root.Attribute(XName.Get("startTime"))
    let startTime = DateTime.ParseExact(startTimeAttr.Value, "o", null)
    Assert.That (startTime.ToUniversalTime(), Is.LessThan epoch)
    Assert.That (startTime.ToUniversalTime(), Is.EqualTo (Instance.startTime.ToUniversalTime()))

  [<Test>]
  member self.NewDocumentStartIsMadeEarlier() =
    let epoch = DateTime (1970, 1, 1)
    Instance.startTime <- epoch
    use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream("Shadow.Tests.SimpleCoverage.xml")
    use worker = new MemoryStream()
    stream.CopyTo worker
    worker.Position <- 0L
    let before = XDocument.Load (Assembly.GetExecutingAssembly().GetManifestResourceStream("Shadow.Tests.SimpleCoverage.xml"))
    self.UpdateReport [] worker
    worker.Position <- 0L
    let after = XDocument.Load worker
    let startTimeAttr = after.Root.Attribute(XName.Get("startTime"))
    let startTime = DateTime.ParseExact(startTimeAttr.Value, "o", null)
    Assert.That (startTime.ToUniversalTime(), Is.EqualTo epoch)
    Assert.That (startTime.ToUniversalTime(), Is.EqualTo (Instance.startTime.ToUniversalTime()))

  [<Test>]
  member self.NewDocumentMeasureIsNotMadeEarlier() =
    let epoch = DateTime (1970, 1, 1)
    Instance.measureTime <- epoch
    use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream("Shadow.Tests.SimpleCoverage.xml")
    use worker = new MemoryStream()
    stream.CopyTo worker
    worker.Position <- 0L
    let before = XDocument.Load (Assembly.GetExecutingAssembly().GetManifestResourceStream("Shadow.Tests.SimpleCoverage.xml"))
    self.UpdateReport [] worker
    worker.Position <- 0L
    let after = XDocument.Load worker
    let startTimeAttr = after.Root.Attribute(XName.Get("measureTime"))
    let startTime = DateTime.ParseExact(startTimeAttr.Value, "o", null)
    Assert.That (startTime.ToUniversalTime(), Is.GreaterThan epoch)
    Assert.That (startTime.ToUniversalTime(), Is.EqualTo (Instance.measureTime.ToUniversalTime()))

  [<Test>]
  member self.OldDocumentMeasureIsUpdated() =
    let epoch = DateTime.UtcNow
    Instance.measureTime <- epoch
    use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream("Shadow.Tests.SimpleCoverage.xml")
    use worker = new MemoryStream()
    stream.CopyTo worker
    worker.Position <- 0L
    let before = XDocument.Load (Assembly.GetExecutingAssembly().GetManifestResourceStream("Shadow.Tests.SimpleCoverage.xml"))
    self.UpdateReport [] worker
    worker.Position <- 0L
    let after = XDocument.Load worker
    let startTimeAttr = after.Root.Attribute(XName.Get("measureTime"))
    let startTime = DateTime.ParseExact(startTimeAttr.Value, "o", null)
    Assert.That (startTime.ToUniversalTime(), Is.EqualTo epoch)
    Assert.That (startTime.ToUniversalTime(), Is.EqualTo (Instance.measureTime.ToUniversalTime()))

  [<Test>]
  member self.UnknownModuleMakesNoChange() =
    Instance.measureTime <- DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)
    use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream("Shadow.Tests.SimpleCoverage.xml")
    use worker = new MemoryStream()
    stream.CopyTo worker
    worker.Position <- 0L
    use before = new StreamReader (Assembly.GetExecutingAssembly().GetManifestResourceStream("Shadow.Tests.SimpleCoverage.xml"))
    let item = KeyValuePair<string, Dictionary<int,int>>("not a guid", null)
    self.UpdateReport [item] worker
    worker.Position <- 0L
    let after = new StreamReader(worker)
    Assert.That (after.ReadToEnd().Replace("\r\n", "\n"),
                 Is.EqualTo (before.ReadToEnd().Replace("\r\n", "\n")))

  [<Test>]
  member self.KnownModuleWithNothingMakesNoChange() =
    Instance.measureTime <- DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)
    use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream("Shadow.Tests.SimpleCoverage.xml")
    use worker = new MemoryStream()
    stream.CopyTo worker
    worker.Position <- 0L
    use before = new StreamReader (Assembly.GetExecutingAssembly().GetManifestResourceStream("Shadow.Tests.SimpleCoverage.xml"))
    let item = KeyValuePair<string, Dictionary<int,int>>("f6e3edb3-fb20-44b3-817d-f69d1a22fc2f", Dictionary<int,int>())
    self.UpdateReport [item] worker
    worker.Position <- 0L
    let after = new StreamReader(worker)
    Assert.That (after.ReadToEnd().Replace("\r\n", "\n"),
                 Is.EqualTo (before.ReadToEnd().Replace("\r\n", "\n")))

  [<Test>]
  member self.KnownModuleWithNothingInRangeMakesNoChange() =
    Instance.measureTime <- DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)
    use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream("Shadow.Tests.SimpleCoverage.xml")
    use worker = new MemoryStream()
    stream.CopyTo worker
    worker.Position <- 0L
    use before = new StreamReader (Assembly.GetExecutingAssembly().GetManifestResourceStream("Shadow.Tests.SimpleCoverage.xml"))
    let payload = Dictionary<int,int>()
    payload.[-1] <- 10
    payload.[100] <- 10
    let item = KeyValuePair<string, Dictionary<int,int>>("f6e3edb3-fb20-44b3-817d-f69d1a22fc2f", payload)
    self.UpdateReport [item] worker
    worker.Position <- 0L
    let after = new StreamReader(worker)
    Assert.That (after.ReadToEnd().Replace("\r\n", "\n"),
                 Is.EqualTo (before.ReadToEnd().Replace("\r\n", "\n")))

  [<Test>]
  member self.KnownModuleWithPayloadMakesExpectedChange() =
    Instance.measureTime <- DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)
    use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream("Shadow.Tests.SimpleCoverage.xml")
    use worker = new MemoryStream()
    stream.CopyTo worker
    worker.Position <- 0L
    let payload = Dictionary<int,int>()
    [0..9 ]
    |> Seq.iter(fun i -> payload.[i] <- (i+1))
    let item = KeyValuePair<string, Dictionary<int,int>>("f6e3edb3-fb20-44b3-817d-f69d1a22fc2f", payload)
    self.UpdateReport [item] worker
    worker.Position <- 0L
    let after = XDocument.Load worker
    Assert.That( after.Descendants(XName.Get("seqpnt"))
                 |> Seq.map (fun x -> x.Attribute(XName.Get("visitcount")).Value),
                 Is.EquivalentTo [ "11"; "10"; "9"; "8"; "7"; "6"; "4"; "3"; "2"; "1"])


end