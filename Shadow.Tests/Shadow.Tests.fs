namespace Shadow.Tests

open System
open System.IO
open System.Reflection
open System.Xml.Linq

open AltCover.Recorder
open Mono.Cecil
open NUnit.Framework
open System.Collections.Generic
open Mono.Cecil.Cil

[<TestFixture>]
type AltCoverTests() = class

  [<Test>]
  member self.ShouldBeLinkingTheCorrectCopyOfThisCode() =
    let locker = { Tracer = String.Empty }
    Assert.That(locker.GetType().Assembly.GetName().Name, Is.EqualTo "AltCover.Shadow")

  [<Test;Ignore("Need to fix the visibility of the method")>]
  member self.ShouldBeExecutingTheCorrectCopyOfThisCode() =
    let mutable where = ""
    Locking.WithLockerLocked self (fun () -> where <- Assembly.GetCallingAssembly().GetName().Name)
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
    let epoch = DateTime (1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)
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
    let epoch = DateTime (1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)
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

  [<Test>]
  member self.EmptyFlushLeavesNoTrace() =
    let saved = Console.Out
    try
      Instance.Visits.Clear()
      use stdout = new StringWriter()
      Console.SetOut stdout

      Instance.FlushCounter ()
      Assert.That (stdout.ToString(), Is.Empty)
    finally
      Instance.Visits.Clear()
      Console.SetOut saved

  [<Test>]
  member self.FlushLeavesExpectedTraces() =
    let saved = Console.Out
    let here = Directory.GetCurrentDirectory()
    try
      Instance.Visits.Clear()
      use stdout = new StringWriter()
      Console.SetOut stdout
      Directory.SetCurrentDirectory(Environment.GetEnvironmentVariable("TEMP"))

      Instance.measureTime <- DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)
      use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream("Shadow.Tests.SimpleCoverage.xml")
      if File.Exists Instance.ReportFile then File.Delete Instance.ReportFile
      do
        use worker = new FileStream(Instance.ReportFile, FileMode.CreateNew)
        stream.CopyTo worker
        ()

      let payload = Dictionary<int,int>()
      [0..9 ]
      |> Seq.iter(fun i -> payload.[i] <- (i+1))
      Instance.Visits.["f6e3edb3-fb20-44b3-817d-f69d1a22fc2f"] <- payload

      Instance.FlushCounter ()

      let head = "Coverage statistics flushing took "
      let tail = " seconds\n"
      let recorded = stdout.ToString().Replace("\r\n","\n")
      Assert.That (recorded.StartsWith(head, StringComparison.Ordinal))
      Assert.That (recorded.EndsWith(tail, StringComparison.Ordinal))
      use worker' = new FileStream(Instance.ReportFile, FileMode.Open)
      let after = XDocument.Load worker'
      Assert.That( after.Descendants(XName.Get("seqpnt"))
                   |> Seq.map (fun x -> x.Attribute(XName.Get("visitcount")).Value),
                   Is.EquivalentTo [ "11"; "10"; "9"; "8"; "7"; "6"; "4"; "3"; "2"; "1"])
    finally
      if File.Exists Instance.ReportFile then File.Delete Instance.ReportFile
      Instance.Visits.Clear()
      Console.SetOut saved
      Directory.SetCurrentDirectory(here)

  [<Test>]
  member self.FlushShouldBeRegisteredForUnload() =
    Instance.Visits.Clear()
    let d = AppDomain.CurrentDomain
    let unloaded = d.GetType().GetField(
                     "_domainUnload", BindingFlags.NonPublic ||| BindingFlags.Instance
                     ).GetValue(d) :?> MulticastDelegate
    Assert.That (unloaded, Is.Not.Null)
    let targets = unloaded.GetInvocationList()
                  |> Array.map (fun x -> string x.Target)

    let shadow = AssemblyDefinition.ReadAssembly typeof<AltCover.Recorder.Tracer>.Assembly.Location
    let flush = "System.Void AltCover.Recorder.Instance::FlushCounter<System.EventArgs>(a)"
    let handlers = shadow.MainModule.Types
                   |> Seq.collect (fun t -> t.NestedTypes)
                   |> Seq.filter (fun t -> t.Methods
                                           |> Seq.exists (fun m -> m.Name = "Invoke" &&
                                                                   m.Body.Instructions
                                                                   |> Seq.filter (fun i -> i.OpCode = Cil.OpCodes.Call)
                                                                   |> Seq.exists (fun i -> (string i.Operand) = flush)))
                   // Implementation dependent hack
                   |> Seq.map (fun t -> let f = t.FullName.Replace("/", "+")
                                        let last = Seq.last f
                                                   |> string
                                        let g = string ((Int32.Parse last) - 1)
                                        f.Substring(0, f.Length - 1) + g)

    Assert.That (targets
                 |> Array.tryFind (fun x -> handlers
                                            |> Seq.tryFind (fun h -> h = x)
                                            |> Option.isSome)
                 |> Option.isSome,
                 sprintf "%A" targets)

  [<Test>]
  member self.FlushShouldBeRegisteredForExit() =
    Instance.Visits.Clear()
    let d = AppDomain.CurrentDomain
    let exit = d.GetType().GetField(
                     "_processExit", BindingFlags.NonPublic ||| BindingFlags.Instance
                     ).GetValue(d) :?> MulticastDelegate
    let targets = exit.GetInvocationList()
                  |> Array.map (fun x -> string x.Target)

    let shadow = AssemblyDefinition.ReadAssembly typeof<AltCover.Recorder.Tracer>.Assembly.Location
    let flush = "System.Void AltCover.Recorder.Instance::FlushCounter<System.EventArgs>(a)"
    let handlers = shadow.MainModule.Types
                   |> Seq.collect (fun t -> t.NestedTypes)
                   |> Seq.filter (fun t -> t.Methods
                                           |> Seq.exists (fun m -> m.Name = "Invoke" &&
                                                                   m.Body.Instructions
                                                                   |> Seq.filter (fun i -> i.OpCode = Cil.OpCodes.Call)
                                                                   |> Seq.exists (fun i -> (string i.Operand) = flush)))
                   // Implementation dependent hack
                   |> Seq.map (fun t -> let f = t.FullName.Replace("/", "+")
                                        let last = Seq.last f
                                                   |> string
                                        let g = string ((Int32.Parse last) - 1)
                                        f.Substring(0, f.Length - 1) + g)

    Assert.That (targets
                 |> Array.tryFind (fun x -> handlers
                                            |> Seq.tryFind (fun h -> h = x)
                                            |> Option.isSome)
                 |> Option.isSome,
                 sprintf "%A" targets)

end