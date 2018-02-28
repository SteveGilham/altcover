#if NETCOREAPP2_0
namespace Tests.Shadow.Core
#else
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
#endif

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Reflection
open System.Runtime.CompilerServices
open System.Xml

open AltCover.Recorder
open NUnit.Framework
open System.Threading
open System

[<TestFixture>]
type AltCoverTests() = class

  [<MethodImpl(MethodImplOptions.NoInlining)>]
  member private self.GetMyMethodName tag =
    ignore tag
//    let st = StackTrace(StackFrame(1))
//    st.GetFrame(0).GetMethod().Name |>
//#if NET2
//    printfn "%s %s 2" tag
//#else
//    printfn "%s %s" tag
//#endif

  [<Test>]
  member self.ShouldBeLinkingTheCorrectCopyOfThisCode() =
    self.GetMyMethodName "=>"
    let tracer = {
        Tracer = String.Empty
        Stream = null
        Formatter = null
    }
    Assert.That(tracer.GetType().Assembly.GetName().Name, Is.EqualTo "AltCover.Shadow")
    self.GetMyMethodName "<="

  [<Test>]
  member self.NullIdShouldNotGiveACount() =
    self.GetMyMethodName "=>"
    lock Instance.Visits (fun () ->
    try
      Instance.Visits.Clear()
      Instance.VisitImpl null 23
      Assert.That (Instance.Visits, Is.Empty)
    finally
      Instance.Visits.Clear())
    self.GetMyMethodName "<="

  [<Test>]
  member self.EmptyIdShouldNotGiveACount() =
    self.GetMyMethodName "=>"
    lock Instance.Visits (fun () ->
    try
      Instance.Visits.Clear()
      Instance.VisitImpl String.Empty 23
      Assert.That (Instance.Visits, Is.Empty)
    finally
      Instance.Visits.Clear())
    self.GetMyMethodName "<="

  member self.RealIdShouldIncrementCount() =
    self.GetMyMethodName "=>"
    lock Instance.Visits (fun () ->
    let save = Instance.trace
    try
      Instance.Visits.Clear()
      Instance.trace <- { Tracer=null; Stream=null; Formatter=null }
      let key = " "
      Assert.That (Instance.Backlog(), Is.EqualTo 0)
      Thread.Sleep 1000 // provoke a timeout
      Instance.Visit key 23
      Assert.That(Instance.Backlog(), Is.LessThan 2)
      while Instance.Backlog () > 0 do
        Thread.Sleep 100
        Assert.That(Instance.Backlog(), Is.LessThan 2)

      Thread.Sleep 100
      Assert.That (Instance.Visits.Count, Is.EqualTo 1)
      Assert.That (Instance.Visits.[key].Count, Is.EqualTo 1)
      Assert.That (Instance.Visits.[key].[23], Is.EqualTo 1)
    finally
      Instance.Visits.Clear()
      Instance.trace <- save)
    self.GetMyMethodName "<="

#if NET4
  // passing lambdas across the CLR divide doesn't work
#else
  [<Test>]
  member self.RealIdShouldIncrementCountSynchronously() =
    self.GetMyMethodName "=>"
    lock Instance.Visits (fun () ->
    let save = Instance.trace
    try
      Instance.Visits.Clear()
      Instance.trace <- { Tracer=null; Stream=null; Formatter=null }
      let key = " "
      Instance.VisitSelection (fun () -> true) key 23
      Thread.Sleep 100
      Assert.That (Instance.Visits.Count, Is.EqualTo 1, "A visit happened")
      Assert.That (Instance.Visits.[key].Count, Is.EqualTo 1, "keys = " + String.Join("; ", Instance.Visits.Keys|> Seq.toArray))
      Assert.That (Instance.Visits.[key].[23], Is.EqualTo 1)
    finally
      Instance.Visits.Clear()
      Instance.trace <- save)
    self.GetMyMethodName "<="
#endif

  [<Test>]
  member self.DistinctIdShouldBeDistinct() =
    self.GetMyMethodName "=>"
    lock Instance.Visits (fun () ->
    try
      Instance.Visits.Clear()
      let key = " "
      Instance.VisitImpl key 23
      Instance.VisitImpl "key" 42
      Assert.That (Instance.Visits.Count, Is.EqualTo 2)
    finally
      Instance.Visits.Clear())
    self.GetMyMethodName "<="

  [<Test>]
  member self.DistinctLineShouldBeDistinct() =
    self.GetMyMethodName "=>"
    lock Instance.Visits (fun () ->
    try
      Instance.Visits.Clear()
      let key = " "
      Instance.VisitImpl key 23
      Instance.VisitImpl key 42
      Assert.That (Instance.Visits.Count, Is.EqualTo 1)
      Assert.That (Instance.Visits.[key].Count, Is.EqualTo 2)
    finally
      Instance.Visits.Clear())
    self.GetMyMethodName "<="

  [<Test>]
  member self.RepeatVisitsShouldIncrementCount() =
    self.GetMyMethodName "=>"
    lock Instance.Visits (fun () ->
    try
      Instance.Visits.Clear()
      let key = " "
      Instance.VisitImpl key 23
      Instance.VisitImpl key 23
      Assert.That (Instance.Visits.[key].[23], Is.EqualTo 2)
    finally
      Instance.Visits.Clear())
    self.GetMyMethodName "<="

  member private self.UpdateReport a b =
    Counter.UpdateReport true a ReportFormat.NCover b
    |> ignore

   member self.resource = Assembly.GetExecutingAssembly().GetManifestResourceNames()
                         |> Seq.find (fun n -> n.EndsWith("SimpleCoverage.xml", StringComparison.Ordinal))
   member self.resource2 = Assembly.GetExecutingAssembly().GetManifestResourceNames()
                          |> Seq.find (fun n -> n.EndsWith("Sample1WithModifiedOpenCover.xml", StringComparison.Ordinal))

  [<Test>]
  member self.OldDocumentStartIsNotUpdated() =
    self.GetMyMethodName "=>"
    lock Instance.Visits (fun () ->
    let epoch = DateTime.UtcNow
    Counter.startTime <- epoch
    use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(self.resource)
    let size = int stream.Length
    let buffer = Array.create size 0uy
    Assert.That (stream.Read(buffer, 0, size), Is.EqualTo size)
    use worker = new MemoryStream()
    worker.Write (buffer, 0, size)
    worker.Position <- 0L
    let before = XmlDocument()
    before.Load (Assembly.GetExecutingAssembly().GetManifestResourceStream(self.resource))
    self.UpdateReport (Dictionary<string, Dictionary<int, int>>()) worker
    worker.Position <- 0L
    let after = XmlDocument()
    after.Load worker
    let startTimeAttr = after.DocumentElement.GetAttribute("startTime")
    let startTime = DateTime.ParseExact(startTimeAttr, "o", null)
    Assert.That (startTime.ToUniversalTime(), Is.LessThan epoch)
    Assert.That (startTime.ToUniversalTime(), Is.EqualTo (Counter.startTime.ToUniversalTime())))
    self.GetMyMethodName "<="

  [<Test>]
  member self.NewDocumentStartIsMadeEarlier() =
    self.GetMyMethodName "=>"
    lock Instance.Visits (fun () ->
    let epoch = DateTime (1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)
    Counter.startTime <- epoch
    use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(self.resource)
    let size = int stream.Length
    let buffer = Array.create size 0uy
    Assert.That (stream.Read(buffer, 0, size), Is.EqualTo size)
    use worker = new MemoryStream()
    worker.Write (buffer, 0, size)
    worker.Position <- 0L
    let before = XmlDocument()
    before.Load (Assembly.GetExecutingAssembly().GetManifestResourceStream(self.resource))
    self.UpdateReport (Dictionary<string, Dictionary<int, int>>()) worker
    worker.Position <- 0L
    let after = XmlDocument()
    after.Load worker
    let startTimeAttr = after.DocumentElement.GetAttribute("startTime")
    let startTime = DateTime.ParseExact(startTimeAttr, "o", null)
    Assert.That (startTime.ToUniversalTime(), Is.EqualTo epoch)
    Assert.That (startTime.ToUniversalTime(), Is.EqualTo (Counter.startTime.ToUniversalTime())))
    self.GetMyMethodName "<="

  [<Test>]
  member self.NewDocumentMeasureIsNotMadeEarlier() =
    self.GetMyMethodName "=>"
    lock Instance.Visits (fun () ->
    let epoch = DateTime (1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)
    Counter.measureTime <- epoch
    use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(self.resource)
    let size = int stream.Length
    let buffer = Array.create size 0uy
    Assert.That (stream.Read(buffer, 0, size), Is.EqualTo size)
    use worker = new MemoryStream()
    worker.Write (buffer, 0, size)
    worker.Position <- 0L
    let before = XmlDocument()
    before.Load (Assembly.GetExecutingAssembly().GetManifestResourceStream(self.resource))
    self.UpdateReport (Dictionary<string, Dictionary<int, int>>()) worker
    worker.Position <- 0L
    let after = XmlDocument()
    after.Load worker
    let startTimeAttr = after.DocumentElement.GetAttribute("measureTime")
    let startTime = DateTime.ParseExact(startTimeAttr, "o", null)
    Assert.That (startTime.ToUniversalTime(), Is.GreaterThan epoch)
    Assert.That (startTime.ToUniversalTime(), Is.EqualTo (Counter.measureTime.ToUniversalTime())))
    self.GetMyMethodName "<="

  [<Test>]
  member self.OldDocumentMeasureIsUpdated() =
    self.GetMyMethodName "=>"
    lock Instance.Visits (fun () ->
    let epoch = DateTime.UtcNow
    Counter.measureTime <- epoch
    use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(self.resource)
    let size = int stream.Length
    let buffer = Array.create size 0uy
    Assert.That (stream.Read(buffer, 0, size), Is.EqualTo size)
    use worker = new MemoryStream()
    worker.Write (buffer, 0, size)
    worker.Position <- 0L
    let before = XmlDocument()
    before.Load (Assembly.GetExecutingAssembly().GetManifestResourceStream(self.resource))
    self.UpdateReport (Dictionary<string, Dictionary<int, int>>()) worker
    worker.Position <- 0L
    let after = XmlDocument()
    after.Load worker
    let startTimeAttr = after.DocumentElement.GetAttribute("measureTime")
    let startTime = DateTime.ParseExact(startTimeAttr, "o", null)
    Assert.That (startTime.ToUniversalTime(), Is.EqualTo epoch)
    Assert.That (startTime.ToUniversalTime(), Is.EqualTo (Counter.measureTime.ToUniversalTime())))
    self.GetMyMethodName "<="

  [<Test>]
  member self.UnknownModuleMakesNoChange() =
    self.GetMyMethodName "=>"
    lock Instance.Visits (fun () ->
    Counter.measureTime <- DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)
    use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(self.resource)
    let size = int stream.Length
    let buffer = Array.create size 0uy
    Assert.That (stream.Read(buffer, 0, size), Is.EqualTo size)
    use worker = new MemoryStream()
    worker.Write (buffer, 0, size)
    worker.Position <- 0L
    use before = new StreamReader (Assembly.GetExecutingAssembly().GetManifestResourceStream(self.resource))
    let item = Dictionary<string, Dictionary<int, int>>()
    item.Add ("not a guid", null)
    self.UpdateReport item worker
    worker.Position <- 0L
    let after = new StreamReader(worker)
    let result = after.ReadToEnd().Split([|'\r';'\n'|], StringSplitOptions.RemoveEmptyEntries)
                 |> Seq.skip 3
    let expected = before.ReadToEnd().Split([|'\r';'\n'|], StringSplitOptions.RemoveEmptyEntries)
                 |> Seq.skip 3
    Assert.That (result,
                 Is.EquivalentTo expected))
    self.GetMyMethodName "<="

  [<Test>]
  member self.KnownModuleWithNothingMakesNoChange() =
    self.GetMyMethodName "=>"
    lock Instance.Visits (fun () ->
    Counter.measureTime <- DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)
    use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(self.resource)
    let size = int stream.Length
    let buffer = Array.create size 0uy
    Assert.That (stream.Read(buffer, 0, size), Is.EqualTo size)
    use worker = new MemoryStream()
    worker.Write (buffer, 0, size)
    worker.Position <- 0L
    use before = new StreamReader (Assembly.GetExecutingAssembly().GetManifestResourceStream(self.resource))
    let item = Dictionary<string, Dictionary<int, int>>()
    item.Add("f6e3edb3-fb20-44b3-817d-f69d1a22fc2f", Dictionary<int,int>())
    self.UpdateReport item worker
    worker.Position <- 0L
    let after = new StreamReader(worker)
    let result = after.ReadToEnd().Split([|'\r';'\n'|], StringSplitOptions.RemoveEmptyEntries)
                 |> Seq.skip 3
    let expected = before.ReadToEnd().Split([|'\r';'\n'|], StringSplitOptions.RemoveEmptyEntries)
                 |> Seq.skip 3
    Assert.That (result,
                 Is.EquivalentTo expected))
    self.GetMyMethodName "<="

  [<Test>]
  member self.KnownModuleWithNothingInRangeMakesNoChange() =
    self.GetMyMethodName "=>"
    lock Instance.Visits (fun () ->
    Counter.measureTime <- DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)
    use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(self.resource)
    let size = int stream.Length
    let buffer = Array.create size 0uy
    Assert.That (stream.Read(buffer, 0, size), Is.EqualTo size)
    use worker = new MemoryStream()
    worker.Write (buffer, 0, size)
    worker.Position <- 0L
    use before = new StreamReader (Assembly.GetExecutingAssembly().GetManifestResourceStream(self.resource))
    let payload = Dictionary<int,int>()
    payload.[-1] <- 10
    payload.[100] <- 10
    let item = Dictionary<string, Dictionary<int, int>>()
    item.Add("f6e3edb3-fb20-44b3-817d-f69d1a22fc2f", payload)
    self.UpdateReport item worker
    worker.Position <- 0L
    let after = new StreamReader(worker)
    let result = after.ReadToEnd().Split([|'\r';'\n'|], StringSplitOptions.RemoveEmptyEntries)
                 |> Seq.skip 3
    let expected = before.ReadToEnd().Split([|'\r';'\n'|], StringSplitOptions.RemoveEmptyEntries)
                 |> Seq.skip 3
    Assert.That (result,
                 Is.EquivalentTo expected))
    self.GetMyMethodName "<="

  [<Test>]
  member self.KnownModuleWithPayloadMakesExpectedChange() =
    self.GetMyMethodName "=>"
    lock Instance.Visits (fun () ->
    Counter.measureTime <- DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)
    use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(self.resource)
    let size = int stream.Length
    let buffer = Array.create size 0uy
    Assert.That (stream.Read(buffer, 0, size), Is.EqualTo size)
    use worker = new MemoryStream()
    worker.Write (buffer, 0, size)
    worker.Position <- 0L
    let payload = Dictionary<int,int>()
    [0..9 ]
    |> Seq.iter(fun i -> payload.[i] <- (i+1))
    let item = Dictionary<string, Dictionary<int, int>>()
    item.Add("f6e3edb3-fb20-44b3-817d-f69d1a22fc2f", payload)
    self.UpdateReport item worker
    worker.Position <- 0L
    let after = XmlDocument()
    after.Load worker
    Assert.That( after.SelectNodes("//seqpnt")
                 |> Seq.cast<XmlElement>
                 |> Seq.map (fun x -> x.GetAttribute("visitcount")),
                 Is.EquivalentTo [ "11"; "10"; "9"; "8"; "7"; "6"; "4"; "3"; "2"; "1"]))
    self.GetMyMethodName "<="

  [<Test>]
  member self.KnownModuleWithPayloadMakesExpectedChangeInOpenCover() =
    self.GetMyMethodName "=>"
    lock Instance.Visits (fun () ->
    Counter.measureTime <- DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)
    use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(self.resource2)
    let size = int stream.Length
    let buffer = Array.create size 0uy
    Assert.That (stream.Read(buffer, 0, size), Is.EqualTo size)
    use worker = new MemoryStream()
    worker.Write (buffer, 0, size)
    worker.Position <- 0L
    let payload = Dictionary<int,int>()
    [0..9 ]
    |> Seq.iter(fun i -> payload.[10 - i] <- (i+1))
    let item = Dictionary<string, Dictionary<int, int>>()
    item.Add("7C-CD-66-29-A3-6C-6D-5F-A7-65-71-0E-22-7D-B2-61-B5-1F-65-9A", payload)
    Counter.UpdateReport true item ReportFormat.OpenCover worker |> ignore
    worker.Position <- 0L
    let after = XmlDocument()
    after.Load worker
    Assert.That( after.SelectNodes("//SequencePoint")
                 |> Seq.cast<XmlElement>
                 |> Seq.map (fun x -> x.GetAttribute("vc")),
                 Is.EquivalentTo [ "11"; "10"; "9"; "8"; "7"; "6"; "4"; "3"; "2"; "1"]))
    self.GetMyMethodName "<="

  [<Test>]
  member self.EmptyFlushLeavesNoTrace() =
    self.GetMyMethodName "=>"
    lock Instance.Visits (fun () ->
    let saved = Console.Out
    try
      Instance.Visits.Clear()
      use stdout = new StringWriter()
      Console.SetOut stdout

      Instance.FlushCounterImpl ProcessExit
      Assert.That (stdout.ToString(), Is.Empty)
    finally
      Instance.Visits.Clear()
      Console.SetOut saved)
    self.GetMyMethodName "<="

  member self.FlushLeavesExpectedTraces() =
    self.GetMyMethodName "=>"
    lock Instance.Visits (fun () ->
    try
      let saved = Console.Out
      let here = Directory.GetCurrentDirectory()
      let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
      let unique = Path.Combine(where, Guid.NewGuid().ToString())
      let save = Instance.trace
      Instance.trace <- { Tracer=null; Stream=null; Formatter=null }
      try
        Instance.Visits.Clear()
        use stdout = new StringWriter()
        Console.SetOut stdout
        Directory.CreateDirectory(unique) |> ignore
        Directory.SetCurrentDirectory(unique)

        Counter.measureTime <- DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)
        use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(self.resource)
        let size = int stream.Length
        let buffer = Array.create size 0uy
        Assert.That (stream.Read(buffer, 0, size), Is.EqualTo size)
        do
          use worker = new FileStream(Instance.ReportFile, FileMode.CreateNew)
          worker.Write(buffer, 0, size)
          ()

        let payload = Dictionary<int,int>()
        [0..9 ]
        |> Seq.iter(fun i -> payload.[i] <- (i+1))
        Instance.Visits.["f6e3edb3-fb20-44b3-817d-f69d1a22fc2f"] <- payload

        Instance.FlushCounter ProcessExit ()
        while Instance.Backlog () > 0 do
          Thread.Sleep 100

        // Restart the mailbox
        Instance.RunMailbox ()

        let head = "Coverage statistics flushing took "
        let tail = " seconds\n"
        let recorded = stdout.ToString().Replace("\r\n","\n")
        Assert.That (recorded.StartsWith(head, StringComparison.Ordinal))
        Assert.That (recorded.EndsWith(tail, StringComparison.Ordinal))
        use worker' = new FileStream(Instance.ReportFile, FileMode.Open)
        let after = XmlDocument()
        after.Load worker'
        Assert.That( after.SelectNodes("//seqpnt")
                     |> Seq.cast<XmlElement>
                     |> Seq.map (fun x -> x.GetAttribute("visitcount")),
                     Is.EquivalentTo [ "11"; "10"; "9"; "8"; "7"; "6"; "4"; "3"; "2"; "1"])
      finally
        Instance.trace <- save
        if File.Exists Instance.ReportFile then File.Delete Instance.ReportFile
        Instance.Visits.Clear()
        Console.SetOut saved
        Directory.SetCurrentDirectory(here)
        try
          Directory.Delete(unique)
        with
        | :? IOException -> ()
    with
    | :? AbandonedMutexException -> Instance.mutex.ReleaseMutex())

    self.GetMyMethodName "<="

#if NET2
#else
  // Dead simple sequential operation
  // run only once in Framework mode to avoid contention
  [<Test>]
  member self.MailboxFunctionsAsExpected() =
    self.RealIdShouldIncrementCount()
    self.FlushLeavesExpectedTraces()
#endif
end