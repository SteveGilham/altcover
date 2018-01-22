#if NETCOREAPP2_0
namespace Shadow.TestsCore
#else
#if NET4
namespace Shadow.Tests4
#else
#if NET2
namespace Shadow.Tests2
#else
#if MONO
namespace Shadow.TestsMono
#else
namespace Shadow.TestsUnknown
#endif
#endif
#endif
#endif

open System
open System.IO
open System.Reflection
#if NETCOREAPP2_0
open System.Threading
#endif
open System.Xml

open AltCover.Recorder
open NUnit.Framework
open System.Collections.Generic

[<TestFixture>]
type AltCoverTests() = class

  [<Test>]
  member self.ShouldBeLinkingTheCorrectCopyOfThisCode() =
    let locker = { Tracer = String.Empty
#if NETCOREAPP2_0
                   Pipe = null
                   Activated = null
#endif
    }
    Assert.That(locker.GetType().Assembly.GetName().Name, Is.EqualTo
#if NETCOREAPP2_0
    "AltCover.Recorder")
#else
    "AltCover.Shadow")
#endif

#if NET4
  // Doesn't work across framework boundaries, as the unit -> unit type
  // is rooted in a different runtime.  But the locking code gets executed
  // incidentally anyway in later tests.
#else
  // Do run .net2 to .net2, .netcore to .netcore (and Mono to Mono)
  [<Test>]
#endif
  member self.ShouldBeExecutingTheCorrectCopyOfThisCode() =
    let mutable where = ""
    Locking.WithLockerLocked self (fun () -> where <- Assembly.GetCallingAssembly().GetName().Name)
    Assert.That(where, Is.EqualTo
#if NETCOREAPP2_0
    "AltCover.Recorder")
#else
    "AltCover.Shadow")
#endif

#if NETCOREAPP2_0
  member self.PipeVisitShouldSignal() =
    let save = Instance.pipe
    let token = Guid.NewGuid().ToString() + "PipeVisitShouldSignal"
    printfn "token = %s" token
    use server = new System.IO.Pipes.NamedPipeServerStream(token)
    printfn "Created NamedPipeServerStream"
    try
      let client = Tracer.CreatePipe(token)
      printfn "Created client"
      try
        let expected = ("name", 23)
        let formatter = System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()
        use signal = new AutoResetEvent false
        async {
            try
              client.Connect 5000
              printfn "Connected."
            with
            | :? TimeoutException ->
                printfn "timed out"
            signal.Set() |> ignore
            } |> Async.Start
        server.WaitForConnection()
        signal.WaitOne() |> ignore
        printfn "after connection wait"
        Instance.pipe <- client
        Assert.That (Instance.pipe.IsConnected(), "connection failed")
        printfn "about to act"
        server.WriteByte(0uy)
        Assert.That(client.Activated.WaitOne(1000), "never got activated")
        Assert.That (Instance.pipe.IsActivated(), "activation failed")
        async { Instance.Visit "name" 23 } |> Async.Start
        printfn "about to read"
        let result = formatter.Deserialize(server) :?> (string*int)
        Assert.That (Instance.Visits, Is.Empty, "unexpected local write")
        Assert.That (result, Is.EqualTo expected, "unexpected result")
        printfn "after all work"
      finally
        printfn "finally 1"
        Instance.pipe.Close()
        Instance.pipe <- save
    finally
      printfn "finally 2"
      Instance.Visits.Clear()
    printfn "all done"
#endif

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

   member self.resource = Assembly.GetExecutingAssembly().GetManifestResourceNames()
                         |> Seq.find (fun n -> n.EndsWith("SimpleCoverage.xml", StringComparison.Ordinal))

  [<Test>]
  member self.OldDocumentStartIsNotUpdated() =
    let epoch = DateTime.UtcNow
    Instance.startTime <- epoch
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
    Assert.That (startTime.ToUniversalTime(), Is.EqualTo (Instance.startTime.ToUniversalTime()))

  [<Test>]
  member self.NewDocumentStartIsMadeEarlier() =
    let epoch = DateTime (1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)
    Instance.startTime <- epoch
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
    Assert.That (startTime.ToUniversalTime(), Is.EqualTo (Instance.startTime.ToUniversalTime()))

  [<Test>]
  member self.NewDocumentMeasureIsNotMadeEarlier() =
    let epoch = DateTime (1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)
    Instance.measureTime <- epoch
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
    Assert.That (startTime.ToUniversalTime(), Is.EqualTo (Instance.measureTime.ToUniversalTime()))

  [<Test>]
  member self.OldDocumentMeasureIsUpdated() =
    let epoch = DateTime.UtcNow
    Instance.measureTime <- epoch
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
    Assert.That (startTime.ToUniversalTime(), Is.EqualTo (Instance.measureTime.ToUniversalTime()))

  [<Test>]
  member self.UnknownModuleMakesNoChange() =
    Instance.measureTime <- DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)
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
    Assert.That (after.ReadToEnd().Replace("\r\n", "\n"),
                 Is.EqualTo (before.ReadToEnd().Replace("\r\n", "\n")))

  [<Test>]
  member self.KnownModuleWithNothingMakesNoChange() =
    Instance.measureTime <- DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)
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
    Assert.That (after.ReadToEnd().Replace("\r\n", "\n"),
                 Is.EqualTo (before.ReadToEnd().Replace("\r\n", "\n")))

  [<Test>]
  member self.KnownModuleWithNothingInRangeMakesNoChange() =
    Instance.measureTime <- DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)
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
    Assert.That (after.ReadToEnd().Replace("\r\n", "\n"),
                 Is.EqualTo (before.ReadToEnd().Replace("\r\n", "\n")))

  [<Test>]
  member self.KnownModuleWithPayloadMakesExpectedChange() =
    Instance.measureTime <- DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)
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
                 Is.EquivalentTo [ "11"; "10"; "9"; "8"; "7"; "6"; "4"; "3"; "2"; "1"])

  [<Test>]
  member self.EmptyFlushLeavesNoTrace() =
    let saved = Console.Out
    try
      Instance.Visits.Clear()
      use stdout = new StringWriter()
      Console.SetOut stdout

      Instance.FlushCounter true ()
      Assert.That (stdout.ToString(), Is.Empty)
    finally
      Instance.Visits.Clear()
      Console.SetOut saved

  [<Test>]
  member self.FlushLeavesExpectedTraces() =
    let saved = Console.Out
    let here = Directory.GetCurrentDirectory()
    let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
    let unique = Path.Combine(where, Guid.NewGuid().ToString())
    try
      Instance.Visits.Clear()
      use stdout = new StringWriter()
      Console.SetOut stdout
      Directory.CreateDirectory(unique) |> ignore
      Directory.SetCurrentDirectory(unique)

      Instance.measureTime <- DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)
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

      Instance.FlushCounter true ()

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
      if File.Exists Instance.ReportFile then File.Delete Instance.ReportFile
      Instance.Visits.Clear()
      Console.SetOut saved
      Directory.SetCurrentDirectory(here)
      try
        Directory.Delete(unique)
      with
      | :? IOException -> ()

#if NETCOREAPP2_0
  [<Test>]
  member self.PipeFlushShouldTidyUp() =
    // make these sequential in the simplest possible way
    self.PipeVisitShouldSignal()

    let save = Instance.pipe
    let token = Guid.NewGuid().ToString() + "PipeFlushShouldTidyUp"
    printfn "pipe token = %s" token
    use server = new System.IO.Pipes.NamedPipeServerStream(token)
    printfn "Created server"
    try
      let client = Tracer.CreatePipe token
      printfn "Created client"
      try
        let expected = ("name", 23)
        let formatter = System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()
        Instance.pipe <- client
        printfn "Ready to connect"
        use signal = new AutoResetEvent false
        async {
            try
              client.Connect 5000
              printfn "Connected."
            with
            | :? TimeoutException ->
                printfn "timed out"
            signal.Set() |> ignore
            } |> Async.Start
        server.WaitForConnection()
        signal.WaitOne() |> ignore
        printfn "After connection wait"
        Assert.That (Instance.pipe.IsConnected(), "connection failed")
        printfn "About to act"
        server.WriteByte(0uy)
        Assert.That(client.Activated.WaitOne(1000), "never got activated")
        Assert.That (Instance.pipe.IsActivated(), "activation failed")
        async { formatter.Serialize(Instance.pipe.Pipe, expected)
                Instance.FlushCounter true () } |> Async.Start
        printfn "About to read"
        let result = formatter.Deserialize(server) :?> (string*int)
        let result' = formatter.Deserialize(server) :?> (string*int)
        printfn "About to assert"
        Assert.That (Instance.Visits, Is.Empty, "unexpected local write")
        Assert.That (result, Is.EqualTo expected, "unexpected result")
        Assert.That (result' |> fst |> String.IsNullOrEmpty, Is.True, "unexpected end-of-message")
        printfn "done"
      finally
        printfn "first finally"
        Instance.pipe.Close()
        Instance.pipe <- save
    finally
      printfn "second finally"
      Instance.Visits.Clear()
    printfn "all done"

  [<Test>]
  member self.CoreFindsThePlace() =
    Assert.That (AltCover.Recorder.Tracer.Core(),
                 Does.EndWith("FSharp.Core.dll"))

  // The hack doesn't work in .net core
#else
  [<Test>]
  member self.FlushShouldBeRegisteredForUnload() =
   // The hack doesn't work in Mono, either
   let pdb = Path.ChangeExtension(Assembly.GetExecutingAssembly().Location, ".pdb")
   if File.Exists(pdb) then
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
   let pdb = Path.ChangeExtension(Assembly.GetExecutingAssembly().Location, ".pdb")
   if File.Exists(pdb) then
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