#if NETCOREAPP2_0
namespace Tests.Recorder.Core
#else
#if NET4
namespace Tests.Recorder.Clr4
#else
#if NET2
namespace Tests.Recorder.Clr2
#else
#if MONO
namespace Tests.Recorder.Mono
#else
namespace Tests.Recorder.Unknown
#endif
#endif
#endif
#endif

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.IO.Compression
open System.Reflection
open System.Runtime.CompilerServices
open System.Threading
open System.Xml

open AltCover.Recorder
open NUnit.Framework
open Swensen.Unquote

[<AutoOpen>]
module Helper =
  let ( <|| ) f (a,b) =
#if MONO
    f (a,b)
#else
    f a b
#endif

  let ( <||| ) f (a,b,c) =
#if MONO
    f (a,b,c)
#else
    f a b c
#endif

  let ( <|||| ) f (a,b,c,d) =
#if MONO
    f (a,b,c,d)
#else
    f a b c d
#endif

[<TestFixture>]
type AltCoverTests() =
  class

    let test' x message =
      try
        test x
      with fail ->
        let extended = message + Environment.NewLine + fail.Message
#if NET2
        Assert.Fail(extended)
#else
        AssertionFailedException(extended, fail)|> raise
#endif

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

    member self.resource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find (fun n -> n.EndsWith("SimpleCoverage.xml", StringComparison.Ordinal))

    member private self.UpdateReport a b =
      Adapter.UpdateReport <|||| (a, ReportFormat.NCover, b, b) |> ignore

    member private self.PointVisitInit a b =
      PointVisit.Init <|| (a, b)

    member self.resource2 =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find
           (fun n ->
           n.EndsWith("Sample1WithModifiedOpenCover.xml", StringComparison.Ordinal))

    [<Test>]
    member self.ShouldBeAbleToGetTheDefaultReportFileName() =
      test <@ Instance.ReportFile = "Coverage.Default.xml" @>

    [<Test>]
    member self.SafeDisposalProtects() =
      let obj1 =
        { new System.IDisposable with
            member x.Dispose() = ObjectDisposedException("Bang!") |> raise }
      Assist.SafeDispose obj1
      test <@ true @>

    [<Test>]
    member self.DefaultAccessorsBehaveAsExpected() =
      let v1 = DateTime.UtcNow.Ticks
      let probe = Instance.Clock()
      let v2 = DateTime.UtcNow.Ticks
      test <@ Instance.Granularity() = 0L @>
      test <@ probe >= v1 @>
      test <@ probe <= v2 @>

    [<Test>]
    member self.ShouldBeLinkingTheCorrectCopyOfThisCode() =
      self.GetMyMethodName "=>"
      let tracer =
#if MONO
                    Adapter.MakeNullTrace String.Empty
#else

        { Tracer = String.Empty
          Runner = false
          Definitive = false
          Stream = null
          Formatter = null }
#endif
      test <@ tracer.GetType().Assembly.GetName().Name = "AltCover.Recorder" @>
      self.GetMyMethodName "<="

    [<Test>]
    member self.OnlyNewIdPairShouldBeSampled() =
      self.GetMyMethodName "=>"
      lock Adapter.Lock (fun () ->
        try
          Adapter.SamplesClear()
          test <@ Adapter.AddSample <|| ("module", 23) @>
          test <@ Adapter.AddSample <|| ("module", 24) @>
          test <@ Adapter.AddSample <|| ("newmodule", 23) @>
          test <@ Adapter.AddSample <|| ("module", 23) |> not @>
          test <@ Adapter.AddSampleUnconditional <|| ("module", 23) @>
        finally
          Adapter.SamplesClear())
      self.GetMyMethodName "<="

    member self.RealIdShouldIncrementCount() =
      self.GetMyMethodName "=>"
      lock Adapter.Lock (fun () ->
        let save = Instance.trace
        try
          Adapter.VisitsClear()
#if MONO
          Instance.trace <- Adapter.MakeNullTrace null
#else
          Instance.trace <- { Tracer = null
                              Stream = null
                              Formatter = null
                              Runner = false
                              Definitive = false }
#endif
          let key = " "
          Instance.Recording <- false
          Instance.Visit <|| ("key", 17)
          Instance.Recording <- true
          Instance.CoverageFormat <- ReportFormat.NCover
          Instance.Visit <|| (key, 23)
          Instance.CoverageFormat <- ReportFormat.OpenCoverWithTracking
          Instance.Visit <|| (key, 23)
          test <@ Adapter.VisitsSeq()
                  |> Seq.length = 1 @>
          test <@ Adapter.VisitsEntrySeq key
                  |> Seq.length = 1 @>
          test <@ Adapter.VisitCount <|| (key, 23) = 2L @>
        finally
          Instance.CoverageFormat <- ReportFormat.NCover
          Instance.Recording <- true
          Adapter.VisitsClear()
          Instance.trace <- save)
      self.GetMyMethodName "<="

    [<Test>]
    member self.JunkUspidGivesNegativeIndex() =
      let key = " "
      let index = Counter.FindIndexFromUspid <|| (0, key)
      test <@ index < 0 @>

    [<Test>]
    member self.PayloadGeneratedIsAsExpected() =
      try
        test <@ Instance.CallerId() = 0 @>
        test <@ Adapter.PayloadSelector false = Adapter.Null() @>
        test <@ Adapter.PayloadSelector true = Adapter.Null() @>
        Instance.Push 4321
        test <@ Adapter.PayloadSelector false = Adapter.Null() @>
        test <@ Adapter.PayloadSelector true = (Adapter.Call 4321) @>
        try
          Instance.Push 6789
          // 0x1234123412341234 == 1311693406324658740
          test <@ Adapter.PayloadSelection <||| (0x1234123412341234L, 1000L, true)
                   = (Adapter.NewBoth <|| (1311693406324658000L, 6789)) @>
        finally
          Instance.Pop()
        test <@ Adapter.PayloadSelector true =
                    (Adapter.Call 4321) @>
      finally
        Instance.Pop()
      test <@ Adapter.PayloadSelection <||| (0x1234123412341234L, 1000L, true) =
                (Adapter.Time 1311693406324658000L) @>
      let v1 = DateTime.UtcNow.Ticks
      let probed = Adapter.PayloadControl <|| (1000L, true)
      let v2 = DateTime.UtcNow.Ticks
      test <@ Adapter.Null() |> Adapter.untime |> Seq.isEmpty @>
      match Adapter.untime probed |> Seq.toList with
      | [probe] ->
        test <@ probe % 1000L = 0L @>
        test <@ probe <= v2 @>
        test <@ probe >= (1000L*(v1/1000L)) @>
      | _ -> test <@ false @>
      test <@ Instance.CallerId() = 0 @>
      Instance.Pop()
      test <@ Instance.CallerId() = 0 @>

    [<Test>]
    member self.RealIdShouldIncrementCountSynchronously() =
      self.GetMyMethodName "=>"
      lock Instance.Visits (fun () ->
      let save = Instance.trace
      Adapter.Reset()
      try
        Instance.Visits.Clear()
#if MONO
        Instance.trace <- Adapter.MakeNullTrace null
#else
        Instance.trace <- { Tracer=null; Stream=null; Formatter=null;
                            Runner = false; Definitive = false }
#endif
        let key = " "
        Instance.VisitSelection <||| ((Adapter.Null()), key, 23)
        Assert.That (Instance.Visits.Count, Is.EqualTo 1, "A visit that should have happened, didn't")
        Assert.That (Instance.Visits.[key].Count, Is.EqualTo 1, "keys = " + String.Join("; ", Instance.Visits.Keys|> Seq.toArray))
        Assert.That (Instance.Visits.[key].[23].Count, Is.EqualTo 1)
        Assert.That (Instance.Visits.[key].[23].Tracks, Is.Empty)
      finally
        Instance.Visits.Clear()
        Instance.trace <- save)
      self.GetMyMethodName "<="

    [<Test>]
    member self.DistinctIdShouldBeDistinct() =
      self.GetMyMethodName "=>"
      lock Instance.Visits (fun () ->
      try
        Instance.Visits.Clear()
        let key = " "
        Instance.VisitImpl <||| (key, 23, (Adapter.Null()))
        Instance.VisitImpl <||| (null, 17, (Adapter.Null()))
        Instance.VisitImpl <||| ("key", 42, (Adapter.Null()))
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
        Instance.VisitImpl <||| (key, 23, (Adapter.Null()))
        Instance.VisitImpl <||| (key, 42, (Adapter.Null()))
        Assert.That (Instance.Visits.Count, Is.EqualTo 1)
        Assert.That (Instance.Visits.[key].Count, Is.EqualTo 2)
      finally
        Adapter.VisitsClear())
      self.GetMyMethodName "<="

    [<Test>]
    member self.RepeatVisitsShouldIncrementCount() =
      self.GetMyMethodName "=>"
      lock Instance.Visits (fun () ->
      Adapter.Reset()
      try
        Instance.Visits.Clear()
        let key = " "
        Instance.VisitImpl <||| (key, 23, (Adapter.Null()))
        Instance.VisitImpl <||| (key, 23, (Adapter.Null()))
        Assert.That (Instance.Visits.[key].[23].Count, Is.EqualTo 2)
        Assert.That (Instance.Visits.[key].[23].Tracks, Is.Empty)
      finally
        Instance.Visits.Clear())
      self.GetMyMethodName "<="

    [<Test>]
    member self.RepeatVisitsShouldIncrementTotal() =
      self.GetMyMethodName "=>"
      lock Instance.Visits (fun () ->
      Adapter.Reset()
      try
        Instance.Visits.Clear()
        let key = " "
        let payload = Adapter.Time DateTime.UtcNow.Ticks
        Instance.VisitImpl <||| (key, 23, (Adapter.Null()))
        Instance.VisitImpl <||| (key, 23, payload)
        Assert.That (Instance.Visits.[key].[23].Count, Is.EqualTo 1)
        Assert.That (Instance.Visits.[key].[23].Tracks, Is.EquivalentTo [payload])
      finally
        Instance.Visits.Clear())
      self.GetMyMethodName "<="

    [<Test>]
    member self.TabledVisitsShouldIncrementCount() =
      self.GetMyMethodName "=>"
      lock Instance.Visits (fun () ->
      Adapter.Reset()
      try
        Instance.Visits.Clear()
        let key = " "
        Instance.VisitImpl <||| (key, 23, (Adapter.Null()))
        let table = Dictionary<string, Dictionary<int, PointVisit>>()
        table.Add(key, Dictionary<int, PointVisit>())
        let payloads =
          [ Adapter.Call 17
            Adapter.Time 23L
            Adapter.NewBoth <|| (5L, 42) ]
        let pv = self.PointVisitInit 42L payloads
        table.[key].Add(23, pv)
        let n = Counter.AddTable <|| (Instance.Visits, table)
        Assert.That (n, Is.EqualTo 45)
        Assert.That (Instance.Visits.[key].[23].Count, Is.EqualTo 43)
        Assert.That (Instance.Visits.[key].[23].Tracks, Is.EquivalentTo payloads)
      finally
        Instance.Visits.Clear())
      self.GetMyMethodName "<="

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
      self.UpdateReport (Dictionary<string, Dictionary<int, PointVisit>>()) worker
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
      self.UpdateReport (Dictionary<string, Dictionary<int, PointVisit>>()) worker
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
      self.UpdateReport (Dictionary<string, Dictionary<int, PointVisit>>()) worker
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
      self.UpdateReport (Dictionary<string, Dictionary<int, PointVisit>>()) worker
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
      let item = Dictionary<string, Dictionary<int, PointVisit>>()
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
      let item = Dictionary<string, Dictionary<int, PointVisit>>()
      item.Add("f6e3edb3-fb20-44b3-817d-f69d1a22fc2f", Dictionary<int,PointVisit>())
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
      let payload = Dictionary<int,PointVisit>()
      payload.[-1] <- self.PointVisitInit 10L []
      payload.[100] <- self.PointVisitInit 10L []
      let item = Dictionary<string, Dictionary<int, PointVisit>>()
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
      let payload = Dictionary<int,PointVisit>()
      [0..9 ]
      |> Seq.iter(fun i -> payload.[i] <- self.PointVisitInit (int64(i+1)) [])
      let item = Dictionary<string, Dictionary<int, PointVisit>>()
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
      let payload = Dictionary<int,PointVisit>()
      [0..9 ]
      |> Seq.iter(fun i -> payload.[10 - i] <- self.PointVisitInit (int64(i+1)) [])
      [11..12]
      |> Seq.iter(fun i -> payload.[i ||| Counter.BranchFlag] <- self.PointVisitInit (int64(i-10)) [])
      let item = Dictionary<string, Dictionary<int, PointVisit>>()
      item.Add("7C-CD-66-29-A3-6C-6D-5F-A7-65-71-0E-22-7D-B2-61-B5-1F-65-9A", payload)
      Adapter.UpdateReport <|||| (item, ReportFormat.OpenCover, worker, worker) |> ignore
      worker.Position <- 0L
      let after = XmlDocument()
      after.Load worker
      Assert.That( after.SelectNodes("//SequencePoint")
                   |> Seq.cast<XmlElement>
                   |> Seq.map (fun x -> x.GetAttribute("vc")),
                   Is.EquivalentTo [ "11"; "10"; "9"; "8"; "7"; "6"; "4"; "3"; "2"; "1"])
      Assert.That( after.SelectNodes("//BranchPoint")
                   |> Seq.cast<XmlElement>
                   |> Seq.map (fun x -> x.GetAttribute("vc")),
                   Is.EquivalentTo [ "2"; "2"]))
      self.GetMyMethodName "<="

    [<Test>]
    member self.EmptyFlushLeavesNoTrace() =
      self.GetMyMethodName "=>"
      lock Adapter.Lock (fun () ->
        let saved = Console.Out
        try
          Adapter.VisitsClear()
          use stdout = new StringWriter()
          Console.SetOut stdout
          Adapter.FlushAll()
          Assert.That(stdout.ToString(), Is.Empty)
        finally
          Adapter.VisitsClear()
          Console.SetOut saved)
      self.GetMyMethodName "<="

    member self.PauseLeavesExpectedTraces() =
      self.GetMyMethodName "=>"
      lock Adapter.Lock (fun () ->
        try
          let saved = Console.Out
          let here = Directory.GetCurrentDirectory()
          let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
          let unique = Path.Combine(where, Guid.NewGuid().ToString())
          let save = Instance.trace
          use s = new MemoryStream()
          let s1 = new Compression.DeflateStream(s, CompressionMode.Compress)
#if MONO
          Instance.trace <- Adapter.MakeStreamTrace s1
#else
          Instance.trace <- { Tracer = null
                              Stream = new MemoryStream()
                              Formatter = new BinaryWriter(s1)
                              Runner = true
                              Definitive = false }
#endif
          try
            Instance.IsRunner <- true
            Adapter.VisitsClear()
            use stdout = new StringWriter()
            Console.SetOut stdout
            Directory.CreateDirectory(unique) |> ignore
            Directory.SetCurrentDirectory(unique)
            Counter.measureTime <- DateTime.ParseExact
                                     ("2017-12-29T16:33:40.9564026+00:00", "o", null)
            use stream =
              Assembly.GetExecutingAssembly().GetManifestResourceStream(self.resource)
            let size = int stream.Length
            let buffer = Array.create size 0uy
            Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)
            do use worker = new FileStream(Instance.ReportFile, FileMode.CreateNew)
               worker.Write(buffer, 0, size)
               ()
            [ 0..9 ]
            |> Seq.iter
                 (fun i ->
                 Adapter.VisitsAdd <||| ("f6e3edb3-fb20-44b3-817d-f69d1a22fc2f", i,
                   (int64 (i + 1))))
            Adapter.DoPause()
            Assert.That(Adapter.VisitsSeq(), Is.Empty)
            let recorded = stdout.ToString().Trim()
            Assert.That(recorded, Is.EqualTo "Pausing...")
            use worker' = new FileStream(Instance.ReportFile, FileMode.Open)
            let after = XmlDocument()
            after.Load worker'
            Assert.That
              (after.SelectNodes("//seqpnt")
               |> Seq.cast<XmlElement>
               |> Seq.map (fun x -> x.GetAttribute("visitcount")),
               Is.EquivalentTo
                 [ "1"; "1"; "1"; "1"; "1"; "1"; "0"; String.Empty; "X"; "-1" ])
          finally
            Instance.trace <- save
            if File.Exists Instance.ReportFile then File.Delete Instance.ReportFile
            Adapter.VisitsClear()
            Instance.IsRunner <- false
            Console.SetOut saved
            Directory.SetCurrentDirectory(here)
            try
              Directory.Delete(unique)
            with :? IOException -> ()
        with :? AbandonedMutexException -> Instance.mutex.ReleaseMutex())
      self.GetMyMethodName "<="

    member self.ResumeLeavesExpectedTraces() =
      self.GetMyMethodName "=>"
      lock Adapter.Lock (fun () ->
        try
          let saved = Console.Out
          let here = Directory.GetCurrentDirectory()
          let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
          let unique = Path.Combine(where, Guid.NewGuid().ToString())
          let save = Instance.trace
          let tag = unique + ".xml.acv"

          do use stream = File.Create tag
             ()

          try
            Adapter.Reset()
            Instance.trace <- Tracer.Create(tag)

            use stdout = new StringWriter()
            Console.SetOut stdout
            Directory.CreateDirectory(unique) |> ignore
            Directory.SetCurrentDirectory(unique)
            Counter.measureTime <- DateTime.ParseExact
                                     ("2017-12-29T16:33:40.9564026+00:00", "o", null)
            use stream =
              Assembly.GetExecutingAssembly().GetManifestResourceStream(self.resource)
            let size = int stream.Length
            let buffer = Array.create size 0uy
            Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)
            do use worker = new FileStream(Instance.ReportFile, FileMode.CreateNew)
               worker.Write(buffer, 0, size)
               ()
            [ 0..9 ]
            |> Seq.iter
                 (fun i ->
                 Adapter.VisitsAdd <||| ("f6e3edb3-fb20-44b3-817d-f69d1a22fc2f", i,
                   (int64 (i + 1))))
            Adapter.DoResume()
            Assert.That(Adapter.VisitsSeq(), Is.Empty, "Visits should be cleared")
            Assert.That
              (Object.ReferenceEquals(Instance.trace, save), Is.False,
               "trace should be replaced")
            let recorded = stdout.ToString().Trim()
            Assert.That(recorded, Is.EqualTo "Resuming...", recorded)
            use worker' = new FileStream(Instance.ReportFile, FileMode.Open)
            let after = XmlDocument()
            after.Load worker'
            Assert.That
              (after.SelectNodes("//seqpnt")
               |> Seq.cast<XmlElement>
               |> Seq.map (fun x -> x.GetAttribute("visitcount")),
               Is.EquivalentTo
                 [ "1"; "1"; "1"; "1"; "1"; "1"; "0"; String.Empty; "X"; "-1" ])
          finally
            Adapter.Reset()
            Instance.trace <- save
            if File.Exists Instance.ReportFile then File.Delete Instance.ReportFile
            Adapter.VisitsClear()
            Console.SetOut saved
            Directory.SetCurrentDirectory(here)
            File.Delete tag
            try
              Directory.Delete(unique)
            with :? IOException -> ()
        with :? AbandonedMutexException -> Instance.mutex.ReleaseMutex())
      self.GetMyMethodName "<="

    member self.FlushLeavesExpectedTraces() =
      self.GetMyMethodName "=>"
      lock Adapter.Lock (fun () ->
        Instance.IsRunner <- false
        try
          let saved = Console.Out
          let here = Directory.GetCurrentDirectory()
          let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
          let unique = Path.Combine(where, Guid.NewGuid().ToString())
          let save = Instance.trace
#if MONO
          Instance.trace <- Adapter.MakeNullTrace null
#else
          Instance.trace <- { Tracer = null
                              Stream = null
                              Formatter = null
                              Runner = false
                              Definitive = false }
#endif
          try
            Adapter.VisitsClear()
            use stdout = new StringWriter()
            Console.SetOut stdout
            Directory.CreateDirectory(unique) |> ignore
            Directory.SetCurrentDirectory(unique)
            Counter.measureTime <- DateTime.ParseExact
                                     ("2017-12-29T16:33:40.9564026+00:00", "o", null)
            use stream =
              Assembly.GetExecutingAssembly().GetManifestResourceStream(self.resource)
            let size = int stream.Length
            let buffer = Array.create size 0uy
            Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)
            do use worker = new FileStream(Instance.ReportFile, FileMode.CreateNew)
               worker.Write(buffer, 0, size)
               ()
            [ 0..9 ]
            |> Seq.iter
                 (fun i ->
                 Adapter.VisitsAdd <||| ("f6e3edb3-fb20-44b3-817d-f69d1a22fc2f", i,
                   (int64 (i + 1))))
            Instance.FlushCounter <|| (Adapter.ProcessExit(),  ())
            let head = "Coverage statistics flushing took "
            let tail = " seconds\n"
            let recorded = stdout.ToString().Replace("\r\n", "\n")
            let index1 = recorded.IndexOf(head, StringComparison.Ordinal)
            let index2 = recorded.IndexOf(tail, StringComparison.Ordinal)
            Assert.That(index1, Is.GreaterThanOrEqualTo 0, recorded)
            Assert.That(index2, Is.GreaterThan index1, recorded)
            use worker' = new FileStream(Instance.ReportFile, FileMode.Open)
            let after = XmlDocument()
            after.Load worker'
            Assert.That
              (after.SelectNodes("//seqpnt")
               |> Seq.cast<XmlElement>
               |> Seq.map (fun x -> x.GetAttribute("visitcount")),
               Is.EquivalentTo [ "11"; "10"; "9"; "8"; "7"; "6"; "4"; "3"; "2"; "1" ])
          finally
            Instance.trace <- save
            if File.Exists Instance.ReportFile then File.Delete Instance.ReportFile
            Adapter.VisitsClear()
            Console.SetOut saved
            Directory.SetCurrentDirectory(here)
            try
              Directory.Delete(unique)
            with :? IOException -> ()
        with :? AbandonedMutexException -> Instance.mutex.ReleaseMutex())
      self.GetMyMethodName "<="

    [<Test>]
    member self.SupervisedFlushLeavesExpectedTraces() =
      self.GetMyMethodName "=>"
      lock Adapter.Lock (fun () ->
        try
          let saved = Console.Out
          let here = Directory.GetCurrentDirectory()
          let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
          let unique = Path.Combine(where, Guid.NewGuid().ToString())
          let save = Instance.trace
#if MONO
          Instance.trace <- Adapter.MakeNullTrace null
#else
          Instance.trace <- { Tracer = null
                              Stream = null
                              Formatter = null
                              Runner = false
                              Definitive = false }
#endif
          Instance.Supervision <- true
          try
            Adapter.VisitsClear()
            use stdout = new StringWriter()
            Console.SetOut stdout
            Directory.CreateDirectory(unique) |> ignore
            Directory.SetCurrentDirectory(unique)
            Counter.measureTime <- DateTime.ParseExact
                                     ("2017-12-29T16:33:40.9564026+00:00", "o", null)
            use stream =
              Assembly.GetExecutingAssembly().GetManifestResourceStream(self.resource)
            let size = int stream.Length
            let buffer = Array.create size 0uy
            Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)
            do use worker = new FileStream(Instance.ReportFile, FileMode.CreateNew)
               worker.Write(buffer, 0, size)
               ()
            [ 0..9 ]
            |> Seq.iter
                 (fun i ->
                 Adapter.VisitsAdd <||| ("f6e3edb3-fb20-44b3-817d-f69d1a22fc2f", i,
                   (int64 (i + 1))))
            Instance.FlushCounter <|| (Adapter.ProcessExit(),  ())
            let head = "Coverage statistics flushing took "
            let tail = " seconds\n"
            let recorded = stdout.ToString().Replace("\r\n", "\n")
            Assert.That(recorded, Is.Empty, recorded)
            use worker' = new FileStream(Instance.ReportFile, FileMode.Open)
            let after = XmlDocument()
            after.Load worker'
            Assert.That
              (after.SelectNodes("//seqpnt")
               |> Seq.cast<XmlElement>
               |> Seq.map (fun x -> x.GetAttribute("visitcount")),
               Is.EquivalentTo
                 [ "1"; "1"; "1"; "1"; "1"; "1"; "0"; String.Empty; "X"; "-1" ])
          finally
            Instance.trace <- save
            Instance.Supervision <- false
            if File.Exists Instance.ReportFile then File.Delete Instance.ReportFile
            Adapter.VisitsClear()
            Console.SetOut saved
            Directory.SetCurrentDirectory(here)
            try
              Directory.Delete(unique)
            with :? IOException -> ()
        with :? AbandonedMutexException -> Instance.mutex.ReleaseMutex())
      self.GetMyMethodName "<="

    [<Test>]
    member self.FlushLeavesExpectedTracesWhenDiverted() =
      let saved = Console.Out
      let here = Directory.GetCurrentDirectory()
      let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
      let unique = Path.Combine(where, Guid.NewGuid().ToString())
      let reportFile = Path.Combine(unique, "FlushLeavesExpectedTraces.xml")
      let outputFile = Path.Combine(unique, "FlushLeavesExpectedTracesWhenDiverted.xml")
      try
        let visits = new Dictionary<string, Dictionary<int, PointVisit>>()
        use stdout = new StringWriter()
        Console.SetOut stdout
        Directory.CreateDirectory(unique) |> ignore
        Directory.SetCurrentDirectory(unique)
        Counter.measureTime <- DateTime.ParseExact
                                 ("2017-12-29T16:33:40.9564026+00:00", "o", null)
        use stream =
          Assembly.GetExecutingAssembly().GetManifestResourceStream(self.resource)
        let size = int stream.Length
        let buffer = Array.create size 0uy
        Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)
        do use worker = new FileStream(reportFile, FileMode.CreateNew)
           worker.Write(buffer, 0, size)
           ()
        let payload = Dictionary<int, PointVisit>()
        [ 0..9 ] |> Seq.iter (fun i -> payload.[i] <- self.PointVisitInit (int64 (i + 1)) [])
        visits.["f6e3edb3-fb20-44b3-817d-f69d1a22fc2f"] <- payload
        Adapter.DoFlush <|||| (visits,
          AltCover.Recorder.ReportFormat.NCover, reportFile, outputFile) |> ignore
        use worker' = new FileStream(outputFile, FileMode.Open)
        let after = XmlDocument()
        after.Load worker'
        Assert.That
          (after.SelectNodes("//seqpnt")
           |> Seq.cast<XmlElement>
           |> Seq.map (fun x -> x.GetAttribute("visitcount")),
           Is.EquivalentTo [ "11"; "10"; "9"; "8"; "7"; "6"; "4"; "3"; "2"; "1" ])
      finally
        if File.Exists reportFile then File.Delete reportFile
        Console.SetOut saved
        Directory.SetCurrentDirectory(here)
        try
          Directory.Delete(unique)
        with :? IOException -> ()

    // Dead simple sequential operation
    // run only once in Framework mode to avoid contention
    [<Test>]
    member self.MailboxFunctionsAsExpected() =
      self.RealIdShouldIncrementCount()
      self.PauseLeavesExpectedTraces()
      self.ResumeLeavesExpectedTraces()
      self.FlushLeavesExpectedTraces()

  end