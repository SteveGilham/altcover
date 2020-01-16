#if NETCOREAPP2_0
namespace Tests.Recorder.Core
#else
#if NET4
namespace Tests.Recorder.Clr4
#else
#if NET2
namespace Tests.Recorder.Clr2
#else
namespace Tests.Recorder.Unknown
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

module AltCoverTests =

  let test' x message =
    try
      test x
    with fail ->
      let extended = message + Environment.NewLine + fail.Message
#if NET2
      Assert.Fail(extended)
#else
      AssertionFailedException(extended, fail) |> raise
#endif

  [<MethodImpl(MethodImplOptions.NoInlining)>]
  let private GetMyMethodName tag = ignore tag
  //    let st = StackTrace(StackFrame(1))
  //    st.GetFrame(0).GetMethod().Name |>
  //#if NET2
  //    printfn "%s %s 2" tag
  //#else
  //    printfn "%s %s" tag
  //#endif

  let resource =
    Assembly.GetExecutingAssembly().GetManifestResourceNames()
    |> Seq.find (fun n -> n.EndsWith("SimpleCoverage.xml", StringComparison.Ordinal))

  let private UpdateReport a b =
    Adapter.UpdateReport(a, ReportFormat.NCover, b, b) |> ignore

  let private PointVisitInit a b = PointVisit.Init(a, b)

  let resource2 =
    Assembly.GetExecutingAssembly().GetManifestResourceNames()
    |> Seq.find
         (fun n ->
           n.EndsWith("Sample1WithModifiedOpenCover.xml", StringComparison.Ordinal))

  [<Test>]
  let ShouldBeAbleToGetTheDefaultReportFileName() =
    test <@ Instance.ReportFile = "Coverage.Default.xml" @>

  [<Test>]
  let SafeDisposalProtects() =
    let obj1 =
      { new System.IDisposable with
          member x.Dispose() = ObjectDisposedException("Bang!") |> raise }
    Assist.SafeDispose obj1
    test <@ true @>

  [<Test>]
  let DefaultAccessorsBehaveAsExpected() =
    let v1 = DateTime.UtcNow.Ticks
    let probe = Instance.Clock()
    let v2 = DateTime.UtcNow.Ticks
    test <@ Instance.Granularity() = 0L @>
    test <@ probe >= v1 @>
    test <@ probe <= v2 @>

  [<Test>]
  let ShouldBeLinkingTheCorrectCopyOfThisCode() =
    GetMyMethodName "=>"
    let tracer = Adapter.MakeNullTrace String.Empty

    test <@ tracer.GetType().Assembly.GetName().Name = "AltCover.Recorder" @>
    GetMyMethodName "<="

  [<Test>]
  let OnlyNewIdPairShouldBeSampled() =
    GetMyMethodName "=>"
    lock Adapter.Lock (fun () ->
      try
        Adapter.SamplesClear()
        test <@ Adapter.AddSample("module", 23) @>
        test <@ Adapter.AddSample("module", 24) @>
        test <@ Adapter.AddSample("newmodule", 23) @>
        test <@ Adapter.AddSample("module", 23) |> not @>
        test <@ Adapter.AddSampleUnconditional("module", 23) @>
      finally
        Adapter.SamplesClear())
    GetMyMethodName "<="

  let RealIdShouldIncrementCount() =
    GetMyMethodName "=>"
    lock Adapter.Lock (fun () ->
      let save = Instance.trace
      try
        Adapter.VisitsClear()
        Instance.trace <- Adapter.MakeNullTrace null

        let key = " "
        Instance.Recording <- false
        Instance.Visit("key", 17)
        Instance.Recording <- true
        Instance.CoverageFormat <- ReportFormat.NCover
        Instance.Visit(key, 23)
        Instance.CoverageFormat <- ReportFormat.OpenCoverWithTracking
        Instance.Visit(key, 23)
        test
          <@ Adapter.VisitsSeq()
             |> Seq.length = 1 @>
        test
          <@ Adapter.VisitsEntrySeq key
             |> Seq.length = 1 @>
        test <@ Adapter.VisitCount(key, 23) = 2L @>
      finally
        Instance.CoverageFormat <- ReportFormat.NCover
        Instance.Recording <- true
        Adapter.VisitsClear()
        Instance.trace <- save)
    GetMyMethodName "<="

  [<Test>]
  let JunkUspidGivesNegativeIndex() =
    let key = " "
    let index = Counter.FindIndexFromUspid(0, key)
    test <@ index < 0 @>

  [<Test>]
  let PayloadGeneratedIsAsExpected() =
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
        test
          <@ Adapter.PayloadSelection(1311693406324658740L, 1000L, true) =
               (Adapter.NewBoth(1311693406324658000L, 6789)) @>
      finally
        Instance.Pop()
      test <@ Adapter.PayloadSelector true = (Adapter.Call 4321) @>
    finally
      Instance.Pop()
    test
      <@ Adapter.PayloadSelection(1311693406324658740L, 1000L, true) =
           (Adapter.Time 1311693406324658000L) @>
    let v1 = DateTime.UtcNow.Ticks
    let probed = Adapter.PayloadControl(1000L, true)
    let v2 = DateTime.UtcNow.Ticks
    test
      <@ Adapter.Null()
         |> Adapter.untime
         |> Seq.isEmpty @>
    match Adapter.untime probed |> Seq.toList with
    | [ probe ] ->
        test <@ probe % 1000L = 0L @>
        test <@ probe <= v2 @>
        test <@ probe >= (1000L * (v1 / 1000L)) @>
    | _ -> test <@ false @>
    test <@ Instance.CallerId() = 0 @>
    Instance.Pop()
    test <@ Instance.CallerId() = 0 @>

  [<Test>]
  let RealIdShouldIncrementCountSynchronously() =
    GetMyMethodName "=>"
    lock Instance.Visits (fun () ->
      let save = Instance.trace
      Adapter.Reset()
      try
        Instance.Visits.Clear()
        Instance.trace <- Adapter.MakeNullTrace null
        let key = " "
        Instance.VisitSelection((Adapter.Null()), key, 23)
        Assert.That
          (Instance.Visits.Count, Is.EqualTo 1,
           "A visit that should have happened, didn't")
        Assert.That
          (Instance.Visits.[key].Count, Is.EqualTo 1,
           "keys = " + String.Join("; ", Instance.Visits.Keys |> Seq.toArray))
        Assert.That(Instance.Visits.[key].[23].Count, Is.EqualTo 1)
        Assert.That(Instance.Visits.[key].[23].Tracks, Is.Empty)
      finally
        Instance.Visits.Clear()
        Instance.trace <- save)
    GetMyMethodName "<="

  [<Test>]
  let ExceptionLoggedToFile() =
    let path = Instance.ReportFile |> Path.GetFullPath
    let where = path |> Path.GetDirectoryName
    let before = Directory.GetFiles(where, "*.exn")
    Instance.LogException("a", "b", "c", "ex")
    let after = Directory.GetFiles(where, "*.exn")
    Assert.That(after.Length, Is.GreaterThan before.Length)
    let all = HashSet<String>(after)
    before
    |> Seq.iter (fun x ->
         Assert.That(all.Contains x)
         all.Remove x |> ignore)
    Assert.That(all.Count, Is.EqualTo 1)
    let file = all |> Seq.head
    let lines = file |> File.ReadAllLines
    File.Delete file
    Assert.That(lines.Length, Is.GreaterThan 4)
    lines
    |> Seq.take 4
    |> Seq.zip
         [ "ModuleId = \"a\""; "hitPointId = \"b\""; "context = \"c\""; "exception = ex" ]
    |> Seq.iter (fun (a, b) -> Assert.That(a, Is.EqualTo b))
    let third = Directory.GetFiles(where, "*.exn")
    Assert.That(third.Length, Is.EqualTo before.Length)

  [<Test>]
  let Issue71WrapperHandlesKeyNotFoundException() =
    let pair = [| false; false |]
    let unique = System.Guid.NewGuid().ToString()
    Adapter.InvokeIssue71Wrapper<KeyNotFoundException>(unique, pair)
    Assert.That(pair |> Seq.head, Is.True)
    Assert.That(pair |> Seq.last, Is.True)

  [<Test>]
  let Issue71WrapperHandlesNullReferenceException() =
    let pair = [| false; false |]
    let unique = System.Guid.NewGuid().ToString()
    Adapter.InvokeIssue71Wrapper<NullReferenceException>(unique, pair)
    Assert.That(pair |> Seq.head, Is.True)
    Assert.That(pair |> Seq.last, Is.True)

  [<Test>]
  let Issue71WrapperHandlesArgumentNullException() =
    let pair = [| false; false |]
    let unique = System.Guid.NewGuid().ToString()
    Adapter.InvokeIssue71Wrapper<ArgumentNullException>(unique, pair)
    Assert.That(pair |> Seq.head, Is.True)
    Assert.That(pair |> Seq.last, Is.True)

  [<Test>]
  let Issue71WrapperDoesntHandleOtherException() =
    let pair = [| false; false |]
    let unique = System.Guid.NewGuid().ToString()
    let exn =
      Assert.Throws<InvalidOperationException>
        (fun () -> Adapter.InvokeIssue71Wrapper<InvalidOperationException>(unique, pair))
    Assert.That(pair |> Seq.head, Is.False)
    Assert.That(pair |> Seq.last, Is.False)
    Assert.That(exn.Message, Is.EqualTo unique)

#if NETCOREAPP2_0
  [<Test>]
  let NullRefShouldBeHandled() =
    GetMyMethodName "=>"
    let handle = Instance.Visits
    lock handle (fun () ->
      try
        Instance.Visits <- null
        let key = " "
        let path = Instance.ReportFile |> Path.GetFullPath
        let where = path |> Path.GetDirectoryName
        let before = Directory.GetFiles(where, "*.exn")
        Instance.VisitImpl(key, 23, (Adapter.Null()))
        let after = Directory.GetFiles(where, "*.exn")
        Assert.That(after.Length, Is.GreaterThan before.Length)
        let all = HashSet<String>(after)
        before
        |> Seq.iter (fun x ->
             Assert.That(all.Contains x)
             all.Remove x |> ignore)
        Assert.That(all.Count, Is.EqualTo 1)
        let file = all |> Seq.head
        let lines = file |> File.ReadAllLines
        File.Delete file
        Assert.That(lines.Length, Is.GreaterThan 4)
        lines
        |> Seq.take 4
        |> Seq.zip
             [ "ModuleId = \" \""; "hitPointId = 23"; "context = Null"; "exception = System.NullReferenceException: Object reference not set to an instance of an object." ]
        |> Seq.iter (fun (a, b) -> test <@ a = b @> )
        let third = Directory.GetFiles(where, "*.exn")
        Assert.That(third.Length, Is.EqualTo before.Length)
      finally
        Instance.Visits <- handle
        Instance.Visits.Clear())
    GetMyMethodName "<="
#endif

  [<Test>]
  let DistinctIdShouldBeDistinct() =
    GetMyMethodName "=>"
    lock Instance.Visits (fun () ->
      try
        Instance.Visits.Clear()
        let key = " "
        Instance.VisitImpl(key, 23, (Adapter.Null()))
        Instance.VisitImpl("key", 42, (Adapter.Null()))
        Assert.That(Instance.Visits.Count, Is.EqualTo 2)
      finally
        Instance.Visits.Clear())
    GetMyMethodName "<="

  [<Test>]
  let DistinctLineShouldBeDistinct() =
    GetMyMethodName "=>"
    lock Instance.Visits (fun () ->
      try
        Instance.Visits.Clear()
        let key = " "
        Instance.VisitImpl(key, 23, (Adapter.Null()))
        Instance.VisitImpl(key, 42, (Adapter.Null()))
        Assert.That(Instance.Visits.Count, Is.EqualTo 1)
        Assert.That(Instance.Visits.[key].Count, Is.EqualTo 2)
      finally
        Adapter.VisitsClear())
    GetMyMethodName "<="

  [<Test>]
  let RepeatVisitsShouldIncrementCount() =
    GetMyMethodName "=>"
    lock Instance.Visits (fun () ->
      Adapter.Reset()
      try
        Instance.Visits.Clear()
        let key = " "
        Instance.VisitImpl(key, 23, (Adapter.Null()))
        Instance.VisitImpl(key, 23, (Adapter.Null()))
        Assert.That(Instance.Visits.[key].[23].Count, Is.EqualTo 2)
        Assert.That(Instance.Visits.[key].[23].Tracks, Is.Empty)
      finally
        Instance.Visits.Clear())
    GetMyMethodName "<="

  [<Test>]
  let RepeatVisitsShouldIncrementTotal() =
    GetMyMethodName "=>"
    lock Instance.Visits (fun () ->
      Adapter.Reset()
      try
        Instance.Visits.Clear()
        let key = " "
        let payload = Adapter.Time DateTime.UtcNow.Ticks
        Instance.VisitImpl(key, 23, (Adapter.Null()))
        Instance.VisitImpl(key, 23, payload)
        Assert.That(Instance.Visits.[key].[23].Count, Is.EqualTo 1)
        Assert.That(Instance.Visits.[key].[23].Tracks, Is.EquivalentTo [ payload ])
      finally
        Instance.Visits.Clear())
    GetMyMethodName "<="

  [<Test>]
  let TabledVisitsShouldIncrementCount() =
    GetMyMethodName "=>"
    lock Instance.Visits (fun () ->
      Adapter.Reset()
      try
        Instance.Visits.Clear()
        let key = " "
        Instance.VisitImpl(key, 23, (Adapter.Null()))
        let table = Dictionary<string, Dictionary<int, PointVisit>>()
        table.Add(key, Dictionary<int, PointVisit>())
        let payloads =
          [ Adapter.Call 17
            Adapter.Time 23L
            Adapter.NewBoth(5L, 42) ]

        let pv = PointVisitInit 42L payloads
        table.[key].Add(23, pv)
        let n = Counter.AddTable(Instance.Visits, table)
        Assert.That(n, Is.EqualTo 45)
        Assert.That(Instance.Visits.[key].[23].Count, Is.EqualTo 43)
        Assert.That(Instance.Visits.[key].[23].Tracks, Is.EquivalentTo payloads)
      finally
        Instance.Visits.Clear())
    GetMyMethodName "<="

  [<Test>]
  let OldDocumentStartIsNotUpdated() =
    GetMyMethodName "=>"
    lock Instance.Visits (fun () ->
      let epoch = DateTime.UtcNow
      Counter.startTime <- epoch
      use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
      let size = int stream.Length
      let buffer = Array.create size 0uy
      Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)
      use worker = new MemoryStream()
      worker.Write(buffer, 0, size)
      worker.Position <- 0L
      let before = XmlDocument()
      before.Load(Assembly.GetExecutingAssembly().GetManifestResourceStream(resource))
      UpdateReport (Dictionary<string, Dictionary<int, PointVisit>>()) worker
      worker.Position <- 0L
      let after = XmlDocument()
      after.Load worker
      let startTimeAttr = after.DocumentElement.GetAttribute("startTime")
      let startTime = DateTime.ParseExact(startTimeAttr, "o", null)
      Assert.That(startTime.ToUniversalTime(), Is.LessThan epoch)
      Assert.That
        (startTime.ToUniversalTime(), Is.EqualTo(Counter.startTime.ToUniversalTime())))
    GetMyMethodName "<="

  [<Test>]
  let NewDocumentStartIsMadeEarlier() =
    GetMyMethodName "=>"
    lock Instance.Visits (fun () ->
      let epoch = DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)
      Counter.startTime <- epoch
      use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
      let size = int stream.Length
      let buffer = Array.create size 0uy
      Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)
      use worker = new MemoryStream()
      worker.Write(buffer, 0, size)
      worker.Position <- 0L
      let before = XmlDocument()
      before.Load(Assembly.GetExecutingAssembly().GetManifestResourceStream(resource))
      UpdateReport (Dictionary<string, Dictionary<int, PointVisit>>()) worker
      worker.Position <- 0L
      let after = XmlDocument()
      after.Load worker
      let startTimeAttr = after.DocumentElement.GetAttribute("startTime")
      let startTime = DateTime.ParseExact(startTimeAttr, "o", null)
      Assert.That(startTime.ToUniversalTime(), Is.EqualTo epoch)
      Assert.That
        (startTime.ToUniversalTime(), Is.EqualTo(Counter.startTime.ToUniversalTime())))
    GetMyMethodName "<="

  [<Test>]
  let NewDocumentMeasureIsNotMadeEarlier() =
    GetMyMethodName "=>"
    lock Instance.Visits (fun () ->
      let epoch = DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)
      Counter.measureTime <- epoch
      use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
      let size = int stream.Length
      let buffer = Array.create size 0uy
      Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)
      use worker = new MemoryStream()
      worker.Write(buffer, 0, size)
      worker.Position <- 0L
      let before = XmlDocument()
      before.Load(Assembly.GetExecutingAssembly().GetManifestResourceStream(resource))
      UpdateReport (Dictionary<string, Dictionary<int, PointVisit>>()) worker
      worker.Position <- 0L
      let after = XmlDocument()
      after.Load worker
      let startTimeAttr = after.DocumentElement.GetAttribute("measureTime")
      let startTime = DateTime.ParseExact(startTimeAttr, "o", null)
      Assert.That(startTime.ToUniversalTime(), Is.GreaterThan epoch)
      Assert.That
        (startTime.ToUniversalTime(), Is.EqualTo(Counter.measureTime.ToUniversalTime())))
    GetMyMethodName "<="

  [<Test>]
  let OldDocumentMeasureIsUpdated() =
    GetMyMethodName "=>"
    lock Instance.Visits (fun () ->
      let epoch = DateTime.UtcNow
      Counter.measureTime <- epoch
      use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
      let size = int stream.Length
      let buffer = Array.create size 0uy
      Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)
      use worker = new MemoryStream()
      worker.Write(buffer, 0, size)
      worker.Position <- 0L
      let before = XmlDocument()
      before.Load(Assembly.GetExecutingAssembly().GetManifestResourceStream(resource))
      UpdateReport (Dictionary<string, Dictionary<int, PointVisit>>()) worker
      worker.Position <- 0L
      let after = XmlDocument()
      after.Load worker
      let startTimeAttr = after.DocumentElement.GetAttribute("measureTime")
      let startTime = DateTime.ParseExact(startTimeAttr, "o", null)
      Assert.That(startTime.ToUniversalTime(), Is.EqualTo epoch)
      Assert.That
        (startTime.ToUniversalTime(), Is.EqualTo(Counter.measureTime.ToUniversalTime())))
    GetMyMethodName "<="

  [<Test>]
  let UnknownModuleMakesNoChange() =
    GetMyMethodName "=>"
    lock Instance.Visits (fun () ->
      Counter.measureTime <-
        DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)
      use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
      let size = int stream.Length
      let buffer = Array.create size 0uy
      Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)
      use worker = new MemoryStream()
      worker.Write(buffer, 0, size)
      worker.Position <- 0L
      use before =
        new StreamReader(Assembly.GetExecutingAssembly()
                                 .GetManifestResourceStream(resource))
      let item = Dictionary<string, Dictionary<int, PointVisit>>()
      item.Add("not a guid", null)
      UpdateReport item worker
      worker.Position <- 0L
      let after = new StreamReader(worker)
      let result =
        after.ReadToEnd().Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
        |> Seq.skip 3
      let expected =
        before.ReadToEnd().Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
        |> Seq.skip 3
      Assert.That(result, Is.EquivalentTo expected))
    GetMyMethodName "<="

  [<Test>]
  let KnownModuleWithNothingMakesNoChange() =
    GetMyMethodName "=>"
    lock Instance.Visits (fun () ->
      Counter.measureTime <-
        DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)
      use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
      let size = int stream.Length
      let buffer = Array.create size 0uy
      Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)
      use worker = new MemoryStream()
      worker.Write(buffer, 0, size)
      worker.Position <- 0L
      use before =
        new StreamReader(Assembly.GetExecutingAssembly()
                                 .GetManifestResourceStream(resource))
      let item = Dictionary<string, Dictionary<int, PointVisit>>()
      item.Add("f6e3edb3-fb20-44b3-817d-f69d1a22fc2f", Dictionary<int, PointVisit>())
      UpdateReport item worker
      worker.Position <- 0L
      let after = new StreamReader(worker)
      let result =
        after.ReadToEnd().Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
        |> Seq.skip 3
      let expected =
        before.ReadToEnd().Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
        |> Seq.skip 3
      Assert.That(result, Is.EquivalentTo expected))
    GetMyMethodName "<="

  [<Test>]
  let KnownModuleWithNothingInRangeMakesNoChange() =
    GetMyMethodName "=>"
    lock Instance.Visits (fun () ->
      Counter.measureTime <-
        DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)
      use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
      let size = int stream.Length
      let buffer = Array.create size 0uy
      Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)
      use worker = new MemoryStream()
      worker.Write(buffer, 0, size)
      worker.Position <- 0L
      use before =
        new StreamReader(Assembly.GetExecutingAssembly()
                                 .GetManifestResourceStream(resource))
      let payload = Dictionary<int, PointVisit>()
      payload.[-1] <- PointVisitInit 10L []
      payload.[100] <- PointVisitInit 10L []
      let item = Dictionary<string, Dictionary<int, PointVisit>>()
      item.Add("f6e3edb3-fb20-44b3-817d-f69d1a22fc2f", payload)
      UpdateReport item worker
      worker.Position <- 0L
      let after = new StreamReader(worker)
      let result =
        after.ReadToEnd().Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
        |> Seq.skip 3
      let expected =
        before.ReadToEnd().Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
        |> Seq.skip 3
      Assert.That(result, Is.EquivalentTo expected))
    GetMyMethodName "<="

  [<Test>]
  let KnownModuleWithPayloadMakesExpectedChange() =
    GetMyMethodName "=>"
    lock Instance.Visits (fun () ->
      Counter.measureTime <-
        DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)
      use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
      let size = int stream.Length
      let buffer = Array.create size 0uy
      Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)
      use worker = new MemoryStream()
      worker.Write(buffer, 0, size)
      worker.Position <- 0L
      let payload = Dictionary<int, PointVisit>()
      [ 0 .. 9 ] |> Seq.iter (fun i -> payload.[i] <- PointVisitInit (int64 (i + 1)) [])
      let item = Dictionary<string, Dictionary<int, PointVisit>>()
      item.Add("f6e3edb3-fb20-44b3-817d-f69d1a22fc2f", payload)
      UpdateReport item worker
      worker.Position <- 0L
      let after = XmlDocument()
      after.Load worker
      Assert.That
        (after.SelectNodes("//seqpnt")
         |> Seq.cast<XmlElement>
         |> Seq.map (fun x -> x.GetAttribute("visitcount")),
         Is.EquivalentTo [ "11"; "10"; "9"; "8"; "7"; "6"; "4"; "3"; "2"; "1" ]))
    GetMyMethodName "<="

  [<Test>]
  let KnownModuleWithPayloadMakesExpectedChangeInOpenCover() =
    GetMyMethodName "=>"
    lock Instance.Visits (fun () ->
      Counter.measureTime <-
        DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)
      use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)
      let size = int stream.Length
      let buffer = Array.create size 0uy
      Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)
      use worker = new MemoryStream()
      worker.Write(buffer, 0, size)
      worker.Position <- 0L
      let payload = Dictionary<int, PointVisit>()
      [ 0 .. 9 ]
      |> Seq.iter (fun i -> payload.[10 - i] <- PointVisitInit (int64 (i + 1)) [])
      [ 11 .. 12 ]
      |> Seq.iter (fun i ->
           payload.[i ||| Counter.BranchFlag] <- PointVisitInit (int64 (i - 10)) [])
      let item = Dictionary<string, Dictionary<int, PointVisit>>()
      item.Add("7C-CD-66-29-A3-6C-6D-5F-A7-65-71-0E-22-7D-B2-61-B5-1F-65-9A", payload)
      Adapter.UpdateReport(item, ReportFormat.OpenCover, worker, worker) |> ignore
      worker.Position <- 0L
      let after = XmlDocument()
      after.Load worker
      Assert.That
        (after.SelectNodes("//SequencePoint")
         |> Seq.cast<XmlElement>
         |> Seq.map (fun x -> x.GetAttribute("vc")),
         Is.EquivalentTo [ "11"; "10"; "9"; "8"; "7"; "6"; "4"; "3"; "2"; "1" ])
      Assert.That
        (after.SelectNodes("//BranchPoint")
         |> Seq.cast<XmlElement>
         |> Seq.map (fun x -> x.GetAttribute("vc")), Is.EquivalentTo [ "2"; "2" ]))
    GetMyMethodName "<="

  [<Test>]
  let EmptyFlushLeavesNoTrace() =
    GetMyMethodName "=>"
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
    GetMyMethodName "<="

  let PauseLeavesExpectedTraces() =
    GetMyMethodName "=>"
    lock Adapter.Lock (fun () ->
      try
        let saved = Console.Out
        let here = Directory.GetCurrentDirectory()
        let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
        let unique = Path.Combine(where, Guid.NewGuid().ToString())
        let save = Instance.trace
        use s = new MemoryStream()
        let s1 = new Compression.DeflateStream(s, CompressionMode.Compress)
        Instance.trace <- Adapter.MakeStreamTrace s1
        try
          Instance.IsRunner <- true
          Adapter.VisitsClear()
          use stdout = new StringWriter()
          Console.SetOut stdout
          Directory.CreateDirectory(unique) |> ignore
          Directory.SetCurrentDirectory(unique)
          Counter.measureTime <-
            DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)
          use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
          let size = int stream.Length
          let buffer = Array.create size 0uy
          Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)
          do use worker = new FileStream(Instance.ReportFile, FileMode.CreateNew)
             worker.Write(buffer, 0, size)
             ()
          [ 0 .. 9 ]
          |> Seq.iter (fun i ->
               Adapter.VisitsAdd
                 ("f6e3edb3-fb20-44b3-817d-f69d1a22fc2f", i, (int64 (i + 1))))
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
    GetMyMethodName "<="

  let ResumeLeavesExpectedTraces() =
    GetMyMethodName "=>"
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
          Counter.measureTime <-
            DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)
          use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
          let size = int stream.Length
          let buffer = Array.create size 0uy
          Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)
          do use worker = new FileStream(Instance.ReportFile, FileMode.CreateNew)
             worker.Write(buffer, 0, size)
             ()
          [ 0 .. 9 ]
          |> Seq.iter (fun i ->
               Adapter.VisitsAdd
                 ("f6e3edb3-fb20-44b3-817d-f69d1a22fc2f", i, (int64 (i + 1))))
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
    GetMyMethodName "<="

  let FlushLeavesExpectedTraces() =
    GetMyMethodName "=>"
    lock Adapter.Lock (fun () ->
      Instance.IsRunner <- false
      try
        let saved = Console.Out
        let here = Directory.GetCurrentDirectory()
        let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
        let unique = Path.Combine(where, Guid.NewGuid().ToString())
        let save = Instance.trace
        Instance.trace <- Adapter.MakeNullTrace null
        try
          Adapter.VisitsClear()
          use stdout = new StringWriter()
          Console.SetOut stdout
          Directory.CreateDirectory(unique) |> ignore
          Directory.SetCurrentDirectory(unique)
          Counter.measureTime <-
            DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)
          use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
          let size = int stream.Length
          let buffer = Array.create size 0uy
          Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)
          do use worker = new FileStream(Instance.ReportFile, FileMode.CreateNew)
             worker.Write(buffer, 0, size)
             ()
          [ 0 .. 9 ]
          |> Seq.iter (fun i ->
               Adapter.VisitsAdd
                 ("f6e3edb3-fb20-44b3-817d-f69d1a22fc2f", i, (int64 (i + 1))))
          Adapter.DoExit ()
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
    GetMyMethodName "<="

  [<Test>]
  let SupervisedFlushLeavesExpectedTraces() =
    GetMyMethodName "=>"
    lock Adapter.Lock (fun () ->
      try
        let saved = Console.Out
        let here = Directory.GetCurrentDirectory()
        let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
        let unique = Path.Combine(where, Guid.NewGuid().ToString())
        let save = Instance.trace
        Instance.trace <- Adapter.MakeNullTrace null

        Instance.Supervision <- true
        try
          Adapter.VisitsClear()
          use stdout = new StringWriter()
          Console.SetOut stdout
          Directory.CreateDirectory(unique) |> ignore
          Directory.SetCurrentDirectory(unique)
          Counter.measureTime <-
            DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)
          use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
          let size = int stream.Length
          let buffer = Array.create size 0uy
          Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)
          do use worker = new FileStream(Instance.ReportFile, FileMode.CreateNew)
             worker.Write(buffer, 0, size)
             ()
          [ 0 .. 9 ]
          |> Seq.iter (fun i ->
               Adapter.VisitsAdd
                 ("f6e3edb3-fb20-44b3-817d-f69d1a22fc2f", i, (int64 (i + 1))))
          Adapter.DoUnload ()
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
    GetMyMethodName "<="

  [<Test>]
  let FlushLeavesExpectedTracesWhenDiverted() =
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
      Counter.measureTime <-
        DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)
      use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
      let size = int stream.Length
      let buffer = Array.create size 0uy
      Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)
      do use worker = new FileStream(reportFile, FileMode.CreateNew)
         worker.Write(buffer, 0, size)
         ()
      let payload = Dictionary<int, PointVisit>()
      [ 0 .. 9 ] |> Seq.iter (fun i -> payload.[i] <- PointVisitInit (int64 (i + 1)) [])
      visits.["f6e3edb3-fb20-44b3-817d-f69d1a22fc2f"] <- payload
      Adapter.DoFlush
        (visits, AltCover.Recorder.ReportFormat.NCover, reportFile, outputFile) |> ignore
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
  let MailboxFunctionsAsExpected() =
    RealIdShouldIncrementCount()
    PauseLeavesExpectedTraces()
    ResumeLeavesExpectedTraces()
    FlushLeavesExpectedTraces()