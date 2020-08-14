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

// fsharplint:disable  MemberNames NonPublicValuesNames RedundantNewKeyword

open System
open System.Collections.Generic
open System.IO
open System.IO.Compression
open System.Reflection
open System.Runtime.CompilerServices
open System.Threading
open System.Xml

open AltCover.Recorder
open NUnit.Framework

#nowarn "25" // partial pattern match

module AltCoverTests =

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

  let private PointVisitInit a b = Adapter.Init(a, b)

  let resource2 =
    Assembly.GetExecutingAssembly().GetManifestResourceNames()
    |> Seq.find
         (fun n ->
           n.EndsWith("Sample1WithModifiedOpenCover.xml", StringComparison.Ordinal))

  [<Test>]
  let ShouldBeAbleToGetTheDefaultReportFileName() =
    Assert.True( Instance.ReportFile = "Coverage.Default.xml" )

  [<Test>]
  let DefaultAccessorsBehaveAsExpected() =
    let v1 = DateTime.UtcNow.Ticks
    let probe = Instance.I.clock()
    let v2 = DateTime.UtcNow.Ticks
    Assert.True( Instance.I.granularity() = 0L )
    Assert.True( probe >= v1 )
    Assert.True( probe <= v2 )

  [<Test>]
  let ShouldBeLinkingTheCorrectCopyOfThisCode() =
    GetMyMethodName "=>"
    let tracer = Adapter.MakeNullTrace String.Empty

    Assert.True( tracer.GetType().Assembly.GetName().Name = "AltCover.Recorder" )
    GetMyMethodName "<="

  [<Test>]
  let OnlyNewIdPairShouldBeSampled() =
    GetMyMethodName "=>"
    lock Adapter.Lock (fun () ->
      try
        Adapter.SamplesClear()
        Assert.True( Adapter.AddSample("module", 23) )
        Assert.True( Adapter.AddSample("module", 24) )
        Assert.True( Adapter.AddSample("newmodule", 23) )
        Assert.True( Adapter.AddSample("module", 23) |> not )
        Assert.True( Adapter.AddSampleUnconditional("module", 23) )
      finally
        Adapter.SamplesClear())
    GetMyMethodName "<="

  let RealIdShouldIncrementCount() =
    GetMyMethodName "=>"
    lock Adapter.Lock (fun () ->
      let save = Instance.I.trace
      try
        Adapter.VisitsClear()
        Instance.I.trace <- Adapter.MakeNullTrace null

        let key = " "
        Instance.I.recording <- false
        Instance.Visit "key" 17
        Instance.I.recording <- true
        Instance.CoverageFormat <- ReportFormat.NCover
        Instance.Visit key 23
        Instance.CoverageFormat <- ReportFormat.OpenCoverWithTracking
        Instance.Visit key 23
        Assert.True( Adapter.VisitsSeq()
                     |> Seq.length = 1 )
        Assert.True( Adapter.VisitsEntrySeq key
                     |> Seq.length = 1 )
        Assert.True( Adapter.VisitCount(key, 23) = 2L )
      finally
        Instance.CoverageFormat <- ReportFormat.NCover
        Instance.I.recording <- true
        Adapter.VisitsClear()
        Instance.I.trace <- save)
    GetMyMethodName "<="

  [<Test>]
  let JunkUspidGivesNegativeIndex() =
    let key = " "
    let index = Counter.I.findIndexFromUspid 0 key
    Assert.True( index < 0 )

  [<Test>]
  let PayloadGeneratedIsAsExpected() =
    try
      Assert.True( Instance.I.callerId() = 0 )
      Assert.True( Adapter.PayloadSelector false = Adapter.Null() )
      Assert.True( Adapter.PayloadSelector true = Adapter.Null() )
      Instance.Push 4321
      Assert.True( Adapter.PayloadSelector false = Adapter.Null() )
      Assert.True( Adapter.PayloadSelector true = (Adapter.Call 4321) )
      try
        Instance.Push 6789
        // 0x1234123412341234 == 1311693406324658740
        let result = Adapter.PayloadSelection(1311693406324658740L, 1000L, true)
        let expected = Adapter.NewBoth(1311693406324658000L, 6789)
        Assert.True(( result = expected ))
      finally
        Instance.Pop()
      Assert.True(( Adapter.PayloadSelector true = (Adapter.Call 4321) ))
    finally
      Instance.Pop()

    let result2 = Adapter.PayloadSelection(1311693406324658740L, 1000L, true)
    let expected2 = Adapter.Time 1311693406324658000L
    Assert.True(( result2 = expected2 ))
    let v1 = DateTime.UtcNow.Ticks
    let probed = Adapter.PayloadControl(1000L, true)
    let v2 = DateTime.UtcNow.Ticks
    Assert.True( Adapter.Null()
                 |> Adapter.untime
                 |> Seq.isEmpty )
    let [ probe ] =  Adapter.untime probed |> Seq.toList
    Assert.True( probe % 1000L = 0L )
    Assert.True( probe <= v2 )
    Assert.True( probe >= (1000L * (v1 / 1000L)) )
    Assert.True( Instance.I.callerId() = 0 )
    Instance.Pop()
    Assert.True( Instance.I.callerId() = 0 )

  [<Test>]
  let RealIdShouldIncrementCountSynchronously() =
    GetMyMethodName "=>"
    lock Instance.I.visits (fun () ->
      let save = Instance.I.trace
      Adapter.Reset()
      try
        Instance.I.visits.Clear()
        Instance.I.trace <- Adapter.MakeNullTrace null
        let key = " "
        Instance.I.visitSelection (Adapter.Null()) key 23
        Assert.That
          (Instance.I.visits.Count, Is.EqualTo 1,
           "A visit that should have happened, didn't")
        Assert.That
          (Instance.I.visits.[key].Count, Is.EqualTo 1,
           "keys = " + String.Join("; ", Instance.I.visits.Keys |> Seq.toArray))
        Assert.That(Instance.I.visits.[key].[23].Count, Is.EqualTo 1)
        Assert.That(Instance.I.visits.[key].[23].Tracks, Is.Empty)
      finally
        Instance.I.visits.Clear()
        Instance.I.trace <- save)
    GetMyMethodName "<="

  let strip before (all:HashSet<String>) =
        before
        |> Seq.iter (fun x ->
             Assert.That(all.Contains x)
             all.Remove x |> ignore)

  [<Test>]
  let StripWorks() =
    let b1 = [ "1" ]
    let a1 = [ "1"; "2" ]
    let a1' = HashSet<String>(a1)
    strip b1 a1'
    let stripped1 = a1' |> Seq.toList
    Assert.That(stripped1, Is.EquivalentTo [ "2" ])
    Assert.Throws<AssertionException>( fun () ->  strip b1 a1') |> ignore

  [<Test>]
  let ExceptionLoggedToFile() =
    let path = Instance.ReportFile |> Path.GetFullPath
    let where = path |> Path.GetDirectoryName
    let before = Directory.GetFiles(where, "*.exn")
    Instance.I.logException "a" "b" "c" "ex"
    let after = Directory.GetFiles(where, "*.exn")
    Assert.That(after.Length, Is.GreaterThan before.Length)
    let all = HashSet<String>(after)
    strip before all
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
    let handle = Instance.I.visits
    lock handle (fun () ->
      try
        Instance.I.visits <- null
        let key = " "
        let path = Instance.ReportFile |> Path.GetFullPath
        let where = path |> Path.GetDirectoryName
        let before = Directory.GetFiles(where, "*.exn")
        Instance.I.visitImpl key 23 (Adapter.Null())
        let after = Directory.GetFiles(where, "*.exn")
        Assert.That(after.Length, Is.GreaterThan before.Length)
        let all = HashSet<String>(after)
        strip before all

        Assert.That(all.Count, Is.EqualTo 1)
        let file = all |> Seq.head
        let lines = file |> File.ReadAllLines
        File.Delete file
        Assert.That(lines.Length, Is.GreaterThan 4)
        lines
        |> Seq.take 4
        |> Seq.zip
             [ "ModuleId = \" \""; "hitPointId = 23"; "context = Null"; "exception = System.NullReferenceException: Object reference not set to an instance of an object." ]
        |> Seq.iter (fun (a, b) -> Assert.True(( a = b )) )
        let third = Directory.GetFiles(where, "*.exn")
        Assert.That(third.Length, Is.EqualTo before.Length)
      finally
        Instance.I.visits <- handle
        Instance.I.visits.Clear())
    GetMyMethodName "<="
#endif

  [<Test>]
  let DistinctIdShouldBeDistinct() =
    GetMyMethodName "=>"
    lock Instance.I.visits (fun () ->
      try
        Instance.I.visits.Clear()
        let key = " "
        Instance.I.visitImpl key 23 (Adapter.Null())
        Instance.I.visitImpl "key" 42 (Adapter.Null())
        Assert.That(Instance.I.visits.Count, Is.EqualTo 2)
      finally
        Instance.I.visits.Clear())
    GetMyMethodName "<="

  [<Test>]
  let DistinctLineShouldBeDistinct() =
    GetMyMethodName "=>"
    lock Instance.I.visits (fun () ->
      try
        Instance.I.visits.Clear()
        let key = " "
        Instance.I.visitImpl key 23 (Adapter.Null())
        Instance.I.visitImpl key 42 (Adapter.Null())
        Assert.That(Instance.I.visits.Count, Is.EqualTo 1)
        Assert.That(Instance.I.visits.[key].Count, Is.EqualTo 2)
      finally
        Adapter.VisitsClear())
    GetMyMethodName "<="

  [<Test>]
  let RepeatVisitsShouldIncrementCount() =
    GetMyMethodName "=>"
    lock Instance.I.visits (fun () ->
      Adapter.Reset()
      try
        Instance.I.visits.Clear()
        let key = " "
        Instance.I.visitImpl key 23 (Adapter.Null())
        Instance.I.visitImpl key 23 (Adapter.Null())
        Assert.That(Instance.I.visits.[key].[23].Count, Is.EqualTo 2)
        Assert.That(Instance.I.visits.[key].[23].Tracks, Is.Empty)
      finally
        Instance.I.visits.Clear())
    GetMyMethodName "<="

  [<Test>]
  let RepeatVisitsShouldIncrementTotal() =
    GetMyMethodName "=>"
    lock Instance.I.visits (fun () ->
      Adapter.Reset()
      try
        Instance.I.visits.Clear()
        let key = " "
        let payload = Adapter.Time DateTime.UtcNow.Ticks
        Instance.I.visitImpl key 23 (Adapter.Null())
        Instance.I.visitImpl key 23 payload
        Assert.That(Instance.I.visits.[key].[23].Count, Is.EqualTo 1)
        Assert.That(Instance.I.visits.[key].[23].Tracks, Is.EquivalentTo [ payload ])
      finally
        Instance.I.visits.Clear())
    GetMyMethodName "<="

  //[<Test>]
  //let TabledVisitsShouldIncrementCount() =
  //  GetMyMethodName "=>"
  //  lock Instance.I.visits (fun () ->
  //    Adapter.Reset()
  //    try
  //      Instance.I.visits.Clear()
  //      let key = " "
  //      Instance.I.visitImpl(key, 23, (Adapter.Null()))
  //      let table = Dictionary<string, Dictionary<int, PointVisit>>()
  //      table.Add(key, Dictionary<int, PointVisit>())
  //      let payloads =
  //        [ Adapter.Call 17
  //          Adapter.Time 23L
  //          Adapter.NewBoth(5L, 42) ]

  //      let pv = PointVisitInit 42L payloads
  //      table.[key].Add(23, pv)
  //      let n = Counter.AddTable(Instance.I.visits, table)
  //      Assert.That(n, Is.EqualTo 45)
  //      Assert.That(Instance.I.visits.[key].[23].Count, Is.EqualTo 43)
  //      Assert.That(Instance.I.visits.[key].[23].Tracks, Is.EquivalentTo payloads)
  //    finally
  //      Instance.I.visits.Clear())
  //  GetMyMethodName "<="

  [<Test>]
  let OldDocumentStartIsNotUpdated() =
    GetMyMethodName "=>"
    lock Instance.I.visits (fun () ->
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
    lock Instance.I.visits (fun () ->
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
    lock Instance.I.visits (fun () ->
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
    lock Instance.I.visits (fun () ->
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
    lock Instance.I.visits (fun () ->
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
    lock Instance.I.visits (fun () ->
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
    lock Instance.I.visits (fun () ->
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
    lock Instance.I.visits (fun () ->
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
    lock Instance.I.visits (fun () ->
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
           payload.[i ||| Counter.branchFlag] <- PointVisitInit (int64 (i - 10)) [])
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
        Instance.FlushFinish()
        Assert.That(stdout.ToString(), Is.Empty)
      finally
        Adapter.VisitsClear()
        Console.SetOut saved)
    GetMyMethodName "<="

  let trywith<'a when 'a :> exn> f g =
    try
      f()
    with
    | :? 'a -> g()

  let trywithrelease<'a when 'a :> exn> f =
    trywith f Instance.I.mutex.ReleaseMutex

  [<Test>]
  let CanTryWith() =
    let mutable flag = false
    let setFlag =  (fun () -> flag <- true)
    trywith<InvalidOperationException>
      (fun () -> ())
      setFlag
    Assert.That(flag, Is.False)

    trywith<InvalidOperationException>
      (fun () -> InvalidOperationException() |> raise)
      setFlag
    Assert.That(flag, Is.True)

    Instance.I.mutex.WaitOne(1000) |> ignore
    trywithrelease<InvalidOperationException>
      (fun () -> InvalidOperationException() |> raise)

  let PauseLeavesExpectedTraces() =
    GetMyMethodName "=>"
    lock Adapter.Lock (fun () ->
      trywithrelease (fun () ->
        let saved = Console.Out
        let here = Directory.GetCurrentDirectory()
        let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
        let unique = Path.Combine(where, Guid.NewGuid().ToString())
        let save = Instance.I.trace
        use s = new MemoryStream()
        let s1 = new Compression.DeflateStream(s, CompressionMode.Compress)
        Instance.I.trace <- Adapter.MakeStreamTrace s1
        try
          Instance.I.isRunner <- true
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
          Adapter.DoPause().Invoke(null, null)
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
          Instance.I.trace <- save
          AltCoverCoreTests.maybeDeleteFile Instance.ReportFile
          Adapter.VisitsClear()
          Instance.I.isRunner <- false
          Console.SetOut saved
          Directory.SetCurrentDirectory(here)
          AltCoverCoreTests.maybeIOException
            (fun () -> Directory.Delete(unique))
      ))
    GetMyMethodName "<="

  let ResumeLeavesExpectedTraces() =
    GetMyMethodName "=>"
    lock Adapter.Lock (fun () ->
      trywithrelease (fun () ->
        let saved = Console.Out
        let here = Directory.GetCurrentDirectory()
        let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
        let unique = Path.Combine(where, Guid.NewGuid().ToString())
        let save = Instance.I.trace
        let tag = unique + ".xml.acv"

        do use stream = File.Create tag
           ()

        try
          Adapter.Reset()
          Instance.I.trace <- Tracer.Create(tag)

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
          Adapter.DoResume().Invoke(null, null)
          Assert.That(Adapter.VisitsSeq(), Is.Empty, "Visits should be cleared")
          Assert.That
            (Object.ReferenceEquals(Instance.I.trace, save), Is.False,
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
          Instance.I.trace <- save
          AltCoverCoreTests.maybeDeleteFile Instance.ReportFile
          Adapter.VisitsClear()
          Console.SetOut saved
          Directory.SetCurrentDirectory(here)
          File.Delete tag
          AltCoverCoreTests.maybeIOException
            (fun () -> Directory.Delete(unique))
      ))
    GetMyMethodName "<="

  let FlushLeavesExpectedTraces() =
    GetMyMethodName "=>"
    lock Adapter.Lock (fun () ->
      Instance.I.isRunner <- false
      trywithrelease (fun () ->
        let saved = Console.Out
        let here = Directory.GetCurrentDirectory()
        let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
        let unique = Path.Combine(where, Guid.NewGuid().ToString())
        let save = Instance.I.trace
        Instance.I.trace <- Adapter.MakeNullTrace null
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
          Adapter.DoExit().Invoke(null, null)
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
          Instance.I.trace <- save
          AltCoverCoreTests.maybeDeleteFile Instance.ReportFile
          Adapter.VisitsClear()
          Console.SetOut saved
          Directory.SetCurrentDirectory(here)
          AltCoverCoreTests.maybeIOException
            (fun () -> Directory.Delete(unique))
      ))
    GetMyMethodName "<="

  [<Test>]
  let SupervisedFlushLeavesExpectedTraces() =
    GetMyMethodName "=>"
    lock Adapter.Lock (fun () ->
      trywithrelease (fun () ->
        let saved = Console.Out
        let here = Directory.GetCurrentDirectory()
        let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
        let unique = Path.Combine(where, Guid.NewGuid().ToString())
        let save = Instance.I.trace
        Instance.I.trace <- Adapter.MakeNullTrace null

        Instance.supervision <- true
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
          Adapter.DoUnload().Invoke(null, null)
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
          Instance.I.trace <- save
          Instance.supervision <- false
          AltCoverCoreTests.maybeDeleteFile Instance.ReportFile
          Adapter.VisitsClear()
          Console.SetOut saved
          Directory.SetCurrentDirectory(here)
          AltCoverCoreTests.maybeIOException
            (fun () -> Directory.Delete(unique))
      ))
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
      AltCoverCoreTests.maybeDeleteFile reportFile
      Console.SetOut saved
      Directory.SetCurrentDirectory(here)
      AltCoverCoreTests.maybeIOException
            (fun () -> Directory.Delete(unique))

  [<Test>]
  let FlushLeavesExpectedTracesWhenBroken() =
    let saved = Console.Out
    let here = Directory.GetCurrentDirectory()
    let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
    let unique = Path.Combine(where, Guid.NewGuid().ToString())
    let reportFile = Path.Combine(unique, "FlushLeavesExpectedTraces.xml")
    let outputFile = Path.Combine(unique, "FlushLeavesExpectedTracesWhenBroken.xml")
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
         worker.Write(buffer, 0, 0)
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
        (after.OuterXml,
         Is.EqualTo "<null />")
    finally
      AltCoverCoreTests.maybeDeleteFile reportFile
      Console.SetOut saved
      Directory.SetCurrentDirectory(here)
      AltCoverCoreTests.maybeIOException
            (fun () -> Directory.Delete(unique))

#if !NET2
  [<Test>]
  let ZipFlushLeavesExpectedTracesWhenDiverted() =
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
      do use archive = ZipFile.Open(reportFile + ".zip", ZipArchiveMode.Create)
         let entry = reportFile
                     |> Path.GetFileName
                     |> archive.CreateEntry
         use sink = entry.Open()
         sink.Write(buffer, 0, size)
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
      AltCoverCoreTests.maybeDeleteFile reportFile
      Console.SetOut saved
      Directory.SetCurrentDirectory(here)
      AltCoverCoreTests.maybeIOException
            (fun () -> Directory.Delete(unique))

  [<Test>]
  let ZipFlushLeavesExpectedTracesWhenBroken() =
    let saved = Console.Out
    let here = Directory.GetCurrentDirectory()
    let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
    let unique = Path.Combine(where, Guid.NewGuid().ToString())
    let reportFile = Path.Combine(unique, "FlushLeavesExpectedTraces.xml")
    let outputFile = Path.Combine(unique, "FlushLeavesExpectedTracesWhenBroken.xml")
    let zipFile = reportFile + ".zip"
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
      do use worker = new FileStream(zipFile, FileMode.CreateNew)
         worker.Write(buffer, 0, 0)
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
        (after.OuterXml,
         Is.EqualTo "<null />")
    finally
      AltCoverCoreTests.maybeDeleteFile reportFile
      AltCoverCoreTests.maybeDeleteFile outputFile
      AltCoverCoreTests.maybeDeleteFile zipFile
      Console.SetOut saved
      Directory.SetCurrentDirectory(here)
      AltCoverCoreTests.maybeIOException
            (fun () -> Directory.Delete(unique))

  [<Test>]
  let ZipFlushLeavesExpectedTracesWhenBrokenInPlace() =
    let saved = Console.Out
    let here = Directory.GetCurrentDirectory()
    let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
    let unique = Path.Combine(where, Guid.NewGuid().ToString())
    let reportFile = Path.Combine(unique, "FlushLeavesExpectedTraces.xml")
    let zipFile = reportFile + ".zip"
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
      do use worker = new FileStream(zipFile, FileMode.CreateNew)
         worker.Write(buffer, 0, 0)
         ()
      let payload = Dictionary<int, PointVisit>()
      [ 0 .. 9 ] |> Seq.iter (fun i -> payload.[i] <- PointVisitInit (int64 (i + 1)) [])
      visits.["f6e3edb3-fb20-44b3-817d-f69d1a22fc2f"] <- payload
      Adapter.DoFlush
        (visits, AltCover.Recorder.ReportFormat.NCover, reportFile, null) |> ignore

      Assert.That(reportFile |> File.Exists |> not)
      let zipInfo = FileInfo(zipFile)
      Assert.That(zipInfo.Length, Is.EqualTo 0)
    finally
      AltCoverCoreTests.maybeDeleteFile zipFile
      Console.SetOut saved
      Directory.SetCurrentDirectory(here)
      AltCoverCoreTests.maybeIOException
            (fun () -> Directory.Delete(unique))

  let ZipFlushLeavesExpectedTraces() =
    GetMyMethodName "=>"
    lock Adapter.Lock (fun () ->
      Instance.I.isRunner <- false
      trywithrelease (fun () ->
        let saved = Console.Out
        let here = Directory.GetCurrentDirectory()
        let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
        let unique = Path.Combine(where, Guid.NewGuid().ToString())
        let save = Instance.I.trace
        Instance.I.trace <- Adapter.MakeNullTrace null
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
          do use archive = ZipFile.Open(Instance.ReportFile + ".zip", ZipArchiveMode.Create)
             let entry = Instance.ReportFile
                         |> Path.GetFileName
                         |> archive.CreateEntry
             use sink = entry.Open()
             sink.Write(buffer, 0, size)
             ()
          [ 0 .. 9 ]
          |> Seq.iter (fun i ->
               Adapter.VisitsAdd
                 ("f6e3edb3-fb20-44b3-817d-f69d1a22fc2f", i, (int64 (i + 1))))
          Adapter.DoExit().Invoke(null, null)
          let head = "Coverage statistics flushing took "
          let tail = " seconds\n"
          let recorded = stdout.ToString().Replace("\r\n", "\n")
          let index1 = recorded.IndexOf(head, StringComparison.Ordinal)
          let index2 = recorded.IndexOf(tail, StringComparison.Ordinal)
          Assert.That(index1, Is.GreaterThanOrEqualTo 0, recorded)
          Assert.That(index2, Is.GreaterThan index1, recorded)

          use zip = ZipFile.Open(Instance.ReportFile + ".zip", ZipArchiveMode.Update)
          let entry = Instance.ReportFile
                      |> Path.GetFileName
                      |> zip.GetEntry
          use worker' = entry.Open()
          let after = XmlDocument()
          after.Load worker'
          Assert.That
            (after.SelectNodes("//seqpnt")
             |> Seq.cast<XmlElement>
             |> Seq.map (fun x -> x.GetAttribute("visitcount")),
             Is.EquivalentTo [ "11"; "10"; "9"; "8"; "7"; "6"; "4"; "3"; "2"; "1" ])
        finally
          Instance.I.trace <- save
          AltCoverCoreTests.maybeDeleteFile Instance.ReportFile
          Adapter.VisitsClear()
          Console.SetOut saved
          Directory.SetCurrentDirectory(here)
          AltCoverCoreTests.maybeIOException
            (fun () -> Directory.Delete(unique))
      ))
    GetMyMethodName "<="
#endif

  // Dead simple sequential operation
  // run only once in Framework mode to avoid contention
  [<Test>]
  let MailboxFunctionsAsExpected() =
    let dummy = AltCover.Recorder.ExcludeFromCodeCoverageAttribute()
    Assert.That(dummy, Is.Not.Null)
    RealIdShouldIncrementCount()
    PauseLeavesExpectedTraces()
    ResumeLeavesExpectedTraces()
#if !NET2
    ZipFlushLeavesExpectedTraces()
#endif
    FlushLeavesExpectedTraces()