#if !NET472
#if NET20
namespace Tests.Recorder.Clr2
#else
namespace Tests.Recorder.Core
#endif
#else
namespace Tests.Recorder.Clr4
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
  let private getMyMethodName tag = ignore tag
  //    let st = StackTrace(StackFrame(1))
  //    st.GetFrame(0).GetMethod().Name |>
  //#if NET20
  //    printfn "%s %s 2" tag
  //#else
  //    printfn "%s %s" tag
  //#endif

  let resource =
    Assembly
      .GetExecutingAssembly()
      .GetManifestResourceNames()
    |> Seq.find (fun n -> n.EndsWith("SimpleCoverage.xml", StringComparison.Ordinal))

  let private updateReport a b =
    Adapter.updateReport(a, ReportFormat.NCover, b, b)
    |> ignore

  let private pointVisitInit a b = Adapter.init(a, b)

  let resource2 =
    Assembly
      .GetExecutingAssembly()
      .GetManifestResourceNames()
    |> Seq.find
         (fun n ->
           n.EndsWith("Sample1WithModifiedOpenCover.xml", StringComparison.Ordinal))

  [<Test>]
  let ShouldFailXmlDataForNativeJson () =
    Assert.Throws<NotSupportedException>
      (fun () ->
        ReportFormat.NativeJson
        |> Counter.I.xmlByFormat
        |> ignore)
    |> ignore

  [<Test>]
  let ShouldBeAbleToGetTheDefaultReportFileName () =
    Assert.True(Instance.ReportFile = "Coverage.Default.xml")

  [<Test>]
  let DefaultAccessorsBehaveAsExpected () =
    let v1 = DateTime.UtcNow.Ticks
    let probe = Instance.I.clock ()
    let v2 = DateTime.UtcNow.Ticks
    Assert.True(Instance.I.granularity () = 0L)
    Assert.True(probe >= v1)
    Assert.True(probe <= v2)

  [<Test>]
  let ShouldBeLinkingTheCorrectCopyOfThisCode () =
    getMyMethodName "=>"
    let tracer = Adapter.makeNullTrace String.Empty

    Assert.True(tracer.GetType().Assembly.GetName().Name = "AltCover.Recorder")
    getMyMethodName "<="

  [<Test>]
  let OnlyNewIdPairShouldBeSampled () =
    getMyMethodName "=>"

    lock
      Adapter.Lock
      (fun () ->
        try
          Adapter.SamplesClear()
          Assert.True(Adapter.addSample("module", 23, Null))
          Assert.True(Adapter.addSample("module", 24, Null))
          Assert.True(Adapter.addSample("newmodule", 23, Null))
          Assert.True(Adapter.addSample("module", 23, Null) |> not)
          Assert.True(Adapter.addSampleUnconditional("module", 23, Null))
          Assert.True(Adapter.addSample("module", 23, Call 1))
          Assert.True(Adapter.addSample("module", 23, Time 0L))
          Assert.True(Adapter.addSample("module", 24, Both { Call = 1; Time = 0L }))
          Assert.True(Adapter.addSample("module", 25, Both { Call = 1; Time = 0L }))
          Assert.True(Adapter.addSample("module", 25, Call 1) |> not)
          Assert.True(Adapter.addSample("module", 25, Call 1) |> not)
          Assert.True(Adapter.addSample("module", 25, Null) |> not)

          Assert.Throws<InvalidDataException>
            (fun () ->
              Adapter.addSample("module", 23, Table null)
              |> ignore)
          |> ignore
        finally
          Adapter.SamplesClear())

    getMyMethodName "<="

  let RealIdShouldIncrementCount () =
    getMyMethodName "=>"

    lock
      Adapter.Lock
      (fun () ->
        let save = Instance.I.trace

        try
          Adapter.VisitsClear()
          Instance.I.trace <- Adapter.makeNullTrace null

          let key = " "
          Instance.I.recording <- false
          Instance.Visit "key" 17
          Instance.I.recording <- true
          Instance.CoverageFormat <- ReportFormat.NCover
          Instance.Visit key -23
          Instance.CoverageFormat <- ReportFormat.OpenCoverWithTracking
          Instance.Visit key -23
          Assert.True(Adapter.VisitsSeq() |> Seq.length = 1)
          Assert.True(Adapter.VisitsEntrySeq key |> Seq.length = 1)
          Assert.True(Adapter.VisitCount(key, -23) = 2L)
          Assert.That(Counter.totalVisits, Is.EqualTo 1L)
          Assert.That(Counter.branchVisits, Is.EqualTo 1L)
        finally
          Instance.CoverageFormat <- ReportFormat.NCover
          Instance.I.recording <- true
          Adapter.VisitsClear()
          Instance.I.trace <- save)

    getMyMethodName "<="

  [<Test>]
  let JunkUspidGivesNegativeIndex () =
    let key = " "
    let index = Counter.I.findIndexFromUspid 0 key
    Assert.True(index < 0)

  [<Test>]
  let PayloadGeneratedIsAsExpected () =
    try
      Instance.I.isRunner <- false
      Instance.CoverageFormat <- ReportFormat.OpenCoverWithTracking
      Assert.True(Instance.I.callerId () |> Option.isNone)
      Assert.True(Adapter.payloadSelector false = Adapter.asNull())
      Assert.True(Adapter.payloadSelector true = Adapter.asNull())
      Instance.Push 4321
      Assert.True(Adapter.payloadSelector false = Adapter.asNull())
      Assert.True(Adapter.payloadSelector true = (Adapter.asCall 4321))

      try
        Instance.Push 6789
        // 0x1234123412341234 == 1311693406324658740
        let result =
          Adapter.payloadSelection(1311693406324658740L, 1000L, true)

        let expected =
          Adapter.newBoth(1311693406324658000L, 6789)

        Assert.True((result = expected))
      finally
        Instance.Pop()

      Assert.True((Adapter.payloadSelector true = (Adapter.asCall 4321)))
    finally
      Instance.Pop()
      Instance.CoverageFormat <- ReportFormat.NCover

    let result2 =
      Adapter.payloadSelection(1311693406324658740L, 1000L, true)

    let expected2 = Adapter.time 1311693406324658000L
    Assert.True((result2 = expected2))
    let v1 = DateTime.UtcNow.Ticks
    let probed = Adapter.payloadControl(1000L, true)
    let v2 = DateTime.UtcNow.Ticks
    Assert.True(Adapter.asNull() |> Adapter.untime |> Seq.isEmpty)
    let [ probe ] = Adapter.untime probed |> Seq.toList
    Assert.True(probe % 1000L = 0L)
    Assert.True(probe <= v2)
    Assert.True(probe >= (1000L * (v1 / 1000L)))
    Assert.True(Instance.I.callerId () |> Option.isNone)
    Instance.Pop()
    Assert.True(Instance.I.callerId () |> Option.isNone)

  [<Test>]
  let PayloadWithEntryExitGeneratedIsAsExpected () =
    try
      Instance.I.isRunner <- true
      Instance.CoverageFormat <- ReportFormat.OpenCoverWithTracking
      Adapter.VisitsClear()

      Assert.True(Instance.I.callerId () |> Option.isNone)
      Assert.True(Adapter.payloadSelector false = Adapter.asNull())
      Assert.True(Adapter.payloadSelector true = Adapter.asNull())
      Instance.Push 4321
      Assert.True(Adapter.payloadSelector false = Adapter.asNull())
      Assert.True(Adapter.payloadSelector true = (Adapter.asCall 4321))

      try
        Instance.Push 6789
        // 0x1234123412341234 == 1311693406324658740
        let result =
          Adapter.payloadSelection(1311693406324658740L, 1000L, true)

        let expected =
          Adapter.newBoth(1311693406324658000L, 6789)

        Assert.True((result = expected))
      finally
        Instance.Pop()

      Assert.True((Adapter.payloadSelector true = (Adapter.asCall 4321)))
    finally
      Instance.Pop()
      Instance.I.isRunner <- false
      Instance.CoverageFormat <- ReportFormat.NCover

    let result2 =
      Adapter.payloadSelection(1311693406324658740L, 1000L, true)

    let expected2 = Adapter.time 1311693406324658000L
    Assert.True((result2 = expected2))
    let v1 = DateTime.UtcNow.Ticks
    let probed = Adapter.payloadControl(1000L, true)
    let v2 = DateTime.UtcNow.Ticks
    Assert.True(Adapter.asNull() |> Adapter.untime |> Seq.isEmpty)
    let [ probe ] = Adapter.untime probed |> Seq.toList
    Assert.True(probe % 1000L = 0L)
    Assert.True(probe <= v2)
    Assert.True(probe >= (1000L * (v1 / 1000L)))
    Assert.True(Instance.I.callerId () |> Option.isNone)
    Instance.Pop()
    Assert.True(Instance.I.callerId () |> Option.isNone)

    Assert.That(
      Instance.I.visits.Keys,
      Is.EquivalentTo [ Track.Entry
                        Track.Exit ]
    )

    Assert.That(Instance.I.visits.[Track.Entry].Keys, Is.EquivalentTo [ 4321; 6789 ])
    Assert.That(Instance.I.visits.[Track.Exit].Keys, Is.EquivalentTo [ 4321; 6789 ])
    let a = Instance.I.visits.[Track.Entry].[4321]
    Assert.That(a.Count, Is.EqualTo 0L)
    Assert.That(a.Tracks |> Seq.length, Is.EqualTo 1)
    let b = Instance.I.visits.[Track.Entry].[6789]
    Assert.That(b.Count, Is.EqualTo 0L)
    Assert.That(b.Tracks |> Seq.length, Is.EqualTo 1)
    let c = Instance.I.visits.[Track.Exit].[6789]
    Assert.That(c.Count, Is.EqualTo 0L)
    Assert.That(c.Tracks |> Seq.length, Is.EqualTo 1)
    let d = Instance.I.visits.[Track.Exit].[4321]
    Assert.That(d.Count, Is.EqualTo 0L)
    Assert.That(d.Tracks |> Seq.length, Is.EqualTo 1)

    let a2 =
      a.Tracks |> Seq.head |> Adapter.untime |> Seq.head

    let b2 =
      b.Tracks |> Seq.head |> Adapter.untime |> Seq.head

    Assert.That(b2 >= a2)

    let c2 =
      c.Tracks |> Seq.head |> Adapter.untime |> Seq.head

    Assert.That(c2 >= b2)

    let d2 =
      d.Tracks |> Seq.head |> Adapter.untime |> Seq.head

    Assert.That(d2 >= c2, sprintf "%A >= %A" d2 c2)

    Adapter.VisitsClear()

  [<Test>]
  let RealIdShouldIncrementCountSynchronously () =
    getMyMethodName "=>"

    lock
      Instance.I.visits
      (fun () ->
        let save = Instance.I.trace
        Adapter.Reset()

        try
          Instance.I.visits.Clear()
          Instance.I.trace <- Adapter.makeNullTrace null

          let key = " "
          Instance.I.visitSelection (Adapter.asNull()) key 23

          Assert.That(
            Instance.I.visits.Count,
            Is.EqualTo 1,
            "A visit that should have happened, didn't"
          )

          Assert.That(
            Instance.I.visits.[key].Count,
            Is.EqualTo 1,
            "keys = "
            + String.Join("; ", Instance.I.visits.Keys |> Seq.toArray)
          )

          Assert.That(Instance.I.visits.[key].[23].Count, Is.EqualTo 1)
          Assert.That(Instance.I.visits.[key].[23].Tracks, Is.Empty)
        finally
          Instance.I.visits.Clear()
          Instance.I.trace <- save)

    getMyMethodName "<="

  let strip before (all: HashSet<String>) =
    before
    |> Seq.iter
         (fun x ->
           Assert.That(all.Contains x)
           all.Remove x |> ignore)

  [<Test>]
  let StripWorks () =
    let b1 = [ "1" ]
    let a1 = [ "1"; "2" ]
    let a1' = HashSet<String>(a1)
    strip b1 a1'
    let stripped1 = a1' |> Seq.toList
    Assert.That(stripped1, Is.EquivalentTo [ "2" ])

    Assert.Throws<AssertionException>(fun () -> strip b1 a1')
    |> ignore

  [<Test>]
  let ExceptionLoggedToFile () =
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
    |> Seq.zip [ "ModuleId = \"a\""
                 "hitPointId = \"b\""
                 "context = \"c\""
                 "exception = ex" ]
    |> Seq.iter (fun (a, b) -> Assert.That(a, Is.EqualTo b))

    let third = Directory.GetFiles(where, "*.exn")
    Assert.That(third.Length, Is.EqualTo before.Length)

  [<Test>]
  let Issue71WrapperHandlesKeyNotFoundException () =
    let pair = [| false; false |]
    let unique = System.Guid.NewGuid().ToString()
    Adapter.invokeIssue71Wrapper<KeyNotFoundException>(unique, pair)
    Assert.That(pair |> Seq.head, Is.True)
    Assert.That(pair |> Seq.last, Is.True)

  [<Test>]
  let Issue71WrapperHandlesNullReferenceException () =
    let pair = [| false; false |]
    let unique = System.Guid.NewGuid().ToString()
    Adapter.invokeIssue71Wrapper<NullReferenceException>(unique, pair)
    Assert.That(pair |> Seq.head, Is.True)
    Assert.That(pair |> Seq.last, Is.True)

  [<Test>]
  let Issue71WrapperHandlesArgumentNullException () =
    let pair = [| false; false |]
    let unique = System.Guid.NewGuid().ToString()
    Adapter.invokeIssue71Wrapper<ArgumentNullException>(unique, pair)
    Assert.That(pair |> Seq.head, Is.True)
    Assert.That(pair |> Seq.last, Is.True)

  [<Test>]
  let Issue71WrapperDoesntHandleOtherException () =
    let pair = [| false; false |]
    let unique = System.Guid.NewGuid().ToString()

    let exn =
      Assert.Throws<InvalidOperationException>
        (fun () -> Adapter.invokeIssue71Wrapper<InvalidOperationException>(unique, pair))

    Assert.That(pair |> Seq.head, Is.False)
    Assert.That(pair |> Seq.last, Is.False)
    Assert.That(exn.Message, Is.EqualTo unique)

#if NET5_0
  [<Test>]
  let NullRefShouldBeHandled () =
    getMyMethodName "=>"
    let handle = Instance.I.visits

    lock
      handle
      (fun () ->
        try
          Instance.I.visits <- null
          let key = " "
          let path = Instance.ReportFile |> Path.GetFullPath
          let where = path |> Path.GetDirectoryName
          let before = Directory.GetFiles(where, "*.exn")
          Instance.I.visitImpl key 23 (Adapter.asNull())
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
          |> Seq.zip [ "ModuleId = \" \""
                       "hitPointId = 23"
                       "context = Null"
                       "exception = System.NullReferenceException: Object reference not set to an instance of an object." ]
          |> Seq.iter (fun (a, b) -> Assert.True((a = b)))

          let third = Directory.GetFiles(where, "*.exn")
          Assert.That(third.Length, Is.EqualTo before.Length)
        finally
          Instance.I.visits <- handle
          Instance.I.visits.Clear())

    getMyMethodName "<="
#endif

  [<Test>]
  let DistinctIdShouldBeDistinct () =
    getMyMethodName "=>"

    lock
      Instance.I.visits
      (fun () ->
        try
          Instance.I.visits.Clear()
          let key = " "
          Instance.I.visitImpl key 23 (Adapter.asNull())
          Instance.I.visitImpl "key" 42 (Adapter.asNull())
          Assert.That(Instance.I.visits.Count, Is.EqualTo 2)
        finally
          Instance.I.visits.Clear())

    getMyMethodName "<="

  [<Test>]
  let DistinctLineShouldBeDistinct () =
    getMyMethodName "=>"

    lock
      Instance.I.visits
      (fun () ->
        try
          Instance.I.visits.Clear()
          let key = " "
          Instance.I.visitImpl key 23 (Adapter.asNull())
          Instance.I.visitImpl key 42 (Adapter.asNull())
          Assert.That(Instance.I.visits.Count, Is.EqualTo 1)
          Assert.That(Instance.I.visits.[key].Count, Is.EqualTo 2)
        finally
          Adapter.VisitsClear())

    getMyMethodName "<="

  [<Test>]
  let RepeatVisitsShouldIncrementCount () =
    getMyMethodName "=>"

    lock
      Instance.I.visits
      (fun () ->
        Adapter.Reset()

        try
          Instance.I.visits.Clear()
          let key = " "
          Instance.I.visitImpl key 23 (Adapter.asNull())
          Instance.I.visitImpl key 23 (Adapter.asNull())
          Assert.That(Instance.I.visits.[key].[23].Count, Is.EqualTo 2)
          Assert.That(Instance.I.visits.[key].[23].Tracks, Is.Empty)
        finally
          Instance.I.visits.Clear())

    getMyMethodName "<="

  [<Test>]
  let RepeatVisitsShouldIncrementTotal () =
    getMyMethodName "=>"

    lock
      Instance.I.visits
      (fun () ->
        Adapter.Reset()

        try
          Instance.I.visits.Clear()
          let key = " "
          let payload = Adapter.time DateTime.UtcNow.Ticks
          Instance.I.visitImpl key 23 (Adapter.asNull())
          Instance.I.visitImpl key 23 payload
          Assert.That(Instance.I.visits.[key].[23].Count, Is.EqualTo 1)
          Assert.That(Instance.I.visits.[key].[23].Tracks, Is.EquivalentTo [ payload ])
        finally
          Instance.I.visits.Clear())

    getMyMethodName "<="

  //[<Test>]
  //let TabledVisitsShouldIncrementCount() =
  //  getMyMethodName "=>"
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

  //      let pv = pointVisitInit 42L payloads
  //      table.[key].Add(23, pv)
  //      let n = Counter.AddTable(Instance.I.visits, table)
  //      Assert.That(n, Is.EqualTo 45)
  //      Assert.That(Instance.I.visits.[key].[23].Count, Is.EqualTo 43)
  //      Assert.That(Instance.I.visits.[key].[23].Tracks, Is.EquivalentTo payloads)
  //    finally
  //      Instance.I.visits.Clear())
  //  getMyMethodName "<="

  [<Test>]
  let OldDocumentStartIsNotUpdated () =
    getMyMethodName "=>"

    lock
      Instance.I.visits
      (fun () ->
        let epoch = DateTime.UtcNow
        Counter.startTime <- epoch

        use stream =
          Assembly
            .GetExecutingAssembly()
            .GetManifestResourceStream(resource)

        let size = int stream.Length
        let buffer = Array.create size 0uy
        Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)
        use worker = new MemoryStream()
        worker.Write(buffer, 0, size)
        worker.Position <- 0L
        let before = XmlDocument()

        before.Load(
          Assembly
            .GetExecutingAssembly()
            .GetManifestResourceStream(resource)
        )

        updateReport(Dictionary<string, Dictionary<int, PointVisit>>()) worker
        worker.Position <- 0L
        let after = XmlDocument()
        after.Load worker

        let startTimeAttr =
          after.DocumentElement.GetAttribute("startTime")

        let startTime =
          DateTime.ParseExact(startTimeAttr, "o", null)

        Assert.That(startTime.ToUniversalTime(), Is.LessThan epoch)

        Assert.That(
          startTime.ToUniversalTime(),
          Is.EqualTo(Counter.startTime.ToUniversalTime())
        ))

    getMyMethodName "<="

  [<Test>]
  let NewDocumentStartIsMadeEarlier () =
    getMyMethodName "=>"

    lock
      Instance.I.visits
      (fun () ->
        let epoch =
          DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)

        Counter.startTime <- epoch

        use stream =
          Assembly
            .GetExecutingAssembly()
            .GetManifestResourceStream(resource)

        let size = int stream.Length
        let buffer = Array.create size 0uy
        Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)
        use worker = new MemoryStream()
        worker.Write(buffer, 0, size)
        worker.Position <- 0L
        let before = XmlDocument()

        before.Load(
          Assembly
            .GetExecutingAssembly()
            .GetManifestResourceStream(resource)
        )

        updateReport(Dictionary<string, Dictionary<int, PointVisit>>()) worker
        worker.Position <- 0L
        let after = XmlDocument()
        after.Load worker

        let startTimeAttr =
          after.DocumentElement.GetAttribute("startTime")

        let startTime =
          DateTime.ParseExact(startTimeAttr, "o", null)

        Assert.That(startTime.ToUniversalTime(), Is.EqualTo epoch)

        Assert.That(
          startTime.ToUniversalTime(),
          Is.EqualTo(Counter.startTime.ToUniversalTime())
        ))

    getMyMethodName "<="

  [<Test>]
  let NewDocumentMeasureIsNotMadeEarlier () =
    getMyMethodName "=>"

    lock
      Instance.I.visits
      (fun () ->
        let epoch =
          DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)

        Counter.measureTime <- epoch

        use stream =
          Assembly
            .GetExecutingAssembly()
            .GetManifestResourceStream(resource)

        let size = int stream.Length
        let buffer = Array.create size 0uy
        Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)
        use worker = new MemoryStream()
        worker.Write(buffer, 0, size)
        worker.Position <- 0L
        let before = XmlDocument()

        before.Load(
          Assembly
            .GetExecutingAssembly()
            .GetManifestResourceStream(resource)
        )

        updateReport(Dictionary<string, Dictionary<int, PointVisit>>()) worker
        worker.Position <- 0L
        let after = XmlDocument()
        after.Load worker

        let startTimeAttr =
          after.DocumentElement.GetAttribute("measureTime")

        let startTime =
          DateTime.ParseExact(startTimeAttr, "o", null)

        Assert.That(startTime.ToUniversalTime(), Is.GreaterThan epoch)

        Assert.That(
          startTime.ToUniversalTime(),
          Is.EqualTo(Counter.measureTime.ToUniversalTime())
        ))

    getMyMethodName "<="

  [<Test>]
  let OldDocumentMeasureIsUpdated () =
    getMyMethodName "=>"

    lock
      Instance.I.visits
      (fun () ->
        let epoch = DateTime.UtcNow
        Counter.measureTime <- epoch

        use stream =
          Assembly
            .GetExecutingAssembly()
            .GetManifestResourceStream(resource)

        let size = int stream.Length
        let buffer = Array.create size 0uy
        Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)
        use worker = new MemoryStream()
        worker.Write(buffer, 0, size)
        worker.Position <- 0L
        let before = XmlDocument()

        before.Load(
          Assembly
            .GetExecutingAssembly()
            .GetManifestResourceStream(resource)
        )

        updateReport(Dictionary<string, Dictionary<int, PointVisit>>()) worker
        worker.Position <- 0L
        let after = XmlDocument()
        after.Load worker

        let startTimeAttr =
          after.DocumentElement.GetAttribute("measureTime")

        let startTime =
          DateTime.ParseExact(startTimeAttr, "o", null)

        Assert.That(startTime.ToUniversalTime(), Is.EqualTo epoch)

        Assert.That(
          startTime.ToUniversalTime(),
          Is.EqualTo(Counter.measureTime.ToUniversalTime())
        ))

    getMyMethodName "<="

  [<Test>]
  let UnknownModuleMakesNoChange () =
    getMyMethodName "=>"

    lock
      Instance.I.visits
      (fun () ->
        Counter.measureTime <-
          DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)

        use stream =
          Assembly
            .GetExecutingAssembly()
            .GetManifestResourceStream(resource)

        let size = int stream.Length
        let buffer = Array.create size 0uy
        Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)
        use worker = new MemoryStream()
        worker.Write(buffer, 0, size)
        worker.Position <- 0L

        use before =
          new StreamReader(
            Assembly
              .GetExecutingAssembly()
              .GetManifestResourceStream(resource)
          )

        let item =
          Dictionary<string, Dictionary<int, PointVisit>>()

        item.Add("not a guid", null)
        updateReport item worker
        worker.Position <- 0L
        let after = new StreamReader(worker)

        let result =
          after
            .ReadToEnd()
            .Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
          |> Seq.skip 3

        let expected =
          before
            .ReadToEnd()
            .Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
          |> Seq.skip 3

        Assert.That(result, Is.EquivalentTo expected))

    getMyMethodName "<="

  [<Test>]
  let KnownModuleWithNothingMakesNoChange () =
    getMyMethodName "=>"

    lock
      Instance.I.visits
      (fun () ->
        Counter.measureTime <-
          DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)

        use stream =
          Assembly
            .GetExecutingAssembly()
            .GetManifestResourceStream(resource)

        let size = int stream.Length
        let buffer = Array.create size 0uy
        Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)
        use worker = new MemoryStream()
        worker.Write(buffer, 0, size)
        worker.Position <- 0L

        use before =
          new StreamReader(
            Assembly
              .GetExecutingAssembly()
              .GetManifestResourceStream(resource)
          )

        let item =
          Dictionary<string, Dictionary<int, PointVisit>>()

        item.Add("f6e3edb3-fb20-44b3-817d-f69d1a22fc2f", Dictionary<int, PointVisit>())
        updateReport item worker
        worker.Position <- 0L
        let after = new StreamReader(worker)

        let result =
          after
            .ReadToEnd()
            .Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
          |> Seq.skip 3

        let expected =
          before
            .ReadToEnd()
            .Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
          |> Seq.skip 3

        Assert.That(result, Is.EquivalentTo expected))

    getMyMethodName "<="

  [<Test>]
  let KnownModuleWithNothingInRangeMakesNoChange () =
    getMyMethodName "=>"

    lock
      Instance.I.visits
      (fun () ->
        Counter.measureTime <-
          DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)

        use stream =
          Assembly
            .GetExecutingAssembly()
            .GetManifestResourceStream(resource)

        let size = int stream.Length
        let buffer = Array.create size 0uy
        Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)
        use worker = new MemoryStream()
        worker.Write(buffer, 0, size)
        worker.Position <- 0L

        use before =
          new StreamReader(
            Assembly
              .GetExecutingAssembly()
              .GetManifestResourceStream(resource)
          )

        let payload = Dictionary<int, PointVisit>()
        payload.[-1] <- pointVisitInit 10L []
        payload.[100] <- pointVisitInit 10L []

        let item =
          Dictionary<string, Dictionary<int, PointVisit>>()

        item.Add("f6e3edb3-fb20-44b3-817d-f69d1a22fc2f", payload)
        updateReport item worker
        worker.Position <- 0L
        let after = new StreamReader(worker)

        let result =
          after
            .ReadToEnd()
            .Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
          |> Seq.skip 3

        let expected =
          before
            .ReadToEnd()
            .Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
          |> Seq.skip 3

        Assert.That(result, Is.EquivalentTo expected))

    getMyMethodName "<="

  [<Test>]
  let KnownModuleWithPayloadMakesExpectedChange () =
    getMyMethodName "=>"

    lock
      Instance.I.visits
      (fun () ->
        Counter.measureTime <-
          DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)

        use stream =
          Assembly
            .GetExecutingAssembly()
            .GetManifestResourceStream(resource)

        let size = int stream.Length
        let buffer = Array.create size 0uy
        Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)
        use worker = new MemoryStream()
        worker.Write(buffer, 0, size)
        worker.Position <- 0L
        let payload = Dictionary<int, PointVisit>()

        [ 0 .. 9 ]
        |> Seq.iter (fun i -> payload.[i] <- pointVisitInit(int64 (i + 1)) [])

        let item =
          Dictionary<string, Dictionary<int, PointVisit>>()

        item.Add("f6e3edb3-fb20-44b3-817d-f69d1a22fc2f", payload)
        updateReport item worker
        worker.Position <- 0L
        let after = XmlDocument()
        after.Load worker

        Assert.That(
          after.SelectNodes("//seqpnt")
          |> Seq.cast<XmlElement>
          |> Seq.map (fun x -> x.GetAttribute("visitcount")),
          Is.EquivalentTo [ "11"
                            "10"
                            "9"
                            "8"
                            "7"
                            "6"
                            "4"
                            "3"
                            "2"
                            "1" ]
        ))

    getMyMethodName "<="

  [<Test>]
  let KnownModuleWithPayloadMakesExpectedChangeInOpenCover () =
    getMyMethodName "=>"

    lock
      Instance.I.visits
      (fun () ->
        Counter.measureTime <-
          DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)

        use stream =
          Assembly
            .GetExecutingAssembly()
            .GetManifestResourceStream(resource2)

        let size = int stream.Length
        let buffer = Array.create size 0uy
        Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)
        use worker = new MemoryStream()
        worker.Write(buffer, 0, size)
        worker.Position <- 0L
        let payload = Dictionary<int, PointVisit>()

        [ 0 .. 9 ]
        |> Seq.iter (fun i -> payload.[10 - i] <- pointVisitInit(int64 (i + 1)) [])

        [ 11 .. 12 ]
        |> Seq.iter
             (fun i ->
               payload.[i ||| Counter.branchFlag] <- pointVisitInit(int64 (i - 10)) [])

        let item =
          Dictionary<string, Dictionary<int, PointVisit>>()

        item.Add("7C-CD-66-29-A3-6C-6D-5F-A7-65-71-0E-22-7D-B2-61-B5-1F-65-9A", payload)

        Adapter.updateReport(item, ReportFormat.OpenCover, worker, worker)
        |> ignore

        worker.Position <- 0L
        let after = XmlDocument()
        after.Load worker

        Assert.That(
          after.SelectNodes("//SequencePoint")
          |> Seq.cast<XmlElement>
          |> Seq.map (fun x -> x.GetAttribute("vc")),
          Is.EquivalentTo [ "11"
                            "10"
                            "9"
                            "8"
                            "7"
                            "6"
                            "4"
                            "3"
                            "2"
                            "1" ]
        )

        Assert.That(
          after.SelectNodes("//BranchPoint")
          |> Seq.cast<XmlElement>
          |> Seq.map (fun x -> x.GetAttribute("vc")),
          Is.EquivalentTo [ "2"; "2" ]
        ))

    getMyMethodName "<="

  [<Test>]
  let EmptyFlushLeavesNoTrace () =
    getMyMethodName "=>"

    lock
      Adapter.Lock
      (fun () ->
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

    getMyMethodName "<="

  let trywith<'a when 'a :> exn> f g =
    try
      f ()
    with :? 'a -> g ()

  let trywithrelease<'a when 'a :> exn> f = trywith f Instance.I.mutex.ReleaseMutex

  [<Test>]
  let CanTryWith () =
    let mutable flag = false
    let setFlag = (fun () -> flag <- true)
    trywith<InvalidOperationException> (fun () -> ()) setFlag
    Assert.That(flag, Is.False)

    trywith<InvalidOperationException>
      (fun () -> InvalidOperationException() |> raise)
      setFlag

    Assert.That(flag, Is.True)

    Instance.I.mutex.WaitOne(1000) |> ignore

    trywithrelease<InvalidOperationException>
      (fun () -> InvalidOperationException() |> raise)

  let PauseLeavesExpectedTraces () =
    getMyMethodName "=>"

    lock
      Adapter.Lock
      (fun () ->
        trywithrelease
          (fun () ->
            let saved = Console.Out
            let here = Directory.GetCurrentDirectory()

            let where =
              Assembly.GetExecutingAssembly().Location
              |> Path.GetDirectoryName

            let unique =
              Path.Combine(where, Guid.NewGuid().ToString())

            let save = Instance.I.trace
            use s = new MemoryStream()

            let s1 =
              new Compression.DeflateStream(s, CompressionMode.Compress)

            Instance.I.trace <- Adapter.makeStreamTrace s1

            try
              Instance.I.isRunner <- true
              Adapter.VisitsClear()

              use stdout = new StringWriter()
              Console.SetOut stdout
              Directory.CreateDirectory(unique) |> ignore
              Directory.SetCurrentDirectory(unique)

              Counter.measureTime <-
                DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)

              use stream =
                Assembly
                  .GetExecutingAssembly()
                  .GetManifestResourceStream(resource)

              let size = int stream.Length
              let buffer = Array.create size 0uy
              Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)

              do
                use worker =
                  new FileStream(Instance.ReportFile, FileMode.CreateNew)

                worker.Write(buffer, 0, size)
                ()

              [ 0 .. 9 ]
              |> Seq.iter
                   (fun i ->
                     Adapter.VisitsAdd(
                       "f6e3edb3-fb20-44b3-817d-f69d1a22fc2f",
                       i,
                       (int64 (i + 1))
                     ))

              Adapter.DoPause().Invoke(null, null)
              Assert.That(Adapter.VisitsSeq(), Is.Empty)
              let recorded = stdout.ToString().Trim()
              Assert.That(recorded, Is.EqualTo "Pausing...")

              use worker' =
                new FileStream(Instance.ReportFile, FileMode.Open)

              let after = XmlDocument()
              after.Load worker'

              Assert.That(
                after.SelectNodes("//seqpnt")
                |> Seq.cast<XmlElement>
                |> Seq.map (fun x -> x.GetAttribute("visitcount")),
                Is.EquivalentTo [ "1"
                                  "1"
                                  "1"
                                  "1"
                                  "1"
                                  "1"
                                  "0"
                                  String.Empty
                                  "X"
                                  "-1" ]
              )
            finally
              Instance.I.trace <- save
              AltCoverCoreTests.maybeDeleteFile Instance.ReportFile
              Adapter.VisitsClear()
              Instance.I.isRunner <- false
              Console.SetOut saved
              Directory.SetCurrentDirectory(here)
              AltCoverCoreTests.maybeIOException (fun () -> Directory.Delete(unique))))

    getMyMethodName "<="

  let ResumeLeavesExpectedTraces () =
    getMyMethodName "=>"

    lock
      Adapter.Lock
      (fun () ->
        trywithrelease
          (fun () ->
            let saved = Console.Out
            let here = Directory.GetCurrentDirectory()

            let where =
              Assembly.GetExecutingAssembly().Location
              |> Path.GetDirectoryName

            let unique =
              Path.Combine(where, Guid.NewGuid().ToString())

            let save = Instance.I.trace
            let tag = unique + ".xml.acv"

            do
              use stream = File.Create tag
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

              use stream =
                Assembly
                  .GetExecutingAssembly()
                  .GetManifestResourceStream(resource)

              let size = int stream.Length
              let buffer = Array.create size 0uy
              Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)

              do
                use worker =
                  new FileStream(Instance.ReportFile, FileMode.CreateNew)

                worker.Write(buffer, 0, size)
                ()

              [ 0 .. 9 ]
              |> Seq.iter
                   (fun i ->
                     Adapter.VisitsAdd(
                       "f6e3edb3-fb20-44b3-817d-f69d1a22fc2f",
                       i,
                       (int64 (i + 1))
                     ))

              Adapter.DoResume().Invoke(null, null)
              Assert.That(Adapter.VisitsSeq(), Is.Empty, "Visits should be cleared")

              Assert.That(
                Object.ReferenceEquals(Instance.I.trace, save),
                Is.False,
                "trace should be replaced"
              )

              let recorded = stdout.ToString().Trim()
              Assert.That(recorded, Is.EqualTo "Resuming...", recorded)

              use worker' =
                new FileStream(Instance.ReportFile, FileMode.Open)

              let after = XmlDocument()
              after.Load worker'

              Assert.That(
                after.SelectNodes("//seqpnt")
                |> Seq.cast<XmlElement>
                |> Seq.map (fun x -> x.GetAttribute("visitcount")),
                Is.EquivalentTo [ "1"
                                  "1"
                                  "1"
                                  "1"
                                  "1"
                                  "1"
                                  "0"
                                  String.Empty
                                  "X"
                                  "-1" ]
              )
            finally
              Adapter.Reset()
              Instance.I.trace <- save
              AltCoverCoreTests.maybeDeleteFile Instance.ReportFile
              Adapter.VisitsClear()
              Console.SetOut saved
              Directory.SetCurrentDirectory(here)
              File.Delete tag
              AltCoverCoreTests.maybeIOException (fun () -> Directory.Delete(unique))))

    getMyMethodName "<="

  let FlushLeavesExpectedTraces () =
    getMyMethodName "=>"

    lock
      Adapter.Lock
      (fun () ->
        Instance.I.isRunner <- false

        trywithrelease
          (fun () ->
            let saved = Console.Out
            let here = Directory.GetCurrentDirectory()

            let where =
              Assembly.GetExecutingAssembly().Location
              |> Path.GetDirectoryName

            let unique =
              Path.Combine(where, Guid.NewGuid().ToString())

            let save = Instance.I.trace
            Instance.I.trace <- Adapter.makeNullTrace null

            try
              Adapter.VisitsClear()
              use stdout = new StringWriter()
              Console.SetOut stdout
              Directory.CreateDirectory(unique) |> ignore
              Directory.SetCurrentDirectory(unique)

              Counter.measureTime <-
                DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)

              use stream =
                Assembly
                  .GetExecutingAssembly()
                  .GetManifestResourceStream(resource)

              let size = int stream.Length
              let buffer = Array.create size 0uy
              Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)

              do
                use worker =
                  new FileStream(Instance.ReportFile, FileMode.CreateNew)

                worker.Write(buffer, 0, size)
                ()

              [ 0 .. 9 ]
              |> Seq.iter
                   (fun i ->
                     Adapter.VisitsAdd(
                       "f6e3edb3-fb20-44b3-817d-f69d1a22fc2f",
                       i,
                       (int64 (i + 1))
                     ))

              Adapter.DoExit().Invoke(null, null)
              let head = "Coverage statistics flushing took "
              let tail = " seconds\n"
              let recorded = stdout.ToString().Replace("\r\n", "\n")

              let index1 =
                recorded.IndexOf(head, StringComparison.Ordinal)

              let index2 =
                recorded.IndexOf(tail, StringComparison.Ordinal)

              Assert.That(index1, Is.GreaterThanOrEqualTo 0, recorded)
              Assert.That(index2, Is.GreaterThan index1, recorded)

              use worker' =
                new FileStream(Instance.ReportFile, FileMode.Open)

              let after = XmlDocument()
              after.Load worker'

              Assert.That(
                after.SelectNodes("//seqpnt")
                |> Seq.cast<XmlElement>
                |> Seq.map (fun x -> x.GetAttribute("visitcount")),
                Is.EquivalentTo [ "11"
                                  "10"
                                  "9"
                                  "8"
                                  "7"
                                  "6"
                                  "4"
                                  "3"
                                  "2"
                                  "1" ]
              )
            finally
              Instance.I.trace <- save
              AltCoverCoreTests.maybeDeleteFile Instance.ReportFile
              Adapter.VisitsClear()
              Console.SetOut saved
              Directory.SetCurrentDirectory(here)
              AltCoverCoreTests.maybeIOException (fun () -> Directory.Delete(unique))))

    getMyMethodName "<="

  [<Test>]
  let SupervisedFlushLeavesExpectedTraces () =
    getMyMethodName "=>"

    lock
      Adapter.Lock
      (fun () ->
        trywithrelease
          (fun () ->
            let saved = Console.Out
            let here = Directory.GetCurrentDirectory()

            let where =
              Assembly.GetExecutingAssembly().Location
              |> Path.GetDirectoryName

            let unique =
              Path.Combine(where, Guid.NewGuid().ToString())

            let save = Instance.I.trace
            Instance.I.trace <- Adapter.makeNullTrace null

            Instance.supervision <- true

            try
              Adapter.VisitsClear()
              use stdout = new StringWriter()
              Console.SetOut stdout
              Directory.CreateDirectory(unique) |> ignore
              Directory.SetCurrentDirectory(unique)

              Counter.measureTime <-
                DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)

              use stream =
                Assembly
                  .GetExecutingAssembly()
                  .GetManifestResourceStream(resource)

              let size = int stream.Length
              let buffer = Array.create size 0uy
              Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)

              do
                use worker =
                  new FileStream(Instance.ReportFile, FileMode.CreateNew)

                worker.Write(buffer, 0, size)
                ()

              [ 0 .. 9 ]
              |> Seq.iter
                   (fun i ->
                     Adapter.VisitsAdd(
                       "f6e3edb3-fb20-44b3-817d-f69d1a22fc2f",
                       i,
                       (int64 (i + 1))
                     ))

              Adapter.DoUnload().Invoke(null, null)
              let head = "Coverage statistics flushing took "
              let tail = " seconds\n"
              let recorded = stdout.ToString().Replace("\r\n", "\n")
              Assert.That(recorded, Is.Empty, recorded)

              use worker' =
                new FileStream(Instance.ReportFile, FileMode.Open)

              let after = XmlDocument()
              after.Load worker'

              Assert.That(
                after.SelectNodes("//seqpnt")
                |> Seq.cast<XmlElement>
                |> Seq.map (fun x -> x.GetAttribute("visitcount")),
                Is.EquivalentTo [ "1"
                                  "1"
                                  "1"
                                  "1"
                                  "1"
                                  "1"
                                  "0"
                                  String.Empty
                                  "X"
                                  "-1" ]
              )
            finally
              Instance.I.trace <- save
              Instance.supervision <- false
              AltCoverCoreTests.maybeDeleteFile Instance.ReportFile
              Adapter.VisitsClear()
              Console.SetOut saved
              Directory.SetCurrentDirectory(here)
              AltCoverCoreTests.maybeIOException (fun () -> Directory.Delete(unique))))

    getMyMethodName "<="

  [<Test>]
  let FlushLeavesExpectedTracesWhenDiverted () =
    let saved = Console.Out
    let here = Directory.GetCurrentDirectory()

    let where =
      Assembly.GetExecutingAssembly().Location
      |> Path.GetDirectoryName

    let unique =
      Path.Combine(where, Guid.NewGuid().ToString())

    let reportFile =
      Path.Combine(unique, "FlushLeavesExpectedTraces.xml")

    let outputFile =
      Path.Combine(unique, "FlushLeavesExpectedTracesWhenDiverted.xml")

    try
      let visits =
        new Dictionary<string, Dictionary<int, PointVisit>>()

      use stdout = new StringWriter()
      Console.SetOut stdout
      Directory.CreateDirectory(unique) |> ignore
      Directory.SetCurrentDirectory(unique)

      Counter.measureTime <-
        DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)

      use stream =
        Assembly
          .GetExecutingAssembly()
          .GetManifestResourceStream(resource)

      let size = int stream.Length
      let buffer = Array.create size 0uy
      Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)

      do
        use worker =
          new FileStream(reportFile, FileMode.CreateNew)

        worker.Write(buffer, 0, size)
        ()

      let payload = Dictionary<int, PointVisit>()

      [ 0 .. 9 ]
      |> Seq.iter (fun i -> payload.[i] <- pointVisitInit(int64 (i + 1)) [])

      visits.["f6e3edb3-fb20-44b3-817d-f69d1a22fc2f"] <- payload

      Adapter.doFlush(
        visits,
        AltCover.Recorder.ReportFormat.NCover,
        reportFile,
        outputFile
      )
      |> ignore

      use worker' =
        new FileStream(outputFile, FileMode.Open)

      let after = XmlDocument()
      after.Load worker'

      Assert.That(
        after.SelectNodes("//seqpnt")
        |> Seq.cast<XmlElement>
        |> Seq.map (fun x -> x.GetAttribute("visitcount")),
        Is.EquivalentTo [ "11"
                          "10"
                          "9"
                          "8"
                          "7"
                          "6"
                          "4"
                          "3"
                          "2"
                          "1" ]
      )
    finally
      AltCoverCoreTests.maybeDeleteFile reportFile
      Console.SetOut saved
      Directory.SetCurrentDirectory(here)
      AltCoverCoreTests.maybeIOException (fun () -> Directory.Delete(unique))

  [<Test>]
  let FlushLeavesExpectedTracesWhenBroken () =
    let saved = Console.Out
    let here = Directory.GetCurrentDirectory()

    let where =
      Assembly.GetExecutingAssembly().Location
      |> Path.GetDirectoryName

    let unique =
      Path.Combine(where, Guid.NewGuid().ToString())

    let reportFile =
      Path.Combine(unique, "FlushLeavesExpectedTraces.xml")

    let outputFile =
      Path.Combine(unique, "FlushLeavesExpectedTracesWhenBroken.xml")

    try
      let visits =
        new Dictionary<string, Dictionary<int, PointVisit>>()

      use stdout = new StringWriter()
      Console.SetOut stdout
      Directory.CreateDirectory(unique) |> ignore
      Directory.SetCurrentDirectory(unique)

      Counter.measureTime <-
        DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)

      use stream =
        Assembly
          .GetExecutingAssembly()
          .GetManifestResourceStream(resource)

      let size = int stream.Length
      let buffer = Array.create size 0uy
      Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)

      do
        use worker =
          new FileStream(reportFile, FileMode.CreateNew)

        worker.Write(buffer, 0, 0)
        ()

      let payload = Dictionary<int, PointVisit>()

      [ 0 .. 9 ]
      |> Seq.iter (fun i -> payload.[i] <- pointVisitInit(int64 (i + 1)) [])

      visits.["f6e3edb3-fb20-44b3-817d-f69d1a22fc2f"] <- payload

      Adapter.doFlush(
        visits,
        AltCover.Recorder.ReportFormat.NCover,
        reportFile,
        outputFile
      )
      |> ignore

      use worker' =
        new FileStream(outputFile, FileMode.Open)

      let after = XmlDocument()
      after.Load worker'
      Assert.That(after.OuterXml, Is.EqualTo "<null />")
    finally
      AltCoverCoreTests.maybeDeleteFile reportFile
      Console.SetOut saved
      Directory.SetCurrentDirectory(here)
      AltCoverCoreTests.maybeIOException (fun () -> Directory.Delete(unique))

#if !NET20
  [<Test>]
  let ZipFlushLeavesExpectedTracesWhenDiverted () =
    let saved = Console.Out
    let here = Directory.GetCurrentDirectory()

    let where =
      Assembly.GetExecutingAssembly().Location
      |> Path.GetDirectoryName

    let unique =
      Path.Combine(where, Guid.NewGuid().ToString())

    let reportFile =
      Path.Combine(unique, "FlushLeavesExpectedTraces.xml")

    let outputFile =
      Path.Combine(unique, "FlushLeavesExpectedTracesWhenDiverted.xml")

    try
      let visits =
        new Dictionary<string, Dictionary<int, PointVisit>>()

      use stdout = new StringWriter()
      Console.SetOut stdout
      Directory.CreateDirectory(unique) |> ignore
      Directory.SetCurrentDirectory(unique)

      Counter.measureTime <-
        DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)

      use stream =
        Assembly
          .GetExecutingAssembly()
          .GetManifestResourceStream(resource)

      let size = int stream.Length
      let buffer = Array.create size 0uy
      Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)

      do
        use archive =
          ZipFile.Open(reportFile + ".zip", ZipArchiveMode.Create)

        let entry =
          reportFile
          |> Path.GetFileName
          |> archive.CreateEntry

        use sink = entry.Open()
        sink.Write(buffer, 0, size)
        ()

      let payload = Dictionary<int, PointVisit>()

      [ 0 .. 9 ]
      |> Seq.iter (fun i -> payload.[i] <- pointVisitInit(int64 (i + 1)) [])

      visits.["f6e3edb3-fb20-44b3-817d-f69d1a22fc2f"] <- payload

      Adapter.doFlush(
        visits,
        AltCover.Recorder.ReportFormat.NCover,
        reportFile,
        outputFile
      )
      |> ignore

      use worker' =
        new FileStream(outputFile, FileMode.Open)

      let after = XmlDocument()
      after.Load worker'

      Assert.That(
        after.SelectNodes("//seqpnt")
        |> Seq.cast<XmlElement>
        |> Seq.map (fun x -> x.GetAttribute("visitcount")),
        Is.EquivalentTo [ "11"
                          "10"
                          "9"
                          "8"
                          "7"
                          "6"
                          "4"
                          "3"
                          "2"
                          "1" ]
      )
    finally
      AltCoverCoreTests.maybeDeleteFile reportFile
      Console.SetOut saved
      Directory.SetCurrentDirectory(here)
      AltCoverCoreTests.maybeIOException (fun () -> Directory.Delete(unique))

  [<Test>]
  let ZipFlushLeavesExpectedTracesWhenBroken () =
    let saved = Console.Out
    let here = Directory.GetCurrentDirectory()

    let where =
      Assembly.GetExecutingAssembly().Location
      |> Path.GetDirectoryName

    let unique =
      Path.Combine(where, Guid.NewGuid().ToString())

    let reportFile =
      Path.Combine(unique, "FlushLeavesExpectedTraces.xml")

    let outputFile =
      Path.Combine(unique, "FlushLeavesExpectedTracesWhenBroken.xml")

    let zipFile = reportFile + ".zip"

    try
      let visits =
        new Dictionary<string, Dictionary<int, PointVisit>>()

      use stdout = new StringWriter()
      Console.SetOut stdout
      Directory.CreateDirectory(unique) |> ignore
      Directory.SetCurrentDirectory(unique)

      Counter.measureTime <-
        DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)

      use stream =
        Assembly
          .GetExecutingAssembly()
          .GetManifestResourceStream(resource)

      let size = int stream.Length
      let buffer = Array.create size 0uy

      do
        use worker =
          new FileStream(zipFile, FileMode.CreateNew)

        worker.Write(buffer, 0, 0)
        ()

      let payload = Dictionary<int, PointVisit>()

      [ 0 .. 9 ]
      |> Seq.iter (fun i -> payload.[i] <- pointVisitInit(int64 (i + 1)) [])

      visits.["f6e3edb3-fb20-44b3-817d-f69d1a22fc2f"] <- payload

      Adapter.doFlush(
        visits,
        AltCover.Recorder.ReportFormat.NCover,
        reportFile,
        outputFile
      )
      |> ignore

      use worker' =
        new FileStream(outputFile, FileMode.Open)

      let after = XmlDocument()
      after.Load worker'
      Assert.That(after.OuterXml, Is.EqualTo "<null />")
    finally
      AltCoverCoreTests.maybeDeleteFile reportFile
      AltCoverCoreTests.maybeDeleteFile outputFile
      AltCoverCoreTests.maybeDeleteFile zipFile
      Console.SetOut saved
      Directory.SetCurrentDirectory(here)
      AltCoverCoreTests.maybeIOException (fun () -> Directory.Delete(unique))

  [<Test>]
  let ZipFlushLeavesExpectedTracesWhenBrokenInPlace () =
    let saved = Console.Out
    let here = Directory.GetCurrentDirectory()

    let where =
      Assembly.GetExecutingAssembly().Location
      |> Path.GetDirectoryName

    let unique =
      Path.Combine(where, Guid.NewGuid().ToString())

    let reportFile =
      Path.Combine(unique, "FlushLeavesExpectedTraces.xml")

    let zipFile = reportFile + ".zip"

    try
      let visits =
        new Dictionary<string, Dictionary<int, PointVisit>>()

      use stdout = new StringWriter()
      Console.SetOut stdout
      Directory.CreateDirectory(unique) |> ignore
      Directory.SetCurrentDirectory(unique)

      Counter.measureTime <-
        DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)

      use stream =
        Assembly
          .GetExecutingAssembly()
          .GetManifestResourceStream(resource)

      let size = int stream.Length
      let buffer = Array.create size 0uy

      do
        use worker =
          new FileStream(zipFile, FileMode.CreateNew)

        worker.Write(buffer, 0, 0)
        ()

      let payload = Dictionary<int, PointVisit>()

      [ 0 .. 9 ]
      |> Seq.iter (fun i -> payload.[i] <- pointVisitInit(int64 (i + 1)) [])

      visits.["f6e3edb3-fb20-44b3-817d-f69d1a22fc2f"] <- payload

      Adapter.doFlush(visits, AltCover.Recorder.ReportFormat.NCover, reportFile, null)
      |> ignore

      Assert.That(reportFile |> File.Exists |> not)
      let zipInfo = FileInfo(zipFile)
      Assert.That(zipInfo.Length, Is.EqualTo 0)
    finally
      AltCoverCoreTests.maybeDeleteFile zipFile
      Console.SetOut saved
      Directory.SetCurrentDirectory(here)
      AltCoverCoreTests.maybeIOException (fun () -> Directory.Delete(unique))

  let ZipFlushLeavesExpectedTraces () =
    getMyMethodName "=>"

    lock
      Adapter.Lock
      (fun () ->
        Instance.I.isRunner <- false

        trywithrelease
          (fun () ->
            let saved = Console.Out
            let here = Directory.GetCurrentDirectory()

            let where =
              Assembly.GetExecutingAssembly().Location
              |> Path.GetDirectoryName

            let unique =
              Path.Combine(where, Guid.NewGuid().ToString())

            let save = Instance.I.trace
            Instance.I.trace <- Adapter.makeNullTrace null

            try
              Adapter.VisitsClear()
              use stdout = new StringWriter()
              Console.SetOut stdout
              Directory.CreateDirectory(unique) |> ignore
              Directory.SetCurrentDirectory(unique)

              Counter.measureTime <-
                DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)

              use stream =
                Assembly
                  .GetExecutingAssembly()
                  .GetManifestResourceStream(resource)

              let size = int stream.Length
              let buffer = Array.create size 0uy
              Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)

              do
                use archive =
                  ZipFile.Open(Instance.ReportFile + ".zip", ZipArchiveMode.Create)

                let entry =
                  Instance.ReportFile
                  |> Path.GetFileName
                  |> archive.CreateEntry

                use sink = entry.Open()
                sink.Write(buffer, 0, size)
                ()

              [ 0 .. 9 ]
              |> Seq.iter
                   (fun i ->
                     Adapter.VisitsAdd(
                       "f6e3edb3-fb20-44b3-817d-f69d1a22fc2f",
                       i,
                       (int64 (i + 1))
                     ))

              Adapter.DoExit().Invoke(null, null)
              let head = "Coverage statistics flushing took "
              let tail = " seconds\n"
              let recorded = stdout.ToString().Replace("\r\n", "\n")

              let index1 =
                recorded.IndexOf(head, StringComparison.Ordinal)

              let index2 =
                recorded.IndexOf(tail, StringComparison.Ordinal)

              Assert.That(index1, Is.GreaterThanOrEqualTo 0, recorded)
              Assert.That(index2, Is.GreaterThan index1, recorded)

              use zip =
                ZipFile.Open(Instance.ReportFile + ".zip", ZipArchiveMode.Update)

              let entry =
                Instance.ReportFile
                |> Path.GetFileName
                |> zip.GetEntry

              use worker' = entry.Open()
              let after = XmlDocument()
              after.Load worker'

              Assert.That(
                after.SelectNodes("//seqpnt")
                |> Seq.cast<XmlElement>
                |> Seq.map (fun x -> x.GetAttribute("visitcount")),
                Is.EquivalentTo [ "11"
                                  "10"
                                  "9"
                                  "8"
                                  "7"
                                  "6"
                                  "4"
                                  "3"
                                  "2"
                                  "1" ]
              )
            finally
              Instance.I.trace <- save
              AltCoverCoreTests.maybeDeleteFile Instance.ReportFile
              Adapter.VisitsClear()
              Console.SetOut saved
              Directory.SetCurrentDirectory(here)
              AltCoverCoreTests.maybeIOException (fun () -> Directory.Delete(unique))))

    getMyMethodName "<="
#endif

  // Dead simple sequential operation
  // run only once in Framework mode to avoid contention
  [<Test>]
  let MailboxFunctionsAsExpected () =
    let dummy =
      AltCover.Recorder.ExcludeFromCodeCoverageAttribute()

    Assert.That(dummy, Is.Not.Null)
    RealIdShouldIncrementCount()
    PauseLeavesExpectedTraces()
    ResumeLeavesExpectedTraces()
#if !NET20
    ZipFlushLeavesExpectedTraces()
#endif
    FlushLeavesExpectedTraces()