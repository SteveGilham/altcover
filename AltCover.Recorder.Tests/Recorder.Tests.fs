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

#if RECORDER2
  let resource =
    "AltCover.Recorder2.Tests.SimpleCoverage.xml"

  let resource2 =
    "AltCover.Recorder2.Tests.Sample1WithModifiedOpenCover.xml"

  let resource3 =
    "AltCover.Recorder2.Tests.Sample2NCoverage.xml"
#else
  let resource =
    "AltCover.Recorder.Tests.SimpleCoverage.xml"

  let resource2 =
    "AltCover.Recorder.Tests.Sample1WithModifiedOpenCover.xml"

  let resource3 =
    "AltCover.Recorder.Tests.Sample2NCoverage.xml"
#endif

  let internal updateReport0 (counts, format, coverageFile, outputFile) =
    Counter.I.UpdateReport(
      ignore,
      (fun _ _ -> ()),
      true,
      counts,
      format,
      coverageFile,
      outputFile
    )

  let private updateReport a b =
    updateReport0 (a, ReportFormat.NCover, b, b)
    |> ignore

  let private pointVisitInit a b = AltCoverCoreTests.init (a, b)

  [<Test>]
  let ShouldCoverTrivalClass () =
    let mark = InstrumentationAttribute() // Constructor has all the instrumented code
    Assert.That(mark.Assembly, Is.EqualTo "AltCover.Recorder.g!")
    Assert.That(mark.Configuration, Is.EqualTo "Uninstrumented!!")
    mark.Assembly <- String.Empty
    mark.Configuration <- String.Empty
    Assert.True(String.IsNullOrEmpty mark.Assembly)
    Assert.True(String.IsNullOrEmpty mark.Configuration)

  [<Test>]
  let ShouldFailXmlDataForNativeJson () =
    Assert.Throws<NotSupportedException>(fun () ->
      ignore (Counter.I.XmlByFormat ReportFormat.NativeJson))
    |> ignore

  [<Test>]
  let ShouldBeAbleToGetTheDefaultReportFileName () =
    Assert.True(Instance.ReportFile = "Coverage.Default.xml")

  [<Test>]
  let DefaultAccessorsBehaveAsExpected () =
    let v1 = DateTime.UtcNow.Ticks
    let probe = Instance.I.Clock()
    let v2 = DateTime.UtcNow.Ticks
    Assert.True(Instance.I.Granularity() = 0L)
    Assert.True(probe >= v1)
    Assert.True(probe <= v2)

  [<Test>]
  let ShouldBeLinkingTheCorrectCopyOfThisCode () =
    getMyMethodName "=>"

    let tracer = Tracer.Create String.Empty

    // whitelist test not recorder.g

    let n =
      tracer.GetType().Assembly.GetName().Name

    Assert.That(n, Is.EqualTo "AltCover.Recorder")

    getMyMethodName "<="

  let internal addSample (moduleId, hitPointId, context) =
    Instance.I.TakeSample(Sampling.Single, moduleId, hitPointId, context)

  let internal addSampleUnconditional (moduleId, hitPointId, context) =
    Instance.I.TakeSample(Sampling.All, moduleId, hitPointId, context)

  [<Test>]
  let OnlyNewIdPairShouldBeSampled () =
    getMyMethodName "=>"

    lock Instance.I.visits (fun () ->
      try
        AltCoverCoreTests.ModuleReset [| "module"; "newmodule" |]
        let n = Null() :> Track

        Assert.True(addSample ("module", 23, n), "Test 1")
        Assert.True(addSample ("module", 24, n), "Test 2")
        Assert.True(addSample ("newmodule", 23, n), "Test 3")
        Assert.True(addSample ("module", 23, n) |> not, "Test 4")
        Assert.True(addSampleUnconditional ("module", 23, n), "Test 5")
        Assert.True(addSample ("module", 23, Call 1), "Test 6")
        Assert.True(addSample ("module", 23, Time 0L), "Test 7")
        Assert.True(addSample ("module", 23, Time 1L), "Test 7a")
        Assert.True(addSample ("module", 23, Time 0L) |> not, "Test 7b")

        Assert.True(addSample ("module", 24, new Both(Pair.Create(0, 1))), "Test 8")

        Assert.True(addSample ("module", 25, new Both(Pair.Create(0, 1))), "Test 9")

        Assert.True(addSample ("module", 25, Call 1) |> not, "Test 10")
        Assert.True(addSample ("module", 25, Call 1) |> not, "Test 11")
        Assert.True(addSample ("module", 25, n) |> not, "Test 12")

        // out of band example
        Assert.True(addSample ("nonesuch", 25, n) |> not, "Test 12a")

        Assert.Throws<InvalidDataException>(fun () ->
          addSample ("module", 23, Table null) |> ignore)
        |> ignore
      finally
        AltCoverCoreTests.HardReset())

    getMyMethodName "<="

  let VisitsEntrySeq key =
    Instance.I.visits.[key]
    |> Seq.cast<KeyValuePair<int, PointVisit>>

  let VisitCount (key, key2) = (Instance.I.visits.[key].[key2]).Count

  [<Test>]
  let RealIdShouldIncrementCount () =
    getMyMethodName "=>"

    lock Instance.I.visits (fun () ->
      let save = Instance.I.Trace

      try
        let key = " "
        AltCoverCoreTests.ModuleReset [| key |]
        Instance.I.Trace <- Tracer.Create null

        Instance.I.Recording <- false
        Instance.Visit("key", 17)
        Instance.I.Recording <- true
        Instance.CoverageFormat <- ReportFormat.NCover
        Instance.Visit(key, -23)

        Instance.CoverageFormat <-
          ReportFormat.OpenCover
          ||| ReportFormat.WithTracking

        Instance.Visit(key, -23)

        let vs = AltCoverCoreTests.VisitsSeq()

        Assert.True(
          vs |> Seq.length = 3,
          sprintf "AltCoverCoreTests.VisitsSeq() = %A" vs
        )

        let vesk = VisitsEntrySeq key

        Assert.True(vesk |> Seq.length = 1, sprintf "VisitsEntrySeq %A = %A" key vesk)

        Assert.That(VisitCount(key, -23), Is.EqualTo 2L)
        Assert.That(Counter.totalVisits, Is.EqualTo 1L)
        Assert.That(Counter.branchVisits, Is.EqualTo 1L)
      finally
        Instance.CoverageFormat <- ReportFormat.NCover
        Instance.I.Recording <- true
        AltCoverCoreTests.HardReset()
        Instance.I.Trace <- save)

    getMyMethodName "<="

  let internal payloadSelector x = Instance.I.PayloadSelector(fun _ -> x)

  let internal payloadControl (x, y) =
    Instance.I.PayloadControl((fun _ -> x), (fun _ -> y))

  let internal payloadSelection (x, y, z) =
    Instance.I.PayloadSelection((fun _ -> x), (fun _ -> y), (fun _ -> z))

  let internal untime (at: Track) =
    match at with
    | :? Time as t -> Some t.Value
    | _ -> None

  [<Test>]
  let JunkUspidGivesNegativeIndex () =
    let key = " "

    let index =
      Counter.I.FindIndexFromUspid(0, key)

    Assert.True(index < 0)

  [<Test>]
  let PayloadGeneratedIsAsExpected () =
    try
      Instance.I.isRunner <- false

      Instance.CoverageFormat <-
        ReportFormat.OpenCover
        ||| ReportFormat.WithTracking

      Assert.False(Instance.I.CallerId.HasValue)
      Assert.That(payloadSelector false, Is.EqualTo <| Null())
      Assert.That(payloadSelector true, Is.EqualTo <| Null())
      Instance.Push 4321
      Assert.That(payloadSelector false, Is.EqualTo <| Null())
      Assert.That(payloadSelector true, Is.EqualTo <| (Call 4321))

      try
        Instance.Push 6789
        // 0x1234123412341234 == 1311693406324658740
        let result =
          payloadSelection (1311693406324658740L, 1000L, true)

        let expected =
          AltCoverCoreTests.newBoth (1311693406324658000L, 6789)

        Assert.True((result = expected))
      finally
        Instance.Pop()

      Assert.That(payloadSelector true, Is.EqualTo(Call 4321))
    finally
      Instance.Pop()
      Instance.CoverageFormat <- ReportFormat.NCover

    let result2 =
      payloadSelection (1311693406324658740L, 1000L, true)

    let expected2 = Time 1311693406324658000L

    Assert.True((result2 = expected2))
    let v1 = DateTime.UtcNow.Ticks

    let probed = payloadControl (1000L, true)

    let v2 = DateTime.UtcNow.Ticks
    Assert.True(Null() |> untime |> Option.isNone)

    let probe = (untime probed).Value

    Assert.True(probe % 1000L = 0L)
    Assert.True(probe <= v2)
    Assert.True(probe >= (1000L * (v1 / 1000L)))
    Assert.True(Instance.I.CallerId.HasValue |> not)
    Instance.Pop()
    Assert.True(Instance.I.CallerId.HasValue |> not)

  [<Test>]
  let PayloadWithEntryExitGeneratedIsAsExpected () =
    AltCoverCoreTests.ModuleReset [||]

    try
      Instance.I.isRunner <- true

      Instance.CoverageFormat <-
        ReportFormat.OpenCover
        ||| ReportFormat.WithTracking

      AltCoverCoreTests.VisitsClear()

      Assert.True(Instance.I.CallerId.HasValue |> not)
      Assert.That(payloadSelector false, Is.EqualTo <| Null())
      Assert.That(payloadSelector true, Is.EqualTo <| Null())
      Instance.Push 4321
      Assert.That(payloadSelector false, Is.EqualTo <| Null())
      Assert.That(payloadSelector true, Is.EqualTo(Call 4321))

      try
        Instance.Push 6789
        // 0x1234123412341234 == 1311693406324658740
        let result =
          payloadSelection (1311693406324658740L, 1000L, true)

        let expected =
          AltCoverCoreTests.newBoth (1311693406324658000L, 6789)

        Assert.True((result = expected))
      finally
        Instance.Pop()

      Assert.That(payloadSelector true, Is.EqualTo(Call 4321))
    finally
      Instance.Pop()
      Instance.I.isRunner <- false
      Instance.CoverageFormat <- ReportFormat.NCover

    let result2 =
      payloadSelection (1311693406324658740L, 1000L, true)

    let expected2 = Time 1311693406324658000L

    Assert.True((result2 = expected2))
    let v1 = DateTime.UtcNow.Ticks

    let probed = payloadControl (1000L, true)

    let v2 = DateTime.UtcNow.Ticks
    Assert.True(Null() |> untime |> Option.isNone)

    let probe = (untime probed).Value

    Assert.True(probe % 1000L = 0L)
    Assert.True(probe <= v2)
    Assert.True(probe >= (1000L * (v1 / 1000L)))
    Assert.True(Instance.I.CallerId.HasValue |> not)
    Instance.Pop()
    Assert.True(Instance.I.CallerId.HasValue |> not)

    Assert.That(Instance.I.visits.Keys, Is.EquivalentTo [ Track.Entry; Track.Exit ])

    Assert.That(Instance.I.visits.[Track.Entry].Keys, Is.EquivalentTo [ 4321; 6789 ])
    Assert.That(Instance.I.visits.[Track.Exit].Keys, Is.EquivalentTo [ 4321; 6789 ])

    let a =
      Instance.I.visits.[Track.Entry].[4321]

    Assert.That(a.Count, Is.EqualTo 0L)
    Assert.That(a.Tracks |> Seq.length, Is.EqualTo 1)

    let b =
      Instance.I.visits.[Track.Entry].[6789]

    Assert.That(b.Count, Is.EqualTo 0L)
    Assert.That(b.Tracks |> Seq.length, Is.EqualTo 1)

    let c =
      Instance.I.visits.[Track.Exit].[6789]

    Assert.That(c.Count, Is.EqualTo 0L)
    Assert.That(c.Tracks |> Seq.length, Is.EqualTo 1)

    let d =
      Instance.I.visits.[Track.Exit].[4321]

    Assert.That(d.Count, Is.EqualTo 0L)
    Assert.That(d.Tracks |> Seq.length, Is.EqualTo 1)

    let a2 =
      (a.Tracks |> Seq.head |> untime).Value

    let b2 =
      (b.Tracks |> Seq.head |> untime).Value

    Assert.That(b2 >= a2)

    let c2 =
      (c.Tracks |> Seq.head |> untime).Value

    Assert.That(c2 >= b2)

    let d2 =
      (d.Tracks |> Seq.head |> untime).Value

    Assert.That(d2 >= c2, sprintf "%A >= %A" d2 c2)

    AltCoverCoreTests.HardReset()

  [<Test>]
  let RealIdShouldIncrementCountSynchronously () =
    getMyMethodName "=>"

    lock Instance.I.visits (fun () ->
      let save = Instance.I.Trace
      let key = " "
      AltCoverCoreTests.ModuleReset [| key |]

      try
        Instance.I.Trace <- Tracer.Create null

        Instance.I.VisitSelection(Null(), key, 23)

        Assert.That(
          Instance.I.visits.Keys,
          Is.EquivalentTo [ key; Track.Entry; Track.Exit ]
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
        AltCoverCoreTests.HardReset()
        Instance.I.Trace <- save)

    getMyMethodName "<="

  let strip before (all: HashSet<String>) =
    before
    |> Seq.iter (fun x ->
      Assert.That(all.Contains x)
      all.Remove x |> ignore)

  let internal invokeCurriedIssue71Wrapper<'T when 'T :> System.Exception>
    (unique: string)
    =
    let constructor =
      typeof<'T>
        .GetConstructor([| typeof<System.String> |])

    let pitcher =
      fun _ _ _ _ ->
        constructor.Invoke([| unique |]) :?> System.Exception
        |> raise

    Instance.I.CurriedIssue71Wrapper<string, string, string, string>(
      "a",
      "b",
      "c",
      "d",
      pitcher
    )
    |> ignore

  let internal invokeIssue71Wrapper<'T when 'T :> System.Exception>
    ((unique: string), (called: bool array))
    =
    let constructor =
      typeof<'T>
        .GetConstructor([| typeof<System.String> |])

    let pitcher =
      fun _ _ _ _ ->
        constructor.Invoke([| unique |]) :?> System.Exception
        |> raise

    let catcher =
      fun _ _ _ (x: System.Exception) ->
        called.[0] <- true

        called.[1] <-
          match x with
          | :? System.ArgumentNullException as ane -> ane.ParamName = unique
          | _ -> x.Message = unique

    Instance.I.Issue71Wrapper((), (), (), (), catcher, pitcher)
    |> ignore

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
    let path =
      Instance.ReportFilePath |> Path.GetFullPath

    let where = path |> Path.GetDirectoryName

    let before =
      Directory.GetFiles(where, "*.exn")

    Instance.I.LogException("a", "b", "c", "ex")

    let after =
      Directory.GetFiles(where, "*.exn")

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
      [ "ModuleId = a"
        "hitPointId = b"
        "context = c"
        "exception = ex" ]
    |> Seq.iter (fun (a, b) -> Assert.That(a, Is.EqualTo b))

    let third =
      Directory.GetFiles(where, "*.exn")

    Assert.That(third.Length, Is.EqualTo before.Length)

  [<Test>]
  let WrappedExceptionLoggedToFile () =
    let path =
      Instance.ReportFilePath |> Path.GetFullPath

    let where = path |> Path.GetDirectoryName

    let before =
      Directory.GetFiles(where, "*.exn")

    let unique =
      System.Guid.NewGuid().ToString()

    invokeCurriedIssue71Wrapper<NullReferenceException> unique

    let after =
      Directory.GetFiles(where, "*.exn")

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
      [ "ModuleId = b"
        "hitPointId = c"
        "context = d"
        "exception = System.NullReferenceException: "
        + unique ]
    |> Seq.iter (fun (a, b) -> Assert.That(b, Is.EqualTo a))

    let third =
      Directory.GetFiles(where, "*.exn")

    Assert.That(third.Length, Is.EqualTo before.Length)

  [<Test>]
  let Issue71WrapperHandlesKeyNotFoundException () =
    let pair = [| false; false |]

    let unique =
      System.Guid.NewGuid().ToString()

    invokeIssue71Wrapper<KeyNotFoundException> (unique, pair)
    Assert.That(pair |> Seq.head, Is.True)
    Assert.That(pair |> Seq.last, Is.True)

  [<Test>]
  let Issue71WrapperHandlesNullReferenceException () =
    let pair = [| false; false |]

    let unique =
      System.Guid.NewGuid().ToString()

    invokeIssue71Wrapper<NullReferenceException> (unique, pair)
    Assert.That(pair |> Seq.head, Is.True)
    Assert.That(pair |> Seq.last, Is.True)

  [<Test>]
  let Issue71WrapperHandlesArgumentNullException () =
    let pair = [| false; false |]

    let unique =
      System.Guid.NewGuid().ToString()

    invokeIssue71Wrapper<ArgumentNullException> (unique, pair)
    Assert.That(pair |> Seq.head, Is.True)
    Assert.That(pair |> Seq.last, Is.True)

  [<Test>]
  let Issue71WrapperDoesntHandleOtherException () =
    let pair = [| false; false |]

    let unique =
      System.Guid.NewGuid().ToString()

    let exn =
      Assert.Throws<InvalidOperationException>(fun () ->
        invokeIssue71Wrapper<InvalidOperationException> (unique, pair))

    Assert.That(pair |> Seq.head, Is.False)
    Assert.That(pair |> Seq.last, Is.False)
    Assert.That(exn.Message, Is.EqualTo unique)

#if !NET472 && !NET20
  [<Test>]
  let NullRefShouldBeHandled () =
    getMyMethodName "=>"
    let handle = Instance.I.visits

    lock handle (fun () ->
      try
        Instance.I.visits <- null
        let key = " "

        let path =
          Instance.ReportFilePath |> Path.GetFullPath

        let where = path |> Path.GetDirectoryName

        let before =
          Directory.GetFiles(where, "*.exn")

        Instance.I.VisitImpl(key, 23, Null())

        let after =
          Directory.GetFiles(where, "*.exn")

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
          [ "ModuleId =  "
            "hitPointId = 23"
            "context = AltCover.Null"
            "exception = System.NullReferenceException: Object reference not set to an instance of an object." ]
        |> Seq.iter (fun (a, b) -> Assert.That(a, Is.EqualTo(b)))

        let third =
          Directory.GetFiles(where, "*.exn")

        Assert.That(third.Length, Is.EqualTo before.Length)
      finally
        Instance.I.visits <- handle
        Instance.I.visits.Clear())

    getMyMethodName "<="
#endif

  [<Test>]
  let DistinctIdShouldBeDistinct () =
    getMyMethodName "=>"

    lock Instance.I.visits (fun () ->
      try
        let key = " "
        AltCoverCoreTests.ModuleReset [| key; "key" |]
        Instance.I.VisitImpl(key, 23, Null())
        Instance.I.VisitImpl("key", 42, Null())

        Assert.That(
          Instance.I.visits.Keys,
          Is.EquivalentTo [ key; "key"; Track.Entry; Track.Exit ]
        )
      finally
        AltCoverCoreTests.HardReset())

    getMyMethodName "<="

  [<Test>]
  let DistinctLineShouldBeDistinct () =
    getMyMethodName "=>"

    lock Instance.I.visits (fun () ->
      try
        let key = " "
        AltCoverCoreTests.ModuleReset [| key |]
        Instance.I.VisitImpl(key, 23, Null())
        Instance.I.VisitImpl(key, 42, Null())

        Assert.That(
          Instance.I.visits.Keys,
          Is.EquivalentTo [ key; Track.Entry; Track.Exit ]
        )

        Assert.That(Instance.I.visits.[key].Count, Is.EqualTo 2)
      finally
        AltCoverCoreTests.HardReset())

    getMyMethodName "<="

  [<Test>]
  let RepeatVisitsShouldIncrementCount () =
    getMyMethodName "=>"

    lock Instance.I.visits (fun () ->
      let key = " "
      AltCoverCoreTests.ModuleReset [| key |]

      try
        Instance.I.VisitImpl(key, 23, Null())
        Instance.I.VisitImpl(key, 23, Null())
        Assert.That(Instance.I.visits.[key].[23].Count, Is.EqualTo 2)
        Assert.That(Instance.I.visits.[key].[23].Tracks, Is.Empty)
      finally
        AltCoverCoreTests.HardReset())

    getMyMethodName "<="

  [<Test>]
  let RepeatVisitsShouldIncrementTotal () =
    getMyMethodName "=>"

    lock Instance.I.visits (fun () ->
      let key = " "
      AltCoverCoreTests.ModuleReset [| key |]

      try
        let payload = Time DateTime.UtcNow.Ticks

        Instance.I.VisitImpl(key, 23, Null())
        Instance.I.VisitImpl(key, 23, payload)
        Assert.That(Instance.I.visits.[key].[23].Count, Is.EqualTo 1)
        Assert.That(Instance.I.visits.[key].[23].Tracks, Is.EquivalentTo [ payload ])
      finally
        AltCoverCoreTests.HardReset())

    getMyMethodName "<="

  [<Test>]
  let OldDocumentStartIsNotUpdated () =
    getMyMethodName "=>"

    lock Instance.I.visits (fun () ->
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

      updateReport (Dictionary<string, Dictionary<int, PointVisit>>()) worker
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

    lock Instance.I.visits (fun () ->
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

      updateReport (Dictionary<string, Dictionary<int, PointVisit>>()) worker
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

    lock Instance.I.visits (fun () ->
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

      updateReport (Dictionary<string, Dictionary<int, PointVisit>>()) worker
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

    lock Instance.I.visits (fun () ->
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

      updateReport (Dictionary<string, Dictionary<int, PointVisit>>()) worker
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

    lock Instance.I.visits (fun () ->
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

    lock Instance.I.visits (fun () ->
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

    lock Instance.I.visits (fun () ->
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

    lock Instance.I.visits (fun () ->
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

      [ 0..9 ]
      |> Seq.iter (fun i -> payload.[i] <- pointVisitInit (int64 (i + 1)) [])

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
        Is.EquivalentTo
          [ "11"
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
  let KnownModuleWithPayloadMakesExpectedComplexChange () =
    getMyMethodName "=>"

    lock Instance.I.visits (fun () ->
      Counter.measureTime <-
        DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)

      use stream =
        Assembly
          .GetExecutingAssembly()
          .GetManifestResourceStream(resource3)

      let size = int stream.Length
      let buffer = Array.create size 0uy
      Assert.That(stream.Read(buffer, 0, size), Is.EqualTo size)
      use worker = new MemoryStream()
      worker.Write(buffer, 0, size)
      worker.Position <- 0L
      let payload = Dictionary<int, PointVisit>()

      payload.[5] <- pointVisitInit 1L []
      payload.[1] <- pointVisitInit 1L []
      payload.[-2147483648] <- pointVisitInit 1L []
      payload.[4] <- pointVisitInit 1L []
      payload.[2] <- pointVisitInit 1L []
      payload.[-2147483646] <- pointVisitInit 1L []
      payload.[3] <- pointVisitInit 1L []
      payload.[13] <- pointVisitInit 1L []
      payload.[12] <- pointVisitInit 1L []
      payload.[-2147483643] <- pointVisitInit 1L []
      payload.[11] <- pointVisitInit 1L []
      payload.[18] <- pointVisitInit 1L []
      payload.[16] <- pointVisitInit 2L []
      payload.[-2147483640] <- pointVisitInit 1L []
      payload.[17] <- pointVisitInit 1L []
      payload.[19] <- pointVisitInit 1L []

      let item =
        Dictionary<string, Dictionary<int, PointVisit>>()

      item.Add("f7f80a77-e131-f5c7-da33-cd3b6036778a", payload)
      updateReport item worker
      worker.Position <- 0L
      let after = XmlDocument()
      after.Load worker

      let join (x: string seq) = String.Join(" ", x)

      Assert.That(
        after.SelectNodes("//seqpnt")
        |> Seq.cast<XmlElement>
        |> Seq.map (fun x -> x.GetAttribute("visitcount"))
        |> join,
        Is.EqualTo "0 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 2 1 1 1"

      ))

    getMyMethodName "<="

  [<Test>]
  let KnownModuleWithPayloadMakesExpectedChangeInOpenCover () =
    getMyMethodName "=>"

    lock Instance.I.visits (fun () ->
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

      [ 0..9 ]
      |> Seq.iter (fun i -> payload.[10 - i] <- pointVisitInit (int64 (i + 1)) [])

      [ 11..12 ]
      |> Seq.iter (fun i ->
        payload.[i ||| Counter.branchFlag] <- pointVisitInit (int64 (i - 10)) [])

      let item =
        Dictionary<string, Dictionary<int, PointVisit>>()

      item.Add("7C-CD-66-29-A3-6C-6D-5F-A7-65-71-0E-22-7D-B2-61-B5-1F-65-9A", payload)

      updateReport0 (item, ReportFormat.OpenCover, worker, worker)
      |> ignore

      worker.Position <- 0L
      let after = XmlDocument()
      after.Load worker

      Assert.That(
        after.SelectNodes("//SequencePoint")
        |> Seq.cast<XmlElement>
        |> Seq.map (fun x -> x.GetAttribute("vc")),
        Is.EquivalentTo
          [ "11"
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

    lock Instance.I.visits (fun () ->
      let saved = Console.Out

      try
        AltCoverCoreTests.VisitsClear()
        use stdout = new StringWriter()
        Console.SetOut stdout
        Instance.FlushFinish()
        Assert.That(stdout.ToString(), Is.Empty)
      finally
        AltCoverCoreTests.VisitsClear()
        Console.SetOut saved)

    getMyMethodName "<="

  let trywith<'a when 'a :> exn> f g =
    try
      f ()
    with :? 'a ->
      g ()

  let failsaferelease () =
    try
      Instance.I.mutex.ReleaseMutex()
    with :? ApplicationException ->
      ()

  let trywithrelease<'a when 'a :> exn> f = trywith f failsaferelease

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

    trywithrelease<InvalidOperationException> (fun () ->
      InvalidOperationException() |> raise)

    Instance.I.mutex.WaitOne(1000) |> ignore

    trywithrelease<InvalidOperationException> (fun () ->
      InvalidOperationException() |> raise)

  let internal makeStreamTrace s1 =
    let mutable t = Tracer.Create(null)
    // fsharplint:disable-next-line  RedundantNewKeyword
    t.Stream <- new System.IO.MemoryStream()
    // fsharplint:disable-next-line  RedundantNewKeyword
    t.Formatter <- new System.IO.BinaryWriter(s1)
    t.Runner <- true
    t.Definitive <- false
    t

  [<Test>]
  let PauseLeavesExpectedTraces () =
    getMyMethodName "=>"

    lock Instance.I.visits (fun () ->
      trywithrelease (fun () ->
        let saved = Console.Out
        let here = Directory.GetCurrentDirectory()

        let where =
          Assembly.GetExecutingAssembly().Location
          |> Path.GetDirectoryName

        let unique =
          Path.Combine(where, Guid.NewGuid().ToString())

        let save = Instance.I.Trace
        use s = new MemoryStream()

        let s1 =
          new Compression.DeflateStream(s, CompressionMode.Compress)

        Instance.I.Trace <- makeStreamTrace s1

        try
          Instance.I.isRunner <- true
          AltCoverCoreTests.VisitsClear()

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
              new FileStream(Instance.ReportFilePath, FileMode.CreateNew)

            worker.Write(buffer, 0, size)
            ()

          [ 0..9 ]
          |> Seq.iter (fun i ->
            AltCoverCoreTests.VisitsAdd(
              "f6e3edb3-fb20-44b3-817d-f69d1a22fc2f",
              i,
              (int64 (i + 1))
            ))

          let nullObj: obj = null
          Instance.I.doPause.Invoke(nullObj, null)

          AltCoverCoreTests.VisitsSeq()
          |> Seq.iter (fun v ->
            Assert.That(v.Value, Is.Empty, sprintf "Unexpected write %A" v))

          let recorded = stdout.ToString().Trim()
          Assert.That(recorded, Is.EqualTo "Pausing...")

          use worker' =
            new FileStream(Instance.ReportFilePath, FileMode.Open)

          let after = XmlDocument()
          after.Load worker'

          Assert.That(
            after.SelectNodes("//seqpnt")
            |> Seq.cast<XmlElement>
            |> Seq.map (fun x -> x.GetAttribute("visitcount")),
            Is.EquivalentTo
              [ "1"
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
          Instance.I.Trace <- save
          AltCoverCoreTests.maybeDeleteFile Instance.ReportFilePath
          AltCoverCoreTests.VisitsClear()
          Instance.I.isRunner <- false
          Console.SetOut saved
          Directory.SetCurrentDirectory(here)
          AltCoverCoreTests.maybeIOException (fun () -> Directory.Delete(unique))))

    getMyMethodName "<="

  [<Test>]
  let ResumeLeavesExpectedTraces () =
    getMyMethodName "=>"

    lock Instance.I.visits (fun () ->
      trywithrelease (fun () ->
        let saved = Console.Out
        let here = Directory.GetCurrentDirectory()

        let where =
          Assembly.GetExecutingAssembly().Location
          |> Path.GetDirectoryName

        let unique =
          Path.Combine(where, Guid.NewGuid().ToString())

        let save = Instance.I.Trace
        let tag = unique + ".xml.acv"

        do
          use stream = File.Create tag
          ()

        try
          let key =
            "f6e3edb3-fb20-44b3-817d-f69d1a22fc2f"

          AltCoverCoreTests.ModuleReset [| key |]
          Instance.I.Trace <- Tracer.Create(tag)

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
              new FileStream(Instance.ReportFilePath, FileMode.CreateNew)

            worker.Write(buffer, 0, size)
            ()

          [ 0..9 ]
          |> Seq.iter (fun i -> AltCoverCoreTests.VisitsAdd(key, i, (int64 (i + 1))))

          let nullObj: obj = null
          Instance.I.doResume.Invoke(nullObj, null)

          AltCoverCoreTests.VisitsSeq()
          |> Seq.iter (fun v ->
            Assert.That(v.Value, Is.Empty, sprintf "Visits should be cleared %A" v))

          Assert.That(
            Object.ReferenceEquals(Instance.I.Trace, save),
            Is.False,
            "trace should be replaced"
          )

          let recorded = stdout.ToString().Trim()
          Assert.That(recorded, Is.EqualTo "Resuming...", recorded)

          use worker' =
            new FileStream(Instance.ReportFilePath, FileMode.Open)

          let after = XmlDocument()
          after.Load worker'

          Assert.That(
            after.SelectNodes("//seqpnt")
            |> Seq.cast<XmlElement>
            |> Seq.map (fun x -> x.GetAttribute("visitcount")),
            Is.EquivalentTo
              [ "1"
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
          AltCoverCoreTests.HardReset()
          Instance.I.Trace <- save
          AltCoverCoreTests.maybeDeleteFile Instance.ReportFilePath
          AltCoverCoreTests.VisitsClear()
          Console.SetOut saved
          Directory.SetCurrentDirectory(here)
          File.Delete tag
          AltCoverCoreTests.maybeIOException (fun () -> Directory.Delete(unique))))

    getMyMethodName "<="

  [<Test>]
  let FlushLeavesExpectedTraces () =
    getMyMethodName "=>"

    lock Instance.I.visits (fun () ->
      Instance.I.isRunner <- false

      trywithrelease (fun () ->
        let saved = Console.Out
        let here = Directory.GetCurrentDirectory()

        let where =
          Assembly.GetExecutingAssembly().Location
          |> Path.GetDirectoryName

        let unique =
          Path.Combine(where, Guid.NewGuid().ToString())

        let save = Instance.I.Trace
        Instance.I.Trace <- Tracer.Create null

        try
          AltCoverCoreTests.VisitsClear()
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
              new FileStream(Instance.ReportFilePath, FileMode.CreateNew)

            worker.Write(buffer, 0, size)
            ()

          [ 0..9 ]
          |> Seq.iter (fun i ->
            AltCoverCoreTests.VisitsAdd(
              "f6e3edb3-fb20-44b3-817d-f69d1a22fc2f",
              i,
              (int64 (i + 1))
            ))

          let nullObj: obj = null
          Instance.I.doExit.Invoke(nullObj, null)

          let head =
            "Coverage statistics flushing took "

          let tail = " seconds\n"

          let recorded =
            stdout.ToString().Replace("\r\n", "\n")

          let index1 =
            recorded.IndexOf(head, StringComparison.Ordinal)

          let index2 =
            recorded.IndexOf(tail, StringComparison.Ordinal)

          Assert.That(index1, Is.GreaterThanOrEqualTo 0, recorded)
          Assert.That(index2, Is.GreaterThan index1, recorded)

          use worker' =
            new FileStream(Instance.ReportFilePath, FileMode.Open)

          let after = XmlDocument()
          after.Load worker'

          Assert.That(
            after.SelectNodes("//seqpnt")
            |> Seq.cast<XmlElement>
            |> Seq.map (fun x -> x.GetAttribute("visitcount")),
            Is.EquivalentTo
              [ "11"
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
          Instance.I.Trace <- save
          AltCoverCoreTests.maybeDeleteFile Instance.ReportFilePath
          AltCoverCoreTests.VisitsClear()
          Console.SetOut saved
          Directory.SetCurrentDirectory(here)
          AltCoverCoreTests.maybeIOException (fun () -> Directory.Delete(unique))))

    getMyMethodName "<="

  [<Test>]
  let SupervisedFlushLeavesExpectedTraces () =
    getMyMethodName "=>"

    lock Instance.I.visits (fun () ->
      trywithrelease (fun () ->
        let saved = Console.Out
        let here = Directory.GetCurrentDirectory()

        let where =
          Assembly.GetExecutingAssembly().Location
          |> Path.GetDirectoryName

        let unique =
          Path.Combine(where, Guid.NewGuid().ToString())

        let save = Instance.I.Trace
        Instance.I.Trace <- Tracer.Create null

        Instance.supervision <- true

        try
          AltCoverCoreTests.VisitsClear()
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
              new FileStream(Instance.ReportFilePath, FileMode.CreateNew)

            worker.Write(buffer, 0, size)
            ()

          [ 0..9 ]
          |> Seq.iter (fun i ->
            AltCoverCoreTests.VisitsAdd(
              "f6e3edb3-fb20-44b3-817d-f69d1a22fc2f",
              i,
              (int64 (i + 1))
            ))

          let nullObj: obj = null
          Instance.I.doUnload.Invoke(nullObj, null)

          let head =
            "Coverage statistics flushing took "

          let tail = " seconds\n"

          let recorded =
            stdout.ToString().Replace("\r\n", "\n")

          Assert.That(recorded, Is.Empty, recorded)

          use worker' =
            new FileStream(Instance.ReportFilePath, FileMode.Open)

          let after = XmlDocument()
          after.Load worker'

          Assert.That(
            after.SelectNodes("//seqpnt")
            |> Seq.cast<XmlElement>
            |> Seq.map (fun x -> x.GetAttribute("visitcount")),
            Is.EquivalentTo
              [ "1"
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
          Instance.I.Trace <- save
          Instance.supervision <- false
          AltCoverCoreTests.maybeDeleteFile Instance.ReportFilePath
          AltCoverCoreTests.VisitsClear()
          Console.SetOut saved
          Directory.SetCurrentDirectory(here)
          AltCoverCoreTests.maybeIOException (fun () -> Directory.Delete(unique))))

    getMyMethodName "<="

  let internal doFlush (visits, format, report, output) =
    let output' =
      if System.String.IsNullOrEmpty output then
        null
      else
        output

    Counter.DoFlushFile(ignore, (fun _ _ -> ()), true, visits, format, report, output')

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

      [ 0..9 ]
      |> Seq.iter (fun i -> payload.[i] <- pointVisitInit (int64 (i + 1)) [])

      visits.["f6e3edb3-fb20-44b3-817d-f69d1a22fc2f"] <- payload

      doFlush (visits, ReportFormat.NCover, reportFile, outputFile)
      |> ignore

      use worker' =
        new FileStream(outputFile, FileMode.Open)

      let after = XmlDocument()
      after.Load worker'

      Assert.That(
        after.SelectNodes("//seqpnt")
        |> Seq.cast<XmlElement>
        |> Seq.map (fun x -> x.GetAttribute("visitcount")),
        Is.EquivalentTo
          [ "11"
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

      [ 0..9 ]
      |> Seq.iter (fun i -> payload.[i] <- pointVisitInit (int64 (i + 1)) [])

      visits.["f6e3edb3-fb20-44b3-817d-f69d1a22fc2f"] <- payload

      doFlush (visits, ReportFormat.NCover, reportFile, outputFile)
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
    let mutable complete = false

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

      [ 0..9 ]
      |> Seq.iter (fun i -> payload.[i] <- pointVisitInit (int64 (i + 1)) [])

      visits.["f6e3edb3-fb20-44b3-817d-f69d1a22fc2f"] <- payload

      doFlush (
        visits,
        ReportFormat.NCover ||| ReportFormat.Zipped,
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
        Is.EquivalentTo
          [ "11"
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

      complete <- true
    finally
      AltCoverCoreTests.maybeDeleteFile reportFile
      Console.SetOut saved
      Directory.SetCurrentDirectory(here)
      AltCoverCoreTests.maybeIOException (fun () -> Directory.Delete(unique))
      Assert.That(complete, Is.True, "incomplete")
#endif

  [<Test>]
  let ZipFlushLeavesExpectedTracesWhenBroken () =
    let saved = Console.Out
    let here = Directory.GetCurrentDirectory()
    let mutable complete = false

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

      [ 0..9 ]
      |> Seq.iter (fun i -> payload.[i] <- pointVisitInit (int64 (i + 1)) [])

      visits.["f6e3edb3-fb20-44b3-817d-f69d1a22fc2f"] <- payload

      doFlush (
        visits,
        ReportFormat.NCover ||| ReportFormat.Zipped,
        reportFile,
        outputFile
      )
      |> ignore

      use worker' =
        new FileStream(outputFile, FileMode.Open)

      let after = XmlDocument()
      after.Load worker'
      Assert.That(after.OuterXml, Is.EqualTo "<null />")
      complete <- true
    finally
      AltCoverCoreTests.maybeDeleteFile reportFile
      AltCoverCoreTests.maybeDeleteFile outputFile
      AltCoverCoreTests.maybeDeleteFile zipFile
      Console.SetOut saved
      Directory.SetCurrentDirectory(here)
      AltCoverCoreTests.maybeIOException (fun () -> Directory.Delete(unique))
      Assert.That(complete, Is.True, "incomplete")

  [<Test>]
  let ZipFlushLeavesExpectedTracesWhenBrokenInPlace () =
    let saved = Console.Out
    let here = Directory.GetCurrentDirectory()
    let mutable complete = false

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

      [ 0..9 ]
      |> Seq.iter (fun i -> payload.[i] <- pointVisitInit (int64 (i + 1)) [])

      visits.["f6e3edb3-fb20-44b3-817d-f69d1a22fc2f"] <- payload

      doFlush (visits, ReportFormat.NCover ||| ReportFormat.Zipped, reportFile, null)
      |> ignore

      Assert.That(reportFile |> File.Exists |> not)
      let zipInfo = FileInfo(zipFile)
      Assert.That(zipInfo.Length, Is.EqualTo 0)
      complete <- true
    finally
      AltCoverCoreTests.maybeDeleteFile zipFile
      Console.SetOut saved
      Directory.SetCurrentDirectory(here)
      AltCoverCoreTests.maybeIOException (fun () -> Directory.Delete(unique))
      Assert.That(complete, Is.True, "incomplete")

#if !NET20
  [<Test>]
  let ZipFlushLeavesExpectedTraces () =
    getMyMethodName "=>"
    let mutable complete = false

    lock Instance.I.visits (fun () ->
      Instance.I.isRunner <- false
      Instance.CoverageFormat <- ReportFormat.NCover ||| ReportFormat.Zipped

      trywithrelease (fun () ->
        let saved = Console.Out
        let here = Directory.GetCurrentDirectory()

        let where =
          Assembly.GetExecutingAssembly().Location
          |> Path.GetDirectoryName

        let unique =
          Path.Combine(where, Guid.NewGuid().ToString())

        let save = Instance.I.Trace
        Instance.I.Trace <- Tracer.Create null

        try
          AltCoverCoreTests.VisitsClear()
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
            AltCoverCoreTests.maybeDeleteFile (Instance.ReportFilePath + ".zip")

            use archive =
              ZipFile.Open(Instance.ReportFilePath + ".zip", ZipArchiveMode.Create)

            let entry =
              Instance.ReportFilePath
              |> Path.GetFileName
              |> archive.CreateEntry

            use sink = entry.Open()
            sink.Write(buffer, 0, size)
            ()

          [ 0..9 ]
          |> Seq.iter (fun i ->
            AltCoverCoreTests.VisitsAdd(
              "f6e3edb3-fb20-44b3-817d-f69d1a22fc2f",
              i,
              (int64 (i + 1))
            ))

          let nullObj: obj = null
          Instance.I.doExit.Invoke(nullObj, null)

          let head =
            "Coverage statistics flushing took "

          let tail = " seconds\n"

          let recorded =
            stdout.ToString().Replace("\r\n", "\n")

          let index1 =
            recorded.IndexOf(head, StringComparison.Ordinal)

          let index2 =
            recorded.IndexOf(tail, StringComparison.Ordinal)

          Assert.That(index1, Is.GreaterThanOrEqualTo 0, recorded)
          Assert.That(index2, Is.GreaterThan index1, recorded)

          use zip =
            ZipFile.Open(Instance.ReportFilePath + ".zip", ZipArchiveMode.Update)

          let entry =
            Instance.ReportFilePath
            |> Path.GetFileName
            |> zip.GetEntry

          use worker' = entry.Open()
          let after = XmlDocument()
          after.Load worker'

          Assert.That(
            after.SelectNodes("//seqpnt")
            |> Seq.cast<XmlElement>
            |> Seq.map (fun x -> x.GetAttribute("visitcount")),
            Is.EquivalentTo
              [ "11"
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

          complete <- true
        finally
          Instance.I.Trace <- save
          AltCoverCoreTests.maybeDeleteFile Instance.ReportFilePath
          AltCoverCoreTests.maybeDeleteFile (Instance.ReportFilePath + ".zip")
          AltCoverCoreTests.VisitsClear()
          Console.SetOut saved
          Directory.SetCurrentDirectory(here)
          AltCoverCoreTests.maybeIOException (fun () -> Directory.Delete(unique))
          Assert.That(complete, Is.True, "incomplete")))

    getMyMethodName "<="
#endif

  [<Test>]
  let ShouldCreateDummyAttribute () =
    let dummy =
      AltCover.Recorder.ExcludeFromCodeCoverageAttribute()

    Assert.That(dummy, Is.Not.Null)