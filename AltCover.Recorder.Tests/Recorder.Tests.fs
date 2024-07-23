#if !NET472
namespace Tests.Recorder.Core
#else
namespace Tests.Recorder.Clr4
#endif

#nowarn "3559"

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
  //    printfn "%s %s" tag

  let resource =
    Assembly
      .GetExecutingAssembly()
      .GetManifestResourceNames()
    |> Seq.find (fun n -> n.EndsWith("SimpleCoverage.xml", StringComparison.Ordinal))

  let private updateReport a b =
    Adapter.updateReport (a, ReportFormat.NCover, b, b)
    |> ignore

  let private pointVisitInit a b = Adapter.init (a, b)

  let resource2 =
    Assembly
      .GetExecutingAssembly()
      .GetManifestResourceNames()
    |> Seq.find (fun n ->
      n.EndsWith("Sample1WithModifiedOpenCover.xml", StringComparison.Ordinal))

  let resource3 =
    Assembly
      .GetExecutingAssembly()
      .GetManifestResourceNames()
    |> Seq.find (fun n ->
      n.EndsWith("Sample2NCoverage.xml", StringComparison.Ordinal))

  [<Test>]
  let ShouldCoverTrivalClass () =
    let mark = InstrumentationAttribute() // Constructor has all the instrumented code
    Assert.That(mark.Assembly, Is.EqualTo "AltCover.Recorder.g!")
    Assert.That(mark.Configuration, Is.EqualTo "Uninstrumented!!")
    mark.Assembly <- String.Empty
    mark.Configuration <- String.Empty
    Assert.That(String.IsNullOrEmpty mark.Assembly)
    Assert.That(String.IsNullOrEmpty mark.Configuration)

  [<Test>]
  let ShouldFailXmlDataForNativeJson () =
    Assert.Throws<NotSupportedException>(fun () ->
      ReportFormat.NativeJson
      |> Counter.I.xmlByFormat
      |> ignore)
    |> ignore

  [<Test>]
  let ShouldBeAbleToGetTheDefaultReportFileName () =
    Assert.That(Instance.ReportFile = "Coverage.Default.xml")

  [<Test>]
  let DefaultAccessorsBehaveAsExpected () =
    let v1 = DateTime.UtcNow.Ticks
    let probe = Instance.I.clock ()
    let v2 = DateTime.UtcNow.Ticks
    Assert.That(Instance.I.granularity () = 0L)
    Assert.That(probe >= v1)
    Assert.That(probe <= v2)

  [<Test>]
  let ShouldBeLinkingTheCorrectCopyOfThisCode () =
    getMyMethodName "=>"

    let tracer =
      Adapter.makeNullTrace String.Empty

    // whitelist test not recorder.g

    let n =
      tracer.GetType().Assembly.GetName().Name
    Assert.That(n, Is.EqualTo "AltCover.Recorder")

    getMyMethodName "<="

  [<Test>]
  let OnlyNewIdPairShouldBeSampled () =
    getMyMethodName "=>"

    lock Adapter.Lock (fun () ->
      try
        Adapter.ModuleReset [| "module"; "newmodule" |]

        Assert.That(Adapter.addSample ("module", 23, Null), "Test 1")
        Assert.That(Adapter.addSample ("module", 24, Null), "Test 2")
        Assert.That(Adapter.addSample ("newmodule", 23, Null), "Test 3")
        Assert.That(Adapter.addSample ("module", 23, Null) |> not, "Test 4")
        Assert.That(Adapter.addSampleUnconditional ("module", 23, Null), "Test 5")
        Assert.That(Adapter.addSample ("module", 23, Call 1), "Test 6")
        Assert.That(Adapter.addSample ("module", 23, Time 0L), "Test 7")

        Assert.That(
          Adapter.addSample ("module", 24, Both { Call = 1; Time = 0L }),
          "Test 8"
        )

        Assert.That(
          Adapter.addSample ("module", 25, Both { Call = 1; Time = 0L }),
          "Test 9"
        )

        Assert.That(Adapter.addSample ("module", 25, Call 1) |> not, "Test 10")
        Assert.That(Adapter.addSample ("module", 25, Call 1) |> not, "Test 11")
        Assert.That(Adapter.addSample ("module", 25, Null) |> not, "Test 12")

        // out of band example
        Assert.That(Adapter.addSample ("nonesuch", 25, Null) |> not, "Test 12a")

        Assert.Throws<InvalidDataException>(fun () ->
          Adapter.addSample ("module", 23, Table null)
          |> ignore)
        |> ignore
      finally
        Adapter.HardReset())

    getMyMethodName "<="

  [<Test>]
  let RealIdShouldIncrementCount () =
    getMyMethodName "=>"

    lock Adapter.Lock (fun () ->
      let save = Instance.I.trace

      try
        let key = " "
        Adapter.ModuleReset [| key |]
        Instance.I.trace <- Adapter.makeNullTrace null

        Instance.I.recording <- false
        Instance.Visit "key" 17
        Instance.I.recording <- true
        Instance.CoverageFormat <- ReportFormat.NCover
        Instance.Visit key -23

        Instance.CoverageFormat <-
          ReportFormat.OpenCover
          ||| ReportFormat.WithTracking

        Instance.Visit key -23

        let vs = Adapter.VisitsSeq()
        Assert.That(vs |> Seq.length = 3, sprintf "Adapter.VisitsSeq() = %A" vs)

        let vesk = Adapter.VisitsEntrySeq key

        Assert.That(
          vesk |> Seq.length = 1,
          sprintf "Adapter.VisitsEntrySeq %A = %A" key vesk
        )

        Assert.That(Adapter.VisitCount(key, -23) = 2L)
        Assert.That(Counter.totalVisits, Is.EqualTo 1L)
        Assert.That(Counter.branchVisits, Is.EqualTo 1L)
      finally
        Instance.CoverageFormat <- ReportFormat.NCover
        Instance.I.recording <- true
        Adapter.HardReset()
        Instance.I.trace <- save)

    getMyMethodName "<="

  [<Test>]
  let JunkUspidGivesNegativeIndex () =
    let key = " "

    let index =
      Counter.I.findIndexFromUspid 0 key

    Assert.That(index < 0)

  [<Test>]
  let PayloadGeneratedIsAsExpected () =
    try
      Instance.I.isRunner <- false

      Instance.CoverageFormat <-
        ReportFormat.OpenCover
        ||| ReportFormat.WithTracking

      Assert.That(Instance.I.callerId () |> Option.isNone)
      Assert.That(Adapter.payloadSelector false = Adapter.asNull ())
      Assert.That(Adapter.payloadSelector true = Adapter.asNull ())
      Instance.Push 4321
      Assert.That(Adapter.payloadSelector false = Adapter.asNull ())
      Assert.That(Adapter.payloadSelector true = (Adapter.asCall 4321))

      try
        Instance.Push 6789
        // 0x1234123412341234 == 1311693406324658740
        let result =
          Adapter.payloadSelection (1311693406324658740L, 1000L, true)

        let expected =
          Adapter.newBoth (1311693406324658000L, 6789)

        Assert.That((result = expected))
      finally
        Instance.Pop()

      Assert.That((Adapter.payloadSelector true = (Adapter.asCall 4321)))
    finally
      Instance.Pop()
      Instance.CoverageFormat <- ReportFormat.NCover

    let result2 =
      Adapter.payloadSelection (1311693406324658740L, 1000L, true)

    let expected2 =
      Adapter.time 1311693406324658000L

    Assert.That((result2 = expected2))
    let v1 = DateTime.UtcNow.Ticks

    let probed =
      Adapter.payloadControl (1000L, true)

    let v2 = DateTime.UtcNow.Ticks
    Assert.That(Adapter.asNull () |> Adapter.untime |> Seq.isEmpty)

    let [ probe ] =
      Adapter.untime probed |> Seq.toList

    Assert.That(probe % 1000L = 0L)
    Assert.That(probe <= v2)
    Assert.That(probe >= (1000L * (v1 / 1000L)))
    Assert.That(Instance.I.callerId () |> Option.isNone)
    Instance.Pop()
    Assert.That(Instance.I.callerId () |> Option.isNone)

  [<Test>]
  let PayloadWithEntryExitGeneratedIsAsExpected () =
    Adapter.ModuleReset [||]

    try
      Instance.I.isRunner <- true

      Instance.CoverageFormat <-
        ReportFormat.OpenCover
        ||| ReportFormat.WithTracking

      Adapter.VisitsClear()

      Assert.That(Instance.I.callerId () |> Option.isNone)
      Assert.That(Adapter.payloadSelector false = Adapter.asNull ())
      Assert.That(Adapter.payloadSelector true = Adapter.asNull ())
      Instance.Push 4321
      Assert.That(Adapter.payloadSelector false = Adapter.asNull ())
      Assert.That(Adapter.payloadSelector true = (Adapter.asCall 4321))

      try
        Instance.Push 6789
        // 0x1234123412341234 == 1311693406324658740
        let result =
          Adapter.payloadSelection (1311693406324658740L, 1000L, true)

        let expected =
          Adapter.newBoth (1311693406324658000L, 6789)

        Assert.That((result = expected))
      finally
        Instance.Pop()

      Assert.That((Adapter.payloadSelector true = (Adapter.asCall 4321)))
    finally
      Instance.Pop()
      Instance.I.isRunner <- false
      Instance.CoverageFormat <- ReportFormat.NCover

    let result2 =
      Adapter.payloadSelection (1311693406324658740L, 1000L, true)

    let expected2 =
      Adapter.time 1311693406324658000L

    Assert.That((result2 = expected2))
    let v1 = DateTime.UtcNow.Ticks

    let probed =
      Adapter.payloadControl (1000L, true)

    let v2 = DateTime.UtcNow.Ticks
    Assert.That(Adapter.asNull () |> Adapter.untime |> Seq.isEmpty)

    let [ probe ] =
      Adapter.untime probed |> Seq.toList

    Assert.That(probe % 1000L = 0L)
    Assert.That(probe <= v2)
    Assert.That(probe >= (1000L * (v1 / 1000L)))
    Assert.That(Instance.I.callerId () |> Option.isNone)
    Instance.Pop()
    Assert.That(Instance.I.callerId () |> Option.isNone)

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

    Adapter.HardReset()

  [<Test>]
  let RealIdShouldIncrementCountSynchronously () =
    getMyMethodName "=>"

    lock Instance.I.visits (fun () ->
      let save = Instance.I.trace
      let key = " "
      Adapter.ModuleReset [| key |]

      try
        Instance.I.trace <- Adapter.makeNullTrace null

        Instance.I.visitSelection (Adapter.asNull ()) key 23

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
        Adapter.HardReset()
        Instance.I.trace <- save)

    getMyMethodName "<="

  let strip before (all: HashSet<String>) =
    before
    |> Seq.iter (fun x ->
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
    let path =
      Instance.ReportFilePath |> Path.GetFullPath

    let where = path |> Path.GetDirectoryName

    let before =
      Directory.GetFiles(where, "*.exn")

    Instance.I.logException "a" "b" "c" "ex"

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
      [ "ModuleId = \"a\""
        "hitPointId = \"b\""
        "context = \"c\""
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

    Adapter.invokeCurriedIssue71Wrapper<NullReferenceException> unique

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
      [ "ModuleId = \"b\""
        "hitPointId = \"c\""
        "context = \"d\""
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

    Adapter.invokeIssue71Wrapper<KeyNotFoundException> (unique, pair)
    Assert.That(pair |> Seq.head, Is.True)
    Assert.That(pair |> Seq.last, Is.True)

  [<Test>]
  let Issue71WrapperHandlesNullReferenceException () =
    let pair = [| false; false |]

    let unique =
      System.Guid.NewGuid().ToString()

    Adapter.invokeIssue71Wrapper<NullReferenceException> (unique, pair)
    Assert.That(pair |> Seq.head, Is.True)
    Assert.That(pair |> Seq.last, Is.True)

  [<Test>]
  let Issue71WrapperHandlesArgumentNullException () =
    let pair = [| false; false |]

    let unique =
      System.Guid.NewGuid().ToString()

    Adapter.invokeIssue71Wrapper<ArgumentNullException> (unique, pair)
    Assert.That(pair |> Seq.head, Is.True)
    Assert.That(pair |> Seq.last, Is.True)

  [<Test>]
  let Issue71WrapperDoesntHandleOtherException () =
    let pair = [| false; false |]

    let unique =
      System.Guid.NewGuid().ToString()

    let exn =
      Assert.Throws<InvalidOperationException>(fun () ->
        Adapter.invokeIssue71Wrapper<InvalidOperationException> (unique, pair))

    Assert.That(pair |> Seq.head, Is.False)
    Assert.That(pair |> Seq.last, Is.False)
    Assert.That(exn.Message, Is.EqualTo unique)

#if !NET472
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

        Instance.I.visitImpl key 23 (Adapter.asNull ())

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
          [ "ModuleId = \" \""
            "hitPointId = 23"
            "context = Null"
            "exception = System.NullReferenceException: Object reference not set to an instance of an object." ]
        |> Seq.iter (fun (a, b) -> Assert.That((a = b)))

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
        Adapter.ModuleReset [| key; "key" |]
        Instance.I.visitImpl key 23 (Adapter.asNull ())
        Instance.I.visitImpl "key" 42 (Adapter.asNull ())

        Assert.That(
          Instance.I.visits.Keys,
          Is.EquivalentTo [ key; "key"; Track.Entry; Track.Exit ]
        )
      finally
        Adapter.HardReset())

    getMyMethodName "<="

  [<Test>]
  let DistinctLineShouldBeDistinct () =
    getMyMethodName "=>"

    lock Instance.I.visits (fun () ->
      try
        let key = " "
        Adapter.ModuleReset [| key |]
        Instance.I.visitImpl key 23 (Adapter.asNull ())
        Instance.I.visitImpl key 42 (Adapter.asNull ())

        Assert.That(
          Instance.I.visits.Keys,
          Is.EquivalentTo [ key; Track.Entry; Track.Exit ]
        )

        Assert.That(Instance.I.visits.[key].Count, Is.EqualTo 2)
      finally
        Adapter.HardReset())

    getMyMethodName "<="

  [<Test>]
  let RepeatVisitsShouldIncrementCount () =
    getMyMethodName "=>"

    lock Instance.I.visits (fun () ->
      let key = " "
      Adapter.ModuleReset [| key |]

      try
        Instance.I.visitImpl key 23 (Adapter.asNull ())
        Instance.I.visitImpl key 23 (Adapter.asNull ())
        Assert.That(Instance.I.visits.[key].[23].Count, Is.EqualTo 2)
        Assert.That(Instance.I.visits.[key].[23].Tracks, Is.Empty)
      finally
        Adapter.HardReset())

    getMyMethodName "<="

  [<Test>]
  let RepeatVisitsShouldIncrementTotal () =
    getMyMethodName "=>"

    lock Instance.I.visits (fun () ->
      let key = " "
      Adapter.ModuleReset [| key |]

      try
        let payload =
          Adapter.time DateTime.UtcNow.Ticks

        Instance.I.visitImpl key 23 (Adapter.asNull ())
        Instance.I.visitImpl key 23 payload
        Assert.That(Instance.I.visits.[key].[23].Count, Is.EqualTo 1)
        Assert.That(Instance.I.visits.[key].[23].Tracks, Is.EquivalentTo [ payload ])
      finally
        Adapter.HardReset())

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

      let join (x: string array) = String.Join(" ", x)

      Assert.That(
        after.SelectNodes("//seqpnt")
        |> Seq.cast<XmlElement>
        |> Seq.map (fun x -> x.GetAttribute("visitcount"))
        |> Seq.toArray
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

      Adapter.updateReport (item, ReportFormat.OpenCover, worker, worker)
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

  [<Test>]
  let PauseLeavesExpectedTraces () =
    getMyMethodName "=>"

    lock Adapter.Lock (fun () ->
      trywithrelease (fun () ->
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
              new FileStream(Instance.ReportFilePath, FileMode.CreateNew)

            worker.Write(buffer, 0, size)
            ()

          [ 0..9 ]
          |> Seq.iter (fun i ->
            Adapter.VisitsAdd(
              "f6e3edb3-fb20-44b3-817d-f69d1a22fc2f",
              i,
              (int64 (i + 1))
            ))

          Adapter.DoPause().Invoke(null, null)

          Adapter.VisitsSeq()
          |> Seq.cast<KeyValuePair<string, Dictionary<int, PointVisit>>>
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
          Instance.I.trace <- save
          AltCoverCoreTests.maybeDeleteFile Instance.ReportFilePath
          Adapter.VisitsClear()
          Instance.I.isRunner <- false
          Console.SetOut saved
          Directory.SetCurrentDirectory(here)
          AltCoverCoreTests.maybeIOException (fun () -> Directory.Delete(unique))))

    getMyMethodName "<="

  [<Test>]
  let ResumeLeavesExpectedTraces () =
    getMyMethodName "=>"

    lock Adapter.Lock (fun () ->
      trywithrelease (fun () ->
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
          let key =
            "f6e3edb3-fb20-44b3-817d-f69d1a22fc2f"

          Adapter.ModuleReset [| key |]
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
              new FileStream(Instance.ReportFilePath, FileMode.CreateNew)

            worker.Write(buffer, 0, size)
            ()

          [ 0..9 ]
          |> Seq.iter (fun i -> Adapter.VisitsAdd(key, i, (int64 (i + 1))))

          Adapter.DoResume().Invoke(null, null)

          Adapter.VisitsSeq()
          |> Seq.cast<KeyValuePair<string, Dictionary<int, PointVisit>>>
          |> Seq.iter (fun v ->
            Assert.That(v.Value, Is.Empty, sprintf "Visits should be cleared %A" v))

          Assert.That(
            Object.ReferenceEquals(Instance.I.trace, save),
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
          Adapter.HardReset()
          Instance.I.trace <- save
          AltCoverCoreTests.maybeDeleteFile Instance.ReportFilePath
          Adapter.VisitsClear()
          Console.SetOut saved
          Directory.SetCurrentDirectory(here)
          File.Delete tag
          AltCoverCoreTests.maybeIOException (fun () -> Directory.Delete(unique))))

    getMyMethodName "<="

  [<Test>]
  let FlushLeavesExpectedTraces () =
    getMyMethodName "=>"

    lock Adapter.Lock (fun () ->
      Instance.I.isRunner <- false

      trywithrelease (fun () ->
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
              new FileStream(Instance.ReportFilePath, FileMode.CreateNew)

            worker.Write(buffer, 0, size)
            ()

          [ 0..9 ]
          |> Seq.iter (fun i ->
            Adapter.VisitsAdd(
              "f6e3edb3-fb20-44b3-817d-f69d1a22fc2f",
              i,
              (int64 (i + 1))
            ))

          Adapter.DoExit().Invoke(null, null)

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
          Instance.I.trace <- save
          AltCoverCoreTests.maybeDeleteFile Instance.ReportFilePath
          Adapter.VisitsClear()
          Console.SetOut saved
          Directory.SetCurrentDirectory(here)
          AltCoverCoreTests.maybeIOException (fun () -> Directory.Delete(unique))))

    getMyMethodName "<="

  [<Test>]
  let SupervisedFlushLeavesExpectedTraces () =
    getMyMethodName "=>"

    lock Adapter.Lock (fun () ->
      trywithrelease (fun () ->
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
              new FileStream(Instance.ReportFilePath, FileMode.CreateNew)

            worker.Write(buffer, 0, size)
            ()

          [ 0..9 ]
          |> Seq.iter (fun i ->
            Adapter.VisitsAdd(
              "f6e3edb3-fb20-44b3-817d-f69d1a22fc2f",
              i,
              (int64 (i + 1))
            ))

          Adapter.DoUnload().Invoke(null, null)

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
          Instance.I.trace <- save
          Instance.supervision <- false
          AltCoverCoreTests.maybeDeleteFile Instance.ReportFilePath
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

      [ 0..9 ]
      |> Seq.iter (fun i -> payload.[i] <- pointVisitInit (int64 (i + 1)) [])

      visits.["f6e3edb3-fb20-44b3-817d-f69d1a22fc2f"] <- payload

      Adapter.doFlush (visits, ReportFormat.NCover, reportFile, outputFile)
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

      Adapter.doFlush (visits, ReportFormat.NCover, reportFile, outputFile)
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

      [ 0..9 ]
      |> Seq.iter (fun i -> payload.[i] <- pointVisitInit (int64 (i + 1)) [])

      visits.["f6e3edb3-fb20-44b3-817d-f69d1a22fc2f"] <- payload

      Adapter.doFlush (
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

      [ 0..9 ]
      |> Seq.iter (fun i -> payload.[i] <- pointVisitInit (int64 (i + 1)) [])

      visits.["f6e3edb3-fb20-44b3-817d-f69d1a22fc2f"] <- payload

      Adapter.doFlush (
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

      [ 0..9 ]
      |> Seq.iter (fun i -> payload.[i] <- pointVisitInit (int64 (i + 1)) [])

      visits.["f6e3edb3-fb20-44b3-817d-f69d1a22fc2f"] <- payload

      Adapter.doFlush (
        visits,
        ReportFormat.NCover ||| ReportFormat.Zipped,
        reportFile,
        null
      )
      |> ignore

      Assert.That(reportFile |> File.Exists |> not)
      let zipInfo = FileInfo(zipFile)
      Assert.That(zipInfo.Length, Is.EqualTo 0)
    finally
      AltCoverCoreTests.maybeDeleteFile zipFile
      Console.SetOut saved
      Directory.SetCurrentDirectory(here)
      AltCoverCoreTests.maybeIOException (fun () -> Directory.Delete(unique))

  [<Test>]
  let ZipFlushLeavesExpectedTraces () =
    getMyMethodName "=>"

    lock Adapter.Lock (fun () ->
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
            Adapter.VisitsAdd(
              "f6e3edb3-fb20-44b3-817d-f69d1a22fc2f",
              i,
              (int64 (i + 1))
            ))

          Adapter.DoExit().Invoke(null, null)

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
        finally
          Instance.I.trace <- save
          AltCoverCoreTests.maybeDeleteFile Instance.ReportFilePath
          AltCoverCoreTests.maybeDeleteFile (Instance.ReportFilePath + ".zip")
          Adapter.VisitsClear()
          Console.SetOut saved
          Directory.SetCurrentDirectory(here)
          AltCoverCoreTests.maybeIOException (fun () -> Directory.Delete(unique))))

    getMyMethodName "<="

  [<Test>]
  let ShouldCreateDummyAttribute () =
    let dummy =
      AltCover.Recorder.ExcludeFromCodeCoverageAttribute()

    Assert.That(dummy, Is.Not.Null)