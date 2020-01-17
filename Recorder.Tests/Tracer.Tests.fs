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
open System.IO
open System.IO.Compression
open System.Reflection
open AltCover.Recorder
open NUnit.Framework
open Swensen.Unquote

//[<TestFixture>]
module AltCoverCoreTests =

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

  [<Test>]
  let WillNotConnectSpontaneously() =
    let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
    let unique = Path.Combine(where, Guid.NewGuid().ToString())
    let mutable client = Tracer.Create unique
    try
      client <- client.OnStart()
      test <@ client.IsConnected() |> not @>
    with _ ->
      client.Close()
      reraise()

  [<Test>]
  let ValidTokenWillConnect() =
    let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
    let unique = Path.Combine(where, Guid.NewGuid().ToString())
    do use stream = File.Create(unique)
       ()
    let mutable client = Tracer.Create unique
    try
      client <- client.OnStart()
      test <@ client.IsConnected() @>
    finally
      client.Close()

  let internal ReadResults(stream : Stream) =
    let hits = List<string * int * Track>()
    use formatter = new System.IO.BinaryReader(stream)

    let rec sink() =
      try
        let id = formatter.ReadString()
        let strike = formatter.ReadInt32()
        let tag = formatter.ReadByte() |> int
        (id, strike,
         match enum tag with
         | Tag.Time -> Adapter.Time <| formatter.ReadInt64()
         | Tag.Call -> Adapter.Call <| formatter.ReadInt32()
         | Tag.Both -> Adapter.NewBoth((formatter.ReadInt64()), (formatter.ReadInt32()))
         | Tag.Table ->
             test <@ id = String.Empty @>
             test <@ strike = 0 @>
             let t = Dictionary<string, Dictionary<int, PointVisit>>()

             let rec ``module``() =
               let m = formatter.ReadString()
               if String.IsNullOrEmpty m then
                 ()
               else
                 t.Add(m, Dictionary<int, PointVisit>())
                 let points = formatter.ReadInt32()

                 let rec sequencePoint pts =
                   if pts > 0 then
                     let p = formatter.ReadInt32()
                     let n = formatter.ReadInt64()
                     let pv = PointVisit.Init(n, [])
                     t.[m].Add(p, pv)
                     let rec tracking() =
                       let track = formatter.ReadByte() |> int
                       match enum track with
                       | Tag.Time ->
                           pv.Tracks.Add(Adapter.Time <| formatter.ReadInt64())
                           tracking()
                       | Tag.Call ->
                           pv.Tracks.Add(Adapter.Call <| formatter.ReadInt32())
                           tracking()
                       | Tag.Both ->
                           pv.Tracks.Add
                             (Adapter.NewBoth
                               ((formatter.ReadInt64()), (formatter.ReadInt32())))
                           tracking()
                       | Tag.Table -> test' <@ false @> "No nested tables!!"
                       | _ -> sequencePoint (pts - 1)
                     tracking()
                   else
                     ``module``()
                 sequencePoint points
             ``module``()
             Adapter.Table t
         | _ -> Adapter.Null())
        |> hits.Add
        sink()
      with :? System.IO.IOException -> ()
    sink()
    hits

  [<Test>]
  let VisitShouldSignal() =
    let save = Instance.trace
    let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
    let unique = Path.Combine(where, Guid.NewGuid().ToString())
    let tag = unique + ".acv"

    let expected = [ ("name", 23, Adapter.Null()) ]
    do use stream = File.Create tag
       ()
    try
      let mutable client = Tracer.Create tag
      try
        Adapter.VisitsClear()
        Instance.trace <- client.OnStart()
        test' <@ Instance.trace.IsConnected() @> "connection failed"
        Instance.IsRunner <- true
        Adapter.VisitImplNone("name", 23)
      finally
        Instance.trace.Close()
        Instance.trace.Close()
        Instance.trace.Close()
        Instance.trace <- save
      use stream =
        new DeflateStream(File.OpenRead(unique + ".0.acv"), CompressionMode.Decompress)
      let results = ReadResults stream |> Seq.toList
      test' <@ Adapter.VisitsSeq() |> Seq.isEmpty @> "unexpected local write"
      test' <@ results = expected @> "unexpected result"
    finally
      Adapter.Reset()

  [<Test>]
  let VisitShouldSignalTrack() =
    let save = Instance.trace
    let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
    let unique = Path.Combine(where, Guid.NewGuid().ToString())
    let tag = unique + ".acv"
    let t = Dictionary<string, Dictionary<int, PointVisit>>()
    t.["name"] <- Dictionary<int, PointVisit>()
    let expect23 =
      [ Adapter.Call 17
        Adapter.Call 42 ]

    let expect24 =
      [ Adapter.Time 17L
        Adapter.NewBoth(42L, 23) ]

    t.["name"].[23] <- PointVisit.Init(1L, expect23)
    t.["name"].[24] <- PointVisit.Init(2L, expect24)

    let expected =
      [ (String.Empty, 0, Adapter.Table t)
        ("name", 23, Adapter.Call 5) ]
    do use stream = File.Create tag
       ()
    try
      let mutable client = Tracer.Create tag
      try
        Instance.trace <- client.OnStart()
        test' <@ Instance.trace.IsConnected() @> "connection failed"
        Instance.IsRunner <- true

        Adapter.VisitsClear()
        Adapter.VisitsAddTrack("name", 23, 1L)
        Adapter.VisitImplMethod("name", 23, 5)
      finally
        Instance.IsRunner <- false
        Instance.trace.Close()
        Instance.trace <- save
      use stream =
        new DeflateStream(File.OpenRead(unique + ".0.acv"), CompressionMode.Decompress)
      let results = ReadResults stream
      test
        <@ ("no", 0, Adapter.Null())
           |> Adapter.untable
           |> Seq.isEmpty @>
      test' <@ Adapter.VisitsSeq() |> Seq.isEmpty @> "unexpected local write"
      test <@ results.Count = 2 @>
      test'
        <@ (results
            |> Seq.skip 1
            |> Seq.head) =
             (expected
              |> Seq.skip 1
              |> Seq.head) @> "unexpected result"
      match results
            |> Seq.head
            |> Adapter.untable
            |> Seq.toList with
      | [ n'; p'; d' ] ->
          let n = n' :?> String
          let p = p' :?> int
          let d = d' :?> Dictionary<string, Dictionary<int, PointVisit>>
          test <@ n |> Seq.isEmpty @>
          test <@ p = 0 @>
          test <@ d.Count = 1 @>
          test
            <@ d.["name"]
               |> Seq.sortBy (fun kv -> kv.Key)
               |> Seq.map (fun kv -> kv.Key)
               |> Seq.toList =
                 (t.["name"]
                  |> Seq.sortBy (fun kv -> kv.Key)
                  |> Seq.map (fun kv -> kv.Key)
                  |> Seq.toList) @>
          let left =
            d.["name"]
            |> Seq.sortBy (fun kv -> kv.Key)
            |> Seq.map (fun kv -> kv.Value.Count)
            |> Seq.toList

          let right =
            t.["name"]
            |> Seq.sortBy (fun kv -> kv.Key)
            |> Seq.map (fun kv -> kv.Value.Count)
            |> Seq.toList

          test <@ left = right @>
          let left2 =
            d.["name"]
            |> Seq.sortBy (fun kv -> kv.Key)
            |> Seq.map (fun kv -> kv.Value.Tracks |> Seq.toList)
            |> Seq.toList

          let right2 =
            t.["name"]
            |> Seq.sortBy (fun kv -> kv.Key)
            |> Seq.map (fun kv -> kv.Value.Tracks |> Seq.toList)
            |> Seq.toList

          test <@ left2 = right2 @>
      | _ -> test <@ false @>
    finally
      Adapter.VisitsClear()

  [<Test>]
  let FlushShouldTidyUp() = // also throw a bone to OpenCover 615
    let save = Instance.trace
    let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
    let root = Path.Combine(where, Guid.NewGuid().ToString())
    let unique = root + ".acv"
    do use stream = File.Create unique
       ()
    try
      let client = Tracer.Create unique
      let expected = [ ("name", client.GetHashCode(), Adapter.Null()) ]
      try
        Adapter.VisitsClear()
        Instance.trace <- client.OnStart()
        Assert.That(Instance.trace.Equals client, Is.False)
        Assert.That(Instance.trace.Equals expected, Is.False)
        Assert.That(Instance.trace.IsConnected(), "connection failed")
        let formatter = System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()
        let (a, b, c) = expected |> Seq.head
        Instance.trace.Push(a, b, c)
        Adapter.FlushAll()
      finally
        Instance.trace.Close()
        System.Threading.Thread.Sleep 100
        Instance.trace <- save
      use stream =
        new DeflateStream(File.OpenRead(root + ".0.acv"), CompressionMode.Decompress)

      let results =
        stream
        |> ReadResults
        |> Seq.toList
      test' <@ Adapter.VisitsSeq() |> Seq.isEmpty @> "unexpected local write"
      test' <@ results = expected @> "unexpected result"
    finally
      Adapter.VisitsClear()