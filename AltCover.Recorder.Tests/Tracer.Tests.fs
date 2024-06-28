﻿#if !NET472
#if NET20
namespace Tests.Recorder.Clr2
#else
namespace Tests.Recorder.Core
#endif
#else
namespace Tests.Recorder.Clr4
#endif

#nowarn "25" // partial pattern match

open System
open System.Collections.Generic
open System.IO
open System.IO.Compression
open System.Reflection
open AltCover.Recorder
open NUnit.Framework

//[<TestFixture>]
module AltCoverCoreTests =
  let maybeIOException f =
    try
      f ()
    with :? IOException ->
      ()

  let maybeDeleteFile f =
    if File.Exists f then
      File.Delete f

  let maybeReraise f g =
    try
      f ()
    with _ ->
      g ()
      reraise ()

  [<Test>]
  let ExcerciseItAll () =
    let where =
      Assembly.GetExecutingAssembly().Location
      |> Path.GetDirectoryName

    let unique =
      Path.Combine(Path.Combine(where, Guid.NewGuid().ToString()), "nonesuch.txt")

    maybeDeleteFile unique
    maybeIOException (fun () -> maybeReraise (fun () -> File.Delete unique) ignore)
    maybeIOException (fun () -> maybeReraise (fun () -> IOException() |> raise) ignore)

  [<Test>]
  let WillNotConnectSpontaneously () =
    let where =
      Assembly.GetExecutingAssembly().Location
      |> Path.GetDirectoryName

    let unique =
      Path.Combine(where, Guid.NewGuid().ToString())

    let mutable client = Tracer.Create unique
    let close = (fun () -> client.Close())

    maybeReraise
      (fun () ->
        client <- client.OnStart()
        Assert.True(client.IsConnected |> not)
        close ())
      close

  [<Test>]
  let ValidTokenWillConnect () =
    let where =
      Assembly.GetExecutingAssembly().Location
      |> Path.GetDirectoryName

    let unique =
      Path.Combine(where, Guid.NewGuid().ToString())

    do
      use stream = File.Create(unique)
      ()

    let mutable client = Tracer.Create unique

    try
      client <- client.OnStart()
      Assert.True(client.IsConnected)
    finally
      client.Close()

  let internal readResults (stream: Stream) =
    let hits = List<string * int * Track>()

    use formatter = // fsharplint:disable-next-line  RedundantNewKeyword
      new System.IO.BinaryReader(stream)

    // [<TailCall>]
    let rec sink () =
      maybeIOException (fun () ->
        let id = formatter.ReadString()
        let strike = formatter.ReadInt32()
        let tag = formatter.ReadByte() |> int

        (id,
         strike,
         match enum tag with
         //| Tag.Time -> Adapter.Time <| formatter.ReadInt64()
         | Tag.Call -> Adapter.asCall <| formatter.ReadInt32()
         //| Tag.Both -> Adapter.NewBoth((formatter.ReadInt64()), (formatter.ReadInt32()))
         | Tag.Table ->
           Assert.True((id = String.Empty))
           Assert.True((strike = 0))

           let t =
             Dictionary<string, Dictionary<int, PointVisit>>()

           // [<TailCall>]
           let rec ``module`` () =
             let m = formatter.ReadString()

             if String.IsNullOrEmpty m then
               ()
             else
               t.Add(m, Dictionary<int, PointVisit>())
               let points = formatter.ReadInt32()

               // [<TailCall>]
               let rec sequencePoint pts =
                 if pts > 0 then
                   let p = formatter.ReadInt32()
                   let n = formatter.ReadInt64()
                   let pv = Adapter.init (n, [])
                   t.[m].Add(p, pv)

                   // [<TailCall>]
                   let rec tracking () =
                     let track = formatter.ReadByte() |> int

                     match enum track with
                     | Tag.Time ->
                       pv.Tracks.Add(Adapter.time <| formatter.ReadInt64())
                       tracking ()
                     | Tag.Call ->
                       pv.Tracks.Add(Adapter.asCall <| formatter.ReadInt32())
                       tracking ()
                     | Tag.Both ->
                       pv.Tracks.Add(
                         Adapter.newBoth (
                           (formatter.ReadInt64()),
                           (formatter.ReadInt32())
                         )
                       )

                       tracking ()
                     //| Tag.Table -> Assert.True( false, "No nested tables!!")
                     | _ -> sequencePoint (pts - 1)

                   tracking ()
                 else
                   ``module`` ()

               sequencePoint points

           ``module`` ()
           Adapter.table t
         | _ -> Adapter.asNull ())
        |> hits.Add

        sink ())

    sink ()
    hits

  [<Test>]
  let VisitShouldSignal () =
    let save = Instance.I.trace

    let where =
      Assembly.GetExecutingAssembly().Location
      |> Path.GetDirectoryName

    let unique =
      Path.Combine(where, Guid.NewGuid().ToString())

    let tag = unique + ".acv"

    let expected =
      [ ("name", 23, Adapter.asNull ()) ]

    do
      use stream = File.Create tag
      ()

    try
      let mutable client = Tracer.Create tag

      try
        Adapter.HardReset()
        Instance.I.trace <- client.OnStart()
        Assert.True(Instance.I.trace.IsConnected, "connection failed")
        Instance.I.isRunner <- true
        Adapter.VisitImplNone("name", 23)
      finally
        Instance.I.trace.Close()
        Instance.I.trace.Close()
        Instance.I.trace.Close()
        Instance.I.trace <- save

      use stream = // fsharplint:disable-next-line  RedundantNewKeyword
        new DeflateStream(File.OpenRead(unique + ".0.acv"), CompressionMode.Decompress)

      let results =
        readResults stream |> Seq.toList

      Adapter.VisitsSeq()
      |> Seq.cast<KeyValuePair<string, Dictionary<int, PointVisit>>>
      |> Seq.iter (fun v ->
        Assert.That(v.Value, Is.Empty, sprintf "Unexpected local write %A" v))

      Assert.That(
        Adapter.VisitsSeq() |> Seq.length,
        Is.EqualTo 3,
        sprintf "unexpected local write %A"
        <| Adapter.VisitsSeq()
      )

      Assert.True((results = expected), sprintf "unexpected result %A" results)
    finally
      Adapter.HardReset()

  [<Test>]
  let VisitShouldSignalTrack () =
    let save = Instance.I.trace

    let where =
      Assembly.GetExecutingAssembly().Location
      |> Path.GetDirectoryName

    let unique =
      Path.Combine(where, Guid.NewGuid().ToString())

    let tag = unique + ".acv"

    let t =
      Dictionary<string, Dictionary<int, PointVisit>>()

    t.["name"] <- Dictionary<int, PointVisit>()

    let expect23 =
      [ Adapter.asCall 17; Adapter.asCall 42 ]

    let expect24 =
      [ Adapter.time 17L
        Adapter.newBoth (42L, 23) ]

    t.["name"].[23] <- Adapter.init (1L, expect23)
    t.["name"].[24] <- Adapter.init (2L, expect24)

    let expected =
      [ (String.Empty, 0, Adapter.table t)
        ("name", 23, Adapter.asCall 5) ]

    do
      use stream = File.Create tag
      ()

    try
      let mutable client = Tracer.Create tag

      try
        Instance.I.trace <- client.OnStart()
        Assert.True(Instance.I.trace.IsConnected, "connection failed")
        Instance.I.isRunner <- true

        Adapter.HardReset()
        Adapter.VisitsAddTrack("name", 23, 1L)
        Adapter.VisitImplMethod("name", 23, 5)
      finally
        Instance.I.isRunner <- false
        Instance.I.trace.Close()
        Instance.I.trace <- save

      use stream = // fsharplint:disable-next-line  RedundantNewKeyword
        new DeflateStream(File.OpenRead(unique + ".0.acv"), CompressionMode.Decompress)

      let results = readResults stream

      Assert.True(
        ("no", 0, Adapter.asNull ())
        |> Adapter.untable
        |> Seq.isEmpty
      )

      Adapter.VisitsSeq()
      |> Seq.cast<KeyValuePair<string, Dictionary<int, PointVisit>>>
      |> Seq.iter (fun v ->
        Assert.That(v.Value, Is.Empty, sprintf "Unexpected local write %A" v))

      Assert.That(
        Adapter.VisitsSeq() |> Seq.length,
        Is.EqualTo 3,
        sprintf "unexpected local write %A"
        <| Adapter.VisitsSeq()
      )

      Assert.True(results.Count = 2)

      Assert.True(
        (results |> Seq.skip 1 |> Seq.head) = (expected |> Seq.skip 1 |> Seq.head),
        sprintf "unexpected result %A" results
      )

      let [ n'; p'; d' ] =
        results
        |> Seq.head
        |> Adapter.untable
        |> Seq.toList

      let n = n' :?> String
      let p = p' :?> int

      let d =
        d' :?> Dictionary<string, Dictionary<int, PointVisit>>

      Assert.True(n |> Seq.isEmpty)
      Assert.True((p = 0))
      Assert.True((d.Count = 1))

      Assert.True(
        d.["name"]
        |> Seq.sortBy (fun kv -> kv.Key)
        |> Seq.map (fun kv -> kv.Key)
        |> Seq.toList =
          (t.["name"]
           |> Seq.sortBy (fun kv -> kv.Key)
           |> Seq.map (fun kv -> kv.Key)
           |> Seq.toList)
      )

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

      Assert.True((left = right))

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

      Assert.True((left2 = right2))
    finally
      Adapter.HardReset()

  [<Test>]
  let FlushShouldTidyUp () = // also throw a bone to OpenCover 615
    let save = Instance.I.trace

    let where =
      Assembly.GetExecutingAssembly().Location
      |> Path.GetDirectoryName

    let root =
      Path.Combine(where, Guid.NewGuid().ToString())

    let unique = root + ".acv"

    do
      use stream = File.Create unique
      ()

    try
      let client = Tracer.Create unique

      let expected =
        [ ("name", client.GetHashCode(), Adapter.asNull ()) ]

      try
        Adapter.HardReset()
        Instance.I.trace <- client.OnStart()
        Assert.That(Instance.I.trace.Equals client, Is.False)
        Assert.That(Instance.I.trace.Equals expected, Is.False)
        Assert.True(Instance.I.trace.IsConnected, "connection failed")

        let (a, b, c) = expected |> Seq.head
        Adapter.tracePush (a, b, c)
        Instance.FlushFinish()
      finally
        Instance.I.trace.Close()
        System.Threading.Thread.Sleep 100
        Instance.I.trace <- save

      use stream = // fsharplint:disable-next-line  RedundantNewKeyword
        new DeflateStream(File.OpenRead(root + ".0.acv"), CompressionMode.Decompress)

      let results =
        stream |> readResults |> Seq.toList

      Adapter.VisitsSeq()
      |> Seq.cast<KeyValuePair<string, Dictionary<int, PointVisit>>>
      |> Seq.iter (fun v ->
        Assert.That(v.Value, Is.Empty, sprintf "Unexpected local write %A" v))

      Assert.That(
        Adapter.VisitsSeq() |> Seq.length,
        Is.EqualTo 3,
        sprintf "unexpected local write %A"
        <| Adapter.VisitsSeq()
      )

      Assert.True((results = expected), sprintf "unexpected result %A" results)
    finally
      Adapter.VisitsClear()