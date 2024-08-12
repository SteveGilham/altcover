#if !NET472
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

  let internal init (n, l) =
    let tmp = PointVisit.Create()
    tmp.Count <- n

    tmp.Tracks.AddRange l
    tmp

  let internal newBoth (time, call) = Both(Pair.Create(time, call))

  let VisitsClear () =
    Instance.I.Clear()
    Counter.branchVisits <- 0L
    Counter.totalVisits <- 0L

  let SamplesClear () =
    Instance.Strategy <- Sampling.Invalid
    Instance.I.samples <- Instance.I.MakeSamples()

  let private reset () =
    Instance.I.isRunner <- false
    VisitsClear()
    SamplesClear()

  let ModuleReset (m: string array) =
    Instance.Modules <- m
    reset ()

  let HardReset () =
    Instance.Modules <- [| System.String.Empty |]
    reset ()

  let VisitsSeq () =
    Instance.I.visits
    |> Seq.cast<KeyValuePair<string, Dictionary<int, PointVisit>>>

  let VisitImplMethod (moduleId, hitPointId, mId) =
    Instance.I.VisitImpl(moduleId, hitPointId, (Call mId))

  let internal prepareName name =
    if name |> Instance.I.visits.ContainsKey |> not then
      let entry = Dictionary<int, PointVisit>()
      Instance.I.visits.Add(name, entry)

  let VisitsAdd (name, line, number) =
    prepareName name
    let v = init (number, [])
    Instance.I.visits.[name].Add(line, v)

  let VisitsAddTrack (name, line, number) =
    prepareName name
    let v1 = init (number, [ Call 17; Call 42 ])
    Instance.I.visits.[name].Add(line, v1)

    let v2 =
      init (
        (number + 1L),
        [ Time(17L)
          Both(Pair.Create(42L, 23)) ]
      )

    Instance.I.visits.[name].Add(line + 1, v2)

  type Untable =
    | Name of string
    | Place of int
    | Token of Track

  let internal untable t =
    let r = List<Untable>()
    //let r = System.Collections.ArrayList()

    let n, p, (t': Track) = t

    match t' with
    | :? Table as d ->
      r.Add(Untable.Name n) |> ignore
      r.Add(Untable.Place p) |> ignore
      r.Add(Untable.Token d) |> ignore
    | _ -> ()

    r

  //=======================================

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
         | Tag.Call -> (Call <| formatter.ReadInt32()) :> Track
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
                   let pv = init (n, [])
                   t.[m].Add(p, pv)

                   // [<TailCall>]
                   let rec tracking () =
                     let track = formatter.ReadByte() |> int

                     match enum track with
                     | Tag.Time ->
                       pv.Tracks.Add(Time <| formatter.ReadInt64())
                       tracking ()
                     | Tag.Call ->
                       pv.Tracks.Add(Call <| formatter.ReadInt32())
                       tracking ()
                     | Tag.Both ->
                       pv.Tracks.Add(
                         newBoth ((formatter.ReadInt64()), (formatter.ReadInt32()))
                       )

                       tracking ()
                     //| Tag.Table -> Assert.True( false, "No nested tables!!")
                     | _ -> sequencePoint (pts - 1)

                   tracking ()
                 else
                   ``module`` ()

               sequencePoint points

           ``module`` ()
           Table t
         | _ -> Null())
        |> hits.Add

        sink ())

    sink ()
    hits

  [<Test>]
  let VisitShouldSignal () =
    let save = Instance.I.Trace

    let where =
      Assembly.GetExecutingAssembly().Location
      |> Path.GetDirectoryName

    let unique =
      Path.Combine(where, Guid.NewGuid().ToString())

    let tag = unique + ".acv"

    let expected =
      [ ("name", 23, Null() :> Track) ]

    do
      use stream = File.Create tag
      ()

    try
      let mutable client = Tracer.Create tag

      try
        HardReset()
        Instance.I.Trace <- client.OnStart()
        Assert.True(Instance.I.Trace.IsConnected, "connection failed")
        Instance.I.isRunner <- true
        Instance.Strategy <- Sampling.All
        Instance.I.VisitImpl("name", 23, Null())
      finally
        Instance.I.Trace.Close()
        Instance.I.Trace.Close()
        Instance.I.Trace.Close()
        Instance.Strategy <- Sampling.Invalid
        Instance.I.Trace <- save

      use stream = // fsharplint:disable-next-line  RedundantNewKeyword
        new DeflateStream(File.OpenRead(unique + ".0.acv"), CompressionMode.Decompress)

      let results =
        readResults stream |> Seq.toList

      VisitsSeq()
      |> Seq.iter (fun v ->
        Assert.That(v.Value, Is.Empty, sprintf "Unexpected local write %A" v))

      Assert.That(
        VisitsSeq() |> Seq.length,
        Is.EqualTo 3,
        sprintf "unexpected local write %A" <| VisitsSeq()
      )

      Assert.True((results = expected), sprintf "unexpected result %A" results)
    finally
      HardReset()

  [<Test>]
  let VisitShouldSignalTrack () =
    let save = Instance.I.Trace

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
      [ Call 17; Call 42 ] |> Seq.cast<Track>

    let expect24 =
      [ (Time 17L) :> Track
        (newBoth (42L, 23)) :> Track ]

    t.["name"].[23] <- init (1L, expect23)
    t.["name"].[24] <- init (2L, expect24)

    let expected =
      [ (String.Empty, 0, (Table t) :> Track)
        ("name", 23, Call 5) ]

    do
      use stream = File.Create tag
      ()

    try
      let mutable client = Tracer.Create tag

      try
        Instance.I.Trace <- client.OnStart()
        Assert.True(Instance.I.Trace.IsConnected, "connection failed")
        Instance.I.isRunner <- true

        HardReset()

        Instance.Strategy <- Sampling.All
        VisitsAddTrack("name", 23, 1L)
        VisitImplMethod("name", 23, 5)
      finally
        Instance.I.isRunner <- false
        Instance.I.Trace.Close()
        Instance.I.Trace <- save
        Instance.Strategy <- Sampling.Invalid

      use stream = // fsharplint:disable-next-line  RedundantNewKeyword
        new DeflateStream(File.OpenRead(unique + ".0.acv"), CompressionMode.Decompress)

      let results = readResults stream

      Assert.True(("no", 0, Null()) |> untable |> Seq.isEmpty)

      VisitsSeq()
      |> Seq.cast<KeyValuePair<string, Dictionary<int, PointVisit>>>
      |> Seq.iter (fun v ->
        Assert.That(v.Value, Is.Empty, sprintf "Unexpected local write %A" v))

      Assert.That(
        VisitsSeq() |> Seq.length,
        Is.EqualTo 3,
        sprintf "unexpected local write %A" <| VisitsSeq()
      )

      Assert.True(results.Count = 2)

      Assert.True(
        (results |> Seq.skip 1 |> Seq.head) = (expected |> Seq.skip 1 |> Seq.head),
        sprintf "unexpected result %A" results
      )

      let [ n'; p'; d' ] =
        results |> Seq.head |> untable |> Seq.toList

      let n =
        match n' with
        | Untable.Name x -> x

      let p =
        match p' with
        | Untable.Place x -> x

      let d =
        match d' with
        | Untable.Token x -> (x :?> Table).Value

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
      HardReset()

  [<Test>]
  let FlushShouldTidyUp () = // also throw a bone to OpenCover 615
    let save = Instance.I.Trace

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
        [ ("name", client.GetHashCode(), Null() :> Track) ]

      try
        HardReset()
        Instance.I.Trace <- client.OnStart()
        Assert.That(Instance.I.Trace.Equals client, Is.False)
        Assert.That(Instance.I.Trace.Equals expected, Is.False)
        Assert.True(Instance.I.Trace.IsConnected, "connection failed")

        let (a, b, c) = expected |> Seq.head
        Instance.I.Trace.Push(a, b, c)
        Instance.FlushFinish()
      finally
        Instance.I.Trace.Close()
        System.Threading.Thread.Sleep 100
        Instance.I.Trace <- save

      use stream = // fsharplint:disable-next-line  RedundantNewKeyword
        new DeflateStream(File.OpenRead(root + ".0.acv"), CompressionMode.Decompress)

      let results =
        stream |> readResults |> Seq.toList

      VisitsSeq()
      |> Seq.cast<KeyValuePair<string, Dictionary<int, PointVisit>>>
      |> Seq.iter (fun v ->
        Assert.That(v.Value, Is.Empty, sprintf "Unexpected local write %A" v))

      Assert.That(
        VisitsSeq() |> Seq.length,
        Is.EqualTo 3,
        sprintf "unexpected local write %A" <| VisitsSeq()
      )

      Assert.True((results = expected), sprintf "unexpected result %A" results)
    finally
      VisitsClear()