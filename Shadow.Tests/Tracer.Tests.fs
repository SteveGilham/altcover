#if NETCOREAPP2_0
namespace Tests.Shadow.Core
#else
#if NET4
namespace Tests.Shadow.Clr4
#else
#if NET2
namespace Tests.Shadow.Clr2
#else
#if MONO
namespace Tests.Shadow.Mono
#else
namespace Tests.Shadow.Unknown
#endif
#endif
#endif
#endif

open System
open System.Collections.Generic
open System.IO
open System.IO.Compression
open System.Reflection

open AltCover.Recorder
open AltCover.Shadow
open NUnit.Framework

[<TestFixture>]
type AltCoverCoreTests() =
  class

    [<Test>]
    member self.WillNotConnectSpontaneously() =
      let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
      let unique = Path.Combine(where, Guid.NewGuid().ToString())
      let mutable client = Tracer.Create unique
      try
        client <- client.OnStart()
        Assert.That(client.IsConnected(), Is.False)
      with _ ->
        client.Close()
        reraise()

    [<Test>]
    member self.ValidTokenWillConnect() =
      let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
      let unique = Path.Combine(where, Guid.NewGuid().ToString())
      do use stream = File.Create(unique)
         ()
      let mutable client = Tracer.Create unique
      try
        client <- client.OnStart()
        Assert.That(client.IsConnected(), Is.True)
      finally
        client.Close()

    member internal self.ReadResults(stream : Stream) =
      let hits = List<string * int * Track>()
      use formatter = new System.IO.BinaryReader(stream)

      let rec sink() =
        try
          let id = formatter.ReadString()
          let strike = formatter.ReadInt32()
          let tag = formatter.ReadByte() |> int
          (id, strike,
           match enum tag with
           | Tag.Time -> Time <| formatter.ReadInt64()
           | Tag.Call -> Call <| formatter.ReadInt32()
#if NET4
           | Tag.Both -> Adapter.NewBoth (formatter.ReadInt64()) (formatter.ReadInt32())
#else
           | Tag.Both -> Both (formatter.ReadInt64(), formatter.ReadInt32())
#endif
           | Tag.Table -> Assert.That (id, Is.Empty)
                          Assert.That (strike, Is.EqualTo 0)
                          let t = Dictionary<string, Dictionary<int, PointVisit>>()
                          let rec ``module`` () =
                            let m = formatter.ReadString()
                            if String.IsNullOrEmpty m
                            then ()
                            else
                              t.Add(m, Dictionary<int, PointVisit>())
                              let rec sequencePoint () =
                                let p = formatter.ReadInt32()
                                if p <> 0 then
                                  let n = formatter.ReadInt32()
                                  let pv = PointVisit.Init n []
                                  t.[m].Add(p, pv)
                                  let rec tracking () =
                                    let track = formatter.ReadByte() |> int
                                    match enum track with
                                    | Tag.Time -> pv.Tracks.Add (Time <| formatter.ReadInt64())
                                                  tracking ()
                                    | Tag.Call -> pv.Tracks.Add (Call <| formatter.ReadInt32())
                                                  tracking ()
#if NET4
                                    | Tag.Both -> pv.Tracks.Add (Adapter.NewBoth (formatter.ReadInt64()) (formatter.ReadInt32()))
#else
                                    | Tag.Both -> pv.Tracks.Add (Both (formatter.ReadInt64(), formatter.ReadInt32()))
#endif
                                                  tracking ()
                                    | Tag.Table -> Assert.Fail ("No nested tables!!")
                                    | _ -> sequencePoint()
                                  tracking()
                                else ``module``()
                              sequencePoint()
                          ``module`` ()
                          Table t

           | _ -> Null)
          |> hits.Add
          sink()
        with :? System.IO.IOException -> ()
      sink()
      hits

    [<Test>]
    member self.VisitShouldSignal() =
      let save = Instance.trace
      let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
      let unique = Path.Combine(where, Guid.NewGuid().ToString())
      let tag = unique + ".acv"

      let expected =
        [ ("name", 23, Null) ]
      do use stream = File.Create tag
         ()
      try
        let mutable client = Tracer.Create tag
        try
          Adapter.VisitsClear()
          Adapter.VisitsAdd "name" 23 1
          Instance.trace <- client.OnStart()
          Assert.That(Instance.trace.IsConnected(), "connection failed")
          Adapter.VisitImplNone "name" 23
        finally
          Instance.trace.Close()
          Instance.trace.Close()
          Instance.trace.Close()
          Instance.trace <- save
        use stream =
          new DeflateStream(File.OpenRead(unique + ".0.acv"), CompressionMode.Decompress)
        let results = self.ReadResults stream
        Assert.That(Adapter.VisitsSeq(), Is.Empty, "unexpected local write")
        let h = Seq.head results
        let tail = results |> (Seq.skip 1)
        match h with
        | ("", 0, Table t) -> Assert.That (t.Count, Is.EqualTo 1)
                              Assert.That (t.["name"].Count, Is.EqualTo 1)
                              Assert.That (t.["name"].[23].Count, Is.EqualTo 1)
                              Assert.That (t.["name"].[23].Tracks, Is.Empty)
        | _ -> h |> (sprintf "%A") |> Assert.Fail
        Assert.That(tail, Is.EquivalentTo expected, "unexpected result")

      finally
        Adapter.VisitsClear()

    [<Test>]
    member self.VisitShouldSignalTrack() =
      let save = Instance.trace
      let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
      let unique = Path.Combine(where, Guid.NewGuid().ToString())
      let tag = unique + ".acv"

      let expected = [ ("name", 23, Call 5) ]
      let expect23 = [
                        Call 17
                        Call 42
                     ]
      let expect24 = [
                        Time 17L
#if NET4
                        Adapter.NewBoth 42L 23
#else
                        Both (42L, 23)
#endif
      ]

      do use stream = File.Create tag
         ()
      try
        let mutable client = Tracer.Create tag
        try
          Adapter.VisitsClear()
          Adapter.VisitsAddTrack "name" 23 1
          Instance.trace <- client.OnStart()
          Assert.That(Instance.trace.IsConnected(), "connection failed")
          Adapter.VisitImplMethod "name" 23 5
        finally
          Instance.trace.Close()
          Instance.trace <- save
        use stream =
          new DeflateStream(File.OpenRead(unique + ".0.acv"), CompressionMode.Decompress)
        let results = self.ReadResults stream
        Assert.That(Adapter.VisitsSeq(), Is.Empty, "unexpected local write")
        let h = Seq.head results
        let tail = results |> (Seq.skip 1)
        match h with
        | ("", 0, Table t) -> Assert.That (t.Count, Is.EqualTo 1)
                              Assert.That (t.["name"].Count, Is.EqualTo 2)
                              Assert.That (t.["name"].[23].Count, Is.EqualTo 1)
                              Assert.That (t.["name"].[23].Tracks, Is.EquivalentTo expect23)
                              Assert.That (t.["name"].[24].Count, Is.EqualTo 2)
                              Assert.That (t.["name"].[24].Tracks, Is.EquivalentTo expect24)
        | _ -> h |> (sprintf "%A") |> Assert.Fail
        Assert.That(tail, Is.EquivalentTo expected, "unexpected result")
      finally
        Adapter.VisitsClear()

    [<Test>]
    member self.FlushShouldTidyUp() = // also throw a bone to OpenCover 615
      let save = Instance.trace
      let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
      let root = Path.Combine(where, Guid.NewGuid().ToString())
      let unique = root + ".acv"
      do use stream = File.Create unique
         ()
      try
        let client = Tracer.Create unique
        let expected = [ ("name", client.GetHashCode(), Null) ]
        try
          Adapter.VisitsClear()
          Instance.trace <- client.OnStart()
          Assert.That(Instance.trace.Equals client, Is.False)
          Assert.That(Instance.trace.Equals expected, Is.False)
          Assert.That(Instance.trace.IsConnected(), "connection failed")
          let formatter = System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()
          let (a, b, c) = expected |> Seq.head
          Instance.trace.Push a b c
          Adapter.FlushAll()
        finally
          Instance.trace.Close()
          System.Threading.Thread.Sleep 100
          Instance.trace <- save
        use stream =
          new DeflateStream(File.OpenRead(root + ".0.acv"), CompressionMode.Decompress)
        let results = self.ReadResults stream
        Assert.That(Adapter.VisitsSeq(), Is.Empty, "unexpected local write")
        Assert.That(results, Is.EquivalentTo expected, "unexpected result")
      finally
        Adapter.VisitsClear()

#if NETCOREAPP2_0
    [<Test>]
    member self.CoreFindsThePlace() =
      Assert.That(AltCover.Recorder.Tracer.Core(), Does.EndWith("FSharp.Core.dll"))
#endif

  end