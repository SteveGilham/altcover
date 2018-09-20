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
           | AltCover.Recorder.Tag.Time -> Time <| formatter.ReadInt64()
           | AltCover.Recorder.Tag.Call -> Call <| formatter.ReadInt32()
#if NET4
           | AltCover.Recorder.Tag.Both ->
             Adapter.NewBoth (formatter.ReadInt64()) (formatter.ReadInt32())
#else
                                       | AltCover.Recorder.Tag.Both -> Both (formatter.ReadInt64(), formatter.ReadInt32())
#endif
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
        [ ("name", 23, Null)
          ("name", 23, Null) ]
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
          Instance.trace <- save
        use stream =
          new DeflateStream(File.OpenRead(unique + ".0.acv"), CompressionMode.Decompress)
        let results = self.ReadResults stream
        Assert.That(Adapter.VisitsSeq(), Is.Empty, "unexpected local write")
        Assert.That(results, Is.EquivalentTo expected, "unexpected result")
      finally
        Adapter.VisitsClear()

    [<Test>]
    member self.VisitShouldSignalTrack() =
      let save = Instance.trace
      let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
      let unique = Path.Combine(where, Guid.NewGuid().ToString())
      let tag = unique + ".acv"

      let expected =
        [ ("name", 23, Null)
          ("name", 23, Call 17)
          ("name", 23, Call 42)
          ("name", 24, Null)
          ("name", 24, Null)
          ("name", 24, Time 17L)
#if NET4
          ("name", 24, Adapter.NewBoth 42L 23)
#else
          ("name", 24, Both (42L, 23))
#endif

          ("name", 23, Call 5) ]
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
        Assert.That(results, Is.EquivalentTo expected, "unexpected result")
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
          Instance.FlushAll()
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