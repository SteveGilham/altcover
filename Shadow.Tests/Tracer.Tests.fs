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

type UpdateBinder () =
  inherit System.Runtime.Serialization.SerializationBinder()
  override self.BindToType (_:string, n:string) =
    if n.StartsWith("System.Tuple`2") then typeof<(string*int)> else
    if n.StartsWith("System.Tuple`3") then typeof<(string*int*Track)> else
    if n = "AltCover.Recorder.Track+Call" then (Track.Call 0).GetType() else
    raise (System.Runtime.Serialization.SerializationException n)

[<TestFixture>]
type AltCoverCoreTests() = class

  [<Test>]
  member self.WillNotConnectSpontaneously () =
    let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
    let unique = Path.Combine(where, Guid.NewGuid().ToString())
    let mutable client = Tracer.Create unique
    try
        client <- client.OnStart()
        Assert.That (client.IsConnected(), Is.False)
    with
    | _ -> client.Close()
           reraise()

  [<Test>]
  member self.ValidTokenWillConnect () =
    let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
    let unique = Path.Combine(where, Guid.NewGuid().ToString())
    do
        use stream = File.Create(unique)
        ()

    let mutable client = Tracer.Create unique
    try
        client <- client.OnStart()
        Assert.That (client.IsConnected(), Is.True)
    finally
      client.Close()

  member internal self.ReadResults (stream:Stream) =
    let hits = List<(string*int*Track)>()
    let formatter = System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()
    formatter.Binder <- UpdateBinder()
    let rec sink() = try
                          let raw = formatter.Deserialize(stream)
                          match raw with
                          | :? (string*int) as pair -> (fst pair, snd pair, Null)
                          | :? (string * int * Track) as x -> x
                          | _ -> raise (System.Runtime.Serialization.SerializationException (raw.GetType().FullName))
                          |> hits.Add
                          sink()
                       with
                       | :? System.Runtime.Serialization.SerializationException as x ->
                           ()
    sink()
    hits

  [<Test>]
  member self.VisitShouldSignal() =
    let save = Instance.trace
    let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
    let unique = Path.Combine(where, Guid.NewGuid().ToString())
    let tag = unique + ".acv"
    let expected = [("name", 23, Null); ("name", 23, Null)]

    do
        use stream = File.Create tag
        ()
    try
      let mutable client = Tracer.Create tag
      try
        Adapter.VisitsClear()
        Adapter.VisitsAdd "name" 23 1

        Instance.trace <- client.OnStart()
        Assert.That (Instance.trace.IsConnected(), "connection failed")
        Adapter.VisitImplNone "name" 23
      finally
        Instance.trace.Close()
        Instance.trace <- save

      use stream = new DeflateStream(File.OpenRead (unique + ".0.acv"), CompressionMode.Decompress)
      let results = self.ReadResults stream
      Assert.That (Adapter.VisitsSeq(), Is.Empty, "unexpected local write")
      Assert.That (results, Is.EquivalentTo expected, "unexpected result")

    finally
      Adapter.VisitsClear()

  [<Test>]
  member self.VisitShouldSignalTrack() =
    let save = Instance.trace
    let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
    let unique = Path.Combine(where, Guid.NewGuid().ToString())
    let tag = unique + ".acv"
    let expected = [
                     ("name", 23, Null)
                     ("name", 23, Call 17)
                     ("name", 23, Call 42)
                     ("name", 23, Call 5)
    ]

    do
        use stream = File.Create tag
        ()
    try
      let mutable client = Tracer.Create tag
      try
        Adapter.VisitsClear()
        Adapter.VisitsAddTrack "name" 23 1

        Instance.trace <- client.OnStart()
        Assert.That (Instance.trace.IsConnected(), "connection failed")
        Adapter.VisitImplMethod "name" 23 5
      finally
        Instance.trace.Close()
        Instance.trace <- save

      use stream = new DeflateStream(File.OpenRead (unique + ".0.acv"), CompressionMode.Decompress)
      let results = self.ReadResults stream
      Assert.That (Adapter.VisitsSeq(), Is.Empty, "unexpected local write")
      Assert.That (results, Is.EquivalentTo expected, "unexpected result")

    finally
      Adapter.VisitsClear()

  [<Test>]
  member self.FlushShouldTidyUp() = // also throw a bone to OpenCover 615
    let save = Instance.trace
    let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
    let root = Path.Combine(where, Guid.NewGuid().ToString())
    let unique = root + ".acv"

    do
        use stream = File.Create unique
        ()

    try
      let client = Tracer.Create unique
      let expected = [("name", client.GetHashCode(), Null); (null, -1, Null)]

      try
        Adapter.VisitsClear()
        Instance.trace <- client.OnStart()
        Assert.That(Instance.trace.Equals client, Is.False)
        Assert.That(Instance.trace.Equals expected, Is.False)

        Assert.That (Instance.trace.IsConnected(), "connection failed")
        let formatter = System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()
        let (a, b, _) = expected |> Seq.head
        formatter.Serialize(Instance.trace.Stream, (a,b))
        Instance.FlushAll ()
      finally
        Instance.trace.Close()
        System.Threading.Thread.Sleep 100
        Instance.trace <- save

      use stream = new DeflateStream(File.OpenRead (root + ".0.acv"), CompressionMode.Decompress)
      let results = self.ReadResults stream
      Assert.That (Adapter.VisitsSeq(), Is.Empty, "unexpected local write")
      Assert.That (results, Is.EquivalentTo expected, "unexpected result")

    finally
      Adapter.VisitsClear()

#if NETCOREAPP2_0
  [<Test>]
  member self.CoreFindsThePlace() =
    Assert.That (AltCover.Recorder.Tracer.Core(),
                 Does.EndWith("FSharp.Core.dll"))
#endif

end