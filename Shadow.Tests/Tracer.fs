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
open NUnit.Framework

type UpdateBinder () =
  inherit System.Runtime.Serialization.SerializationBinder()
  override self.BindToType (_:string, _:string) =
    typeof<(string*int)> // anything else is an error

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

  member self.ReadResults (stream:Stream) =
    let hits = List<(string*int)>()
    let formatter = System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()
    formatter.Binder <- UpdateBinder()
    let rec sink() = try
                          let hit = formatter.Deserialize(stream) :?> (string*int)
                          hit |> hits.Add
                          sink()
                       with
                       | :? System.Runtime.Serialization.SerializationException as x ->
                           ()
    sink()
    hits

#if NET4
  // passing F# types across the CLR divide doesn't work
#else
  [<Test>]
  member self.VisitShouldSignal() =
    let save = Instance.trace
    let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
    let unique = Path.Combine(where, Guid.NewGuid().ToString())
    let tag = unique + ".acv"
    let expected = [("name", 23); ("name", 23)]

    do
        use stream = File.Create tag
        ()
    try
      let mutable client = Tracer.Create tag
      try
        Instance.Visits.Clear()
        let entry = Dictionary<int, int  * (int64 option * int option) list>()
        entry.Add(23, (1, []))
        Instance.Visits.Add("name", entry)

        Instance.trace <- client.OnStart()
        Assert.That (Instance.trace.IsConnected(), "connection failed")
        Instance.VisitImpl "name" 23 (None,None)
      finally
        Instance.trace.Close()
        Instance.trace <- save

      use stream = new DeflateStream(File.OpenRead (unique + ".0.acv"), CompressionMode.Decompress)
      let results = self.ReadResults stream
      Assert.That (Instance.Visits, Is.Empty, "unexpected local write")
      Assert.That (results, Is.EquivalentTo expected, "unexpected result")

    finally
      Instance.Visits.Clear()

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
      let expected = [("name", client.GetHashCode()); (null, -1)]

      try
        Instance.Visits.Clear()
        Instance.trace <- client.OnStart()
        Assert.That(Instance.trace.Equals client, Is.False)
        Assert.That(Instance.trace.Equals expected, Is.False)

        Assert.That (Instance.trace.IsConnected(), "connection failed")
        let formatter = System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()
        formatter.Serialize(Instance.trace.Stream, expected |> Seq.head)
        Instance.FlushCounterImpl ProcessExit
      finally
        Instance.trace.Close()
        System.Threading.Thread.Sleep 100
        Instance.trace <- save

      use stream = new DeflateStream(File.OpenRead (root + ".0.acv"), CompressionMode.Decompress)
      let results = self.ReadResults stream
      Assert.That (Instance.Visits, Is.Empty, "unexpected local write")
      Assert.That (results, Is.EquivalentTo expected, "unexpected result")

    finally
      Instance.Visits.Clear()
#endif
#if NETCOREAPP2_0
  [<Test>]
  member self.CoreFindsThePlace() =
    Assert.That (AltCover.Recorder.Tracer.Core(),
                 Does.EndWith("FSharp.Core.dll"))
#endif

end