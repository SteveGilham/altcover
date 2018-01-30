namespace Tests.Shadow.Core

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Threading
open System.Threading.Tasks

open AltCover.Recorder
open NUnit.Framework

[<TestFixture>]
type AltCoverCoreTests() = class

  [<Test>]
  member self.ValidTokenWillConnect () =
    let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
    let unique = Path.Combine(where, Guid.NewGuid().ToString())
    do
        use stream = File.Create(unique)
        ()

    let client = Tracer.Create unique
    try
        client.OnStart()
        Assert.That (client.IsConnected(), Is.True)
    finally
      client.Close()

  member self.ReadResults (stream:Stream) =
    let hits = List<(string*int)>()
    let formatter = System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()
    let rec sink() = try
                          let hit = formatter.Deserialize(stream) :?> (string*int)
                          hit |> hits.Add
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
    let expected = [("name", 23)]

    do
        use stream = File.Create unique
        ()
    try
      let client = Tracer.Create unique
      try
        Instance.Visits.Clear()
        client.OnStart()
        Instance.trace <- client
        Assert.That (Instance.trace.IsConnected(), "connection failed")
        Instance.Visit "name" 23
      finally
        client.Close()
        Instance.trace <- save

      use stream = File.OpenRead unique
      let results = self.ReadResults stream
      Assert.That (Instance.Visits, Is.Empty, "unexpected local write")
      Assert.That (results, Is.EquivalentTo expected, "unexpected result")

    finally
      Instance.Visits.Clear()


  [<Test>]
  member self.FlushShouldTidyUp() =
    let save = Instance.trace
    let where = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
    let unique = Path.Combine(where, Guid.NewGuid().ToString())
    let expected = [("name", 23); (null, -1)]

    do
        use stream = File.Create unique
        ()

    try
      let client = Tracer.Create unique
      try
        Instance.Visits.Clear()
        client.OnStart()
        Instance.trace <- client
        Assert.That (Instance.trace.IsConnected(), "connection failed")
        let formatter = System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()
        formatter.Serialize(!Instance.trace.Stream, expected |> Seq.head)
        Instance.FlushCounter true ()
      finally
        client.Close()
        Instance.trace <- save

      use stream = File.OpenRead unique
      let results = self.ReadResults stream
      Assert.That (Instance.Visits, Is.Empty, "unexpected local write")
      Assert.That (results, Is.EquivalentTo expected, "unexpected result")

    finally
      Instance.Visits.Clear()

  [<Test>]
  member self.CoreFindsThePlace() =
    Assert.That (AltCover.Recorder.Tracer.Core(),
                 Does.EndWith("FSharp.Core.dll"))

end