namespace Tests.Shadow.Core

open System
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks

open AltCover.Recorder
open NUnit.Framework

[<TestFixture>]
type AltCoverCoreTests() = class

  member private self.RunWithTimeout (f:unit -> unit) (t:int) =
    let task = Task.Run f
    if task.Wait(t) |> not then Assert.Fail("Task timeout")

  member self.ResilientPassesThroughTest () =
    let one = ref false
    let two = ref false
    Communications.ResilientAgainstDisposedObject (fun () -> one := true) (fun () -> two := true)
    Assert.That(!one)
    Assert.That(!two, Is.False)

  [<Test>]
  member self.ResilientPassesThrough () =
    self.RunWithTimeout self.ResilientPassesThroughTest 5000

  member self.ResilientHandlesExceptionTest () =
    let one = ref false
    let two = ref false
    Communications.ResilientAgainstDisposedObject (fun () ->
        ObjectDisposedException("fail") |> raise
        one := true) (fun () -> two := true)
    Assert.That(!one, Is.False)
    Assert.That(!two)

  [<Test>]
  member self.ResilientHandlesException () =
    self.RunWithTimeout self.ResilientHandlesExceptionTest 5000

  member self.PipeTimeoutShouldRaiseTest () =
    let token = Guid.NewGuid().ToString() + "PipeTimeoutShouldRaise"
    let client = Tracer.Create token
    try
      let os = Environment.OSVersion.ToString()
      if os.StartsWith("Microsoft Windows", StringComparison.Ordinal) then
        Assert.Throws<TimeoutException> (fun () -> 1 |> client.Connect) |> ignore
    finally
      client.Close()

  [<Test>]
  member self.PipeTimeoutShouldRaise () =
    self.RunWithTimeout self.PipeTimeoutShouldRaiseTest 5000

  member self.InitialConnectDefaultsUnconnectedTest() =
    let os = Environment.OSVersion.ToString()
    let token = "AltCover"
    use server = new System.IO.Pipes.NamedPipeServerStream(token)
    let client = Tracer.Create token
    try
      if os.StartsWith("Microsoft Windows", StringComparison.Ordinal) then
        client.OnStart 100
        Assert.That (client.Pipe.IsConnected, Is.False)
    finally
      client.Close()

  [<Test>]
  member self.InitialConnectDefaultsUnconnected () =
    self.RunWithTimeout self.InitialConnectDefaultsUnconnectedTest 5000

  member self.ValidTokenWillConnectTest() =
    let os = Environment.OSVersion.ToString()
    let token = "ValidToken"
    use server = new System.IO.Pipes.NamedPipeServerStream(token)
    let client = Tracer.Create token
    try
      if os.StartsWith("Microsoft Windows", StringComparison.Ordinal) then
        client.OnStart 100
        Assert.That (client.Pipe.IsConnected, Is.True)
    finally
      client.Close()

  [<Test>]
  member self.ValidTokenWillConnect () =
    self.RunWithTimeout self.ValidTokenWillConnectTest 5000

  member self.ValidTokenWillTimeOutTest() =
    let os = Environment.OSVersion.ToString()
    let token = "ValidToken"
    let client = Tracer.Create token
    try
      if os.StartsWith("Microsoft Windows", StringComparison.Ordinal) then
        client.OnStart 100
        Assert.That (client.Pipe.IsConnected, Is.False)
    finally
      client.Close()

  [<Test>]
  member self.ValidTokenWillTimeOut () =
    self.RunWithTimeout self.ValidTokenWillTimeOutTest 5000

  member self.PipeVisitShouldFailSafeTest() =
    let save = Instance.trace
    let token = Guid.NewGuid().ToString() + "PipeVisitShouldFailSafe"
    printfn "token = %s" token
    use server = new System.IO.Pipes.NamedPipeServerStream(token)
    printfn "Created NamedPipeServerStream"
    try
      let client = Tracer.Create token
      printfn "Created client"
      try
        let expected = ("name", 23)
        let formatter = System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()
        use signal = new AutoResetEvent false
        async {
            try
              client.Connect 2000
              printfn "Connected."
            with
            | :? TimeoutException ->
                printfn "timed out"
            signal.Set() |> ignore
            } |> Async.Start
        server.WaitForConnection()
        signal.WaitOne() |> ignore
        printfn "after connection wait"
        Instance.trace <- client
        Assert.That (Instance.trace.IsConnected(), "connection failed")
        printfn "about to act"
        server.WriteByte(0uy)
        Assert.That(client.Activated.WaitOne(1000), "never got activated")
        Assert.That (Instance.trace.IsActivated(), "activation failed")
        client.Close()
        printfn "about to read"
        Assert.Throws<System.Runtime.Serialization.SerializationException>(fun () -> formatter.Deserialize(server) |> ignore) |> ignore
        printfn "after all work"
      finally
        printfn "finally 1"
        Instance.trace.Close()
        Instance.trace <- save
    finally
      printfn "finally 2"
      Instance.Visits.Clear()
    printfn "all done"

  [<Test>]
  member self.PipeVisitShouldFailSafe () =
    self.RunWithTimeout self.PipeVisitShouldFailSafeTest 5000

  member self.PipeVisitShouldSignalTest() =
    let save = Instance.trace
    let token = Guid.NewGuid().ToString() + "PipeVisitShouldSignal"
    printfn "token = %s" token
    use server = new System.IO.Pipes.NamedPipeServerStream(token)
    printfn "Created NamedPipeServerStream"
    try
      let client = Tracer.Create token
      printfn "Created client"
      try
        Instance.Visits.Clear()
        let expected = ("name", 23)
        let formatter = System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()
        use signal = new AutoResetEvent false
        async {
            try
              client.Connect 2000
              printfn "Connected."
            with
            | :? TimeoutException ->
                printfn "timed out"
            signal.Set() |> ignore
            } |> Async.Start
        server.WaitForConnection()
        signal.WaitOne() |> ignore
        printfn "after connection wait"
        Instance.trace <- client
        Assert.That (Instance.trace.IsConnected(), "connection failed")
        printfn "about to act"
        server.WriteByte(0uy)
        Assert.That(client.Activated.WaitOne(1000), "never got activated")
        Assert.That (Instance.trace.IsActivated(), "activation failed")
        async { Instance.Visit "name" 23 } |> Async.Start
        printfn "about to read"
        let result = formatter.Deserialize(server) :?> (string*int)
        Assert.That (Instance.Visits, Is.Empty, "unexpected local write")
        Assert.That (result, Is.EqualTo expected, "unexpected result")
        printfn "after all work"
      finally
        printfn "finally 1"
        Instance.trace.Close()
        Instance.trace <- save
    finally
      printfn "finally 2"
      Instance.Visits.Clear()
    printfn "all done"

  [<Test>]
  member self.PipeVisitShouldSignal () =
    self.RunWithTimeout self.PipeVisitShouldSignalTest 5000

  member self.PipeVisitShouldFailFastTest() =
    let save = Instance.trace
    let token = Guid.NewGuid().ToString() + "PipeVisitShouldFailFast"
    printfn "token = %s" token
    use server = new System.IO.Pipes.NamedPipeServerStream(token)
    printfn "Created NamedPipeServerStream"
    try
      let client = Tracer.Create token
      printfn "Created client"
      try
        let expected = ("name", 23)
        let formatter = System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()
        use signal = new AutoResetEvent false
        client.Close()
        let blew = ref false
        async {
            try
              client.Connect 500
              printfn "Connected."
            with
            | :? TimeoutException ->
                printfn "timed out"
            | :? ObjectDisposedException ->
                blew := true
                printfn "blew up"
            signal.Set() |> ignore
            } |> Async.Start
        signal.WaitOne() |> ignore
        printfn "after connection wait"
        Instance.trace <- client
        Assert.That(!blew, "Should have blown")
        Assert.That (Instance.trace.IsConnected(), Is.False, "connected")
        Assert.That (Instance.trace.IsActivated(), Is.False, "activated")
        printfn "after all work"
      finally
        printfn "finally 1"
        Instance.trace.Close()
        Instance.trace <- save
    finally
      printfn "finally 2"
      Instance.Visits.Clear()
    printfn "all done"

  [<Test>]
  member self.PipeVisitShouldFailFast () =
    self.RunWithTimeout self.PipeVisitShouldFailFastTest 5000

  [<Test>]
  member self.PipeFlushShouldTidyUpTest() =
    let save = Instance.trace
    let token = Guid.NewGuid().ToString() + "PipeFlushShouldTidyUp"
    printfn "pipe token = %s" token
    use server = new System.IO.Pipes.NamedPipeServerStream(token)
    printfn "Created server"
    try
      let client = Tracer.Create token
      printfn "Created client"
      try
        let expected = ("name", 23)
        let formatter = System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()
        Instance.trace <- client
        printfn "Ready to connect"
        use signal = new AutoResetEvent false
        async {
            try
              client.Connect 2000
              printfn "Connected."
            with
            | :? TimeoutException ->
                printfn "timed out"
            signal.Set() |> ignore
            } |> Async.Start
        server.WaitForConnection()
        signal.WaitOne() |> ignore
        printfn "After connection wait"
        Assert.That (Instance.trace.IsConnected(), "connection failed")
        printfn "About to act"
        server.WriteByte(0uy)
        Assert.That(client.Activated.WaitOne(1000), "never got activated")
        Assert.That (Instance.trace.IsActivated(), "activation failed")
        async { formatter.Serialize(Instance.trace.Pipe, expected)
                Instance.FlushCounter true () } |> Async.Start
        printfn "About to read"
        let result = formatter.Deserialize(server) :?> (string*int)
        let result' = formatter.Deserialize(server) :?> (string*int)
        printfn "About to assert"
        Assert.That (Instance.Visits, Is.Empty, "unexpected local write")
        Assert.That (result, Is.EqualTo expected, "unexpected result")
        Assert.That (result' |> fst |> String.IsNullOrEmpty, Is.True, "unexpected end-of-message")
        printfn "done"
      finally
        printfn "first finally"
        Instance.trace.Close()
        Instance.trace <- save
    finally
      printfn "second finally"
      Instance.Visits.Clear()
    printfn "all done"

  [<Test>]
  member self.PipeFlushShouldTidyUp () =
    self.RunWithTimeout self.PipeFlushShouldTidyUpTest 5000

  [<Test>]
  member self.CoreFindsThePlace() =
    Assert.That (AltCover.Recorder.Tracer.Core(),
                 Does.EndWith("FSharp.Core.dll"))

end