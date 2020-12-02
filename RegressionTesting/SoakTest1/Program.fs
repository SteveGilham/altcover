// Learn more about F# at http://fsharp.org

open System
open System.Threading

module ThreadLevel =
    let internal synchronize = Object()

    /// <summary>
    /// Gets or sets the current test method
    /// </summary>
    [<Sealed; AbstractClass>] // = static class
    type private CallTrack =
      // Option chosen for the default value
      [<ThreadStatic; DefaultValue>] // class needed for "[ThreadStatic] static val mutable"
      static val mutable private instance : Option<int list>

      static member private Update l =
        CallTrack.instance <- Some l

      // Per thread initialization of [ThreadStatic] => no race conditions here
      static member Instance =
        match CallTrack.instance with
        | None -> CallTrack.Update []
        | _ -> ()
        CallTrack.instance.Value

      static member Peek () =
        match CallTrack.Instance with
        | [] ->([], None)
        | h :: xs -> (xs, Some h)

      static member Push x =
        CallTrack.Update (x :: CallTrack.Instance)
      static member Pop () =
        let (stack, head) =  CallTrack.Peek()
        CallTrack.Update stack
        head

    let internal callerId() = CallTrack.Peek() |> snd
    let internal push x = CallTrack.Push x
    let internal pop() = CallTrack.Pop()

    let rec stack1 i =
      let here = Thread.CurrentThread.GetHashCode()
      try
        push i
        lock (synchronize) (fun _ -> printfn "push on %A state = %A" here CallTrack.Instance)
        Thread.Yield() |> ignore
        if i < 10
        then stack1 (i + 1)
      finally
        pop () |> ignore
        lock (synchronize) (fun _ -> printfn "pop on %A state = %A" here CallTrack.Instance)

module AsyncLevel =
    let internal synchronize = Object()

    /// <summary>
    /// Gets or sets the current test method
    /// </summary>
    // [<Sealed; AbstractClass>] = static class not required
    module CallTrack =
      // Option chosen for the default value
      // [<ThreadStatic; DefaultValue>] // class needed for "[ThreadStatic] static val mutable"
      let instance = AsyncLocal<Option<int list>>()

      let private Update l = // fsharplint:disable-line NonPublicValuesNames
        instance.Value <- Some l //.Value

      // no race conditions here
      let Instance () =
        match instance.Value with  //.Value
        | None -> Update []
        | _ -> ()
        instance.Value.Value //.Value

      let Peek () =
        match Instance() with
        | [] ->([], None)
        | h :: xs -> (xs, Some h)

      let Push x =
        Update (x :: Instance())
      let Pop () =
        let (stack, head) =  Peek()
        Update stack
        head

    let internal callerId() = CallTrack.Peek() |> snd
    let internal push x = CallTrack.Push x
    let internal pop() = CallTrack.Pop()

    let rec stack1 i =
      let here = Thread.CurrentThread.GetHashCode()
      try
        push i
        lock (synchronize) (fun _ -> printfn "push on %A state = %A" here <| CallTrack.Instance())
        Thread.Yield() |> ignore
        if i < 10
        then stack1 (i + 1)
      finally
        pop () |> ignore
        lock (synchronize) (fun _ -> printfn "pop on %A state = %A" here <| CallTrack.Instance())

[<EntryPoint>]
let main argv =
    [1; 3; 5; 7; 2; 4; 6; 0]
    |> List.map (fun i -> System.Action((fun () -> ThreadLevel.stack1 i)) )
    |> List.toArray
    |> System.Threading.Tasks.Parallel.Invoke

    //[1; 3; 5; 7; 2; 4; 6; 0]
    //|> List.map (fun i -> System.Action((fun () -> AsyncLevel.stack1 i)) )
    //|> List.toArray
    //|> System.Threading.Tasks.Parallel.Invoke

    // printfn "default list = %A" Unchecked.defaultof<int list> = null
    0 // return an integer exit code