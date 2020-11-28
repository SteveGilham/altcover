// Learn more about F# at http://fsharp.org

open System
open System.Threading

module ThreadLevel =
    let internal synchronize = Object()

    /// <summary>
    /// Gets or sets the current test method
    /// </summary>
    type CallStack =
      // Option chosen for the default value
      [<ThreadStatic; DefaultValue>] // class needed for "[ThreadStatic] static val mutable"
      static val mutable private instance : Option<int list>

      // Per thread initialization of [ThreadStatic] => no race conditions here
      static member Instance =
        match CallStack.instance with
        | None -> CallStack.update []
        | _ -> ()
        CallStack.instance.Value

      static member private update l =
        CallStack.instance <- Some l

      static member peek () =
        match CallStack.Instance with
        | [] ->([], None)
        | h :: xs -> (xs, Some h)

      static member Push x =
        CallStack.update (x :: CallStack.Instance)
      static member Pop () =
        let (stack, head) =  CallStack.peek()
        CallStack.update stack
        head

    let internal callerId() = CallStack.peek() |> snd
    let internal push x = CallStack.Push x
    let internal pop() = CallStack.Pop()

    let rec stack1 i =
      let here = Thread.CurrentThread.GetHashCode()
      try
        push i
        lock (synchronize) (fun _ -> printfn "push on %A state = %A" here CallStack.Instance)
        Thread.Yield() |> ignore
        if i < 10
        then stack1 (i + 1)
      finally
        pop () |> ignore
        lock (synchronize) (fun _ -> printfn "pop on %A state = %A" here CallStack.Instance)

module AsyncLevel =
    let internal synchronize = Object()

    /// <summary>
    /// Gets or sets the current test method
    /// </summary>
    module CallStack =
      // Option chosen for the default value
      let instance = AsyncLocal<Option<int list>>()

      let private update l =
        instance.Value <- Some l

      let Instance () =
        match instance.Value with
        | None -> instance.Value <- Some([])
        | _ -> ()
        instance.Value.Value

      let peek ()  =
        match Instance() with
        | [] ->([], None)
        | h :: xs -> (xs, Some h)

      let Push x =
        update (x :: Instance())
      let Pop () =
        let (stack, head) = peek()
        update stack
        head

    let internal callerId() = CallStack.peek() |> snd
    let internal push x = CallStack.Push x
    let internal pop() = CallStack.Pop()

    let rec stack1 i =
      let here = Thread.CurrentThread.GetHashCode()
      try
        push i
        lock (synchronize) (fun _ -> printfn "push on %A state = %A" here <| CallStack.Instance())
        Thread.Yield() |> ignore
        if i < 10
        then stack1 (i + 1)
      finally
        pop () |> ignore
        lock (synchronize) (fun _ -> printfn "pop on %A state = %A" here <| CallStack.Instance())

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