let Example (x : int option) =
    x
    |> Option.iter (fun i -> Seq.initInfinite id
                             |> Seq.take i
                             |> Seq.iter (printfn "%A"))

[<EntryPoint>]
let main argv = 
    printfn "CLR Version %A" <| System.Environment.Version.ToString()
    Example None
    printfn "Done"
    0
