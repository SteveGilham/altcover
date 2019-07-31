open System

open AltCover.Recorder

let run _ =
   let mutable step = 0
   {0..3}
   |> Seq.iter (fun i -> step <- step + 1
                         Instance.Visit "module1" i)

   {0..5}
   |> Seq.iter( fun i -> {0..5}
                         |> Seq.iter (fun j -> step <- step + 1
                                               Instance.Visit "module1" j))
   step

let runALot n =
    Seq.sumBy run ({0..n})

let runMany m n =
    {0..m}
    |> Seq.map (fun _ ->
        async {
            runALot n |> ignore
        })
    |> Async.Parallel
    |> Async.Ignore

[<EntryPoint>]
let main argv =
    runMany 100 10000
    |> Async.RunSynchronously

    0 // return an integer exit code