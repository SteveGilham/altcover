namespace Sample6

open System
open System.IO
open System.Net

module Module =
  let F1 l =
    let aux i = i + 1

    let FI li =
      let rec FII lii acc =
        match lii with
        | [] -> acc
        | x :: xs -> FII xs (aux acc)
      FII li 0
    l |> List.map (fun i -> (string i).Length)

  let F2 l =
    let fetchUrlAsync url =
      async {
        let req = WebRequest.Create(Uri(url))
        use! resp = req.AsyncGetResponse() // new keyword "use!"
        use stream = resp.GetResponseStream()
        use reader = new StreamReader(stream)
        let html = reader.ReadToEnd()
        printfn "finished downloading %s" url
      }
    l
    |> List.map fetchUrlAsync // make a list of async tasks
    |> Async.Parallel // set up the tasks to run in parallel
    |> Async.RunSynchronously