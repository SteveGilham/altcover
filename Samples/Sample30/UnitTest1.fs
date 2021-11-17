namespace Sample30

open System.Threading.Tasks
open Xunit
open Swensen.Unquote

module Tests =

  [<Fact>]
  let Test1 () = test <@ true @>

  let AddSynch (x, y) = x + y

  let AddTask (x, y) =
    task {
      do! Task.Delay(100)
      do! Task.Yield()
      do! Task.Delay(100)
      do! Task.Yield()
      do! Task.Delay(100)
      do! Task.Yield()
      do! Task.Delay(100)
      do! Task.Yield()
      return AddSynch(x, y)
    }

  [<Fact>]
  let AddTaskReturnsTheSumOfXAndY () =
    task {
      let! result = AddTask(1, 1)
      test <@ AddSynch(1, 1) = result @>
    }

  let AddTaskReturnsTheSumOfXAndYUnwait () =
    try
      let t = task {
                let! result = AddTask(1, 1)
                test <@ AddSynch(1, 1) = result @>
              }
      t
    finally
      printfn "Done"

  let AddTaskReturnsTheSumOfXAndYAlter () =
    try
      let t = task {
                let! result = AddTask(1, 1)
                test <@ AddSynch(1, 1) = result @>
              }
      t.Wait(65535) |> ignore
      t
    finally
      printfn "Done"

    //public IEnumerable<int> Yielder()
    //{
    //  yield return 1;
    //  yield return 2;
    //  yield return 3;
    //}