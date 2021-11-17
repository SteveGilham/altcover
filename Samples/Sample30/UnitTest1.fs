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

  let AddTaskReturnsTheSumOfXAndYAlter () =
    try
      let t = task {
                let! result = AddTask(1, 1)
                test <@ AddSynch(1, 1) = result @>
              }
      t.Wait()
      t
    finally
      printfn "Done"

    //public IEnumerable<int> Yielder()
    //{
    //  yield return 1;
    //  yield return 2;
    //  yield return 3;
    //}