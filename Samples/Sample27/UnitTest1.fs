namespace Sample27

open Xunit
open Swensen.Unquote

module Tests =

  [<Fact>]
  let Test1() =
    test <@ true @>

  let AddSynch(x, y) = x + y

  let AddAsync(x, y) = async {
      do! Async.Sleep(100)
      do! Async.Sleep(100)
      do! Async.Sleep(100)
      do! Async.Sleep(100)
      return AddSynch(x, y)
  }

  [<Fact>]
  let AddAsyncReturnsTheSumOfXAndY() = async {
    let! result = AddAsync(1, 1)
    test <@ AddSynch(1, 1) = result @>
  }

    //public IEnumerable<int> Yielder()
    //{
    //  yield return 1;
    //  yield return 2;
    //  yield return 3;
    //}