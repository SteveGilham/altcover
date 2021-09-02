namespace Tests

open NUnit.Framework
open FsUnitTyped

type Size(width : int, height : int) =
  struct
    member __.Width = width
    member __.Height = height
  end

type Thing(size : Size) =
  member val Height = size.Height
  member val Width = size.Width
  member __.Rectangle(x, y) = System.Drawing.Rectangle(x, y, x, y)

module Tested =
  let add6To n = n + 6

open Tested

module Problematic =
  [<Test>]
  let ``Using FsUnit``() = add6To 3 |> shouldEqual 9

  [<Test>]
  let ``Using FsUnit bis``() =
    try
      add6To 3 |> shouldEqual 9
    finally
      System.Console.WriteLine("Finally")

  let add6To n = n + 6

  let inline shouldBeOdd x =
    if x % 2 = 0 then Assert.Fail "Not odd enough"

  [<Test>]
  let ``add6To3 should be odd``() = add6To 3 |> shouldBeOdd

  [<Test>]
  let ``add6To3 should be odd bis``() =
    try
      add6To 3 |> shouldBeOdd
    finally
      System.Console.WriteLine("Finally")

  [<Test>]
  let ``Thing Rectangle``() =
    let g = Thing(Size(11, 21))
    for row = 0 to g.Height do
      for col = 0 to g.Width do
        Assert.AreEqual
          (System.Drawing.Rectangle(col, row, col, row), g.Rectangle(col, row))

  [<Test>]
  let ``Thing Rectangle bis``() =
    try
      let g = Thing(Size(11, 21))
      for row = 0 to g.Height do
        for col = 0 to g.Width do
          Assert.AreEqual
            (System.Drawing.Rectangle(col, row, col, row), g.Rectangle(col, row))
    finally
      System.Console.WriteLine("Finally")

module Program =
  [<EntryPoint>]
  let main _ = 0      