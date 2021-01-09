namespace Tests

open System
open System.IO
open NUnit.Framework

[<TestFixture>]
type SimpleTest() =

  [<SetUp>]
  member this.Setup() =
    let here = System.Reflection.Assembly.GetExecutingAssembly().Location
    let heredir =  here |> Path.GetDirectoryName
    let wheredir = if (heredir |> Path.GetFileName).StartsWith("__Instrumented", StringComparison.Ordinal)
                   then heredir |> Path.GetDirectoryName
                   else heredir

    let herefile = here |> Path.GetFileName
    use file = Path.Combine(wheredir, herefile + ".txt") |> System.IO.File.Create
    ()

  [<Test>]
  member this.Test1() = Assert.Fail("This test should fail")