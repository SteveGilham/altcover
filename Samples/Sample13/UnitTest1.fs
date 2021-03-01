namespace Tests

open System
open System.IO
open NUnit.Framework

[<TestFixture>]
type SimpleTest() =
  let mutable filepath = "Unset!"

  [<SetUp>]
  member this.Setup() =
    let here = System.Reflection.Assembly.GetExecutingAssembly().Location
    let heredir =  here |> Path.GetDirectoryName
    let herefile = here |> Path.GetFileName
    filepath <- Path.Combine((if (heredir |> Path.GetFileName).StartsWith("__Instrumented", StringComparison.Ordinal)
                              then heredir |> Path.GetDirectoryName
                              else heredir), herefile + ".txt")

    System.IO.File.WriteAllText(filepath, filepath)

  [<Test>]
  member this.Test1() =
    Assert.That(filepath |> File.Exists, Is.True, filepath + " should exist")
    Assert.Fail("This test should fail -- " + filepath)