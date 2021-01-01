namespace Tests

open System
open NUnit.Framework

[<TestFixture>]
type SimpleTest() =

  [<SetUp>]
  member this.Setup() = 
    let here = System.Reflection.Assembly.GetExecutingAssembly().Location.Replace("__Instrumented", String.Empty)
    use file = System.IO.File.Create (here + ".txt")
    ()

  [<Test>]
  member this.Test1() = Assert.Fail("This test should fail")