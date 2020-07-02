namespace Tests

open NUnit.Framework

[<TestFixture>]
type SimpleTest() =

  [<SetUp>]
  member this.Setup() = 
    use file = System.IO.File.Create (System.Reflection.Assembly.GetExecutingAssembly().Location + ".txt")
    ()

  [<Test>]
  member this.Test1() = Assert.Fail("This test should fail")