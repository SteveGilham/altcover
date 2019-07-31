namespace Tests

open NUnit.Framework

[<TestFixture>]
type SimpleTest () =

    [<SetUp>]
    member this.Setup () =
        ()

    [<Test>]
    member this.Test1 () =
        Assert.Fail("This test should fail")