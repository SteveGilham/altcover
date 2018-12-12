namespace Tests2

open System2.DU
open System.Reflection
open Xunit

[<assembly:AssemblyVersionAttribute("1.0.0.0")>]
[<assembly:AssemblyFileVersionAttribute("1.0.0.0")>]
do ()

module DU =
  [<Fact>]
  let testMakeUnion() =
    Assert.Equal(returnFoo 10, Foo 10)
    Assert.Equal(returnBar "s", Bar "s")
    Assert.Equal(Bar "10", (Foo 10).as_bar())