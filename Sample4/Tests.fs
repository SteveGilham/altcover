namespace Tests

open System
open System.Reflection
open Xunit

[<assembly:AssemblyVersionAttribute("1.0.0.0")>]
[<assembly:AssemblyFileVersionAttribute("1.0.0.0")>]
do ()

module M =
  type Thing =
    { Thing : string }
    member this.bytes() = System.Text.Encoding.UTF8.GetBytes(this.Thing)

  let makeThing s = { Thing = s }

  [<Fact>]
  let testMakeThing() =
    Assert.Equal("s", (makeThing "s").Thing)
    Assert.Equal(5, (makeThing "aeiou").bytes().Length)

module DU =
  type MyUnion =
    | Foo of int
    | Bar of string
    | Baz of double
    | Bop of DateTime

    member this.as_bar() =
      try
        match this with
        | Foo n -> Bar(string n)
        | Baz d -> Bar(string d)
        | Bop t -> Bar(string t)
        // New cases go in here
        | _ -> this
      with _ -> Bar "none"

    member this.MyBar = this.as_bar

  type MyClass() =
    member val Property = 0 with get, set

  let returnFoo v = Foo v
  let returnBar v = Bar v

  [<Fact>]
  let testMakeUnion() =
    Assert.Equal(returnFoo 10, Foo 10)
    Assert.Equal(returnBar "s", Bar "s")
    Assert.Equal(Bar "10", (Foo 10).as_bar())