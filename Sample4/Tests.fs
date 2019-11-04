namespace Tests

open System
open System.Reflection
open Xunit
open AltCode.Test.Common
open AltCode.Test.Xunit

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
    AltAssert.Equal { Expected = "s"; Actual = (makeThing "s").Thing}
    AltAssert.Equal { Expected = 5; Actual = (makeThing "aeiou").bytes().Length}

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
    AltAssert.Equal { Actual = returnFoo 10; Expected = Foo 10}
    AltAssert.Equal { Actual = returnBar "s"; Expected = Bar "s"}
    AltAssert.Equal { Expected = Bar "10"; Actual = (Foo 10).as_bar()}