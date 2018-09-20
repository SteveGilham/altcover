namespace N

open System
open NUnit.Framework

module M =
  type Thing =
    { Thing : string }
    member this.bytes() = System.Text.Encoding.UTF8.GetBytes(this.Thing)

  let makeThing s = { Thing = s }

  [<Test>]
  let testMakeThing() =
    Assert.AreEqual("s", (makeThing "s").Thing)
    Assert.AreEqual(5, (makeThing "aeiou").bytes().Length)

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

  [<Test>]
  let testMakeUnion() =
    Assert.AreEqual(returnFoo 10, Foo 10)
    Assert.AreEqual(returnBar "s", Bar "s")
    Assert.AreEqual(Bar "10", (Foo 10).as_bar())
#if NETCOREAPP2_1

module Program =
  [<EntryPoint>]
  let main _ = 0
#endif