namespace N
// fsharplint:disable  MemberNames NonPublicValuesNames RedundantNewKeyword
open System
open System.Reflection
open NUnit.Framework

open Swensen.Unquote

[<assembly:AssemblyVersionAttribute("1.0.0.0")>]
[<assembly:AssemblyFileVersionAttribute("1.0.0.0")>]
do ()

module M =
  type Thing =
    { Thing : string }
    member this.bytes() = System.Text.Encoding.UTF8.GetBytes(this.Thing)

  let makeThing s = { Thing = s }

  [<Test>]
  let testMakeThing() =
    test <@ (makeThing "s").Thing = "s" @>
    test <@ (makeThing "aeiou").bytes().Length = 5 @>

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
  //let testMakeUnion ([<Range(10,136)>] x) = // at buffer limit
  //  Assert.AreEqual(returnFoo x, Foo x)
    test <@ returnFoo 10 = Foo 10 @>
    test <@ returnBar "s" = Bar "s" @>
    test <@ (Foo 10).as_bar() = Bar "10" @>
#if NETCOREAPP2_1

module Program =
  [<EntryPoint>]
  let main _ = 0
#endif