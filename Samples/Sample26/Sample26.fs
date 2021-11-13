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
  let monitor tuple =
    let (a, b) = AltCover.Monitor.TryGetVisitTotals()
    if a
    then
      let code = b.Code
      let branch = b.Branch
      test <@ (code, branch) = tuple @>

  type Thing =
    { Thing : string }
    member this.bytes() = System.Text.Encoding.UTF8.GetBytes(this.Thing)

  let makeThing s = { Thing = s }

  [<Test>]
  let testMakeThing() =
    test <@ (makeThing "s").Thing = "s" @>
    monitor (19, 6)
    test <@ (makeThing "aeiou").bytes().Length = 5 @>
    monitor (22, 7)

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
    test <@ returnFoo 10 = Foo 10 @>
    M.monitor (4, 1)
    test <@ returnBar "s" = Bar "s" @>
    M.monitor (11, 3)
    test <@ (Foo 10).as_bar() = Bar "10" @>
    M.monitor (16, 5)
    
#if !NET472
module Program =
  [<EntryPoint>]
  let main _ = 0
#endif
