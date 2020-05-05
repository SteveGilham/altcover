namespace Tests

open System
open System.Reflection
open Xunit

open Swensen.Unquote

[<assembly:AssemblyVersionAttribute("1.0.0.0")>]
[<assembly:AssemblyFileVersionAttribute("1.0.0.0")>]
do ()

module M =
  type Thing =
    { Thing : string }
    member this.Bytes() = System.Text.Encoding.UTF8.GetBytes(this.Thing)

  let makeThing s = { Thing = s }

  [<Fact>]
  let testMakeThing() =
    test <@ (makeThing "s").Thing = "s" @>
    test <@ (makeThing "aeiou").Bytes().Length = 5 @>

module DU =
  type MyUnion =
    | Foo of int
    | Bar of string
    | Baz of double
    | Bop of DateTime

    member this.AsBar() =
      try
        match this with
        | Foo n -> Bar(string n)
        | Baz d -> Bar(string d)
        | Bop t -> Bar(string t)
        // New cases go in here
        | _ -> this
      with _ -> Bar "none"

    member this.MyBar = this.AsBar

  type MyClass() =
    member val Property = 0 with get, set

  let returnFoo v = Foo v
  let returnBar v = Bar v

  [<Fact; System.CodeDom.Compiler.GeneratedCodeAttribute("Not really", "0.0")>]
  let testMakeUnion() =
    test <@ returnFoo 10 = Foo 10 @>
    test <@ returnBar "s" = Bar "s" @>
    test <@ (Foo 10).AsBar() = Bar "10" @>

  let LineSpanning1 a =
    if a > 0
    then "positive"
    else "non-positive"

  let LineSpanning2 a =
    a
    |> Seq.mapi (fun i j -> if j > 0
                            then i
                            else 0)
    |> Seq.filter (fun i -> i % 2 = 0)
    |> Seq.sum

  let LineSpanning3 a =
    "intro " +
    a.ToString() +
    " outro"

  let Multiples (a:float) (b:float) (c:float) =
    let compute a' b' discr' =
      ((discr' - b')/(2.0 * a'), (-1.0 * (discr' + b'))/(2.0 * a'))
    let bs = printfn "%A" b;b * b
    let spread = printfn "%A %A" a c; 4.0 * a * c
    let discr = if bs < spread then failwith "no real roots" else Math.Sqrt (bs - spread)
    compute a b discr

#if NETCOREAPP2_1
module Program =
  [<EntryPoint>]
  let main _ = 0
#endif