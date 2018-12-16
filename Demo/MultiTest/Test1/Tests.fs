namespace Tests1

open System1.M
open System.Reflection
open Xunit

[<assembly:AssemblyVersionAttribute("1.0.0.0")>]
[<assembly:AssemblyFileVersionAttribute("1.0.0.0")>]
do ()

module M =
  [<Fact>]
  let testMakeThing() =
    Assert.Equal("s", (makeThing "s").Thing)
    Assert.Equal(5, (makeThing "aeiou").bytes().Length)