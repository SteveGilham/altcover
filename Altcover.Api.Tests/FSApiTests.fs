namespace Tests

open System
open System.IO
open System.Linq
open System.Reflection
open System.Text.RegularExpressions
open System.Xml.Linq

open AltCover
open Swensen.Unquote

#if NETCOREAPP3_0
[<AttributeUsage(AttributeTargets.Method)>]
type TestAttribute() = class
    inherit Attribute()
end
#else
type TestAttribute = NUnit.Framework.TestAttribute
#endif

module FSApiTests =
  let SolutionDir() =
    SolutionRoot.location

  [<Test>]
  let IsOK() =
    let input = [ "string"; null; "another string" ]
    test <@ input |> List.length = 3 @>