namespace Tests
// fsharplint:disable  MemberNames NonPublicValuesNames RedundantNewKeyword

open AltCover

#nowarn "25"

[<AutoOpen>]
module Extensions =
  type internal Exemption with
    static member internal OfInt i =
      if i > 0 then
        Exemption.Visited
      else if i < -4 then
        Exemption.None
      else
        i
        |> sbyte
        |> Microsoft.FSharp.Core.LanguagePrimitives.EnumOfValue<sbyte, Exemption>

module Augment =

  [<Test>]
  let ZeroIsNotVisited () =
    // DELETE ME
    let x = ProxyObject()
    test <@ x.IsNotNull @>
    // end DELETE ME

    test <@ Exemption.OfInt 0 = Exemption.None @>

  [<Test>]
  let PositiveIsVisited () =
    test
      <@
        [ 1..255 ]
        |> Seq.map Exemption.OfInt
        |> Seq.tryFind (fun x -> x <> Exemption.Visited) = None
      @>

  [<Test>]
  let NegativesSpray () =
    test
      <@
        [ 0..5 ]
        |> Seq.map ((~-) >> Exemption.OfInt)
        |> Seq.toList = [ Exemption.None
                          Exemption.Declared
                          Exemption.Automatic
                          Exemption.StaticAnalysis
                          Exemption.Excluded
                          Exemption.None ]
      @>