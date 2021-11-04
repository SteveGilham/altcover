#nowarn "25"
namespace AltCover

open System.Diagnostics.CodeAnalysis

[<SuppressMessage("Gendarme.Rules.Smells", "AvoidSpeculativeGeneralityRule",
  Justification = "AvoidCodeDuplicatedInSameClassRule")>]
[<AutoOpen>]
#if GUI
module Augment =
#else
module internal Augment =
#endif

#if !ValidateGendarmeEmulation
  [<SuppressMessage("Microsoft.Globalization", "CA1307:SpecifyStringComparison",
    Justification="Preferred overload, no comparison exists in netstd2.0/net472")>]
  let internal charIndexOf (name:string) (token:char) =
    name.IndexOf(token)
#endif

  type System.Object with
    member self.IsNotNull
      with get() =
        self |> isNull |> not

#if !ValidateGendarmeEmulation
  type System.String with
    member self.X
      with get() =
        System.Xml.Linq.XName.Get self
#if !GUI

    member self.InvariantParseDouble() =
      System.Double.TryParse(self,
                             System.Globalization.NumberStyles.Number,
                             System.Globalization.CultureInfo.InvariantCulture)
#endif
#endif
  type internal Either<'a, 'b> = Choice<'b, 'a>

  [<SuppressMessage(
    "Gendarme.Rules.Design.Generic", "AvoidMethodWithUnusedGenericTypeRule",
    Justification = "Context in F# has to be sufficient")>]
  [<SuppressMessage("Gendarme.Rules.Naming",
                    "UseCorrectCasingRule",
                    Justification = "Idiomatic F# style")>]
  let Right x : Either<'a, 'b> = Choice1Of2 x
  [<System.Diagnostics.CodeAnalysis.SuppressMessage(
    "Gendarme.Rules.Design.Generic", "AvoidMethodWithUnusedGenericTypeRule",
    Justification = "Context in F# has to be sufficient")>]
  [<SuppressMessage("Gendarme.Rules.Naming",
                    "UseCorrectCasingRule",
                    Justification = "Idiomatic F# style")>]
  let Left x : Either<'a, 'b> = Choice2Of2 x

  let internal (|Right|Left|) =
    function
    | Choice1Of2 x -> Right x
    | Choice2Of2 x -> Left x

#if !(GUI || ValidateGendarmeEmulation)
  let internal doWithStream (create : unit -> 'a) (action : 'a -> unit) =
    use stream = create()
    action stream
#endif

  type System.Boolean with
    member self.ToInt32
      with get() =
        if self then 1 else 0

#if !ValidateGendarmeEmulation
  type System.Int32 with
    member self.Increment (b : bool) =
      self + b.ToInt32
#endif

#if !GUI
  type Microsoft.FSharp.Collections.List<'T> with
   member self.Split
     with get () =
      (self.Head, self.Tail) // since Gendarme thinks the concatenation operator is a hardcoded path!
#endif

[<assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
  Scope="member", Target="AltCover.Augment.#Left`2(!!0)", MessageId="a",
  Justification="Compiler Generated")>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
  Scope="member", Target="AltCover.Augment.#Left`2(!!0)", MessageId="b",
  Justification="Compiler Generated")>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
  Scope="member", Target="AltCover.Augment.#Left`2(!!0)", MessageId="x",
  Justification="Trivial usage")>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
  Scope="member", Target="AltCover.Augment.#Object.get_IsNotNull(System.Object)",
  MessageId="param",
  Justification="Compiler Generated")>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1707:IdentifiersShouldNotContainUnderscores",
  Scope="member", Target="AltCover.Augment.#Object.get_IsNotNull(System.Object)",
  Justification="Compiler Generated")>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
  Scope="member", Target="AltCover.Augment.#Right`2(!!0)", MessageId="a",
  Justification="Compiler Generated")>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
  Scope="member", Target="AltCover.Augment.#Right`2(!!0)", MessageId="b",
  Justification="Compiler Generated")>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
  Scope="member", Target="AltCover.Augment.#Right`2(!!0)", MessageId="x",
  Justification="Trivial usage")>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
  Scope="member", Target="AltCover.Augment.#String.get_X(System.String)", MessageId="param",
  Justification="Compiler generated")>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1707:IdentifiersShouldNotContainUnderscores",
  Scope="member", Target="AltCover.Augment.#String.get_X(System.String)",
  Justification="Compiler generated")>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
  Scope="member", Target="AltCover.Augment.#Boolean.get_ToInt32(System.Boolean)", MessageId="param",
  Justification="Compiler generated")>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1707:IdentifiersShouldNotContainUnderscores",
  Scope="member", Target="AltCover.Augment.#Boolean.get_ToInt32(System.Boolean)",
  Justification="Compiler generated")>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
  Scope="member",
  Target="AltCover.Augment.#Int32.Increment(System.Int32,System.Boolean)", MessageId="b",
  Justification="Compiler generated")>]
#if GUI
[<assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
  Scope="member", Target="AltCover.Augment.#|Right|Left|`2(Microsoft.FSharp.Core.FSharpChoice`2<!!0,!!1>)",
  MessageId="a", Justification="Compiler Generated")>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
  Scope="member", Target="AltCover.Augment.#|Right|Left|`2(Microsoft.FSharp.Core.FSharpChoice`2<!!0,!!1>)",
  MessageId="b", Justification="Compiler Generated")>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1707:IdentifiersShouldNotContainUnderscores",
  Scope="member", Target="AltCover.Augment.#|Right|Left|`2(Microsoft.FSharp.Core.FSharpChoice`2<!!0,!!1>)",
  Justification="Compiler Generated")>]
#endif
()