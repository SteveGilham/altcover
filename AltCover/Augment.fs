#nowarn "25"
namespace AltCover

#if GUI
module Augment =
#else
module internal Augment =
#endif
  type System.Object with
    member self.IsNotNull
      with get() =
        self |> isNull |> not

#if GUI
#else
  type Microsoft.FSharp.Core.Option<'T> with
    static member getOrElse (fallback : 'T) (x : option<'T>) = defaultArg x fallback
    static member nullable (x : 'T) : option<'T> =
      if isNull (x :> obj) then None else Some x
#endif
  type Either<'a, 'b> = Choice<'b, 'a>

  [<System.Diagnostics.CodeAnalysis.SuppressMessage(
    "Gendarme.Rules.Design.Generic", "AvoidMethodWithUnusedGenericTypeRule",
    Justification = "Context in F# has to be sufficient")>]
  let Right x : Either<'a, 'b> = Choice1Of2 x
  [<System.Diagnostics.CodeAnalysis.SuppressMessage(
    "Gendarme.Rules.Design.Generic", "AvoidMethodWithUnusedGenericTypeRule",
    Justification = "Context in F# has to be sufficient")>]
  let Left x : Either<'a, 'b> = Choice2Of2 x

  let (|Right|Left|) =
    function
    | Choice1Of2 x -> Right x
    | Choice2Of2 x -> Left x
#if GUI
#else
  let Increment b =
    if b then 1 else 0

  let internal Split(l : 'a list) =
    (l.Head, l.Tail) // since Gendarme thinks the concatenation operator is a hardcoded path!
#endif