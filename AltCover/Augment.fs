#nowarn "25"
namespace AltCover

[<System.Diagnostics.CodeAnalysis.SuppressMessage(
  "Gendarme.Rules.Smells", "AvoidSpeculativeGeneralityRule",
  Justification = "AvoidCodeDuplicatedInSameClassRule")>]
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
  type System.String with
    member self.X
      with get() =
        System.Xml.Linq.XName.Get self

  type Microsoft.FSharp.Core.Option<'T> with
    // fsharplint:disable-next-line MemberNames
    static member getOrElse (fallback : 'T) (x : option<'T>) = defaultArg x fallback
    // fsharplint:disable-next-line MemberNames
    static member nullable (x : 'T) : option<'T> =
      if isNull (x :> obj) then None else Some x
#endif
  type internal Either<'a, 'b> = Choice<'b, 'a>

  [<System.Diagnostics.CodeAnalysis.SuppressMessage(
    "Gendarme.Rules.Design.Generic", "AvoidMethodWithUnusedGenericTypeRule",
    Justification = "Context in F# has to be sufficient")>]
  let Right x : Either<'a, 'b> = Choice1Of2 x
  [<System.Diagnostics.CodeAnalysis.SuppressMessage(
    "Gendarme.Rules.Design.Generic", "AvoidMethodWithUnusedGenericTypeRule",
    Justification = "Context in F# has to be sufficient")>]
  let Left x : Either<'a, 'b> = Choice2Of2 x

  let internal (|Right|Left|) =
    function
    | Choice1Of2 x -> Right x
    | Choice2Of2 x -> Left x
#if GUI
#else
  type System.Boolean with
    member self.ToInt32
      with get() =
        if self then 1 else 0

  type System.Int32 with
    member self.Increment (b : bool) =
      self + b.ToInt32

#if WEAKNAME
  type System.UInt64 with
    member self.Increment (b : bool) =
      self + (uint64 b.ToInt32)
#endif

  type Microsoft.FSharp.Collections.List<'T> with
   member self.Split
     with get () =
      (self.Head, self.Tail) // since Gendarme thinks the concatenation operator is a hardcoded path!

  let internal doWithStream (create : unit -> 'a) (action : 'a -> unit) =
    use stream = create()
    action stream
#endif