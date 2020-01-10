#nowarn "25"
namespace AltCover

#if GUI
module Augment =
#else
module internal Augment =
#endif

  type Microsoft.FSharp.Core.Option<'T> with
    static member getOrElse (fallback : 'T) (x : option<'T>) = defaultArg x fallback
    static member nullable (x : 'a when 'a : null) : option<'a> =
      if isNull x then None else Some x

  type Either<'a, 'b> = Choice<'b, 'a>

  let Right x : Either<'a, 'b> = Choice1Of2 x
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