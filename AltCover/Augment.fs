namespace AltCover

module Augment =
  type Microsoft.FSharp.Core.Option<'T> with
      static member getOrElse (fallback : 'T) (x : option<'T>) = defaultArg x fallback

      static member nullable (x : 'a when 'a : null) : option<'a> =
        if isNull x then None
        else Some x

  type Either<'a,'b> = Choice<'b,'a>
  let  Right x :Either<'a,'b> = Choice1Of2 x
  let  Left  x :Either<'a,'b> = Choice2Of2 x
  let  (|Right|Left|) = function Choice1Of2 x -> Right x | Choice2Of2 x -> Left x