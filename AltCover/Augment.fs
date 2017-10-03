namespace AltCover

module Augment =
  type Microsoft.FSharp.Core.Option<'T> with
      static member getOrElse (fallback : 'T) (x : option<'T>) = defaultArg x fallback

      static member nullable (x : 'a when 'a : null) : option<'a> =
        if isNull x then None
        else Some x