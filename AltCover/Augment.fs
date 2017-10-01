namespace AltCover

module Augment =
  type Microsoft.FSharp.Core.Option<'T> with
      static member filter (f : 'T -> bool) (x : option<'T>) =
        match x with
        | Some v when f(v) -> Some v
        | _ -> None

      static member getOrElse (fallback : 'T) (x : option<'T>) = defaultArg x fallback

      static member select (f : 'T -> bool) (x : 'T) =
        if f(x) then Some x
        else None

      static member nullable (x : 'a when 'a : null) : option<'a> =
        if isNull x then None
        else Some x