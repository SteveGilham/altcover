namespace AltCover

type OptionBuilder() =
    member self.Bind(input, operation) = Option.bind operation input
    member self.Delay (operation:(unit -> option<'T>)) = operation ()
    member self.Return input = Some input
    member self.ReturnFrom (input : option<'T>) = input

module Monads =
  let option = OptionBuilder()

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
        if x <> null then Some x
        else None