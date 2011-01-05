namespace AltCover

type OptionBuilder() =
    member self.Bind(input, operation) = Option.bind operation input
    member self.Delay (operation:(unit -> option<'T>)) = operation ()
    member self.Return input = Some input
    member self.ReturnFrom (input : option<'T>) = input

module Monads =   
  let option = OptionBuilder()
  
module Augment =   
  type Option<'T> with
      member self.filter (f : 'T -> bool) =
        self |>
        Option.bind (fun x -> if f(x) then Some(x) else None)