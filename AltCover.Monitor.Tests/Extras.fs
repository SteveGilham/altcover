namespace AltCover

[<AutoOpen>]
module internal Augment =
  type System.Object with
    member self.IsNotNull = self |> isNull |> not