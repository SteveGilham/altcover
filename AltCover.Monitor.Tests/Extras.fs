namespace AltCover

[<AutoOpen>]
module internal Augment =
  type System.Object with
    member self.IsNotNull
      with get() =
        self |> isNull |> not