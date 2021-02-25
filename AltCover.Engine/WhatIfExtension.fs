namespace AltCover

open System.Diagnostics.CodeAnalysis
open System.Runtime.CompilerServices

[<Extension>]
module PrepareExtension =
  [<Extension>]
  let WhatIf (prepare: Abstract.IPrepareOptions) : AltCover.ValidatedCommandLine =
    { Command = Args.prepare prepare
      Errors =
        (AltCover.PrepareOptions.Abstract prepare)
          .Validate() }

[<Extension>]
module CollectExtension =
  [<Extension>]
  let WhatIf
    (collect: Abstract.ICollectOptions)
    afterPreparation
    : AltCover.ValidatedCommandLine =
    { Command = Args.collect collect
      Errors =
        (AltCover.CollectOptions.Abstract collect)
          .Validate(afterPreparation) }

[<SuppressMessage("Gendarme.Rules.Smells",
                  "AvoidSpeculativeGeneralityRule",
                  Justification = "Two different extension mechanisms need placating")>]
[<AutoOpen>]
module WhatIfExtension =
  type Abstract.ICollectOptions with
    [<CompiledName("WhatIf")>]
    member self.WhatIf afterPreparation : AltCover.ValidatedCommandLine =
      CollectExtension.WhatIf self afterPreparation

  type Abstract.IPrepareOptions with
    [<SuppressMessage("Microsoft.Naming",
                      "CA1704:IdentifiersShouldBeSpelledCorrectly",
                      Justification = "Compiler generated name for unit parameter")>]
    [<CompiledName("WhatIf")>]
    member self.WhatIf() = PrepareExtension.WhatIf self