namespace AltCover

open System.Diagnostics.CodeAnalysis
open System.Runtime.CompilerServices

[<assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
   Scope="namespace", Target="ApiExtension", MessageId="Api",
    Justification="'Api' is OK")>]
()

[<Extension>]
module PrepareExtension =
  [<Extension>]
  let WhatIf (prepare : AltCover.PrepareOptions) : AltCover.ValidatedCommandLine=
      { Command = Args.prepare prepare
        Errors = prepare.Validate() }

[<Extension>]
module CollectExtension =
  [<Extension>]
  let WhatIf (collect : AltCover.CollectOptions) afterPreparation : AltCover.ValidatedCommandLine=
      { Command = Args.collect collect
        Errors = collect.Validate(afterPreparation) }

[<SuppressMessage("Gendarme.Rules.Smells", "AvoidSpeculativeGeneralityRule",
  Justification="Two different extension mechanisms need placating")>]
[<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1704",
    Justification="'Api' is OK")>]
[<AutoOpen>]
module AltCoverExtension =
  type AltCover.CollectOptions with
    member self.WhatIf afterPreparation : AltCover.ValidatedCommandLine =
      CollectExtension.WhatIf self afterPreparation

  type AltCover.PrepareOptions with
    [<SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
      Justification="Compiler generated name for self parameter")>]
    member self.WhatIf() =
      PrepareExtension.WhatIf self