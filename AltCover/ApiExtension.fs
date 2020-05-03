namespace AltCover.ApiExtension

open System.Diagnostics.CodeAnalysis
open System.Runtime.CompilerServices

[<assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
   Scope="namespace", Target="AltCover.ApiExtension", MessageId="Api",
    Justification="'Api' is OK")>]
()

[<Extension>]
module Prepare =
  [<Extension>]
  let WhatIf (prepare : AltCover.FSApi.PrepareOptions) : AltCover.FSApi.ValidatedCommandLine=
      { Command = AltCover.Args.prepare prepare
        Errors = prepare.Validate() }

[<Extension>]
module Collect =
  [<Extension>]
  let WhatIf (collect : AltCover.FSApi.CollectOptions) afterPreparation : AltCover.FSApi.ValidatedCommandLine=
      { Command = AltCover.Args.collect collect
        Errors = collect.Validate(afterPreparation) }

[<SuppressMessage("Gendarme.Rules.Smells", "AvoidSpeculativeGeneralityRule",
  Justification="Two different extension mechanisms need placating")>]
[<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1704",
    Justification="'Api' is OK")>]
module FSApiExtension =
  type AltCover.FSApi.CollectOptions with
    member self.WhatIf afterPreparation : AltCover.FSApi.ValidatedCommandLine =
      Collect.WhatIf self afterPreparation

  type AltCover.FSApi.PrepareOptions with
    [<SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
      Justification="Compiler generated name for self parameter")>]
    member self.WhatIf() =
      Prepare.WhatIf self