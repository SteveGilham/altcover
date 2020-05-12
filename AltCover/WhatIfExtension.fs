namespace AltCover

open System.Diagnostics.CodeAnalysis
open System.Runtime.CompilerServices

[<Extension>]
module PrepareExtension =
  [<Extension>]
  let WhatIf (prepare : Abstract.IPrepareOptions) : AltCover.ValidatedCommandLine=
      { Command = Args.prepare prepare
        Errors = (AltCover.PrepareOptions.Abstract prepare).Validate() }

[<Extension>]
module CollectExtension =
  [<Extension>]
  let WhatIf (collect : Abstract.ICollectOptions) afterPreparation : AltCover.ValidatedCommandLine=
      { Command = Args.collect collect
        Errors = (AltCover.CollectOptions.Abstract collect).Validate(afterPreparation) }