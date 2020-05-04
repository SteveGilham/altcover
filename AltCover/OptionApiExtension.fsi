// # namespace `AltCover.Extension`
// ```
namespace AltCover.Extension

open System.Runtime.CompilerServices

// ```
// ## module `Prepare` and module `Collect`
// ```
module Prepare = begin
  [<Extension>]
  val WhatIf : prepare:AltCover.OptionApi.PrepareOptions -> AltCover.OptionApi.ValidatedCommandLine
end
module Collect = begin
  [<Extension>]
  val WhatIf :
    collect:AltCover.OptionApi.CollectOptions ->
      afterPreparation:bool -> AltCover.OptionApi.ValidatedCommandLine
end
// ```
// These provide C#-compatible extension methods to perform a `WhatIf` style command like validation
//
// `WhatIf` compiles the effective command-line and the result of `Validate`
//
// ## module `OptionApi`
// ```
module OptionApi = begin
  type AltCover.OptionApi.CollectOptions with
    member WhatIf : afterPreparation:bool -> AltCover.OptionApi.ValidatedCommandLine
  type AltCover.OptionApi.PrepareOptions with
    member WhatIf : unit -> AltCover.OptionApi.ValidatedCommandLine
end
//```
// F# style extensions