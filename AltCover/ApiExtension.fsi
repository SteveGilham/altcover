// # namespace AltCover.ApiExtension
// ```
namespace AltCover.ApiExtension

open System.Runtime.CompilerServices
open AltCover
// ```
// ## module Prepare and module Collect
// ```
module Prepare = begin
  [<Extension>]
  val WhatIf : prepare:FSApi.PrepareOptions -> FSApi.ValidatedCommandLine
end
module Collect = begin
  [<Extension>]
  val WhatIf :
    collect:FSApi.CollectOptions ->
      afterPreparation:bool -> FSApi.ValidatedCommandLine
end
// ```
// These provide C#-compatible extension methods to perform a `WhatIf` style command like validation 
//
// `WhatIf` compiles the effective command-line and the result of `Validate`
//
// ## module FSApiExtension
// ```
module FSApiExtension = begin
  type FSApi.CollectOptions with
    member WhatIf : afterPreparation:bool -> FSApi.ValidatedCommandLine
  type FSApi.PrepareOptions with
    member WhatIf : unit -> FSApi.ValidatedCommandLine
end
//```
// F# style extensions