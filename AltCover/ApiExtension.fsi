namespace AltCover.ApiExtension

open System.Runtime.CompilerServices
open AltCover

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
module FSApiExtension = begin
  type FSApi.CollectOptions with
    member WhatIf : afterPreparation:bool -> FSApi.ValidatedCommandLine
  type FSApi.PrepareOptions with
    member WhatIf : unit -> FSApi.ValidatedCommandLine
end