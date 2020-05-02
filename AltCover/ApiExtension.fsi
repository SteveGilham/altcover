namespace AltCover.ApiExtension

open System.Runtime.CompilerServices
open AltCover

module Prepare = begin
  [<Extension>]
  val WhatIf : prepare:FSApi.PrepareParameters -> FSApi.ValidatedCommandLine
end
module Collect = begin
  [<Extension>]
  val WhatIf :
    collect:FSApi.CollectParameters ->
      afterPreparation:bool -> FSApi.ValidatedCommandLine
end
module FSApiExtension = begin
  type FSApi.CollectParameters with
    member WhatIf : afterPreparation:bool -> FSApi.ValidatedCommandLine
  type FSApi.PrepareParameters with
    member WhatIf : unit -> FSApi.ValidatedCommandLine
end