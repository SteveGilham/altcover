namespace AltCover

[<AutoOpen>]
module Abstraction =

  // abstract away selection based on OS dependencies and other environment constants
  let Maybe predicate (result: 'TResult) (fallBack: 'TResult) = if predicate
                                                                then result
                                                                else fallBack