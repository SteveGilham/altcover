namespace AltCover

open System

module Runner =

  let DoCoverage _ = ()

  [<EntryPoint>]
  let private Main arguments =
    DoCoverage arguments
    0