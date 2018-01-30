namespace AltCover

module Main =
  [<EntryPoint>]
  let private Main arguments =
    Runner.DoCoverage arguments
    0