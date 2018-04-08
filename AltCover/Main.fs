namespace AltCover

module AltCover =

  [<EntryPoint>]
  let private Main arguments =
    Output.Error <- CommandLine.WriteErr
    Output.Usage <- CommandLine.Usage
    Output.Echo <- CommandLine.WriteErr
    Output.Info <- CommandLine.WriteOut

    AltCover.Main.EffectiveMain arguments