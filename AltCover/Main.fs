namespace AltCover

module AltCover =

  [<EntryPoint>]
  let private Main arguments =
    Base.Output.Error <- CommandLine.WriteErr
    Base.Output.Usage <- CommandLine.Usage
    Base.Output.Echo <- CommandLine.WriteErr
    Base.Output.Info <- CommandLine.WriteOut

    AltCover.Main.EffectiveMain arguments