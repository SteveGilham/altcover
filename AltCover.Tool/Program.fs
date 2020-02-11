namespace AltCover

module DotNetAltCover =
  let internal ToConsole() =
    Output.Error <- CommandLine.WriteErr
    Output.Usage <- CommandLine.Usage
    Output.Echo <- CommandLine.WriteErr
    Output.Info <- CommandLine.WriteOut

  [<EntryPoint>]
  let private Main arguments =
    ToConsole()
    Output.Error "DotNetCliToolReference Tools are deprecated at .net core 3+"
    Output.Error "This tool will be removed in AltCover version 7+ releases"
    AltCover.Main.EffectiveMain arguments