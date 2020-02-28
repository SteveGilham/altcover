namespace AltCover

module internal AltCover =
  let internal ToConsole() =
    Output.Error <- CommandLine.WriteErr
    Output.Usage <- CommandLine.UsageBase
    Output.Echo <- CommandLine.WriteErr
    Output.Info <- CommandLine.WriteOut
    Output.Warn <- CommandLine.WriteOut

  [<EntryPoint>]
  [<System.Diagnostics.CodeAnalysis.SuppressMessage(
      "Gendarme.Rules.Portability",
      "ExitCodeIsLimitedOnUnixRule",
      Justification="limited 0-255 elsewhere")>]
  let private Main arguments =
    ToConsole()
    AltCover.Main.EffectiveMain arguments