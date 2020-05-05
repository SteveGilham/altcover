namespace AltCover

module internal AltCover =
  let internal toConsole() =
    Output.error <- CommandLine.writeErr
    Output.usage <- CommandLine.usageBase
    Output.echo <- CommandLine.writeErr
    Output.info <- CommandLine.writeOut
    Output.warn <- CommandLine.writeOut

  [<EntryPoint>]
  [<System.Diagnostics.CodeAnalysis.SuppressMessage(
      "Gendarme.Rules.Portability",
      "ExitCodeIsLimitedOnUnixRule",
      Justification="limited 0-255 elsewhere")>]
  let private main arguments =
    toConsole()
    AltCover.Main.effectiveMain arguments