namespace AltCover

module internal AltCover =
  let internal ToConsole() =
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
  let private Main arguments =
    ToConsole()
    AltCover.Main.effectiveMain arguments