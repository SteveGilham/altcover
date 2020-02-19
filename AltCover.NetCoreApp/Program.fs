namespace AltCover

open System
open System.Runtime.InteropServices

[<assembly:CLSCompliant(true)>]
[<assembly:ComVisible(false)>]
()

module DotNetAltCover =
  let internal ToConsole() =
    Output.Error <- CommandLine.WriteErr
    Output.Usage <- CommandLine.UsageBase
    Output.Echo <- CommandLine.WriteErr
    Output.Info <- CommandLine.WriteOut

  [<EntryPoint>]
  [<System.Diagnostics.CodeAnalysis.SuppressMessage(
      "Gendarme.Rules.Portability",
      "ExitCodeIsLimitedOnUnixRule",
      Justification="limited 0-255 elsewhere")>]
  let private Main arguments =
    ToConsole()
    AltCover.Main.EffectiveMain arguments