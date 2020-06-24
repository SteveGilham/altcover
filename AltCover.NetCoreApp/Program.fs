namespace AltCover

open System
open System.Runtime.InteropServices
#if GLOBALTOOL
open System.IO
open System.Reflection
open AltCover.Main
#endif

[<assembly:CLSCompliant(true)>]
[<assembly:ComVisible(false)>]
()

module DotNetAltCover =
  let internal toConsole() =
    Output.error <- CommandLine.writeErr
    Output.usage <- CommandLine.usageBase
    Output.echo <- CommandLine.writeErr
    Output.info <- CommandLine.writeOut

  [<EntryPoint>]
  [<System.Diagnostics.CodeAnalysis.SuppressMessage(
      "Gendarme.Rules.Portability",
      "ExitCodeIsLimitedOnUnixRule",
      Justification="limited 0-255 elsewhere")>]
  let private main arguments =
    toConsole()
    let result =
#if GLOBALTOOL
        let first =
          arguments
          |> Seq.tryHead
          |> Option.defaultValue String.Empty
        init()
        match first with
        | Select "TargetsPath" _ ->
          let here = Assembly.GetExecutingAssembly().Location
          let targets =
            Path.Combine(
              here |> Path.GetDirectoryName,
              "../../../build/netstandard2.0/altcover.global.targets")
            |> Path.GetFullPath
          targets |> Output.info
          0
        | _ ->
#endif
          AltCover.Main.effectiveMain arguments
    result