namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open TaskIO

[<RequireQualifiedAccess>]
[<SuppressMessage("Gendarme.Rules.Smells",
                  "AvoidSpeculativeGeneralityRule",
                  Justification = "This assembles the significant public surface for the assembly")>]
module Command =
  let Prepare (args: Abstract.IPrepareOptions) (log: AltCover.LoggingOptions) =
    log.Apply()

    args
    |> Args.prepare
    |> List.toArray
    |> Main.effectiveMain

  let Collect (args: Abstract.ICollectOptions) (log: AltCover.LoggingOptions) =
    log.Apply()

    Args.collect args
    |> List.toArray
    |> Main.effectiveMain

  let ImportModule () = TaskIO.getStringValue "ImportModule"

  let Version () =
    Version(AssemblyVersionInformation.AssemblyFileVersion)

  let FormattedVersion () = TaskIO.getStringValue "Version"
  let Summary () = Runner.summary.ToString()