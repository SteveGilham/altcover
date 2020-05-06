namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open TaskIO

[<RequireQualifiedAccess>]
module Command =
  let Prepare (args : AltCover.PrepareOptions) (log : AltCover.LoggingOptions) =
    log.Apply()
    args
    |> Args.prepare
    |> List.toArray
    |> Main.effectiveMain

  let Collect (args : AltCover.CollectOptions) (log : AltCover.LoggingOptions) =
    log.Apply()
    Args.collect args
    |> List.toArray
    |> Main.effectiveMain

  let ImportModule() = TaskIO.getStringValue "ImportModule"
  let Version() = Version(AssemblyVersionInformation.AssemblyFileVersion)
  let FormattedVersion() = TaskIO.getStringValue "Version"