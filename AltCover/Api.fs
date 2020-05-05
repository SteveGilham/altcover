namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open TaskIO

[<RequireQualifiedAccess>]
[<SuppressMessage("Microsoft.Naming", "CA1704",
  Justification="'Api' works")>]
module Api =
  let Prepare (args : OptionApi.PrepareOptions) (log : OptionApi.LoggingOptions) =
    log.Apply()
    args
    |> Args.prepare
    |> List.toArray
    |> Main.effectiveMain

  let Collect (args : OptionApi.CollectOptions) (log : OptionApi.LoggingOptions) =
    log.Apply()
    Args.collect args
    |> List.toArray
    |> Main.effectiveMain

  let ImportModule() = TaskIO.getStringValue "ImportModule"
  let Version() = Version(AssemblyVersionInformation.AssemblyFileVersion)
  let FormattedVersion() = TaskIO.getStringValue "Version"