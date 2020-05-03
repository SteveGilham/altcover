namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open ApiStore

[<RequireQualifiedAccess>]
[<SuppressMessage("Microsoft.Naming", "CA1704",
  Justification="'Api' works")>]
module Api =
  let Prepare (args : FSApi.PrepareOptions) (log : FSApi.LoggingOptions) =
    log.Apply()
    args
    |> Args.prepare
    |> List.toArray
    |> Main.effectiveMain

  let Collect (args : FSApi.CollectOptions) (log : FSApi.LoggingOptions) =
    log.Apply()
    Args.collect args
    |> List.toArray
    |> Main.effectiveMain

  let ImportModule() = ApiStore.getStringValue "ImportModule"
  let Version() = Version(AssemblyVersionInformation.AssemblyFileVersion)
  let FormattedVersion() = ApiStore.getStringValue "Version"