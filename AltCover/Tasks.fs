namespace AltCover

open System
open Microsoft.Build.Utilities
open Microsoft.Build.Framework
open Augment

[<RequireQualifiedAccess>]
module Api =
  let Prepare (args : FSApi.PrepareParams) (log : FSApi.Logging) =
    log.Apply()
    args
    |> FSApi.Args.Prepare
    |> List.toArray
    |> Main.EffectiveMain

  let Collect (args : FSApi.CollectParams) (log : FSApi.Logging) =
    log.Apply()
    FSApi.Args.Collect args
    |> List.toArray
    |> Main.EffectiveMain

  let mutable internal store = String.Empty
  let private writeToStore s = store <- s
  let internal LogToStore = FSApi.Logging.Primitive { Primitive.Logging.Create() with Info = writeToStore }

  let internal GetStringValue s =
    writeToStore String.Empty
    LogToStore.Apply()
    [| s |]
    |> Main.EffectiveMain
    |> ignore
    store

  let Ipmo() = GetStringValue "ipmo"
  let Version() = GetStringValue "version"

type Prepare() =
  inherit Task(null)
  member val internal ACLog : FSApi.Logging option = None with get, set

  member val InputDirectory = String.Empty with get, set
  member val OutputDirectory = String.Empty with get, set
  member val SymbolDirectories : string array = [||] with get, set
#if NETCOREAPP2_0
  member val Dependencies : string array = [||] with get, set
#else
  member val Keys : string array = [| |] with get, set
  member val StrongNameKey = String.Empty with get, set
#endif
  member val XmlReport = String.Empty with get, set
  member val FileFilter : string array = [||] with get, set
  member val AssemblyFilter : string array = [||] with get, set
  member val AssemblyExcludeFilter : string array = [||] with get, set
  member val TypeFilter : string array = [||] with get, set
  member val MethodFilter : string array = [||] with get, set
  member val AttributeFilter : string array = [||] with get, set
  member val PathFilter : string array = [||] with get, set
  member val CallContext : string array = [||] with get, set
  member val OpenCover = true with get, set
  member val InPlace = true with get, set
  member val Save = true with get, set
  member val Single = true |> not with get, set // work around Gendarme insistence on non-default values only
  member val LineCover = true |> not with get, set
  member val BranchCover = true |> not with get, set
  member val CommandLine : string array = [||] with get, set
  member val SourceLink = false with get, set

  member self.Message x = base.Log.LogMessage(MessageImportance.High, x)
  override self.Execute() =
    let log =
      Option.getOrElse (FSApi.Logging.Primitive { Primitive.Logging.Create() with Error = base.Log.LogError
                                                                                  Warn = base.Log.LogWarning
                                                                                  Info = self.Message }) self.ACLog

    let task =
      FSApi.PrepareParams.Primitive { InputDirectory = self.InputDirectory
                                      OutputDirectory = self.OutputDirectory
                                      SymbolDirectories = self.SymbolDirectories
#if NETCOREAPP2_0
                                      Dependencies = self.Dependencies
                                      Keys = []
                                      StrongNameKey = String.Empty
#else
                                      Dependencies = []
                                      Keys = self.Keys
                                      StrongNameKey = self.StrongNameKey
#endif
                                      XmlReport = self.XmlReport
                                      FileFilter = self.FileFilter
                                      AssemblyFilter = self.AssemblyFilter
                                      AssemblyExcludeFilter = self.AssemblyExcludeFilter
                                      TypeFilter = self.TypeFilter
                                      MethodFilter = self.MethodFilter
                                      AttributeFilter = self.AttributeFilter
                                      PathFilter = self.PathFilter
                                      CallContext = self.CallContext
                                      OpenCover = self.OpenCover
                                      InPlace = self.InPlace
                                      Save = self.Save
                                      Single = self.Single
                                      LineCover = self.LineCover
                                      BranchCover = self.BranchCover
                                      CommandLine = self.CommandLine
                                      ExposeReturnCode = true
                                      SourceLink = self.SourceLink}

    Api.Prepare task log = 0

type Collect() =
  inherit Task(null)

  member val internal ACLog : FSApi.Logging option = None with get, set

  [<Required>]
  member val RecorderDirectory = String.Empty with get, set

  member val WorkingDirectory = String.Empty with get, set
  member val Executable = String.Empty with get, set
  member val LcovReport = String.Empty with get, set
  member val Threshold = String.Empty with get, set
  member val Cobertura = String.Empty with get, set
  member val OutputFile = String.Empty with get, set
  member val CommandLine : string array = [||] with get, set
  member self.Message x = base.Log.LogMessage(MessageImportance.High, x)
  override self.Execute() =
    let log =
      Option.getOrElse (FSApi.Logging.Primitive { Primitive.Logging.Create() with Error = base.Log.LogError
                                                                                  Warn = base.Log.LogWarning
                                                                                  Info = self.Message }) self.ACLog

    let task =
      FSApi.CollectParams.Primitive { RecorderDirectory = self.RecorderDirectory
                                      WorkingDirectory = self.WorkingDirectory
                                      Executable = self.Executable
                                      LcovReport = self.LcovReport
                                      Threshold = self.Threshold
                                      Cobertura = self.Cobertura
                                      OutputFile = self.OutputFile
                                      CommandLine = self.CommandLine
                                      ExposeReturnCode = true }

    Api.Collect task log = 0

type PowerShell() =
  inherit Task(null)

  member val internal IO = FSApi.Logging.Primitive { Primitive.Logging.Create() with Error = base.Log.LogError
                                                                                     Warn = base.Log.LogWarning } with get, set

  override self.Execute() =
    let r = Api.Ipmo()
    self.IO.Apply()
    r |> Output.Warn
    true

type GetVersion() =
  inherit Task(null)

  member val internal IO = FSApi.Logging.Primitive { Primitive.Logging.Create() with Error = base.Log.LogError
                                                                                     Warn = base.Log.LogWarning } with get, set

  override self.Execute() =
    let r = Api.Version()
    self.IO.Apply()
    r |> Output.Warn
    true