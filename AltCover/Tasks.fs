namespace AltCover

open System
open System.Linq
open AltCover.Augment
open System.Diagnostics.CodeAnalysis
open Microsoft.Build.Utilities
open Microsoft.Build.Framework

[<ExcludeFromCodeCoverage>]
type CollectParams =
  { RecorderDirectory : String
    WorkingDirectory : String
    Executable : String
    LcovReport : String
    Threshold : String
    Cobertura : String
    OutputFile : String
    CommandLine : String }

  static member Default : CollectParams =
    { RecorderDirectory = String.Empty
      WorkingDirectory = String.Empty
      Executable = String.Empty
      LcovReport = String.Empty
      Threshold = String.Empty
      Cobertura = String.Empty
      OutputFile = String.Empty
      CommandLine = String.Empty }

  member self.Validate afterPreparation =
    let saved = CommandLine.error

    let validate f x =
      if x
         |> String.IsNullOrWhiteSpace
         |> not
      then f x |> ignore

    let validateOptional f key x = validate (f key) x

    let toOption s =
      if s |> String.IsNullOrWhiteSpace then None
      else Some s
    try
      [ ("--recorderDirectory", self.RecorderDirectory)
        ("--workingDirectory", self.WorkingDirectory) ]
      |> List.iter (fun (n, x) -> validateOptional CommandLine.ValidateDirectory n x)
      [ ("--executable", self.Executable)
        ("--lcovReport", self.LcovReport)
        ("--cobertura", self.Cobertura)
        ("--outputFile", self.OutputFile) ]
      |> List.iter (fun (n, x) -> validateOptional CommandLine.ValidatePath n x)
      validate Runner.ValidateThreshold self.Threshold
      if afterPreparation then
        Runner.RequireRecorderTest (self.RecorderDirectory |> toOption) () ()
      CommandLine.error |> List.toArray
    finally
      CommandLine.error <- saved

[<ExcludeFromCodeCoverage>]
type PrepareParams =
  { InputDirectory : String
    OutputDirectory : String
    SymbolDirectories : string array
    Dependencies : string array
    Keys : string array
    StrongNameKey : String
    XmlReport : String
    FileFilter : string array
    AssemblyFilter : string array
    AssemblyExcludeFilter : string array
    TypeFilter : string array
    MethodFilter : string array
    AttributeFilter : string array
    PathFilter : string array
    CallContext : string array
    OpenCover : bool
    InPlace : bool
    Save : bool
    Single : bool
    LineCover : bool
    BranchCover : bool
    CommandLine : String }

  static member Default : PrepareParams =
    { InputDirectory = String.Empty
      OutputDirectory = String.Empty
      SymbolDirectories = [||]
      Dependencies = [||]
      Keys = [||]
      StrongNameKey = String.Empty
      XmlReport = String.Empty
      FileFilter = [||]
      AssemblyFilter = [||]
      AssemblyExcludeFilter = [||]
      TypeFilter = [||]
      MethodFilter = [||]
      AttributeFilter = [||]
      PathFilter = [||]
      CallContext = [||]
      OpenCover = true
      InPlace = true
      Save = true
      Single = false
      LineCover = false
      BranchCover = false
      CommandLine = String.Empty }

  static member private validateArray a f key =
    PrepareParams.validateArraySimple a (f key)

  static member private nonNull a =
    a
    |> isNull
    |> not

  static member private validateArraySimple a f =
    if a |> PrepareParams.nonNull then a |> Array.iter (fun s -> f s |> ignore)

  static member private validateOptional f key x =
    if x
       |> String.IsNullOrWhiteSpace
       |> not
    then f key x |> ignore

  member private self.consistent() =
    if self.Single && self.CallContext |> PrepareParams.nonNull && self.CallContext.Any() then
      CommandLine.error <- String.Format
                             (System.Globalization.CultureInfo.CurrentCulture,
                              CommandLine.resources.GetString "Incompatible", "--single",
                              "--callContext") :: CommandLine.error

  member private self.consistent'() =
    if self.LineCover && self.BranchCover then
      CommandLine.error <- String.Format
                             (System.Globalization.CultureInfo.CurrentCulture,
                              CommandLine.resources.GetString "Incompatible",
                              "--branchcover", "--linecover") :: CommandLine.error

  member self.Validate() =
    let saved = CommandLine.error

    let validateContext context =
      if context
         |> isNull
         |> not
      then
        let select state x =
          let (_, n) = Main.ValidateCallContext state x
          match (state, n) with
          | (true, _) | (_, Left(Some _)) -> true
          | _ -> false
        context
        |> Array.fold select false
        |> ignore
    try
      CommandLine.error <- []
      PrepareParams.validateOptional CommandLine.ValidateDirectory "--inputDirectory"
        self.InputDirectory
      PrepareParams.validateOptional CommandLine.ValidatePath "--outputDirectory"
        self.OutputDirectory
      PrepareParams.validateOptional CommandLine.ValidateStrongNameKey "--strongNameKey"
        self.StrongNameKey
      PrepareParams.validateOptional CommandLine.ValidatePath "--xmlReport" self.XmlReport
      PrepareParams.validateArray self.SymbolDirectories CommandLine.ValidateDirectory
        "--symbolDirectory"
      PrepareParams.validateArray self.Dependencies CommandLine.ValidateAssembly
        "--dependency"
      PrepareParams.validateArray self.Keys CommandLine.ValidateStrongNameKey "--key"
      [ self.FileFilter; self.AssemblyFilter; self.AssemblyExcludeFilter; self.TypeFilter;
        self.MethodFilter; self.AttributeFilter; self.PathFilter ]
      |> Seq.iter
           (fun a -> PrepareParams.validateArraySimple a CommandLine.ValidateRegexes)
      self.consistent()
      self.consistent'()
      validateContext self.CallContext
      CommandLine.error |> List.toArray
    finally
      CommandLine.error <- saved

[<ExcludeFromCodeCoverage; NoComparison; NoEquality>]
type Logging =
  { Info : String -> unit
    Warn : String -> unit
    Error : String -> unit
    Echo : String -> unit }

  static member Default : Logging =
    { Info = ignore
      Warn = ignore
      Error = ignore
      Echo = ignore }

  static member ActionAdapter(a : Action<String>) =
    match a with
    | null -> ignore
    | _ -> a.Invoke

  member internal self.Apply() =
    Output.Error <- self.Error
    Output.Warn <- self.Warn
    Output.Info <- self.Info
    Output.Echo <- self.Echo

module internal Args =
  let Item a x =
    if x |> String.IsNullOrWhiteSpace then []
    else [ a; x ]

  let ItemList a x =
    if x |> isNull then []
    else
      x
      |> Seq.collect (fun i -> [ a; i ])
      |> Seq.toList

  let Flag a x =
    if x then [ a ]
    else []

module Api =
  let Prepare (args : PrepareParams) (log : Logging) =
    log.Apply()
    [ Args.Item "-i" args.InputDirectory
      Args.Item "-o" args.OutputDirectory
      Args.ItemList "-y" args.SymbolDirectories
#if NETCOREAPP2_0
      Args.ItemList "-d" args.Dependencies
#else
      Args.ItemList "-k" args.Keys
      Args.Item "--sn" args.StrongNameKey
#endif
      Args.Item "-x" args.XmlReport
      Args.ItemList "-f" args.FileFilter
      Args.ItemList "-s" args.AssemblyFilter
      Args.ItemList "-e" args.AssemblyExcludeFilter
      Args.ItemList "-t" args.TypeFilter
      Args.ItemList "-m" args.MethodFilter
      Args.ItemList "-a" args.AttributeFilter
      Args.ItemList "-p" args.PathFilter
      Args.ItemList "-c" args.CallContext
      Args.Flag "--opencover" args.OpenCover
      Args.Flag "--inplace" args.InPlace
      Args.Flag "--save" args.Save
      Args.Flag "--single" args.Single
      Args.Flag "--linecover" args.LineCover
      Args.Flag "--branchcover" args.BranchCover
      Args.Item "--" args.CommandLine ]
    |> List.concat
    |> List.toArray
    |> AltCover.Main.EffectiveMain

  let Collect (args : CollectParams) (log : Logging) =
    log.Apply()
    [ [ "Runner" ]
      Args.Item "-r" args.RecorderDirectory
      Args.Item "-w" args.WorkingDirectory
      Args.Item "-x" args.Executable
      Args.Item "-l" args.LcovReport
      Args.Item "-t" args.Threshold
      Args.Item "-c" args.Cobertura
      Args.Item "-o" args.OutputFile
      Args.Flag "--collect" (args.Executable |> String.IsNullOrWhiteSpace)
      Args.Item "--" args.CommandLine ]
    |> List.concat
    |> List.toArray
    |> AltCover.Main.EffectiveMain

  let mutable internal store = String.Empty
  let private writeToStore s = store <- s
  let internal LogToStore = { Logging.Default with Info = writeToStore }

  let internal GetStringValue s =
    writeToStore String.Empty
    LogToStore.Apply()
    [| s |]
    |> AltCover.Main.EffectiveMain
    |> ignore
    store

  let Ipmo() = GetStringValue "ipmo"
  let Version() = GetStringValue "version"

type Prepare() =
  inherit Task(null)
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
  member val CommandLine = String.Empty with get, set
  member self.Message x = base.Log.LogMessage(MessageImportance.High, x)
  override self.Execute() =
    let log =
      { Logging.Default with Error = base.Log.LogError
                             Warn = base.Log.LogWarning
                             Info = self.Message }

    let task =
      { PrepareParams.Default with InputDirectory = self.InputDirectory
                                   OutputDirectory = self.OutputDirectory
                                   SymbolDirectories = self.SymbolDirectories
#if NETCOREAPP2_0
                                   Dependencies = self.Dependencies
#else
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
                                   CommandLine = self.CommandLine }

    Api.Prepare task log = 0

type Collect() =
  inherit Task(null)

  [<Required>]
  member val RecorderDirectory = String.Empty with get, set

  member val WorkingDirectory = String.Empty with get, set
  member val Executable = String.Empty with get, set
  member val LcovReport = String.Empty with get, set
  member val Threshold = String.Empty with get, set
  member val Cobertura = String.Empty with get, set
  member val OutputFile = String.Empty with get, set
  member val CommandLine = String.Empty with get, set
  member self.Message x = base.Log.LogMessage(MessageImportance.High, x)
  override self.Execute() =
    let log =
      { Logging.Default with Error = base.Log.LogError
                             Warn = base.Log.LogWarning
                             Info = self.Message }

    let task =
      { CollectParams.Default with RecorderDirectory = self.RecorderDirectory
                                   WorkingDirectory = self.WorkingDirectory
                                   Executable = self.Executable
                                   LcovReport = self.LcovReport
                                   Threshold = self.Threshold
                                   Cobertura = self.Cobertura
                                   OutputFile = self.OutputFile
                                   CommandLine = self.CommandLine }

    Api.Collect task log = 0

type PowerShell() =
  inherit Task(null)

  member val internal IO = { Logging.Default with Error = base.Log.LogError
                                                  Warn = base.Log.LogWarning } with get, set

  override self.Execute() =
    let r = Api.Ipmo()
    self.IO.Apply()
    r |> Output.Warn
    true

type GetVersion() =
  inherit Task(null)

  member val internal IO = { Logging.Default with Error = base.Log.LogError
                                                  Warn = base.Log.LogWarning } with get, set

  override self.Execute() =
    let r = Api.Version()
    self.IO.Apply()
    r |> Output.Warn
    true