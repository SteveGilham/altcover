#if RUNNER
namespace AltCover
#else
[<RequireQualifiedAccess>]
module AltCover_Fake.DotNet.Testing.AltCover
#endif

open System
open System.Diagnostics.CodeAnalysis
open System.Linq
#if RUNNER
open AltCover.Augment
#else
open System.Reflection
open System.IO
open Fake.Core
#endif

[<ExcludeFromCodeCoverage; NoComparison>]
type CollectParams =
  { RecorderDirectory : String
    WorkingDirectory : String
    Executable : String
    LcovReport : String
    Threshold : String
    Cobertura : String
    OutputFile : String
    CommandLine : String }

#if RUNNER
  [<Obsolete("Please AltCover.CollectParams.Create() instead instead.")>]
  static member Default : CollectParams =
    { RecorderDirectory = String.Empty
      WorkingDirectory = String.Empty
      Executable = String.Empty
      LcovReport = String.Empty
      Threshold = String.Empty
      Cobertura = String.Empty
      OutputFile = String.Empty
      CommandLine = String.Empty }
#endif

  static member Create() =
    { RecorderDirectory = String.Empty
      WorkingDirectory = String.Empty
      Executable = String.Empty
      LcovReport = String.Empty
      Threshold = String.Empty
      Cobertura = String.Empty
      OutputFile = String.Empty
      CommandLine = String.Empty }

#if RUNNER
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
#else
  member self.withCommandLine args =
    { self with CommandLine =
                  (CreateProcess.fromRawCommand String.Empty
                    args).CommandLine}
#endif

[<ExcludeFromCodeCoverage; NoComparison>]
type PrepareParams =
  { InputDirectory : String
    OutputDirectory : String
    SymbolDirectories : string seq
    Dependencies : string seq
    Keys : string seq
    StrongNameKey : String
    XmlReport : String
    FileFilter : string seq
    AssemblyFilter : string seq
    AssemblyExcludeFilter : string seq
    TypeFilter : string seq
    MethodFilter : string seq
    AttributeFilter : string seq
    PathFilter : string seq
    CallContext : string seq
    OpenCover : bool
    InPlace : bool
    Save : bool
    Single : bool
    LineCover : bool
    BranchCover : bool
    CommandLine : String }

#if RUNNER
  [<Obsolete("Please AltCover.CollectParams.Create() instead instead.")>]
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
#endif

  static member Create() =
    { InputDirectory = String.Empty
      OutputDirectory = String.Empty
      SymbolDirectories = Seq.empty
      Dependencies = Seq.empty
      Keys = Seq.empty
      StrongNameKey = String.Empty
      XmlReport = String.Empty
      FileFilter = Seq.empty
      AssemblyFilter = Seq.empty
      AssemblyExcludeFilter = Seq.empty
      TypeFilter = Seq.empty
      MethodFilter = Seq.empty
      AttributeFilter = Seq.empty
      PathFilter = Seq.empty
      CallContext = Seq.empty
      OpenCover = true
      InPlace = true
      Save = true
      Single = false
      LineCover = false
      BranchCover = false
      CommandLine = String.Empty }

#if RUNNER
  static member private validateArray a f key =
    PrepareParams.validateArraySimple a (f key)

  static member private nonNull a =
    a
    |> isNull
    |> not

  [<SuppressMessage("Microsoft.Usage", "CA2208",
                    Justification = "Some in-lined code must be creating an ArgumentNullException")>]
  static member private validateArraySimple a f =
    if a |> PrepareParams.nonNull then a |> Seq.iter (fun s -> f s |> ignore)

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
        |> Seq.fold select false
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
#else
  member self.withCommandLine args =
    { self with CommandLine =
                  (CreateProcess.fromRawCommand String.Empty
                    args).CommandLine}
#endif

module internal Args =
  let private Item a x =
    if x |> String.IsNullOrWhiteSpace then []
    else [ a; x ]

  let internal ItemList a x =
    if x |> isNull then []
    else
      x
      |> Seq.collect (fun i -> [ a; i ])
      |> Seq.toList

  let private Flag a x =
    if x then [ a ]
    else []

  let Prepare(args : PrepareParams) =
    [ Item "-i" args.InputDirectory
      Item "-o" args.OutputDirectory
      ItemList "-y" args.SymbolDirectories
      ItemList "-d" args.Dependencies
      ItemList "-k" args.Keys
      Item "--sn" args.StrongNameKey
      Item "-x" args.XmlReport
      ItemList "-f" args.FileFilter
      ItemList "-s" args.AssemblyFilter
      ItemList "-e" args.AssemblyExcludeFilter
      ItemList "-t" args.TypeFilter
      ItemList "-m" args.MethodFilter
      ItemList "-a" args.AttributeFilter
      ItemList "-p" args.PathFilter
      ItemList "-c" args.CallContext
      Flag "--opencover" args.OpenCover
      Flag "--inplace" args.InPlace
      Flag "--save" args.Save
      Flag "--single" args.Single
      Flag "--linecover" args.LineCover
      Flag "--branchcover" args.BranchCover
      Item "--" args.CommandLine ]
    |> List.concat

  let Collect(args : CollectParams) =
    [ [ "Runner" ]
      Item "-r" args.RecorderDirectory
      Item "-w" args.WorkingDirectory
      Item "-x" args.Executable
      Item "-l" args.LcovReport
      Item "-t" args.Threshold
      Item "-c" args.Cobertura
      Item "-o" args.OutputFile
      Flag "--collect" (args.Executable |> String.IsNullOrWhiteSpace)
      Item "--" args.CommandLine ]
    |> List.concat

#if RUNNER
#else
[<NoComparison>]
type ArgType =
  | Collect of CollectParams
  | Prepare of PrepareParams
  | ImportModule
  | GetVersion

[<NoComparison>]
type ToolType =
  | DotNet of string option
  | Mono of string option
  | Global
  | Framework

[<NoComparison>]
type Params =
  { /// Path to the Altcover executable.
    ToolPath : string
    /// Which version of the tool
    ToolType : ToolType
    /// Working directory for relative file paths.  Default is the current working directory
    WorkingDirectory : string
    /// Command arguments
    Args : ArgType }

  static member Create (a:ArgType) =
    {
        ToolPath = "altcover"
        ToolType = Global
        WorkingDirectory = String.Empty
        Args = a
    }
let internal createArgs parameters =
  match parameters.Args with
  | Collect c -> Args.Collect c
  | Prepare p ->
    (match parameters.ToolType with
     | Framework
     | Mono _ -> { p with Dependencies = Seq.empty }
     | _ -> { p with Keys = Seq.empty
                     StrongNameKey = String.Empty})
     |> Args.Prepare
  | ImportModule -> [ "ipmo" ]
  | GetVersion -> [ "version" ]

let internal createProcess parameters args =
  let baseline () = CreateProcess.fromRawCommand parameters.ToolPath args
  match parameters.ToolType with
  | Framework -> baseline () |> CreateProcess.withFramework
  | Global -> baseline ()
  | DotNet dotnetPath ->
       let path =
         match dotnetPath with
         | None -> "dotnet"
         | Some p -> p
       CreateProcess.fromRawCommand path (parameters.ToolPath::args)
  | Mono monoPath ->
       let path =
         match monoPath with
         | None -> "mono"
         | Some p -> p
       CreateProcess.fromRawCommand path ("--debug"::parameters.ToolPath::args)
  |> if String.IsNullOrWhiteSpace parameters.WorkingDirectory then id
     else CreateProcess.withWorkingDirectory parameters.WorkingDirectory

let composeCommandLine parameters =
  let args = createArgs parameters
  createProcess parameters args

let run parameters =
  use __ = Trace.traceTask "AltCover" String.Empty
  let command = composeCommandLine parameters
  let run = command |> Proc.run
  if 0 <> run.ExitCode then failwithf "AltCover '%s' failed." command.CommandLine
  __.MarkSuccess()
#endif