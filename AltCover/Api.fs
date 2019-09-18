#if RUNNER
[<RequireQualifiedAccess>]
module AltCover.FSApi
#else
[<RequireQualifiedAccess>]
module AltCover_Fake.DotNet.Testing.AltCover
#endif

open System
open System.Diagnostics.CodeAnalysis
open System.Linq
#if RUNNER
open AltCover
open AltCover.Augment
#else
open AltCover_Fake.DotNet.Testing
open Fake.Core
#endif

[<ExcludeFromCodeCoverage; NoComparison>]
type CollectParams =
  | Primitive of Primitive.CollectParams
  | TypeSafe of TypeSafe.CollectParams

  static member private ToSeq(s : String seq) =
    match s with
    | null -> Seq.empty<string>
    | _ -> s

  member self.RecorderDirectory =
    match self with
    | Primitive p -> p.RecorderDirectory
    | TypeSafe t -> t.RecorderDirectory.AsString()

  member self.WorkingDirectory =
    match self with
    | Primitive p -> p.WorkingDirectory
    | TypeSafe t -> t.WorkingDirectory.AsString()

  member self.Executable =
    match self with
    | Primitive p -> p.Executable
    | TypeSafe t -> t.Executable.AsString()

  member self.LcovReport =
    match self with
    | Primitive p -> p.LcovReport
    | TypeSafe t -> t.LcovReport.AsString()

  member self.Threshold =
    match self with
    | Primitive p -> p.Threshold
    | TypeSafe t -> t.Threshold.AsString()

  member self.Cobertura =
    match self with
    | Primitive p -> p.Cobertura
    | TypeSafe t -> t.Cobertura.AsString()

  member self.OutputFile =
    match self with
    | Primitive p -> p.OutputFile
    | TypeSafe t -> t.OutputFile.AsString()

  member self.CommandLine =
    match self with
    | Primitive p -> p.CommandLine |> CollectParams.ToSeq
    | TypeSafe t -> t.CommandLine.AsStrings()

  member self.ExposeReturnCode =
    match self with
    | Primitive p -> p.ExposeReturnCode
    | TypeSafe t -> t.ExposeReturnCode.AsBool()

  member self.SummaryFormat =
    match self with
    | Primitive p -> p.SummaryFormat
    | TypeSafe t -> t.SummaryFormat.AsString()

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
      let recorder = self.RecorderDirectory
      [ ("--recorderDirectory", recorder)
        ("--workingDirectory", self.WorkingDirectory) ]
      |> List.iter (fun (n, x) -> validateOptional CommandLine.ValidateDirectory n x)
      [ ("--executable", self.Executable)
        ("--lcovReport", self.LcovReport)
        ("--cobertura", self.Cobertura)
        ("--outputFile", self.OutputFile) ]
      |> List.iter (fun (n, x) -> validateOptional CommandLine.ValidatePath n x)
      validate Runner.ValidateThreshold self.Threshold
      if afterPreparation then
        Runner.RequireRecorderTest (recorder |> toOption) () ()
      CommandLine.error |> List.toArray
    finally
      CommandLine.error <- saved
#else
#endif

[<ExcludeFromCodeCoverage; NoComparison>]
type PrepareParams =
  | Primitive of Primitive.PrepareParams
  | TypeSafe of TypeSafe.PrepareParams

  static member private ToSeq(s : 'a seq) =
    match s with
    | null -> Seq.empty<'a>
    | _ -> s

  static member private ToList(s : 'a seq) =
    s
    |> PrepareParams.ToSeq
    |> Seq.toList

  member self.InputDirectories =
    match self with
    | Primitive p -> p.InputDirectories |> PrepareParams.ToList
    | TypeSafe t -> t.InputDirectories.AsStrings()

  member self.OutputDirectories =
    match self with
    | Primitive p -> p.OutputDirectories |> PrepareParams.ToList
    | TypeSafe t -> t.OutputDirectories.AsStrings()

  member self.SymbolDirectories =
    match self with
    | Primitive p -> p.SymbolDirectories |> PrepareParams.ToList
    | TypeSafe t -> t.SymbolDirectories.AsStrings()

  member self.Dependencies =
    match self with
    | Primitive p -> p.Dependencies |> PrepareParams.ToList
    | TypeSafe t -> t.Dependencies.AsStrings()

  member self.Keys =
    match self with
    | Primitive p -> p.Keys |> PrepareParams.ToList
    | TypeSafe t -> t.Keys.AsStrings()

  member self.StrongNameKey =
    match self with
    | Primitive p -> p.StrongNameKey
    | TypeSafe t -> t.StrongNameKey.AsString()

  member self.XmlReport =
    match self with
    | Primitive p -> p.XmlReport
    | TypeSafe t -> t.XmlReport.AsString()

  member self.FileFilter =
    match self with
    | Primitive p -> p.FileFilter |> PrepareParams.ToList
    | TypeSafe t -> t.FileFilter.AsStrings()

  member self.AssemblyFilter =
    match self with
    | Primitive p -> p.AssemblyFilter |> PrepareParams.ToList
    | TypeSafe t -> t.AssemblyFilter.AsStrings()

  member self.AssemblyExcludeFilter =
    match self with
    | Primitive p -> p.AssemblyExcludeFilter |> PrepareParams.ToList
    | TypeSafe t -> t.AssemblyExcludeFilter.AsStrings()

  member self.TypeFilter =
    match self with
    | Primitive p -> p.TypeFilter |> PrepareParams.ToList
    | TypeSafe t -> t.TypeFilter.AsStrings()

  member self.MethodFilter =
    match self with
    | Primitive p -> p.MethodFilter |> PrepareParams.ToList
    | TypeSafe t -> t.MethodFilter.AsStrings()

  member self.AttributeFilter =
    match self with
    | Primitive p -> p.AttributeFilter |> PrepareParams.ToList
    | TypeSafe t -> t.AttributeFilter.AsStrings()

  member self.PathFilter =
    match self with
    | Primitive p -> p.PathFilter |> PrepareParams.ToList
    | TypeSafe t -> t.PathFilter.AsStrings()

  member self.CallContext =
    match self with
    | Primitive p -> p.CallContext |> PrepareParams.ToList
    | TypeSafe t -> t.CallContext.AsStrings()

  member self.OpenCover =
    match self with
    | Primitive p -> p.OpenCover
    | TypeSafe t -> t.OpenCover.AsBool()

  member self.InPlace =
    match self with
    | Primitive p -> p.InPlace
    | TypeSafe t -> t.InPlace.AsBool()

  member self.Save =
    match self with
    | Primitive p -> p.Save
    | TypeSafe t -> t.Save.AsBool()

  member self.Single =
    match self with
    | Primitive p -> p.Single
    | TypeSafe t -> t.Single.AsBool()

  member self.LineCover =
    match self with
    | Primitive p -> p.LineCover
    | TypeSafe t -> t.LineCover.AsBool()

  member self.BranchCover =
    match self with
    | Primitive p -> p.BranchCover
    | TypeSafe t -> t.BranchCover.AsBool()

  member self.CommandLine =
    match self with
    | Primitive p -> p.CommandLine |> PrepareParams.ToSeq
    | TypeSafe t -> t.CommandLine.AsStrings()

  member self.ExposeReturnCode =
    match self with
    | Primitive p -> p.ExposeReturnCode
    | TypeSafe t -> t.ExposeReturnCode.AsBool()

  member self.SourceLink =
    match self with
    | Primitive p -> p.SourceLink
    | TypeSafe t -> t.SourceLink.AsBool()

  member self.Defer =
    match self with
    | Primitive p -> p.Defer
    | TypeSafe t -> t.Defer.AsBool()

  member self.LocalSource =
    match self with
    | Primitive p -> p.LocalSource
    | TypeSafe t -> t.LocalSource.AsBool()

  member self.VisibleBranches =
    match self with
    | Primitive p -> p.VisibleBranches
    | TypeSafe t -> t.VisibleBranches.AsBool()

#if RUNNER
  static member private validateArray a f key =
    PrepareParams.validateArraySimple a (f key)

  static member private validateArraySimple a f =
    a |> Seq.iter (fun s -> f s |> ignore)

  static member private validateOptional f key x =
    if x
       |> String.IsNullOrWhiteSpace
       |> not
    then f key x |> ignore

  member private self.consistent() =
    if self.Single && self.CallContext.Any() then
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
      let select state x =
        let (_, n) = Main.ValidateCallContext state x
        match (state, n) with
        | (true, _) | (_, Left(Some _)) -> true
        | _ -> false
      context
      |> PrepareParams.ToSeq
      |> Seq.fold select false
      |> ignore

    try
      CommandLine.error <- []
      PrepareParams.validateArray self.InputDirectories CommandLine.ValidateDirectory
        "--inputDirectory"
      PrepareParams.validateArray self.OutputDirectories CommandLine.ValidatePath
        "--outputDirectory"
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
  | Primitive of Primitive.Logging

  static member Create() =
    Primitive.Logging.Create() |> Primitive

  static member ActionAdapter(a : Action<String>) =
    match a with
    | null -> ignore
    | _ -> a.Invoke

  member self.Error =
    match self with
    | Primitive p -> p.Error

  member self.Warn =
    match self with
    | Primitive p -> p.Warn

  member self.Echo =
    match self with
    | Primitive p -> p.Echo

  member self.Info =
    match self with
    | Primitive p -> p.Info

  member internal self.Apply() =
    Output.Error <- self.Error
    Output.Warn <- self.Warn
    Output.Info <- self.Info
    Output.Echo <- self.Echo
#else
[<ExcludeFromCodeCoverage; NoComparison>]
type CoverageEnvironment = Framework | Dotnet
#endif

module internal Args =
  let private Item a x =
    if x |> String.IsNullOrWhiteSpace then []
    else [ a; x ]

  let private OptItem a x =
    if x |> String.IsNullOrWhiteSpace then []
    else [ a + ":" + x ]

  let internal ItemList a x =
    if x |> isNull then []
    else
      x
      |> Seq.collect (fun i -> [ a; i ])
      |> Seq.toList

  let private Flag a x =
    if x then [ a ]
    else []

  let Prepare
#if RUNNER
#else
      (_ : CoverageEnvironment)
#endif
      (args : PrepareParams) =
    let argsList = args.CommandLine |> Seq.toList

    let trailing =
      if List.isEmpty argsList then []
      else "--" :: argsList

    [ ItemList "-i" args.InputDirectories
      ItemList "-o" args.OutputDirectories
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
      Flag "--dropReturnCode" (args.ExposeReturnCode |> not)
      Flag "--sourcelink" args.SourceLink
      Flag "--defer" args.Defer
      Flag "--localSource" args.LocalSource
      Flag "--visibleBranches" args.VisibleBranches
      trailing ]
    |> List.concat

  let Collect(args : CollectParams) =
    let argsList = args.CommandLine |> Seq.toList

    let trailing =
      if List.isEmpty argsList then []
      else "--" :: argsList

    let exe = args.Executable

    [ [ "Runner" ]
      Item "-r" args.RecorderDirectory
      Item "-w" args.WorkingDirectory
      Item "-x" exe
      Item "-l" args.LcovReport
      Item "-t" args.Threshold
      Item "-c" args.Cobertura
      Item "-o" args.OutputFile
      Flag "--collect" (exe |> String.IsNullOrWhiteSpace)
      Flag "--dropReturnCode" (args.ExposeReturnCode |> not)
      OptItem "--teamcity" args.SummaryFormat
      trailing ]
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
     p
     |> Args.Prepare (match parameters.ToolType with
                      | Framework
                      | Mono _ -> CoverageEnvironment.Framework
                      | _ -> CoverageEnvironment.Dotnet)
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