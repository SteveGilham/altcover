#if RUNNER
namespace AltCover
#else
[<RequireQualifiedAccess>]
module AltCover_Fake.DotNet.Testing.AltCover
#endif

open System
open System.Diagnostics.CodeAnalysis
open System.Globalization
open System.IO
open System.Linq
open System.Text.RegularExpressions
open BlackFox.CommandLine
#if RUNNER
open AltCover.Augment
#else
open System.Reflection
open System.IO
open Fake.Core
#endif

// No more primitive obsession!
[<ExcludeFromCodeCoverage; NoComparison>]
type FilePath =
  | FilePath of String
  | Info of FileInfo
  | NoFile
  member self.AsString() =
    match self with
    | NoFile -> String.Empty
    | Info i -> i.FullName
    | FilePath s -> s

[<ExcludeFromCodeCoverage; NoComparison>]
type DirectoryPath =
  | DirectoryPath of String
  | Info of DirectoryInfo
  | NoDirectory
  member self.AsString() =
    match self with
    | NoDirectory -> String.Empty
    | Info i -> i.FullName
    | DirectoryPath s -> s

[<ExcludeFromCodeCoverage; NoComparison>]
type CommandArgument =
  | CommandArgument of String
  member self.AsString() =
    match self with
    | CommandArgument s -> s

[<ExcludeFromCodeCoverage; NoComparison>]
type Command =
  | Command of CommandArgument seq
  | NoCommand
  member self.AsStrings() =
    match self with
    | NoCommand -> Seq.empty<String>
    | Command c -> c |> Seq.map (fun a -> a.AsString())

[<ExcludeFromCodeCoverage; NoComparison>]
type Threshold =
  | Threshold of uint8
  | NoThreshold
  member self.AsString() =
    match self with
    | NoThreshold -> String.Empty
    | Threshold t -> t.ToString(CultureInfo.InvariantCulture)

[<ExcludeFromCodeCoverage; NoComparison>]
type Flag =
  | Flag of bool
  | Set
  | Clear
  member self.AsBool() =
    match self with
    | Set -> true
    | Clear -> false
    | Flag b -> b

[<ExcludeFromCodeCoverage; NoComparison>]
type FilePaths =
  | FilePaths of FilePath seq
  | NoPaths
  member self.AsStrings() =
    match self with
    | NoPaths -> List.empty<String>
    | FilePaths c ->
      c
      |> Seq.map (fun a -> a.AsString())
      |> Seq.toList

[<ExcludeFromCodeCoverage; NoComparison>]
type DirectoryPaths =
  | DirectoryPaths of DirectoryPath seq
  | NoDirectories
  member self.AsStrings() =
    match self with
    | NoDirectories -> List.empty<String>
    | DirectoryPaths c ->
      c
      |> Seq.map (fun a -> a.AsString())
      |> Seq.toList

[<ExcludeFromCodeCoverage; NoComparison>]
type FilterItem =
  | FilterItem of Regex
  | Raw of String
  member self.AsString() =
    match self with
    | FilterItem r -> r.ToString()
    | Raw r -> r

[<ExcludeFromCodeCoverage; NoComparison>]
type Filters =
  | Filters of FilterItem seq
  | Unfiltered
  member self.AsStrings() =
    match self with
    | Unfiltered -> List.empty<String>
    | Filters c ->
      c
      |> Seq.map (fun a -> a.AsString())
      |> Seq.toList

[<ExcludeFromCodeCoverage; NoComparison>]
type ContextItem =
  | CallItem of String
  | TimeItem of uint8
  member self.AsString() =
    match self with
    | CallItem c -> c
    | TimeItem t -> t.ToString(CultureInfo.InvariantCulture)

[<ExcludeFromCodeCoverage; NoComparison>]
type Context =
  | Context of ContextItem seq
  | NoContext
  member self.AsStrings() =
    match self with
    | NoContext -> List.empty<String>
    | Context c ->
      c
      |> Seq.map (fun a -> a.AsString())
      |> Seq.toList

// --------------------------------------------------------
[<ExcludeFromCodeCoverage; NoComparison>]
type PrimitiveCollectParams =
  { RecorderDirectory : String
    WorkingDirectory : String
    Executable : String
    LcovReport : String
    Threshold : String
    Cobertura : String
    OutputFile : String
    CommandLine : String seq }
  static member Create() =
    { RecorderDirectory = String.Empty
      WorkingDirectory = String.Empty
      Executable = String.Empty
      LcovReport = String.Empty
      Threshold = String.Empty
      Cobertura = String.Empty
      OutputFile = String.Empty
      CommandLine = [] }

[<ExcludeFromCodeCoverage; NoComparison>]
type TypeSafeCollectParams =
  { RecorderDirectory : DirectoryPath
    WorkingDirectory : DirectoryPath
    Executable : FilePath
    LcovReport : FilePath
    Threshold : Threshold
    Cobertura : FilePath
    OutputFile : FilePath
    CommandLine : Command }
  static member Create() =
    { RecorderDirectory = NoDirectory
      WorkingDirectory = NoDirectory
      Executable = NoFile
      LcovReport = NoFile
      Threshold = NoThreshold
      Cobertura = NoFile
      OutputFile = NoFile
      CommandLine = NoCommand }

[<ExcludeFromCodeCoverage; NoComparison>]
type CollectParams =
  | Primitive of PrimitiveCollectParams
  | TypeSafe of TypeSafeCollectParams

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
type PrimitivePrepareParams =
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
    CommandLine : String seq }
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
      CommandLine = [] }

[<ExcludeFromCodeCoverage; NoComparison>]
type TypeSafePrepareParams =
  { InputDirectory : DirectoryPath
    OutputDirectory : DirectoryPath
    SymbolDirectories : DirectoryPaths
    Dependencies : FilePaths
    Keys : FilePaths
    StrongNameKey : FilePath
    XmlReport : FilePath
    FileFilter : Filters
    AssemblyFilter : Filters
    AssemblyExcludeFilter : Filters
    TypeFilter : Filters
    MethodFilter : Filters
    AttributeFilter : Filters
    PathFilter : Filters
    CallContext : Context
    OpenCover : Flag
    InPlace : Flag
    Save : Flag
    Single : Flag
    LineCover : Flag
    BranchCover : Flag
    CommandLine : Command }
  static member Create() =
    { InputDirectory = NoDirectory
      OutputDirectory = NoDirectory
      SymbolDirectories = NoDirectories
      Dependencies = NoPaths
      Keys = NoPaths
      StrongNameKey = NoFile
      XmlReport = NoFile
      FileFilter = Unfiltered
      AssemblyFilter = Unfiltered
      AssemblyExcludeFilter = Unfiltered
      TypeFilter = Unfiltered
      MethodFilter = Unfiltered
      AttributeFilter = Unfiltered
      PathFilter = Unfiltered
      CallContext = NoContext
      OpenCover = Set
      InPlace = Set
      Save = Set
      Single = Clear
      LineCover = Clear
      BranchCover = Clear
      CommandLine = NoCommand }

[<ExcludeFromCodeCoverage; NoComparison>]
type PrepareParams =
  | Primitive of PrimitivePrepareParams
  | TypeSafe of TypeSafePrepareParams

  static member private ToSeq(s : String seq) =
    match s with
    | null -> Seq.empty<string>
    | _ -> s

  static member private ToList(s : String seq) =
    s
    |> PrepareParams.ToSeq
    |> Seq.toList

  member self.InputDirectory =
    match self with
    | Primitive p -> p.InputDirectory
    | TypeSafe t -> t.InputDirectory.AsString()

  member self.OutputDirectory =
    match self with
    | Primitive p -> p.OutputDirectory
    | TypeSafe t -> t.OutputDirectory.AsString()

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
[<ExcludeFromCodeCoverage; NoComparison>]
type CoverageEnvironment = Framework | Dotnet
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

  let Prepare
#if RUNNER
#else
      (e : CoverageEnvironment)
#endif
      (args : PrepareParams) =
    let argsList = args.CommandLine |> Seq.toList

    let trailing =
      if List.isEmpty argsList then []
      else "--" :: argsList

    [ Item "-i" args.InputDirectory
      Item "-o" args.OutputDirectory
      ItemList "-y" args.SymbolDirectories
      ItemList "-d"
#if RUNNER
                    args.Dependencies
#else
                    (if e = Framework then [] else args.Dependencies)
#endif

      ItemList "-k"
#if RUNNER
                    args.Keys
#else
                    (if e = Dotnet then [] else args.Keys)
#endif

      Item "--sn"
#if RUNNER
                    args.StrongNameKey
#else
                    (if e = Dotnet then String.Empty else args.StrongNameKey)
#endif
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