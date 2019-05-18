#if RUNNER
[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage; RequireQualifiedAccess>] // work around coverlet attribute bug
module AltCover.TypeSafe
#else
[<RequireQualifiedAccess>]
module AltCover_Fake.DotNet.Testing.TypeSafe
#endif

open System
open System.Diagnostics.CodeAnalysis
open System.Globalization
open System.IO
open System.Text.RegularExpressions

// No more primitive obsession!
[<ExcludeFromCodeCoverage; NoComparison>]
type FilePath =
  | FilePath of String
  | FInfo of FileInfo
  | NoFile
  member self.AsString() =
    match self with
    | NoFile -> String.Empty
    | FInfo i -> i.FullName
    | FilePath s -> s

[<ExcludeFromCodeCoverage; NoComparison>]
type DirectoryPath =
  | DirectoryPath of String
  | DInfo of DirectoryInfo
  | NoDirectory
  member self.AsString() =
    match self with
    | NoDirectory -> String.Empty
    | DInfo i -> i.FullName
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

[<ExcludeFromCodeCoverage; NoComparison>]
type SummaryFormat =
  | Default
  | R
  | B
  | RPlus
  | BPlus
  member self.AsString () =
    match self with
    | Default -> String.Empty
    | B -> "B"
    | R -> "R"
    | BPlus -> "+B"
    | RPlus -> "+R"

[<ExcludeFromCodeCoverage; NoComparison>]
type CollectParams =
  { RecorderDirectory : DirectoryPath
    WorkingDirectory : DirectoryPath
    Executable : FilePath
    LcovReport : FilePath
    Threshold : Threshold
    Cobertura : FilePath
    OutputFile : FilePath
    CommandLine : Command
    ExposeReturnCode : Flag
    SummaryFormat : SummaryFormat
  }
  static member Create() =
    { RecorderDirectory = NoDirectory
      WorkingDirectory = NoDirectory
      Executable = NoFile
      LcovReport = NoFile
      Threshold = NoThreshold
      Cobertura = NoFile
      OutputFile = NoFile
      CommandLine = NoCommand
      ExposeReturnCode = Set
      SummaryFormat = Default
    }

[<ExcludeFromCodeCoverage; NoComparison>]
type PrepareParams =
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
    CommandLine : Command
    ExposeReturnCode : Flag
    SourceLink : Flag
    Defer : Flag
  }
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
      CommandLine = NoCommand
      ExposeReturnCode = Set
      SourceLink = Clear
      Defer = Clear
    }