#if RUNNER
// # namespace `AltCover`
// ```
namespace AltCover
// ```
#else
// # namespace `AltCoverFake.DotNet.Testing`
// ```
namespace AltCoverFake.DotNet.Testing
// ```
#endif
// ## module `TypeSafe`
// This holds the strongly-typed equivalent of the command line options
// ```
  [<RequireQualifiedAccess>]
  module TypeSafe = begin
// ```
// ### Individual files and directories
// ```
    [<NoComparison>]
    type FilePath =
      | Tool of System.String (* a name *)
      | FilePath of System.String (* Expanded to an absolute path *)
      | FInfo of System.IO.FileInfo
      | NoFile
      with
        member AsString : unit -> string
      end
    [<NoComparison>]
    type DirectoryPath =
      | DirectoryPath of System.String  (* Expanded to an absolute path *)
      | DInfo of System.IO.DirectoryInfo
      | NoDirectory
      with
        member AsString : unit -> string
      end
// ```
// ### Test application command line
// ```
    [<NoComparison>]
    type CommandArgument =
      | CommandArgument of System.String
      with
        member AsString : unit -> System.String
      end
    [<NoComparison>]
    type Command =
      | Command of seq<CommandArgument>
      | NoCommand
      with
        member AsStrings : unit -> seq<System.String>
      end
// ```
// ### Coverage thresholds
// ```
    [<NoComparison>]
    type Thresholds =
      { Statements: uint8
        Branches: uint8
        Methods: uint8
        MaxCrap: uint8 }
      with
        static member Create : unit -> Thresholds
      end
    [<NoComparison>]
    type Threshold =
      | Threshold of Thresholds
      | NoThreshold
      with
        member AsString : unit -> string
      end
// ```
// ### Yes/No choices
// ```
    [<NoComparison>]
    type Flag =
      | Flag of bool (* as the bool *)
      | Set (* true *)
      | Clear (* false *)
      with
        member AsBool : unit -> bool
      end
// ```
// ### Multiple files and directories
// ```
    [<NoComparison>]
    type FilePaths =
      | FilePaths of seq<FilePath>
      | NoPaths
      with
        member AsStrings : unit -> System.String list
      end
    [<NoComparison>]
    type DirectoryPaths =
      | DirectoryPaths of seq<DirectoryPath>
      | NoDirectories
      with
        member AsStrings : unit -> System.String list
      end
// ```
// ### Selection filters
// ```
    [<NoComparison>]
    type FilterItem =
      | FilterItem of System.Text.RegularExpressions.Regex
      | IncludeItem of System.Text.RegularExpressions.Regex
      | Raw of System.String
      with
        member AsString : unit -> string
      end
    [<NoComparison>]
    type Filters =
      | Filters of seq<FilterItem>
      | Unfiltered
      with
        member AsStrings : unit -> System.String list
      end
// ```
// ### Call tracking
// ```
    [<NoComparison>]
    type ContextItem =
      | CallItem of System.String
      | TimeItem of uint8
      with
        member AsString : unit -> System.String
      end
    [<NoComparison>]
    type Context =
      | Context of seq<ContextItem>
      | NoContext
      with
        member AsStrings : unit -> System.String list
      end
// ```
// ### Display formats
// ```
    [<NoComparison>]
    type SummaryFormat =
      | Default (* OpenCover style *)
      | R (* TeamCity bRanch only *)
      | B (* TeamCity Block only *)
      | RPlus (* R + default *)
      | BPlus (* B + default *)
      with
        member AsString : unit -> string
      end
    [<NoComparison>]
    type StaticFormat =
      | Default (* Don't include *)
      | Show (* include with negative visit count if unvisited *)
      | ShowZero (* include with zero visit count if unvisited *)
      with
        member AsString : unit -> string
      end
    [<NoComparison>]
    type ReportFormat =
      | NCover
      | OpenCover
      with
        member AsString : unit -> string
      end
// ```
// ### type `CollectOptions`
// ```
    [<NoComparison>]
    type CollectOptions =
      { RecorderDirectory: DirectoryPath
        WorkingDirectory: DirectoryPath
        Executable: FilePath
        LcovReport: FilePath
        Threshold: Threshold
        Cobertura: FilePath
        OutputFile: FilePath
        CommandLine: Command
        ExposeReturnCode: Flag
        SummaryFormat: SummaryFormat }
      with
        static member Create : unit -> CollectOptions
      end
// ```
// `Create()` returns an instance with all values unset/default except `ExposeReturnCode`, which is `Set.
//
// Fields that are not applicable to the use case or platform are silently ignored.
//
// ### type `PrepareOptions`
// ```
    [<NoComparison>]
    type PrepareOptions =
      { InputDirectories: DirectoryPaths
        OutputDirectories: DirectoryPaths
        SymbolDirectories: DirectoryPaths
        Dependencies: FilePaths
        Keys: FilePaths
        StrongNameKey: FilePath
        XmlReport: FilePath
        FileFilter: Filters
        AssemblyFilter: Filters
        AssemblyExcludeFilter: Filters
        TypeFilter: Filters
        MethodFilter: Filters
        AttributeFilter: Filters
        PathFilter: Filters
        CallContext: Context
        ReportFormat: ReportFormat
        InPlace: Flag
        Save: Flag
        ZipFile: Flag
        MethodPoint: Flag
        SingleVisit: Flag
        LineCover: Flag
        BranchCover: Flag
        CommandLine: Command
        ExposeReturnCode: Flag
        SourceLink: Flag
        Defer: Flag
        LocalSource: Flag
        VisibleBranches: Flag
        ShowStatic: StaticFormat
        ShowGenerated: Flag }
      with
        static member Create : unit -> PrepareOptions
      end
// ```
// `Create()` returns an instance that has all fields unset/default except `ExposeReturnCode`, `OpenCover`, `InPlace` and `Save` are `Set`
//
// Fields that are not applicable to the use case or platform are silently ignored.
//
// ```
  end
 // ```