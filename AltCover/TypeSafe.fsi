#if RUNNER
namespace AltCover
#else
namespace AltCoverFake.DotNet.Testing
#endif
  [<RequireQualifiedAccess>]
  module TypeSafe = begin
    [<NoComparison>]
    type FilePath =
      | Tool of System.String
      | FilePath of System.String
      | FInfo of System.IO.FileInfo
      | NoFile
      with
        member AsString : unit -> string
      end
    [<NoComparison>]
    type DirectoryPath =
      | DirectoryPath of System.String
      | DInfo of System.IO.DirectoryInfo
      | NoDirectory
      with
        member AsString : unit -> string
      end
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
    [<NoComparison>]
    type Flag =
      | Flag of bool
      | Set
      | Clear
      with
        member AsBool : unit -> bool
      end
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
    [<NoComparison>]
    type SummaryFormat =
      | Default
      | R
      | B
      | RPlus
      | BPlus
      with
        member AsString : unit -> string
      end
    [<NoComparison>]
    type StaticFormat =
      | Default
      | Show
      | ShowZero
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
    [<NoComparison>]
    type CollectParameters =
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
        static member Create : unit -> CollectParameters
      end
    [<NoComparison>]
    type PrepareParameters =
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
        Single: Flag
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
        static member Create : unit -> PrepareParameters
      end
  end