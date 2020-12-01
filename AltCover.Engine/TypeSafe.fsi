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
///<summary>
/// This represents the strongly-typed equivalent of the command line options, with the
/// strong types expressed as F# discriminated unions.  It is not recommented for consumption
/// from C# code.
///</summary>
// ```
  [<RequireQualifiedAccess>]
  module TypeSafe = begin
// ```
// This holds the strongly-typed equivalent of the command line options
//
// ### Individual files and directories
// ```
    [<NoComparison>]
    ///<summary>
    /// Corresponds to a file or executable tool
    ///</summary>
    type FilePath =
      ///<summary>
      /// A named `dotnet` tool, carried unaltered
      ///</summary>
      | Tool of System.String (* a name *)
      ///<summary>
      /// A file path, expanded to an absolute path is supplied as a relative one
      ///</summary>
      | FilePath of System.String (* Expanded to an absolute path *)
      ///<summary>
      /// A strongly typed file
      ///</summary>
      | FInfo of System.IO.FileInfo
      ///<summary>
      /// Nothing
      ///</summary>
      | NoFile
      with
        ///<summary>
        /// Returns the string to be used in the effective command line
        ///</summary>
        ///<returns>the string to be used in the effective command line</returns>
        member AsString : unit -> string
      end
    ///<summary>
    /// Corresponds to a directory
    ///</summary>
    [<NoComparison>]
    type DirectoryPath =
      ///<summary>
      /// A directory path, expanded to an absolute path is supplied as a relative one
      ///</summary>
      | DirectoryPath of System.String  (* Expanded to an absolute path *)
      ///<summary>
      /// A strongly typed directory
      ///</summary>
      | DInfo of System.IO.DirectoryInfo
      ///<summary>
      /// Nothing
      ///</summary>
      | NoDirectory
      with
        ///<summary>
        /// Returns the string to be used in the effective command line
        ///</summary>
        ///<returns>the string to be used in the effective command line</returns>
        member AsString : unit -> string
      end
// ```
// ### Test application command line
// ```
    ///<summary>
    /// Corresponds to a value after `-- ` on the command line
    ///</summary>
    [<NoComparison>]
    type CommandArgument =
      ///<summary>
      /// Strongly typed string value
      ///</summary>
      | CommandArgument of System.String
      with
        ///<summary>
        /// Returns the string to be used in the effective command line
        ///</summary>
        ///<returns>the string to be used in the effective command line</returns>
        member AsString : unit -> System.String
      end
    ///<summary>
    /// Corresponds to the values after `-- ` on the command line
    ///</summary>
    [<NoComparison>]
    type CommandLine =
      ///<summary>
      /// Strongly typed string collection
      ///</summary>
      | CommandArguments of seq<CommandArgument>
      ///<summary>
      /// Nothing
      ///</summary>
      | NoCommand
      with
        ///<summary>
        /// Returns the strings to be used in the effective command line
        ///</summary>
        ///<returns>the strings to be used in the effective command line</returns>
        member AsStrings : unit -> seq<System.String>
      end
// ```
// ### Coverage thresholds
// ```
    ///<summary>
    /// Corresponds to the set of options available for coverage thresholds; zero values are ignored
    ///</summary>
    [<NoComparison>]
    type Thresholds =
      {
        ///<summary>
        /// Minimum statement (sequence point) coverage %age
        ///</summary>
        Statements: uint8
        ///<summary>
        /// Minimum branch coverage %age
        ///</summary>
        Branches: uint8
        ///<summary>
        /// Minimum method coverage %age
        ///</summary>
        Methods: uint8
        ///<summary>
        /// Maximum acceptable CRAP score
        ///</summary>
        MaxCrap: uint8
        ///<summary>
        /// Minimum method coverage %age including methods with no source
        ///</summary>
        AltMethods : uint8
        ///<summary>
        /// Maximum acceptable CRAP score including methods with no source
        ///</summary>
        AltMaxCrap : uint8
      }
      with
        ///<summary>
        /// Returns an all-zero value
        ///</summary>
        static member Create : unit -> Thresholds
      end
    ///<summary>
    /// Corresponds desired coverage threshold
    ///</summary>
    [<NoComparison>]
    type Threshold =
      ///<summary>
      /// Corresponds to an explicit value
      ///</summary>
      | Threshold of Thresholds
      ///<summary>
      /// Nothing
      ///</summary>
      | NoThreshold
      with
        ///<summary>
        /// Returns the string to be used in the effective command line
        ///</summary>
        ///<returns>the string to be used in the effective command line</returns>
         member AsString : unit -> string
      end
// ```
// ### Yes/No choices
// ```
    [<NoComparison>]
    ///<summary>
    /// Corresponds to a yes/no choice
    ///</summary>
    type Flag =
      ///<summary>
      /// A type-safe boolean
      ///</summary>
      | Flag of bool (* as the bool *)
      ///<summary>
      /// Corresponds to a `true` value
      ///</summary>
      | Set (* true *)
      ///<summary>
      /// Corresponds to a `false` value
      ///</summary>
      | Clear (* false *)
      with
        ///<summary>
        /// Returns the effective boolean value
        ///</summary>
        ///<returns>the effective boolean value</returns>
        member AsBool : unit -> bool
      end
// ```
// ### Multiple files and directories
// ```
    ///<summary>
    /// Corresponds to a collection of files
    ///</summary>
    [<NoComparison>]
    type FilePaths =
      ///<summary>
      /// A type-safe collection
      ///</summary>
      | FilePaths of seq<FilePath>
      ///<summary>
      /// Nothing
      ///</summary>
      | NoPaths
      with
        ///<summary>
        /// Returns the strings to use in the command line
        ///</summary>
        ///<returns>the strings to be used in the effective command line</returns>
        member AsStrings : unit -> System.String list
      end
    ///<summary>
    /// Corresponds to a collection of directories
    ///</summary>
    [<NoComparison>]
    type DirectoryPaths =
      ///<summary>
      /// A type-safe collection
      ///</summary>
      | DirectoryPaths of seq<DirectoryPath>
      ///<summary>
      /// Nothing
      ///</summary>
      | NoDirectories
      with
        ///<summary>
        /// Returns the strings to use in the command line
        ///</summary>
        ///<returns>the strings to be used in the effective command line</returns>
        member AsStrings : unit -> System.String list
      end
// ```
// ### Selection filters
// ```
    ///<summary>
    /// Corresponds to a filter entry
    ///</summary>
    [<NoComparison>]
    type FilterItem =
      ///<summary>
      /// Apply to items that match the regex
      ///</summary>
      | MatchItem of System.Text.RegularExpressions.Regex
      ///<summary>
      /// Apply to items that don't match the regex
      ///</summary>
      | NegateMatchItem of System.Text.RegularExpressions.Regex
      ///<summary>
      /// A string as would be used on the command line (regex with optional leading `'?`)
      ///</summary>
      | Raw of System.String
      with
        ///<summary>
        /// Returns the string as would be used on the command line (regex with optional leading `'?`)
        ///</summary>
        ///<returns>the string to be used in the effective command line</returns>
        member AsString : unit -> string
      end
    ///<summary>
    /// Corresponds to a collection of filter entries
    ///</summary>
    [<NoComparison>]
    type Filters =
      ///<summary>
      /// A type-safe collection
      ///</summary>
      | Filters of seq<FilterItem>
      ///<summary>
      /// Nothing
      ///</summary>
      | Unfiltered
      with
        ///<summary>
        /// Joins a set of filter items to this filter
        ///</summary>
        ///<param name="filters">The filter items to concatenate with</param>
        ///<returns>the combined filter</returns>
        member Join : filters:FilterItem seq -> Filters
        ///<summary>
        /// Returns the strings to use in the command line
        ///</summary>
        ///<returns>the strings to be used in the effective command line</returns>
        member AsStrings : unit -> System.String list
      end
// ```
// ### Call tracking
// ```
    ///<summary>
    /// Corresponds to a context value
    ///</summary>
    [<NoComparison>]
    type ContextItem =
      ///<summary>
      /// A method
      ///</summary>
      | Caller of System.Reflection.MethodInfo
      ///<summary>
      /// A method name
      ///</summary>
      | CallerName of System.String
      ///<summary>
      /// A method-level attribute name
      ///</summary>
      | AttributeName of System.String
      ///<summary>
      /// A method-level attribute type
      ///</summary>
      | AttributeKind of System.Type
      ///<summary>
      /// A call time granularity
      ///</summary>
      | TimeItem of uint8
      with
        ///<summary>
        /// Returns the string to use in the command line
        ///</summary>
        ///<returns>the string to be used in the effective command line</returns>
        member AsString : unit -> System.String
      end
    ///<summary>
    /// Corresponds to a collection of context entries
    ///</summary>
    [<NoComparison>]
    type Context =
      ///<summary>
      /// A type-safe collection
      ///</summary>
      | Context of seq<ContextItem>
      ///<summary>
      /// Nothing
      ///</summary>
      | NoContext
      with
        ///<summary>
        /// Returns the strings to use in the command line
        ///</summary>
        ///<returns>the string to be used in the effective command line</returns>
        member AsStrings : unit -> System.String list
      end
// ```
// ### Display formats
// ```
    ///<summary>
    /// Corresponds to a summary format choice
    ///</summary>
    [<NoComparison; AutoSerializable(false)>]
    type SummaryFormat =
      /// <summary>
      /// <para type="description">OpenCover format with CRAP score, equivalent to Many [O; C] if no other values given </para>
      /// </summary>
      | Default
      /// <summary>
      /// <para type="description">No summary, overriding any other value given</para>
      /// </summary>
      | N
      /// <summary>
      /// <para type="description">OpenCover classic summary only</para>
      /// </summary>
      | O
      /// <summary>
      /// <para type="description">Change Risk Anti-Patterns score only</para>
      /// </summary>
      | C
      /// <summary>
      /// <para type="description">TeamCity with R for bRanch</para>
      /// </summary>
      | R
      /// <summary>
      /// <para type="description">TeamCity with B for Block representing branch coverage</para>
      /// </summary>
      | B
      /// <summary>
      /// <para type="description">OpenCover plus CRAP score plus TeamCity with R for bRanch, equivalent to Many [B; O; C]</para>
      /// </summary>
      | RPlus
      /// <summary>
      /// <para type="description">OpenCover plus CRAP score plus TeamCity with B for Block representing branch coverage, equivalent to Many [R; O; C]</para>
      /// </summary>
      | BPlus
      /// <summary>
      /// <para type="description">Aggregation of the above</para>
      /// </summary>
      | Many of SummaryFormat seq
      with
        ///<summary>
        /// Returns the string to use in the command line
        ///</summary>
        ///<returns>the string to be used in the effective command line</returns>
        member AsString : unit -> string
      end
    ///<summary>
    /// Corresponds to a an option for treating trivial methods
    ///</summary>
    [<NoComparison>]
    type StaticFormat =
      ///<summary>
      /// Don't include
      ///</summary>
      | Default (* Don't include *)
      ///<summary>
      /// include with negative visit count if unvisited
      ///</summary>
      | Show (* include with negative visit count if unvisited *)
      ///<summary>
      /// include with zero visit count if unvisited
      ///</summary>
      | ShowZero (* include with zero visit count if unvisited *)
      with
        ///<summary>
        /// Returns the string to use in the command line
        ///</summary>
        ///<returns>the string to be used in the effective command line</returns>
        member AsString : unit -> string
      end
    ///<summary>
    /// Corresponds to the desired format of the XML report
    ///</summary>
    [<NoComparison>]
    type ReportFormat =
      ///<summary>
      /// NCover 1.5.8 style
      ///</summary>
      | NCover
      ///<summary>
      /// OpenCover style
      ///</summary>
      | OpenCover
      with
        ///<summary>
        /// Returns the string to use in the command line
        ///</summary>
        ///<returns>the string to be used in the effective command line</returns>
        member AsString : unit -> string
      end
// ```
// ### type `CollectOptions`
//
//The members correspond to the like-named command line options for `AltCover Runner`, except
//* `ExposeReturnCode` being the converse of the `dropReturnCode` option
//* `CommandLine` being the material after a `-- `
//
// ```
    ///<summary>
    /// <para>Command line options for `AltCover Runner`</para>
    ///</summary>
    [<NoComparison>]
    type CollectOptions =
      {
        ///<summary>
        /// Corresponds to command line option `-r, --recorderDirectory=VALUE`
        ///</summary>
        RecorderDirectory: DirectoryPath
        ///<summary>
        /// Corresponds to command line option `-w, --workingDirectory=VALUE`
        ///</summary>
        WorkingDirectory: DirectoryPath
        ///<summary>
        /// Corresponds to command line option `-x, --executable=VALUE`
        ///</summary>
        Executable: FilePath
        ///<summary>
        /// Corresponds to command line option `-l, --lcovReport=VALUE`
        ///</summary>
        LcovReport: FilePath
        ///<summary>
        /// Corresponds to command line option `-t, --threshold=VALUE`
        ///</summary>
        Threshold: Threshold
        ///<summary>
        /// Corresponds to command line option `-c, --cobertura=VALUE`
        ///</summary>
        Cobertura: FilePath
        ///<summary>
        /// Corresponds to command line option `-o, --outputFile=VALUE`
        ///</summary>
        OutputFile: FilePath
        ///<summary>
        /// Corresponds to the command line arguments for the executable, given after a `-- `
        ///</summary>
        CommandLine: CommandLine
        ///<summary>
        /// Corresponds to the converse of command line option `--dropReturnCode `
        ///</summary>
        ExposeReturnCode: Flag
        ///<summary>
        /// Corresponds to command line option `--teamcity[=VALUE]`
        ///</summary>
        SummaryFormat: SummaryFormat }
      with
        ///<summary>
        /// Returns an instance with all fields empty save `ExposeReturnCode` being `Set`
        ///</summary>
        ///<returns>a default instance</returns>
        static member Create : unit -> CollectOptions
      end
// ```
// `Create()` returns an instance with all values unset/default except `ExposeReturnCode`, which is `Set.
//
// Fields that are not applicable to the use case or platform are silently ignored.
//
// ### type `PrepareOptions`
//
//The members correspond to the like-named command line options for `AltCover`, except
//* `ExposeReturnCode` being the converse of the `dropReturnCode` option
//* `CommandLine` being the material after a `-- `
//* `SingleVisit` being the name for `--single`
//
// ```
  ///<summary>
  /// <para>Command line options for `AltCover`</para>
  ///</summary>
    [<NoComparison>]
    type PrepareOptions =
      {
        ///<summary>
        /// Corresponds to command line option ` -i, --inputDirectory=VALUE`
        ///</summary>
        InputDirectories: DirectoryPaths
        ///<summary>
        /// Corresponds to command line option `-o, --outputDirectory=VALUE`
        ///</summary>
        OutputDirectories: DirectoryPaths
        ///<summary>
        /// Corresponds to command line option `-y, --symbolDirectory=VALUE`
        ///</summary>
        SymbolDirectories: DirectoryPaths
        ///<summary>
        /// Corresponds to command line option `-d, --dependency=VALUE`
        ///</summary>
        Dependencies: FilePaths
        ///<summary>
        /// Corresponds to command line option ` -k, --key=VALUE`
        ///</summary>
        Keys: FilePaths
        ///<summary>
        /// Corresponds to command line option `--sn, --strongNameKey=VALUE`
        ///</summary>
        StrongNameKey: FilePath
        ///<summary>
        /// Corresponds to command line option `-x, --xmlReport=VALUE`
        ///</summary>
        XmlReport: FilePath
        ///<summary>
        /// Corresponds to command line option `-f, --fileFilter=VALUE`
        ///</summary>
        FileFilter: Filters
        ///<summary>
        /// Corresponds to command line option `-s, --assemblyFilter=VALUE`
        ///</summary>
        AssemblyFilter: Filters
        ///<summary>
        /// Corresponds to command line option `-e, --assemblyExcludeFilter=VALUE`
        ///</summary>
        AssemblyExcludeFilter: Filters
        ///<summary>
        /// Corresponds to command line option `-t, --typeFilter=VALUE`
        ///</summary>
        TypeFilter: Filters
        ///<summary>
        /// Corresponds to command line option ` -m, --methodFilter=VALUE`
        ///</summary>
        MethodFilter: Filters
        ///<summary>
        /// Corresponds to command line option `-a, --attributeFilter=VALUE`
        ///</summary>
        AttributeFilter: Filters
        ///<summary>
        /// Corresponds to command line option `-p, --pathFilter=VALUE`
        ///</summary>
        PathFilter: Filters
        ///<summary>
        /// Corresponds to command line option -`-attributetoplevel=VALUE`
        ///</summary>
        AttributeTopLevel: Filters
        ///<summary>
        /// Corresponds to command line option `--typetoplevel=VALUE`
        ///</summary>
        TypeTopLevel: Filters
        ///<summary>
        /// Corresponds to command line option `--methodtoplevel=VALUE`
        ///</summary>
        MethodTopLevel: Filters
        ///<summary>
        /// Corresponds to command line option `-c, --callContext=VALUE`
        ///</summary>
        CallContext: Context
        ///<summary>
        /// Corresponds to command line option `--reportFormat=VALUE`
        ///</summary>
        ReportFormat: ReportFormat
        ///<summary>
        /// Corresponds to command line option `--inplace`
        ///</summary>
        InPlace: Flag
        ///<summary>
        /// Corresponds to command line option `--save`
        ///</summary>
        Save: Flag
        ///<summary>
        /// Corresponds to command line option `--zipfile`
        ///</summary>
        ZipFile: Flag
        ///<summary>
        /// Corresponds to command line option `--methodpoint`
        ///</summary>
        MethodPoint: Flag
        ///<summary>
        /// Corresponds to command line option `--single`
        ///</summary>
        SingleVisit: Flag
        ///<summary>
        /// Corresponds to command line option `--linecover`
        ///</summary>
        LineCover: Flag
        ///<summary>
        /// Corresponds to command line option `--branchcover`
        ///</summary>
        BranchCover: Flag
        ///<summary>
        /// Corresponds to the command line to run, given after a `-- `
        ///</summary>
        CommandLine: CommandLine
        ///<summary>
        /// Corresponds to the converse of command line option `--dropReturnCode `
        ///</summary>
        ExposeReturnCode: Flag
        ///<summary>
        /// Corresponds to command line option `--sourcelink`
        ///</summary>
        SourceLink: Flag
        ///<summary>
        /// Corresponds to command line option `--defer`
        ///</summary>
        Defer: Flag
        ///<summary>
        /// Corresponds to command line option `-l, --localSource`
        ///</summary>
        LocalSource: Flag
        ///<summary>
        /// Corresponds to command line option ` -v, --visibleBranches`
        ///</summary>
        VisibleBranches: Flag
        ///<summary>
        /// Corresponds to command line option `--showstatic[=VALUE]`
        ///</summary>
        ShowStatic: StaticFormat
        ///<summary>
        /// Corresponds to command line option ` --showGenerated`
        ///</summary>
        ShowGenerated: Flag }
      with
        ///<summary>
        /// returns an instance that has all fields unset/default except `ExposeReturnCode`, `InPlace` and `Save` are `Set`
        ///</summary>
        ///<returns>a default instance</returns>
        static member Create : unit -> PrepareOptions
      end
// ```
// `Create()` returns an instance that has all fields unset/default except `ExposeReturnCode`, `InPlace` and `Save` are `Set`
//
// Fields that are not applicable to the use case or platform are silently ignored.
  end //// no doc