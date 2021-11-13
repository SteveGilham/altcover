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
// ## module `Primitive`
//
// ```
///<summary>
/// This represents the weakly ("stringly") typed equivalent of the command line options.
/// It is primarily for F# use.  Undocumented methods are compiler generated.
///</summary>
  [<RequireQualifiedAccess>]
  module Primitive = begin
// ```
//
// This holds the weakly ("stringly") typed equivalent of the command line options
//
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
        RecorderDirectory: System.String
        ///<summary>
        /// Corresponds to command line option `-w, --workingDirectory=VALUE`
        ///</summary>
        WorkingDirectory: System.String
        ///<summary>
        /// Corresponds to command line option `-x, --executable=VALUE`
        ///</summary>
        Executable: System.String
        ///<summary>
        /// Corresponds to command line option `-l, --lcovReport=VALUE`
        ///</summary>
        LcovReport: System.String
        ///<summary>
        /// Corresponds to command line option `-t, --threshold=VALUE`
        ///</summary>
        Threshold: System.String
        ///<summary>
        /// Corresponds to command line option `-c, --cobertura=VALUE`
        ///</summary>
        Cobertura: System.String
        ///<summary>
        /// Corresponds to command line option `-o, --outputFile=VALUE`
        ///</summary>
        OutputFile: System.String
        ///<summary>
        /// Corresponds to the command line arguments for the executable, given after a `-- `
        ///</summary>
        CommandLine: seq<System.String>
        ///<summary>
        /// Corresponds to the converse of command line option `--dropReturnCode `
        ///</summary>
        ExposeReturnCode: bool
        ///<summary>
        /// Corresponds to command line option `--teamcity[=VALUE]`
        ///</summary>
        SummaryFormat: System.String
        ///<summary>
        /// Corresponds to command line option ` -q`
        ///</summary>
        Verbosity : System.Diagnostics.TraceLevel
      }
      with
        ///<summary>
        /// Returns an instance with all fields empty save `ExposeReturnCode` being `true`
        ///</summary>
        ///<returns>a default instance</returns>
        static member Create : unit -> CollectOptions
      end
// ```
// `Create()` returns an instance with all values empty and `ExposeReturnCode` is `true`.
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
        InputDirectories: seq<System.String>
        ///<summary>
        /// Corresponds to command line option `-o, --outputDirectory=VALUE`
        ///</summary>
        OutputDirectories: seq<System.String>
        ///<summary>
        /// Corresponds to command line option `-y, --symbolDirectory=VALUE`
        ///</summary>
        SymbolDirectories: seq<System.String>
        ///<summary>
        /// Corresponds to command line option `-d, --dependency=VALUE`
        ///</summary>
        Dependencies: seq<System.String>
        ///<summary>
        /// Corresponds to command line option ` -k, --key=VALUE`
        ///</summary>
        Keys: seq<System.String>
        ///<summary>
        /// Corresponds to command line option `--sn, --strongNameKey=VALUE`
        ///</summary>
        StrongNameKey: System.String
        ///<summary>
        /// Corresponds to command line option `-r, --report=VALUE`
        ///</summary>
        Report: System.String
        ///<summary>
        /// Corresponds to command line option `-f, --fileFilter=VALUE`
        ///</summary>
        FileFilter: seq<System.String>
        ///<summary>
        /// Corresponds to command line option `-s, --assemblyFilter=VALUE`
        ///</summary>
        AssemblyFilter: seq<System.String>
        ///<summary>
        /// Corresponds to command line option `-e, --assemblyExcludeFilter=VALUE`
        ///</summary>
        AssemblyExcludeFilter: seq<System.String>
        ///<summary>
        /// Corresponds to command line option `-t, --typeFilter=VALUE`
        ///</summary>
        TypeFilter: seq<System.String>
        ///<summary>
        /// Corresponds to command line option ` -m, --methodFilter=VALUE`
        ///</summary>
        MethodFilter: seq<System.String>
        ///<summary>
        /// Corresponds to command line option `-a, --attributeFilter=VALUE`
        ///</summary>
        AttributeFilter: seq<System.String>
        ///<summary>
        /// Corresponds to command line option `-p, --pathFilter=VALUE`
        ///</summary>
        PathFilter: seq<System.String>
        ///<summary>
        /// Corresponds to command line option `--attributetoplevel=VALUE`
        ///</summary>
        AttributeTopLevel: seq<System.String>
        ///<summary>
        /// Corresponds to command line option `--typetoplevel=VALUE`
        ///</summary>
        TypeTopLevel: seq<System.String>
        ///<summary>
        /// Corresponds to command line option `--methodtoplevel=VALUE`
        ///</summary>
        MethodTopLevel: seq<System.String>
        ///<summary>
        /// Corresponds to command line option `-c, --callContext=VALUE`
        ///</summary>
        CallContext: seq<System.String>
        ///<summary>
        /// Corresponds to command line option `--reportFormat=VALUE`
        ///</summary>
        ReportFormat: System.String
        ///<summary>
        /// Corresponds to command line option `--inplace`
        ///</summary>
        InPlace: bool
        ///<summary>
        /// Corresponds to command line option `--save`
        ///</summary>
        Save: bool
        ///<summary>
        /// Corresponds to command line option `--zipfile`
        ///</summary>
        ZipFile: bool
        ///<summary>
        /// Corresponds to command line option `--methodpoint`
        ///</summary>
        MethodPoint: bool
        ///<summary>
        /// Corresponds to command line option `--single`
        ///</summary>
        SingleVisit: bool
        ///<summary>
        /// Corresponds to command line option `--linecover`
        ///</summary>
        LineCover: bool
        ///<summary>
        /// Corresponds to command line option `--branchcover`
        ///</summary>
        BranchCover: bool
        ///<summary>
        /// Corresponds to the command line to run, given after a `-- `
        ///</summary>
        CommandLine: seq<System.String>
        ///<summary>
        /// Corresponds to the converse of command line option `--dropReturnCode `
        ///</summary>
        ExposeReturnCode: bool
        ///<summary>
        /// Corresponds to command line option `--sourcelink`
        ///</summary>
        SourceLink: bool
        ///<summary>
        /// Corresponds to command line option `--defer`
        ///</summary>
        Defer: bool
        ///<summary>
        /// Corresponds to command line option `-l, --localSource`
        ///</summary>
        LocalSource: bool
        ///<summary>
        /// Corresponds to command line option ` -v, --visibleBranches`
        ///</summary>
        VisibleBranches: bool
        ///<summary>
        /// Corresponds to command line option `--showstatic[=VALUE]`
        ///</summary>
        ShowStatic: string
        ///<summary>
        /// Corresponds to command line option ` --showGenerated`
        ///</summary>
        ShowGenerated: bool
        ///<summary>
        /// Corresponds to command line option ` -q`
        ///</summary>
        Verbosity : System.Diagnostics.TraceLevel
      }
    with
        ///<summary>
        /// Returns an instance with all fields empty that has all empty or `false` fields except `ExposeReturnCode`, `OpenCover`, `InPlace` and `Save` are `true`, and `ShowStatic` is `-`
        ///</summary>
        ///<returns>a default instance</returns>
        static member Create : unit -> PrepareOptions
    end
// ```
// `Create()` returns an instance that has all empty or `false` fields except `ExposeReturnCode`, `OpenCover`, `InPlace` and `Save` are `true`, and `ShowStatic` is `-`
//
// Fields that are not applicable to the use case or platform are silently ignored.
//
#if RUNNER
// ### type `LoggingOptions`
//
// Destinations for user level output at various levels of success.  `Echo` is for the
// command line and usage warninings only.
//
// ```
  ///<summary>
  /// Destinations for user level output
  ///</summary>
    [<NoComparison; NoEquality>]
    type LoggingOptions =
        {
        ///<summary>
        /// Sink for informational messages
        ///</summary>
        Info : System.String -> unit
        ///<summary>
        /// Sink for warning messages
        ///</summary>
        Warn : System.String -> unit
        ///<summary>
        /// Sink for error messages
        ///</summary>
        Failure : System.String -> unit
        ///<summary>
        /// Sink for command line/usage messages
        ///</summary>
        Echo : System.String -> unit }
    with
        ///<summary>
        /// Returns an instance that just discards all strings input.
        ///</summary>
        ///<returns>a default instance</returns>
        static member Create : unit -> LoggingOptions
    end
// ```
// `Create()` returns an instance that just discards all strings input.  For your particular use, direct message severities appropriately.  `Echo` is used to echo the synthetic command line in case of inconsistent inputs.
#endif
  end //// no doc