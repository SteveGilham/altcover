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
// ## module `AltCover`
//
// ```
///<summary>
/// This represents the  command line options and its validation
///</summary>
  [<RequireQualifiedAccess>]
  module AltCover = begin
// ```
// This represents the  command line options and its validation
#if RUNNER
// ### type `ValidatedCommandLine`
// ```
    ///<summary>
    /// Holds the composed command line and any validation errors.
    ///</summary>
    [<NoComparison>]
    type ValidatedCommandLine =
      {
        ///<summary>
        /// The composed command line in `Command`
        ///</summary>
        Command: string list
        ///<summary>
        /// Any validations errors in `Errors`.
        ///</summary>
        Errors: seq<string> }
      with
        ///<summary>
        /// Returns a nicely formatted representation
        ///</summary>
        override ToString : unit -> string
      end
// ```
// Holds the composed command line in `Command`, and any validation errors in `Errors`.
//
// The `ToString()` override formats the outcome for pretty-printing
#endif
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
      ///<summary>
      /// <para>Options expressed as an F# "stringly" typed record</para>
      ///</summary>
      | Primitive of Primitive.CollectOptions
      ///<summary>
      /// <para>Options expressed as an F# strongly-typed record</para>
      ///</summary>
      | TypeSafe of TypeSafe.CollectOptions
      ///<summary>
      /// <para>Options expressed as an interface</para>
      ///</summary>
      | Abstract of Abstract.ICollectOptions
      with
        interface Abstract.ICollectOptions
        ///<summary>
        /// Corresponds to command line option `-r, --recorderDirectory=VALUE`
        ///</summary>
        member RecorderDirectory : System.String

        ///<summary>
        /// Corresponds to command line option `-w, --workingDirectory=VALUE`
        ///</summary>
        member WorkingDirectory : System.String
        ///<summary>
        /// Corresponds to command line option `-x, --executable=VALUE`
        ///</summary>
        member Executable : System.String
        ///<summary>
        /// Corresponds to command line option `-l, --lcovReport=VALUE`
        ///</summary>
        member LcovReport : System.String
        ///<summary>
        /// Corresponds to command line option `-t, --threshold=VALUE`
        ///</summary>
        member Threshold : System.String
        ///<summary>
        /// Corresponds to command line option `-c, --cobertura=VALUE`
        ///</summary>
        member Cobertura : System.String
        ///<summary>
        /// Corresponds to command line option `-o, --outputFile=VALUE`
        ///</summary>
        member OutputFile : System.String
        ///<summary>
        /// Corresponds to the command line arguments for the executable, given after a `-- `
        ///</summary>
        member CommandLine : seq<string>
        ///<summary>
        /// Corresponds to the converse of command line option `--dropReturnCode `
        ///</summary>
        member ExposeReturnCode : bool
        ///<summary>
        /// Corresponds to command line option `--teamcity[=VALUE]`
        ///</summary>
        member SummaryFormat : System.String
// ```
#if RUNNER
// ```
        ///<summary>
        /// Does simple checking of the arguments without causing any changes to the system
        ///</summary>
        /// <param name="afterPreparation">`true` if the Prepare step has already run and there should be instrumented code the `RecorderDirectory`</param>
        /// <returns>All the problems that the application command-line could report, so empty is success.</returns>
        member Validate : afterPreparation:bool -> string []
      end
// ```
// `Validate` does simple checking of the arguments without causing any changes to the system; set the input argument `true` if the Prepare step has already run (and there should be instrumented code the `RecorderDirectory`; returns all the problems that the application command-line could report, so empty is success.
//
#else
// ```
      end
// ```
#endif
//
// The property members corresponding to the command line options read the values from the contained instance; null strings will be retrurned as null, but null sequences will be returned as empty ones.
//
//  Values that are not applicable to the use case or platform are silently ignored.
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
      ///<summary>
      /// <para>Options expressed as an F# "stringly" typed record</para>
      ///</summary>
      | Primitive of Primitive.PrepareOptions
      ///<summary>
      /// <para>Options expressed as an F# strongly-typed record</para>
      ///</summary>
      | TypeSafe of TypeSafe.PrepareOptions
      ///<summary>
      /// <para>Options expressed as an interface</para>
      ///</summary>
      | Abstract of Abstract.IPrepareOptions
      with
        interface Abstract.IPrepareOptions
        ///<summary>
        /// Corresponds to command line option ` -i, --inputDirectory=VALUE`
        ///</summary>
        member InputDirectories : System.String list
        ///<summary>
        /// Corresponds to command line option `-o, --outputDirectory=VALUE`
        ///</summary>
        member OutputDirectories : System.String list
        ///<summary>
        /// Corresponds to command line option `-y, --symbolDirectory=VALUE`
        ///</summary>
        member SymbolDirectories : System.String list
        ///<summary>
        /// Corresponds to command line option `-d, --dependency=VALUE`
        ///</summary>
        member Dependencies : System.String list
        ///<summary>
        /// Corresponds to command line option ` -k, --key=VALUE`
        ///</summary>
        member Keys : System.String list
        ///<summary>
        /// Corresponds to command line option `--sn, --strongNameKey=VALUE`
        ///</summary>
        member StrongNameKey : System.String
        ///<summary>
        /// Corresponds to command line option `-x, --xmlReport=VALUE`
        ///</summary>
        member XmlReport : System.String
        ///<summary>
        /// Corresponds to command line option `-f, --fileFilter=VALUE`
        ///</summary>
        member FileFilter : System.String list
        ///<summary>
        /// Corresponds to command line option `-s, --assemblyFilter=VALUE`
        ///</summary>
        member AssemblyFilter : System.String list
        ///<summary>
        /// Corresponds to command line option `-e, --assemblyExcludeFilter=VALUE`
        ///</summary>
        member AssemblyExcludeFilter : System.String list
        ///<summary>
        /// Corresponds to command line option `-t, --typeFilter=VALUE`
        ///</summary>
        member TypeFilter : System.String list
        ///<summary>
        /// Corresponds to command line option ` -m, --methodFilter=VALUE`
        ///</summary>
        member MethodFilter : System.String list
        ///<summary>
        /// Corresponds to command line option `-a, --attributeFilter=VALUE`
        ///</summary>
        member AttributeFilter : System.String list
        ///<summary>
        /// Corresponds to command line option `-p, --pathFilter=VALUE`
        ///</summary>
        member PathFilter : System.String list
        ///<summary>
        /// Corresponds to command line option --attributetoplevel=VALUE`
        ///</summary>
        member AttributeTopLevel : System.String list
        ///<summary>
        /// Corresponds to command line option `--typetoplevel=VALUE`
        ///</summary>
        member TypeTopLevel : System.String list
        ///<summary>
        /// Corresponds to command line option `--methodtoplevel=VALUE`
        ///</summary>
        member MethodTopLevel : System.String list
        ///<summary>
        /// Corresponds to command line option `-c, --callContext=VALUE`
        ///</summary>
        member CallContext : System.String list
        ///<summary>
        /// Corresponds to command line option `--reportFormat=VALUE`
        ///</summary>
        member ReportFormat : string
        ///<summary>
        /// Corresponds to command line option `--inplace`
        ///</summary>
        member InPlace : bool
        ///<summary>
        /// Corresponds to command line option `--save`
        ///</summary>
        member Save : bool
        ///<summary>
        /// Corresponds to command line option `--zipfile`
        ///</summary>
        member ZipFile : bool
        ///<summary>
        /// Corresponds to command line option `--methodpoint`
        ///</summary>
        member MethodPoint : bool
        ///<summary>
        /// Corresponds to command line option `--single`
        ///</summary>
        member SingleVisit : bool
        ///<summary>
        /// Corresponds to command line option `--linecover`
        ///</summary>
        member LineCover : bool
        ///<summary>
        /// Corresponds to command line option `--branchcover`
        ///</summary>
        member BranchCover : bool
        ///<summary>
        /// Corresponds to the command line to run, given after a `-- `
        ///</summary>
        member CommandLine : seq<System.String>
        ///<summary>
        /// Corresponds to the converse of command line option `--dropReturnCode `
        ///</summary>
        member ExposeReturnCode : bool
        ///<summary>
        /// Corresponds to command line option `--sourcelink`
        ///</summary>
        member SourceLink : bool
        ///<summary>
        /// Corresponds to command line option `--defer`
        ///</summary>
        member Defer : bool
        ///<summary>
        /// Corresponds to command line option `-l, --localSource`
        ///</summary>
        member LocalSource : bool
        ///<summary>
        /// Corresponds to command line option ` -v, --visibleBranches`
        ///</summary>
        member VisibleBranches : bool
        ///<summary>
        /// Corresponds to command line option `--showstatic[=VALUE]`
        ///</summary>
        member ShowStatic : string
        ///<summary>
        /// Corresponds to command line option ` --showGenerated`
        ///</summary>
        member ShowGenerated : bool
// ```
#if RUNNER
// ```
        ///<summary>
        /// Does simple checking of the arguments without causing any changes to the system
        ///</summary>
        /// <returns>All the problems that the application command-line could report, so empty is success.</returns>
        member Validate : unit -> string []
      end
// ```
// `Validate` does simple checking of the arguments without causing any changes to the system; returns all the problems that the application command-line could report, so empty is success.
//
#else
// ```
      end
// ```
#endif
//
// The property members corresponding to the command line options read the values from the contained instance; null strings will be retrurned as null, but null sequences will be returned as empty ones.
//
//  Values that are not applicable to the use case or platform are silently ignored.
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
    type LoggingOptions =
      ///<summary>
      /// <para>Options expressed as an F# "stringly" typed record</para>
      ///</summary>
      | Primitive of Primitive.LoggingOptions
      ///<summary>
      /// <para>Options expressed as an interface</para>
      ///</summary>
      | Abstract of Abstract.ILoggingOptions
      with
        ///<summary>
        /// Sink for informational messages
        ///</summary>
        member Info : (System.String -> unit)
        ///<summary>
        /// Sink for warning messages
        ///</summary>
        member Warn : (System.String -> unit)
        ///<summary>
        /// Sink for error messages
        ///</summary>
        member Error : (System.String -> unit)
        ///<summary>
        /// Sink for command line/usage messages
        ///</summary>
        member Echo : (System.String -> unit)
        static member
        ///<summary>
        /// A helper for C# use
        ///</summary>
        /// <param name="action">A C# `Action` value e.g. from the `ILoggingOptions` interface</param>
        /// <returns>All the problems that the application command-line could report, so empty is success.</returns>
          ActionAdapter : action:System.Action<System.String> ->
                            (System.String -> unit)
        ///<summary>
        /// Returns a pure sink (all input ignored) instance
        ///</summary>
        static member Create : unit -> LoggingOptions
        ///<summary>
        /// Translates a C# style interface to the corresponding F# type
        ///</summary>
        /// <param name="options">A C# `Action` based logging description</param>
        /// <returns>The F# function equivalent.</returns>
        static member Translate : options:Abstract.ILoggingOptions -> LoggingOptions
      end
// ```
//  Of the static methods, `Create()` returns a pure sink instance; while `ActionAdapter` and `Translate` are helpers for C# use.
// The instance methods return the corresponding value from the underlying structure, translating `Failure` to `Error` (not a keyword in F#)
#endif
  end //// no doc