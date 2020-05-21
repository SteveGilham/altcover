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
// ```
open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
// ```
// ## module `Abstract`
// This represents the weakly ("stringly") typed equivalent of the command line options in a C# friendly manner
// as interfaces with the values expressed as read-only properties
//
// Refer to the types in C# either as
//
// ```
// using Altcover;
// Abstract.IPrepareOptions prep = ...  // or whichever
// ```
// or
// ```
// using static Altcover.Abstract;
// IPrepareOptions prep = ... // or whichever
// ```
//
//
// ```
module Abstract =
// ```
// ## interface `ICollectOptions`
//
// ```
  ///<summary>
  /// Command line options for `AltCover Runner`
  ///</summary>
  type ICollectOptions =
    interface
    ///<summary>
    /// Corresponds to command line option `-r, --recorderDirectory=VALUE`
    ///</summary>
    abstract member RecorderDirectory : String with get
    ///<summary>
    /// Corresponds to command line option `-w, --workingDirectory=VALUE`
    ///</summary>
    abstract member WorkingDirectory : String with get
    ///<summary>
    /// Corresponds to command line option `-x, --executable=VALUE`
    ///</summary>
    abstract member Executable : String with get
    ///<summary>
    /// Corresponds to command line option `-l, --lcovReport=VALUE`
    ///</summary>
      [<SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
                        Justification="Lcov is a name")>]
    abstract member LcovReport : String with get
    ///<summary>
    /// Corresponds to command line option `-t, --threshold=VALUE`
    ///</summary>
    abstract member Threshold : String with get
    ///<summary>
    /// Corresponds to command line option `-c, --cobertura=VALUE`
    ///</summary>
      [<SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
                        Justification="Cobertura is a name")>]
    abstract member Cobertura : String with get
    ///<summary>
    /// Corresponds to command line option `-o, --outputFile=VALUE`
    ///</summary>
    abstract member OutputFile : String with get
    ///<summary>
    /// Corresponds to the command line arhuments for the executable, given after a `-- ``
    ///</summary>
    abstract member CommandLine : IEnumerable<String> with get
    ///<summary>
    /// Corresponds to the converse of command line option `--dropReturnCode `
    ///</summary>
    abstract member ExposeReturnCode : bool with get
    ///<summary>
    /// Corresponds to command line option `--teamcity[=VALUE]`
    ///</summary>
    abstract member SummaryFormat : String with get
  end
// ```
// ## interface `IPrepareOptions`
//
// ```
  ///<summary>
  /// Command line options for `AltCover Runner`
  ///</summary>
  type IPrepareOptions =
    interface
    ///<summary>
    /// Corresponds to command line option ` -i, --inputDirectory=VALUE`
    ///</summary>
    abstract member InputDirectories : IEnumerable<String> with get
    ///<summary>
    /// Corresponds to command line option `-o, --outputDirectory=VALUE`
    ///</summary>
    abstract member OutputDirectories : IEnumerable<String> with get
    ///<summary>
    /// Corresponds to command line option `-y, --symbolDirectory=VALUE`
    ///</summary>
    abstract member SymbolDirectories : IEnumerable<String> with get
    ///<summary>
    /// Corresponds to command line option `-d, --dependency=VALUE`
    ///</summary>
    abstract member Dependencies : IEnumerable<String> with get
    ///<summary>
    /// Corresponds to command line option ` -k, --key=VALUE`
    ///</summary>
    abstract member Keys : IEnumerable<String> with get
    ///<summary>
    /// Corresponds to command line option `--sn, --strongNameKey=VALUE`
    ///</summary>
    abstract member StrongNameKey : String with get
    ///<summary>
    /// Corresponds to command line option `-x, --xmlReport=VALUE`
    ///</summary>
    abstract member XmlReport : String with get
    ///<summary>
    /// Corresponds to command line option `-f, --fileFilter=VALUE`
    ///</summary>
    abstract member FileFilter : IEnumerable<String> with get
    ///<summary>
    /// Corresponds to command line option `-s, --assemblyFilter=VALUE`
    ///</summary>
    abstract member AssemblyFilter : IEnumerable<String> with get
    ///<summary>
    /// Corresponds to command line option `-e, --assemblyExcludeFilter=VALUE`
    ///</summary>
    abstract member AssemblyExcludeFilter : IEnumerable<String> with get
    ///<summary>
    /// Corresponds to command line option `-t, --typeFilter=VALUE`
    ///</summary>
    abstract member TypeFilter : IEnumerable<String> with get
    ///<summary>
    /// Corresponds to command line option ` -m, --methodFilter=VALUE`
    ///</summary>
    abstract member MethodFilter : IEnumerable<String> with get
    ///<summary>
    /// Corresponds to command line option `-a, --attributeFilter=VALUE`
    ///</summary>
    abstract member AttributeFilter : IEnumerable<String> with get
    ///<summary>
    /// Corresponds to command line option `-p, --pathFilter=VALUE`
    ///</summary>
    abstract member PathFilter : IEnumerable<String> with get
    ///<summary>
    /// Corresponds to command line option --attributetoplevel=VALUE`
    ///</summary>
    abstract member AttributeTopLevel : IEnumerable<String> with get
    ///<summary>
    /// Corresponds to command line option `--typetoplevel=VALUE`
    ///</summary>
    abstract member TypeTopLevel : IEnumerable<String> with get
    ///<summary>
    /// Corresponds to command line option `--methodtoplevel=VALUE`
    ///</summary>
    abstract member MethodTopLevel : IEnumerable<String> with get
    ///<summary>
    /// Corresponds to command line option `-c, --callContext=VALUE`
    ///</summary>
    abstract member CallContext : IEnumerable<String> with get
    ///<summary>
    /// Corresponds to command line option `--reportFormat=VALUE`
    ///</summary>
    abstract member ReportFormat : String with get
    ///<summary>
    /// Corresponds to command line option `--inplace`
    ///</summary>
    abstract member InPlace : bool with get
    ///<summary>
    /// Corresponds to command line option `--save`
    ///</summary>
    abstract member Save : bool with get
    ///<summary>
    /// Corresponds to command line option `--zipfile`
    ///</summary>
    abstract member ZipFile : bool with get
    ///<summary>
    /// Corresponds to command line option `--methodpoint`
    ///</summary>
    abstract member MethodPoint : bool with get
    ///<summary>
    /// Corresponds to command line option `--single`
    ///</summary>
    abstract member SingleVisit : bool with get
    ///<summary>
    /// Corresponds to command line option `--linecover`
    ///</summary>
    abstract member LineCover : bool with get
    ///<summary>
    /// Corresponds to command line option `--branchcover=VALUE`
    ///</summary>
    abstract member BranchCover : bool with get
    ///<summary>
    /// Corresponds to the command line to run, given after a `-- ``
    ///</summary>
    abstract member CommandLine : IEnumerable<String> with get
    ///<summary>
    /// Corresponds to the converse of command line option `--dropReturnCode `
    ///</summary>
    abstract member ExposeReturnCode : bool with get
    ///<summary>
    /// Corresponds to command line option `--sourcelink`
    ///</summary>
    abstract member SourceLink : bool with get
    ///<summary>
    /// Corresponds to command line option `--defer[=VALUE]`
    ///</summary>
    abstract member Defer : bool with get
    ///<summary>
    /// Corresponds to command line option `-l, --localSource`
    ///</summary>
    abstract member LocalSource : bool with get
    ///<summary>
    /// Corresponds to command line option ` -v, --visibleBranches`
    ///</summary>
    abstract member VisibleBranches : bool with get
    ///<summary>
    /// Corresponds to command line option `--showstatic[=VALUE]`
    ///</summary>
    abstract member ShowStatic : string with get
    ///<summary>
    /// Corresponds to command line option ` --showGenerated`
    ///</summary>
    abstract member ShowGenerated : bool with get
  end
// ```
#if RUNNER
// ### interface `ILoggingOptions`
//
// ```
  ///<summary>
  /// Destinations for user level output
  ///</summary>
  type ILoggingOptions =
    interface
    ///<summary>
    /// Sink for warning messages
    ///</summary>
    abstract member Info : Action<String> with get
    ///<summary>
    /// Sink for informational messages
    ///</summary>
    abstract member Warn : Action<String> with get
    ///<summary>
    /// Sink for error messages
    ///</summary>
    abstract member Failure : Action<String> with get
    ///<summary>
    /// Sink for command line/usage messages
    ///</summary>
    abstract member Echo : Action<String> with get
    end
#endif