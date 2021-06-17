// Both the .net framework/mono and .net core releases publish MSBuild tasks from the main assembly (AltCover.exe or AltCover.dll, respectively) that wrap the command-line functionality (as documented here under [Usage](https://github.com/SteveGilham/altcover/wiki/Usage)).
//
// # namespace `AltCover`
//
// For the C# programmer,  attributes have the extran angle-brackets, while `member [Name] : [type] with get, set` is a `[type]` valued property called `[Name]`; and `string array` is just `string[]` spelled out longhand.
//
//
// For `AltCover.Prepare` and `AltCover.Collect`, the task parameters match the command line arguments in name and function, except that `SymbolDirectories` is pluralised, `SingleVisit` represents the `--single` option, `ExposeReturnCode` is the converse of the command line option `dropReturnCode`, and `CommandLine` is everything after a `--` .  If `AltCover.Collect`'s `Executable` parameter is set, that switches the virtual `--collect` flag off.
// ```
namespace AltCover
open Microsoft.Build.Framework //// no doc
open Microsoft.Build.Utilities //// no doc
// ```
// ## Task `AltCover.Prepare`
// This is the instrumentation mode with `--save --inplace --defer` as default.  Associated parameters are
// ```
///<summary>
/// <para>Description of the `AltCover` command</para>
///</summary>
type Prepare =
  class
    inherit Task
    ///<summary>
    /// <para>The default constructor</para>
    ///</summary>
    new : unit -> Prepare
    ///<summary>
    /// <para>Perform the operation</para>
    /// <returns>The success of the outcome.</returns>
    ///</summary>
    override Execute : unit -> bool
    ///<summary>
    /// Corresponds to command line option ` -i, --inputDirectory=VALUE`
    ///</summary>
    member InputDirectories : string array with get, set
    ///<summary>
    /// Corresponds to command line option `-o, --outputDirectory=VALUE`
    ///</summary>
    member OutputDirectories : string array with get, set
    ///<summary>
    /// Corresponds to command line option `-y, --symbolDirectory=VALUE`
    ///</summary>
    member SymbolDirectories : string array with get, set
    ///<summary>
    /// Corresponds to command line option `-d, --dependency=VALUE`
    ///</summary>
    member Dependencies : string array with get, set
    ///<summary>
    /// Corresponds to command line option ` -k, --key=VALUE`
    ///</summary>
    member Keys : string array with get, set
    ///<summary>
    /// Corresponds to command line option `--sn, --strongNameKey=VALUE`
    ///</summary>
    member StrongNameKey : string with get, set
    ///<summary>
    /// Corresponds to command line option `-r, --report=VALUE`
    ///</summary>
    member Report : string with get, set
    ///<summary>
    /// Corresponds to command line option `-f, --fileFilter=VALUE`
    ///</summary>
    member FileFilter : string array with get, set
    ///<summary>
    /// Corresponds to command line option `-s, --assemblyFilter=VALUE`
    ///</summary>
    member AssemblyFilter : string array with get, set
    ///<summary>
    /// Corresponds to command line option `-e, --assemblyExcludeFilter=VALUE`
    ///</summary>
    member AssemblyExcludeFilter : string array with get, set
    ///<summary>
    /// Corresponds to command line option `-t, --typeFilter=VALUE`
    ///</summary>
    member TypeFilter : string array with get, set
    ///<summary>
    /// Corresponds to command line option ` -m, --methodFilter=VALUE`
    ///</summary>
    member MethodFilter : string array with get, set
    ///<summary>
    /// Corresponds to command line option `-a, --attributeFilter=VALUE`
    ///</summary>
    member AttributeFilter : string array with get, set
    ///<summary>
    /// Corresponds to command line option `-p, --pathFilter=VALUE`
    ///</summary>
    member PathFilter : string array with get, set
    ///<summary>
    /// Corresponds to command line option --attributetoplevel=VALUE`
    ///</summary>
    member AttributeTopLevel : string array with get, set
    ///<summary>
    /// Corresponds to command line option `--typetoplevel=VALUE`
    ///</summary>
    member TypeTopLevel : string array with get, set
    ///<summary>
    /// Corresponds to command line option `--methodtoplevel=VALUE`
    ///</summary>
    member MethodTopLevel : string array with get, set
    ///<summary>
    /// Corresponds to command line option `-c, --callContext=VALUE`
    ///</summary>
    member CallContext : string array with get, set
    ///<summary>
    /// Corresponds to command line option `--reportFormat=VALUE`
    ///</summary>
    member ReportFormat : string with get, set
    ///<summary>
    /// Corresponds to command line option `--inplace`
    ///</summary>
    member InPlace : bool with get, set
    ///<summary>
    /// Corresponds to command line option `--save`
    ///</summary>
    member Save : bool with get, set
    ///<summary>
    /// Corresponds to command line option `--zipfile`
    ///</summary>
    member ZipFile : bool with get, set
    ///<summary>
    /// Corresponds to command line option `--methodpoint`
    ///</summary>
    member MethodPoint : bool with get, set
    ///<summary>
    /// Corresponds to command line option `--single`
    ///</summary>
    member SingleVisit : bool with get, set
    ///<summary>
    /// Corresponds to command line option `--linecover`
    ///</summary>
    member LineCover : bool with get, set
    ///<summary>
    /// Corresponds to command line option `--branchcover`
    ///</summary>
    member BranchCover : bool with get, set
    ///<summary>
    /// Corresponds to the command line to run, given after a `-- `
    ///</summary>
    member CommandLine : string array with get, set
    ///<summary>
    /// Corresponds to the converse of command line option `--dropReturnCode `
    ///</summary>
    member ExposeReturnCode : bool with get, set
    ///<summary>
    /// Corresponds to command line option `--sourcelink`
    ///</summary>
    member SourceLink : bool with get, set
    ///<summary>
    /// Corresponds to command line option `--defer`
    ///</summary>
    member Defer : bool with get, set
    ///<summary>
    /// Corresponds to command line option `-l, --localSource`
    ///</summary>
    member LocalSource : bool with get, set
    ///<summary>
    /// Corresponds to command line option ` -v, --visibleBranches`
    ///</summary>
    member VisibleBranches : bool with get, set
    ///<summary>
    /// Corresponds to command line option `--showstatic[=VALUE]`
    ///</summary>
    member ShowStatic : string with get, set
    ///<summary>
    /// Corresponds to command line option ` --showGenerated`
    ///</summary>
    member ShowGenerated : bool with get, set
    ///<summary>
    /// Corresponds to command line option ` -q` (expects names of `System.Diagnostics.TraceLevel` values)
    ///</summary>
    member Verbosity : string with get, set
  end
// ```
// ## Task `AltCover.Collect`
// This is `runner` mode with `--collect` as default.  Associated parameters are
// ```
///<summary>
/// <para>Description of the `AltCover Runner` command</para>
///</summary>
type Collect =
  class
    inherit Task
    ///<summary>
    /// <para>The default constructor</para>
    ///</summary>
    new : unit -> Collect
    ///<summary>
    /// <para>Perform the operation</para>
    /// <returns>The success of the outcome.</returns>
    ///</summary>
    override Execute : unit -> bool
    ///<summary>
    /// Task output calue, the summary of coverage.
    ///</summary>
    [<Output>]
    member Summary : string
    ///<summary>
    /// Corresponds to command line option `-r, --recorderDirectory=VALUE`; a required parameter
    ///</summary>
    [<Required>]
    member RecorderDirectory : string with get, set
    ///<summary>
    /// Corresponds to command line option `-w, --workingDirectory=VALUE`
    ///</summary>
    member WorkingDirectory : string with get, set
    ///<summary>
    /// Corresponds to command line option `-x, --executable=VALUE`
    ///</summary>
    member Executable : string with get, set
    ///<summary>
    /// Corresponds to command line option `-l, --lcovReport=VALUE`
    ///</summary>
    member LcovReport : string with get, set
    ///<summary>
    /// Corresponds to command line option `-t, --threshold=VALUE`
    ///</summary>
    member Threshold : string with get, set
    ///<summary>
    /// Corresponds to command line option `-c, --cobertura=VALUE`
    ///</summary>
    member Cobertura : string with get, set
    ///<summary>
    /// Corresponds to command line option `-o, --outputFile=VALUE`
    ///</summary>
    member OutputFile : string with get, set
    ///<summary>
    /// Corresponds to the command line arguments for the executable, given after a `-- `
    ///</summary>
    member CommandLine : string array with get, set
    ///<summary>
    /// Corresponds to the converse of command line option `--dropReturnCode `
    ///</summary>
    member ExposeReturnCode : bool with get, set
    ///<summary>
    /// Corresponds to command line option `--teamcity[=VALUE]`
    ///</summary>
    member SummaryFormat : string with get, set
    ///<summary>
    /// Corresponds to command line option ` -q` (expects names of `System.Diagnostics.TraceLevel` values)
    ///</summary>
    member Verbosity : string with get, set
  end
// ```
// ## Task `AltCover.PowerShell`
// This is the `ImportModule` option; it takes no parameters.
// ```
///<summary>
/// <para>Description of the `AltCover ImportModule` command</para>
///</summary>
type PowerShell =
  class
    inherit Task
    ///<summary>
    /// <para>The default constructor</para>
    ///</summary>
    new : unit -> PowerShell
    ///<summary>
    /// <para>Perform the operation</para>
    /// <returns>The success of the outcome.</returns>
    ///</summary>
    override Execute : unit -> bool
  end
// ```
// ## Task `AltCover.GetVersion`
// This is the `Version` option; it takes no parameters.
// ```
///<summary>
/// <para>Description of the `AltCover Version` command</para>
///</summary>
type GetVersion =
  class
    inherit Task
    ///<summary>
    /// <para>The default constructor</para>
    ///</summary>
    new : unit -> GetVersion
    ///<summary>
    /// <para>Perform the operation</para>
    /// <returns>The success of the outcome.</returns>
    ///</summary>
    override Execute : unit -> bool
  end
// ```
// ## Task `AltCover.Echo`
// Outputs a possibly coloured string of text to `stdout`.
// ```
///<summary>
/// <para>Outputs a possibly coloured string of text to `stdout`.</para>
///</summary>
type Echo =
  class
    inherit Task
    ///<summary>
    /// <para>The default constructor</para>
    ///</summary>
    new : unit -> Echo
    ///<summary>
    /// <para>Perform the operation</para>
    /// <returns>The success of the outcome.</returns>
    ///</summary>
    override Execute : unit -> bool
    ///<summary>
    /// The name of the output colour, defaulting to current
    ///</summary>
    member Colour : string with get, set
    ///<summary>
    /// The text to write, a required parameter
    ///</summary>
    [<Required>]
    member Text : string with get, set
    ///<summary>
    /// Corresponds to command line option ` -q` (expects names of `System.Diagnostics.TraceLevel` values)
    ///</summary>
    member Verbosity : string with get, set
  end
// ```
// ## Task `AltCover.RunSettings`
// Used by the .net core implementation to inject an AltCover data collector, by creating a temporary run-settings file that includes AltCover as well as any user-defined settings.
//
// Not intended for general use, but see the `AltCover.targets` file for how it is used around the test stage.
// ```
///<summary>
/// <para>Used by the .net core implementation to inject an AltCover data collector, by creating a temporary run-settings file that includes AltCover as well as any user-defined settings.</para>
/// <para>Not intended for general use, but see the `AltCover.targets` file for how it is used around the test stage.</para>
///</summary>
type RunSettings =
  class
    inherit Task
    ///<summary>
    /// <para>The default constructor</para>
    ///</summary>
    new : unit -> RunSettings
    ///<summary>
    /// <para>Perform the operation</para>
    /// <returns>The success of the outcome.</returns>
    ///</summary>
    override Execute : unit -> bool
    ///<summary>
    /// The settings file generated, an output parameter
    ///</summary>
    [<Output>]
    member Extended : string with get, set
    ///<summary>
    /// The current settings file to be extended
    ///</summary>
    member TestSetting : string with get, set
    ///<summary>
    /// Corresponds to command line option ` -q` (expects names of `System.Diagnostics.TraceLevel` values)
    ///</summary>
    member Verbosity : string with get, set
  end
// ```
// ## Task `AltCover.ContingentCopy`
// Used by the .net core implementation to copy files copied relative to the output directory to the same locations relative to the instrumented files folder
//
// Not intended for general use, but see the `AltCover.targets` file for how it is used around the test stage.
// ```
///<summary>
/// <para>Used by the .net core implementation to copy files copied relative to the output directory to the same locations relative to the instrumented files folder.</para>
/// <para>Not intended for general use, but see the `AltCover.targets` file for how it is used around the test stage.</para>
///</summary>
type ContingentCopy =
  class
    inherit Task
    ///<summary>
    /// <para>The default constructor</para>
    ///</summary>
    new : unit -> ContingentCopy
    ///<summary>
    /// <para>Perform the operation</para>
    /// <returns>The success of the outcome.</returns>
    ///</summary>
    override Execute : unit -> bool
    ///<summary>
    /// The file relative location (if empty, then no-op)
    ///</summary>
    member RelativeDir : string with get, set
    ///<summary>
    /// The project directory for basing relative paths from (possibly empty; ignored if not an absolute path)
    ///</summary>
    member ProjectDir : string with get, set
    ///<summary>
    /// The file copying property (if empty, then no-op)
    ///</summary>
    member CopyToOutputDirectory : string with get, set
    ///<summary>
    /// The name of the file
    ///</summary>
    member FileName : string with get, set
    ///<summary>
    /// The base of the relative from directory
    ///</summary>
    [<Required>]
    member BuildOutputDirectory : string with get, set
    ///<summary>
    /// The base of the relative to directory
    ///</summary>
    [<Required>]
    member InstrumentDirectory : string with get, set
  end
// ```
// ## Task `AltCover.RetryDelete`
// Used by the .net core implementation to safely delete files
//
// Not intended for general use, but see the `AltCover.targets` file for how it is used around the test stage.
// ```
///<summary>
/// <para>Used by the .net core implementation to tidy files that may be in a lingering 'in use' state</para>
/// <para>Not intended for general use, but see the `AltCover.targets` file for how it is used around the test stage.</para>
///</summary>
type RetryDelete =
  class
    inherit Task
    ///<summary>
    /// <para>The default constructor</para>
    ///</summary>
    new : unit -> RetryDelete
    ///<summary>
    /// <para>Perform the operation</para>
    /// <returns>The success of the outcome.</returns>
    ///</summary>
    override Execute : unit -> bool
    ///<summary>
    /// The file relative location (if empty, then no-op)
    ///</summary>
    member Files : string array with get, set
  end
// ```