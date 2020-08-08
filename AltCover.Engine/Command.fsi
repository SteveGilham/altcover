// # namespace `AltCover`
// ```
namespace AltCover
// ```
// ## module `Command`
//
// ```
///<summary>
/// This represents the various operations available
///</summary>
module Command = begin
// ```
// This represents the various operations available
// ```
  ///<summary>
  /// Instrument assemblies
  ///</summary>
  /// <param name="args">The command line</param>
  /// <param name="log">How to report feedback</param>
  /// <returns>operation return code.</returns>
  val Prepare : args:Abstract.IPrepareOptions -> log:AltCover.LoggingOptions -> int
  ///<summary>
  /// Process coverage
  ///</summary>
  /// <param name="args">The command line</param>
  /// <param name="log">How to report feedback</param>
  /// <returns>operation return code.</returns>
  val Collect : args:Abstract.ICollectOptions -> log:AltCover.LoggingOptions -> int
  ///<summary>
  /// Indicate how to consume for PowerShell
  ///</summary>
  /// <returns>The `Import-Module` command required.</returns>
  val ImportModule : unit -> string
  ///<summary>
  /// Indicate the current version
  ///</summary>
  /// <returns>The strongly-typed version.</returns>
  val Version : unit -> System.Version
  ///<summary>
  /// Indicate the current version
  ///</summary>
  /// <returns>The version as a string.</returns>
  val FormattedVersion : unit -> string
  ///<summary>
  /// Return the last computed coverage summary
  ///</summary>
  /// <returns>The last computed coverage summary.</returns>
  val Summary : unit -> string
end
// ```
//
// where `int` results are 0 for success and otherwise for failure (this would be the return code of the operation if run as a command-line function)
//
// `FormattedVersion` return is of the form "AltCover version #.#.##.##" as per the command-line "Version" option.
//
// `Summary` returns the last computed coverage summary if any