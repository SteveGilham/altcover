// # namespace `AltCover.FSApi`
// ```
namespace AltCover.FSApi
// ```
// ## module `DotNetCLI`
// Composes the input options into a command line for `dotnet test`
// ```

/// <summary>
/// <para type="description">Composes the input options into a command line for `dotnet test`.</para>
/// </summary>
  [<RequireQualifiedAccess>]
  module DotNetCLI = begin
    /// <summary>
    /// Converts the input into the command line for `dotnet test`
    /// </summary>
    /// <param name="prepare">Description of the instrumentation operation to perform</param>
    /// <param name="collect">Description of the collection operation to perform</param>
    /// <param name="options">All other `altcover` related command line arguments</param>
    /// <returns>The command line as a sequence of individual items</returns>
    val ToTestArgumentList :
      prepare:AltCover.OptionApi.PrepareOptions ->
        collect:AltCover.OptionApi.CollectOptions -> options:DotNet.CLIOptions -> string list

    /// <summary>
    /// Converts the input into the command line for `dotnet test`
    /// </summary>
    /// <param name="prepare">Description of the instrumentation operation to perform</param>
    /// <param name="collect">Description of the collection operation to perform</param>
    /// <param name="options">All other `altcover` related command line arguments</param>
    /// <returns>The composed command line</returns>
    val ToTestArguments :
      prepare:AltCover.OptionApi.PrepareOptions ->
        collect:AltCover.OptionApi.CollectOptions -> options:DotNet.CLIOptions -> string
  end
// ```
// The former creates the `/p:AltCoverXXX="yyy"` elements for a `dotnet test` invocation as a list of strings, the latter concatenates them, with space separators, into a single command line string.