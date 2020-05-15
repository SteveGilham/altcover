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
// ## module `DotNet`
// ```
/// <summary>
/// <para type="description">Construction of `dotnet test` command line options.</para>
/// </summary>
[<RequireQualifiedAccess>]
module DotNet = begin
  /// <summary>
  /// <para type="description">Interface defining general command line arguments for `dotnet test` use..</para>
  /// </summary>
  type ICLIOptions =
    interface
    /// <summary>
    /// <para type="description">The `/AltCoverFailFast` value this represents</para>
    /// </summary>
    abstract member ForceDelete : bool with get
    /// <summary>
    /// <para type="description">The `/AltCoverForce` value this represents</para>
    /// </summary>
    abstract member FailFast : bool with get
    /// <summary>
    /// <para type="description">The `/AltCoverShowSummary` value this represents</para>
    /// </summary>
    abstract member ShowSummary : System.String with get
    end

  /// <summary>
  /// <para type="description">Union type defining general command line arguments for `dotnet test` use.</para>
  /// <para type="description">The F# code is [documented here](../DotNet-apidoc)</para>
  /// <para type="description">C# methods without documentation are compiler generated.  Most important for non-F# use are the `NewXxx` factory methods.</para>
  /// </summary>
  [<NoComparison>]
  type CLIOptions =
    | Force of bool
    | Fail of bool
    | Summary of System.String
    | Many of seq<CLIOptions>
    | Abstract of ICLIOptions
    with
      interface ICLIOptions
      /// <summary>
      /// <para type="description">The `/AltCoverFailFast` value this represents</para>
      /// </summary>
      member FailFast : bool
      /// <summary>
      /// <para type="description">The `/AltCoverForce` value this represents</para>
      /// </summary>
      member ForceDelete : bool
      /// <summary>
      /// <para type="description">The `/AltCoverShowSummary` value this represents</para>
      /// </summary>
      member ShowSummary : System.String
    end

// ```
// Union type defining general command line arguments for `dotnet test` use.
// case `Force` indicates a `/AltCoverForce` value
// case `Fail` inicates a `/AltCoverFailFast` value
// case `Summary` indicates a `/AltCoverShowSummary` value
// case `Many` indicates a collection of cases
//
// * value `Fast` gives the `/AltCoverFailFast` value this represents
// * value `ForceDelete` gives the `/AltCoverForce` value this represents
// * value `Summary` gives the `/AltCoverShowSummary` value this represents
#if TRACE // cheat mode here
    module internal I = begin
      val private arg : name:string -> s:string -> string
      val private listArg : name:string -> s:seq<System.String> -> string
      val private isSet : s:string -> bool
      val private fromList : name:string -> s:seq<System.String> -> string * bool
      val fromArg : name:string -> s:string -> string * bool
      val join : l:seq<string> -> string
      val toPrepareListArgumentList :
        prepare:Abstract.IPrepareOptions ->
          ((string -> #seq<System.String> -> string * bool) * string *
           System.String seq) list
      val toPrepareFromArgArgumentList :
        prepare:Abstract.IPrepareOptions ->
          ((string -> string -> string * bool) * string * System.String) list
      val toPrepareArgArgumentList :
        prepare:Abstract.IPrepareOptions ->
          ((string -> string -> string) * string * string * bool) list
      val toCollectFromArgArgumentList :
        collect:Abstract.ICollectOptions ->
          ((string -> string -> string * bool) * string * System.String) list
      val toCLIOptionsFromArgArgumentList :
        options:ICLIOptions ->
          ((string -> string -> string * bool) * string * System.String) list
      val toCLIOptionsArgArgumentList :
        options:ICLIOptions ->
          ((string -> string -> string) * string * string * bool) list
    end
#endif
#if RUNNER
// ```
    /// <summary>
    /// Converts the input into the command line for `dotnet test`
    /// </summary>
    /// <param name="prepare">Description of the instrumentation operation to perform</param>
    /// <param name="collect">Description of the collection operation to perform</param>
    /// <param name="options">All other `altcover` related command line arguments</param>
    /// <returns>The command line as a sequence of individual items</returns>
    val ToTestArgumentList :
      prepare:Abstract.IPrepareOptions ->
        collect:Abstract.ICollectOptions -> options:ICLIOptions -> string list

    /// <summary>
    /// Converts the input into the command line for `dotnet test`
    /// </summary>
    /// <param name="prepare">Description of the instrumentation operation to perform</param>
    /// <param name="collect">Description of the collection operation to perform</param>
    /// <param name="options">All other `altcover` related command line arguments</param>
    /// <returns>The composed command line</returns>
    val ToTestArguments :
      prepare:Abstract.IPrepareOptions ->
        collect:Abstract.ICollectOptions -> options:ICLIOptions -> string
  end
// ```
// The former creates the `/p:AltCoverXXX="yyy"` elements for a `dotnet test` invocation as a list of strings, the latter concatenates them, with space separators, into a single command line string.
#else
#if TRACE  // cheat mode here
    val internal toTestArgumentList :
      prepare:Abstract.IPrepareOptions ->
        collect:Abstract.ICollectOptions -> options:ICLIOptions -> string list
    val internal toTestArguments :
      prepare:Abstract.IPrepareOptions ->
        collect:Abstract.ICollectOptions -> options:ICLIOptions -> string
  end
#endif
#endif