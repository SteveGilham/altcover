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
// * case `Force` indicates a `/AltCoverForce` value
// * case `Fail` indicates a `/AltCoverFailFast` value
// * case `Summary` indicates a `/AltCoverShowSummary` value
// * case `Many` indicates a collection of cases
// * case `Abstract` indicates a collection of cases expressed as an interface
//
// vs
//
// * value `Fast` gives the `/AltCoverFailFast` value this represents
// * value `ForceDelete` gives the `/AltCoverForce` value this represents
// * value `Summary` gives the `/AltCoverShowSummary` value this represents
#if TRACE // cheat mode here
    module internal I = begin
      val private arg : name:string -> s:string -> (string*string)
      val private listArg : name:string -> s:seq<System.String> -> (string*string)
      val private isSet : s:string -> bool
      val private fromList : name:string -> s:seq<System.String> -> (string*string) * bool
      val fromArg : name:string -> s:string -> (string*string) * bool
      val toPrepareListArgumentList :
        prepare:Abstract.IPrepareOptions ->
          ((string -> #seq<System.String> -> (string*string) * bool) * string *
           System.String seq) list
      val toPrepareFromArgArgumentList :
        prepare:Abstract.IPrepareOptions ->
          ((string -> string -> (string*string) * bool) * string * System.String) list
      val toPrepareArgArgumentList :
        prepare:Abstract.IPrepareOptions ->
          ((string -> string -> (string*string)) * string * string * bool) list
      val toCollectFromArgArgumentList :
        collect:Abstract.ICollectOptions ->
          ((string -> string -> (string*string)* bool) * string * System.String) list
      val toSharedFromValueArgumentList :
        verbosity : System.Diagnostics.TraceLevel ->
       ((string -> obj -> bool -> (string*string)*bool) * string * obj * bool) list
      val toCLIOptionsFromArgArgumentList :
        options:ICLIOptions ->
          ((string -> string -> (string*string) * bool) * string * string) list
      val toCLIOptionsArgArgumentList :
        options:ICLIOptions ->
          ((string -> string -> (string*string)) * string * string * bool) list
    end
#endif
// ### Composing the whole command line
// ```
    /// <summary>
    /// Command line properties for `dotnet test` ImportModule option
    /// </summary>
    val ImportModuleProperties : (string*string) list

    /// <summary>
    /// Command line properties for `dotnet test` GetVersion option
    /// </summary>

    val GetVersionProperties : (string*string) list
    /// <summary>
    /// Converts the input into the command line properties for `dotnet test`
    /// </summary>
    /// <param name="prepare">Description of the instrumentation operation to perform</param>
    /// <param name="collect">Description of the collection operation to perform</param>
    /// <param name="options">All other `altcover` related command line arguments</param>
    /// <returns>The command line as a sequence of name, value pairs</returns>
    val ToTestPropertiesList :
      prepare:Abstract.IPrepareOptions ->
        collect:Abstract.ICollectOptions -> options:ICLIOptions -> (string*string) list

#if RUNNER
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
  end //// no doc
// ```
// The former creates the `/p:AltCoverXXX="yyy"` elements for a `dotnet test` invocation as a list of strings, the latter concatenates them, with space separators, into a single command line string.
#else
#if TRACE  // cheat mode here
  end
#endif
#endif
// ## module `Options`
// * `[<RequireQualifiedAccess>]`
// * default interface implementations with get and set members
//   * type CLI - implements DotNet.ICLIOptions : default values fale or empty
//   * type Collect - implements Abstract.ICollectOptions : default values as per the `Primitive.CollectOptions` record
//   * type Prepare - implements Abstract.IPrepareOptions : default values as per the `Primitive.PrepareOptions` record
#if RUNNER
//   * type Logging - implements Abstract.ILoggingOptions : default values write to `Console.Out`, except `Failure` which writes to Console.Error
#endif