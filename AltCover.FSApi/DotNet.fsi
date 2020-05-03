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
  /// <para type="description">Union type defining general command line arguments for `dotnet test` use.</para>
  /// <para type="description">The F# code is [documented here](../DotNet-apidoc)</para>
  /// <para type="description">C# methods without documentation are compiler generated.  Most important for non-F# use are the `NewXxx` factory methods.</para>
  /// </summary>
  [<NoComparison>]
  type CLIOptions =
    | Force of bool
    | FailFast of bool
    | ShowSummary of System.String
    | Many of seq<CLIOptions>
    with
      /// <summary>
      /// <para type="description">The `/AltCoverFailFast` value this represents</para>
      /// </summary>
      member Fast : bool
      /// <summary>
      /// <para type="description">The `/AltCoverForce` value this represents</para>
      /// </summary>
      member ForceDelete : bool
      /// <summary>
      /// <para type="description">The `/AltCoverShowSummary` value this represents</para>
      /// </summary>
      member Summary : System.String
    end
  end
// ```
// Union type defining general command line arguments for `dotnet test` use.
// case `Force` indicates a `/AltCoverForce` value
// case `FailFast` inicates a `/AltCoverForce` value
// case `ShowSummary` indicates a `/AltCoverShowSummary` value
// case `Many` indicates a collection of cases
//
// * value `Fast` gives the `/AltCoverFailFast` value this represents
// * value `ForceDelete` gives the `/AltCoverForce` value this represents
// * value `Summary` gives the `/AltCoverShowSummary` value this represents