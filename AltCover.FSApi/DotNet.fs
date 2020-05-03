#if RUNNER
namespace AltCover
#else
namespace AltCoverFake.DotNet.Testing
#endif

open System
open System.Diagnostics.CodeAnalysis

/// <summary>
/// <para type="description">Construction of `dotnet test` command lines.</para>
/// </summary>
[<RequireQualifiedAccess>]
module DotNet =
  /// <summary>
  /// <para type="description">Union type defining general command line arguments for `dotnet test` use.</para>
  /// <para type="description">The F# code is</para>
  /// <code>
  ///   type CLIOptions =
  ///     | Force of bool
  ///     | FailFast of bool
  ///     | ShowSummary of String
  ///     | Many of CLIOptions seq
  /// </code>
  /// <para type="description">C# methods without documentation are compiler generated.  Most important for non-F# use are the `NewXxx` factory methods.</para>
  /// </summary>
  [<NoComparison; SuppressMessage("Microsoft.Design", "CA1034",
                                  Justification = "Idiomatic F#");
                  SuppressMessage("Gendarme.Rules.Smells",
                                  "RelaxedAvoidCodeDuplicatedInSameClassRule",
                                  Justification = "Idiomatic F#");
    AutoSerializable(false)>]
  type CLIOptions =
    | Force of bool
    | FailFast of bool
    | ShowSummary of String
    | Many of CLIOptions seq

    /// <summary>
    /// <para type="description">The `/AltCoverForce` value this represents</para>
    /// </summary>
    member self.ForceDelete =
      match self with
      | Force b -> b
      | ShowSummary _
      | FailFast _ -> false
      | Many s -> s |> Seq.exists (fun f -> f.ForceDelete)

    /// <summary>
    /// <para type="description">The `/AltCoverFailFast` value this represents</para>
    /// </summary>
    member self.Fast =
      match self with
      | FailFast b -> b
      | ShowSummary _
      | Force _ -> false
      | Many s -> s |> Seq.exists (fun f -> f.Fast)

    /// <summary>
    /// <para type="description">The `/AltCoverShowSummary` value this represents</para>
    /// </summary>
    member self.Summary =
      match self with
      | ShowSummary b -> b
      | FailFast _
      | Force _ -> String.Empty
      | Many s ->
          match s
                |> Seq.map (fun f -> f.Summary)
                |> Seq.filter (String.IsNullOrWhiteSpace >> not)
                |> Seq.tryHead with
          | Some x -> x
          | _ -> String.Empty