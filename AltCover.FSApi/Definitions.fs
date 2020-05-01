#if RUNNER
namespace AltCover
#else
namespace AltCoverFake.DotNet.Testing
#endif

open System
open System.Diagnostics.CodeAnalysis
open System.Linq

[<assembly:CLSCompliant(true)>]
[<assembly:System.Runtime.InteropServices.ComVisible(false)>]
do ()

#if RUNNER
/// <summary>
/// <para type="description">Index used to order branches for branch visit computation.</para>
/// </summary>
type Ordinal =
| Offset = 0
| SL = 1
#endif

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

#if !RUNNER
module internal Internals =
#endif

  let private arg name s = (sprintf """/p:AltCover%s="%s" """ name s).Trim()
  let private listArg name (s : String seq) =
    (sprintf """/p:AltCover%s="%s" """ name <| String.Join("|", s)).Trim()

  let private isSet s =
    s
    |> String.IsNullOrWhiteSpace
    |> not

  let private fromList name (s : String seq) = (listArg name s, s.Any())
  let private fromArg name s = (arg name s, isSet s)
  let private join(l : string seq) = String.Join(" ", l)

  [<SuppressMessage("Gendarme.Rules.Design.Generic", "AvoidMethodWithUnusedGenericTypeRule",
                     Justification="Compiler Generated")>]
#if RUNNER
  let internal toPrepareListArgumentList (prepare : AltCover.FSApi.PrepareParameters) =
#else
  let internal toPrepareListArgumentList (prepare : AltCoverFake.DotNet.Testing.AltCover.PrepareParameters) =
#endif
    [
      fromList, "SymbolDirectories", prepare.SymbolDirectories
      fromList, "DependencyList", prepare.Dependencies
      fromList, "Keys", prepare.Keys
      fromList, "FileFilter", prepare.FileFilter
      fromList, "AssemblyFilter", prepare.AssemblyFilter
      fromList, "AssemblyExcludeFilter", prepare.AssemblyExcludeFilter
      fromList, "TypeFilter", prepare.TypeFilter
      fromList, "MethodFilter", prepare.MethodFilter
      fromList, "AttributeFilter", prepare.AttributeFilter
      fromList, "PathFilter", prepare.PathFilter
      fromList, "CallContext", prepare.CallContext
    ]

#if RUNNER
  let internal toPrepareFromArgArgumentList (prepare : AltCover.FSApi.PrepareParameters) =
#else
  let internal toPrepareFromArgArgumentList (prepare : AltCoverFake.DotNet.Testing.AltCover.PrepareParameters) =
#endif
    [
      fromArg, "StrongNameKey", prepare.StrongNameKey
      fromArg, "XmlReport", prepare.XmlReport
      fromArg, "ReportFormat", prepare.ReportFormat
      fromArg, "ShowStatic", prepare.ShowStatic
    ]

#if RUNNER
  let internal toPrepareArgArgumentList (prepare : AltCover.FSApi.PrepareParameters) =
#else
  let internal toPrepareArgArgumentList (prepare : AltCoverFake.DotNet.Testing.AltCover.PrepareParameters) =
#endif
    [
      (arg, "ZipFile", "false", prepare.ZipFile)
      (arg, "MethodPoint", "false", prepare.MethodPoint)
      (arg, "Single", "false", prepare.Single)
      (arg, "LineCover", "true", prepare.LineCover)
      (arg, "BranchCover", "true", prepare.BranchCover)
      (arg, "SourceLink", "false", prepare.SourceLink)
      (arg, "LocalSource", "true", prepare.LocalSource)
      (arg, "VisibleBranches", "true", prepare.VisibleBranches)
      (arg, "ShowGenerated", "true", prepare.ShowGenerated)
    ]

#if RUNNER
  let internal toCollectFromArgArgumentList (collect : AltCover.FSApi.CollectParameters) =
#else
  let internal toCollectFromArgArgumentList (collect : AltCoverFake.DotNet.Testing.AltCover.CollectParameters) =
#endif
    [
      fromArg, "LcovReport", collect.LcovReport
      fromArg, "Cobertura", collect.Cobertura
      fromArg, "Threshold", collect.Threshold
      fromArg, "SummaryFormat", collect.SummaryFormat
    ]

  [<SuppressMessage("Gendarme.Rules.Naming", "AvoidRedundancyInMethodNameRule",
                     Justification="Internal implementation detail")>]
#if RUNNER
  let internal toCLIOptionsFromArgArgumentList (options : CLIOptions) =
#else
  let internal toCLIOptionsFromArgArgumentList (options : DotNet.CLIOptions) =
#endif
    [
      fromArg, "ShowSummary", options.Summary
    ]

  [<SuppressMessage("Gendarme.Rules.Naming", "AvoidRedundancyInMethodNameRule",
                     Justification="Internal implementation detail")>]
#if RUNNER
  let internal toCLIOptionsArgArgumentList (options : CLIOptions) =
#else
  let internal toCLIOptionsArgArgumentList (options : DotNet.CLIOptions) =
#endif
    [
      arg, "Force", "true", options.ForceDelete
      arg, "FailFast", "true", options.Fast
    ]

  /// <summary>
  /// Converts the input into the command line for `dotnet test`
  /// </summary>
  /// <param name="prepare">Description of the instrumentation operation to perform</param>
  /// <param name="collect">Description of the collection operation to perform</param>
  /// <param name="options">All other `altcover` related command line arguments</param>
  /// <returns>The command line as a sequence of individual items</returns>
#if RUNNER
  let ToTestArgumentList (prepare : AltCover.FSApi.PrepareParameters)
      (collect : AltCover.FSApi.CollectParameters) (options : CLIOptions) =
#else
  let toTestArgumentList (prepare : AltCoverFake.DotNet.Testing.AltCover.PrepareParameters)
      (collect : AltCoverFake.DotNet.Testing.AltCover.CollectParameters)
      (options : DotNet.CLIOptions) =
#endif
    [
      [ fromArg String.Empty "true" ]
      prepare
      |> toPrepareListArgumentList
      |> List.map(fun (f,n,a) -> f n a)
      prepare
      |> toPrepareFromArgArgumentList
      |> List.map(fun (f,n,a) -> f n a)
      prepare
      |> toPrepareArgArgumentList
      |> List.map(fun (f,n,a,x) -> (f n a,x))

      collect
      |> toCollectFromArgArgumentList
      |> List.map(fun (f,n,a) -> f n a)

      options
      |> toCLIOptionsFromArgArgumentList
      |> List.map(fun (f,n,a) -> f n a)

      options
      |> toCLIOptionsArgArgumentList
      |> List.map(fun (f,n,a,x) -> (f n a,x))
    ]
    |> List.concat
    |> List.filter snd
    |> List.map fst

  /// <summary>
  /// Converts the input into the command line for `dotnet test`
  /// </summary>
  /// <param name="prepare">Description of the instrumentation operation to perform</param>
  /// <param name="collect">Description of the collection operation to perform</param>
  /// <param name="options">All other `altcover` related command line arguments</param>
  /// <returns>The composed command line</returns>
#if RUNNER
  let ToTestArguments (prepare : AltCover.FSApi.PrepareParameters)
      (collect : AltCover.FSApi.CollectParameters) (options : CLIOptions) =
    ToTestArgumentList prepare collect options |> join
#else
  let toTestArguments (prepare : AltCoverFake.DotNet.Testing.AltCover.PrepareParameters)
      (collect : AltCoverFake.DotNet.Testing.AltCover.CollectParameters)
      (options : DotNet.CLIOptions) =
    toTestArgumentList prepare collect options |> join
#endif