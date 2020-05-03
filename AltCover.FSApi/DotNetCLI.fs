#if RUNNER
namespace AltCover
open AltCover.Internals
#else
namespace AltCoverFake.DotNet.Testing
open AltCoverFake.DotNet.Testing.Internals
#endif

open System

[<RequireQualifiedAccess>]
#if RUNNER
module DotNetCLI =
#else
module internal DotNetCLI =
#endif
  /// <summary>
  /// Converts the input into the command line for `dotnet test`
  /// </summary>
  /// <param name="prepare">Description of the instrumentation operation to perform</param>
  /// <param name="collect">Description of the collection operation to perform</param>
  /// <param name="options">All other `altcover` related command line arguments</param>
  /// <returns>The command line as a sequence of individual items</returns>
#if RUNNER
  let ToTestArgumentList (prepare : AltCover.FSApi.PrepareOptions)
      (collect : AltCover.FSApi.CollectOptions) (options : DotNet.CLIOptions) =
#else
  let toTestArgumentList (prepare : AltCoverFake.DotNet.Testing.AltCover.PrepareOptions)
      (collect : AltCoverFake.DotNet.Testing.AltCover.CollectOptions)
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
  let ToTestArguments (prepare : AltCover.FSApi.PrepareOptions)
      (collect : AltCover.FSApi.CollectOptions) (options : DotNet.CLIOptions) =
    ToTestArgumentList prepare collect options |> join
#else
  let toTestArguments (prepare : AltCoverFake.DotNet.Testing.AltCover.PrepareOptions)
      (collect : AltCoverFake.DotNet.Testing.AltCover.CollectOptions)
      (options : DotNet.CLIOptions) =
    toTestArgumentList prepare collect options |> join
#endif