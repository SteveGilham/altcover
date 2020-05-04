#if RUNNER
namespace AltCover.FSApi
open AltCover.FSApi.CLIInternals
#else
namespace AltCover.Fake.DotNet.Testing
open AltCoverFake.DotNet.Testing.CLIInternals
#endif

open System

[<RequireQualifiedAccess>]
#if RUNNER
module DotNetCLI =
#else
module internal DotNetCLI =
#endif
#if RUNNER
  let ToTestArgumentList (prepare : AltCover.OptionApi.PrepareOptions)
      (collect : AltCover.OptionApi.CollectOptions) (options : DotNet.CLIOptions) =
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

#if RUNNER
  let ToTestArguments (prepare : AltCover.OptionApi.PrepareOptions)
      (collect : AltCover.OptionApi.CollectOptions) (options : DotNet.CLIOptions) =
    ToTestArgumentList prepare collect options |> join
#else
  let toTestArguments (prepare : AltCoverFake.DotNet.Testing.AltCover.PrepareOptions)
      (collect : AltCoverFake.DotNet.Testing.AltCover.CollectOptions)
      (options : DotNet.CLIOptions) =
    toTestArgumentList prepare collect options |> join
#endif