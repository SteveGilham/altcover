#if RUNNER
namespace AltCover
#else
namespace AltCoverFake.DotNet.Testing
#endif

[<RequireQualifiedAccess>]
module DotNet = begin
  [<NoComparison>]
  type CLIOptions =
    | Force of bool
    | FailFast of bool
    | ShowSummary of System.String
    | Many of seq<CLIOptions>
    with
      member Fast : bool
      member ForceDelete : bool
      member Summary : System.String
    end
  end