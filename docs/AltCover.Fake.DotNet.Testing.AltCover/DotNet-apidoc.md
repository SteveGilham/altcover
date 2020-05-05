








# namespace `AltCoverFake.DotNet.Testing`
```
namespace AltCoverFake.DotNet.Testing
```








## module `DotNet`
```
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
```
Union type defining general command line arguments for `dotnet test` use.
case `Force` indicates a `/AltCoverForce` value
case `FailFast` inicates a `/AltCoverForce` value
case `ShowSummary` indicates a `/AltCoverShowSummary` value
case `Many` indicates a collection of cases

* value `Fast` gives the `/AltCoverFailFast` value this represents
* value `ForceDelete` gives the `/AltCoverForce` value this represents
* value `Summary` gives the `/AltCoverShowSummary` value this represents


































































