namespace AltCover.Shared

[<AutoOpen>]
[<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Smells",
                                                  "AvoidSpeculativeGeneralityRule",
                                                  Justification = "Delegation is DRYing the codebase")>]
module internal StringExtension =

  let
#if !DEBUG
      inline
#endif
             (==) (x: string) (y: string) = x.Equals(y, System.StringComparison.Ordinal)
  let
#if !DEBUG
      inline
#endif
             (!=) (x: string) (y: string) = (x == y) |> not