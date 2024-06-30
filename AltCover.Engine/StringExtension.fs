namespace AltCover.Shared

open System.Diagnostics.CodeAnalysis
open System.Runtime.CompilerServices

[<AutoOpen>]
[<SuppressMessage("Gendarme.Rules.Smells",
                  "AvoidSpeculativeGeneralityRule",
                  Justification = "Delegation is DRYing the codebase")>]
module internal StringExtension =
#if !DEBUG
  [<MethodImplAttribute(MethodImplOptions.AggressiveInlining)>]
#endif
  let (==) (x: string) (y: string) =
    x.Equals(y, System.StringComparison.Ordinal)

#if !FAKEAPI
#if !DEBUG
  [<MethodImplAttribute(MethodImplOptions.AggressiveInlining)>]
#endif
  let (!=) (x: string) (y: string) = (x == y) |> not
#endif