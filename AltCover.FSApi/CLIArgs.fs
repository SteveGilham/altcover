#if RUNNER
namespace AltCover

[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>] // work around coverlet attribute bug
module DotNetTest =
#else
[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage; RequireQualifiedAccess>] // work around coverlet attribute bug
module AltCover_Fake.DotNet.Testing.DotNetTest
#endif
  [<NoComparison; System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design",
                                                                  "CA1034",
                                                                  Justification = "Idiomatic F#")>]

  type CLIArgs =
    | Force of bool
  with member self.ForceDelete =
        match self with
        | Force b -> b