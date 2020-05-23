// # namespace `AltCover`
// ```
namespace AltCover

open System.Runtime.CompilerServices /// no doc

// ```
// ## module `PrepareExtension` and module `CollectExtension`
// ```
[<Extension>]
///<summary>
/// `Abstract.IPrepareOptions` extension methods
///</summary>
module PrepareExtension = begin
  [<Extension>]
  ///<summary>
  /// Validates the supplied options
  /// <param name="prepare">The options</param>
  /// <returns>The validation outcome.</returns>
  ///</summary>
  val WhatIf : prepare:Abstract.IPrepareOptions -> AltCover.ValidatedCommandLine
end
[<Extension>]
///<summary>
/// `Abstract.ICollectOptions` extension methods
///</summary>
module CollectExtension = begin
  [<Extension>]
  val WhatIf :
  ///<summary>
  /// Validates the supplied options
  /// <param name="collect">The options</param>
  /// <param name="afterPreparation">Values indicating whether the instrumentation has already taken place</param>
  /// <returns>The validation outcome.</returns>
  ///</summary>
    collect:Abstract.ICollectOptions ->
      afterPreparation:bool -> AltCover.ValidatedCommandLine
end
// ```
// These provide C#-compatible extension methods to perform a `WhatIf` style command line validation
//
// `WhatIf` compiles the effective command-line and the result of `Validate`