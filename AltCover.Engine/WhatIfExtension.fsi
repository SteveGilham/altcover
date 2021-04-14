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
  ///</summary>
  /// <param name="prepare">The options</param>
  /// <returns>The validation outcome.</returns>
  val WhatIf : prepare:Abstract.IPrepareOptions -> AltCover.ValidatedCommandLine
end
[<Extension>]
///<summary>
/// `Abstract.ICollectOptions` extension methods
///</summary>
module CollectExtension = begin
  [<Extension>]
  ///<summary>
  /// Validates the supplied options
  ///</summary>
  /// <param name="collect">The options</param>
  /// <param name="afterPreparation">Values indicating whether the instrumentation has already taken place</param>
  /// <returns>The validation outcome.</returns>
  val WhatIf :
    collect:Abstract.ICollectOptions ->
      afterPreparation:bool -> AltCover.ValidatedCommandLine
end
// ```
// These provide C#-compatible extension methods to perform a `WhatIf` style command line validation
//
// `WhatIf` compiles the effective command-line and the result of `Validate`
//
// ## module `WhatIfExtension`
// ```
[<AutoOpen>]
///<summary>
/// F#-style I&lt;Whatever&gt;Options extension methods
///</summary>
module WhatIfExtension = begin
  ///<summary>
  /// F# style `Abstract.ICollectOptions` extension methods
  ///</summary>
  type Abstract.ICollectOptions with
    ///<summary>
    /// Validates the supplied options (the `this` parameter)
    ///</summary>
    /// <param name="afterPreparation">Values indicating whether the instrumentation has already taken place</param>
    /// <returns>The validation outcome.</returns>
    [<CompiledName("WhatIf")>]
    member WhatIf : afterPreparation:bool -> AltCover.AltCover.ValidatedCommandLine
  ///<summary>
  /// F# style `Abstract.IPrepareOptions` extension methods
  ///</summary>
  type Abstract.IPrepareOptions with
    ///<summary>
    /// Validates the supplied options (the `this` parameter)
    ///</summary>
    /// <returns>The validation outcome.</returns>
    [<CompiledName("WhatIf")>]
    member WhatIf : unit -> AltCover.AltCover.ValidatedCommandLine
end
//```
// provides seamless F# style extensions