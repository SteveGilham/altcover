namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.Runtime.InteropServices

[<assembly: CLSCompliant(true)>]
[<assembly: ComVisible(false)>]
()

[<SuppressMessage("Gendarme.Rules.Smells",
                  "AvoidSpeculativeGeneralityRule",
                  Justification = "Forwards from .exe to .dll for xplat consistency")>]
module EntryPoint =
  [<EntryPoint>]
  [<ExcludeFromCodeCoverage>]
  let private main arguments =
    let dotnet =
#if !NET472
      true
#else
      false
#endif

    AltCover.Main.main dotnet arguments