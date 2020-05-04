namespace AltCover.FSApi

open System.Diagnostics.CodeAnalysis

[<assembly:System.CLSCompliant(true)>]
[<assembly:System.Runtime.InteropServices.ComVisible(false)>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
  Scope="namespace", Target="AltCover.FSApi", MessageId="Api",
  Justification="It's an API, damn it!")>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
   MessageId="Api",
   Justification="Ditto, ditto.")>]
do ()