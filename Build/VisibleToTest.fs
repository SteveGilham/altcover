namespace AltCover
open System.Reflection
open System.Runtime.CompilerServices
#if DEBUG
[<assembly: AssemblyConfiguration("Debug 1.4.x.x")>]
#else
[<assembly: AssemblyConfiguration("Release 1.4.x.x")>]
#endif

[<assembly: InternalsVisibleTo("AltCover.Shadow.Tests")>]
[<assembly: InternalsVisibleTo("AltCover.Tests")>]

()