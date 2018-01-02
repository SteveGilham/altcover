namespace AltCover

open System
open System.Reflection
open System.Runtime.InteropServices

[<assembly: CLSCompliant(true)>]
[<assembly: ComVisible(false)>]

#if NETSTANDARD2_0
[<assembly:AssemblyKeyFileAttribute("Infrastructure.snk")>]
#endif
()