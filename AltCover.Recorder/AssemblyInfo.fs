namespace AltCover.Recorder

open System
open System.Runtime.InteropServices

[<assembly: CLSCompliant(true)>]
[<assembly: ComVisible(false)>]

#if NETSTANDARD2_0
[<assembly:System.Reflection.AssemblyKeyFileAttribute("Infrastructure.snk")>]
#endif

()