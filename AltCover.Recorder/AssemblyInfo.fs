namespace AltCover.Recorder

open System
open System.Reflection
open System.Runtime.InteropServices

#if DEBUG
[<assembly: AssemblyTitle("DEBUG Recorder") >]
#else
[<assembly: AssemblyTitle("Non-functional RELEASE Recorder") >]
#endif

[<assembly: CLSCompliant(true)>]
[<assembly: ComVisible(false)>] 
()


