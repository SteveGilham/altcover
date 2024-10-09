namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.Reflection
open System.Runtime.InteropServices

[<assembly: CLSCompliant(true)>]
[<assembly: ComVisible(false)>]
[<assembly: AssemblyTitle("AltCover.UICommon")>]
[<assembly: AssemblyDescription("Common code for coverage and static analysis visualizer for NCover (possibly extended) and OpenCover")>]
[<assembly: System.Resources.NeutralResourcesLanguageAttribute("en-GB")>]
[<assembly: SuppressMessage("Microsoft.Performance",
                            "CA1813:AvoidUnsealedAttributes",
                            Scope = "type",
                            Target =
                              "System.Diagnostics.CodeAnalysis.DynamicDependencyAttribute",
                            Justification = "Injected type")>]

[<assembly: SuppressMessage("Microsoft.Performance",
                            "CA1810:InitializeReferenceTypeStaticFieldsInline",
                            Scope = "member",
                            Target =
                              "<StartupCode$AltCover-UICommon>.$NativeJson.#.cctor()",
                            Justification = "F# Compiler did this")>]
()