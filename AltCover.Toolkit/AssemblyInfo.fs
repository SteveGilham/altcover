namespace AltCover

open System.Diagnostics.CodeAnalysis

[<assembly: System.CLSCompliant(true)>]
[<assembly: System.Runtime.InteropServices.ComVisible(false)>]
[<assembly: SuppressMessage("Microsoft.Performance",
                            "CA1813:AvoidUnsealedAttributes",
                            Scope = "type",
                            Target =
                              "System.Diagnostics.CodeAnalysis.DynamicDependencyAttribute",
                            Justification = "Injected type")>]
[<assembly: SuppressMessage("Microsoft.Performance",
                            "CA1810:InitializeReferenceTypeStaticFieldsInline",
                            Scope = "member",
                            Target = "<StartupCode$AltCover-Toolkit>.$OpenCover.#.cctor()",
                            Justification = "F# Compiler did this")>]
do ()