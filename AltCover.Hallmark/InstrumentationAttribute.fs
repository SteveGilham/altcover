namespace AltCover.Recorder

open System
open System.Diagnostics.CodeAnalysis

/// <summary>
/// An attribute to label an instrumented assembly by provenance
/// </summary>
[<AttributeUsage(AttributeTargets.Assembly)>]
[<Sealed>]
type InstrumentationAttribute() =
  class
    inherit Attribute()

    /// <summary>
    /// SHA-256 hash of the original assembly
    /// </summary>
    member val Assembly = "AltCover.Recorder.g!" with get, set

    /// <summary>
    /// SHA-256 hash of instrumentation parameters
    /// </summary>
    member val Configuration = "Uninstrumented!!" with get, set
  end

[<assembly: SuppressMessage("Gendarme.Rules.Design",
                            "MarkAssemblyWithAssemblyVersionRule",
                            Justification = "doing so fails static linkage because of attribute duplication")>]
[<assembly: SuppressMessage("Microsoft.Design", 
                            "CA1016:MarkAssembliesWithAssemblyVersion",
                             Justification="Exactly as above")>]
()