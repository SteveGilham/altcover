namespace AltCover.Recorder

open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.Threading

module Instance =
  module I =
    module private CallTrack =

      let attr =
        System.Runtime.Versioning.TargetFrameworkAttribute(".NETFramework,Version=v4.6")

      let value = AsyncLocal<Stack<int>>()

      let instance () =
        match value.Value with
        | null -> value.Value <- Stack<int>()
        | _ -> ()

        value.Value

[<assembly: SuppressMessage("Microsoft.Design",
                            "CA1014:MarkAssembliesWithClsCompliant",
                            Justification = "Bytecode delta only")>]
[<assembly: SuppressMessage("Microsoft.Design",
                            "CA1017:MarkAssembliesWithComVisible",
                            Justification = "Bytecode delta only")>]
[<assembly: SuppressMessage("Microsoft.Performance",
                            "CA1810:InitializeReferenceTypeStaticFieldsInline",
                            Scope = "member",
                            Target = "<StartupCode$AltCover-Async>.$Library.#.cctor()",
                            Justification = "Bytecode delta only")>]
[<assembly: SuppressMessage("Microsoft.Naming",
                            "CA1704:IdentifiersShouldBeSpelledCorrectly",
                            Scope = "type",
                            Target = "AltCover.Recorder.Instance+I",
                            MessageId = "I",
                            Justification = "Bytecode delta only")>]
[<assembly: SuppressMessage("Microsoft.Performance",
                            "CA1811:AvoidUncalledPrivateCode",
                            Scope = "member",
                            Target = "AltCover.Recorder.Instance+I+CallTrack.#get_attr()",
                            Justification = "Bytecode delta only")>]
[<assembly: SuppressMessage("Microsoft.Performance",
                            "CA1811:AvoidUncalledPrivateCode",
                            Scope = "member",
                            Target = "AltCover.Recorder.Instance+I+CallTrack.#instance()",
                            Justification = "Bytecode delta only")>]
[<assembly: SuppressMessage("Microsoft.Performance",
                            "CA1811:AvoidUncalledPrivateCode",
                            Scope = "member",
                            Target = "AltCover.Recorder.Instance+I+CallTrack.#get_value()",
                            Justification = "Bytecode delta only")>]
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidRepetitiveCallsToPropertiesRule",
                            Scope = "member", // MethodDefinition
                            Target = "AltCover.Recorder.Instance/I/CallTrack::instance()",
                            Justification = "Bytecode delta only")>]
()