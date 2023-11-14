#if RUNNER
namespace AltCover.Fake

open System
open System.IO
open System.Reflection
open AltCover
open AltCover.Shared
open System.Diagnostics.CodeAnalysis

module Trace =
  open Fake.Core

  let Create () =
    AltCover.LoggingOptions.Primitive
      { Info = Trace.trace
        Warn = Trace.traceImportant
        Failure = Trace.traceError
        Echo = Trace.traceVerbose
        Verbose = Trace.traceVerbose }

  let internal doDefault (log: AltCover.LoggingOptions option) =
    match log with
    | Some logging -> logging
    | None -> Create()

[<NoComparison>]
type Implementation =
  | DotNetCore
  | Framework

[<SuppressMessage("Gendarme.Rules.Performance",
                  "AvoidUncalledPrivateCodeRule",
                  Justification = "Can't stop the instance constructor happening")>]
[<AbstractClass; Sealed>] // ~ Static class for methods with optional arguments
type Command private () =
  static member Prepare(args: Abstract.IPrepareOptions, ?log: AltCover.LoggingOptions) =
    AltCover.Command.Prepare args (Trace.doDefault log)

  static member Collect(args: Abstract.ICollectOptions, ?log: AltCover.LoggingOptions) =
    AltCover.Command.Collect args (Trace.doDefault log)

  static member ImportModule() = AltCover.Command.ImportModule()
  static member Version() = AltCover.Command.Version()
  // Finds the tool from within the .nuget package
  [<SuppressMessage("Microsoft.Design", "CA1062", Justification = "Idiomatic F#")>]
  static member ToolPath toolType =
    let here =
      Assembly.GetExecutingAssembly().Location

    let root =
      Path.Combine(Path.GetDirectoryName here, "../..")

    let target =
      match toolType with
      | Framework -> "AltCover.exe"
      | _ -> "AltCover.dll"

    match
      Directory.GetFiles(root, target, SearchOption.AllDirectories)
      |> Seq.tryHead
    with
    | Some path -> path |> Path.GetFullPath
    | None -> String.Empty
#else
namespace AltCoverFake.DotNet

open System
open System.Diagnostics.CodeAnalysis
open System.Reflection
open AltCoverFake.DotNet
open AltCover.Shared
#endif

[<SuppressMessage("Gendarme.Rules.Exceptions",
                  "InstantiateArgumentExceptionCorrectlyRule",
                  Justification = "Inlines Array functions")>]
[<SuppressMessage("Microsoft.Naming",
                  "CA1724",
                  Justification = "clash with '<StartupCode$ type")>]
[<AutoOpen>]
module DotNet =
  open Fake.DotNet

  type DotNet.TestOptions with

    member private self.ExtendCLIProperties options =
      let cliargs = self.MSBuildParams

      let nargs =
        { cliargs with
            Properties = List.concat [ cliargs.Properties; options ] }

      { self with MSBuildParams = nargs }

#if RUNNER
    member self.WithAltCoverOptions
      (prepare: Abstract.IPrepareOptions)
      (collect: Abstract.ICollectOptions)
      (force: DotNet.ICLIOptions)
      =
      DotNet.ToTestPropertiesList
#else
    member self.WithAltCoverOptions
      (prepare: Testing.Abstract.IPrepareOptions)
      (collect: Testing.Abstract.ICollectOptions)
      (force: AltCoverFake.DotNet.Testing.DotNet.ICLIOptions)
      =
      AltCoverFake.DotNet.Testing.DotNet.ToTestPropertiesList
#endif
        prepare
        collect
        force
      |> self.ExtendCLIProperties

    [<SuppressMessage("Microsoft.Naming", "CA1704", Justification = "Anonymous parameter")>]
    member self.WithAltCoverImportModule() =
#if RUNNER
      DotNet.ImportModuleProperties
#else
      AltCoverFake.DotNet.Testing.DotNet.ImportModuleProperties
#endif
      |> self.ExtendCLIProperties

    [<SuppressMessage("Microsoft.Naming", "CA1704", Justification = "Anonymous parameter")>]
    member self.WithAltCoverGetVersion() =
#if RUNNER
      DotNet.GetVersionProperties
#else
      AltCoverFake.DotNet.Testing.DotNet.GetVersionProperties
#endif
      |> self.ExtendCLIProperties

#if RUNNER
[<assembly: CLSCompliant(true)>]
[<assembly: System.Runtime.InteropServices.ComVisible(false)>]
[<assembly: SuppressMessage("Microsoft.Design",
                            "CA1034:NestedTypesShouldNotBeVisible",
                            Scope = "type",
                            Target = "AltCover.Fake.Implementation+Tags",
                            Justification = "Idiomatic F#")>]
#else
[<assembly: SuppressMessage("Microsoft.Naming",
                            "CA1704:IdentifiersShouldBeSpelledCorrectly",
                            Scope = "member",
                            Target =
                              "AltCoverFake.DotNet.Testing.AltCover+Parameters.#WithCreateProcess`1(Fake.Core.CreateProcess`1<!!0>)",
                            MessageId = "a",
                            Justification = "Generated code")>]
[<assembly: SuppressMessage("Microsoft.Naming",
                            "CA1724:TypeNamesShouldNotMatchNamespaces",
                            Scope = "type",
                            Target = "AltCoverFake.DotNet.Testing.DotNet",
                            Justification = "That's life I'm afraid")>]
#endif
[<assembly: SuppressMessage("Microsoft.Performance",
                            "CA1813:AvoidUnsealedAttributes",
                            Scope = "type",
                            Target =
                              "System.Diagnostics.CodeAnalysis.DynamicDependencyAttribute",
                            Justification = "Injected type")>]
()