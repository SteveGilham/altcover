#if RUNNER
namespace AltCover.Fake

open System
open System.IO
open System.Reflection
open AltCover
open System.Diagnostics.CodeAnalysis

module Trace =
  open Fake.Core

  let Create() =
    FSApi.Logging.Primitive
      { Primitive.Logging.Create() with
          Info = Trace.trace
          Warn = Trace.traceImportant
          Error = Trace.traceError
          Echo = Trace.traceVerbose }

  let internal doDefault(log : FSApi.Logging option) =
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
[<SuppressMessage("Microsoft.Naming", "CA1704",
  Justification="'Api' works")>]
[<AbstractClass; Sealed>] // ~ Static class for methods with optional arguments
type Api private () =
  static member Prepare(args : FSApi.PrepareParameters, ?log : FSApi.Logging) =
    AltCover.Api.Prepare args (Trace.doDefault log)
  static member Collect(args : FSApi.CollectParameters, ?log : FSApi.Logging) =
    AltCover.Api.Collect args (Trace.doDefault log)
  static member ImportModule() = AltCover.Api.ImportModule()
  static member Version() = AltCover.Api.Version()
  // Finds the tool from within the .nuget package
  [<SuppressMessage("Microsoft.Design", "CA1062",
                    Justification = "Idiomatic F#")>]
  static member ToolPath toolType =
    let here = Assembly.GetExecutingAssembly().Location
    let root = Path.Combine(Path.GetDirectoryName here, "../..")

    let target =
      match toolType with
      | Framework _ -> "AltCover.exe"
      | _ -> "AltCover.NetCoreApp.dll"
    match Directory.GetFiles(root, target, SearchOption.AllDirectories)
          |> Seq.tryHead with
    | Some path -> path |> Path.GetFullPath
    | None -> String.Empty
#else
namespace AltCoverFake.DotNet

open System
open System.Diagnostics.CodeAnalysis
open System.Reflection
open AltCoverFake.DotNet
#endif

[<SuppressMessage("Gendarme.Rules.Exceptions",
  "InstantiateArgumentExceptionCorrectlyRule",
  Justification="Inlines Array functions")>]
[<SuppressMessage("Microsoft.Naming",
  "CA1724",
  Justification="clash with '<StartupCode$ type")>]
module DotNet =
  open Fake.DotNet

  let internal activate (info : ParameterInfo) =
    let t = info.ParameterType
    if t.GetTypeInfo().IsValueType then Activator.CreateInstance(t) else null

  let internal setCustomParams common extended current (f : FieldInfo) =
    f.SetValue
             (common,
              (if f.Name <> "CustomParams@" then
                f.GetValue current
               else
                 extended :> obj))

  let internal setCommonParams result common self (f : FieldInfo) =
    f.SetValue
             (result,
              (if f.Name <> "Common@" then f.GetValue self else common))

  type DotNet.TestOptions with

    // NOTE: the MSBuildParams member of TestOptions did not exist in Fake 5.0.0
    // so do it this way for backwards compatibility
    [<SuppressMessage("Microsoft.Usage", "CA2208",
      Justification="Inlined calls to ArgumentNullException")>]
    member private self.ExtendCustomParams options =
      let custom = self.Common.CustomParams

      let extended =
        match custom with
        | None -> Some options
        | Some thing -> Some(thing + " " + options)

      // the constructors are version dependent
      let optionsConstructor = self.Common.GetType().GetConstructors().[0]

      let args =
        optionsConstructor.GetParameters()
        |> Array.map activate

      let common = optionsConstructor.Invoke(args)
      self.Common.GetType().GetFields(BindingFlags.NonPublic ||| BindingFlags.Instance)
      |> Array.iter (setCustomParams common extended self.Common)
      let testOptionsConstructor = self.GetType().GetConstructors().[0]

      let args' =
        testOptionsConstructor.GetParameters()
        |> Array.map activate

      let result = testOptionsConstructor.Invoke(args')
      self.GetType().GetFields(BindingFlags.NonPublic ||| BindingFlags.Instance)
      |> Array.iter (setCommonParams result common self)
      result :?> DotNet.TestOptions

#if RUNNER
    member self.WithAltCoverParameters (prepare : FSApi.PrepareParameters)
           (collect : FSApi.CollectParameters) (force : DotNet.CLIArgs) =
      DotNet.ToTestArguments
#else
    member self.WithAltCoverParameters (prepare : AltCoverFake.DotNet.Testing.AltCover.PrepareParameters)
           (collect : AltCoverFake.DotNet.Testing.AltCover.CollectParameters)
           (force : AltCoverFake.DotNet.Testing.DotNet.CLIArgs) =
      AltCoverFake.DotNet.Testing.Internals.toTestArguments
#endif
        prepare collect force |> self.ExtendCustomParams
    [<SuppressMessage("Microsoft.Naming", "CA1704",
      Justification="Anonymous parameter")>]
    member self.WithAltCoverImportModule() =
      self.ExtendCustomParams "/p:AltCoverImportModule=true"
    [<SuppressMessage("Microsoft.Naming", "CA1704",
      Justification="Anonymous parameter")>]
    member self.WithAltCoverGetVersion() =
      self.ExtendCustomParams "/p:AltCoverGetVersion=true"

#if RUNNER
[<assembly:CLSCompliant(true)>]
[<assembly:System.Runtime.InteropServices.ComVisible(false)>]
[<assembly:SuppressMessage("Microsoft.Design", "CA1034:NestedTypesShouldNotBeVisible",
                           Scope="type",
                           Target="AltCover.Fake.Implementation+Tags",
                           Justification = "Idiomatic F#")>]
#else
[<assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", Scope="member", Target="AltCoverFake.DotNet.Testing.AltCover+Parameters.#WithCreateProcess`1(Fake.Core.CreateProcess`1<!!0>)", MessageId="a", Justification="Generated code")>]
[<assembly: SuppressMessage("Microsoft.Performance", "CA1823:AvoidUnusedPrivateFields", Scope="member", Target="AltCoverFake.DotNet.Testing.AltCover+withMono@699T.#monoPath", Justification="Generated code")>]
[<assembly: SuppressMessage("Microsoft.Performance", "CA1823:AvoidUnusedPrivateFields", Scope="member", Target="AltCoverFake.DotNet.Testing.AltCover+withMono@699T.#parameters", Justification="Generated code")>]
[<assembly: SuppressMessage("Microsoft.Performance", "CA1823:AvoidUnusedPrivateFields", Scope="member", Target="AltCoverFake.DotNet.Testing.AltCover+withWorkingDirectory@659T.#parameters", Justification="Generated code")>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1724:TypeNamesShouldNotMatchNamespaces", Scope="type", Target="AltCoverFake.DotNet.Testing.DotNet", Justification="That's life I'm afraid")>]
#endif
()