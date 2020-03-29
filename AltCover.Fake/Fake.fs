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

  let internal DoDefault(log : FSApi.Logging option) =
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
type Api private () =
  static member Prepare(args : FSApi.PrepareParams, ?log : FSApi.Logging) =
    AltCover.Api.Prepare args (Trace.DoDefault log)
  static member Collect(args : FSApi.CollectParams, ?log : FSApi.Logging) =
    AltCover.Api.Collect args (Trace.DoDefault log)
  static member Ipmo() = AltCover.Api.Ipmo()
  static member Version() = AltCover.Api.Version()
  // Finds the tool from within the .nuget package
  [<SuppressMessage("Microsoft.Design", "CA1062",
                    Justification = "Idiomatic F#")>]
  static member toolPath toolType =
    let here = Assembly.GetExecutingAssembly().Location
    let root = Path.Combine(Path.GetDirectoryName here, "../..")

    let target =
      match toolType with
      | Framework _ -> "AltCover.exe"
      | _ -> "altcover.netcoreapp.dll"
    match Directory.GetFiles(root, target, SearchOption.AllDirectories)
          |> Seq.tryHead with
    | Some path -> path |> Path.GetFullPath
    | None -> String.Empty
#else
namespace AltCover_Fake.DotNet

open System
open System.Reflection
open AltCover_Fake.DotNet
#endif

[<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Smells",
  "AvoidSpeculativeGeneralityRule",
  Justification="Until we remove the [Obsolete] methods")>]
[<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Exceptions",
  "InstantiateArgumentExceptionCorrectlyRule",
  Justification="Inlines Array functions")>]
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
    member private self.ExtendCustomParams options =
      let custom = self.Common.CustomParams

      let extended =
        match custom with
        | None -> Some options
        | Some thing -> Some(thing + " " + options)

      // the constructors are version dependent
      let OptionsConstructor = self.Common.GetType().GetConstructors().[0]

      let args =
        OptionsConstructor.GetParameters()
        |> Array.map activate

      let common = OptionsConstructor.Invoke(args)
      self.Common.GetType().GetFields(BindingFlags.NonPublic ||| BindingFlags.Instance)
      |> Array.iter (setCustomParams common extended self.Common)
      let TestOptionsConstructor = self.GetType().GetConstructors().[0]

      let args' =
        TestOptionsConstructor.GetParameters()
        |> Array.map activate

      let result = TestOptionsConstructor.Invoke(args')
      self.GetType().GetFields(BindingFlags.NonPublic ||| BindingFlags.Instance)
      |> Array.iter (setCommonParams result common self)
      result :?> DotNet.TestOptions

#if RUNNER
    member self.WithAltCoverParameters (prepare : FSApi.PrepareParams)
           (collect : FSApi.CollectParams) (force : DotNet.CLIArgs) =
      DotNet.ToTestArguments
#else
    member self.WithAltCoverParameters (prepare : AltCover_Fake.DotNet.Testing.AltCover.PrepareParams)
           (collect : AltCover_Fake.DotNet.Testing.AltCover.CollectParams)
           (force : AltCover_Fake.DotNet.Testing.DotNet.CLIArgs) =
      AltCover_Fake.DotNet.Testing.Internals.ToTestArguments
#endif
        prepare collect force |> self.ExtendCustomParams
    member self.WithAltCoverImportModule() =
      self.ExtendCustomParams "/p:AltCoverIpmo=true"
    member self.WithAltCoverGetVersion() =
      self.ExtendCustomParams "/p:AltCoverGetVersion=true"

    [<Obsolete("Prefer equivalent member .WithAltCoverParameters")>]
    member self.WithParameters a b c = self.WithAltCoverParameters a b c

    [<Obsolete("Prefer equivalent member .WithAltCoverImportModule")>]
    member self.WithImportModule() = self.WithAltCoverImportModule()

    [<Obsolete("Prefer equivalent member .WithAltCoverGetVersion")>]
    member self.WithGetVersion() = self.WithAltCoverGetVersion()

#if RUNNER
[<assembly:CLSCompliant(true)>]
[<assembly:System.Runtime.InteropServices.ComVisible(false)>]
[<assembly:SuppressMessage("Microsoft.Design", "CA1034:NestedTypesShouldNotBeVisible",
                           Scope="type",
                           Target="AltCover.Fake.Implementation+Tags",
                           Justification = "Idiomatic F#")>]
()
#endif