namespace AltCover.Fake

open System
open System.Reflection
open AltCover

module Trace =
  open Fake.Core

  let Default =
    { AltCover.Logging.Default with Info = Trace.trace
                                    Warn = Trace.traceImportant
                                    Error = Trace.traceError
                                    Echo = Trace.traceVerbose }

  let internal DoDefault(log : Logging option) =
    match log with
    | Some logging -> logging
    | None -> Default

type Api =
  static member Prepare(args : PrepareParams, ?log : Logging) =
    AltCover.Api.Prepare args (Trace.DoDefault log)
  static member Collect(args : CollectParams, ?log : Logging) =
    AltCover.Api.Collect args (Trace.DoDefault log)
  static member Ipmo() = AltCover.Api.Ipmo()
  static member Version() = AltCover.Api.Version()

module DotNet =
  open Fake.DotNet

  type DotNet.TestOptions with
    member self.WithParameters (prepare : PrepareParams) (collect : CollectParams) =
      let options = AltCover.DotNet.ToTestArguments prepare collect
      let custom = self.Common.CustomParams

      let extended =
        match custom with
        | None -> Some options
        | Some thing -> Some(thing + " " + options)

      // was
      //      { self with Common = { self.Common with CustomParams = extended } }
      // which really means
      // 	return new global::Fake.DotNet.DotNet.TestOptions(
      //new global::Fake.DotNet.DotNet.Options(
      //        common.DotNetCliPath,
      //        common.WorkingDirectory,
      //        extended,
      //        common.Verbosity,
      //        common.Diagnostics,
      //        common.RedirectOutput,
      //        common.Environment),
      //    self.Settings,
      //    self.ListTests,
      //    self.Filter,
      //    self.TestAdapterPath,
      //    self.Logger,
      //    self.Configuration,
      //    self.Framework,
      //    self.Output,
      //    self.Diag,
      //    self.NoBuild,
      //    self.ResultsDirectory,
      //    self.Collect,
      //    self.NoRestore,
      //    self.RunSettingsArguments);
      // where the constructors are version dependent
      let OptionsConstructor = self.Common.GetType().GetConstructors().[0]

      let args =
        OptionsConstructor.GetParameters()
        |> Array.map (fun info ->
             let t = info.ParameterType
             if t.GetTypeInfo().IsValueType then Activator.CreateInstance(t)
             else null)

      let common = OptionsConstructor.Invoke(args)
      self.Common.GetType().GetFields(BindingFlags.NonPublic ||| BindingFlags.Instance)
      |> Array.iter (fun f ->
           f.SetValue(common,
                      if f.Name <> "CustomParams@" then f.GetValue self.Common
                      else extended :> obj))
      let TestOptionsConstructor = self.GetType().GetConstructors().[0]

      let args' =
        TestOptionsConstructor.GetParameters()
        |> Array.map (fun info ->
             let t = info.ParameterType
             if t.GetTypeInfo().IsValueType then Activator.CreateInstance(t)
             else null)

      let result = TestOptionsConstructor.Invoke(args')
      self.GetType().GetFields(BindingFlags.NonPublic ||| BindingFlags.Instance)
      |> Array.iter (fun f ->
           f.SetValue(result,
                      if f.Name <> "Common@" then f.GetValue self
                      else common))
      result :?> DotNet.TestOptions