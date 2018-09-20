namespace AltCover.Fake

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
      { self with Common = { self.Common with CustomParams = extended } }