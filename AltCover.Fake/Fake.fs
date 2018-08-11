namespace AltCover.Fake

open System.Runtime.InteropServices

open AltCover
open Fake.Core

module Trace =
  let Default = { AltCover.Logging.Default with Info = Trace.trace
                                                Warn = Trace.traceImportant
                                                Error = Trace.traceError
                                                Echo = Trace.traceVerbose
                }
  let internal DoDefault (log:Logging option) =
     match log with
     | Some logging -> logging
     | None -> Default

type Api =
  static member Prepare (args:PrepareParams, ?log:Logging) =
    AltCover.Api.Prepare args (Trace.DoDefault log)
  static member Collect (args:CollectParams, ?log:Logging) =
    AltCover.Api.Collect args (Trace.DoDefault log)
  static member Ipmo () =
    AltCover.Api.Ipmo ()
  static member Version () =
    AltCover.Api.Version ()