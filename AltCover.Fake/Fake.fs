namespace AltCover.Fake

open AltCover
open Fake.Core

module Logging =
    let Default = { AltCover.Logging.Default with Info = Trace.trace
                                                  Warn = Trace.traceImportant
                                                  Error = Trace.traceError
                                                  Echo = Trace.traceVerbose
                  }