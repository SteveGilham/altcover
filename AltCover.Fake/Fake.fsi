#if RUNNER
namespace AltCover.Fake

module Trace =
  val Create: unit -> AltCover.FSApi.Logging

[<NoComparison>]
type Implementation =
  | DotNetCore
  | Framework

[<Sealed; AbstractClass>]
type Api =
  static member Collect : args:AltCover.FSApi.CollectParameters * ?log:AltCover.FSApi.Logging -> int
  static member ImportModule : unit -> string
  static member Prepare : args:AltCover.FSApi.PrepareParameters * ?log:AltCover.FSApi.Logging -> int
  static member Version : unit -> System.Version
  static member ToolPath : Implementation -> string

#else
namespace AltCoverFake.DotNet
#endif

module DotNet =
  type Fake.DotNet.DotNet.TestOptions with

#if RUNNER
    member WithAltCoverParameters: AltCover.FSApi.PrepareParameters -> AltCover.FSApi.CollectParameters ->
                                    AltCover.DotNet.CLIOptions -> Fake.DotNet.DotNet.TestOptions
#else
    member WithAltCoverParameters: AltCoverFake.DotNet.Testing.AltCover.PrepareParameters ->
                                   AltCoverFake.DotNet.Testing.AltCover.CollectParameters ->
                                   AltCoverFake.DotNet.Testing.DotNet.CLIOptions ->
                                   Fake.DotNet.DotNet.TestOptions
#endif
    member WithAltCoverImportModule: unit -> Fake.DotNet.DotNet.TestOptions
    member WithAltCoverGetVersion: unit -> Fake.DotNet.DotNet.TestOptions