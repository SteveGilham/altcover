#if RUNNER
// # namespace AltCover.Fake
// ```
namespace AltCover.Fake
// ```
// ## module `Trace`
// ```
module Trace =
  val Create: unit -> AltCover.AltCover.LoggingOptions
// ```
// Returns an instance of the core API Logging type that hooks into the `Fake.Core.Trace` facilities
//
// ## type `Implementation`
// ```
[<NoComparison>]
type Implementation =
  | DotNetCore
  | Framework
// ```
// to indicate which command-line executable from the current NuGet package to return
//
// ## type `Command`
// ```
[<Sealed; AbstractClass>]
type Command =
  static member Collect : args:AltCover.Abstract.ICollectOptions * ?log:AltCover.AltCover.LoggingOptions -> int
  static member ImportModule : unit -> string
  static member Prepare : args:AltCover.Abstract.IPrepareOptions * ?log:AltCover.AltCover.LoggingOptions -> int
  static member Version : unit -> System.Version
  static member ToolPath : Implementation -> string
// ```
// wraps the core API functions.  If the optional logging argument is not given, then `AltCover.Fake.Trace.Default` is assumed.
//
// The `int` results are 0 for success and otherwise for failure (this would be the return code of the operation if run as a command-line function); and string return is the location of the indicated command-line executable from the current NuGet package
//
// ## Extension methods for type `Fake.DotNet.DotNet.TestOptions` (in module `AltCover.Fake.DotNet`)
#else
// # namespace `AltCoverFake.DotNet`
// ```
namespace AltCoverFake.DotNet
// ```
// ## Extension methods for type `Fake.DotNet.DotNet.TestOptions` (in module `AltCoverFake.DotNet.DotNet`)
#endif
// ```
module DotNet =
  type Fake.DotNet.DotNet.TestOptions with
// ```
#if RUNNER
// ```
    member WithAltCoverOptions: AltCover.Abstract.IPrepareOptions -> AltCover.Abstract.ICollectOptions ->
                                    AltCover.DotNet.CLIOptions -> Fake.DotNet.DotNet.TestOptions
// ```
#else
// ```
    member WithAltCoverOptions: Testing.Abstract.IPrepareOptions ->
                                Testing.Abstract.ICollectOptions ->
                                AltCoverFake.DotNet.Testing.DotNet.CLIOptions ->
                                Fake.DotNet.DotNet.TestOptions
// ```
#endif
// Adds the result of `DotNet.ToTestArguments` to the `CustomParams` member of the `Common` member
// ```
    member WithAltCoverImportModule: unit -> Fake.DotNet.DotNet.TestOptions
// ```
// Adds `"/p:AltCoverImportModule=true"` to the `CustomParams` member of the `Common` member
// ```
    member WithAltCoverGetVersion: unit -> Fake.DotNet.DotNet.TestOptions
// ```
// Adds `"/p:AltCoverGetVersion=true"` to the `CustomParams` member of the `Common` member