# The AltCover.Fake package -- AltCover.Fake.DotNet.Testing.dll

This provides helpers to drive command-line AltCover (or `dotnet test` with AltCover) from any of the other packages.  In these scenarios, AltCover operates outside the Fake build process.
The slightly awkward `AltCoverFake` namespace was chosen to allow co-existence with the previous in-process API's `AltCover.Fake` names.

Fake versions are normally supported for six months after release (when Fake itself deprecates old versions), but deprecation of older versions is not eager.  Check the AltCover release notes to see how far back support actually extends : see [here](https://github.com/SteveGilham/altcover/blob/master/ReleaseNotes.md) and [here](https://github.com/SteveGilham/altcover/blob/master/ReleaseNotes%20-%20Previously.md).

**NOTE:** dotnet SDK v7.0.100 requires special treatment for driving AltCover through `dotnet test` (only; not as a command).  See [here](https://github.com/SteveGilham/altcover/wiki/dotnet-SDK-7.0.100) and [here](https://github.com/SteveGilham/altcover/wiki/Release-8.5.841).

## Composing the AltCover command line

### Basic elements

Start with one of these

* [`module Abstract`](Abstract-fsapidoc) -- This holds the weakly ("stringly") typed equivalent of the command line options in a C# friendly manner as interfaces with the values expressed as read-only properties.
* [`module Primitive`](Primitive-fsapidoc) -- This holds the weakly ("stringly") typed equivalent of the command line options.
* [`module TypeSafe`](TypeSafe-fsapidoc) -- This holds the strongly-typed equivalent of the command line options.

### The common expression of the command line elements

Make one of these

* [`module AltCover`](AltCover-fsapidoc) -- This represents the AltCover command line options and its validation.

### Use with `dotnet test` 

* [`module DotNet`](DotNet-fsapidoc) -- This represents the further `dotnet test` command line options.
* [`module Fake`](Fake-fsapidoc) -- This is the Fake toolkit integration via extension methods for type `Fake.DotNet.DotNet.TestOptions`.

### Use as a command

* [`module AltCoverCommand`](AltCoverCommand-fsapidoc) -- This represents the various `AltCover` operations available, and their execution from a Fake script.

### Example
(based on [the AltCover build script here](https://github.com/SteveGilham/altcover/blob/master/Build/targets.fsx#L984-L1004))

```fsharp
!!./docs/AltCover.Fake.DotNet.Testing.AltCover/BuildSample_1.fsx
```

### Example
(based on [the AltCover build script here](https://github.com/SteveGilham/altcover/blob/master/Build/targets.fsx#L3578-L3591))

```fsharp
!!./docs/AltCover.Fake.DotNet.Testing.AltCover/BuildSample_2.fsx

```
