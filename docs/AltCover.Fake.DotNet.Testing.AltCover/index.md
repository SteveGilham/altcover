# The AltCover.Fake package -- AltCover.Fake.DotNet.Testing.dll

This provides helpers to drive command-line AltCover (or `dotnet test` with AltCover) from any of the other packages.  In these scenarios, AltCover operates outside the Fake build process.
The slightly awkward `AltCoverFake` namespace was chosen to allow co-existence with the previous in-process API's `AltCover.Fake` names.

Requires Fake 5.18 or later

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

```
#r "paket:
nuget AltCover.Fake >= 7.0 //"

let prep = AltCoverFake.DotNet.Testing.AltCover.PrepareOptions.Primitive
             { AltCoverFake.DotNet.Testing.Primitive.PrepareOptions.Create() with
                           XmlReport = "./__UnitTestWithAltCover.xml"
                           OutputDirectories =
                             [| "./__UnitTestWithAltCover" |]
                           StrongNameKey = "./Build/Infrastructure.snk"
                           ReportFormat = "NCover"
                           InPlace = false
                           Save = false }
           |> AltCoverFake.DotNet.Testing.AltCoverCommand.Prepare
{ AltCoverFake.DotNet.Testing.AltCoverCommand.Options.Create prep with
              ToolPath = "altcover"
              ToolType = Fake.DotNet.ToolType.CreateFullFramework()
              WorkingDirectory = "./__UnitTestWithAltCover" }
|> AltCoverFake.DotNet.Testing.AltCoverCommand.run
```

### Example
(based on [the AltCover build script here](https://github.com/SteveGilham/altcover/blob/master/Build/targets.fsx#L3578-L3591))

```
#r "paket:
nuget Fake.DotNet.Cli >= 5.20.3
nuget AltCover.Fake >= 7.0 //"

let ForceTrue = AltCoverFake.DotNet.Testing.DotNet.CLIOptions.Force true 

let prep = AltCoverFake.DotNet.Testing.Primitive.PrepareOptions.Create()
let coll = AltCoverFake.DotNet.Testing.Primitive.CollectOptions.Create()

let prep1 = { prep with CallContext = [ "[Fact]"; "0" ]
                        AssemblyFilter = [| "xunit" |] }

let prepare = AltCoverFake.DotNet.Testing.AltCover.PrepareOptions.Primitive prep1
let collect = AltCoverFake.DotNet.Testing.AltCover.CollectOptions.Primitive { coll with SummaryFormat = "+B" }

open AltCoverFake.DotNet.DotNet // extension method WithAltCoverOptions
Fake.DotNet.DotNet.test (fun to' -> to'.WithAltCoverOptions prepare collect ForceTrue) "dotnettest.fsproj"

```
