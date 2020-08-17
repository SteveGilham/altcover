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
```
open AltCover_Fake.DotNet.Testing
...
  let prep =
    AltCover.PrepareOptions.Primitive
      { Primitive.PrepareOptions.Create() with
           XmlReport = altReport
           OutputDirectories =
             [| "./__UnitTestWithAltCover" |]
           StrongNameKey = keyfile
           ReportFormat = "NCover"
           InPlace = false
           Save = false }
    |> AltCoverCommand.Prepare
  { AltCoverCommand.Options.Create prep with
      ToolPath = altcover
      ToolType = Fake.DotNet.ToolType.CreateFullFramework()
      WorkingDirectory = testDirectory }
  |> AltCoverCommand.run

```

### Example
```
open AltCoverFake.DotNet.Testing
...
  let ForceTrue = AltCover.DotNet.CLIOptions.Force true 

  let p =
    { AltCover.Primitive.PrepareOptions.Create() with
        CallContext = [| "[Fact]"; "0" |]
        AssemblyFilter = [| "xunit" |] }

  let prep = AltCover.AltCover.PrepareOptions.Primitive p
  let c = AltCover.Primitive.CollectOptions.Create()
  let collect = AltCover.AltCover.CollectOptions.Primitive c

  let setBaseOptions (o: DotNet.Options) =
    { o with
        WorkingDirectory = Path.getFullName "./_DotnetTest"
        Verbosity = Some DotNet.Verbosity.Minimal }
```
