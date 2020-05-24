# The AltCover.Fake package

This provides helpers to drive command-line AltCover (or `dotnet test` with AltCover) from any of the other packages.  In these scenarios, AltCover operates outside the Fake build process.
The slightly awkward `AltCoverFake` namespace was chosen to allow co-existence with the previous in-process API's `AltCover.Fake` names.

Requires Fake 5.18 or later

* module `AltCover.DotNet.Testing.Primitive` -- [This module](AltCover.Fake.DotNet.Testing.AltCover/Primitive-apidoc) contains the weakly-typed, all strings, data structures in the style of Fake, to specify the command-line arguments.
* module `AltCover.DotNet.Testing.TypeSafe`-- [This module](AltCover.Fake.DotNet.Testing.AltCover/TypeSafe-apidoc) contains the strongly-typed equivalents of the above.
* module `AltCover.DotNet.Testing.OptionApi` -- [This module](AltCover.Fake.DotNet.Testing.AltCover/OptionApi-apidoc) unifies the above types.
* module `AltCover.DotNet.Testing.DotNet` -- [This module](AltCover.Fake.DotNet.Testing.AltCover/DotNet-apidoc) provides integration with `dotnet test`
* module `AltCover.DotNet.Testing.AltCoverCommand` -- [This module](AltCover.Fake.DotNet.Testing.AltCover/AltCoverCommand-apidoc) pallows a script to drive the `AltCover` executable directly.

### Example
```
open AltCover_Fake.DotNet.Testing
...
    let prep =
      { AltCover.PrepareOptions.Create() with XmlReport = xaltReport
                                              OutputDirectory = "./__UnitTestWithAltCover"
                                              StrongNameKey = keyfile
                                              OpenCover = false
                                              InPlace = false
                                              Save = false }
      |> AltCoverCommand.Prepare
    { AltCoverCommand.Options.Create prep with ToolPath = altcover
                                               ToolType = AltCover.ToolType.Framework
                                               WorkingDirectory = xtestDirectory }
    |> AltCoverCommand.run
```

### Example
```
open AltCoverFake.DotNet.Testing
...
  let ForceTrue = DotNet.CLIOptions.Force true
  let p = Primitive.PrepareOptions.Create()
  let prep = AltCover.PrepareOptions.Primitive p
  let c = Primitive.CollectOptions.Create()
  let p1 = { p with CallContext = [ "[Fact]"; "0" ]
                      AssemblyFilter = [| "xunit" |] }
  let prep1 = AltCover.PrepareOptions.Primitive p1
  let collect = AltCover.CollectOptions.Primitive c
  DotNet.test
      (fun to' ->
      (to'.WithAltCoverOptions prep collect ForceTrue)
      "dotnettest.fsproj"

```
