





































# namespace `AltCoverFake.DotNet`
```
namespace AltCoverFake.DotNet
```
## Extension methods for type `Fake.DotNet.DotNet.TestOptions` (in module `AltCoverFake.DotNet.DotNet`)

```
module DotNet =
  type Fake.DotNet.DotNet.TestOptions with
```






```
    member WithAltCoverOptions: AltCoverFake.DotNet.Testing.AltCover.PrepareOptions ->
                                   AltCoverFake.DotNet.Testing.AltCover.CollectOptions ->
                                   AltCoverFake.DotNet.Testing.DotNet.CLIOptions ->
                                   Fake.DotNet.DotNet.TestOptions
```

Adds the result of `DotNet.ToTestArguments` to the `CustomParams` member of the `Common` member
```
    member WithAltCoverImportModule: unit -> Fake.DotNet.DotNet.TestOptions
```
Adds `"/p:AltCoverImportModule=true"` to the `CustomParams` member of the `Common` member
```
    member WithAltCoverGetVersion: unit -> Fake.DotNet.DotNet.TestOptions
```
Adds `"/p:AltCoverGetVersion=true"` to the `CustomParams` member of the `Common` member
