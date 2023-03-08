#i "nuget: https://api.nuget.org/v3/index.json"

#r "nuget: AltCover.Api"
#r "nuget: Fake.DotNet.Cli"

let ForceTrue =
  AltCover.DotNet.CLIOptions.Force true

let p =
  { AltCover.Primitive.PrepareOptions.Create() with
      CallContext = [| "[Fact]"; "0" |]
      AssemblyFilter = [| "xunit" |] }

let prepare =
  AltCover.AltCover.PrepareOptions.Primitive p

let c =
  AltCover.Primitive.CollectOptions.Create()

let collect =
  AltCover.AltCover.CollectOptions.Primitive c

open AltCover.Fake.DotNet // extension method WithAltCoverOptions

Fake.DotNet.DotNet.test
  (fun to' -> to'.WithAltCoverOptions prepare collect ForceTrue)
  "dotnettest.fsproj"