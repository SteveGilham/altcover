#r "paket:
nuget Fake.DotNet.Cli >= 5.20.3
nuget AltCover.Api >= 7.0 //"

let ForceTrue = AltCover.DotNet.CLIOptions.Force true 

let p =
  { AltCover.Primitive.PrepareOptions.Create() with
      CallContext = [| "[Fact]"; "0" |]
      AssemblyFilter = [| "xunit" |] }

let prepare = AltCover.AltCover.PrepareOptions.Primitive p
let c = AltCover.Primitive.CollectOptions.Create()
let collect = AltCover.AltCover.CollectOptions.Primitive c

open AltCover.Fake.DotNet // extension method WithAltCoverOptions
Fake.DotNet.DotNet.test
  (fun to' -> to'.WithAltCoverOptions prepare collect ForceTrue)
  "dotnettest.fsproj"