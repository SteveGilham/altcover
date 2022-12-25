#i "nuget: https://api.nuget.org/v3/index.json"

#r "nuget: AltCover.Fake"
#r "nuget: Fake.DotNet.Cli"

let ForceTrue = AltCoverFake.DotNet.Testing.DotNet.CLIOptions.Force true

let prep = AltCoverFake.DotNet.Testing.Primitive.PrepareOptions.Create()

let coll = AltCoverFake.DotNet.Testing.Primitive.CollectOptions.Create()

let prep1 =
    { prep with
        CallContext = [ "[Fact]"; "0" ]
        AssemblyFilter = [| "xunit" |] }

let prepare = AltCoverFake.DotNet.Testing.AltCover.PrepareOptions.Primitive prep1

let collect =
    AltCoverFake.DotNet.Testing.AltCover.CollectOptions.Primitive { coll with SummaryFormat = "+B" }

open AltCoverFake.DotNet.DotNet // extension method WithAltCoverOptions
Fake.DotNet.DotNet.test (fun to' -> to'.WithAltCoverOptions prepare collect ForceTrue) "dotnettest.fsproj"
