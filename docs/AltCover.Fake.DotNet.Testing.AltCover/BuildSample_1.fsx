#i "nuget: https://api.nuget.org/v3/index.json"

#r "nuget: AltCover.Fake"

let prep =
  AltCoverFake.DotNet.Testing.AltCover.PrepareOptions.Primitive
    { AltCoverFake.DotNet.Testing.Primitive.PrepareOptions.Create() with
        XmlReport = "./__UnitTestWithAltCover.xml"
        OutputDirectories = [| "./__UnitTestWithAltCover" |]
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