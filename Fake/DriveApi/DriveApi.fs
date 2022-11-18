namespace ApiUse

module DriveApi =

  open System
  open Fake.IO
  open Fake.Core
  open Fake.DotNet
  open Fake.IO.FileSystemOperators

  //open AltCover.Fake.DotNet // extension methods
  open AltCoverFake.DotNet.Testing

  let _Target s f =
    Target.description s
    Target.create s f

  let dotnetVersion = DotNet.getVersion id

  let withEnvironment l (o: DotNet.Options) =
    let before = o.Environment |> Map.toList

    let after =
      [ l; before ] |> List.concat |> Map.ofList

    { o with Environment = after }

  let withTestEnvironment l (o: DotNet.TestOptions) =
    { o with Common = withEnvironment l o.Common }

  let withAltCoverOptions
    (prepare: Abstract.IPrepareOptions)
    (collect: Abstract.ICollectOptions)
    (force: DotNet.ICLIOptions)
    =
    withTestEnvironment (
      AltCoverFake.DotNet.Testing.DotNet.toTestPropertiesList prepare collect force
    )

  let DoIt =
    (fun _ ->
      let expected = "{0}"
      let acv = AltCover.Command.Version()
      printfn "AltCover.Command.Version - Returned %A expected %A" acv expected

      if acv.ToString() <> expected then
        failwith "AltCover.Command.Version mismatch"

      let acfv =
        AltCover.Command.FormattedVersion()

      printfn
        "AltCover.Command.FormattedVersion - Returned '%s' expected %A"
        acfv
        expected

      if acfv <> (sprintf "AltCover version %s" expected) then
        failwith "AltCover.Command.FormattedVersion mismatch"

      let afcv =
        AltCover.Fake.Command.Version().ToString()

      afcv |> Trace.trace
      printfn "expected %A" expected

      if afcv.ToString() <> expected then
        failwith "AltCover.Fake.Command.Version mismatch"

      let collect =
        AltCover.AltCover.CollectOptions.Primitive
          { AltCover.Primitive.CollectOptions.Create() with LcovReport = "x" }

      let prepare =
        AltCover.AltCover.PrepareOptions.Primitive
          { AltCover.Primitive.PrepareOptions.Create() with TypeFilter = [| "a"; "b" |] }

      let forceTrue =
        AltCover.DotNet.CLIOptions.Force true

      printfn
        "Test arguments : '%s'"
        (AltCover.DotNet.ToTestArguments prepare collect forceTrue)

      let t =
        DotNet.TestOptions.Create().WithAltCoverOptions prepare collect forceTrue

      printfn "WithAltCoverOptions returned '%A'" t.Common.CustomParams

      let p2 =
        { AltCover.Primitive.PrepareOptions.Create() with
            LocalSource = true
            CallContext = [| "[Fact]"; "0" |]
            AssemblyFilter = [| "xunit" |] }

      let pp2 =
        AltCover.AltCover.PrepareOptions.Primitive p2

      let c2 =
        AltCover.Primitive.CollectOptions.Create()

      let cc2 =
        AltCover.AltCover.CollectOptions.Primitive c2

      let setBaseOptions (o: DotNet.Options) =
        { o with
            WorkingDirectory = Path.getFullName "./_DotnetTest"
            Verbosity = Some DotNet.Verbosity.Minimal }

      let cliArguments =
        { MSBuild.CliArguments.Create() with
            ConsoleLogParameters = []
            DistributedLoggers = None
            Properties = []
            DisableInternalBinLog = true }

      DotNet.test
        (fun to' ->
          { (to'.WithCommon(setBaseOptions)
             |> (AltCoverOptions pp2 cc2 forceTrue)) with MSBuildParams = cliArguments })
        "apiuse_dotnettest.fsproj"

      let im = AltCover.Command.ImportModule()
      printfn "Import module %A" im

      let importModule =
        (im.Trim().Split()
         |> Seq.take 2
         |> Seq.skip 1
         |> Seq.head)
          .Trim([| '"' |])

      let command =
        "$ImportModule = '"
        + importModule
        + "'; Import-Module $ImportModule; ConvertTo-BarChart -?"

      let corePath =
        AltCover.Fake.Command.ToolPath AltCover.Fake.Implementation.DotNetCore

      printfn "corePath = %A" corePath

      let frameworkPath =
        AltCover.Fake.Command.ToolPath AltCover.Fake.Implementation.Framework

      printfn "frameworkPath = %A" frameworkPath

      if frameworkPath |> String.IsNullOrEmpty |> not then
        let framework =
          Fake.DotNet.ToolType.CreateFullFramework()

        { AltCoverFake.DotNet.Testing.AltCoverCommand.Options.Create
            AltCoverFake.DotNet.Testing.AltCoverCommand.ArgumentType.GetVersion with
            ToolType = framework
            ToolPath = frameworkPath }
        |> AltCoverFake.DotNet.Testing.AltCoverCommand.run

      let core =
        Fake.DotNet.ToolType.CreateFrameworkDependentDeployment id

      { AltCoverFake.DotNet.Testing.AltCoverCommand.Options.Create
          AltCoverFake.DotNet.Testing.AltCoverCommand.ArgumentType.GetVersion with
          ToolType = core
          ToolPath = corePath }
      |> AltCoverFake.DotNet.Testing.AltCoverCommand.run

      let pwsh =
        if Environment.isWindows then
          Fake.Core.ProcessUtils.findLocalTool
            String.Empty
            "pwsh.exe"
            [ Environment.environVar "ProgramFiles"
              @@ "PowerShell" ]
        else
          "pwsh"

      let r =
        CreateProcess.fromRawCommand pwsh [ "-NoProfile"; "-Command"; command ]
        |> CreateProcess.withWorkingDirectory "."
        |> Proc.run

      if (r.ExitCode <> 0) then
        InvalidOperationException("Non zero return code")
        |> raise)

  let Execute argv =
    argv
    |> Array.toList
    |> Context.FakeExecutionContext.Create false "build.fsx"
    |> Context.RuntimeContext.Fake
    |> Context.setExecutionContext

    _Target "DoIt" DoIt
    Target.runOrDefault "DoIt"

#if !INTERACTIVE
  [<EntryPoint>]
  let main argv =
    Execute argv

    0 // return an integer exit code
#endif