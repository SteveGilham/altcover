namespace ApiUse

module DriveApi =

  open System
  open Fake.IO
  open Fake.Core
  open Fake.DotNet
  open Fake.IO.FileSystemOperators
  open Swensen.Unquote.Assertions

  open AltCoverFake.DotNet.DotNet // extension methods
  open AltCoverFake.DotNet.Testing

  let _Target s f =
    Target.description s
    Target.create s f

  let dotnetVersion = DotNet.getVersion id

  let withTestEnvironment l (o: DotNet.TestOptions) =
    let before = o.Environment |> Map.toList

    let after =
      [ l; before ] |> List.concat |> Map.ofList

    o.WithEnvironment after

  // use this to keep optesting the 7.0.100 workround
  let withAltCoverOptions prepare collect force (o: DotNet.TestOptions) =
    withTestEnvironment
      (AltCoverFake.DotNet.Testing.DotNet.ToTestPropertiesList prepare collect force)
      o

  let DoIt =
    (fun _ ->
      let expected = "{0}"
      let acv = AltCover.Command.Version()
      printfn "AltCover.Command.Version - Returned %A expected %A" acv expected
      test <@ acv.ToString() = expected @>

      let acfv =
        AltCover.Command.FormattedVersion()

      test <@ acfv = (sprintf "AltCover version %s" expected) @>

      printfn
        "AltCover.Command.FormattedVersion - Returned '%s' expected %A"
        acfv
        expected

      let afcv =
        AltCover.Fake.Command.Version().ToString()

      afcv |> Trace.trace
      printfn "expected %A" expected

      test <@ afcv.ToString() = expected @>

      let collect =
        AltCover.CollectOptions.Primitive
          { Primitive.CollectOptions.Create() with
              LcovReport = "x" }

      let prepare =
        AltCover.PrepareOptions.Primitive
          { Primitive.PrepareOptions.Create() with
              All = true
              TypeFilter = [| "a"; "b" |] }

      // let t = prepare.GetType()
      // printfn "prepare type is %A" t.FullName

      // t.GetInterfaces()
      // |> Seq.iter (fun i -> printfn "\timplements %A" i.FullName)

      let forceTrue = DotNet.CLIOptions.Force true

      //      printfn
      //        "Test arguments : '%s'"
      //        (AltCover.DotNet.ToTestArgumentList prepare collect forceTrue)
      printfn
        "Test arguments : '%A'"
        (AltCoverFake.DotNet.Testing.DotNet.ToTestPropertiesList
          prepare
          collect
          forceTrue)

      let t =
        DotNet.TestOptions.Create()
        |> (withAltCoverOptions prepare collect forceTrue)

      printfn "WithAltCoverOptions returned '%A'" t.Common.Environment

      let p2 =
        { Primitive.PrepareOptions.Create() with
            LocalSource = true
            All = true
            CallContext = [| "[Fact]"; "0" |]
            AssemblyFilter = [| "xunit" |] }

      let pp2 =
        AltCover.PrepareOptions.Primitive p2

      let c2 = Primitive.CollectOptions.Create()

      let cc2 =
        AltCover.CollectOptions.Primitive c2

      let setBaseOptions (o: DotNet.Options) =
        { o with
            WorkingDirectory = Path.getFullName "./_DotnetTest"
            Verbosity = Some DotNet.Verbosity.Minimal }

      let testWithCLIArguments (o: Fake.DotNet.DotNet.TestOptions) =
        let msb =
          { o.MSBuildParams with
              ConsoleLogParameters = []
              DistributedLoggers = None
              DisableInternalBinLog = true }

        { o with MSBuildParams = msb }

      DotNet.test
        ((fun to' ->
          to'.WithCommon(setBaseOptions)
          |> (withAltCoverOptions pp2 cc2 forceTrue))
         >> testWithCLIArguments)
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