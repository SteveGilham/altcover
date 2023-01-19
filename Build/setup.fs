namespace AltCover

module Setup =

  open System
  open System.IO
  open System.Reflection
  open System.Runtime.InteropServices
  open System.Xml.Linq

  open Fake.Core
  open Fake.Core.TargetOperators
  open Fake.DotNet
  open Fake.DotNet.NuGet.Restore
  open Fake.IO
  open Fake.IO.FileSystemOperators

  [<assembly: CLSCompliant(true)>]
  [<assembly: ComVisible(false)>]
  [<assembly: AssemblyVersionAttribute("1.0.0.0")>]
  [<assembly: AssemblyFileVersionAttribute("1.0.0.0")>]
  ()

  let consoleBefore =
    (Console.ForegroundColor, Console.BackgroundColor)

  Shell.copyFile "./AltCover.Engine/Abstract.fsi" "./AltCover.Engine/Abstract.fs"

  // Really bootstrap
  let dotnetPath =
    "dotnet" |> ProcessUtils.tryFindFileOnPath

  let dotnetOptions (o: DotNet.Options) =
    match dotnetPath with
    | Some f -> { o with DotNetCliPath = f }
    | None -> o

  DotNet.restore
    (fun o ->
      { o with
          Packages = [ "./packages" ]
          Common = dotnetOptions o.Common })
    "./Build/NuGet.csproj"

  DotNet.restore
    (fun o ->
      { o with
          Common = dotnetOptions o.Common })
    "./Build/DriveApi.fsproj"

  let toolPackages =
    let xml =
      "./Directory.Packages.props"
      |> Path.getFullName
      |> XDocument.Load

    xml.Descendants()
    |> Seq.filter (fun x -> x.Attribute(XName.Get("Include")) |> isNull |> not)
    |> Seq.map (fun x ->
      (x.Attribute(XName.Get("Include")).Value, x.Attribute(XName.Get("Version")).Value))
    |> Map.ofSeq

  let packageVersion (p: string) =
    p.ToLowerInvariant() + "/" + (toolPackages.Item p)

  let nuget =
    ("./packages/"
     + (packageVersion "NuGet.CommandLine")
     + "/tools/NuGet.exe")
    |> Path.getFullName

  let dixon =
    ("./packages/"
     + (packageVersion "AltCode.Dixon")
     + "/tools")
    |> Path.getFullName

  let fxcop =
    if Environment.isWindows then
      BlackFox.VsWhere.VsInstances.getAll ()
      |> Seq.filter (fun i -> System.Version(i.InstallationVersion).Major = 17)
      |> Seq.map (fun i ->
        i.InstallationPath
        @@ "Team Tools/Static Analysis Tools/FxCop")
      |> Seq.filter Directory.Exists
      |> Seq.tryHead
    else
      None

  let restore (o: RestorePackageParams) = { o with ToolPath = nuget }

  let _Target s f =
    Target.description s
    Target.create s f

  let resetColours _ =
    Console.ForegroundColor <- consoleBefore |> fst
    Console.BackgroundColor <- consoleBefore |> snd

  let FxCop =
    (fun _ ->
      fxcop
      |> Option.iter (fun fx ->
        Directory.ensure "./packages/fxcop/"

        let target =
          Path.getFullName "./packages/fxcop/"

        let prefix = fx.Length

        let check t pf (f: string) =
          let destination = t @@ (f.Substring pf)
          // printfn "%A" destination
          destination |> File.Exists |> not
          && destination |> Path.GetFileName
             <> "SecurityTransparencyRules.dll"

        Shell.copyDir target fx (check target prefix)

        Shell.copyDir target dixon (fun f ->
          Path.GetFileNameWithoutExtension f
          <> "AltCode.Dixon")

        let config =
          XDocument.Load "./packages/fxcop/FxCopCmd.exe.config"
        // Maybe process here...
        config.Save "./packages/fxcop/DixonCmd.exe.config"))

  let initTargets () =
    Target.description "ResetConsoleColours"
    Target.createFinal "ResetConsoleColours" resetColours
    Target.activateFinal "ResetConsoleColours"

    _Target "FxCop" FxCop

    _Target "Preparation" ignore

    "FxCop" =?> ("Preparation", Environment.isWindows)
    |> ignore

  let defaultTarget () =
    resetColours ()
    "Preparation"

  [<EntryPoint>]
  let private main argv =
    use c =
      argv
      |> Array.toList
      |> Context.FakeExecutionContext.Create false "setup.fsx"

    c
    |> Context.RuntimeContext.Fake
    |> Context.setExecutionContext

    initTargets ()
    Target.runOrDefault <| defaultTarget ()

    0 // return an integer exit code