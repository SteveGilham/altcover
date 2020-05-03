namespace AltCoverFake.DotNet.Testing

open System
open System.Diagnostics.CodeAnalysis
open System.Reflection
open Fake.Core
open Fake.DotNet

[<RequireQualifiedAccess>]
[<SuppressMessage("Gendarme.Rules.Smells", "RelaxedAvoidCodeDuplicatedInSameClassRule",
  Justification="Two match statements aren't worth bothering with")>]
module AltCoverCommand =
  [<NoComparison; AutoSerializable(false)>]
  type ArgumentType =
    | Collect of AltCover.CollectOptions
    | Prepare of AltCover.PrepareOptions
    | ImportModule
    | GetVersion

  let internal setExecutable tool collect =
      match collect with
      | AltCoverFake.DotNet.Testing.AltCover.Primitive p ->
          AltCoverFake.DotNet.Testing.AltCover.Primitive { p with Executable = tool }
      | AltCoverFake.DotNet.Testing.AltCover.TypeSafe t ->
          AltCoverFake.DotNet.Testing.AltCover.TypeSafe { t with Executable = TypeSafe.Tool tool }

  let internal setCollectCommandLine (args : string seq) collect =
      match collect with
      | AltCoverFake.DotNet.Testing.AltCover.Primitive p ->
          AltCoverFake.DotNet.Testing.AltCover.Primitive { p with CommandLine = args }
      | AltCoverFake.DotNet.Testing.AltCover.TypeSafe t ->
          AltCoverFake.DotNet.Testing.AltCover.TypeSafe
            { t with
                CommandLine =
                  let newargs =
                    args
                    |> (Seq.map TypeSafe.CommandArgument)
                    |> Seq.toList
                  match newargs with
                  | [] -> TypeSafe.NoCommand
                  | _ -> TypeSafe.Command newargs }

  let internal setPrepareCommandLine (args : string seq) (prepare:PrepareParams) =
      match prepare with
      | PrepareParams.Primitive p ->
          PrepareParams.Primitive { p with CommandLine = args }
      | PrepareParams.TypeSafe t ->
          PrepareParams.TypeSafe
            { t with
                CommandLine =
                  let newargs =
                    args
                    |> (Seq.map TypeSafe.CommandArgument)
                    |> Seq.toList
                  match newargs with
                  | [] -> TypeSafe.NoCommand
                  | _ -> TypeSafe.Command newargs }

  [<SuppressMessage("Gendarme.Rules.Naming",
                    "UseCorrectCasingRule",
                    Justification = "Fake.build style")>]
  let splitCommandLine line =
    line
    |> if Environment.isWindows
       then BlackFox.CommandLine.MsvcrCommandLine.parse
       else BlackFox.CommandLine.MonoUnixCommandLine.parse
    |> Seq.toList

  [<SuppressMessage("Gendarme.Rules.Naming",
                    "UseCorrectCasingRule",
                    Justification = "Fake.build style")>]
  let buildDotNetTestCommandLine (options : DotNet.TestOptions -> DotNet.TestOptions)
      project =
    let dotnet = typeof<Fake.DotNet.DotNet.TestOptions>.DeclaringType
    let builder =
      dotnet.GetMethod("buildTestArgs", BindingFlags.Static ||| BindingFlags.NonPublic)
    let builder2 =
      dotnet.GetMethod("buildCommand", BindingFlags.Static ||| BindingFlags.NonPublic)
    let parameters = Fake.DotNet.DotNet.TestOptions.Create() |> options
    let args = builder.Invoke(null, [| parameters |]) :?> string list

    let cmdArgs =
      builder2.Invoke
        (null,
         [| ("test"
             |> Args.fromWindowsCommandLine
             |> Seq.toList)
            project :: args
            parameters.Common |]) :?> string list
    (parameters.Common.DotNetCliPath,
     cmdArgs |> List.filter (String.IsNullOrWhiteSpace >> not))

  [<NoComparison; NoEquality; AutoSerializable(false)>]
  type Options =
    { /// Path to the Altcover executable.
      ToolPath : string
      /// Which version of the tool (FAKE 5.18 ToolType)
      ToolType : ToolType
      /// Working directory for relative file paths.  Default is the current working directory
      WorkingDirectory : string
      /// Command arguments
      Args : ArgumentType }

    static member Create(argumentType : ArgumentType) =
      { ToolPath = "altcover"
        ToolType = ToolType.CreateGlobalTool()
        WorkingDirectory = String.Empty
        Args = argumentType }

    [<SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
       Justification="Compiler generated generic name")>]
    member this.WithCreateProcess(command : CreateProcess<_>) =
      match command.Command with
      | RawCommand(tool, args) ->
          match this.Args with
          | Collect c ->
              { this with
                  Args =
                    ArgumentType.Collect
                      (c
                       |> setExecutable tool
                       |> setCollectCommandLine (Arguments.toList args)) }
          | Prepare p ->
              { this with
                  Args =ArgumentType.Prepare(p |> setPrepareCommandLine(tool :: (Arguments.toList args))) }
          | ImportModule -> this
          | GetVersion -> this
      | _ -> this

  let internal createArgs options =
    match options.Args with
    | Collect c -> Args.collect c
    | Prepare p -> Args.prepare p
    | ImportModule -> [ "ImportModule" ]
    | GetVersion -> [ "version" ]

  let internal createProcess options args =
    let doTool (tool : Fake.DotNet.ToolType) =
      CreateProcess.fromCommand (RawCommand(options.ToolPath, args |> Arguments.OfArgs))
      |> CreateProcess.withToolType (tool.WithDefaultToolCommandName "altcover")

    let withWorkingDirectory c = // withWorkingDirectory line
      c
      |> if String.IsNullOrWhiteSpace options.WorkingDirectory
         then id
         else CreateProcess.withWorkingDirectory options.WorkingDirectory

    doTool options.ToolType
    |> withWorkingDirectory
    |> CreateProcess.ensureExitCode
    |> fun command ->
      Trace.trace command.CommandLine
      command

  [<SuppressMessage("Gendarme.Rules.Naming",
                    "UseCorrectCasingRule",
                    Justification = "Fake.build style")>]
  let composeCommandLine options =
    let args = createArgs options
    createProcess options args

  [<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1715",
                                                    Justification =
                                                      "Generic types are implicit")>]

  let internal runCore options modifyCommand =
    use __ = Trace.traceTask "AltCover" String.Empty
    let command = (composeCommandLine options) |> modifyCommand
    let run = command |> Proc.run
    if 0 <> run.ExitCode then failwithf "AltCover '%s' failed." command.CommandLine
    __.MarkSuccess()

  [<SuppressMessage("Gendarme.Rules.Naming",
                    "UseCorrectCasingRule",
                    Justification = "Fake.build style")>]
  let run options = runCore options id

  [<SuppressMessage("Gendarme.Rules.Naming",
                    "UseCorrectCasingRule",
                    Justification = "Fake.build style")>]
  let runWithMono monoPath options =
    let withMono (command : CreateProcess<_>) = // withMono line
      if options.ToolType.GetType().FullName = "Fake.DotNet.ToolType+FullFramework"
             && Fake.Core.Environment.isWindows then
            match command.Command with
            | RawCommand(tool, args) ->
                let newArgs = tool :: "--debug" :: (Arguments.toList args)

                let newRaw =
                  RawCommand
                    ((match monoPath with
                      | Some x -> x
                      | _ -> "mono"), Arguments.OfArgs newArgs)
                command |> CreateProcess.withCommand newRaw

            | _ -> command
          else
            command

    runCore options withMono

[<assembly: SuppressMessage("Microsoft.Performance", "CA1823:AvoidUnusedPrivateFields", Scope="member", Target="AltCoverFake.DotNet.Testing.AltCoverCommand+withMono@182T.#monoPath", Justification="Generated code")>]
[<assembly: SuppressMessage("Microsoft.Performance", "CA1823:AvoidUnusedPrivateFields", Scope="member", Target="AltCoverFake.DotNet.Testing.AltCoverCommand+withMono@182T.#options", Justification="Generated code")>]
[<assembly: SuppressMessage("Microsoft.Performance", "CA1823:AvoidUnusedPrivateFields", Scope="member", Target="AltCoverFake.DotNet.Testing.AltCoverCommand+withWorkingDirectory@142T.#options", Justification="Generated code")>]()