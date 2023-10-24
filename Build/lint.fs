module FSharpLint.Console.Program

open Argu
open System
open FSharpLint.Framework
open FSharpLint.Application

/// Output format the linter will use.
type private OutputFormat =
  | Standard = 1
  | MSBuild = 2

/// File type the linter is running against.
type private FileType =
  | Project = 1
  | Solution = 2
  | File = 3
  | Source = 4

// Allowing underscores in union case names for proper Argu command line option formatting.
// fsharplint:disable UnionCasesNames
type private ToolArgs =
  | [<AltCommandLine("-f")>] Format of OutputFormat
  | [<CliPrefix(CliPrefix.None)>] Lint of ParseResults<LintArgs>
  interface IArgParserTemplate with
    member this.Usage =
      match this with
      | Format _ -> "Output format of the linter."
      | Lint _ -> "Runs FSharpLint against a file or a collection of files."

// TODO: investigate erroneous warning on this type definition
// fsharplint:disable UnionDefinitionIndentation
and private LintArgs =
  | [<MainCommand; Mandatory>] Target of target: string
  | [<AltCommandLine("-l")>] Lint_Config of lintConfig: string
  | File_Type of FileType
  // fsharplint:enable UnionDefinitionIndentation
  interface IArgParserTemplate with
    member this.Usage =
      match this with
      | Target _ -> "Input to lint."
      | File_Type _ ->
        "Input type the linter will run against. If this is not set, the file type will be inferred from the file extension."
      | Lint_Config _ -> "Path to the config for the lint."
// fsharplint:enable UnionCasesNames

let private parserProgress (output: Output.IOutput) =
  function
  | Starting file ->
    String.Format(Resources.GetString("ConsoleStartingFile"), file)
    |> output.WriteInfo
  | ReachedEnd(_, warnings) ->
    String.Format(Resources.GetString("ConsoleFinishedFile"), List.length warnings)
    |> output.WriteInfo
  | Failed(file, parseException) ->
    String.Format(Resources.GetString("ConsoleFailedToParseFile"), file)
    |> output.WriteError

    "Exception Message:"
    + Environment.NewLine
    + parseException.Message
    + Environment.NewLine
    + "Exception Stack Trace:"
    + Environment.NewLine
    + parseException.StackTrace
    + Environment.NewLine
    |> output.WriteError

/// Infers the file type of the target based on its file extension.
let private inferFileType (target: string) =
  if target.EndsWith ".fs" || target.EndsWith ".fsx" then
    FileType.File
  else if target.EndsWith ".fsproj" then
    FileType.Project
  else if target.EndsWith ".sln" then
    FileType.Solution
  else
    FileType.Source

let private start
  (arguments: ParseResults<ToolArgs>)
  (toolsPath: Ionide.ProjInfo.Types.ToolsPath)
  =
  let mutable exitCode = 0

  let output =
    match arguments.TryGetResult Format with
    | Some OutputFormat.MSBuild -> Output.MSBuildOutput() :> Output.IOutput
    | Some OutputFormat.Standard
    | Some _
    | None -> Output.StandardOutput() :> Output.IOutput

  let handleError (str: string) =
    output.WriteError str
    exitCode <- -1

  match arguments.GetSubCommand() with
  | Lint lintArgs ->

    let handleLintResult =
      function
      | LintResult.Success(warnings) ->
        String.Format(Resources.GetString("ConsoleFinished"), List.length warnings)
        |> output.WriteInfo

        if not (List.isEmpty warnings) then
          exitCode <- -1
      | LintResult.Failure(failure) -> handleError failure.Description

    let lintConfig =
      lintArgs.TryGetResult Lint_Config

    let configParam =
      match lintConfig with
      | Some configPath -> FromFile configPath
      | None -> Default


    let lintParams =
      { CancellationToken = None
        ReceivedWarning = Some output.WriteWarning
        Configuration = configParam
        ReportLinterProgress = Some(parserProgress output) }

    let target = lintArgs.GetResult Target

    let fileType =
      lintArgs.TryGetResult File_Type
      |> Option.defaultValue (inferFileType target)

    try
      let lintResult =
        match fileType with
        | FileType.File -> Lint.lintFile lintParams target
        | FileType.Source -> Lint.lintSource lintParams target
        | FileType.Solution -> Lint.lintSolution lintParams target toolsPath
        | FileType.Project
        | _ -> Lint.lintProject lintParams target toolsPath

      handleLintResult lintResult
    with e ->
      let target =
        if fileType = FileType.Source then
          "source"
        else
          target

      sprintf
        "Lint failed while analysing %s.\nFailed with: %s\nStack trace: %s"
        target
        e.Message
        e.StackTrace
      |> handleError
  | _ -> ()

  exitCode

/// Must be called only once per process.
/// We're calling it globally so we can call main multiple times from our tests.
let toolsPath = Ionide.ProjInfo.Init.init ()

[<EntryPoint>]
let main argv =
  let errorHandler =
    ProcessExiter(
      colorizer =
        function
        | ErrorCode.HelpText -> None
        | _ -> Some ConsoleColor.Red
    )

  let parser =
    ArgumentParser.Create<ToolArgs>(
      programName = "fsharplint",
      errorHandler = errorHandler
    )

  let parseResults =
    parser.ParseCommandLine argv

  start parseResults toolsPath