#if RUNNER
[<RequireQualifiedAccess>]
module AltCover.FSApi
#else
[<RequireQualifiedAccess>]
module AltCover_Fake.DotNet.Testing.AltCover
#endif

open System
open System.Diagnostics.CodeAnalysis
open System.Linq
#if RUNNER
open AltCover
open AltCover.Augment
#else
open System.Reflection
open AltCover_Fake.DotNet.Testing
open Fake.Core
open Fake.DotNet
#endif

[<ExcludeFromCodeCoverage; NoComparison;
                  SuppressMessage("Gendarme.Rules.Smells",
                                  "RelaxedAvoidCodeDuplicatedInSameClassRule",
                                  Justification = "Idiomatic F#")>]
type CollectParams =
  | Primitive of Primitive.CollectParams
  | TypeSafe of TypeSafe.CollectParams

  static member private ToSeq(s : String seq) =
    match s with
    | null -> Seq.empty<string>
    | _ -> s

  member self.RecorderDirectory =
    match self with
    | Primitive p -> p.RecorderDirectory
    | TypeSafe t -> t.RecorderDirectory.AsString()

  member self.WorkingDirectory =
    match self with
    | Primitive p -> p.WorkingDirectory
    | TypeSafe t -> t.WorkingDirectory.AsString()

  member self.Executable =
    match self with
    | Primitive p -> p.Executable
    | TypeSafe t -> t.Executable.AsString()

#if RUNNER
#else
  member internal self.SetExecutable tool =
    match self with
    | Primitive p -> Primitive { p with Executable = tool }
    | TypeSafe t -> TypeSafe { t with Executable = TypeSafe.Tool tool }
#endif

  member self.LcovReport =
    match self with
    | Primitive p -> p.LcovReport
    | TypeSafe t -> t.LcovReport.AsString()

  member self.Threshold =
    match self with
    | Primitive p -> p.Threshold
    | TypeSafe t -> t.Threshold.AsString()

  member self.Cobertura =
    match self with
    | Primitive p -> p.Cobertura
    | TypeSafe t -> t.Cobertura.AsString()

  member self.OutputFile =
    match self with
    | Primitive p -> p.OutputFile
    | TypeSafe t -> t.OutputFile.AsString()

  member self.CommandLine =
    match self with
    | Primitive p -> p.CommandLine |> CollectParams.ToSeq
    | TypeSafe t -> t.CommandLine.AsStrings()

#if RUNNER
#else
  member internal self.SetCommandLine(args : string seq) =
    match self with
    | Primitive p -> Primitive { p with CommandLine = args }
    | TypeSafe t ->
        TypeSafe
          { t with
              CommandLine =
                let newargs =
                  args
                  |> (Seq.map TypeSafe.CommandArgument)
                  |> Seq.toList
                match newargs with
                | [] -> TypeSafe.NoCommand
                | _ -> TypeSafe.Command newargs }
#endif

  member self.ExposeReturnCode =
    match self with
    | Primitive p -> p.ExposeReturnCode
    | TypeSafe t -> t.ExposeReturnCode.AsBool()

  member self.SummaryFormat =
    match self with
    | Primitive p -> p.SummaryFormat
    | TypeSafe t -> t.SummaryFormat.AsString()

#if RUNNER
  member self.Validate afterPreparation =
    let saved = CommandLine.error

    let validate f x =
      if x
         |> String.IsNullOrWhiteSpace
         |> not
      then f x |> ignore

    let validateOptional f key x = validate (f key) x

    let toOption s =
      if s |> String.IsNullOrWhiteSpace then None else Some s
    try
      let recorder = self.RecorderDirectory
      [ ("--recorderDirectory", recorder)
        ("--workingDirectory", self.WorkingDirectory) ]
      |> List.iter (fun (n, x) -> validateOptional CommandLine.ValidateDirectory n x)
      [ ("--executable", self.Executable)
        ("--lcovReport", self.LcovReport)
        ("--cobertura", self.Cobertura)
        ("--outputFile", self.OutputFile) ]
      |> List.iter (fun (n, x) -> validateOptional CommandLine.ValidatePath n x)
      validate Runner.ValidateThreshold self.Threshold
      if afterPreparation then Runner.RequireRecorderTest (recorder |> toOption) () ()
      CommandLine.error |> List.toArray
    finally
      CommandLine.error <- saved
#else
#endif

[<ExcludeFromCodeCoverage; NoComparison;
                  SuppressMessage("Gendarme.Rules.Smells",
                                  "RelaxedAvoidCodeDuplicatedInSameClassRule",
                                  Justification = "Idiomatic F#")>]
type PrepareParams =
  | Primitive of Primitive.PrepareParams
  | TypeSafe of TypeSafe.PrepareParams

  static member private ToSeq(s : 'a seq) =
    match s with
    | null -> Seq.empty<'a>
    | _ -> s

  static member private ToList(s : 'a seq) =
    s
    |> PrepareParams.ToSeq
    |> Seq.toList

  member self.InputDirectories =
    match self with
    | Primitive p -> p.InputDirectories |> PrepareParams.ToList
    | TypeSafe t -> t.InputDirectories.AsStrings()

  member self.OutputDirectories =
    match self with
    | Primitive p -> p.OutputDirectories |> PrepareParams.ToList
    | TypeSafe t -> t.OutputDirectories.AsStrings()

  member self.SymbolDirectories =
    match self with
    | Primitive p -> p.SymbolDirectories |> PrepareParams.ToList
    | TypeSafe t -> t.SymbolDirectories.AsStrings()

  member self.Dependencies =
    match self with
    | Primitive p -> p.Dependencies |> PrepareParams.ToList
    | TypeSafe t -> t.Dependencies.AsStrings()

  member self.Keys =
    match self with
    | Primitive p -> p.Keys |> PrepareParams.ToList
    | TypeSafe t -> t.Keys.AsStrings()

  member self.StrongNameKey =
    match self with
    | Primitive p -> p.StrongNameKey
    | TypeSafe t -> t.StrongNameKey.AsString()

  member self.XmlReport =
    match self with
    | Primitive p -> p.XmlReport
    | TypeSafe t -> t.XmlReport.AsString()

  member self.FileFilter =
    match self with
    | Primitive p -> p.FileFilter |> PrepareParams.ToList
    | TypeSafe t -> t.FileFilter.AsStrings()

  member self.AssemblyFilter =
    match self with
    | Primitive p -> p.AssemblyFilter |> PrepareParams.ToList
    | TypeSafe t -> t.AssemblyFilter.AsStrings()

  member self.AssemblyExcludeFilter =
    match self with
    | Primitive p -> p.AssemblyExcludeFilter |> PrepareParams.ToList
    | TypeSafe t -> t.AssemblyExcludeFilter.AsStrings()

  member self.TypeFilter =
    match self with
    | Primitive p -> p.TypeFilter |> PrepareParams.ToList
    | TypeSafe t -> t.TypeFilter.AsStrings()

  member self.MethodFilter =
    match self with
    | Primitive p -> p.MethodFilter |> PrepareParams.ToList
    | TypeSafe t -> t.MethodFilter.AsStrings()

  member self.AttributeFilter =
    match self with
    | Primitive p -> p.AttributeFilter |> PrepareParams.ToList
    | TypeSafe t -> t.AttributeFilter.AsStrings()

  member self.PathFilter =
    match self with
    | Primitive p -> p.PathFilter |> PrepareParams.ToList
    | TypeSafe t -> t.PathFilter.AsStrings()

  member self.CallContext =
    match self with
    | Primitive p -> p.CallContext |> PrepareParams.ToList
    | TypeSafe t -> t.CallContext.AsStrings()

  member self.OpenCover =
    match self with
    | Primitive p -> p.OpenCover
    | TypeSafe t -> t.OpenCover.AsBool()

  member self.InPlace =
    match self with
    | Primitive p -> p.InPlace
    | TypeSafe t -> t.InPlace.AsBool()

  member self.Save =
    match self with
    | Primitive p -> p.Save
    | TypeSafe t -> t.Save.AsBool()

  member self.Single =
    match self with
    | Primitive p -> p.Single
    | TypeSafe t -> t.Single.AsBool()

  member self.LineCover =
    match self with
    | Primitive p -> p.LineCover
    | TypeSafe t -> t.LineCover.AsBool()

  member self.BranchCover =
    match self with
    | Primitive p -> p.BranchCover
    | TypeSafe t -> t.BranchCover.AsBool()

  member self.CommandLine =
    match self with
    | Primitive p -> p.CommandLine |> PrepareParams.ToSeq
    | TypeSafe t -> t.CommandLine.AsStrings()

#if RUNNER
#else
  member internal self.SetCommandLine(args : string seq) =
    match self with
    | Primitive p -> Primitive { p with CommandLine = args }
    | TypeSafe t ->
        TypeSafe
          { t with
              CommandLine =
                let newargs =
                  args
                  |> (Seq.map TypeSafe.CommandArgument)
                  |> Seq.toList
                match newargs with
                | [] -> TypeSafe.NoCommand
                | _ -> TypeSafe.Command newargs }
#endif

  member self.ExposeReturnCode =
    match self with
    | Primitive p -> p.ExposeReturnCode
    | TypeSafe t -> t.ExposeReturnCode.AsBool()

  member self.SourceLink =
    match self with
    | Primitive p -> p.SourceLink
    | TypeSafe t -> t.SourceLink.AsBool()

  member self.Defer =
    match self with
    | Primitive p -> p.Defer
    | TypeSafe t -> t.Defer.AsBool()

  member self.LocalSource =
    match self with
    | Primitive p -> p.LocalSource
    | TypeSafe t -> t.LocalSource.AsBool()

  member self.VisibleBranches =
    match self with
    | Primitive p -> p.VisibleBranches
    | TypeSafe t -> t.VisibleBranches.AsBool()

  member self.ShowStatic =
    match self with
    | Primitive p -> p.ShowStatic
    | TypeSafe t -> t.ShowStatic.AsString()

  member self.ShowGenerated =
    match self with
    | Primitive p -> p.ShowGenerated
    | TypeSafe t -> t.ShowGenerated.AsBool()

#if RUNNER
  static member private validateArray a f key =
    PrepareParams.validateArraySimple a (f key)

  static member private validateArraySimple a f = a |> Seq.iter (fun s -> f s |> ignore)

  static member private validateOptional f key x =
    if x
       |> String.IsNullOrWhiteSpace
       |> not
    then f key x |> ignore

  member private self.consistent() =
    if self.Single && self.CallContext.Any() then
      CommandLine.error <-
        String.Format
          (System.Globalization.CultureInfo.CurrentCulture,
           CommandLine.resources.GetString "Incompatible", "--single", "--callContext")
        :: CommandLine.error

  member private self.consistent'() =
    if self.LineCover && self.BranchCover then
      CommandLine.error <-
        String.Format
          (System.Globalization.CultureInfo.CurrentCulture,
           CommandLine.resources.GetString "Incompatible", "--branchcover", "--linecover")
        :: CommandLine.error

  member self.Validate() =
    let saved = CommandLine.error

    let validateContext context =
      let select state x =
        let (_, n) = Main.ValidateCallContext state x
        match (state, n) with
        | (true, _)
        | (_, Left(Some _)) -> true
        | _ -> false
      context
      |> PrepareParams.ToSeq
      |> Seq.fold select false
      |> ignore

    try
      CommandLine.error <- []
      PrepareParams.validateArray self.InputDirectories CommandLine.ValidateDirectory
        "--inputDirectory"
      PrepareParams.validateArray self.OutputDirectories CommandLine.ValidatePath
        "--outputDirectory"
      PrepareParams.validateOptional CommandLine.ValidateStrongNameKey "--strongNameKey"
        self.StrongNameKey
      PrepareParams.validateOptional CommandLine.ValidatePath "--xmlReport"
        self.XmlReport
      PrepareParams.validateArray self.SymbolDirectories CommandLine.ValidateDirectory
        "--symbolDirectory"
      PrepareParams.validateArray self.Dependencies CommandLine.ValidateAssembly
        "--dependency"
      PrepareParams.validateArray self.Keys CommandLine.ValidateStrongNameKey "--key"
      [ self.FileFilter
        self.AssemblyFilter
        self.AssemblyExcludeFilter
        self.TypeFilter
        self.MethodFilter
        self.AttributeFilter
        self.PathFilter ]
      |> Seq.iter
           (fun a -> PrepareParams.validateArraySimple a CommandLine.ValidateRegexes)
      self.consistent()
      self.consistent'()
      validateContext self.CallContext
      CommandLine.error |> List.toArray
    finally
      CommandLine.error <- saved

[<ExcludeFromCodeCoverage; NoComparison; NoEquality;
                  SuppressMessage("Gendarme.Rules.Smells",
                                  "RelaxedAvoidCodeDuplicatedInSameClassRule",
                                  Justification = "Idiomatic F#")>]
type Logging =
  | Primitive of Primitive.Logging

  static member Create() = Primitive.Logging.Create() |> Primitive

  static member ActionAdapter(a : Action<String>) =
    match a with
    | null -> ignore
    | _ -> a.Invoke

  member self.Error =
    match self with
    | Primitive p -> p.Error

  member self.Warn =
    match self with
    | Primitive p -> p.Warn

  member self.Echo =
    match self with
    | Primitive p -> p.Echo

  member self.Info =
    match self with
    | Primitive p -> p.Info

  member internal self.Apply() =
    Output.Error <- self.Error
    Output.Warn <- self.Warn
    Output.Info <- self.Info
    Output.Echo <- self.Echo
#else
#endif
[<SuppressMessage("Gendarme.Rules.Smells",
                                  "RelaxedAvoidCodeDuplicatedInSameClassRule",
                                  Justification = "Not worth trying to unify these functions")>]
module private ArgsHelper =
  let Item a x =
    if x |> String.IsNullOrWhiteSpace then [] else [ a; x ]

  let OptItem a x l =
    if x
       |> String.IsNullOrWhiteSpace
       || l |> List.exists (fun i -> i = x) then
      []
    else
      [ a + ":" + x ]

module internal Args =
  let internal ItemList a x =
    if x |> isNull then
      []
    else
      x
      |> Seq.collect (fun i -> [ a; i ])
      |> Seq.toList

  let private Flag a x =
    if x then [ a ] else []

  let internal ItemLists(args : PrepareParams) =
    [ ("-i", args.InputDirectories)
      ("-o", args.OutputDirectories)
      ("-y", args.SymbolDirectories)
      ("-d", args.Dependencies)
      ("-k", args.Keys)
      ("-f", args.FileFilter)
      ("-s", args.AssemblyFilter)
      ("-e", args.AssemblyExcludeFilter)
      ("-t", args.TypeFilter)
      ("-m", args.MethodFilter)
      ("-a", args.AttributeFilter)
      ("-p", args.PathFilter)
      ("-c", args.CallContext) ]
    |> List.collect (fun (a, b) -> ItemList a b)

  let internal Items(args : PrepareParams) =
    [ ("--sn", args.StrongNameKey)
      ("-x", args.XmlReport) ]
    |> List.collect (fun (a, b) -> ArgsHelper.Item a b)

  let internal OptItems(args : PrepareParams) =
    [ ("--showstatic", args.ShowStatic, [ "-" ]) ]
    |> List.collect (fun (a, b, c) -> ArgsHelper.OptItem a b c)

  let internal Flags(args : PrepareParams) =
    [ ("--opencover", args.OpenCover)
      ("--inplace", args.InPlace)
      ("--save", args.Save)
      ("--single", args.Single)
      ("--linecover", args.LineCover)
      ("--branchcover", args.BranchCover)
      ("--dropReturnCode", (args.ExposeReturnCode |> not))
      ("--sourcelink", args.SourceLink)
      ("--defer", args.Defer)
      ("--localSource", args.LocalSource)
      ("--visibleBranches", args.VisibleBranches)
      ("--showGenerated", args.ShowGenerated) ]
    |> List.collect (fun (a, b) -> Flag a b)

  let Prepare(args : PrepareParams) =
    let argsList = args.CommandLine |> Seq.toList

    let trailing =
      if List.isEmpty argsList then [] else "--" :: argsList

    let parameters =
      [ ItemLists; Items; OptItems; Flags ] |> List.collect (fun f -> f args)

    [ parameters; trailing ] |> List.concat

  let Collect(args : CollectParams) =
    let argsList = args.CommandLine |> Seq.toList

    let trailing =
      if List.isEmpty argsList then [] else "--" :: argsList

    let exe = args.Executable

    [ [ "Runner" ]
      ArgsHelper.Item "-r" args.RecorderDirectory
      ArgsHelper.Item "-w" args.WorkingDirectory
      ArgsHelper.Item "-x" exe
      ArgsHelper.Item "-l" args.LcovReport
      ArgsHelper.Item "-t" args.Threshold
      ArgsHelper.Item "-c" args.Cobertura
      ArgsHelper.Item "-o" args.OutputFile
      Flag "--collect" (exe |> String.IsNullOrWhiteSpace)
      Flag "--dropReturnCode" (args.ExposeReturnCode |> not)
      ArgsHelper.OptItem "--teamcity" args.SummaryFormat []
      trailing ]
    |> List.concat

#if RUNNER

[<ExcludeFromCodeCoverage; NoComparison>]
type ValidatedCommandLine =
  { Command : string list
    Errors : string array }
  override self.ToString() =
    let cl =
      String.Join
        (" ",
         Seq.concat
           [ [ "altcover" ]
             self.Command ])
    String.Join
      (Environment.NewLine,
       Seq.concat
         [ [| cl |]
           self.Errors ])

type CollectParams with
  member self.WhatIf afterPreparation =
    { Command = Args.Collect self
      Errors = self.Validate afterPreparation }

type PrepareParams with
  member self.WhatIf() =
    { Command = Args.Prepare self
      Errors = self.Validate() }

#else
let splitCommandLine s =
  s
  |> if Environment.isWindows
     then BlackFox.CommandLine.MsvcrCommandLine.parse
     else BlackFox.CommandLine.MonoUnixCommandLine.parse
  |> Seq.toList

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

[<NoComparison>]
type ArgType =
  | Collect of CollectParams
  | Prepare of PrepareParams
  | ImportModule
  | GetVersion

#nowarn "44"

[<NoComparison; Obsolete("Use Fake.DotNet.ToolType instead")>]
type ToolType =
  | DotNet of string option // can't attribute this type and constructor for Gendarme
  | Mono of string option // can't attribute this type and constructor for Gendarme
  | Global
  | Framework

[<NoComparison; NoEquality>]
type Params =
  { /// Path to the Altcover executable.
    ToolPath : string
    /// Which version of the tool
    [<SuppressMessage("Gendarme.Rules.Maintainability",
      "RemoveDependenceOnObsoleteCodeRule",Justification="Goes at Genbu")>]
    ToolType : ToolType
    /// Define the tool through FAKE 5.18 ToolType -- if set, overrides
    FakeToolType : Fake.DotNet.ToolType option
    /// Working directory for relative file paths.  Default is the current working directory
    WorkingDirectory : string
    /// Command arguments
    Args : ArgType }

  [<SuppressMessage("Gendarme.Rules.Maintainability",
      "RemoveDependenceOnObsoleteCodeRule",Justification="Goes at Genbu")>]
  static member Create(a : ArgType) =
    { ToolPath = "altcover"
      ToolType = Global
      FakeToolType = None
      WorkingDirectory = String.Empty
      Args = a }

  member this.WithCreateProcess(command : CreateProcess<_>) =
    match command.Command with
    | RawCommand(tool, args) ->
        match this.Args with
        | Collect c ->
            { this with
                Args =
                  ArgType.Collect
                    ((c.SetExecutable tool).SetCommandLine(Arguments.toList args)) }
        | Prepare p ->
            { this with
                Args = ArgType.Prepare(p.SetCommandLine(tool :: (Arguments.toList args))) }
        | ImportModule -> this
        | GetVersion -> this
    | _ -> this

  member this.WithToolType(tool : Fake.DotNet.ToolType) =
    { this with FakeToolType = Some tool }

let internal createArgs parameters =
  match parameters.Args with
  | Collect c -> Args.Collect c
  | Prepare p -> Args.Prepare p
  | ImportModule -> [ "ipmo" ]
  | GetVersion -> [ "version" ]

[<SuppressMessage("Gendarme.Rules.Maintainability",
      "RemoveDependenceOnObsoleteCodeRule",Justification="Goes at Genbu")>]
let private altCoverTool parameters args =
    let baseline() = CreateProcess.fromRawCommand parameters.ToolPath args
    match parameters.ToolType with
    | Framework -> baseline() |> CreateProcess.withFramework
    | Global -> baseline()
    | DotNet dotnetPath ->
        let path =
          match dotnetPath with
          | None -> "dotnet"
          | Some p -> p
        CreateProcess.fromRawCommand path (parameters.ToolPath :: args)
    | Mono monoPath ->
        let path =
          match monoPath with
          | None -> "mono"
          | Some p -> p
        CreateProcess.fromRawCommand path ("--debug" :: parameters.ToolPath :: args)

let internal createProcess parameters args =
  let fakeTool (tool : Fake.DotNet.ToolType) =
    CreateProcess.fromCommand (RawCommand(parameters.ToolPath, args |> Arguments.OfArgs))
    |> CreateProcess.withToolType (tool.WithDefaultToolCommandName "altcover")

  let doTool() =
    match parameters.FakeToolType with
    | Some tool -> fakeTool tool
    | None -> altCoverTool parameters args

  let withWorkingDirectory c =
    c
    |> if String.IsNullOrWhiteSpace parameters.WorkingDirectory
       then id
       else CreateProcess.withWorkingDirectory parameters.WorkingDirectory

  doTool()
  |> withWorkingDirectory
  |> CreateProcess.ensureExitCode
  |> fun command ->
    Trace.trace command.CommandLine
    command

let composeCommandLine parameters =
  let args = createArgs parameters
  createProcess parameters args

[<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1715",
                                                  Justification =
                                                    "Generic types are implicit")>]

let internal runCore parameters modifyCommand =
  use __ = Trace.traceTask "AltCover" String.Empty
  let command = (composeCommandLine parameters) |> modifyCommand
  let run = command |> Proc.run
  if 0 <> run.ExitCode then failwithf "AltCover '%s' failed." command.CommandLine
  __.MarkSuccess()

let run parameters = runCore parameters id

let runWithMono monoPath parameters =
  let withMono (command : CreateProcess<_>) =
    match parameters.FakeToolType with
    | Some tool ->
        if tool.GetType().FullName = "Fake.DotNet.ToolType+FullFramework"
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
    | None -> command

  runCore parameters withMono
#endif