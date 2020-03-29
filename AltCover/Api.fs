#if RUNNER
[<RequireQualifiedAccess>]
[<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1704",
    Justification="'Api' is OK")>]
module AltCover.FSApi
#else
[<RequireQualifiedAccess>]
module AltCoverFake.DotNet.Testing.AltCover
#endif

open System
open System.Diagnostics.CodeAnalysis
open System.Linq
#if RUNNER
open AltCover
open AltCover.Augment
#else
open System.Reflection
open AltCoverFake.DotNet.Testing
open Fake.Core
open Fake.DotNet
#endif

[<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false);
                  SuppressMessage("Gendarme.Rules.Smells",
                                  "RelaxedAvoidCodeDuplicatedInSameClassRule",
                                  Justification = "Idiomatic F#")>]
type CollectParameters =
  | Primitive of Primitive.CollectParameters
  | TypeSafe of TypeSafe.CollectParameters

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

  [<SuppressMessage("Microsoft.Naming", "CA1704",
      Justification="'Lcov' is jargon")>]
  member self.LcovReport =
    match self with
    | Primitive p -> p.LcovReport
    | TypeSafe t -> t.LcovReport.AsString()

  member self.Threshold =
    match self with
    | Primitive p -> p.Threshold
    | TypeSafe t -> t.Threshold.AsString()

  [<SuppressMessage("Microsoft.Naming", "CA1704",
      Justification="'Cobertura' is jargon")>]
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
    | Primitive p -> p.CommandLine |> CollectParameters.ToSeq
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
      |> List.iter (fun (n, x) -> validateOptional CommandLine.validateDirectory n x)
      [ ("--executable", self.Executable)
        ("--lcovReport", self.LcovReport)
        ("--cobertura", self.Cobertura)
        ("--outputFile", self.OutputFile) ]
      |> List.iter (fun (n, x) -> validateOptional CommandLine.validatePath n x)
      validate Runner.validateThreshold self.Threshold
      if afterPreparation then Runner.requireRecorderTest (recorder |> toOption) () ()
      CommandLine.error |> List.toArray
    finally
      CommandLine.error <- saved
#else
#endif

[<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false);
                  SuppressMessage("Gendarme.Rules.Smells",
                                  "RelaxedAvoidCodeDuplicatedInSameClassRule",
                                  Justification = "Idiomatic F#")>]
type PrepareParameters =
  | Primitive of Primitive.PrepareParameters
  | TypeSafe of TypeSafe.PrepareParameters

  static member private ToSeq(s : 'a seq) =
    match s with
    | null -> Seq.empty<'a>
    | _ -> s

  static member private ToList(s : 'a seq) =
    s
    |> PrepareParameters.ToSeq
    |> Seq.toList

  member self.InputDirectories =
    match self with
    | Primitive p -> p.InputDirectories |> PrepareParameters.ToList
    | TypeSafe t -> t.InputDirectories.AsStrings()

  member self.OutputDirectories =
    match self with
    | Primitive p -> p.OutputDirectories |> PrepareParameters.ToList
    | TypeSafe t -> t.OutputDirectories.AsStrings()

  member self.SymbolDirectories =
    match self with
    | Primitive p -> p.SymbolDirectories |> PrepareParameters.ToList
    | TypeSafe t -> t.SymbolDirectories.AsStrings()

  member self.Dependencies =
    match self with
    | Primitive p -> p.Dependencies |> PrepareParameters.ToList
    | TypeSafe t -> t.Dependencies.AsStrings()

  member self.Keys =
    match self with
    | Primitive p -> p.Keys |> PrepareParameters.ToList
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
    | Primitive p -> p.FileFilter |> PrepareParameters.ToList
    | TypeSafe t -> t.FileFilter.AsStrings()

  member self.AssemblyFilter =
    match self with
    | Primitive p -> p.AssemblyFilter |> PrepareParameters.ToList
    | TypeSafe t -> t.AssemblyFilter.AsStrings()

  member self.AssemblyExcludeFilter =
    match self with
    | Primitive p -> p.AssemblyExcludeFilter |> PrepareParameters.ToList
    | TypeSafe t -> t.AssemblyExcludeFilter.AsStrings()

  member self.TypeFilter =
    match self with
    | Primitive p -> p.TypeFilter |> PrepareParameters.ToList
    | TypeSafe t -> t.TypeFilter.AsStrings()

  member self.MethodFilter =
    match self with
    | Primitive p -> p.MethodFilter |> PrepareParameters.ToList
    | TypeSafe t -> t.MethodFilter.AsStrings()

  member self.AttributeFilter =
    match self with
    | Primitive p -> p.AttributeFilter |> PrepareParameters.ToList
    | TypeSafe t -> t.AttributeFilter.AsStrings()

  member self.PathFilter =
    match self with
    | Primitive p -> p.PathFilter |> PrepareParameters.ToList
    | TypeSafe t -> t.PathFilter.AsStrings()

  member self.CallContext =
    match self with
    | Primitive p -> p.CallContext |> PrepareParameters.ToList
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
    | Primitive p -> p.CommandLine |> PrepareParameters.ToSeq
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
  static member private ValidateArray a f key =
    PrepareParameters.ValidateArraySimple a (f key)

  static member private ValidateArraySimple a f = a |> Seq.iter (fun s -> f s |> ignore)

  static member private ValidateOptional f key x =
    if x
       |> String.IsNullOrWhiteSpace
       |> not
    then f key x |> ignore

  member private self.Consistent() =
    if self.Single && self.CallContext.Any() then
      CommandLine.error <-
        String.Format
          (System.Globalization.CultureInfo.CurrentCulture,
           CommandLine.resources.GetString "Incompatible", "--single", "--callContext")
        :: CommandLine.error

  member private self.Consistent'() =
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
        let (_, n) = Main.validateCallContext state x
        match (state, n) with
        | (true, _)
        | (_, Left(Some _)) -> true
        | _ -> false
      context
      |> PrepareParameters.ToSeq
      |> Seq.fold select false
      |> ignore

    try
      CommandLine.error <- []
      PrepareParameters.ValidateArray self.InputDirectories CommandLine.validateDirectory
        "--inputDirectory"
      PrepareParameters.ValidateArray self.OutputDirectories CommandLine.validatePath
        "--outputDirectory"
      PrepareParameters.ValidateOptional CommandLine.validateStrongNameKey "--strongNameKey"
        self.StrongNameKey
      PrepareParameters.ValidateOptional CommandLine.validatePath "--xmlReport"
        self.XmlReport
      PrepareParameters.ValidateArray self.SymbolDirectories CommandLine.validateDirectory
        "--symbolDirectory"
      PrepareParameters.ValidateArray self.Dependencies CommandLine.validateAssembly
        "--dependency"
      PrepareParameters.ValidateArray self.Keys CommandLine.validateStrongNameKey "--key"
      [ self.FileFilter
        self.AssemblyFilter
        self.AssemblyExcludeFilter
        self.TypeFilter
        self.MethodFilter
        self.AttributeFilter
        self.PathFilter ]
      |> Seq.iter
           (fun a -> PrepareParameters.ValidateArraySimple a CommandLine.validateRegexes)
      self.Consistent()
      self.Consistent'()
      validateContext self.CallContext
      CommandLine.error |> List.toArray
    finally
      CommandLine.error <- saved

[<ExcludeFromCodeCoverage; NoComparison; NoEquality; AutoSerializable(false);
                  SuppressMessage("Gendarme.Rules.Smells",
                                  "RelaxedAvoidCodeDuplicatedInSameClassRule",
                                  Justification = "Idiomatic F#")>]
type Logging =
  | Primitive of Primitive.Logging

  static member Create() = Primitive.Logging.Create() |> Primitive

  static member ActionAdapter(action : Action<String>) =
    match action with
    | null -> ignore
    | _ -> action.Invoke

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
    Output.error <- self.Error
    Output.warn <- self.Warn
    Output.info <- self.Info
    Output.echo <- self.Echo
#else
#endif
[<SuppressMessage("Gendarme.Rules.Smells",
                                  "RelaxedAvoidCodeDuplicatedInSameClassRule",
                                  Justification = "Not worth trying to unify these functions")>]
module private ArgsHelper =
  let item a x =
    if x |> String.IsNullOrWhiteSpace then [] else [ a; x ]

  let optionalItem a x l =
    if x
       |> String.IsNullOrWhiteSpace
       || l |> List.exists (fun i -> i = x) then
      []
    else
      [ a + ":" + x ]

module internal Args =
  let internal itemList a x =
    if x |> isNull then
      []
    else
      x
      |> Seq.collect (fun i -> [ a; i ])
      |> Seq.toList

  let private flag a x =
    if x then [ a ] else []

  let internal itemLists(args : PrepareParameters) =
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
    |> List.collect (fun (a, b) -> itemList a b)

  let internal items(args : PrepareParameters) =
    [ ("--sn", args.StrongNameKey)
      ("-x", args.XmlReport) ]
    |> List.collect (fun (a, b) -> ArgsHelper.item a b)

  let internal optItems(args : PrepareParameters) =
    [ ("--showstatic", args.ShowStatic, [ "-" ]) ]
    |> List.collect (fun (a, b, c) -> ArgsHelper.optionalItem a b c)

  let internal flags(args : PrepareParameters) =
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
    |> List.collect (fun (a, b) -> flag a b)

  let prepare(args : PrepareParameters) =
    let argsList = args.CommandLine |> Seq.toList

    let trailing =
      if List.isEmpty argsList then [] else "--" :: argsList

    let parameters =
      [ itemLists; items; optItems; flags ] |> List.collect (fun f -> f args)

    [ parameters; trailing ] |> List.concat

  let collect(args : CollectParameters) =
    let argsList = args.CommandLine |> Seq.toList

    let trailing =
      if List.isEmpty argsList then [] else "--" :: argsList

    let exe = args.Executable

    [ [ "Runner" ]
      ArgsHelper.item "-r" args.RecorderDirectory
      ArgsHelper.item "-w" args.WorkingDirectory
      ArgsHelper.item "-x" exe
      ArgsHelper.item "-l" args.LcovReport
      ArgsHelper.item "-t" args.Threshold
      ArgsHelper.item "-c" args.Cobertura
      ArgsHelper.item "-o" args.OutputFile
      flag "--collect" (exe |> String.IsNullOrWhiteSpace)
      flag "--dropReturnCode" (args.ExposeReturnCode |> not)
      ArgsHelper.optionalItem "--teamcity" args.SummaryFormat []
      trailing ]
    |> List.concat

#if RUNNER

[<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
type ValidatedCommandLine =
  { Command : string list
    Errors : string seq }
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
         [ [| cl |] |> Array.toSeq
           self.Errors ])

type CollectParameters with
  member self.WhatIf afterPreparation =
    { Command = Args.collect self
      Errors = self.Validate afterPreparation }

type PrepareParameters with
  member self.WhatIf() =
    { Command = Args.prepare self
      Errors = self.Validate() }

#else
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

[<NoComparison; AutoSerializable(false)>]
type ArgumentType =
  | Collect of CollectParameters
  | Prepare of PrepareParameters
  | ImportModule
  | GetVersion

#nowarn "44"

[<NoComparison; NoEquality; AutoSerializable(false)>]
type Parameters =
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

  member this.WithCreateProcess(command : CreateProcess<_>) =
    match command.Command with
    | RawCommand(tool, args) ->
        match this.Args with
        | Collect c ->
            { this with
                Args =
                  ArgumentType.Collect
                    ((c.SetExecutable tool).SetCommandLine(Arguments.toList args)) }
        | Prepare p ->
            { this with
                Args = ArgumentType.Prepare(p.SetCommandLine(tool :: (Arguments.toList args))) }
        | ImportModule -> this
        | GetVersion -> this
    | _ -> this

let internal createArgs parameters =
  match parameters.Args with
  | Collect c -> Args.collect c
  | Prepare p -> Args.prepare p
  | ImportModule -> [ "ImportModule" ]
  | GetVersion -> [ "version" ]

let internal createProcess parameters args =
  let doTool (tool : Fake.DotNet.ToolType) =
    CreateProcess.fromCommand (RawCommand(parameters.ToolPath, args |> Arguments.OfArgs))
    |> CreateProcess.withToolType (tool.WithDefaultToolCommandName "altcover")

  let withWorkingDirectory c =
    c
    |> if String.IsNullOrWhiteSpace parameters.WorkingDirectory
       then id
       else CreateProcess.withWorkingDirectory parameters.WorkingDirectory

  doTool parameters.ToolType
  |> withWorkingDirectory
  |> CreateProcess.ensureExitCode
  |> fun command ->
    Trace.trace command.CommandLine
    command

[<SuppressMessage("Gendarme.Rules.Naming",
                  "UseCorrectCasingRule",
                  Justification = "Fake.build style")>]
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

[<SuppressMessage("Gendarme.Rules.Naming",
                  "UseCorrectCasingRule",
                  Justification = "Fake.build style")>]
let run parameters = runCore parameters id

[<SuppressMessage("Gendarme.Rules.Naming",
                  "UseCorrectCasingRule",
                  Justification = "Fake.build style")>]
let runWithMono monoPath parameters =
  let withMono (command : CreateProcess<_>) =
    if parameters.ToolType.GetType().FullName = "Fake.DotNet.ToolType+FullFramework"
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

  runCore parameters withMono
#endif