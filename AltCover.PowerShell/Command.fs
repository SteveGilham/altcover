namespace AltCover.Commands

open System
open System.IO
open System.Management.Automation
open AltCover

type Summary =
  | Default = 0
  | R = 1
  | B = 2
  | RPlus = 3
  | BPlus = 4

type ShowHidden =
  | KeepHidden = 0
  | Mark = 1
  | Reveal = 2

[<Cmdlet(VerbsLifecycle.Invoke, "AltCover", SupportsShouldProcess = true,
         ConfirmImpact = ConfirmImpact.Medium)>]
[<OutputType([| "System.Void"; "System.String" |]); AutoSerializable(false)>]
[<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Smells",
    "AvoidLargeClassesRule",
    Justification="Has lots of parameters to pass")>]
[<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.PowerShell",
                                                  "PS1101:AllCmdletsShouldAcceptPipelineInput",
                                                  Justification = "No valid input")>]
type InvokeAltCoverCommand(runner : bool) =
  inherit PSCmdlet()
  new() = InvokeAltCoverCommand(false)

  [<Parameter(ParameterSetName = "Runner", Mandatory = true, Position = 1,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val Runner : SwitchParameter = SwitchParameter(runner) with get, set

  [<Parameter(ParameterSetName = "Runner", Mandatory = true, ValueFromPipeline = false,
              ValueFromPipelineByPropertyName = false)>]
  member val RecorderDirectory = String.Empty with get, set

  [<Parameter(ParameterSetName = "Runner", Mandatory = false, ValueFromPipeline = false,
              ValueFromPipelineByPropertyName = false)>]
  member val WorkingDirectory = String.Empty with get, set

  [<Parameter(ParameterSetName = "Runner", Mandatory = false, ValueFromPipeline = false,
              ValueFromPipelineByPropertyName = false)>]
  member val Executable = String.Empty with get, set

  [<Parameter(ParameterSetName = "Runner", Mandatory = false, ValueFromPipeline = false,
              ValueFromPipelineByPropertyName = false)>]
  member val LcovReport = String.Empty with get, set

  [<Parameter(ParameterSetName = "Runner", Mandatory = false, ValueFromPipeline = false,
              ValueFromPipelineByPropertyName = false)>]
  member val Threshold = String.Empty with get, set

  [<Parameter(ParameterSetName = "Runner", Mandatory = false, ValueFromPipeline = false,
              ValueFromPipelineByPropertyName = false)>]
  member val Cobertura = String.Empty with get, set

  [<Parameter(ParameterSetName = "Runner", Mandatory = false, ValueFromPipeline = false,
              ValueFromPipelineByPropertyName = false)>]
  member val OutputFile = String.Empty with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "Runner", Mandatory = false, ValueFromPipeline = false,
              ValueFromPipelineByPropertyName = false)>]
  member val CommandLine = Seq.empty<string> with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val InputDirectory = Seq.empty<string> with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val OutputDirectory = Seq.empty<string> with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val SymbolDirectory = Seq.empty<string> with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val Dependency = Seq.empty<string> with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val Key = Seq.empty<string> with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val StrongNameKey = String.Empty with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val XmlReport = String.Empty with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val FileFilter = Seq.empty<string> with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val PathFilter = Seq.empty<string> with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val AssemblyFilter = Seq.empty<string> with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val AssemblyExcludeFilter = Seq.empty<string> with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val TypeFilter = Seq.empty<string> with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val MethodFilter = Seq.empty<string> with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val AttributeFilter = Seq.empty<string> with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val CallContext = Seq.empty<string> with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val OpenCover : SwitchParameter = SwitchParameter(false) with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val InPlace : SwitchParameter = SwitchParameter(false) with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val Save : SwitchParameter = SwitchParameter(false) with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val Single : SwitchParameter = SwitchParameter(false) with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val LineCover : SwitchParameter = SwitchParameter(false) with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val BranchCover : SwitchParameter = SwitchParameter(false) with get, set

  [<Parameter(ParameterSetName = "Version", Mandatory = true, Position = 1,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val Version : SwitchParameter = SwitchParameter(false) with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "Runner", Mandatory = false, ValueFromPipeline = false,
              ValueFromPipelineByPropertyName = false)>]
  member val DropReturnCode : SwitchParameter = SwitchParameter(false) with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val SourceLink : SwitchParameter = SwitchParameter(false) with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val Defer : SwitchParameter = SwitchParameter(false) with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val LocalSource : SwitchParameter = SwitchParameter(false) with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val VisibleBranches : SwitchParameter = SwitchParameter(false) with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val ShowGenerated : SwitchParameter = SwitchParameter(false) with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val ShowStatic = ShowHidden.KeepHidden with get, set

  [<Parameter(ParameterSetName = "Runner", Mandatory = false, ValueFromPipeline = false,
              ValueFromPipelineByPropertyName = false)>]
  member val SummaryFormat : Summary = Summary.Default with get, set

  member val private Fail : String list = [] with get, set

  member private self.Collect() =
    let formats = [| String.Empty; "R"; "B"; "+R"; "+B" |]
    FSApi.CollectParams.Primitive
      { RecorderDirectory = self.RecorderDirectory
        WorkingDirectory = self.WorkingDirectory
        Executable = self.Executable
        LcovReport = self.LcovReport
        Threshold = self.Threshold
        Cobertura = self.Cobertura
        OutputFile = self.OutputFile
        CommandLine = self.CommandLine
        ExposeReturnCode = not self.DropReturnCode.IsPresent
        SummaryFormat = formats.[self.SummaryFormat |> int] }

  member private self.Prepare() =
    let showStatic = [| "-"; "+"; "++ " |]
    FSApi.PrepareParams.Primitive
      { InputDirectories = self.InputDirectory
        OutputDirectories = self.OutputDirectory
        SymbolDirectories = self.SymbolDirectory
        Dependencies = self.Dependency
        Keys = self.Key
        StrongNameKey = self.StrongNameKey
        XmlReport = self.XmlReport
        FileFilter = self.FileFilter
        AssemblyFilter = self.AssemblyFilter
        AssemblyExcludeFilter = self.AssemblyExcludeFilter
        TypeFilter = self.TypeFilter
        MethodFilter = self.MethodFilter
        AttributeFilter = self.AttributeFilter
        PathFilter = self.PathFilter
        CallContext = self.CallContext
        OpenCover = self.OpenCover.IsPresent
        InPlace = self.InPlace.IsPresent
        Save = self.Save.IsPresent
        Single = self.Single.IsPresent
        LineCover = self.LineCover.IsPresent
        BranchCover = self.BranchCover.IsPresent
        CommandLine = self.CommandLine
        ExposeReturnCode = not self.DropReturnCode.IsPresent
        SourceLink = self.SourceLink.IsPresent
        Defer = self.Defer.IsPresent
        LocalSource = self.LocalSource.IsPresent
        VisibleBranches = self.VisibleBranches.IsPresent
        ShowStatic = showStatic.[self.ShowStatic |> int]
        ShowGenerated = self.ShowGenerated.IsPresent }

  member private self.Log() =
    FSApi.Logging.Primitive
      { Primitive.Logging.Create() with
          Error = (fun s -> self.Fail <- s :: self.Fail)
          Info = (fun s -> self.WriteInformation(s, [||]))
          Warn = (fun s -> self.WriteWarning s) }

  member private self.Dispatch() =
    let log = self.Log()
    let zero _ = 0

    (match (self.Version.IsPresent, self.Runner.IsPresent) with
     | (true, _) ->
         (fun _ ->
           Api.Version() |> log.Info
           0)
     | (_, true) ->
         let task = self.Collect()
         // unset is error, but if set the recorder may not exist yet
         let recording =
           self.RecorderDirectory
           |> String.IsNullOrWhiteSpace
           || Path.Combine(self.RecorderDirectory, "AltCover.Recorder.g.dll")
              |> File.Exists
         if (self.ShouldProcess("Command Line : " + task.WhatIf(recording).ToString()))
         then Api.Collect task
         else zero
     | _ ->
         let task = self.Prepare()
         if (self.ShouldProcess("Command Line : " + task.WhatIf().ToString()))
         then Api.Prepare task
         else zero) log

  override self.ProcessRecord() =
    let here = Directory.GetCurrentDirectory()
    try
      let where = self.SessionState.Path.CurrentLocation.Path
      Directory.SetCurrentDirectory where
      let makeError s =
        ErrorRecord(InvalidOperationException(s), s, ErrorCategory.InvalidOperation, self)
        |> self.WriteError

      let status = self.Dispatch()
      if status <> 0 then status.ToString() |> self.Log().Error
      else if self.Runner.IsPresent then Api.Summary() |> self.WriteObject

      match self.Fail with
      | [] -> ()
      | things -> String.Join(Environment.NewLine, things |> List.rev) |> makeError
    finally
      Directory.SetCurrentDirectory here