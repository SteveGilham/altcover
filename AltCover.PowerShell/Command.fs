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

[<Cmdlet(VerbsLifecycle.Invoke, "AltCover")>]
[<OutputType([|"System.Void";"System.String"|])>]
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
  member val CommandLine : string array = [||] with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Alias("InputDirectories")>]
  member val InputDirectory : string array = [||] with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Alias("OutputDirectories")>]
  member val OutputDirectory : string array = [||] with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Alias("SymbolDirectories")>]
  member val SymbolDirectory : string array = [||] with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Alias("Dependencies")>]
  member val Dependency : string array = [||] with get, set
#if NETCOREAPP2_0
#else
  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Alias("Keys")>]
  member val Key  : string array = [| |] with get, set
  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val StrongNameKey = String.Empty with get, set
#endif

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val XmlReport = String.Empty with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val FileFilter : string array = [||] with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val PathFilter : string array = [||] with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val AssemblyFilter : string array = [||] with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val AssemblyExcludeFilter : string array = [||] with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val TypeFilter : string array = [||] with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val MethodFilter : string array = [||] with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val AttributeFilter : string array = [||] with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val CallContext : string array = [||] with get, set

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

  [<Parameter(ParameterSetName = "Runner", Mandatory = false, ValueFromPipeline = false,
              ValueFromPipelineByPropertyName = false)>]
  member val SummaryFormat : Summary = Summary.Default with get, set

  member val private Fail : String list = [] with get, set

  member private self.Collect() =
    let formats = [| String.Empty; "R"; "B"; "+R"; "+B"|]
    FSApi.CollectParams.Primitive { RecorderDirectory = self.RecorderDirectory
                                    WorkingDirectory = self.WorkingDirectory
                                    Executable = self.Executable
                                    LcovReport = self.LcovReport
                                    Threshold = self.Threshold
                                    Cobertura = self.Cobertura
                                    OutputFile = self.OutputFile
                                    CommandLine = self.CommandLine
                                    ExposeReturnCode = not self.DropReturnCode.IsPresent
                                    SummaryFormat = formats.[self.SummaryFormat |> int]}

  member private self.Prepare() =
    FSApi.PrepareParams.Primitive { InputDirectories = self.InputDirectory
                                    OutputDirectories = self.OutputDirectory
                                    SymbolDirectories = self.SymbolDirectory
                                    Dependencies = self.Dependency
#if NETCOREAPP2_0
                                    Keys = []
                                    StrongNameKey = String.Empty
#else
                                    Keys = self.Key;
                                    StrongNameKey = self.StrongNameKey;
#endif
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
                                    Defer = self.Defer.IsPresent }

  member private self.Log() =
    FSApi.Logging.Primitive { Primitive.Logging.Create() with Error = (fun s -> self.Fail <- s :: self.Fail)
                                                              Info = (fun s -> self.WriteInformation(s, [||]))
                                                              Warn = (fun s -> self.WriteWarning s) }

  override self.ProcessRecord() =
    let here = Directory.GetCurrentDirectory()
    let log = self.Log()
    try
      let where = self.SessionState.Path.CurrentLocation.Path
      Directory.SetCurrentDirectory where
      let makeError s =
        ErrorRecord(InvalidOperationException(), s, ErrorCategory.InvalidOperation, self)
        |> self.WriteError

      let status =
        (match (self.Version.IsPresent, self.Runner.IsPresent) with
         | (true, _) ->
           (fun _ ->
           Api.Version() |> log.Info
           0)
         | (_, true) ->
           let task = self.Collect()
           Api.Collect task
         | _ ->
           let task = self.Prepare()
           Api.Prepare task) log
      if status <> 0 then status.ToString() |> self.Log().Error
      else if self.Runner.IsPresent then Api.Summary () |> self.WriteObject
      match self.Fail with
      | [] -> ()
      | things -> String.Join(Environment.NewLine, things |> List.rev) |> makeError
    finally
      Directory.SetCurrentDirectory here