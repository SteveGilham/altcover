namespace AltCover.Commands

#if MONO
module Command =
    let hello name =
        printfn "Hello %s" name
#else

open System
open System.IO
open System.Management.Automation

#if NETCOREAPP2_0
open AltCover
#else
#if DEBUG
open AltCover
#else
open AltCover
module Args =
  let Item a x =
    if x |> String.IsNullOrWhiteSpace
       then []
       else [ a; x ]
  let ItemList a x =
      x
      |> Seq.collect (fun i -> [ a; i ])
      |> Seq.toList
  let Flag a x =
    if x
       then [a]
       else []

#endif
#endif

[<Cmdlet(VerbsLifecycle.Invoke, "AltCover")>]
[<OutputType("System.Void")>]
[<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.PowerShell", "PS1101:AllCmdletsShouldAcceptPipelineInput", Justification = "No valid input")>]
type InvokeAltCoverCommand(runner:bool) =
  inherit PSCmdlet()

  new () = InvokeAltCoverCommand(false)

  [<Parameter(ParameterSetName = "Runner", Mandatory = true, Position = 1,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val Runner:SwitchParameter = SwitchParameter(runner) with get, set

  [<Parameter(ParameterSetName = "Runner", Mandatory = true,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val RecorderDirectory = String.Empty with get, set

  [<Parameter(ParameterSetName = "Runner", Mandatory = false,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val WorkingDirectory = String.Empty with get, set

  [<Parameter(ParameterSetName = "Runner", Mandatory = false,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val Executable = String.Empty with get, set

  [<Parameter(ParameterSetName = "Runner", Mandatory = false,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val LcovReport = String.Empty with get, set

  [<Parameter(ParameterSetName = "Runner", Mandatory = false,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val Threshold = String.Empty with get, set

  [<Parameter(ParameterSetName = "Runner", Mandatory = false,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val Cobertura = String.Empty with get, set

  [<Parameter(ParameterSetName = "Runner", Mandatory = false,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val OutputFile = String.Empty with get, set

  [<Parameter(Mandatory = false,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val CommandLine : string array = [| |] with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val InputDirectory = String.Empty with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val OutputDirectory = String.Empty with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Alias("SymbolDirectories")>]
  member val SymbolDirectory : string array = [| |] with get, set
#if NETCOREAPP2_0
  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Alias("Dependencies")>]
  member val Dependency : string array = [| |] with get, set
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
  member val FileFilter  : string array = [| |] with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val AssemblyFilter  : string array = [| |] with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val AssemblyExcludeFilter  : string array = [| |] with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val TypeFilter  : string array = [| |] with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val MethodFilter  : string array = [| |] with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val AttributeFilter  : string array = [| |] with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val CallContext  : string array = [| |] with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val OpenCover:SwitchParameter = SwitchParameter(false) with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val InPlace:SwitchParameter = SwitchParameter(false) with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val Save:SwitchParameter = SwitchParameter(false) with get, set

  [<Parameter(ParameterSetName = "Version", Mandatory = true, Position = 1,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val Version:SwitchParameter = SwitchParameter(false) with get, set

  override self.ProcessRecord() =
    let here = Directory.GetCurrentDirectory()
    Output.Task <- self.Version.IsPresent |> not
#if NETCOREAPP2_0
    Output.Error <- (fun s -> let fail = ErrorRecord(InvalidOperationException(), s, ErrorCategory.FromStdErr, self)
                              self.WriteError fail)
    Output.Info <- (fun s -> self.WriteInformation (s, [| |]))
    Output.Warn <- (fun s -> self.WriteWarning s)
#else
#if DEBUG
    Output.Error <- (fun s -> let fail = ErrorRecord(InvalidOperationException(), s, ErrorCategory.FromStdErr, self)
                              self.WriteError fail)
    Output.Info <- (fun s -> self.WriteInformation (s, [| |]))
    Output.Warn <- (fun s -> self.WriteWarning s)
#else
    let x = StringSink(fun s -> self.WriteInformation (s, [| |]))
    Output.SetInfo x
    let y = StringSink (fun s -> let fail = ErrorRecord(InvalidOperationException(), s, ErrorCategory.FromStdErr, self)
                                 self.WriteError fail)
    Output.SetError y
    let z = StringSink(fun s -> self.WriteWarning s)
    Output.SetWarn z
#endif
#endif
    try
      let where = self.SessionState.Path.CurrentLocation.Path
      Directory.SetCurrentDirectory where
      let status = (if self.Runner.IsPresent
                    then
                        [
                          ["Runner"];
                          Args.Item "-r" self.RecorderDirectory;
                          Args.Item "-w" self.WorkingDirectory;
                          Args.Item "-x" self.Executable;
                          Args.Item "-l" self.LcovReport;
                          Args.Item "-t" self.Threshold;
                          Args.Item "-c" self.Cobertura;
                          Args.Item "-o" self.OutputFile;

                          Args.Flag "--collect" (self.Executable |> String.IsNullOrWhiteSpace)

                          Args.Item "--" (String.Join(" ", self.CommandLine));
                        ]
                        else if self.Version.IsPresent 
                          then
                             [ ["version"] ]
                          else
                            [
                              Args.Item "-i" self.InputDirectory;
                              Args.Item "-o" self.OutputDirectory;
                              Args.ItemList "-y" self.SymbolDirectory;
    #if NETCOREAPP2_0
                              Args.ItemList "-d" self.Dependency;
    #else
                              Args.ItemList "-k" self.Key;
                              Args.Item "--sn" self.StrongNameKey;
    #endif
                              Args.Item "-x" self.XmlReport;
                              Args.ItemList "-f" self.FileFilter;
                              Args.ItemList "-s" self.AssemblyFilter;
                              Args.ItemList "-e" self.AssemblyExcludeFilter;
                              Args.ItemList "-t" self.TypeFilter;
                              Args.ItemList "-m" self.MethodFilter;
                              Args.ItemList "-a" self.AttributeFilter;
                              Args.ItemList "-c" self.CallContext;

                              Args.Flag "--opencover" self.OpenCover.IsPresent
                              Args.Flag "--inplace" self.InPlace.IsPresent
                              Args.Flag "--save" self.Save.IsPresent

                              Args.Item "--" (String.Join(" ", self.CommandLine));
                            ]
                    )
                    |> List.concat
                    |> List.toArray
#if NETCOREAPP2_0
                    |> AltCover.Main.EffectiveMain
#else
#if DEBUG
                    |> AltCover.Main.EffectiveMain
#else
                    |> AltCover.Main.EffectiveMain.Invoke
#endif
#endif
      if status <> 0 then
        let fail = ErrorRecord(InvalidOperationException(), status.ToString(), ErrorCategory.InvalidOperation, self)
        self.WriteError fail
    finally
      Directory.SetCurrentDirectory here
#endif