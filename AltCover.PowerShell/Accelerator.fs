namespace AltCover.Commands

open System
open System.Collections
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Management.Automation
open AltCover

[<Cmdlet(VerbsCommon.Add, "Accelerator", SupportsShouldProcess = true,
         ConfirmImpact = ConfirmImpact.Medium)>]
[<OutputType([| "System.Void" |]); AutoSerializable(false)>]
type AddAcceleratorCommand() =
  inherit PSCmdlet()

  [<Parameter(Mandatory = false,
              ValueFromPipeline = true, ValueFromPipelineByPropertyName = true)>]
  [<SuppressMessage("Microsoft.Usage", "CA2227:CollectionPropertiesShouldBeReadOnly",
    Justification="PowerShell parameter type")>]
  member val Mapping = Hashtable() with get, set

  [<Parameter(Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val Accelerator = SwitchParameter(false) with get, set

  [<Parameter(Mandatory = false, Position = 1,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val XDocument = SwitchParameter(false) with get, set

  member val private TypeMap = new Dictionary<string, Type>()

  override self.BeginProcessing() = self.TypeMap.Clear()

  override self.ProcessRecord() =
    self.Mapping
    |> Seq.cast<DictionaryEntry>
    |> Seq.map (fun x -> (x.Key.ToString(), match x.Value with
                                            | :? Type as t -> t
                                            | other -> other.GetType()))
    |> Seq.distinctBy snd
    |> Seq.iter (fun (k,v) -> self.TypeMap.Add(k, v))

  override self.EndProcessing() =
    let env = System.AppDomain.CurrentDomain.GetAssemblies()
    let sma = env |> Seq.find (fun a -> a.GetName().Name = "System.Management.Automation")
    let acceleratorsType = sma.GetType("System.Management.Automation.TypeAccelerators")
    let adder = acceleratorsType.GetMethod("Add")

    if self.Accelerator.IsPresent
    then self.TypeMap.Add("accelerators", acceleratorsType)
    if self.XDocument.IsPresent
    then self.TypeMap.Add("xdoc", typeof<System.Xml.Linq.XDocument>)

    let finalmap = self.TypeMap
                   |> Seq.distinctBy(fun kv -> kv.Key)
                   |> Seq.distinctBy(fun kv -> kv.Value)
                   |> Seq.toList
    let display = String.Join("; ", finalmap
                                    |> Seq.map (fun kv -> sprintf "%A = %A" kv.Key, kv.Value.FullName ))

    if  self.ShouldProcess("Command Line : " +
                            (if List.isEmpty finalmap
                             then String.Empty
                             else (" -Type @{"  + display + "}")) +
                            (if self.XDocument.IsPresent
                             then " -XDocument"
                             else String.Empty) +
                            (if self.Accelerator.IsPresent
                             then " -Accelerator"
                             else String.Empty))
    then
      finalmap
      |> Seq.iter (fun kv -> adder.Invoke(null, [| kv.Key :> obj; kv.Value :> obj |]) |> ignore)

[<Cmdlet(VerbsCommon.Get, "Accelerator")>]
[<OutputType([| "System.Collections.Hashtable" |]); AutoSerializable(false)>]
[<SuppressMessage("Microsoft.PowerShell", "PS1101:AllCmdletsShouldAcceptPipelineInput",
  Justification="No valid input to accept")>]
type GetAcceleratorCommand() =
  inherit PSCmdlet()
  override self.EndProcessing() =
    let env = System.AppDomain.CurrentDomain.GetAssemblies()
    let sma = env |> Seq.find (fun a -> a.GetName().Name = "System.Management.Automation")
    let acceleratorsType = sma.GetType("System.Management.Automation.TypeAccelerators")
    let finder = acceleratorsType.GetProperty("Get")

    let result = Hashtable()
    (finder.GetValue(null, [||]) :?> Dictionary<string, Type>)
    |> Seq.iter(fun kv -> result.Add(kv.Key, kv.Value))

    self.WriteObject result