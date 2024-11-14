namespace AltCover.Commands

open System
open System.Collections
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Management.Automation
open AltCover

// # inspired by https://web.archive.org/web/20100330061256/http://www.nivot.org/2008/12/25/ListOfTypeAcceleratorsForPowerShellCTP3.aspx

/// <summary>
/// <para type="synopsis">Add one or more type abbreviations, like the built-in `[xml]` for `System.Xml.XmlDocument`.</para>
/// <para type="description">Extends the built-in set of type abbreviations with user declared ones.  Two common abbreviations are supplied as switch parameters, and then others can be added free-form.</para>
/// <example>
///   <code>Add-Accelerator -XDocument</code>
///   <para>Add `[xdoc]` the easy way</para>
/// </example>
/// <example>
///   <code>Add-Accelerator -Mapping @{ "xdoc" = [type]::gettype("System.Xml.Linq.XDocument")</code>
///   <para>Add `[xdoc]` by the long way round</para>
/// </example>
/// </summary>
[<Cmdlet(VerbsCommon.Add,
         "Accelerator",
         SupportsShouldProcess = true,
         ConfirmImpact = ConfirmImpact.Medium)>]
[<OutputType([| "System.Void" |]); AutoSerializable(false)>]
[<SuppressMessage("Gendarme.Rules.Maintainability",
                  "AvoidLackOfCohesionOfMethodsRule",
                  Justification = "Unchanged code threw this at dotnet 7")>]
type AddAcceleratorCommand() =
  inherit PSCmdlet()

  /// <summary>
  /// <para type="description">Mapping of name to type</para>
  /// <para type="description">`[Key.ToString()]` is the accelerator for `Value.GetType()` (or just `Value` if that is a `System.Type` already.</para>
  /// </summary>
  [<Parameter(Mandatory = false,
              ValueFromPipeline = true,
              ValueFromPipelineByPropertyName = true)>]
  [<SuppressMessage("Microsoft.Usage",
                    "CA2227:CollectionPropertiesShouldBeReadOnly",
                    Justification = "PowerShell parameter type")>]
  member val Mapping = Hashtable() with get, set

  /// <summary>
  /// <para type="description">Add [accelerators] for the accelerator type</para>
  /// </summary>
  [<Parameter(Mandatory = false,
              ValueFromPipeline = false,
              ValueFromPipelineByPropertyName = false)>]
  member val Accelerator = SwitchParameter(false) with get, set

  /// <summary>
  /// <para type="description">Add [xdoc] for the `System.Xml.Linq.XDocument` type</para>
  /// </summary>
  [<Parameter(Mandatory = false,
              ValueFromPipeline = false,
              ValueFromPipelineByPropertyName = false)>]
  member val XDocument = SwitchParameter(false) with get, set

  member val private TypeMap = Dictionary<string, Type>()

  /// <summary>
  /// <para type="description">Initialise the map of accelerator to type</para>
  /// </summary>
  override self.BeginProcessing() = self.TypeMap.Clear()

  /// <summary>
  /// <para type="description">Accumulate new accelerator to type mappings</para>
  /// </summary>
  override self.ProcessRecord() =
    if self.Mapping |> isNull |> not then
      self.Mapping
      |> Seq.cast<DictionaryEntry>
      |> Seq.map (fun x ->
        (x.Key.ToString(),
         match x.Value with
         | :? Type as t -> t
         | other -> other.GetType()))
      |> Seq.distinctBy snd
      |> Seq.iter (fun (k, v) -> self.TypeMap.Add(k, v))

  /// <summary>
  /// <para type="description">Apply the new accelerator to type mappings</para>
  /// </summary>
  override self.EndProcessing() =
    let env =
      System.AppDomain.CurrentDomain.GetAssemblies()

    let sma =
      env
      |> Seq.find (fun a ->
        a
          .GetName()
          .Name.Equals("System.Management.Automation", StringComparison.Ordinal))

    let acceleratorsType =
      sma.GetType("System.Management.Automation.TypeAccelerators")

    let adder =
      acceleratorsType.GetMethod("Add")

    if self.Accelerator.IsPresent then
      self.TypeMap.Add("accelerators", acceleratorsType)

    if self.XDocument.IsPresent then
      self.TypeMap.Add("xdoc", typeof<System.Xml.Linq.XDocument>)

    let finalmap =
      self.TypeMap
      |> Seq.distinctBy _.Key
      |> Seq.distinctBy _.Value
      |> Seq.sortBy _.Key
      |> Seq.toList

    let display =
      String.Join(
        "; ",
        finalmap
        |> Seq.filter (fun kv ->
          (((self.Accelerator.IsPresent
             && kv.Key.Equals("accelerators", StringComparison.Ordinal)
             && kv.Value = acceleratorsType)
            || (self.XDocument.IsPresent
                && kv.Key.Equals("xdoc", StringComparison.Ordinal)
                && kv.Value = typeof<System.Xml.Linq.XDocument>))
           |> not))
        |> Seq.map (fun kv -> sprintf "%A = %A" kv.Key kv.Value.FullName)
      )

    if
      self.ShouldProcess(
        "Command Line : "
        + (if List.isEmpty finalmap then
             String.Empty
           else
             (" -Mapping @{" + display + "}"))
        + (if self.XDocument.IsPresent then
             " -XDocument"
           else
             String.Empty)
        + (if self.Accelerator.IsPresent then
             " -Accelerator"
           else
             String.Empty)
      )
    then
      let nullObject: System.Object = null

      finalmap
      |> Seq.iter (fun kv ->
        adder.Invoke(nullObject, [| kv.Key :> obj; kv.Value :> obj |])
        |> ignore)

/// <summary>
/// <para type="synopsis">List all type abbreviations, like the built-in `[xml]` for `System.Xml.XmlDocument`.</para>
/// <para type="description">Reports all currently available type abbreviations, both system- and user- defined.</para>
/// <example>
///   <code>$a = Get-Accelerator</code>
/// </example>
/// </summary>
[<Cmdlet(VerbsCommon.Get, "Accelerator")>]
[<OutputType([| "System.Collections.Hashtable" |]); AutoSerializable(false)>]
[<SuppressMessage("Microsoft.PowerShell",
                  "PS1101:AllCmdletsShouldAcceptPipelineInput",
                  Justification = "No valid input to accept")>]
type GetAcceleratorCommand() =
  inherit PSCmdlet()

  /// <summary>
  /// <para type="description">List current accelerator to type mappings</para>
  /// </summary>
  override self.EndProcessing() =
    let env =
      System.AppDomain.CurrentDomain.GetAssemblies()

    let sma =
      env
      |> Seq.find (fun a ->
        a
          .GetName()
          .Name.Equals("System.Management.Automation", StringComparison.Ordinal))

    let acceleratorsType =
      sma.GetType("System.Management.Automation.TypeAccelerators")

    let finder =
      acceleratorsType.GetProperty("Get")

    let result = Hashtable()
    let nullObject: System.Object = null

    (finder.GetValue(nullObject, [||]) :?> Dictionary<string, Type>)
    |> Seq.iter (fun kv -> result.Add(kv.Key, kv.Value))

    self.WriteObject result