namespace AltCover.Commands

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Management.Automation
open System.Xml
open System.Xml.Linq

/// <summary>
/// <para type="synopsis">Converts `XDocument` to `[xml]`.</para>
/// <para type="description">Takes an `XDocument` in and puts an `[xml]` to the object pipeline.</para>
/// <example>
///   <code>Add-Accelerator -XDocument; $xd = [xdoc]::Load("./Tests/Sample1WithNCover.xml");  $xml = $xd | ConvertTo-XmlDocument</code>
///   <para>Adds `[xdoc]` the easy way first, then does the conversion</para>
/// </example>
/// </summary>
[<Cmdlet(VerbsData.ConvertTo, "XmlDocument")>]
[<OutputType(typeof<XmlDocument>); AutoSerializable(false)>]
type ConvertToXmlDocumentCommand() =
  inherit PSCmdlet()

  /// <summary>
  /// <para type="description">Input as `XDocument` value</para>
  /// </summary>
  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = true, Position = 1,
              ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val XDocument : XDocument = null with get, set

  override self.ProcessRecord() =
    self.XDocument
    |> AltCover.XmlTypes.ToXmlDocument
    |> self.WriteObject

/// <summary>
/// <para type="synopsis">Converts `[xml]` to `XDocument`.</para>
/// <para type="description">Takes an `[xml]` in and puts an `XDocument` to the object pipeline.</para>
/// <example>
///   <code>$xd = [xml]"<document/>" | ConvertTo-XDocument</code>
/// </example>
/// </summary>
[<Cmdlet(VerbsData.ConvertTo, "XDocument")>]
[<OutputType(typeof<XDocument>); AutoSerializable(false)>]
type ConvertToXDocumentCommand() =
  inherit PSCmdlet()

  /// <summary>
  /// <para type="description">Input as `[xml]` value</para>
  /// </summary>
  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = true, Position = 1,
              ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  [<SuppressMessage("Microsoft.Design", "CA1059:MembersShouldNotExposeCertainConcreteTypes",
    Justification = "AvoidSpeculativeGenerality too")>]
  member val XmlDocument : XmlDocument = null with get, set

  override self.ProcessRecord() =
    self.XmlDocument
    |> AltCover.XmlTypes.ToXDocument
    |> self.WriteObject

/// <summary>
/// <para type="synopsis">Creates an Lcov format report from other report formats.</para>
/// <para type="description">Takes either OpenCover or classic NCover format input as an `XDocument`, as an argument or from the object pipeline. Writes the Lcov report to a file.</para>
/// <example>
///   <code>ConvertTo-Lcov -InputFile "./Tests/HandRolledMonoCoverage.xml" -OutputFile "./_Packaging/OpenCover.lcov"</code>
/// </example>
/// </summary>
[<Cmdlet(VerbsData.ConvertTo, "Lcov")>]
[<OutputType("System.Void"); AutoSerializable(false)>]
[<SuppressMessage("Microsoft.PowerShell", "PS1008", Justification = "Lcov is a name")>]
[<SuppressMessage("Microsoft.Naming", "CA1704", Justification = "Lcov is a name")>]
type ConvertToLcovCommand() =
  inherit PSCmdlet()

  /// <summary>
  /// <para type="description">Input as `XDocument` value</para>
  /// </summary>
  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = true, Position = 1,
              ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val XDocument : XDocument = null with get, set

  /// <summary>
  /// <para type="description">Input as file path</para>
  /// </summary>
  [<Parameter(ParameterSetName = "FromFile", Mandatory = true, Position = 1,
              ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val InputFile : string = null with get, set

  /// <summary>
  /// <para type="description">Output as file path</para>
  /// </summary>
  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = true, Position = 2,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFile", Mandatory = true, Position = 2,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val OutputFile : string = String.Empty with get, set

  override self.ProcessRecord() =
    let here = Directory.GetCurrentDirectory()
    try
      let where = self.SessionState.Path.CurrentLocation.Path
      Directory.SetCurrentDirectory where
      if self.ParameterSetName = "FromFile" then
        self.XDocument <- XDocument.Load self.InputFile
      use stream = File.Open(self.OutputFile, FileMode.OpenOrCreate, FileAccess.Write)
      AltCover.CoverageFormats.ConvertToLcov self.XDocument stream
    finally
      Directory.SetCurrentDirectory here

/// <summary>
/// <para type="synopsis">Creates a Cobertura format report from other report formats.</para>
/// <para type="description">Takes either OpenCover or classic NCover format input as an `XDocument`, as an argument or from the object pipeline.</para>
/// <para type="description">Writes the Cobertura report to the object pipeline as an `XDocument`, and optionally to a file.</para>
/// <example>
///   <code>ConvertTo-Cobertura -InputFile "./Tests/HandRolledMonoCoverage.xml" -OutputFile "./_Packaging/OpenCover.lcov"</code>
/// </example>
/// </summary>
[<Cmdlet(VerbsData.ConvertTo, "Cobertura")>]
[<OutputType(typeof<XDocument>); AutoSerializable(false)>]
[<SuppressMessage("Microsoft.PowerShell", "PS1008", Justification = "Cobertura is a name")>]
[<SuppressMessage("Microsoft.Naming", "CA1704", Justification = "Cobertura is a name")>]
type ConvertToCoberturaCommand() =
  inherit PSCmdlet()

  /// <summary>
  /// <para type="description">Input as `XDocument` value</para>
  /// </summary>
  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = true, Position = 1,
              ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val XDocument : XDocument = null with get, set

  /// <summary>
  /// <para type="description">Input as file path</para>
  /// </summary>
  [<Parameter(ParameterSetName = "FromFile", Mandatory = true, Position = 1,
              ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val InputFile : string = null with get, set

  /// <summary>
  /// <para type="description">Output as file path</para>
  /// </summary>
  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = false, Position = 2,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFile", Mandatory = false, Position = 2,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val OutputFile : string = String.Empty with get, set

  override self.ProcessRecord() =
    let here = Directory.GetCurrentDirectory()
    try
      let where = self.SessionState.Path.CurrentLocation.Path
      Directory.SetCurrentDirectory where
      if self.ParameterSetName = "FromFile" then
        self.XDocument <- XDocument.Load self.InputFile

      let rewrite = AltCover.CoverageFormats.ConvertToCobertura self.XDocument

      if self.OutputFile
         |> String.IsNullOrWhiteSpace
         |> not
      then rewrite.Save(self.OutputFile)

      self.WriteObject rewrite
    finally
      Directory.SetCurrentDirectory here

/// <summary>
/// <para type="synopsis">Converts OpenCover format to NCover format.</para>
/// <para type="description">Takes the OpenCover input either as an ``XDocument`` from the object pipeline or from a file.</para>
/// <para type="description">Writes the classic NCover report to the pipeline as an ``XDocument``, and, optionally, to a file.</para>
/// <example>
///   <code>$xml = ConvertTo-NCover -InputFile "./Tests/HandRolledMonoCoverage.xml" -OutputFile "./_Packaging/HandRolledMonoNCover.xml"</code>
/// </example>
/// </summary>
[<Cmdlet(VerbsData.ConvertTo, "NCover")>]
[<OutputType(typeof<XDocument>); AutoSerializable(false)>]
type ConvertToNCoverCommand() =
  inherit PSCmdlet()

  /// <summary>
  /// <para type="description">Input as `XDocument` value</para>
  /// </summary>
  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = true, Position = 1,
              ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val XDocument : XDocument = null with get, set

  /// <summary>
  /// <para type="description">Input as file path</para>
  /// </summary>
  [<Parameter(ParameterSetName = "FromFile", Mandatory = true, Position = 1,
              ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val InputFile : string = null with get, set

  /// <summary>
  /// <para type="description">Output as file path</para>
  /// </summary>
  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = false, Position = 2,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFile", Mandatory = false, Position = 2,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val OutputFile : string = String.Empty with get, set

  override self.ProcessRecord() =
    let here = Directory.GetCurrentDirectory()
    try
      let where = self.SessionState.Path.CurrentLocation.Path
      Directory.SetCurrentDirectory where
      if self.ParameterSetName = "FromFile" then
        self.XDocument <- XDocument.Load self.InputFile

      let rewrite = AltCover.CoverageFormats.ConvertToNCover self.XDocument

      if self.OutputFile
         |> String.IsNullOrWhiteSpace
         |> not
      then
        rewrite.Save(self.OutputFile)

      self.WriteObject rewrite
    finally
      Directory.SetCurrentDirectory here

/// <summary>
/// <para type="synopsis">Converts classic NCover format and returns OpenCover format.</para>
/// <para type="description">The classic NCover format input may be either may be as an `XDocument` from the object pipeline or from a file.</para>
/// <para type="description">Writes the OpenCover format report to the pipeline as an `XDocument`, and, optionally, to a file.  The report will contain data for the assemblies listed as the `-Assembly` argument and that are in the NCover input.</para>
/// <example>
///   <code>    $xml = ConvertFrom-NCover -InputFile "./_Reports/ReleaseXUnitFSharpTypesDotNetRunner.xml" -Assembly $Assemblies -OutputFile "./_Packaging/AltCoverFSharpTypes.xml"</code>
/// </example>
/// </summary>
[<Cmdlet(VerbsData.ConvertFrom, "NCover")>]
[<OutputType(typeof<XDocument>); AutoSerializable(false)>]
type ConvertFromNCoverCommand() =
  inherit PSCmdlet()

  /// <summary>
  /// <para type="description">Input as `XDocument` value</para>
  /// </summary>
  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = true, Position = 1,
              ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val XDocument : XDocument = null with get, set

  /// <summary>
  /// <para type="description">Input as file path</para>
  /// </summary>
  [<Parameter(ParameterSetName = "FromFile", Mandatory = true, Position = 1,
              ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val InputFile : string = null with get, set

  /// <summary>
  /// <para type="description">Assemblies to use for generating the output</para>
  /// </summary>
  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = true, Position = 2,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFile", Mandatory = true, Position = 2,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<SuppressMessage(
      "Gendarme.Rules.Performance", "AvoidReturningArraysOnPropertiesRule",
      Justification = "Cannot convert 'System.Object[]' to the type 'System.Collections.Generic.IEnumerable`1[System.String]'")>]
  [<SuppressMessage("Microsoft.Performance", "CA1819",
                    Justification = "ditto, ditto")>]
  member val Assembly : string array = [||] with get, set

  /// <summary>
  /// <para type="description">Output as file path</para>
  /// </summary>
  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = false, Position = 3,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFile", Mandatory = false, Position = 3,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val OutputFile : string = String.Empty with get, set

  override self.ProcessRecord() =
    let here = Directory.GetCurrentDirectory()
    try
      let where = self.SessionState.Path.CurrentLocation.Path
      Directory.SetCurrentDirectory where
      if self.ParameterSetName = "FromFile" then
        self.XDocument <- XDocument.Load self.InputFile

      let converted =
        AltCover.CoverageFormats.ConvertFromNCover self.XDocument self.Assembly

      if self.OutputFile
         |> String.IsNullOrWhiteSpace
         |> not
      then converted.Save(self.OutputFile)

      self.WriteObject converted
    finally
      Directory.SetCurrentDirectory here

/// <summary>
/// <para type="synopsis">Fills in gaps in `coverlet`'s OpenCover dialect.</para>
/// <para type="description">Adds summary data and other items to report in `coverlet`'s OpenCover dialect, particularly giving somewhat meaningful start and end column values for its line-based paradigm, as well as npath coverage and branch exits.</para>
/// <example>
///   <code>    $xml = Format-FromCoverletOpenCover -InputFile "./_Reports/OpenCoverForPester/OpenCoverForPester.coverlet.xml" -Assembly $Assemblies -OutputFile "./_Packaging/OpenCoverForPester.coverlet.xml"</code>
/// </example>
/// </summary>
[<Cmdlet(VerbsCommon.Format, "FromCoverletOpenCover")>]
[<OutputType(typeof<XDocument>); AutoSerializable(false)>]
type FormatFromCoverletOpenCoverCommand() =
  inherit PSCmdlet()

  /// <summary>
  /// <para type="description">Input as `XDocument` value</para>
  /// </summary>
  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = true, Position = 1,
              ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val XDocument : XDocument = null with get, set

  /// <summary>
  /// <para type="description">Input as file path</para>
  /// </summary>
  [<Parameter(ParameterSetName = "FromFile", Mandatory = true, Position = 1,
              ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val InputFile : string = null with get, set

  /// <summary>
  /// <para type="description">Assemblies to use for generating the output</para>
  /// </summary>
  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = true, Position = 2,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFile", Mandatory = true, Position = 2,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<SuppressMessage(
      "Gendarme.Rules.Performance", "AvoidReturningArraysOnPropertiesRule",
      Justification = "Cannot convert 'System.Object[]' to the type 'System.Collections.Generic.IEnumerable`1[System.String]'")>]
  [<SuppressMessage("Microsoft.Performance", "CA1819",
                    Justification = "ditto, ditto")>]
  member val Assembly : string array = [||] with get, set

  /// <summary>
  /// <para type="description">Output as file path</para>
  /// </summary>
  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = false, Position = 2,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFile", Mandatory = false, Position = 2,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val OutputFile : string = String.Empty with get, set

  override self.ProcessRecord() =
    let here = Directory.GetCurrentDirectory()
    try
      let where = self.SessionState.Path.CurrentLocation.Path
      Directory.SetCurrentDirectory where

      if self.ParameterSetName = "FromFile" then
        self.XDocument <- XDocument.Load self.InputFile

      let rewrite = AltCover.OpenCover.FormatFromCoverlet self.XDocument self.Assembly

      if self.OutputFile
         |> String.IsNullOrWhiteSpace
         |> not
      then rewrite.Save(self.OutputFile)

      self.WriteObject rewrite
    finally
      Directory.SetCurrentDirectory here