namespace AltCover.Commands

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Management.Automation
open System.Xml.Linq
open System.Xml.XPath

/// <summary>
/// <para type="synopsis">Generates a simple HTML report from coverage data.</para>
/// <para type="description">The report produced is based on the old NCover 1.5.8 XSLT, for both NCover and OpenCover coverage format data.  The input is as a file name or an `XDocument` from the pipeline, the output is to the pipeline as an `XDocument`, and, optionally, to a file. </para>
/// <example>
///   <code>    $xml = ConvertTo-BarChart -InputFile "./Tests/HandRolledMonoCoverage.xml" -OutputFile "./_Packaging/HandRolledMonoCoverage.html"</code>
/// </example>
/// </summary>
[<Cmdlet(VerbsData.ConvertTo, "BarChart")>]
[<OutputType(typeof<XDocument>); AutoSerializable(false)>]
[<SuppressMessage("Microsoft.PowerShell", "PS1008", Justification = "Cobertura is OK")>]
type ConvertToBarChartCommand() =
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

  /// <summary>
  /// <para type="description">Create transformed document</para>
  /// </summary>
  override self.ProcessRecord() =
    let here = Directory.GetCurrentDirectory()
    try
      let where = self.SessionState.Path.CurrentLocation.Path
      Directory.SetCurrentDirectory where
      if self.ParameterSetName = "FromFile" then
        self.XDocument <- XDocument.Load self.InputFile
      let rewrite = AltCover.Xhtml.ConvertToBarChart self.XDocument
      if self.OutputFile
         |> String.IsNullOrWhiteSpace
         |> not
      then
        use w =
          { new StringWriter(System.Globalization.CultureInfo.InvariantCulture) with
              override self.Encoding = System.Text.Encoding.UTF8 }
        rewrite.Save(w)
        File.WriteAllText(self.OutputFile, w.ToString().Replace("\u2442", "&#x2442;"))
      self.WriteObject rewrite
    finally
      Directory.SetCurrentDirectory here