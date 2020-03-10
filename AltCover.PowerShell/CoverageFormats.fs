namespace AltCover.Commands

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Management.Automation
open System.Xml
open System.Xml.Linq
open System.Xml.XPath

[<Cmdlet(VerbsData.ConvertTo, "XmlDocument")>]
[<OutputType(typeof<XmlDocument>); AutoSerializable(false)>]
type ConvertToXmlDocumentCommand(xdocument : XDocument) =
  inherit PSCmdlet()
  new() = ConvertToXmlDocumentCommand(null)

  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = true, Position = 1,
              ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val XDocument : XDocument = xdocument with get, set

  override self.ProcessRecord() =
    self.XDocument
    |> AltCover.XmlUtilities.ToXmlDocument
    |> self.WriteObject

[<Cmdlet(VerbsData.ConvertTo, "XDocument")>]
[<OutputType(typeof<XDocument>); AutoSerializable(false)>]
type ConvertToXDocumentCommand(xmldocument : XmlDocument) =
  inherit PSCmdlet()

  new() = ConvertToXDocumentCommand(null)

  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = true, Position = 1,
              ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val XmlDocument : XmlDocument = xmldocument with get, set

  override self.ProcessRecord() =
    self.XmlDocument
    |> AltCover.XmlUtilities.ToXDocument
    |> self.WriteObject

[<Cmdlet(VerbsData.ConvertTo, "Lcov")>]
[<OutputType("System.Void"); AutoSerializable(false)>]
[<SuppressMessage("Microsoft.PowerShell", "PS1008", Justification = "Lcov is OK")>]
type ConvertToLcovCommand(outputFile : String) =
  inherit PSCmdlet()
  new() = ConvertToLcovCommand(String.Empty)

  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = true, Position = 1,
              ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val XmlDocument : XmlDocument = null with get, set

  [<Parameter(ParameterSetName = "FromFile", Mandatory = true, Position = 1,
              ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val InputFile : string = null with get, set

  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = true, Position = 2,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFile", Mandatory = true, Position = 2,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val OutputFile : string = outputFile with get, set

  override self.ProcessRecord() =
    let here = Directory.GetCurrentDirectory()
    try
      let where = self.SessionState.Path.CurrentLocation.Path
      Directory.SetCurrentDirectory where
      if self.ParameterSetName = "FromFile" then
        self.XmlDocument <- XmlDocument()
        self.XmlDocument.Load self.InputFile
      use stream = File.Open(self.OutputFile, FileMode.OpenOrCreate, FileAccess.Write)
      AltCover.CoverageFormats.ConvertToLcov self.XmlDocument stream
    finally
      Directory.SetCurrentDirectory here

[<Cmdlet(VerbsData.ConvertTo, "Cobertura")>]
[<OutputType(typeof<XDocument>); AutoSerializable(false)>]
[<SuppressMessage("Microsoft.PowerShell", "PS1008", Justification = "Cobertura is OK")>]
type ConvertToCoberturaCommand(outputFile : String) =
  inherit PSCmdlet()
  new() = ConvertToCoberturaCommand(String.Empty)

  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = true, Position = 1,
              ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val XmlDocument : XmlDocument = null with get, set

  [<Parameter(ParameterSetName = "FromFile", Mandatory = true, Position = 1,
              ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val InputFile : string = null with get, set

  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = false, Position = 2,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFile", Mandatory = false, Position = 2,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val OutputFile : string = outputFile with get, set

  override self.ProcessRecord() =
    let here = Directory.GetCurrentDirectory()
    try
      let where = self.SessionState.Path.CurrentLocation.Path
      Directory.SetCurrentDirectory where
      if self.ParameterSetName = "FromFile" then
        self.XmlDocument <- XmlDocument()
        self.XmlDocument.Load self.InputFile

      let rewrite = AltCover.CoverageFormats.ConvertToCobertura self.XmlDocument

      if self.OutputFile
         |> String.IsNullOrWhiteSpace
         |> not
      then rewrite.Save(self.OutputFile)

      self.WriteObject rewrite
    finally
      Directory.SetCurrentDirectory here

[<Cmdlet(VerbsData.ConvertTo, "NCover")>]
[<OutputType(typeof<XmlDocument>); AutoSerializable(false)>]
[<SuppressMessage("Microsoft.PowerShell", "PS1008", Justification = "Cobertura is OK")>]
type ConvertToNCoverCommand(outputFile : String) =
  inherit PSCmdlet()
  new() = ConvertToNCoverCommand(String.Empty)

  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = true, Position = 1,
              ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val XmlDocument : IXPathNavigable = null with get, set

  [<Parameter(ParameterSetName = "FromFile", Mandatory = true, Position = 1,
              ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val InputFile : string = null with get, set

  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = false, Position = 2,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFile", Mandatory = false, Position = 2,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val OutputFile : string = outputFile with get, set

  override self.ProcessRecord() =
    let here = Directory.GetCurrentDirectory()
    try
      let where = self.SessionState.Path.CurrentLocation.Path
      Directory.SetCurrentDirectory where
      if self.ParameterSetName = "FromFile" then
        self.XmlDocument <- XPathDocument self.InputFile

      let rewrite = AltCover.CoverageFormats.ConvertToNCover self.XmlDocument

      if self.OutputFile
         |> String.IsNullOrWhiteSpace
         |> not
      then rewrite.Save(self.OutputFile)

      self.WriteObject rewrite
    finally
      Directory.SetCurrentDirectory here

[<Cmdlet(VerbsData.ConvertFrom, "NCover")>]
[<OutputType(typeof<XmlDocument>); AutoSerializable(false)>]
[<SuppressMessage("Microsoft.PowerShell", "PS1008", Justification = "Cobertura is OK")>]
type ConvertFromNCoverCommand(outputFile : String) =
  inherit PSCmdlet()
  new() = ConvertFromNCoverCommand(String.Empty)

  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = true, Position = 1,
              ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val XmlDocument : IXPathNavigable = null with get, set

  [<Parameter(ParameterSetName = "FromFile", Mandatory = true, Position = 1,
              ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val InputFile : string = null with get, set

  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = true, Position = 2,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFile", Mandatory = true, Position = 2,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<System.Diagnostics.CodeAnalysis.SuppressMessage(
      "Gendarme.Rules.Performance", "AvoidReturningArraysOnPropertiesRule",
      Justification = "Cannot convert 'System.Object[]' to the type 'System.Collections.Generic.IEnumerable`1[System.String]'")>]
  member val Assembly : string array = [||] with get, set

  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = false, Position = 3,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFile", Mandatory = false, Position = 3,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val OutputFile : string = outputFile with get, set

  override self.ProcessRecord() =
    let here = Directory.GetCurrentDirectory()
    try
      let where = self.SessionState.Path.CurrentLocation.Path
      Directory.SetCurrentDirectory where
      if self.ParameterSetName = "FromFile" then
        self.XmlDocument <- XPathDocument self.InputFile

      let converted =
        AltCover.CoverageFormats.ConvertFromNCover self.XmlDocument self.Assembly

      if self.OutputFile
         |> String.IsNullOrWhiteSpace
         |> not
      then converted.Save(self.OutputFile)

      self.WriteObject converted
    finally
      Directory.SetCurrentDirectory here