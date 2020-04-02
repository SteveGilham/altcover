namespace AltCover.Commands

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Management.Automation
open System.Xml
open System.Xml.Linq
open System.Xml.XPath

[<Cmdlet(VerbsData.ConvertTo, "XmlDocument")>]
[<OutputType(typeof<IXPathNavigable>); AutoSerializable(false)>]
type ConvertToXmlDocumentCommand() =
  inherit PSCmdlet()

  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = true, Position = 1,
              ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val XDocument : XDocument = null with get, set

  override self.ProcessRecord() =
    self.XDocument
    |> AltCover.XmlUtilities.ToXmlDocument
    |> self.WriteObject

[<Cmdlet(VerbsData.ConvertTo, "XDocument")>]
[<OutputType(typeof<XDocument>); AutoSerializable(false)>]
type ConvertToXDocumentCommand() =
  inherit PSCmdlet()

  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = true, Position = 1,
              ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val XmlDocument : IXPathNavigable = null with get, set

  override self.ProcessRecord() =
    self.XmlDocument
    |> AltCover.XmlUtilities.ToXDocument
    |> self.WriteObject

[<Cmdlet(VerbsData.ConvertTo, "Lcov")>]
[<OutputType("System.Void"); AutoSerializable(false)>]
[<SuppressMessage("Microsoft.PowerShell", "PS1008", Justification = "Lcov is a name")>]
[<SuppressMessage("Microsoft.Naming", "CA1704", Justification = "Lcov is a name")>]
type ConvertToLcovCommand() =
  inherit PSCmdlet()

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
  member val OutputFile : string = String.Empty with get, set

  override self.ProcessRecord() =
    let here = Directory.GetCurrentDirectory()
    try
      let where = self.SessionState.Path.CurrentLocation.Path
      Directory.SetCurrentDirectory where
      if self.ParameterSetName = "FromFile" then
        let doc = XmlDocument()
        doc.Load self.InputFile
        self.XmlDocument <- doc
      use stream = File.Open(self.OutputFile, FileMode.OpenOrCreate, FileAccess.Write)
      AltCover.CoverageFormats.ConvertToLcov self.XmlDocument stream
    finally
      Directory.SetCurrentDirectory here

[<Cmdlet(VerbsData.ConvertTo, "Cobertura")>]
[<OutputType(typeof<XDocument>); AutoSerializable(false)>]
[<SuppressMessage("Microsoft.PowerShell", "PS1008", Justification = "Cobertura is a name")>]
[<SuppressMessage("Microsoft.Naming", "CA1704", Justification = "Cobertura is a name")>]
type ConvertToCoberturaCommand() =
  inherit PSCmdlet()

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
  member val OutputFile : string = String.Empty with get, set

  override self.ProcessRecord() =
    let here = Directory.GetCurrentDirectory()
    try
      let where = self.SessionState.Path.CurrentLocation.Path
      Directory.SetCurrentDirectory where
      if self.ParameterSetName = "FromFile" then
        let doc = XmlDocument()
        doc.Load self.InputFile
        self.XmlDocument <- doc

      let rewrite = AltCover.CoverageFormats.ConvertToCobertura self.XmlDocument

      if self.OutputFile
         |> String.IsNullOrWhiteSpace
         |> not
      then rewrite.Save(self.OutputFile)

      self.WriteObject rewrite
    finally
      Directory.SetCurrentDirectory here

[<Cmdlet(VerbsData.ConvertTo, "NCover")>]
[<OutputType(typeof<IXPathNavigable>); AutoSerializable(false)>]
type ConvertToNCoverCommand() =
  inherit PSCmdlet()

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
  member val OutputFile : string = String.Empty with get, set

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
      then
        rewrite.Save(self.OutputFile)

      self.WriteObject rewrite
    finally
      Directory.SetCurrentDirectory here

[<Cmdlet(VerbsData.ConvertFrom, "NCover")>]
[<OutputType(typeof<IXPathNavigable>); AutoSerializable(false)>]
type ConvertFromNCoverCommand() =
  inherit PSCmdlet()

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
  [<SuppressMessage(
      "Gendarme.Rules.Performance", "AvoidReturningArraysOnPropertiesRule",
      Justification = "Cannot convert 'System.Object[]' to the type 'System.Collections.Generic.IEnumerable`1[System.String]'")>]
  [<SuppressMessage("Microsoft.Performance", "CA1819",
                    Justification = "ditto, ditto")>]
  member val Assembly : string array = [||] with get, set

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

[<Cmdlet(VerbsCommon.Format, "FromCoverletOpenCover")>]
[<OutputType(typeof<XDocument>); AutoSerializable(false)>]
type FormatFromCoverletOpenCoverCommand() =
  inherit PSCmdlet()

  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = true, Position = 1,
              ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]

  member val Report : XDocument = null with get, set

  [<Parameter(ParameterSetName = "FromFile", Mandatory = true, Position = 1,
              ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val InputFile : string = null with get, set

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
        self.Report <- XDocument.Load self.InputFile

      let rewrite = AltCover.OpenCoverUtilities.FormatFromCoverlet self.Report self.Assembly

      if self.OutputFile
         |> String.IsNullOrWhiteSpace
         |> not
      then rewrite.Save(self.OutputFile)

      self.WriteObject rewrite
    finally
      Directory.SetCurrentDirectory here