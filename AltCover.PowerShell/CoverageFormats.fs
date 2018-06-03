namespace AltCover.Commands

#if MONO
module CoverageFormats =
    let hello name =
        printfn "Hello %s" name
#else

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Management.Automation
open System.Xml
open System.Xml.Linq
open System.Xml.XPath
open System.Xml.Xsl

open AltCover.PowerShell
open AltCover.PowerShell

[<Cmdlet(VerbsData.ConvertTo, "XmlDocument")>]
[<OutputType(typeof<XmlDocument>)>]
type ConvertToXmlDocumentCommand(xdocument:XDocument) =
  inherit PSCmdlet()

  new () = ConvertToXmlDocumentCommand(null)

  [<SuppressMessage("Microsoft.Design", "CA1059", Justification="converts concrete types")>]
  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = true, Position = 1,
      ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val XDocument:XDocument = xdocument with get, set

  override self.ProcessRecord() =
    self.XDocument |> XmlUtilities.ToXmlDocument |> self.WriteObject

[<Cmdlet(VerbsData.ConvertTo, "XDocument")>]
[<OutputType(typeof<XDocument>)>]
type ConvertToXDocumentCommand(xmldocument:XmlDocument) =
  inherit PSCmdlet()

  new () = ConvertToXDocumentCommand(null)

  [<SuppressMessage("Microsoft.Design", "CA1059", Justification="converts concrete types")>]
  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = true, Position = 1,
      ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val XmlDocument:XmlDocument = xmldocument with get, set

  override self.ProcessRecord() =
    self.XmlDocument |> XmlUtilities.ToXDocument |> self.WriteObject

[<Cmdlet(VerbsData.ConvertTo, "Lcov")>]
[<OutputType("System.Void")>]
[<SuppressMessage("Microsoft.PowerShell", "PS1008", Justification = "Lcov is OK")>]
type ConvertToLcovCommand(outputFile:String) =
  inherit PSCmdlet()

  new () = ConvertToLcovCommand(String.Empty)

  [<SuppressMessage("Microsoft.Design", "CA1059", Justification="converts concrete types")>]
  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = true, Position = 1,
      ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val XmlDocument:XmlDocument = null with get, set

  [<Parameter(ParameterSetName = "FromFile", Mandatory = true, Position = 1,
      ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val InputFile:string = null with get, set

  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = true, Position = 2,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFile", Mandatory = true, Position = 2,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val OutputFile:string = outputFile with get, set

  override self.ProcessRecord() =
    let here = Directory.GetCurrentDirectory()
    try
      let where = self.SessionState.Path.CurrentLocation.Path
      Directory.SetCurrentDirectory where
      if self.ParameterSetName = "FromFile" then
        self.XmlDocument <- XmlDocument()
        self.XmlDocument.Load self.InputFile
      let format = XmlUtilities.DiscoverFormat self.XmlDocument
      let xdoc = XmlUtilities.ToXDocument self.XmlDocument
      use stream = File.Open(self.OutputFile, FileMode.OpenOrCreate, FileAccess.Write)
#if NETCOREAPP2_0
      AltCover.LCov.ConvertReport xdoc format stream
#else
#if DEBUG
      AltCover.LCov.ConvertReport xdoc format stream
#else
      AltCover.LCov.ConvertReport (xdoc, format, stream)
#endif
#endif
    finally
      Directory.SetCurrentDirectory here

[<Cmdlet(VerbsData.ConvertTo, "Cobertura")>]
[<OutputType(typeof<XDocument>)>]
[<SuppressMessage("Microsoft.PowerShell", "PS1008", Justification = "Cobertura is OK")>]
type ConvertToCoberturaCommand(outputFile:String) =
  inherit PSCmdlet()

  new () = ConvertToCoberturaCommand(String.Empty)

  [<SuppressMessage("Microsoft.Design", "CA1059", Justification="converts concrete types")>]
  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = true, Position = 1,
      ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val XmlDocument:XmlDocument = null with get, set

  [<Parameter(ParameterSetName = "FromFile", Mandatory = true, Position = 1,
      ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val InputFile:string = null with get, set

  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = false, Position = 2,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFile", Mandatory = false, Position = 2,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val OutputFile:string = outputFile with get, set

  override self.ProcessRecord() =
    let here = Directory.GetCurrentDirectory()
    try
      let where = self.SessionState.Path.CurrentLocation.Path
      Directory.SetCurrentDirectory where
      if self.ParameterSetName = "FromFile" then
        self.XmlDocument <- XmlDocument()
        self.XmlDocument.Load self.InputFile
      let format = XmlUtilities.DiscoverFormat self.XmlDocument
      let xdoc = XmlUtilities.ToXDocument self.XmlDocument
#if NETCOREAPP2_0
      let rewrite = AltCover.Cobertura.ConvertReport xdoc format
#else
#if DEBUG
      let rewrite = AltCover.Cobertura.ConvertReport xdoc format
#else
      let rewrite = AltCover.Cobertura.ConvertReport (xdoc, format)
#endif
#endif
      if self.OutputFile |> String.IsNullOrWhiteSpace |> not then
        rewrite.Save(self.OutputFile)

      self.WriteObject rewrite
    finally
      Directory.SetCurrentDirectory here

[<Cmdlet(VerbsData.ConvertTo, "NCover")>]
[<OutputType(typeof<System.Void>)>]
[<SuppressMessage("Microsoft.PowerShell", "PS1008", Justification = "Cobertura is OK")>]
type ConvertToNCoverCommand(outputFile:String) =
  inherit PSCmdlet()

  new () = ConvertToNCoverCommand(String.Empty)

  [<SuppressMessage("Microsoft.Design", "CA1059", Justification="converts concrete types")>]
  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = true, Position = 1,
      ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val XmlDocument:IXPathNavigable = null with get, set

  [<Parameter(ParameterSetName = "FromFile", Mandatory = true, Position = 1,
      ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val InputFile:string = null with get, set

  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = true, Position = 2,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFile", Mandatory = true, Position = 2,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val OutputFile:string = outputFile with get, set

  override self.ProcessRecord() =
    let here = Directory.GetCurrentDirectory()
    try
      let where = self.SessionState.Path.CurrentLocation.Path
      Directory.SetCurrentDirectory where
      if self.ParameterSetName = "FromFile" then
        self.XmlDocument <- XPathDocument self.InputFile
      let transform = XmlUtilities.LoadTransform "OpenCoverToNCover"
      use output = XmlWriter.Create(self.OutputFile)
      transform.Transform (self.XmlDocument, output)
    finally
      Directory.SetCurrentDirectory here
#endif