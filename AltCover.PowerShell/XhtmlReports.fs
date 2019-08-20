namespace AltCover.Commands

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Management.Automation
open System.Xml
open System.Xml.XPath

[<Cmdlet(VerbsData.ConvertTo, "BarChart")>]
[<OutputType(typeof<XmlDocument>)>]
[<SuppressMessage("Microsoft.PowerShell", "PS1008", Justification = "Cobertura is OK")>]
type ConvertToBarChartCommand(outputFile : String) =
  inherit PSCmdlet()
  new() = ConvertToBarChartCommand(String.Empty)

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
      let rewrite = AltCover.Xhtml.ConvertToBarChart self.XmlDocument
      if self.OutputFile
         |> String.IsNullOrWhiteSpace
         |> not
      then rewrite.Save(self.OutputFile)
      self.WriteObject rewrite
    finally
      Directory.SetCurrentDirectory here

[<Cmdlet(VerbsData.ConvertTo, "SourceMap")>]
[<OutputType(typeof<Dictionary<string, XmlDocument>>)>]
[<SuppressMessage("Microsoft.PowerShell", "PS1008", Justification = "Cobertura is OK")>]
type ConvertToSourceMapCommand(outputFolder : String) =
  inherit PSCmdlet()
  new() = ConvertToSourceMapCommand(String.Empty)

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
  member val OutputFolder : string = outputFolder with get, set

  override self.ProcessRecord() =
    let here = Directory.GetCurrentDirectory()
    try
      let where = self.SessionState.Path.CurrentLocation.Path
      Directory.SetCurrentDirectory where
      if self.ParameterSetName = "FromFile" then
        let doc = XmlDocument()
        doc.Load self.InputFile
        self.XmlDocument <- doc
      let rewrite = AltCover.RenderToHtml.Action self.XmlDocument
      if self.OutputFolder
         |> String.IsNullOrWhiteSpace
         |> not
      then
        let folder = Directory.CreateDirectory(self.OutputFolder)
        rewrite
        |> Seq.iter( fun (name, doc) -> Path.Combine(folder.FullName, name + ".html") |> doc.Save)
      let result = new Dictionary<string, XmlDocument>()
      rewrite |> Seq.iter (fun (name, doc) -> result.Add(name, doc))
      result |> self.WriteObject
    finally
      Directory.SetCurrentDirectory here