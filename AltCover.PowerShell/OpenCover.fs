namespace AltCover.Commands

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.Globalization
open System.IO
open System.Linq
open System.Management.Automation
open System.Xml
open System.Xml.Linq
open System.Xml.XPath

open AltCover.PowerShell

[<Cmdlet(VerbsData.Compress, "Branching")>]
[<OutputType(typeof<XmlDocument>)>]
type CompressBranchingCommand(outputFile:String) =
  inherit PSCmdlet()

  new () = CompressBranchingCommand(String.Empty)

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
  member val Assembly:string array = [| |] with get, set

  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = false, Position = 3,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFile", Mandatory = false, Position = 3,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val OutputFile:string = outputFile with get, set

  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = false, Position = 4,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFile", Mandatory = false, Position = 4,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val SameSpan:SwitchParameter = SwitchParameter(false) with get, set

  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = false, Position = 4,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFile", Mandatory = false, Position = 4,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val WithinSequencePoint:SwitchParameter = SwitchParameter(false) with get, set

  override self.ProcessRecord() =
    let here = Directory.GetCurrentDirectory()
    try
      let where = self.SessionState.Path.CurrentLocation.Path
      Directory.SetCurrentDirectory where
      if self.ParameterSetName = "FromFile" then
        self.XmlDocument <- XPathDocument self.InputFile

      let converted = XmlDocument()

      // do work here

      AltCover.Runner.PostProcess null AltCover.Base.ReportFormat.OpenCover converted

      if self.OutputFile |> String.IsNullOrWhiteSpace |> not then
        converted.Save(self.OutputFile)

      self.WriteObject converted
    finally
      Directory.SetCurrentDirectory here