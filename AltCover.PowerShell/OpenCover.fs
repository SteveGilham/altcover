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
  [<Parameter(ParameterSetName = "XmlDocA", Mandatory = true, Position = 1,
      ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "XmlDocB", Mandatory = true, Position = 1,
      ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val XmlDocument:IXPathNavigable = null with get, set

  [<Parameter(ParameterSetName = "FromFileA", Mandatory = true, Position = 1,
      ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFileB", Mandatory = true, Position = 1,
      ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val InputFile:string = null with get, set

  [<Parameter(ParameterSetName = "XmlDocA", Mandatory = false, Position = 3,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFileA", Mandatory = false, Position = 3,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "XmlDocB", Mandatory = false, Position = 3,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFileB", Mandatory = false, Position = 3,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val OutputFile:string = outputFile with get, set

  [<Parameter(ParameterSetName = "XmlDocA", Mandatory = true, Position = 4,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFileA", Mandatory = true, Position = 4,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val SameSpan:SwitchParameter = SwitchParameter(false) with get, set

  [<Parameter(ParameterSetName = "XmlDocA", Mandatory = false, Position = 4,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFileA", Mandatory = false, Position = 4,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "XmlDocB", Mandatory = true, Position = 4,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFileB", Mandatory = true, Position = 4,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val WithinSequencePoint:SwitchParameter = SwitchParameter(false) with get, set

  member private self.ProcessMethod (m:XmlElement) =
    let sp = m.SelectNodes("/SequencePoint") |> Seq.cast<XmlElement> |> Seq.toList
    let bp = m.SelectNodes("/BranchPoint") |> Seq.cast<XmlElement> |> Seq.toList

    if sp |> List.isEmpty |> not && bp |> List.isEmpty |> not then
        let interleave = List.concat [ sp; bp ]
                        |> List.sortBy (fun x -> x.GetAttribute("offset") |> Int32.TryParse |> snd)
        () // TODO
    ()

  override self.ProcessRecord() =
    let here = Directory.GetCurrentDirectory()
    try
      let where = self.SessionState.Path.CurrentLocation.Path
      Directory.SetCurrentDirectory where
      if self.ParameterSetName.StartsWith("FromFile", StringComparison.Ordinal) then
        self.XmlDocument <- XPathDocument self.InputFile
                
      // Validate
      let xmlDocument =  new XmlDocument()
      self.XmlDocument.CreateNavigator().ReadSubtree() |> xmlDocument.Load
      xmlDocument.Schemas <- XmlUtilities.LoadSchema AltCover.Base.ReportFormat.OpenCover
      xmlDocument.Validate (null)

      // Get all the methods
      xmlDocument.SelectNodes("//Method")
      |> Seq.cast<XmlElement>
      |> Seq.iter self.ProcessMethod

      // tidy up here
      AltCover.Runner.PostProcess null AltCover.Base.ReportFormat.OpenCover xmlDocument

      if self.OutputFile |> String.IsNullOrWhiteSpace |> not then
        xmlDocument.Save(self.OutputFile)

      self.WriteObject xmlDocument
    finally
      Directory.SetCurrentDirectory here