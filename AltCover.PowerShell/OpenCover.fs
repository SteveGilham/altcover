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
    let sp = m.GetElementsByTagName("SequencePoint").OfType<XmlElement>() |> Seq.toList
    let bp = m.GetElementsByTagName("BranchPoint").OfType<XmlElement>() |> Seq.toList

    if sp |> List.isEmpty |> not && bp |> List.isEmpty |> not then
        let interleave = List.concat [ sp; bp ]
                        |> List.sortBy (fun x -> x.GetAttribute("offset") |> Int32.TryParse |> snd)
        interleave
        |> Seq.fold (fun (s:XmlElement, bs:XmlElement list) x -> match x.Name with
                                                                 | "SequencePoint" ->
                                                                   let bx = if self.WithinSequencePoint.IsPresent then
                                                                              let next = x.GetAttribute("offset") |> Int32.TryParse |> snd
                                                                              let (kill, keep) = bs
                                                                                                 |> List.partition (fun b -> b.GetAttribute("offsetend")
                                                                                                                             |> Int32.TryParse |> snd < next)
                                                                              kill
                                                                              |> Seq.iter (fun b -> b.ParentNode.RemoveChild(b) |> ignore)
                                                                              keep
                                                                            else bs

                                                                   let by = if self.SameSpan.IsPresent then
                                                                              let (kill, keep) = bx
                                                                                                 |> List.groupBy (fun b -> (b.GetAttribute("offset"), b.GetAttribute("offsetchain"),b.GetAttribute("offsetend")))
                                                                                                 |> List.fold (fun (ki,ke) (_, bz) -> let totalVisits = bz
                                                                                                                                                        |> Seq.map (fun b -> b.GetAttribute("vc")
                                                                                                                                                                             |> Int32.TryParse |> snd)
                                                                                                                                                        |> Seq.sum
                                                                                                                                      let h = bz |> Seq.head
                                                                                                                                      h.SetAttribute ("vc", totalVisits.ToString(CultureInfo.InvariantCulture))
                                                                                                                                      (List.concat [ki; bz |> Seq.tail |> Seq.toList] ,h::ke)) ([], [])
                                                                              kill
                                                                              |> Seq.iter (fun b -> b.ParentNode.RemoveChild(b) |> ignore)
                                                                              keep
                                                                             else bx

                                                                   // Fix up what remains
                                                                   by
                                                                   |> List.rev // because the list will have been built up in reverse order
                                                                   |> Seq.mapi (fun i b -> (i,b))
                                                                   |> Seq.groupBy (fun (_,b) ->  b.GetAttribute("offset"))
                                                                   |> Seq.iter (fun (_, paths) -> paths  // assume likely ranges for these numbers!
                                                                                                  |> Seq.sortBy (fun (n,p) -> n + 100 * (p.GetAttribute("offsetend")
                                                                                                                                         |> Int32.TryParse |> snd))
                                                                                                  |> Seq.iteri (fun i (_,p) -> p.SetAttribute("path", (i + 1).ToString(CultureInfo.InvariantCulture))))

                                                                   s.SetAttribute("bec", by.Length.ToString(CultureInfo.InvariantCulture))
                                                                   s.SetAttribute("bev", "0")

                                                                   (x, [])
                                                                 | _ -> (s, x::bs)
                         ) (sp.Head, [])
        |> ignore

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
      let xmlDeclaration = xmlDocument.CreateXmlDeclaration("1.0",
                                       "utf-8",
                                       null)
      xmlDocument.InsertBefore(xmlDeclaration, xmlDocument.FirstChild) |> ignore

      if self.OutputFile |> String.IsNullOrWhiteSpace |> not then
        xmlDocument.Save(self.OutputFile)

      self.WriteObject xmlDocument
    finally
      Directory.SetCurrentDirectory here