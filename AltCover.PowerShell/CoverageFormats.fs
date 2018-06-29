namespace AltCover.Commands

#if MONO
module CoverageFormats =
    let hello name =
        printfn "Hello %s" name
#else

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
[<OutputType(typeof<XmlDocument>)>]
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
        self.XmlDocument <- XPathDocument self.InputFile
      let transform = XmlUtilities.LoadTransform "OpenCoverToNCover"
      let rewrite = XmlDocument()
      do
        use output = rewrite.CreateNavigator().AppendChild()
        transform.Transform (self.XmlDocument, output)

      let xmlDeclaration = rewrite.CreateXmlDeclaration(
                                        "1.0",
                                        "utf-8",
                                        null)
      rewrite.InsertBefore(xmlDeclaration, rewrite.FirstChild) |> ignore

      rewrite.SelectNodes("//method").OfType<XmlElement>()
      |> Seq.iter (fun m -> let c = m.GetAttribute("class")
                            m.SetAttribute("class", c.Replace('/', '+'))
                            let name = m.GetAttribute("name")
                            let lead = name.Substring(name.LastIndexOf("::", StringComparison.Ordinal) + 2)
                            m.SetAttribute("name", lead.Substring(0, lead.IndexOf('('))))

      rewrite.SelectNodes("//module").OfType<XmlElement>()
      |> Seq.iter (fun m -> let path = m.GetAttribute("name")
                            let info = System.IO.FileInfo path
                            m.SetAttribute("name", info.Name)
                            let assembly = m.GetAttribute("assembly")
                            m.SetAttribute("assemblyIdentity",
                                           XmlUtilities.AssemblyNameWithFallback path assembly))

      let culture = System.Threading.Thread.CurrentThread.CurrentCulture
      try
          System.Threading.Thread.CurrentThread.CurrentCulture <- CultureInfo.InvariantCulture
          rewrite.SelectNodes("//coverage").OfType<XmlElement>()
          |> Seq.iter (fun c -> let now = DateTime.UtcNow.ToLongDateString() +
                                          ":" + DateTime.UtcNow.ToLongTimeString()
                                c.SetAttribute("startTime", now)
                                c.SetAttribute("measureTime", now)
                      )
      finally
          System.Threading.Thread.CurrentThread.CurrentCulture <- culture

      if self.OutputFile |> String.IsNullOrWhiteSpace |> not then
        rewrite.Save(self.OutputFile)

      self.WriteObject rewrite
    finally
      Directory.SetCurrentDirectory here

[<Cmdlet(VerbsData.ConvertFrom, "NCover")>]
[<OutputType(typeof<XmlDocument>)>]
[<SuppressMessage("Microsoft.PowerShell", "PS1008", Justification = "Cobertura is OK")>]
type ConvertFromNCoverCommand(outputFile:String) =
  inherit PSCmdlet()

  new () = ConvertFromNCoverCommand(String.Empty)

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

  override self.ProcessRecord() =
    let here = Directory.GetCurrentDirectory()
    try
      let where = self.SessionState.Path.CurrentLocation.Path
      Directory.SetCurrentDirectory where
      if self.ParameterSetName = "FromFile" then
        self.XmlDocument <- XPathDocument self.InputFile

      let reporter, rewrite = AltCover.OpenCover.ReportGenerator ()
      let visitors = [ reporter ]
      let navigator = self.XmlDocument.CreateNavigator()
      let identities = Dictionary<string, XPathNavigator>()
      navigator.Select("//module").OfType<XPathNavigator>()
      |> Seq.iter (fun n -> let key = n.GetAttribute("assemblyIdentity",String.Empty)
                            identities.Add(key, n))

      let paths = Dictionary<string, string>()
      self.Assembly
      |> Seq.iter(fun p -> let a = XmlUtilities.AssemblyNameWithFallback p (Path.GetFileNameWithoutExtension p)
                           paths.Add(p, a))

      let assemblies = self.Assembly
                       |> Seq.filter(fun p -> identities.ContainsKey paths.[p])

      // ensure default state -- this switches branch recording off
      AltCover.Main.init()

#if NETCOREAPP2_0
      AltCover.Visitor.Visit visitors assemblies
#else
#if DEBUG
      AltCover.Visitor.Visit visitors assemblies
#else
      AltCover.Visitor.Visit(visitors, assemblies)
#endif
#endif
      let parse s = Int32.TryParse(s,
                                   System.Globalization.NumberStyles.Integer,
                                   System.Globalization.CultureInfo.InvariantCulture) |> snd

      // Match modules
      rewrite.Descendants(XName.Get "Module")
      |> Seq.iter (fun target -> let path = target.Descendants(XName.Get "ModulePath")
                                            |> Seq.map (fun n -> n.Value)
                                            |> Seq.head
                                 let identity = paths.[path]
                                 let source = identities.[identity]
                                 let files = Dictionary<string,string>()
                                 target.Descendants(XName.Get "File").OfType<XElement>()
                                 |> Seq.iter(fun f -> files.Add(f.Attribute(XName.Get "fullPath").Value,
                                                                f.Attribute(XName.Get "uid").Value))

      // Copy sequence points across
                                 source.Select(".//seqpnt").OfType<XPathNavigator>() |>
                                 Seq.iter(fun s -> let sl = s.GetAttribute("line",String.Empty)
                                                   let sc = s.GetAttribute("column",String.Empty)
                                                   let el = s.GetAttribute("endline",String.Empty)
                                                   let ec = s.GetAttribute("endcolumn",String.Empty)
                                                   let uid = files.[s.GetAttribute("document",String.Empty)]
                                                   let vc = parse <| s.GetAttribute("visitcount", String.Empty)
                                                   let xpath = ".//SequencePoint[@sl='" + sl +
                                                                                          "' and @sc='" + sc +
                                                                                          "' and @el='" + el +
                                                                                          "' and @ec='" + ec +
                                                                                          "' and @fileid='" + uid + "']"
                                                   let sp = Extensions.XPathSelectElement(target, xpath)
                                                   let v = parse <| sp.Attribute(XName.Get "vc").Value
                                                   let visits = (max 0 v) + (max 0 vc)
                                                   sp.Attribute(XName.Get "vc").Value <- visits.ToString(System.Globalization.CultureInfo.InvariantCulture)))

      let dec = rewrite.Declaration
      dec.Encoding <- "utf-8"
      dec.Standalone <- null

      let converted = XmlUtilities.ToXmlDocument rewrite
#if NETCOREAPP2_0
      AltCover.Runner.PostProcess null AltCover.Base.ReportFormat.OpenCover converted
#else
#if DEBUG
      AltCover.Runner.PostProcess null AltCover.Base.ReportFormat.OpenCover converted
#else
      AltCover.Runner.PostProcess (null, AltCover.Base.ReportFormat.OpenCover, converted)
#endif
#endif

      if self.OutputFile |> String.IsNullOrWhiteSpace |> not then
        converted.Save(self.OutputFile)

      self.WriteObject converted
    finally
      Directory.SetCurrentDirectory here

#endif