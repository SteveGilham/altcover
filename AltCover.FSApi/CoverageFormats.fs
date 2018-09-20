namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Linq
open System.Xml
open System.Xml.Linq
open System.Xml.XPath

module CoverageFormats =

  [<SuppressMessage("Microsoft.Design", "CA1059",
                    Justification = "converts concrete types")>]
  let ConvertToLcov xmlDocument stream =
    let format = XmlUtilities.DiscoverFormat xmlDocument
    let xdoc = XmlUtilities.ToXDocument xmlDocument
    AltCover.LCov.ConvertReport xdoc format stream

  [<SuppressMessage("Microsoft.Design", "CA1059",
                    Justification = "converts concrete types")>]
  let ConvertToCobertura xmlDocument =
    let format = XmlUtilities.DiscoverFormat xmlDocument
    let xdoc = XmlUtilities.ToXDocument xmlDocument
    AltCover.Cobertura.ConvertReport xdoc format

  [<SuppressMessage("Microsoft.Design", "CA1059",
                    Justification = "returns a specific concrete type")>]
  let ConvertFromNCover (navigable : IXPathNavigable) (assemblies : string array) =
    let reporter, rewrite = AltCover.OpenCover.ReportGenerator()
    let visitors = [ reporter ]
    let navigator = navigable.CreateNavigator()
    let identities = Dictionary<string, XPathNavigator>()
    navigator.Select("//module").OfType<XPathNavigator>()
    |> Seq.iter (fun n ->
         let key = n.GetAttribute("assemblyIdentity", String.Empty)
         identities.Add(key, n))

    let paths = Dictionary<string, string>()
    assemblies
    |> Seq.iter (fun p ->
         let a =
           XmlUtilities.AssemblyNameWithFallback p (Path.GetFileNameWithoutExtension p)
         paths.Add(p, a))
    let usefulAssemblies =
      assemblies |> Seq.filter (fun p -> identities.ContainsKey paths.[p])

    // ensure default state -- this switches branch recording off
    AltCover.Main.init()

    AltCover.Visitor.Visit visitors usefulAssemblies

    let parse s =
      Int32.TryParse
        (s, System.Globalization.NumberStyles.Integer,
         System.Globalization.CultureInfo.InvariantCulture) |> snd
    // Match modules
    rewrite.Descendants(XName.Get "Module")
    |> Seq.iter
         (fun target ->
         let path =
           target.Descendants(XName.Get "ModulePath")
           |> Seq.map (fun n -> n.Value)
           |> Seq.head

         let identity = paths.[path]
         let source = identities.[identity]
         let files = Dictionary<string, string>()
         target.Descendants(XName.Get "File").OfType<XElement>()
         |> Seq.iter
              (fun f ->
              files.Add
                (f.Attribute(XName.Get "fullPath").Value,
                 f.Attribute(XName.Get "uid").Value))

         // Copy sequence points across
         source.Select(".//seqpnt").OfType<XPathNavigator>()
         |> Seq.iter
              (fun s ->
              let sl = s.GetAttribute("line", String.Empty)
              let sc = s.GetAttribute("column", String.Empty)
              let el = s.GetAttribute("endline", String.Empty)
              let ec = s.GetAttribute("endcolumn", String.Empty)
              let uid = files.[s.GetAttribute("document", String.Empty)]
              let vc = parse <| s.GetAttribute("visitcount", String.Empty)
              let xpath =
                ".//SequencePoint[@sl='" + sl + "' and @sc='" + sc + "' and @el='" + el
                + "' and @ec='" + ec + "' and @fileid='" + uid + "']"
              let sp = Extensions.XPathSelectElement(target, xpath)
              let v = parse <| sp.Attribute(XName.Get "vc").Value
              let visits = (max 0 v) + (max 0 vc)
              sp.Attribute(XName.Get "vc").Value <- visits.ToString
                                                      (System.Globalization.CultureInfo.InvariantCulture)))
    let dec = rewrite.Declaration
    dec.Encoding <- "utf-8"
    dec.Standalone <- null

    let converted = XmlUtilities.ToXmlDocument rewrite
    AltCover.Runner.PostProcess null AltCover.Base.ReportFormat.OpenCover converted
    converted

  [<SuppressMessage("Microsoft.Design", "CA1059",
                    Justification = "returns a specific concrete type")>]
  let ConvertToNCover(navigable : IXPathNavigable) =
    let transform = XmlUtilities.LoadTransform "OpenCoverToNCover"
    let rewrite = XmlDocument()
    do use output = rewrite.CreateNavigator().AppendChild()
       transform.Transform(navigable, output)
    XmlUtilities.PrependDeclaration rewrite

    rewrite.SelectNodes("//method").OfType<XmlElement>()
    |> Seq.iter (fun m ->
         let c = m.GetAttribute("class")
         m.SetAttribute("class", c.Replace('/', '+'))
         let name = m.GetAttribute("name")
         let lead = name.Substring(name.LastIndexOf("::", StringComparison.Ordinal) + 2)
         m.SetAttribute("name", lead.Substring(0, lead.IndexOf('('))))

    rewrite.SelectNodes("//module").OfType<XmlElement>()
    |> Seq.iter
         (fun m ->
         let path = m.GetAttribute("name")
         let info = System.IO.FileInfo path
         m.SetAttribute("name", info.Name)
         let assembly = m.GetAttribute("assembly")
         m.SetAttribute
           ("assemblyIdentity", XmlUtilities.AssemblyNameWithFallback path assembly))

    let culture = System.Threading.Thread.CurrentThread.CurrentCulture
    try
      System.Threading.Thread.CurrentThread.CurrentCulture <- CultureInfo.InvariantCulture
      rewrite.SelectNodes("//coverage").OfType<XmlElement>()
      |> Seq.iter (fun c ->
           let now =
             DateTime.UtcNow.ToLongDateString() + ":" + DateTime.UtcNow.ToLongTimeString()
           c.SetAttribute("startTime", now)
           c.SetAttribute("measureTime", now))
    finally
      System.Threading.Thread.CurrentThread.CurrentCulture <- culture
    rewrite