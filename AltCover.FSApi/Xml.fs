namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Linq
open System.Reflection
open System.Xml
open System.Xml.Linq
open System.Xml.Schema
open System.Xml.Xsl

module XmlUtilities =
  [<SuppressMessage("Microsoft.Design", "CA1059",
                    Justification = "converts concrete types")>]
  let ToXmlDocument(xDocument : XDocument) =
    let xmlDocument = XmlDocument()
    use xmlReader = xDocument.CreateReader()
    xmlDocument.Load(xmlReader)

    let xDeclaration = xDocument.Declaration
    if xDeclaration
       |> isNull
       |> not
    then
      let xmlDeclaration =
        xmlDocument.CreateXmlDeclaration(
            xDeclaration.Version,
            xDeclaration.Encoding,
            xDeclaration.Standalone)

      xmlDocument.InsertBefore(xmlDeclaration, xmlDocument.FirstChild) |> ignore
    xmlDocument

  [<SuppressMessage("Microsoft.Design", "CA1059",
                    Justification = "converts concrete types")>]
  let ToXDocument(xmlDocument : XmlDocument) =
    use nodeReader = new XmlNodeReader(xmlDocument)
    nodeReader.MoveToContent() |> ignore // skips leading comments
    let xdoc = XDocument.Load(nodeReader)
    let cn = xmlDocument.ChildNodes
    let decl' = cn.OfType<XmlDeclaration>() |> Seq.tryHead
    match decl' with
    | None -> ()
    | Some decl ->
      xdoc.Declaration <- XDeclaration(
                            decl.Version,
                            decl.Encoding,
                            decl.Standalone)
    cn.OfType<XmlProcessingInstruction>()
    |> Seq.rev
    |> Seq.iter
         (fun func -> xdoc.AddFirst(XProcessingInstruction(func.Target, func.Data)))
    xdoc

  [<SuppressMessage("Microsoft.Usage", "CA2202", Justification = "Observably safe")>]
  let internal LoadSchema(format : AltCover.Base.ReportFormat) =
    let schemas = new XmlSchemaSet()

    use stream =
      match format with
      | AltCover.Base.ReportFormat.NCover ->
        Assembly.GetExecutingAssembly()
                .GetManifestResourceStream("AltCover.FSApi.xsd.NCover.xsd")
      | _ ->
        Assembly.GetExecutingAssembly()
                .GetManifestResourceStream("AltCover.FSApi.xsd.OpenCover.xsd")

    use reader = new StreamReader(stream)
    use xreader = XmlReader.Create(reader)
    schemas.Add(String.Empty, xreader) |> ignore
    schemas

  [<SuppressMessage("Microsoft.Usage", "CA2202", Justification = "Observably safe")>]
  let internal LoadTransform(name : string) =
    let transform = new XslCompiledTransform()
    use stream =
      Assembly.GetExecutingAssembly()
              .GetManifestResourceStream("AltCover.FSApi.xsl." + name + ".xsl")
    use reader = new StreamReader(stream)
    use xreader = XmlReader.Create(reader)
    transform.Load(xreader, XsltSettings.TrustedXslt, XmlUrlResolver())
    transform

  [<SuppressMessage("Microsoft.Design", "CA1059",
                    Justification = "converts concrete types")>]
  let internal DiscoverFormat(xmlDocument : XmlDocument) =
    let format =
      if xmlDocument.SelectNodes("/CoverageSession").OfType<XmlNode>().Any() then
        AltCover.Base.ReportFormat.OpenCover
      else AltCover.Base.ReportFormat.NCover

    let schema = LoadSchema format
    xmlDocument.Schemas <- schema
    xmlDocument.Validate(null)
    format

  let internal AssemblyNameWithFallback path fallback =
    try
      AssemblyName.GetAssemblyName(path).FullName
    with
    | :? ArgumentException
    | :? FileNotFoundException
    | :? System.Security.SecurityException
    | :? BadImageFormatException
    | :? FileLoadException ->
      fallback

  [<SuppressMessage("Microsoft.Design", "CA1059", Justification = "Implies concrete type")>]
  let PrependDeclaration(x : XmlDocument) =
    let xmlDeclaration = x.CreateXmlDeclaration(
                           "1.0",
                           "utf-8",
                           null)
    x.InsertBefore(xmlDeclaration, x.FirstChild) |> ignore