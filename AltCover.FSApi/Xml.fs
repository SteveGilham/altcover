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

open Augment

module XmlExtensions =
  type System.Xml.Linq.XElement with
    member self.SetAttribute(name: string, value : string) =
      let attr = self.Attribute(XName.Get name)
      if attr |> isNull
      then self.Add(XAttribute(XName.Get name, value))
      else attr.Value <- value

[<RequireQualifiedAccess>]
module XmlUtilities =
  [<SuppressMessage("Gendarme.Rules.BadPractice","PreferEmptyInstanceOverNullRule",
    Justification="Null means absent, completely void, in this case")>]
  let private nullIfEmpty s =
    if String.IsNullOrEmpty s
    then null
    else s

  [<SuppressMessage("Microsoft.Design", "CA1059",
                    Justification = "converts concrete types")>]
  let ToXmlDocument(document : XDocument) =
    let xmlDocument = XmlDocument()
    use xmlReader = document.CreateReader()
    xmlDocument.Load(xmlReader)

    let cn = xmlDocument.ChildNodes
    match cn.OfType<XmlDocumentType>() |> Seq.tryHead with
    | None -> ()
    | Some doctype -> let xDoctype = document.DocumentType
                      let newDoctype = xmlDocument.CreateDocumentType(nullIfEmpty xDoctype.Name,
                                                                      nullIfEmpty xDoctype.PublicId,
                                                                      nullIfEmpty xDoctype.SystemId,
                                                                      nullIfEmpty xDoctype.InternalSubset)
                      xmlDocument.ReplaceChild(newDoctype, doctype) |> ignore

    let xDeclaration = document.Declaration
    if xDeclaration.IsNotNull
    then
      let xmlDeclaration =
        xmlDocument.CreateXmlDeclaration
          (xDeclaration.Version, xDeclaration.Encoding, xDeclaration.Standalone)

      xmlDocument.InsertBefore(xmlDeclaration, xmlDocument.FirstChild) |> ignore
    xmlDocument

  [<SuppressMessage("Microsoft.Design", "CA1059",
                    Justification = "converts concrete types")>]
  [<SuppressMessage(
    "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
    Justification = "AvoidSpeculativeGenerality too")>]
  let ToXDocument(xmlDocument : XmlDocument) =
    use nodeReader = new XmlNodeReader(xmlDocument)
    nodeReader.MoveToContent() |> ignore // skips leading comments
    let xdoc = XDocument.Load(nodeReader)
    let cn = xmlDocument.ChildNodes
    match cn.OfType<XmlDocumentType>() |> Seq.tryHead with
    | None -> ()
    | Some doctype -> xdoc.AddFirst(XDocumentType(nullIfEmpty doctype.Name,
                                                  nullIfEmpty doctype.PublicId,
                                                  nullIfEmpty doctype.SystemId,
                                                  nullIfEmpty doctype.InternalSubset))

    let decl' = cn.OfType<XmlDeclaration>() |> Seq.tryHead
    match decl' with
    | None -> ()
    | Some decl ->
        xdoc.Declaration <- XDeclaration(decl.Version, decl.Encoding, decl.Standalone)
    cn.OfType<XmlProcessingInstruction>()
    |> Seq.rev
    |> Seq.iter
         (fun func -> xdoc.AddFirst(XProcessingInstruction(func.Target, func.Data)))
    xdoc

  let internal loadSchema(format : AltCover.Base.ReportFormat) =
    let schemas = new XmlSchemaSet()

    let resource =
      match format with
      | AltCover.Base.ReportFormat.NCover -> "AltCover.FSApi.xsd.NCover.xsd"
      | _ -> "AltCover.FSApi.xsd.OpenCover.xsd"

    use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
    use reader = new StreamReader(stream)
    use xreader = XmlReader.Create(reader)
    schemas.Add(String.Empty, xreader) |> ignore
    schemas

  let internal loadTransform(name : string) =
    let transform = new XslCompiledTransform()
    use stream =
      Assembly.GetExecutingAssembly()
              .GetManifestResourceStream("AltCover.FSApi.xsl." + name + ".xsl")
    use reader = new StreamReader(stream)
    use xreader = XmlReader.Create(reader)
    transform.Load(xreader, XsltSettings.TrustedXslt, XmlUrlResolver())
    transform

  let internal discoverFormat(xmlDocument : XDocument) =
    let format =
      if xmlDocument.Descendants(XName.Get "CoverageSession").Any()
      then AltCover.Base.ReportFormat.OpenCover
      else AltCover.Base.ReportFormat.NCover

    let schema = loadSchema format
    xmlDocument.Validate(schema, null) |> ignore
    format

  let internal assemblyNameWithFallback path fallback =
    try
      AssemblyName.GetAssemblyName(path).FullName
    with
    | :? ArgumentException
    | :? FileNotFoundException
    | :? System.Security.SecurityException
    | :? BadImageFormatException
    | :? FileLoadException -> fallback