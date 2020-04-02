namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Linq
open System.Reflection
open System.Xml
open System.Xml.Linq
open System.Xml.Schema
open System.Xml.XPath
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
  let ToXmlDocument(document : XDocument) =
    let xmlDocument = XmlDocument()
    use xmlReader = document.CreateReader()
    xmlDocument.Load(xmlReader)

    let xDeclaration = document.Declaration
    if xDeclaration.IsNotNull
    then
      let xmlDeclaration =
        xmlDocument.CreateXmlDeclaration
          (xDeclaration.Version, xDeclaration.Encoding, xDeclaration.Standalone)

      xmlDocument.InsertBefore(xmlDeclaration, xmlDocument.FirstChild) |> ignore
    xmlDocument :> IXPathNavigable

  let ToXDocument(xmlDocument : IXPathNavigable) =
    let nav = xmlDocument.CreateNavigator()
    nav.MoveToRoot()
    use nodeReader = nav.ReadSubtree()
    let xdoc = XDocument.Load(nodeReader)
    xdoc.Declaration  <- XDeclaration("1.0", "utf-8", String.Empty)
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

  let internal discoverFormat(xmlDocument : IXPathNavigable) =
    let nav = xmlDocument.CreateNavigator()
    let format =
      if nav.SelectChildren("CoverageSession", String.Empty).OfType<XPathNavigator>().Any()
      then AltCover.Base.ReportFormat.OpenCover
      else AltCover.Base.ReportFormat.NCover

    let schema = loadSchema format
    nav.CheckValidity(schema, null) |> ignore
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

  let internal prependDeclaration(x : IXPathNavigable) =
    x.CreateNavigator().PrependChild("""<?xml version="1.0" encoding="utf-8"?>""" +
                                     Environment.NewLine)