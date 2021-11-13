namespace AltCover

open System
open System.IO
open System.Linq
open System.Reflection
open System.Xml
open System.Xml.Linq
open System.Xml.Schema
open System.Xml.Xsl

module internal XmlExtensions =
  type System.Xml.Linq.XElement with
    member self.SetAttribute(name: string, value: string) =
      let attr = self.Attribute(XName.Get name)

      if attr |> isNull then
        self.Add(XAttribute(XName.Get name, value))
      else
        attr.Value <- value

/// <summary>
/// <para type="description">Methods for working on XML types</para>
/// </summary>
[<RequireQualifiedAccess>]
module internal XmlUtilities =
  let internal loadSchema (format: AltCover.ReportFormat) =
    let schemas = XmlSchemaSet()
    let here = Assembly.GetExecutingAssembly()

    let resource =
      here.GetName().Name
      + match format with
        | AltCover.ReportFormat.NCover -> ".xsd.NCoverEmbedded.xsd"
        | _ -> ".xsd.OpenCover.xsd"

    use stream = here.GetManifestResourceStream(resource)
    use reader = new StreamReader(stream)
    use xreader = XmlReader.Create(reader)
    schemas.Add(String.Empty, xreader) |> ignore
    schemas

  let internal loadTransform (name: string) =
    let transform = XslCompiledTransform()
    let here = Assembly.GetExecutingAssembly()
    let prefix = here.GetName().Name

    use stream =
      here.GetManifestResourceStream(prefix + ".xsl." + name + ".xsl")

    use reader = new StreamReader(stream)
    use xreader = XmlReader.Create(reader)
    transform.Load(xreader, XsltSettings.TrustedXslt, XmlUrlResolver())
    transform

  let internal discoverFormat (xmlDocument: XDocument) =
    let format =
      if xmlDocument
        .Descendants(XName.Get "CoverageSession")
        .Any() then
        AltCover.ReportFormat.OpenCover
      else
        AltCover.ReportFormat.NCover

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