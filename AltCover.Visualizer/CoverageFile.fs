namespace AltCover.Visualizer

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Linq
open System.Reflection
open System.Xml
open System.Xml.Xsl
open System.Xml.Linq
open System.Xml.Schema
open System.Xml.XPath

open AltCover.Augment

type CoverageTool =
        | NCoverAlike = 0
        | PartCover = 1
        | OpenCover = 2

module Exemption =
  [<Literal>]
  let Declared = -1

  [<Literal>]
  let Automatic = -2

  [<Literal>]
  let StaticAnalysis = -3

  [<Literal>]
  let Excluded = -4

type InvalidFile = {
         File : FileInfo;
         Fault : Exception }

module Implementation =

  let DefaultHelper (dummy:XDocument) (document:XDocument) = document

  let LoadTransform (path: string) =
    let stylesheet = XmlReader.Create(Assembly.GetExecutingAssembly().GetManifestResourceStream(path))
    let xmlTransform = new XslCompiledTransform()
    xmlTransform.Load(stylesheet, new XsltSettings(false, true), null)
    xmlTransform

  let TransformFromOtherCover (document : XNode) (path: string) =
    let xmlTransform = LoadTransform path
    let buffer = new MemoryStream()
    let sw = new StreamWriter(buffer)

    // transform the document:
    xmlTransform.Transform(document.CreateReader(), null, sw)
    buffer.Position <- 0L
    XDocument.Load(XmlReader.Create(buffer))

  let TransformFromPartCover (document : XNode) =
    let report = TransformFromOtherCover document "Partcover.xsl"

    // Blazon it for our benfit
    let element = new XElement(XName.Get("PartCoverReport"))
    report.XPathSelectElement("//coverage").LastNode.AddAfterSelf(element)
    report

  let TransformFromOpenCover (document : XNode) =
    let report = TransformFromOtherCover document "Opencover.xsl"

    // Blazon it for our benfit
    let element = new XElement(XName.Get("OpenCoverReport"))
    report.XPathSelectElement("//coverage").LastNode.AddAfterSelf(element)
    report

  // PartCover to NCover style sheet
  let ConvertFile (helper : CoverageTool -> XDocument -> XDocument -> XDocument) (document : XDocument) =
   let schemas = new XmlSchemaSet()
   try
    match (document.XPathSelectElements("/PartCoverReport").Count(),
           document.XPathSelectElements("/CoverageSession").Count()) with
    | (1, 0) -> // looks like a PartCover document so load and apply the XSL transform
        schemas.Add(String.Empty,
                    XmlReader.Create(new StreamReader(Assembly.GetExecutingAssembly()
                                                              .GetManifestResourceStream("PartCover.xsd"))))
          |> ignore
        document.Validate(schemas, null);
        let report = TransformFromPartCover document
        let fixedup = helper CoverageTool.PartCover document report

        // Consistency check our XSLT
        let schemas2 = new XmlSchemaSet()
        schemas2.Add(String.Empty,
                     XmlReader.Create(new StreamReader(Assembly.GetExecutingAssembly()
                                                               .GetManifestResourceStream("Coverage.xsd"))))
          |> ignore
        fixedup.Validate(schemas2, null);

        Right fixedup
    | (0, 1) -> // looks like an OpenCover document so load and apply the XSL transform
        schemas.Add(String.Empty,
                    XmlReader.Create(new StreamReader(Assembly.GetExecutingAssembly()
                                                              .GetManifestResourceStream("OpenCover.xsd"))))
           |> ignore
        document.Validate(schemas, null);
        let report = TransformFromOpenCover document
        let fixedup = helper CoverageTool.OpenCover document report

        // Consistency check our XSLT
        let schemas2 = new XmlSchemaSet()
        schemas2.Add(String.Empty,
                     XmlReader.Create(new StreamReader(Assembly.GetExecutingAssembly()
                                                               .GetManifestResourceStream("Coverage.xsd"))))
          |> ignore
        fixedup.Validate(schemas2, null);

        Right fixedup
    | _ ->  // any other XML
        schemas.Add(String.Empty,
                    XmlReader.Create(new StreamReader(Assembly.GetExecutingAssembly()
                                                              .GetManifestResourceStream("Coverage.xsd"))))
          |> ignore
        document.Validate(schemas, null);
        Right document
   with
   | :? ArgumentNullException as x -> Left (x :> Exception)
   | :? NullReferenceException as x -> Left (x :> Exception)
   | :? IOException as x -> Left (x :> Exception)
   | :? XsltException as x -> Left (x :> Exception)
   | :? XmlSchemaValidationException as x -> Left (x :> Exception)
   | :? ArgumentException as x -> Left (x :> Exception)

type CoverageFile = {
         File : FileInfo;
         [<NonSerialized>]
         Document : XDocument }
     with
      [<SuppressMessage("Microsoft.Design", "CA1006:DoNotNestGenericTypesInMemberSignatures",
       Justification = "F# code -- not visible as such")>]
      static member ToCoverageFile (helper : CoverageTool -> XDocument -> XDocument -> XDocument) (file:FileInfo) =
          try
            let rawDocument = XDocument.Load(file.FullName)
            match Implementation.ConvertFile helper rawDocument with
            | Left x -> Left {Fault = x; File = file}
            | Right doc -> Right {File = file; Document = doc}
          with
          | :? NullReferenceException as e -> Left {Fault = e; File = file}
          | :? XmlException as e -> Left {Fault = e; File = file}
          | :? IOException as e -> Left {Fault = e; File = file}

      static member LoadCoverageFile (file:FileInfo) =
        CoverageFile.ToCoverageFile (fun x -> Implementation.DefaultHelper) file

type Coverage = Either<InvalidFile, CoverageFile>

module Extensions =
    type Choice<'b,'a> with
      static member toOption (x:Either<'a,'b>) =
        match x with
        | Right y -> Some y
        | _ -> None