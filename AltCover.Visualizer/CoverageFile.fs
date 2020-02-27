namespace AltCover.Visualizer

open System
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
  | OpenCover = 2

[<NoComparison>]
type InvalidFile =
  { File : FileInfo
    Fault : Exception }

module Transformer =
  let internal DefaultHelper (_ : XDocument) (document : XDocument) = document

  let internal LoadTransform(path : string) =
    let stylesheet =
      XmlReader.Create(Assembly.GetExecutingAssembly().GetManifestResourceStream(path))
    let xmlTransform = new XslCompiledTransform()
    xmlTransform.Load(stylesheet, new XsltSettings(false, true), null)
    xmlTransform

  [<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2202",
                                 Justification = "Multiple Close() should be safe")>]
  let internal TransformFromOtherCover (document : XNode) (path : string) =
    let xmlTransform = LoadTransform path
    use buffer = new MemoryStream()
    use sw = new StreamWriter(buffer)
    // transform the document:
    xmlTransform.Transform(document.CreateReader(), null, sw)
    buffer.Position <- 0L
    use reader = XmlReader.Create(buffer)
    XDocument.Load(reader)

  let internal TransformFromOpenCover(document : XNode) =
    let report =
      TransformFromOtherCover document "AltCover.Visualizer.OpenCoverToNCoverEx.xsl"
    report

  // PartCover to NCover style sheet
  let internal ConvertFile (helper : CoverageTool -> XDocument -> XDocument -> XDocument)
      (document : XDocument) =
    let schemas = new XmlSchemaSet()
    use ocreader = XmlReader.Create(new StreamReader(Assembly.GetExecutingAssembly()
                                         .GetManifestResourceStream("AltCover.Visualizer.OpenCover.xsd")))
    use ncreader = XmlReader.Create(new StreamReader(Assembly.GetExecutingAssembly()
                                         .GetManifestResourceStream("AltCover.Visualizer.NCover.xsd")))
    try
      match document.XPathSelectElements("/CoverageSession").Count() with
      | 1 ->
          schemas.Add
            (String.Empty, ocreader)
          |> ignore
          document.Validate(schemas, null)
          let report = TransformFromOpenCover document
          let fixedup = helper CoverageTool.OpenCover document report
          // Consistency check our XSLT
          let schemas2 = new XmlSchemaSet()
          schemas2.Add
            (String.Empty, ncreader)
          |> ignore
          fixedup.Validate(schemas2, null)
          Right fixedup
      | _ ->
          schemas.Add
            (String.Empty, ncreader)
          |> ignore
          document.Validate(schemas, null)
          Right document
    with
    | :? ArgumentNullException as x -> Left(x :> Exception)
    | :? NullReferenceException as x -> Left(x :> Exception)
    | :? IOException as x -> Left(x :> Exception)
    | :? XsltException as x -> Left(x :> Exception)
    | :? XmlSchemaValidationException as x -> Left(x :> Exception)
    | :? ArgumentException as x -> Left(x :> Exception)

[<NoComparison>]
type internal CoverageFile =
  { File : FileInfo
    Document : XDocument }

  static member ToCoverageFile (helper : CoverageTool -> XDocument -> XDocument -> XDocument)
                (file : FileInfo) =
    try
      let rawDocument = XDocument.Load(file.FullName)
      match Transformer.ConvertFile helper rawDocument with
      | Left x ->
          Left
            { Fault = x
              File = file }
      | Right doc ->
          Right
            { File = file
              Document = doc }
    with
    | :? NullReferenceException as e ->
        Left
          { Fault = e
            File = file }
    | :? XmlException as e ->
        Left
          { Fault = e
            File = file }
    | :? IOException as e ->
        Left
          { Fault = e
            File = file }

  static member LoadCoverageFile(file : FileInfo) =
    CoverageFile.ToCoverageFile (fun x -> Transformer.DefaultHelper) file

type internal Coverage = Either<InvalidFile, CoverageFile>

module Extensions =
  type Choice<'b, 'a> with
    static member toOption (x : Either<'a, 'b>) =
      match x with
      | Right y -> Some y
      | _ -> None