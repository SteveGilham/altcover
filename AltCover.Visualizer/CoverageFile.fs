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

[<SuppressMessage("Microsoft.Design", "CA1027:MarkEnumsWithFlags",
  Justification="Not used or intended as a flag")>]
type CoverageTool =
  | NCoverAlike = 0
  | OpenCover = 2

[<NoComparison; AutoSerializable(false)>]
type InvalidFile =
  { File : FileInfo
    Fault : Exception }

module Transformer =
  let internal defaultHelper (_ : XDocument) (document : XDocument) = document

  let internal loadTransform(path : string) =
    use str = Assembly.GetExecutingAssembly().GetManifestResourceStream(path)
    use stylesheet =
      XmlReader.Create(str)
    let xmlTransform = new XslCompiledTransform()
    xmlTransform.Load(stylesheet, new XsltSettings(false, true), null)
    xmlTransform

  let internal transformFromOtherCover (document : XNode) (path : string) =
    let xmlTransform = loadTransform path
    use buffer = new MemoryStream()
    use sw = new StreamWriter(buffer)
    // transform the document:
    xmlTransform.Transform(document.CreateReader(), null, sw)
    buffer.Position <- 0L
    use reader = XmlReader.Create(buffer)
    XDocument.Load(reader)

  let internal transformFromOpenCover(document : XNode) =
    let report =
      transformFromOtherCover document "AltCover.Visualizer.OpenCoverToNCoverEx.xsl"
    report

  // PartCover to NCover style sheet
  let internal convertFile (helper : CoverageTool -> XDocument -> XDocument -> XDocument)
      (document : XDocument) =
    let schemas = new XmlSchemaSet()
    use sr1 = new StreamReader(Assembly.GetExecutingAssembly()
                                       .GetManifestResourceStream("AltCover.Visualizer.OpenCover.xsd"))
    use ocreader = XmlReader.Create(sr1)
    use sr2 = new StreamReader(Assembly.GetExecutingAssembly()
                                       .GetManifestResourceStream("AltCover.Visualizer.NCover.xsd"))

    use ncreader = XmlReader.Create(sr2)
    try
      match document.XPathSelectElements("/CoverageSession").Count() with
      | 1 ->
          schemas.Add
            (String.Empty, ocreader)
          |> ignore
          document.Validate(schemas, null)
          let report = transformFromOpenCover document
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

[<NoComparison; AutoSerializable(false)>]
type internal CoverageFile =
  { File : FileInfo
    Document : XDocument }

  static member ToCoverageFile (helper : CoverageTool -> XDocument -> XDocument -> XDocument)
                (file : FileInfo) =
    try
      let rawDocument = XDocument.Load(file.FullName)
      match Transformer.convertFile helper rawDocument with
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
    CoverageFile.ToCoverageFile (fun x -> Transformer.defaultHelper) file

type internal Coverage = Either<InvalidFile, CoverageFile>

module Extensions =
  type Choice<'b, 'a> with
    static member ToOption (x : Either<'a, 'b>) =
      match x with
      | Right y -> Some y
      | _ -> None

[<assembly: SuppressMessage("Microsoft.Naming",
  "CA1704:IdentifiersShouldBeSpelledCorrectly", Scope="member",
  Target="AltCover.Visualizer.Extensions.#Choice`2.ToOption.Static`2(Microsoft.FSharp.Core.FSharpChoice`2<!!0,!!1>)",
  MessageId="x",
  Justification="Trivial usage")>]
()