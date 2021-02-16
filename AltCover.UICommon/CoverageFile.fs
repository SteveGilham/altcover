namespace AltCover

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
    let xmlTransform = XslCompiledTransform()
    xmlTransform.Load(stylesheet, XsltSettings(false, true), null)
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
      transformFromOtherCover document "AltCover.UICommon.OpenCoverToNCoverEx.xsl"
    report

  // PartCover to NCover style sheet
  let internal convertFile (helper : CoverageTool -> XDocument -> XDocument -> XDocument)
      (document : XDocument) =
    let schemas = XmlSchemaSet()
    use sr1 = new StreamReader(Assembly.GetExecutingAssembly()
                                       .GetManifestResourceStream("AltCover.UICommon.OpenCover.xsd"))
    use ocreader = XmlReader.Create(sr1)
    use sr2 = new StreamReader(Assembly.GetExecutingAssembly()
                                       .GetManifestResourceStream("AltCover.UICommon.NCover.xsd"))

// LCov : TN:
// JSON : {
// XML : <

// XML formats after any preamble (skip <? and <!)
// NCover : <coverage profilerVersion=
// OpenCover : <CoverageSession xmlns:xsd=
// Cobertura : <coverage line-rate=
// dotCover : <Root CoveredStatements=
// PartCover : <PartCoverReport version= // many versions
// Dynamic : <results>
// mprof : <coverage version="0.3">
// Visual Studio : <CoverageDSPriv>

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
          let schemas2 = XmlSchemaSet()
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
type CoverageFile =
  { File : FileInfo
    Document : XDocument }

  static member ToCoverageFile (helper : CoverageTool -> XDocument -> XDocument -> XDocument)
                (file : FileInfo) =
    try
      let name = file.FullName
      let rawDocument = if (name |> Path.GetExtension).ToUpperInvariant() = ".JSON"
                        then name |> NativeJson.fileToJson |> NativeJson.jsonToXml
                        else XDocument.Load name
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
    | :? Manatee.Json.JsonSyntaxException as e ->
        Left
          { Fault = e
            File = file }
    | :? Manatee.Json.JsonValueIncorrectTypeException as e ->
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
  Target="AltCover.Extensions.#Choice`2.ToOption.Static`2(Microsoft.FSharp.Core.FSharpChoice`2<!!0,!!1>)",
  MessageId="x",
  Justification="Trivial usage")>]
()