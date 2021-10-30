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

// LCov : TN:  // line-based; branches
// JSON : {
// XML : <
[<NoComparison; AutoSerializable(false)>]
type CoverageDataType =
  | Xml
  | Json
  | LCov
  | Unknown

[<NoComparison; AutoSerializable(false)>]
type XmlCoverageType =
  | NCover // <coverage profilerVersion= // have schema .xsd; SeqPnt based
  | OpenCover // <CoverageSession xmlns:xsd= // have  schema .xsd; SeqPnt based; branches
  | Cobertura

[<NoComparison; AutoSerializable(false)>]
type InvalidFile = { File: FileInfo; Fault: Exception }

module Transformer =
  // now, what was this for??
  let internal defaultHelper (_: XDocument) (document: XDocument) = document

  let internal loadTransform (path: string) =
    use str =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream(path)

    use stylesheet = XmlReader.Create(str)
    let xmlTransform = XslCompiledTransform()
    xmlTransform.Load(stylesheet, XsltSettings(false, true), null)
    xmlTransform

  let internal transformFromOtherCover (document: XNode) (path: string) =
    let xmlTransform = loadTransform path
    use buffer = new MemoryStream()
    use sw = new StreamWriter(buffer)
    // transform the document:
    xmlTransform.Transform(document.CreateReader(), null, sw)
    buffer.Position <- 0L
    use reader = XmlReader.Create(buffer)
    XDocument.Load(reader)

  let internal transformFromOpenCover (document: XNode) =
    let report =
      transformFromOtherCover document "AltCover.UICommon.OpenCoverToNCoverEx.xsl"

    report

  let internal transformFromCobertura (document: XContainer) =
    // normalize file names
    let sources =
      document.Descendants(XName.Get "source")
      |> Seq.map (fun x -> x.Value)
      |> Seq.toList

    document.Descendants(XName.Get "class")
    |> Seq.iter
         (fun x ->
           let name = x.Attribute(XName.Get "filename")
           let value = name.Value

           let fixup =
             sources
             |> Seq.map
                  (fun s ->
                    let p1 = Path.Combine(s, value)
                    Path.GetFullPath p1)
             |> Seq.tryFind File.Exists

           fixup |> Option.iter (fun f -> name.Value <- f))

    // interpret branches
    document.Descendants(XName.Get "line")
    |> Seq.filter
         (fun x ->
           x
             .Attribute(
               XName.Get "condition-coverage"
             )
             .IsNotNull)
    |> Seq.iter
         (fun x ->
           let line = x.Attribute(XName.Get "number").Value

           let coverage =
             x.Attribute(XName.Get "condition-coverage").Value

           let start = Math.Max(0, coverage.IndexOf('(')) + 1

           let mid =
             Math.Max(0, coverage.IndexOf('/', start)) + 1

           let finish = Math.Max(0, coverage.IndexOf(')', mid))

           let first =
             coverage.Substring(start, (mid - start) - 1)
             |> Int32.TryParse
             |> snd

           let second =
             coverage.Substring(mid, finish - mid)
             |> Int32.TryParse
             |> snd

           { 1 .. second }
           |> Seq.iteri
                (fun i _ ->
                  let vc = if i < first then "1" else "0"

                  let branch =
                    XElement(
                      XName.Get "branch",
                      XAttribute(XName.Get "number", line),
                      XAttribute(XName.Get "visitcount", vc)
                    )

                  x.AddAfterSelf(branch)))

    let report =
      transformFromOtherCover document "AltCover.UICommon.CoberturaToNCoverEx.xsl"

    report

  // PartCover to NCover style sheet
  let internal convertFile
    (helper: XmlCoverageType -> XDocument -> XDocument -> XDocument)
    (document: XDocument)
    =
    let schemas = XmlSchemaSet()

    use sr1 =
      new StreamReader(
        Assembly
          .GetExecutingAssembly()
          .GetManifestResourceStream("AltCover.UICommon.OpenCover.xsd")
      )

    use ocreader = XmlReader.Create(sr1)

    use sr2 =
      new StreamReader(
        Assembly
          .GetExecutingAssembly()
          .GetManifestResourceStream("AltCover.UICommon.NCoverEmbedded.xsd")
      )

    use ncreader = XmlReader.Create(sr2)

    try
      // identify coverage type
      match document.Root.Name.LocalName with
      | x when x = "CoverageSession" ->
          // Assume OpenCover
          schemas.Add(String.Empty, ocreader) |> ignore
          document.Validate(schemas, null)

          let report = transformFromOpenCover document

          let fixedup =
            helper XmlCoverageType.OpenCover document report
          // Consistency check our XSLT
          let schemas2 = XmlSchemaSet()
          schemas2.Add(String.Empty, ncreader) |> ignore
          fixedup.Validate(schemas2, null)

          // Fix for column-defective OpenCover
          let lineOnly =
            fixedup.Descendants(XName.Get "seqpnt")
            |> Seq.forall
                 (fun s ->
                   let columns = (s.Attribute(XName.Get "column").Value,
                                  s.Attribute(XName.Get "endcolumn").Value)
                   s.Attribute(XName.Get "line").Value = s
                     .Attribute(
                       XName.Get "endline"
                     )
                     .Value
                   && (columns = ("1", "2") || // For coverlet derived OpenCover (either from coverlet XML or via JsonToXml)
                       columns = ("0", "0")))  // For OpenCover on C++/CLI

          if lineOnly then
            fixedup.Root.Add(XAttribute(XName.Get "lineonly", "true"))

          Right fixedup
      | _ ->
          let root = document.Root

          if root.Name.LocalName = "coverage"
             && root.Attribute(XName.Get "line-rate").IsNotNull then
            // Cobertura
            use cr =
              new StreamReader(
                Assembly
                  .GetExecutingAssembly()
                  .GetManifestResourceStream("AltCover.UICommon.Cobertura.xsd")
              )

            use creader = XmlReader.Create(cr)
            schemas.Add(String.Empty, creader) |> ignore
            document.Validate(schemas, null)
            let report = transformFromCobertura document

            let fixedup =
              helper XmlCoverageType.Cobertura document report
            // Consistency check our XSLT
            let schemas2 = XmlSchemaSet()
            schemas2.Add(String.Empty, ncreader) |> ignore
            fixedup.Validate(schemas2, null)
            Right fixedup
          else
            // Assume NCover
            schemas.Add(String.Empty, ncreader) |> ignore
            document.Validate(schemas, null)
            Right document
    with
    | :? ArgumentNullException as x -> Left(x :> Exception)
    | :? NullReferenceException as x -> Left(x :> Exception)
    | :? IOException as x -> Left(x :> Exception)
    | :? XsltException as x -> Left(x :> Exception)
    | :? XmlSchemaValidationException as x -> Left(x :> Exception)
    | :? ArgumentException as x -> Left(x :> Exception)

  let internal firstChar file =
    use stream = File.OpenRead file
    use reader = new StreamReader(stream)

    let chars =
      Seq.unfold
        (fun (r: StreamReader) ->
          let c = r.Read()
          if c < 0 then None else Some(char c, r))
        reader

    chars
    |> Seq.skipWhile Char.IsWhiteSpace
    |> Seq.tryHead
    |> Option.defaultValue '?'

  // simplest possible heuristic
  let internal identify file =
    match firstChar file with
    | '<' -> Xml
    | '{' -> Json
    | 'T' -> LCov
    | _ -> Unknown

[<NoComparison; AutoSerializable(false)>]
type CoverageFile =
  { File: FileInfo
    Document: XDocument }

  static member ToCoverageFile
    (helper: XmlCoverageType -> XDocument -> XDocument -> XDocument)
    (file: FileInfo)
    =
    try
      let name = file.FullName
      let filetype = Transformer.identify name

      let rawDocument =
        match filetype with
        | Json ->
            name
            |> NativeJson.fileToJson
            |> NativeJson.jsonToXml
        | LCov -> Lcov.toXml name
        | _ -> XDocument.Load name // let invalid files throw XML parse errors

      match Transformer.convertFile helper rawDocument with
      | Left x -> Left { Fault = x; File = file }
      | Right doc -> Right { File = file; Document = doc }
    with
    | :? NullReferenceException as e -> Left { Fault = e; File = file }
    | :? XmlException as e -> Left { Fault = e; File = file }
    | :? Manatee.Json.JsonSyntaxException as e -> Left { Fault = e; File = file }
    | :? Manatee.Json.JsonValueIncorrectTypeException as e ->
        Left { Fault = e; File = file }
    | :? Lcov.LcovParseException as e -> Left { Fault = e; File = file }
    | :? IOException as e -> Left { Fault = e; File = file }

  static member LoadCoverageFile(file: FileInfo) =
    CoverageFile.ToCoverageFile(fun x -> Transformer.defaultHelper) file

type internal Coverage = Either<InvalidFile, CoverageFile>

module Extensions =
  type Choice<'b, 'a> with
    static member ToOption(x: Either<'a, 'b>) =
      match x with
      | Right y -> Some y
      | _ -> None

[<assembly: SuppressMessage("Microsoft.Naming",
                            "CA1704:IdentifiersShouldBeSpelledCorrectly",
                            Scope = "member",
                            Target = "AltCover.Extensions.#Choice`2.ToOption.Static`2(Microsoft.FSharp.Core.FSharpChoice`2<!!0,!!1>)",
                            MessageId = "x",
                            Justification = "Trivial usage")>]
[<assembly: SuppressMessage("Microsoft.Naming",
                            "CA1704:IdentifiersShouldBeSpelledCorrectly",
                            Scope = "member",
                            Target = "AltCover.CoverageDataType+Tags.#LCov",
                            MessageId = "Cov",
                            Justification = "It is jargon")>]
[<assembly: SuppressMessage("Microsoft.Naming",
                            "CA1704:IdentifiersShouldBeSpelledCorrectly",
                            Scope = "member",
                            Target = "AltCover.XmlCoverageType+Tags.#Cobertura",
                            MessageId = "Cobertura",
                            Justification = "It is jargon")>]
()