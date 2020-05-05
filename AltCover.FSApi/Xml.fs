namespace AltCover.FSApi

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Linq
open System.Reflection
open System.Xml
open System.Xml.Linq
open System.Xml.Schema
open System.Xml.Xsl

open AltCover

module XmlTypes =
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