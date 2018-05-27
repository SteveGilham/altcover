namespace AltCover.PowerShell

open System.Diagnostics.CodeAnalysis
open System.Linq
open System.Xml
open System.Xml.Linq

module XmlUtilities =

  [<SuppressMessage("Microsoft.Design", "CA1059", Justification="converts concrete types")>]
  let ToXmlDocument(xDocument:XDocument) =
    let xmlDocument = XmlDocument()
    use xmlReader = xDocument.CreateReader()
    xmlDocument.Load(xmlReader)

    let xDeclaration = xDocument.Declaration
    if xDeclaration |> isNull |> not then
      let xmlDeclaration = xmlDocument.CreateXmlDeclaration(
                                        xDeclaration.Version,
                                        xDeclaration.Encoding,
                                        xDeclaration.Standalone)
      xmlDocument.InsertBefore(xmlDeclaration, xmlDocument.FirstChild) |> ignore

    xmlDocument

  [<SuppressMessage("Microsoft.Design", "CA1059", Justification="converts concrete types")>]
  let ToXDocument(xmlDocument:XmlDocument) =
    use nodeReader = new XmlNodeReader(xmlDocument)
    nodeReader.MoveToContent() |> ignore // skips leading comments
    let xdoc = XDocument.Load(nodeReader)
    let decl' = xmlDocument.ChildNodes.OfType<XmlDeclaration>()
                |> Seq.tryHead
    match decl' with
    | None -> ()
    | Some decl -> xdoc.Declaration <- XDeclaration(decl.Version,
                                                    decl.Encoding,
                                                    decl.Standalone)
    xdoc