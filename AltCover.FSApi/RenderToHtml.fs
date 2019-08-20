namespace AltCover

open System.Diagnostics.CodeAnalysis
open System.IO
open System.Xml
open System.Xml.XPath

module RenderToHtml =
  [<SuppressMessage("Microsoft.Design", "CA1059",
    Justification="Premature abstraction")>]
  let Action (xmlDocument:XmlDocument) =
    let format = XmlUtilities.DiscoverFormat xmlDocument

    let files = match format with
                | AltCover.Base.ReportFormat.OpenCover -> "//Files/File/@fullPath"
                | _ -> "//@document"
                |> xmlDocument.SelectNodes
                |> Seq.cast<XmlNode>
                |> Seq.map (fun n -> n.Value)
                |> Seq.distinct

    files // TODO repeated names in different folders
    |> Seq.map (fun path ->
            let doc = XmlDocument()
            doc.CreateComment(path) |> doc.AppendChild |> ignore
            (Path.GetFileName path, doc))