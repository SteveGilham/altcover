namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Xml
open System.Xml.Linq
open System.Xml.XPath

module RenderToHtml =
  [<SuppressMessage("Microsoft.Design", "CA1059", Justification = "Premature abstraction")>]
  let Action (xmlDocument: XDocument) =
    let format = XmlUtilities.discoverFormat xmlDocument
    let nav = xmlDocument.CreateNavigator()

    let nodes =
      match format with
      | AltCover.ReportFormat.OpenCover -> "//Files/File/@fullPath"
      | _ -> "//@document"
      |> nav.Select
      |> Seq.cast<obj>
      |> Seq.toList

    let files =
      nodes
      |> Seq.map
           (fun n ->
             let pi = n.GetType().GetProperty("Value")
             pi.GetValue(n).ToString())
      |> Seq.distinct
      |> Seq.toList

    files // TODO handle repeated names in different folders
    |> Seq.filter File.Exists
    |> Seq.map
         (fun path ->
           let doc = XmlDocument()

           [ doc.CreateXmlDeclaration("1.0", null, null) :> XmlNode
             doc.CreateComment(path) :> XmlNode
             doc.CreateElement("html") :> XmlNode ]
           |> List.iter (doc.AppendChild >> ignore)

           let head = doc.CreateElement("head")
           head |> doc.DocumentElement.AppendChild |> ignore

           let meta = doc.CreateElement("meta")
           head.AppendChild meta |> ignore
           meta.SetAttribute("http-equiv", "content-type")
           meta.SetAttribute("content", "text/html; charset=utf-8")
           let title = doc.CreateElement("title")
           head.AppendChild title |> ignore
           title.InnerText <- path
           let style = doc.CreateElement("style")
           head.AppendChild style |> ignore

           style.InnerText <-
             [ "body { background: white; }"
               "td.code"
               "{"
               "  font-family: \"Fira Code\", Consolas, \"Lucida Console\", \"Courier New\", monospace;"
               "  color: #c0c0c0;"
               "}"
               "td.plain"
               "{"
               "  color: white; font-weight: bold;"
               "}" ]
             |> (fun l -> String.Join(Environment.NewLine, l))

           // "visited", "#404040", "#cefdce") // "#98FB98") ; // Dark on Pale Green
           //("declared", "#FFA500", "#FFFFFF") // Orange on White
           //("static", "#808080", "#F5F5F5") // Grey on White Smoke
           //("automatic", "#808080", "#FFFF00") // Grey on Yellow
           //("notVisited", "#ff0000", "#FFFFFF") // Red on White
           //("excluded", "#87CEEB", "#FFFFFF") // Sky Blue on white

           let body = doc.CreateElement("body")
           body |> doc.DocumentElement.AppendChild |> ignore

           let table = doc.CreateElement("table")
           body.AppendChild table |> ignore

           File.ReadAllLines path
           |> Seq.iter
                (fun l ->
                  let row = doc.CreateElement("tr")
                  table.AppendChild row |> ignore
                  let cell = doc.CreateElement("td")
                  cell.InnerText <- "\u00A0\u2442\u00A0"
                  cell.SetAttribute("class", "plain")
                  row.AppendChild cell |> ignore
                  let cell2 = doc.CreateElement("td")
                  row.AppendChild cell2 |> ignore
                  cell2.SetAttribute("class", "code")
                  cell2.InnerText <- l)

           (Path.GetFileName path, doc))
