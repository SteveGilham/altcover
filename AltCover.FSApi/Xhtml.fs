namespace AltCover

open System.Diagnostics.CodeAnalysis
open System.Linq
open System.Xml
open System.Xml.Linq
open System.Xml.XPath

[<RequireQualifiedAccess>]
module Xhtml =
  let ConvertToBarChart(document : XContainer) =
    let format =
      if document.Descendants(XName.Get "CoverageSession").Any()
      then AltCover.Base.ReportFormat.OpenCover
      else AltCover.Base.ReportFormat.NCover

    let intermediate =
      if format = AltCover.Base.ReportFormat.NCover then
        document
      else
        let modify = XmlUtilities.loadTransform "OpenCoverToNCoverEx"
        let temp = XDocument()
        do use feed = temp.CreateWriter()
           use from = document.CreateReader()
           modify.Transform(from, feed)
        temp :> XContainer

    let transform = XmlUtilities.loadTransform "NCoverToBarChart"
    let rewrite = XDocument()
    do use output = rewrite.CreateWriter()
       use source = intermediate.CreateReader()
       transform.Transform(source, output)

    rewrite.XPathSelectElements("//script[@language='JavaScript']")
    |> Seq.iter (fun n ->
         let text = n.Value
         n.Value <- "//"
         n.Add(XCData(text)))

    rewrite.AddFirst(XDocumentType("html", null, null, null))
    rewrite