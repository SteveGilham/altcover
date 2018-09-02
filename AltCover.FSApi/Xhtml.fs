namespace AltCover

open System.Diagnostics.CodeAnalysis
open System.Linq
open System.Xml
open System.Xml.Linq
open System.Xml.XPath

module Xhtml =

  [<SuppressMessage("Microsoft.Design", "CA1059", Justification="returns a specific concrete type")>]
  let ConvertToBarChart (navigable:IXPathNavigable) =
      let navigator = navigable.CreateNavigator()
      let format = if navigator.Select("/CoverageSession").OfType<XPathNavigator>().Any() then
                    AltCover.Base.ReportFormat.OpenCover
                   else AltCover.Base.ReportFormat.NCover

      let intermediate = if format = AltCover.Base.ReportFormat.NCover then navigable
                         else
                          let modify = XmlUtilities.LoadTransform "OpenCoverToNCoverEx"
                          let temp = XmlDocument()
                          do
                            use feed = temp.CreateNavigator().AppendChild()
                            modify.Transform (navigable, feed)
                          temp :> IXPathNavigable

      let transform = XmlUtilities.LoadTransform "NCoverToBarChart"
      let rewrite = XmlDocument()
      do
        use output = rewrite.CreateNavigator().AppendChild()
        transform.Transform (intermediate, output)

      rewrite.DocumentElement.SelectNodes("//script[@language='JavaScript']").OfType<XmlNode>()
      |> Seq.iter(fun n -> let text = n.InnerText
                           let cdata = rewrite.CreateCDataSection(text)
                           n.InnerText <- "//"
                           n.AppendChild cdata |> ignore)

      let doctype = rewrite.CreateDocumentType("html", null, null, null)
      rewrite.PrependChild(doctype) |> ignore
      XmlUtilities.PrependDeclaration rewrite
      rewrite