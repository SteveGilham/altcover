namespace AltCover

open System.Diagnostics.CodeAnalysis
open System.Linq
open System.Xml.Linq
open System.Xml.XPath

[<RequireQualifiedAccess>]
module Xhtml =
  /// <summary>
  /// <para type="synopsis">Generates a simple HTML report from coverage data.</para>
  /// <para type="description">The report produced is based on the old NCover 1.5.8 XSLT, for both NCover and OpenCover coverage format data.  The input is as a file name or an `XDocument` from the pipeline, the output is to the pipeline as an `XDocument`, and, optionally, to a file. </para>
  /// </summary>
  /// <param name="document">The input report</param>
  /// <returns>The HTML summary</returns>
  [<SuppressMessage(
    "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
    Justification = "AvoidSpeculativeGenerality too")>]
  [<SuppressMessage("Microsoft.Design", "CA1011:ConsiderPassingBaseTypesAsParameters",
    Justification = "AvoidSpeculativeGenerality too")>]
  let ConvertToBarChart(document : XDocument) =
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
        temp

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