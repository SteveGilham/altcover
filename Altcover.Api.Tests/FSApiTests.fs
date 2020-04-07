namespace Tests
// fsharplint:disable  MemberNames NonPublicValuesNames RedundantNewKeyword

open System
open System.IO
open System.Reflection
open System.Xml
open System.Xml.Linq
open System.Xml.Schema

open AltCover
open Swensen.Unquote

#if NETCOREAPP3_0
[<AttributeUsage(AttributeTargets.Method)>]
type TestAttribute() = class
    inherit Attribute()
end
#else
type TestAttribute = NUnit.Framework.TestAttribute
#endif

module FSApiTests =
  let SolutionDir() =
    SolutionRoot.location

  [<Test>]
  let FormatFromCoverletMeetsSpec() =
    let probe = typeof<Tests.DU.MyClass>.Assembly.Location
    let resource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find
            (fun n -> n.EndsWith("OpenCoverForPester.coverlet.xml", StringComparison.Ordinal))
    use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
    let baseline = XDocument.Load(stream)
    let transformed = AltCover.OpenCoverUtilities.FormatFromCoverlet baseline [| probe |]
    test <@ transformed |> isNull |> not @>

    let schemata = XmlSchemaSet()
    let sresource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find
            (fun n -> n.EndsWith("OpenCoverStrict.xsd", StringComparison.Ordinal))
    use sstream = Assembly.GetExecutingAssembly().GetManifestResourceStream(sresource)
    use sreader = new StreamReader(sstream)
    use xsreader = System.Xml.XmlReader.Create(sreader)
    schemata.Add(String.Empty, xsreader) |> ignore

    transformed.Validate(schemata, null)

    test <@ transformed |> isNull |> not @>

  [<Test>]
  let OpenCoverToLcov() =
    use stream=
        Assembly.GetExecutingAssembly().GetManifestResourceStream("altcover.api.tests.core.HandRolledMonoCoverage.xml")
    let doc = XDocument.Load(stream)
    use stream2 = new MemoryStream()
    AltCover.CoverageFormats.ConvertToLcov doc stream2
    use stream2a = new MemoryStream(stream2.GetBuffer())
    use rdr = new StreamReader(stream2a)
    let result = rdr.ReadToEnd()

    use stream3 =
        Assembly.GetExecutingAssembly().GetManifestResourceStream("altcover.api.tests.core.HandRolledMonoCoverage.lcov")
    use rdr2 = new StreamReader(stream3)
    let expected = rdr2.ReadToEnd()

    test <@ result = expected @>

  [<Test>]
  let OpenCoverToBarChart() =
    use stream=
        Assembly.GetExecutingAssembly().GetManifestResourceStream("altcover.api.tests.core.HandRolledMonoCoverage.xml")
    let doc = XDocument.Load stream
    use mstream = new MemoryStream()
    let rewrite = AltCover.Xhtml.ConvertToBarChart doc
    rewrite.Save mstream
    use mstream2 = new MemoryStream(mstream.GetBuffer(), 0, mstream.Position |> int)
    use rdr = new StreamReader(mstream2)
    let result = rdr.ReadToEnd()

    use stream2 =
        Assembly.GetExecutingAssembly().GetManifestResourceStream("altcover.api.tests.core.HandRolledMonoCoverage.html")
    use rdr2 = new StreamReader(stream2)
    let expected = rdr2.ReadToEnd().Replace("&#x2442;", "\u2442")

    //NUnit.Framework.Assert.That(result, NUnit.Framework.Is.EqualTo expected)
    test <@ result = expected @>

  [<Test>]
  let OpenCoverToNCover() =
    use stream=
        Assembly.GetExecutingAssembly().GetManifestResourceStream("altcover.api.tests.core.HandRolledMonoCoverage.xml")
    let doc = XDocument.Load stream
    use mstream = new MemoryStream()
    let rewrite = AltCover.CoverageFormats.ConvertToNCover doc
    rewrite.Save mstream
    use mstream2 = new MemoryStream(mstream.GetBuffer(), 0, mstream.Position |> int)
    use rdr = new StreamReader(mstream2)
    let result = rdr.ReadToEnd().Replace("\r", String.Empty)

    use stream2 =
        Assembly.GetExecutingAssembly().GetManifestResourceStream("altcover.api.tests.core.HandRolledToNCover.xml")
    use rdr2 = new StreamReader(stream2)
    let time = (rewrite.Descendants(XName.Get "coverage")
                |> Seq.head).Attribute(XName.Get "startTime").Value
    let expected = rdr2.ReadToEnd().Replace("{0}", time).Replace("utf-16", "utf-8").Replace("\r", String.Empty)

    NUnit.Framework.Assert.That(result, NUnit.Framework.Is.EqualTo expected)

  [<Test>]
  let OpenCoverFromNCover() =
    let sample = typeof<M.Thing>.Assembly.Location
    let reporter, doc = AltCover.Report.reportGenerator()
    let visitors = [ reporter ]
    Visitor.visit visitors [(sample, [])]
    use mstream = new MemoryStream()
    let rewrite = AltCover.CoverageFormats.ConvertFromNCover doc [ sample ]
    test <@ rewrite |> isNull |> not @>

  [<Test>]
  let FormatsConvertToXmlDocument() =
    use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream("altcover.api.tests.core.HandRolledMonoCoverage.html")
    use rdr = new StreamReader(stream)
    let expected = rdr.ReadToEnd().Replace("html >", "html>").Replace("\r", String.Empty).Replace("&#x2442;", "\u2442")
    rdr.BaseStream.Position <- 0L
    let doc = XDocument.Load rdr
    let converted = AltCover.XmlUtilities.ToXmlDocument doc
    use mstream = new MemoryStream()
    converted.Save mstream
    use mstream2 = new MemoryStream(mstream.GetBuffer(), 0, mstream.Position |> int)
    use rdr2 = new StreamReader(mstream2)
    let result = rdr2.ReadToEnd().Replace("\r", String.Empty)
    //NUnit.Framework.Assert.That(result, NUnit.Framework.Is.EqualTo expected)
    test <@ result = expected @>

  [<Test>]
  let FormatsConvertToXDocument() =
    use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream("altcover.api.tests.core.HandRolledMonoCoverage.html")
    use rdr = new StreamReader(stream)
    let expected = rdr.ReadToEnd().Replace("\r", String.Empty).Replace("&#x2442;", "\u2442")
    rdr.BaseStream.Position <- 0L
    let doc = XmlDocument()
    doc.Load rdr
    let converted = AltCover.XmlUtilities.ToXDocument doc
    use mstream = new MemoryStream()
    converted.Save mstream
    use mstream2 = new MemoryStream(mstream.GetBuffer(), 0, mstream.Position |> int)
    use rdr2 = new StreamReader(mstream2)
    let result = rdr2.ReadToEnd().Replace("\r", String.Empty)
    //NUnit.Framework.Assert.That(result, NUnit.Framework.Is.EqualTo expected)
    test <@ result = expected @>

  [<Test>]
  let FormatsRoundTripSimply() =
    let documentText = """<?xml-stylesheet href="mystyle.xslt" type="text/xsl"?><document />"""
    use rdr = new StringReader(documentText)
    let doc = XDocument.Load rdr
    let converted = AltCover.XmlUtilities.ToXmlDocument doc
    test <@ converted.OuterXml.Replace(Environment.NewLine, String.Empty) =  documentText @>
    let reverted = AltCover.XmlUtilities.ToXDocument converted
    //NUnit.Framework.Assert.That(reverted.ToString(), NUnit.Framework.Is.EqualTo documentText)
    test <@ reverted.ToString().Replace(Environment.NewLine, String.Empty) =  documentText @>

  [<Test>]
  let NCoverToCobertura() =
    use stream=
        Assembly.GetExecutingAssembly().GetManifestResourceStream("altcover.api.tests.core.Sample1WithNCover.xml")
    let doc = XDocument.Load(stream)
    doc.Descendants()
    |> Seq.map (fun n -> n.Attribute(XName.Get "excluded"))
    |> Seq.filter (fun a -> a |> isNull |> not)
    |> Seq.iter (fun a -> a.Value <- "false")

    let cob = AltCover.CoverageFormats.ConvertToCobertura doc
    use stream2 = new MemoryStream()
    cob.Save stream2
    use stream2a = new MemoryStream(stream2.GetBuffer())
    use rdr = new StreamReader(stream2a)
    let result = rdr.ReadToEnd()

    use stream3 =
        Assembly.GetExecutingAssembly().GetManifestResourceStream("altcover.api.tests.core.Sample1WithNCover.cob.xml")
    use rdr2 = new StreamReader(stream3)

    let coverage = cob.Descendants(XName.Get "coverage") |> Seq.head
    let v = coverage.Attribute(XName.Get "version").Value
    let t = coverage.Attribute(XName.Get "timestamp").Value

    let expected = rdr2.ReadToEnd().Replace("{0}", v).Replace("{1}", t)

    //NUnit.Framework.Assert.That(result, NUnit.Framework.Is.EqualTo expected)
    test <@ result = expected @>

  [<Test>]
  let NCoverToBarChart() =
    use stream=
        Assembly.GetExecutingAssembly().GetManifestResourceStream("altcover.api.tests.core.GenuineNCover158.Xml")
    let doc = XDocument.Load stream
    use mstream = new MemoryStream()
    let rewrite = AltCover.Xhtml.ConvertToBarChart doc
    rewrite.Save mstream
    use mstream2 = new MemoryStream(mstream.GetBuffer(), 0, mstream.Position |> int)
    use rdr = new StreamReader(mstream2)
    let result = rdr.ReadToEnd().Replace("html >", "html>").Replace("\r", String.Empty)

    use stream2 =
        Assembly.GetExecutingAssembly().GetManifestResourceStream("altcover.api.tests.core.GenuineNCover158Chart.html")
    use rdr2 = new StreamReader(stream2)
    let expected = rdr2.ReadToEnd().Replace("\r", String.Empty)

    //NUnit.Framework.Assert.That(result, NUnit.Framework.Is.EqualTo expected)
    test <@ result = expected @>

  [<Test>]
  let OpenCoverBranchCompression() =
    use stream=
        Assembly.GetExecutingAssembly().GetManifestResourceStream("altcover.api.tests.core.Compressible.xml")
    let doc = XDocument.Load stream

    [("CompressInterior", true, false)
     ("SameSpan",false, true)
     ("CompressBoth", true, true)]
    |> List.iter (fun (test, inSeq, sameSpan) ->
        use mstream = new MemoryStream()
        let rewrite = AltCover.OpenCoverUtilities.CompressBranching doc inSeq sameSpan
        rewrite.Save mstream
        use mstream2 = new MemoryStream(mstream.GetBuffer(), 0, mstream.Position |> int)
        use rdr = new StreamReader(mstream2)
        let result = rdr.ReadToEnd().Replace("\r", String.Empty)

        use stream2 =
            Assembly.GetExecutingAssembly().GetManifestResourceStream("altcover.api.tests.core."+ test + ".xml")
        use rdr2 = new StreamReader(stream2)
        let expected = rdr2.ReadToEnd().Replace("\r", String.Empty)

        NUnit.Framework.Assert.That(result, NUnit.Framework.Is.EqualTo expected, test))
        //test <@ result = expected @>)

  [<Test>]
  let ArgumentsBuilt() =
    let force = DotNet.CLIArgs.Force true
    let fail = DotNet.CLIArgs.FailFast true
    let summary = DotNet.CLIArgs.ShowSummary "R"
    let combined =DotNet.CLIArgs.Many [ force; fail; summary ]
    test <@ fail.ForceDelete |> not @>
    test <@ force.Fast |> not @>
    test <@ combined.ForceDelete @>
    test <@ combined.Fast @>
    test <@ combined.Summary = "R" @>
    test <@ (DotNet.CLIArgs.Many [ force; fail ]).Summary |> String.IsNullOrEmpty @>

    let pprep =Primitive.PrepareParameters.Create()
    let prep = AltCover.FSApi.PrepareParameters.Primitive pprep

    let pcoll = Primitive.CollectParameters.Create()
    let coll = AltCover.FSApi.CollectParameters.Primitive pcoll

    test <@ DotNet.ToTestArguments prep coll combined = "/p:AltCover=\"true\" /p:AltCoverForce=\"true\" /p:AltCoverFailFast=\"true\" /p:AltCoverShowSummary=\"R\" /p:AltCoverShowStatic=\"-\"" @>