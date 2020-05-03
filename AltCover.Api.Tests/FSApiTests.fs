namespace Tests
// fsharplint:disable  MemberNames NonPublicValuesNames RedundantNewKeyword

open System
open System.IO
open System.Reflection
open System.Xml
open System.Xml.Linq
open System.Xml.Schema

open AltCover
open Microsoft.FSharp.Reflection
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
    let result = rdr.ReadToEnd().Replace("\r", String.Empty)

    use stream3 =
        Assembly.GetExecutingAssembly().GetManifestResourceStream("altcover.api.tests.core.HandRolledMonoCoverage.lcov")
    use rdr2 = new StreamReader(stream3)
    let expected = rdr2.ReadToEnd().Replace("\r", String.Empty)

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
    let result = rdr.ReadToEnd().Replace("\r", String.Empty)

    use stream2 =
        Assembly.GetExecutingAssembly().GetManifestResourceStream("altcover.api.tests.core.HandRolledMonoCoverage.html")
    use rdr2 = new StreamReader(stream2)
    let expected = rdr2.ReadToEnd().Replace("&#x2442;", "\u2442").Replace("\r", String.Empty)

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
    let result = rdr.ReadToEnd().Replace("\r", String.Empty)

    use stream3 =
        Assembly.GetExecutingAssembly().GetManifestResourceStream("altcover.api.tests.core.Sample1WithNCover.cob.xml")
    use rdr2 = new StreamReader(stream3)

    let coverage = cob.Descendants(XName.Get "coverage") |> Seq.head
    let v = coverage.Attribute(XName.Get "version").Value
    let t = coverage.Attribute(XName.Get "timestamp").Value

    let expected = rdr2.ReadToEnd().Replace("{0}", v).Replace("{1}", t).Replace("\r", String.Empty)

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
  let ArgumentsConsistent() =
    let force = DotNet.CLIOptions.Force true
    let fail = DotNet.CLIOptions.FailFast true
    let summary = DotNet.CLIOptions.ShowSummary "R"
    let combined =DotNet.CLIOptions.Many [ force; fail; summary ]

    let pprep = Primitive.PrepareOptions.Create()
    let prep = AltCover.FSApi.PrepareOptions.Primitive pprep

    let pcoll = Primitive.CollectOptions.Create()
    let coll = AltCover.FSApi.CollectOptions.Primitive pcoll

    let targets =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find
            (fun n -> n.EndsWith("AltCover.targets", StringComparison.Ordinal))
    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(targets)
    let doc = XDocument.Load stream
    let prepare = doc.Descendants()
                  |> Seq.filter (fun d -> d.Name.LocalName = "AltCover.Prepare")
                  |> Seq.head
    let prepareNames = prepare.Attributes()
                       |> Seq.map (fun p -> p.Name.LocalName.ToLowerInvariant())
                       |> Seq.sort
                       |> Seq.toList

    let prepareFragments = [DotNet.toPrepareListArgumentList >> (List.map (fun (_,n,_) -> n))
                            DotNet.toPrepareFromArgArgumentList >> (List.map (fun (_,n,_) -> n))
                            DotNet.toPrepareArgArgumentList >> (List.map (fun (_,n,_,_) -> n))]
                            |> List.collect (fun f -> f prep)
                            |> List.sort

    // not input and output directories
//#if !NETCOREAPP3_0
//    NUnit.Framework.Assert.That(prepareFragments |> List.length, NUnit.Framework.Is.EqualTo ((prepareNames |> List.length) - 2),
//                "expected " + String.Join("; ", prepareNames) + Environment.NewLine +
//                "but got  " + String.Join("; ", prepareFragments))
//#endif
    test <@ (prepareFragments) |> List.length = ((prepareNames |> List.length) - 2) @>

    let collect = doc.Descendants()
                  |> Seq.filter (fun d -> d.Name.LocalName = "AltCover.Collect")
                  |> Seq.head
    let collectNames = collect.Attributes()
                       |> Seq.map (fun p -> p.Name.LocalName.ToLowerInvariant())
                       |> Seq.sort
                       |> Seq.toList

    let collectFragments = [//DotNet.toCollectListArgumentList >> (List.map (fun (_,n,_) -> n))
                            DotNet.toCollectFromArgArgumentList >> (List.map (fun (_,n,_) -> n))
                            //DotNet.toCollectArgArgumentList >> (List.map (fun (_,n,_,_) -> n))
                           ]
                            |> List.collect (fun f -> f coll)
                            |> List.sort

    // not recorder directory
//#if !NETCOREAPP3_0
//    NUnit.Framework.Assert.That(collectFragments |> List.length, NUnit.Framework.Is.EqualTo ((collectNames |> List.length) - 1),
//                "expected " + String.Join("; ", collectNames) + Environment.NewLine +
//                "but got  " + String.Join("; ", collectFragments))
//#endif
    test <@ (collectFragments) |> List.length = ((collectNames |> List.length) - 1) @>

    let optionNames = typeof<DotNet.CLIOptions>.GetProperties()
                       |> Seq.map (fun p -> p.Name.ToLowerInvariant())
                       |> Seq.sort
                       |> Seq.toList
    let optionCases = (typeof<DotNet.CLIOptions>
                      |> FSharpType.GetUnionCases).Length

    let opt = DotNet.CLIOptions.FailFast true
    let optionsFragments = [//DotNet.toCollectListArgumentList >> (List.map (fun (_,n,_) -> n))
                            DotNet.toCLIOptionsFromArgArgumentList >> (List.map (fun (_,n,_) -> n))
                            DotNet.toCLIOptionsArgArgumentList >> (List.map (fun (_,n,_,_) -> n))
                           ]
                            |> List.collect (fun f -> f opt)
                            |> List.sort

    // ignore Is<CaseName> and Tag
//#if !NETCOREAPP3_0
//    NUnit.Framework.Assert.That(optionsFragments |> List.length, NUnit.Framework.Is.EqualTo ((optionNames |> List.length) - (1 + optionCases)),
//                "expected " + String.Join("; ", optionNames) + Environment.NewLine +
//                "but got  " + String.Join("; ", optionsFragments))
//#endif
    test <@ (optionsFragments) |> List.length = ((optionNames |> List.length) - (optionCases + 1)) @>

  [<Test>]
  let ArgumentsBuilt() =
    let force = DotNet.CLIOptions.Force true
    let fail = DotNet.CLIOptions.FailFast true
    let summary = DotNet.CLIOptions.ShowSummary "R"
    let combined =DotNet.CLIOptions.Many [ force; fail; summary ]
    test <@ fail.ForceDelete |> not @>
    test <@ force.Fast |> not @>
    test <@ combined.ForceDelete @>
    test <@ combined.Fast @>
    test <@ combined.Summary = "R" @>
    test <@ (DotNet.CLIOptions.Many [ force; fail ]).Summary |> String.IsNullOrEmpty @>

    let pprep = Primitive.PrepareOptions.Create()
    let prep = AltCover.FSApi.PrepareOptions.Primitive pprep

    let pcoll = Primitive.CollectOptions.Create()
    let coll = AltCover.FSApi.CollectOptions.Primitive pcoll

    test <@ DotNet.ToTestArguments prep coll combined =
      "/p:AltCover=\"true\" /p:AltCoverReportFormat=\"OpenCover\" /p:AltCoverShowStatic=\"-\" /p:AltCoverShowSummary=\"R\" /p:AltCoverForce=\"true\" /p:AltCoverFailFast=\"true\"" @>