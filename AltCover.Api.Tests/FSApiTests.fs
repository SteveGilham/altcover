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

module FSApiTests =

  [<Test>]
  let FormatFromCoverletMeetsSpec() =
    let probe = typeof<Tests.DU.MyClass>.Assembly.Location
    let resource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find
            (fun n -> n.EndsWith("OpenCoverForPester.coverlet.xml", StringComparison.Ordinal))
    use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
    let baseline = XDocument.Load(stream)
    let transformed = OpenCover.FormatFromCoverlet baseline [| probe |]
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
  let PostprocessShouldRestoreBranchOnlyOpenCoverState() =
    Counter.measureTime <- DateTime.ParseExact
                              ("2017-12-29T16:33:40.9564026+00:00", "o", null)
    let resource2 =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find
           (fun n -> n.EndsWith("Sample1WithOpenCover.xml", StringComparison.Ordinal))
    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)
    let after = XDocument.Load(stream)
    let setAttribute (x:XElement) n v =
      x.Attribute(XName.Get n).Value <- v
    let getAttribute (x:XElement) n =
      x.Attribute(XName.Get n).Value

    after.Descendants(XName.Get "Summary")
    |> Seq.iter (fun el ->
          setAttribute el "visitedSequencePoints" "0"
          setAttribute el "sequenceCoverage" "0"
          if getAttribute el "minCrapScore" = "2.11" then
            setAttribute el "minCrapScore" "2.15"
          if getAttribute el "maxCrapScore" = "2.11" then
            setAttribute el "maxCrapScore" "2.15")

    after.Descendants(XName.Get "Method")
    |> Seq.iter
          (fun el ->
          setAttribute el "sequenceCoverage" "0"
          if getAttribute el "crapScore" = "2.11" then
            setAttribute el "crapScore" "2.15")
    after.Descendants(XName.Get "SequencePoint")
    |> Seq.toList
    |> List.iter (fun el ->
          el.Remove())
    after.Descendants(XName.Get "MethodPoint")
    |> Seq.iter (fun el -> setAttribute el "vc" "0")
    let before =
      after.ToString().Replace("uspid=\"13", "uspid=\"100663298")
             .Replace("uspid=\"1\"", "uspid=\"100663297\"")
      |> XDocument.Parse

    after.Descendants(XName.Get "Summary")
    |> Seq.iter (fun el ->
          setAttribute el "visitedBranchPoints" "0"
          setAttribute el "branchCoverage" "0"
          setAttribute el "visitedSequencePoints" "0"
          setAttribute el "sequenceCoverage" "0"
          setAttribute el "visitedClasses" "0"
          setAttribute el "visitedMethods" "0"
          if getAttribute el "minCrapScore"
            |> String.IsNullOrWhiteSpace
            |> not
          then
            setAttribute el "minCrapScore" "0"
            setAttribute el "maxCrapScore" "0")
    after.Descendants(XName.Get "Method")
    |> Seq.iter (fun el ->
          setAttribute el "visited" "false"
          setAttribute el "sequenceCoverage" "0"
          setAttribute el "branchCoverage" "0"
          if getAttribute el "crapScore"
            |> String.IsNullOrWhiteSpace
            |> not
          then setAttribute el "crapScore" "0")
    OpenCover.PostProcess after BranchOrdinal.Offset
//#if !NETCOREAPP3_0
//    NUnit.Framework.Assert.That(after.ToString(),
//        NUnit.Framework.Is.EqualTo(before.ToString()))
//#endif

    test <@ after.ToString() = before.ToString() @>

  [<Test>]
  let OpenCoverToLcov() =
    use stream=
        Assembly.GetExecutingAssembly().GetManifestResourceStream("AltCover.Api.Tests.HandRolledMonoCoverage.xml")
    let doc = XDocument.Load(stream)
    use stream2 = new MemoryStream()
    CoverageFormats.ConvertToLcov doc stream2
    use stream2a = new MemoryStream(stream2.GetBuffer())
    use rdr = new StreamReader(stream2a)
    let result = rdr.ReadToEnd().Replace("\r", String.Empty)

    use stream3 =
        Assembly.GetExecutingAssembly().GetManifestResourceStream("AltCover.Api.Tests.HandRolledMonoCoverage.lcov")
    use rdr2 = new StreamReader(stream3)
    let expected = rdr2.ReadToEnd().Replace("\r", String.Empty)

    test <@ result = expected @>

  [<Test>]
  let OpenCoverToBarChart() =
    use stream=
        Assembly.GetExecutingAssembly().GetManifestResourceStream("AltCover.Api.Tests.HandRolledMonoCoverage.xml")
    let doc = XDocument.Load stream
    use mstream = new MemoryStream()
    let rewrite = Xhtml.ConvertToBarChart doc
    rewrite.Save mstream
    use mstream2 = new MemoryStream(mstream.GetBuffer(), 0, mstream.Position |> int)
    use rdr = new StreamReader(mstream2)
    let result = rdr.ReadToEnd().Replace("\r", String.Empty)

    use stream2 =
        Assembly.GetExecutingAssembly().GetManifestResourceStream("AltCover.Api.Tests.HandRolledMonoCoverage.html")
    use rdr2 = new StreamReader(stream2)
    let expected = rdr2.ReadToEnd().Replace("&#x2442;", "\u2442").Replace("\r", String.Empty)

    //NUnit.Framework.Assert.That(result, NUnit.Framework.Is.EqualTo expected)
    test <@ result = expected @>

  [<Test>]
  let OpenCoverToNCover() =
    use stream=
        Assembly.GetExecutingAssembly().GetManifestResourceStream("AltCover.Api.Tests.HandRolledMonoCoverage.xml")
    let doc = XDocument.Load stream
    use mstream = new MemoryStream()
    let rewrite = CoverageFormats.ConvertToNCover doc
    rewrite.Save mstream
    use mstream2 = new MemoryStream(mstream.GetBuffer(), 0, mstream.Position |> int)
    use rdr = new StreamReader(mstream2)
    let result = rdr.ReadToEnd().Replace("\r", String.Empty)

    use stream2 =
        Assembly.GetExecutingAssembly().GetManifestResourceStream("AltCover.Api.Tests.HandRolledToNCover.xml")
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
    let rewrite = CoverageFormats.ConvertFromNCover doc [ sample ]
    test <@ rewrite |> isNull |> not @>

  [<Test>]
  let FormatsConvertToXmlDocument() =
    use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream("AltCover.Api.Tests.HandRolledMonoCoverage.html")
    use rdr = new StreamReader(stream)
    let expected = rdr.ReadToEnd().Replace("html >", "html>").Replace("\r", String.Empty).Replace("&#x2442;", "\u2442")
    rdr.BaseStream.Position <- 0L
    let doc = XDocument.Load rdr
    let converted = XmlTypes.ToXmlDocument doc
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
        Assembly.GetExecutingAssembly().GetManifestResourceStream("AltCover.Api.Tests.HandRolledMonoCoverage.html")
    use rdr = new StreamReader(stream)
    let expected = rdr.ReadToEnd().Replace("\r", String.Empty).Replace("&#x2442;", "\u2442")
    rdr.BaseStream.Position <- 0L
    let doc = XmlDocument()
    doc.Load rdr
    let converted = XmlTypes.ToXDocument doc
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
    let converted = XmlTypes.ToXmlDocument doc
    test <@ converted.OuterXml.Replace(Environment.NewLine, String.Empty) =  documentText @>
    let reverted = XmlTypes.ToXDocument converted
    //NUnit.Framework.Assert.That(reverted.ToString(), NUnit.Framework.Is.EqualTo documentText)
    test <@ reverted.ToString().Replace(Environment.NewLine, String.Empty) =  documentText @>

  [<Test>]
  let NCoverToCobertura() =
    use stream=
        Assembly.GetExecutingAssembly().GetManifestResourceStream("AltCover.Api.Tests.Sample1WithNCover.xml")
    let doc = XDocument.Load(stream)
    doc.Descendants()
    |> Seq.map (fun n -> n.Attribute(XName.Get "excluded"))
    |> Seq.filter (fun a -> a |> isNull |> not)
    |> Seq.iter (fun a -> a.Value <- "false")

    let cob = CoverageFormats.ConvertToCobertura doc
    use stream2 = new MemoryStream()
    cob.Save stream2
    use stream2a = new MemoryStream(stream2.GetBuffer())
    use rdr = new StreamReader(stream2a)
    let result = rdr.ReadToEnd().Replace("\r", String.Empty)

    use stream3 =
        Assembly.GetExecutingAssembly().GetManifestResourceStream("AltCover.Api.Tests.Sample1WithNCover.cob.xml")
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
        Assembly.GetExecutingAssembly().GetManifestResourceStream("AltCover.Api.Tests.GenuineNCover158.Xml")
    let doc = XDocument.Load stream
    use mstream = new MemoryStream()
    let rewrite = Xhtml.ConvertToBarChart doc
    rewrite.Save mstream
    use mstream2 = new MemoryStream(mstream.GetBuffer(), 0, mstream.Position |> int)
    use rdr = new StreamReader(mstream2)
    let result = rdr.ReadToEnd().Replace("html >", "html>").Replace("\r", String.Empty)

    use stream2 =
        Assembly.GetExecutingAssembly().GetManifestResourceStream("AltCover.Api.Tests.GenuineNCover158Chart.html")
    use rdr2 = new StreamReader(stream2)
    let expected = rdr2.ReadToEnd().Replace("\r", String.Empty)

    NUnit.Framework.Assert.That(result, NUnit.Framework.Is.EqualTo expected)
    test <@ result = expected @>

  [<Test>]
  let OpenCoverBranchCompression() =
    use stream=
        Assembly.GetExecutingAssembly().GetManifestResourceStream("AltCover.Api.Tests.Compressible.xml")
    let doc = XDocument.Load stream

    [("CompressInterior", true, false)
     ("SameSpan",false, true)
     ("CompressBoth", true, true)]
    |> List.iter (fun (test, inSeq, sameSpan) ->
        use mstream = new MemoryStream()
        let rewrite = OpenCover.CompressBranching doc inSeq sameSpan
        rewrite.Save mstream
        use mstream2 = new MemoryStream(mstream.GetBuffer(), 0, mstream.Position |> int)
        use rdr = new StreamReader(mstream2)
        let result = rdr.ReadToEnd().Replace("\r", String.Empty)

        use stream2 =
            Assembly.GetExecutingAssembly().GetManifestResourceStream("AltCover.Api.Tests."+ test + ".xml")
        use rdr2 = new StreamReader(stream2)
        let expected = rdr2.ReadToEnd().Replace("\r", String.Empty)

        NUnit.Framework.Assert.That(result, NUnit.Framework.Is.EqualTo expected, test))
        //test <@ result = expected @>)

  [<Test>]
  let ArgumentsConsistent() =
    let force = DotNet.CLIOptions.Force true
    let fail = DotNet.CLIOptions.Fail true
    let summary = DotNet.CLIOptions.Summary "R"
    let combined =DotNet.CLIOptions.Many [ force; fail; summary ]

    let pprep = Primitive.PrepareOptions.Create()
    let prep = AltCover.PrepareOptions.Primitive pprep

    let pcoll = Primitive.CollectOptions.Create()
    let coll = AltCover.CollectOptions.Primitive pcoll

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

    let prepareFragments = [DotNet.I.toPrepareListArgumentList >> (List.map (fun (_,n,_) -> n))
                            DotNet.I.toPrepareFromArgArgumentList >> (List.map (fun (_,n,_) -> n))
                            DotNet.I.toPrepareArgArgumentList >> (List.map (fun (_,n,_,_) -> n))]
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

    let collectFragments = [//DotNet.I.toCollectListArgumentList >> (List.map (fun (_,n,_) -> n))
                            DotNet.I.toCollectFromArgArgumentList >> (List.map (fun (_,n,_) -> n))
                            //DotNet.I.toCollectArgArgumentList >> (List.map (fun (_,n,_,_) -> n))
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

    let opt = DotNet.CLIOptions.Fail true
    let optionsFragments = [//DotNet.I.toCollectListArgumentList >> (List.map (fun (_,n,_) -> n))
                            DotNet.I.toCLIOptionsFromArgArgumentList >> (List.map (fun (_,n,_) -> n))
                            DotNet.I.toCLIOptionsArgArgumentList >> (List.map (fun (_,n,_,_) -> n))
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
    let fail = DotNet.CLIOptions.Fail true
    let summary = DotNet.CLIOptions.Summary "R"
    let a = { new DotNet.ICLIOptions with
                        member self.ForceDelete = false
                        member self.FailFast = false
                        member self.ShowSummary = String.Empty } |> DotNet.CLIOptions.Abstract

    let combined = DotNet.CLIOptions.Many [ a; force; fail; summary ]

    test <@ fail.ForceDelete |> not @>
    test <@ force.FailFast |> not @>
    test <@ combined.ForceDelete @>
    test <@ combined.FailFast @>
    test <@ combined.ShowSummary = "R" @>
    test <@ (DotNet.CLIOptions.Many [ a; force; fail ]).ShowSummary |> String.IsNullOrEmpty @>

    let pprep = Primitive.PrepareOptions.Create()
    let prep = AltCover.AltCover.PrepareOptions.Primitive pprep

    let pcoll = Primitive.CollectOptions.Create()
    let coll = AltCover.AltCover.CollectOptions.Primitive pcoll

    test <@ DotNet.ToTestArguments prep coll combined =
      "/p:AltCover=\"true\" /p:AltCoverReportFormat=\"OpenCover\" /p:AltCoverShowStatic=\"-\" /p:AltCoverShowSummary=\"R\" /p:AltCoverForce=\"true\" /p:AltCoverFailFast=\"true\"" @>

#if SOURCEMAP
  let SolutionDir() =
    SolutionRoot.location

  let internal mangleFile (f:String) =
    f.Replace(@"C:\Users\steve\Documents\GitHub\altcover", SolutionRoot.location).Replace('\\', Path.DirectorySeparatorChar)

  [<Test>]
  let NCoverFindsFiles() =
    use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream("altcover.api.tests.core.GenuineNCover158.Xml")
    let doc = XDocument.Load stream

    [
      ("seqpnt", "document")
      ("module", "name")
    ]
    |> Seq.map (fun (k,v) -> (XName.Get k, XName.Get v))
    |> Seq.iter (fun (k,v) -> doc.Descendants k
                              |> Seq.iter (fun x -> let old = x.Attribute(v).Value
                                                    x.Attribute(v).Value <- mangleFile old))

    let rewrite = AltCover.RenderToHtml.Action doc
    test<@ rewrite |> Seq.map fst |> Seq.toList = ["Program.fs"] @>

  [<Test>]
  let OpenCoverFindsFiles() =
    use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream("altcover.api.tests.core.Compressible.xml")
    let doc = XDocument.Load stream

    [
      ("File", "fullPath")
    ]
    |> Seq.map (fun (k,v) -> (XName.Get k, XName.Get v))
    |> Seq.iter (fun (k,v) -> doc.Descendants k
                              |> Seq.iter (fun x -> let old = x.Attribute(v).Value
                                                    x.Attribute(v).Value <- mangleFile old))

    let rewrite = AltCover.RenderToHtml.Action doc
    test<@ rewrite |> Seq.map fst |> Seq.toList = ["Filter.fs"] @>
#endif