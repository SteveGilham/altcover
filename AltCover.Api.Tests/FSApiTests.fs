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
open System.Diagnostics

module FSApiTests =

  [<Test>]
  let FormatFromCoverletMeetsSpec () =
    let probe =
      typeof<Tests.DU.MyClass>.Assembly.Location

    let resource =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceNames()
      |> Seq.find
           (fun n ->
             n.EndsWith("OpenCoverForPester.coverlet.xml", StringComparison.Ordinal))

    use stream =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream(resource)

    let baseline = XDocument.Load(stream)

    let transformed =
      OpenCover.FormatFromCoverlet baseline [| probe |]

    test <@ transformed |> isNull |> not @>

    let schemata = XmlSchemaSet()

    let sresource =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceNames()
      |> Seq.find (fun n -> n.EndsWith("OpenCoverStrict.xsd", StringComparison.Ordinal))

    use sstream =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream(sresource)

    use sreader = new StreamReader(sstream)
    use xsreader = System.Xml.XmlReader.Create(sreader)
    schemata.Add(String.Empty, xsreader) |> ignore

    transformed.Validate(schemata, null)

    test <@ transformed |> isNull |> not @>

  [<Test>]
  let PostprocessShouldRestoreBranchOnlyOpenCoverState () =
    Counter.measureTime <-
      DateTime.ParseExact("2017-12-29T16:33:40.9564026+00:00", "o", null)

    let resource2 =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceNames()
      |> Seq.find
           (fun n -> n.EndsWith("Sample1WithOpenCover.xml", StringComparison.Ordinal))

    use stream =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream(resource2)

    let after = XDocument.Load(stream)
    let setAttribute (x: XElement) n v = x.Attribute(XName.Get n).Value <- v
    let getAttribute (x: XElement) n = x.Attribute(XName.Get n).Value

    after.Descendants(XName.Get "Summary")
    |> Seq.iter
         (fun el ->
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
    |> List.iter (fun el -> el.Remove())

    after.Descendants(XName.Get "MethodPoint")
    |> Seq.iter (fun el -> setAttribute el "vc" "0")

    let before =
      after
        .ToString()
        .Replace("uspid=\"13", "uspid=\"100663298")
        .Replace("uspid=\"1\"", "uspid=\"100663297\"")
      |> XDocument.Parse

    after.Descendants(XName.Get "Summary")
    |> Seq.iter
         (fun el ->
           setAttribute el "visitedBranchPoints" "0"
           setAttribute el "branchCoverage" "0"
           setAttribute el "visitedSequencePoints" "0"
           setAttribute el "sequenceCoverage" "0"
           setAttribute el "visitedClasses" "0"
           setAttribute el "visitedMethods" "0"

           if getAttribute el "minCrapScore"
              |> String.IsNullOrWhiteSpace
              |> not then
             setAttribute el "minCrapScore" "0"
             setAttribute el "maxCrapScore" "0")

    after.Descendants(XName.Get "Method")
    |> Seq.iter
         (fun el ->
           setAttribute el "visited" "false"
           setAttribute el "sequenceCoverage" "0"
           setAttribute el "branchCoverage" "0"

           if getAttribute el "crapScore"
              |> String.IsNullOrWhiteSpace
              |> not then
             setAttribute el "crapScore" "0")

    OpenCover.PostProcess after BranchOrdinal.Offset
    //#if !NET472
//    NUnit.Framework.Assert.That(after.ToString(),
//        NUnit.Framework.Is.EqualTo(before.ToString()))
//#endif

    test <@ after.ToString() = before.ToString() @>

  [<Test>]
  let OpenCoverToLcov () =
    use stream =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.HandRolledMonoCoverage.xml")

    let doc = XDocument.Load(stream)
    use stream2 = new MemoryStream()
    CoverageFormats.ConvertToLcov doc stream2
    use stream2a = new MemoryStream(stream2.GetBuffer(), 0, int stream2.Length)
    use rdr = new StreamReader(stream2a)

    let result =
      rdr.ReadToEnd().Replace("\r", String.Empty)

    use stream3 =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.HandRolledMonoCoverage.lcov")

    use rdr2 = new StreamReader(stream3)

    let expected =
      rdr2.ReadToEnd().Replace("\r", String.Empty)
    //printfn "%s" result
    //Assert.That(result, Is.EqualTo expected)
    test <@ result = expected @>

  [<Test>]
  let OpenCoverWithPartialsToLcov () =
    use stream =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.OpenCoverWithPartials.xml")

    let doc = XDocument.Load(stream)
    use stream2 = new MemoryStream()
    CoverageFormats.ConvertToLcov doc stream2
    use stream2a = new MemoryStream(stream2.GetBuffer(), 0, int stream2.Length)
    use rdr = new StreamReader(stream2a)

    let result =
      rdr.ReadToEnd().Replace("\r", String.Empty)

    use stream3 =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.OpenCoverWithPartials.lcov")

    use rdr2 = new StreamReader(stream3)

    let expected =
      rdr2.ReadToEnd().Replace("\r", String.Empty)
    //printfn "%s" result
    Assert.That(result, Is.EqualTo expected)
    test <@ result = expected @>

  [<Test>]
  let JsonToOpenCover () =
    use stream =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.OpenCover.json")

    let doc =
      use reader = new StreamReader(stream)
      reader.ReadToEnd()

    let result = (OpenCover.JsonToXml doc).ToString()

    use stream3 =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.OpenCover.xml")

    use rdr2 = new StreamReader(stream3)
    let expected = rdr2.ReadToEnd()

    //printfn "%s" result
    //Assert.That(
    //  result
    //    .Replace('\r', '\u00FF')
    //    .Replace('\n', '\u00FF')
    //    .Replace("\u00FF\u00FF", "\u00FF")
    //    .Replace("8.12", "8.13")  // CRAP score rounding
    //    .Replace("4.12", "4.13")  // CRAP score rounding
    //    .Trim([| '\u00FF' |]),
    //  Is.EqualTo
    //  <| expected
    //    .Replace('\r', '\u00FF')
    //    .Replace('\n', '\u00FF')
    //    .Replace("\u00FF\u00FF", "\u00FF")
    //    .Trim([| '\u00FF' |])
    //)

    test
      <@ result
        .Replace('\r', '\u00FF')
        .Replace('\n', '\u00FF')
        .Replace("\u00FF\u00FF", "\u00FF")
        .Replace("8.12", "8.13")  // CRAP score rounding
        .Replace("4.12", "4.13")  // CRAP score rounding
        .Trim([| '\u00FF' |]) = expected
        .Replace('\r', '\u00FF')
        .Replace('\n', '\u00FF')
        .Replace("\u00FF\u00FF", "\u00FF")
        .Trim([| '\u00FF' |]) @>

  [<Test>]
  let JsonWithPartialsToOpenCover () =
    use stream =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.JsonWithPartials.json")

    let doc =
      use reader = new StreamReader(stream)
      reader.ReadToEnd()

    let result = (OpenCover.JsonToXml doc).ToString()

    use stream3 =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.JsonWithPartialsToXml.xml")

    use rdr2 = new StreamReader(stream3)
    let expected = rdr2.ReadToEnd()

    //printfn "%s" result

    //Assert.That(
    //  result
    //    .Replace('\r', '\u00FF')
    //    .Replace('\n', '\u00FF')
    //    .Replace("\u00FF\u00FF", "\u00FF")
    //    .Trim([| '\u00FF' |]),
    //  Is.EqualTo
    //  <| expected
    //    .Replace('\r', '\u00FF')
    //    .Replace('\n', '\u00FF')
    //    .Replace("\u00FF\u00FF", "\u00FF")
    //    .Trim([| '\u00FF' |])
    //)

    test
      <@ result
        .Replace('\r', '\u00FF')
        .Replace('\n', '\u00FF')
        .Replace("\u00FF\u00FF", "\u00FF")
        .Trim([| '\u00FF' |]) = expected
        .Replace('\r', '\u00FF')
        .Replace('\n', '\u00FF')
        .Replace("\u00FF\u00FF", "\u00FF")
        .Trim([| '\u00FF' |]) @>

  [<Test>]
  let JsonFromCoverletShouldHaveBranchExitValuesOK() =
    use stream =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.Sample4.coverlet.json")

    let doc =
      use reader = new StreamReader(stream)
      reader.ReadToEnd()

    let result = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\u00FF" +
                    (OpenCover.JsonToXml doc).ToString()

    use stream3 =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.Sample4.fromcoverletjson.xml")

    use rdr2 = new StreamReader(stream3)
    let expected = rdr2.ReadToEnd()

    //printfn "%s" result

    //Assert.That(
    //  result
    //    .Replace('\r', '\u00FF')
    //    .Replace('\n', '\u00FF')
    //    .Replace("\u00FF\u00FF", "\u00FF")
    //    .Trim([| '\u00FF' |]),
    //  Is.EqualTo
    //  <| expected
    //    .Replace('\r', '\u00FF')
    //    .Replace('\n', '\u00FF')
    //    .Replace("\u00FF\u00FF", "\u00FF")
    //    .Trim([| '\u00FF' |])
    //)

    test
      <@ result
        .Replace('\r', '\u00FF')
        .Replace('\n', '\u00FF')
        .Replace("\u00FF\u00FF", "\u00FF")
        .Trim([| '\u00FF' |]) = expected
        .Replace('\r', '\u00FF')
        .Replace('\n', '\u00FF')
        .Replace("\u00FF\u00FF", "\u00FF")
        .Trim([| '\u00FF' |]) @>

  [<Test>]
  let OpenCoverToJson () =
    use stream =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.Sample4FullTracking.xml")

    let doc = XDocument.Load(stream)
    let result = CoverageFormats.ConvertToJson doc

    use stream3 =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.OpenCover.json")

    use rdr2 = new StreamReader(stream3)
    let expected = rdr2.ReadToEnd()

    //printfn "%s" result
    //Assert.That(result
    //              .Replace('\r','\u00FF').Replace('\n','\u00FF')
    //              .Replace("\u00FF\u00FF","\u00FF").Trim([| '\u00FF' |]),
    //              Is.EqualTo <| expected.Replace('\r','\u00FF').Replace('\n','\u00FF')
    //                                    .Replace("\u00FF\u00FF","\u00FF").Trim([| '\u00FF' |]))

    test
      <@ result
        .Replace('\r', '\u00FF')
        .Replace('\n', '\u00FF')
        .Replace("\u00FF\u00FF", "\u00FF")
        .Trim([| '\u00FF' |]) = expected
        .Replace('\r', '\u00FF')
        .Replace('\n', '\u00FF')
        .Replace("\u00FF\u00FF", "\u00FF")
        .Trim([| '\u00FF' |]) @>

  [<Test>]
  let OpenCoverWithPartialsToJson () =
    use stream =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.OpenCoverWithPartials.xml")

    let doc = XDocument.Load(stream)

    let result = CoverageFormats.ConvertToJson doc

    use stream3 =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.JsonWithPartials.json")

    use rdr2 = new StreamReader(stream3)
    let expected = rdr2.ReadToEnd()

    //printfn "%s" result
    //Assert.That(result
    //              .Replace('\r','\u00FF').Replace('\n','\u00FF')
    //              .Replace("\u00FF\u00FF","\u00FF").Trim([| '\u00FF' |]),
    //              Is.EqualTo <| expected.Replace('\r','\u00FF').Replace('\n','\u00FF')
    //                                    .Replace("\u00FF\u00FF","\u00FF").Trim([| '\u00FF' |]),
    //              result)

    test
      <@ result
        .Replace('\r', '\u00FF')
        .Replace('\n', '\u00FF')
        .Replace("\u00FF\u00FF", "\u00FF")
        .Trim([| '\u00FF' |]) = expected
        .Replace('\r', '\u00FF')
        .Replace('\n', '\u00FF')
        .Replace("\u00FF\u00FF", "\u00FF")
        .Trim([| '\u00FF' |]) @>

  [<Test>]
  let NCoverToJson () =
    use stream =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.GenuineNCover158.Xml")

    let doc = XDocument.Load(stream)
    // fix up file path
    let exe =
      Path.Combine(SolutionRoot.location, "Samples/Sample19", "ConsoleApplication1.exe")

    doc.Root.Descendants(XName.Get "module")
    |> Seq.iter (fun e -> e.Attribute(XName.Get "name").Value <- exe)

    let result = CoverageFormats.ConvertToJson doc

    use stream3 =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.GenuineNCover158.json")

    use rdr2 = new StreamReader(stream3)
    let expected = rdr2.ReadToEnd()

    //printfn "%s" result
    //Assert.That(result
    //              .Replace('\r','\u00FF').Replace('\n','\u00FF')
    //              .Replace("\u00FF\u00FF","\u00FF").Trim([| '\u00FF' |]),
    //              Is.EqualTo <| expected.Replace('\r','\u00FF').Replace('\n','\u00FF')
    //                                    .Replace("\u00FF\u00FF","\u00FF").Trim([| '\u00FF' |]))

    test
      <@ result
        .Replace('\r', '\u00FF')
        .Replace('\n', '\u00FF')
        .Replace("\u00FF\u00FF", "\u00FF")
        .Trim([| '\u00FF' |]) = expected
        .Replace('\r', '\u00FF')
        .Replace('\n', '\u00FF')
        .Replace("\u00FF\u00FF", "\u00FF")
        .Trim([| '\u00FF' |]) @>

  [<Test>]
  let NCoverWithPartialsToJson () =
    use stream =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.NCoverWithPartials.xml")

    let doc = XDocument.Load(stream)
    // fix up file path
    let exe =
      Path.Combine(SolutionRoot.location, "AltCover.Tests/SimpleMix.exe")

    doc.Root.Descendants(XName.Get "module")
    |> Seq.iter (fun e -> e.Attribute(XName.Get "name").Value <- exe)

    let result = CoverageFormats.ConvertToJson doc

    use stream3 =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.JsonFromNCoverWithPartials.json")

    use rdr2 = new StreamReader(stream3)
    let expected = rdr2.ReadToEnd()

    //printfn "%s" result
    //Assert.That(result
    //              .Replace('\r','\u00FF').Replace('\n','\u00FF')
    //              .Replace("\u00FF\u00FF","\u00FF").Trim([| '\u00FF' |]),
    //              Is.EqualTo <| expected.Replace('\r','\u00FF').Replace('\n','\u00FF')
    //                                    .Replace("\u00FF\u00FF","\u00FF").Trim([| '\u00FF' |]))

    test
      <@ result
        .Replace('\r', '\u00FF')
        .Replace('\n', '\u00FF')
        .Replace("\u00FF\u00FF", "\u00FF")
        .Trim([| '\u00FF' |]) = expected
        .Replace('\r', '\u00FF')
        .Replace('\n', '\u00FF')
        .Replace("\u00FF\u00FF", "\u00FF")
        .Trim([| '\u00FF' |]) @>

  [<Test>]
  let NCoverToJsonWithEmbeds () =
    use stream =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.HandRolledToNCover.xml")

    let doc = XDocument.Load(stream)

    let result = CoverageFormats.ConvertToJson doc

    use stream3 =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.HandRolledToNCover.json")

    use rdr2 = new StreamReader(stream3)
    let expected = rdr2.ReadToEnd()

    //printfn "%s" result
    //Assert.That(result
    //              .Replace('\r','\u00FF').Replace('\n','\u00FF')
    //              .Replace("\u00FF\u00FF","\u00FF").Trim([| '\u00FF' |]),
    //              Is.EqualTo <| expected.Replace('\r','\u00FF').Replace('\n','\u00FF')
    //                                    .Replace("\u00FF\u00FF","\u00FF").Trim([| '\u00FF' |]))

    test
      <@ result
        .Replace('\r', '\u00FF')
        .Replace('\n', '\u00FF')
        .Replace("\u00FF\u00FF", "\u00FF")
        .Trim([| '\u00FF' |]) = expected
        .Replace('\r', '\u00FF')
        .Replace('\n', '\u00FF')
        .Replace("\u00FF\u00FF", "\u00FF")
        .Trim([| '\u00FF' |]) @>

    use stream =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.GenuineNCover158.Xml")

    let doc = XDocument.Load(stream)
    // fix up file path
    let exe =
      Path.Combine(SolutionRoot.location, "Samples/Sample19", "ConsoleApplication1.exe")

    doc.Root.Descendants(XName.Get "module")
    |> Seq.iter (fun e -> e.Attribute(XName.Get "name").Value <- exe)

    let result = CoverageFormats.ConvertToJson doc

    use stream3 =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.GenuineNCover158.json")

    use rdr2 = new StreamReader(stream3)
    let expected = rdr2.ReadToEnd()

    //printfn "%s" result
    //Assert.That(result
    //              .Replace('\r','\u00FF').Replace('\n','\u00FF')
    //              .Replace("\u00FF\u00FF","\u00FF").Trim([| '\u00FF' |]),
    //              Is.EqualTo <| expected.Replace('\r','\u00FF').Replace('\n','\u00FF')
    //                                    .Replace("\u00FF\u00FF","\u00FF").Trim([| '\u00FF' |]))

    test
      <@ result
        .Replace('\r', '\u00FF')
        .Replace('\n', '\u00FF')
        .Replace("\u00FF\u00FF", "\u00FF")
        .Trim([| '\u00FF' |]) = expected
        .Replace('\r', '\u00FF')
        .Replace('\n', '\u00FF')
        .Replace("\u00FF\u00FF", "\u00FF")
        .Trim([| '\u00FF' |]) @>

  [<Test>]
  let OpenCoverToBarChart () =
    use stream =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.HandRolledMonoCoverage.xml")

    let doc = XDocument.Load stream
    use mstream = new MemoryStream()
    let rewrite = Xhtml.ConvertToBarChart doc
    rewrite.Save mstream

    use mstream2 =
      new MemoryStream(mstream.GetBuffer(), 0, mstream.Position |> int)

    use rdr = new StreamReader(mstream2)

    let result =
      rdr.ReadToEnd().Replace("\r", String.Empty)
                     .Replace("ID0ES", "ID0ET") // flakiness in label autogenerator

    use stream2 =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.HandRolledMonoCoverage.html")

    use rdr2 = new StreamReader(stream2)

    let expected =
      rdr2
        .ReadToEnd()
        .Replace("&#x2442;", "\u2442")
        .Replace("\r", String.Empty)

    //NUnit.Framework.Assert.That(result, NUnit.Framework.Is.EqualTo expected)
    test <@ result = expected @>

  [<Test>]
  let OpenCoverToNCover () =
    use stream =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.HandRolledMonoCoverage.xml")

    let doc = XDocument.Load stream
    use mstream = new MemoryStream()
    let rewrite = CoverageFormats.ConvertToNCover doc
    rewrite.Save mstream

    use mstream2 =
      new MemoryStream(mstream.GetBuffer(), 0, mstream.Position |> int)

    use rdr = new StreamReader(mstream2)

    let result =
      rdr.ReadToEnd().Replace("\r", String.Empty)

    use stream2 =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.HandRolledToNCover.xml")

    use rdr2 = new StreamReader(stream2)

    let time =
      (rewrite.Descendants(XName.Get "coverage")
       |> Seq.head)
        .Attribute(
        XName.Get "startTime"
      )
        .Value

    let expected =
      rdr2
        .ReadToEnd()
        .Replace("{0}", time)
        .Replace("utf-16", "utf-8")
        .Replace("\r", String.Empty)

    NUnit.Framework.Assert.That(result, NUnit.Framework.Is.EqualTo expected)

  [<Test>]
  let OpenCoverWithPartialsToNCover () =
    use stream =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.OpenCoverWithPartials.xml")

    let doc = XDocument.Load stream
    // fix up file path
    let exe =
      Path.Combine(SolutionRoot.location, "AltCover.Tests/SimpleMix.exe")
    doc.Descendants("ModulePath".X)
    |> Seq.iter (fun x -> x.Value <- exe |> Canonical.canonicalPath)

    use mstream = new MemoryStream()
    let rewrite = CoverageFormats.ConvertToNCover doc
    rewrite.Save mstream

    use mstream2 =
      new MemoryStream(mstream.GetBuffer(), 0, mstream.Position |> int)

    use rdr = new StreamReader(mstream2)

    let result =
      rdr.ReadToEnd().Replace("\r", String.Empty)

    use stream2 =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.NCoverWithPartials.xml")

    use rdr2 = new StreamReader(stream2)

    let time =
      (rewrite.Descendants(XName.Get "coverage")
       |> Seq.head)
        .Attribute(
        XName.Get "startTime"
      )
        .Value

    let expected =
      rdr2
        .ReadToEnd()
        .Replace("{0}", time)
        .Replace("utf-16", "utf-8")
        .Replace("\r", String.Empty)

    NUnit.Framework.Assert.That(result, NUnit.Framework.Is.EqualTo expected)

  [<Test>]
  let OpenCoverFromNCover () =
    let sample = typeof<M.Thing>.Assembly.Location
    let reporter, doc = AltCover.Report.reportGenerator ()
    let visitors = [ reporter ]

    Visitor.visit
      visitors
      [ { AssemblyPath = sample
          Destinations = [] } ]

    let document =
      use stash = new MemoryStream()
      stash |> doc
      stash.Position <- 0L
      XDocument.Load stash

    let rewrite =
      CoverageFormats.ConvertFromNCover document [ sample ]

    test <@ rewrite |> isNull |> not @>

  [<Test>]
  let OpenCoverFromNCoverWithPartials () =
    use stream =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.NCoverWithPartials.xml")

    let exe =
      Path.Combine(SolutionRoot.location, "AltCover.Tests/SimpleMix.exe")

    let document =
      XDocument.Load stream
    let now = DateTime.UtcNow.ToLongDateString()

    let rewrite =
      CoverageFormats.ConvertFromNCover document [ exe ]
    rewrite.Descendants("ModuleTime".X)
    |> Seq.iter (fun x -> x.Value <- now)

    use mstream = new MemoryStream()
    rewrite.Save mstream

    use mstream2 =
      new MemoryStream(mstream.GetBuffer(), 0, mstream.Position |> int)

    use rdr = new StreamReader(mstream2)

    let result =
      rdr.ReadToEnd().Replace("\r", String.Empty)

    use stream2 =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.OpenCoverFromNCoverWithPartials.xml")

    use rdr2 = new StreamReader(stream2)
    let doc2 = XDocument.Load rdr2
    doc2.Descendants("ModulePath".X)
    |> Seq.iter (fun x -> x.Value <- exe |> Canonical.canonicalPath)
    doc2.Descendants("ModuleTime".X)
    |> Seq.iter (fun x -> x.Value <- now)

    // OpenCover.PostProcess doc2 BranchOrdinal.Offset
    use stream3 = new MemoryStream()
    doc2.Save(stream3)
    stream3.Position <- 0L
    use rdr3 = new StreamReader(stream3)

    let expected =
      rdr3
        .ReadToEnd()
        .Replace("\r", String.Empty)
    //printfn "%A" result
    NUnit.Framework.Assert.That(result, NUnit.Framework.Is.EqualTo expected)

  [<Test>]
  let FormatsConvertToXmlDocument () =
    use stream =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.HandRolledMonoCoverage.html")

    use rdr = new StreamReader(stream)

    let expected =
      rdr
        .ReadToEnd()
        .Replace("html >", "html>")
        .Replace("\r", String.Empty)
        .Replace("&#x2442;", "\u2442")

    rdr.BaseStream.Position <- 0L
    let doc = XDocument.Load rdr
    let converted = XmlTypes.ToXmlDocument doc
    use mstream = new MemoryStream()
    converted.Save mstream

    use mstream2 =
      new MemoryStream(mstream.GetBuffer(), 0, mstream.Position |> int)

    use rdr2 = new StreamReader(mstream2)

    let result =
      rdr2.ReadToEnd().Replace("\r", String.Empty)
    //NUnit.Framework.Assert.That(result, NUnit.Framework.Is.EqualTo expected)
    test <@ result = expected @>

  [<Test>]
  let FormatsConvertToXDocument () =
    use stream =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.HandRolledMonoCoverage.html")

    use rdr = new StreamReader(stream)

    let expected =
      rdr
        .ReadToEnd()
        .Replace("\r", String.Empty)
        .Replace("&#x2442;", "\u2442")

    rdr.BaseStream.Position <- 0L
    let doc = XmlDocument()
    doc.Load rdr
    let converted = XmlTypes.ToXDocument doc
    use mstream = new MemoryStream()
    converted.Save mstream

    use mstream2 =
      new MemoryStream(mstream.GetBuffer(), 0, mstream.Position |> int)

    use rdr2 = new StreamReader(mstream2)

    let result =
      rdr2.ReadToEnd().Replace("\r", String.Empty)
    //NUnit.Framework.Assert.That(result, NUnit.Framework.Is.EqualTo expected)
    test <@ result = expected @>

  [<Test>]
  let FormatsRoundTripSimply () =
    let documentText =
      """<?xml-stylesheet href="mystyle.xslt" type="text/xsl"?><document />"""

    use rdr = new StringReader(documentText)
    let doc = XDocument.Load rdr
    let converted = XmlTypes.ToXmlDocument doc

    test
      <@ converted.OuterXml.Replace(Environment.NewLine, String.Empty) = documentText @>

    let reverted = XmlTypes.ToXDocument converted
    //NUnit.Framework.Assert.That(reverted.ToString(), NUnit.Framework.Is.EqualTo documentText)
    test
      <@ reverted
        .ToString()
        .Replace(Environment.NewLine, String.Empty) = documentText @>

  [<Test>]
  let NCoverToCobertura () =
    use stream =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.Sample1WithNCover.xml")

    let doc = XDocument.Load(stream)

    doc.Descendants()
    |> Seq.map (fun n -> n.Attribute(XName.Get "excluded"))
    |> Seq.filter (isNull >> not)
    |> Seq.iter (fun a -> a.Value <- "false")

    let cob = CoverageFormats.ConvertToCobertura doc
    use stream2 = new MemoryStream()
    cob.Save stream2
    use stream2a = new MemoryStream(stream2.GetBuffer(), 0, int stream2.Length)
    use rdr = new StreamReader(stream2a)

    let result =
      rdr.ReadToEnd().Replace("\r", String.Empty)
    //printfn "FSApi.NCoverToCobertura\r\n%s" result

    use stream3 =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.Sample1WithNCover.cob.xml")

    use rdr2 = new StreamReader(stream3)

    let coverage =
      cob.Descendants(XName.Get "coverage") |> Seq.head

    let v =
      coverage.Attribute(XName.Get "version").Value

    let t =
      coverage.Attribute(XName.Get "timestamp").Value

    let expected =
      rdr2
        .ReadToEnd()
        .Replace("{0}", v)
        .Replace("{1}", t)
        .Replace("\r", String.Empty)

    //NUnit.Framework.Assert.That(result, NUnit.Framework.Is.EqualTo expected)
    test <@ result = expected @>

  [<Test>]
  let NCoverWithPartialsToCobertura () =
    use stream =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.NCoverWithPartials.xml")

    let doc = XDocument.Load(stream)

    doc.Descendants()
    |> Seq.map (fun n -> n.Attribute(XName.Get "excluded"))
    |> Seq.filter (isNull >> not)
    |> Seq.iter (fun a -> a.Value <- "false")

    let cob = CoverageFormats.ConvertToCobertura doc
    use stream2 = new MemoryStream()
    cob.Save stream2
    use stream2a = new MemoryStream(stream2.GetBuffer(), 0, int stream2.Length)
    use rdr = new StreamReader(stream2a)

    let result =
      rdr.ReadToEnd()
         .Replace("\r", String.Empty)
         .Replace("\\", "/")

    //printfn "FSApi.NCoverToCobertura\r\n%s" result

    use stream3 =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.NCoverWithPartials.cob.xml")

    use rdr2 = new StreamReader(stream3)

    let coverage =
      cob.Descendants(XName.Get "coverage") |> Seq.head

    let v =
      coverage.Attribute(XName.Get "version").Value

    let t =
      coverage.Attribute(XName.Get "timestamp").Value

    let expected =
      rdr2
        .ReadToEnd()
        .Replace("version=\"8.2.0.0\"", "version=\"" + v + "\"")
        .Replace("timestamp=\"xx\"", "timestamp=\"" + t + "\"")
        .Replace("\r", String.Empty)

    //printfn "%A" result
    //NUnit.Framework.Assert.That(result, NUnit.Framework.Is.EqualTo expected)
    test <@ result = expected @>

  [<Test>]
  let NCoverToBarChart () =
    use stream =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.GenuineNCover158.Xml")

    let doc = XDocument.Load stream
    use mstream = new MemoryStream()
    let rewrite = Xhtml.ConvertToBarChart doc
    rewrite.Save mstream

    use mstream2 =
      new MemoryStream(mstream.GetBuffer(), 0, mstream.Position |> int)

    use rdr = new StreamReader(mstream2)

    let result =
      rdr
        .ReadToEnd()
        .Replace("html >", "html>")
        .Replace("\r", String.Empty)

    use stream2 =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.GenuineNCover158Chart.html")

    use rdr2 = new StreamReader(stream2)

    let expected =
      rdr2.ReadToEnd().Replace("\r", String.Empty)

    NUnit.Framework.Assert.That(result, NUnit.Framework.Is.EqualTo expected)
    test <@ result = expected @>

  [<Test>]
  let OpenCoverBranchCompression () =
    use stream =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.Compressible.xml")

    let doc = XDocument.Load stream

    [ ("CompressInterior", true, false)
      ("SameSpan", false, true)
      ("CompressBoth", true, true) ]
    |> List.iter
         (fun (test, inSeq, sameSpan) ->
           use mstream = new MemoryStream()

           let rewrite =
             OpenCover.CompressBranching doc inSeq sameSpan

           rewrite.Save mstream

           use mstream2 =
             new MemoryStream(mstream.GetBuffer(), 0, mstream.Position |> int)

           use rdr = new StreamReader(mstream2)

           let result =
             rdr.ReadToEnd().Replace("\r", String.Empty)

           use stream2 =
             Assembly
               .GetExecutingAssembly()
               .GetManifestResourceStream("AltCover.Api.Tests." + test + ".xml")

           use rdr2 = new StreamReader(stream2)

           let expected =
             rdr2.ReadToEnd().Replace("\r", String.Empty)

           NUnit.Framework.Assert.That(result, NUnit.Framework.Is.EqualTo expected, test))
  //test <@ result = expected @>)

  [<Test>]
  let ArgumentsConsistent () =
    let force = DotNet.CLIOptions.Force true
    let fail = DotNet.CLIOptions.Fail true
    let summary = DotNet.CLIOptions.Summary "R"

    let combined =
      DotNet.CLIOptions.Many [ force
                               fail
                               summary ]

    let pprep = Primitive.PrepareOptions.Create()
    let prep = AltCover.PrepareOptions.Primitive pprep

    let pcoll = Primitive.CollectOptions.Create()
    let coll = AltCover.CollectOptions.Primitive pcoll

    let targets =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceNames()
      |> Seq.find (fun n -> n.EndsWith("AltCover.targets", StringComparison.Ordinal))

    use stream =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream(targets)

    let doc = XDocument.Load stream

    let prepare =
      doc.Descendants()
      |> Seq.filter (fun d -> d.Name.LocalName = "AltCover.Prepare")
      |> Seq.head

    let prepareNames =
      prepare.Attributes()
      |> Seq.map (fun p -> p.Name.LocalName.ToLowerInvariant())
      |> Seq.sort
      |> Seq.toList

    let prepareFragments =
      [ DotNet.I.toPrepareListArgumentList
        >> (List.map (fun (_, n, _) -> n))
        (fun p -> p.Verbosity)
        >> DotNet.I.toSharedFromValueArgumentList
        >> (List.map (fun (_, n, _, _) -> n))
        DotNet.I.toPrepareFromArgArgumentList
        >> (List.map (fun (_, n, _) -> n))
        DotNet.I.toPrepareArgArgumentList
        >> (List.map (fun (_, n, _, _) -> n)) ]
      |> List.collect (fun f -> f prep)
      |> List.sort

    // not input and output directories  (inplace now allowed)
//#if !NET472
//    NUnit.Framework.Assert.That(prepareFragments |> List.length, NUnit.Framework.Is.EqualTo ((prepareNames |> List.length) - 2),
//                "expected " + String.Join("; ", prepareNames) + Environment.NewLine +
//                "but got  " + String.Join("; ", prepareFragments))
//#endif
    test <@ (prepareFragments) |> List.length = ((prepareNames |> List.length) - 2) @>

    let collect =
      doc.Descendants()
      |> Seq.filter (fun d -> d.Name.LocalName = "AltCover.Collect")
      |> Seq.head

    let collectNames =
      collect.Attributes()
      |> Seq.map (fun p -> p.Name.LocalName.ToLowerInvariant())
      |> Seq.sort
      |> Seq.toList

    let collectFragments =
      [ //DotNet.I.toCollectListArgumentList >> (List.map (fun (_,n,_) -> n))
        DotNet.I.toCollectFromArgArgumentList
        >> (List.map (fun (_, n, _) -> n))
        (fun c -> c.Verbosity)
        >> DotNet.I.toSharedFromValueArgumentList
        >> (List.map (fun (_, n, _, _) -> n))
        //DotNet.I.toCollectArgArgumentList >> (List.map (fun (_,n,_,_) -> n))
        ]
      |> List.collect (fun f -> f coll)
      |> List.sort

    // not recorder directory
//#if !NET472
//    NUnit.Framework.Assert.That(collectFragments |> List.length, NUnit.Framework.Is.EqualTo ((collectNames |> List.length) - 1),
//                "expected " + String.Join("; ", collectNames) + Environment.NewLine +
//                "but got  " + String.Join("; ", collectFragments))
//#endif
    test <@ (collectFragments) |> List.length = ((collectNames |> List.length) - 1) @>

    let optionNames =
      typeof<DotNet.CLIOptions>.GetProperties ()
      |> Seq.map (fun p -> p.Name.ToLowerInvariant())
      |> Seq.sort
      |> Seq.toList

    let optionCases =
      (typeof<DotNet.CLIOptions>
       |> FSharpType.GetUnionCases)
        .Length

    let opt = DotNet.CLIOptions.Fail true

    let optionsFragments =
      [ //DotNet.I.toCollectListArgumentList >> (List.map (fun (_,n,_) -> n))
        DotNet.I.toCLIOptionsFromArgArgumentList
        >> (List.map (fun (_, n, _) -> n))
        DotNet.I.toCLIOptionsArgArgumentList
        >> (List.map (fun (_, n, _, _) -> n)) ]
      |> List.collect (fun f -> f opt)
      |> List.sort

    // ignore Is<CaseName> and Tag
//#if !NET472
//    NUnit.Framework.Assert.That(optionsFragments |> List.length, NUnit.Framework.Is.EqualTo ((optionNames |> List.length) - (1 + optionCases)),
//                "expected " + String.Join("; ", optionNames) + Environment.NewLine +
//                "but got  " + String.Join("; ", optionsFragments))
//#endif
    test
      <@ (optionsFragments) |> List.length = ((optionNames |> List.length)
                                              - (optionCases + 1)) @>

  [<Test>]
  let ArgumentsBuilt () =
    let force = DotNet.CLIOptions.Force true
    let fail = DotNet.CLIOptions.Fail true
    let summary = DotNet.CLIOptions.Summary "R"

    let a =
      { new DotNet.ICLIOptions with
          member self.ForceDelete = false
          member self.FailFast = false
          member self.ShowSummary = String.Empty }
      |> DotNet.CLIOptions.Abstract

    let combined =
      DotNet.CLIOptions.Many [ a
                               force
                               fail
                               summary ]

    test <@ fail.ForceDelete |> not @>
    test <@ force.FailFast |> not @>
    test <@ combined.ForceDelete @>
    test <@ combined.FailFast @>
    test <@ combined.ShowSummary = "R" @>

    test
      <@ (DotNet.CLIOptions.Many [ a
                                   force
                                   fail ])
           .ShowSummary
         |> String.IsNullOrEmpty @>

    let pprep = Primitive.PrepareOptions.Create()

    let prep =
      AltCover.AltCover.PrepareOptions.Primitive pprep

    let pcoll = Primitive.CollectOptions.Create()

    let coll =
      AltCover.AltCover.CollectOptions.Primitive pcoll

    test
      <@ DotNet.ToTestArguments prep coll combined = "/p:AltCover=\"true\" /p:AltCoverReportFormat=\"OpenCover\" /p:AltCoverShowStatic=\"-\" /p:AltCoverShowSummary=\"R\" /p:AltCoverForce=\"true\" /p:AltCoverFailFast=\"true\"" @>

    let coll1 =
      { pcoll with
          Verbosity = TraceLevel.Verbose }
      |> AltCover.AltCover.CollectOptions.Primitive

    test
      <@ DotNet.ToTestArguments prep coll1 combined = "/p:AltCover=\"true\" /p:AltCoverReportFormat=\"OpenCover\" /p:AltCoverShowStatic=\"-\" /p:AltCoverShowSummary=\"R\" /p:AltCoverForce=\"true\" /p:AltCoverFailFast=\"true\"" @>

    let coll2 =
      { pcoll with
          Verbosity = TraceLevel.Warning }
      |> AltCover.AltCover.CollectOptions.Primitive

    let prep2 =
      { pprep with
          Verbosity = TraceLevel.Error }
      |> AltCover.AltCover.PrepareOptions.Primitive

    test
      <@ DotNet.ToTestArguments prep2 coll2 combined = "/p:AltCover=\"true\" /p:AltCoverReportFormat=\"OpenCover\" /p:AltCoverShowStatic=\"-\" /p:AltCoverVerbosity=\"Error\" /p:AltCoverShowSummary=\"R\" /p:AltCoverForce=\"true\" /p:AltCoverFailFast=\"true\"" @>

  [<Test>]
  let MergeRejectsNonCoverage () =
    use stream1 =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.OpenCoverStrict.xsd")

    let doc1 = XDocument.Load stream1

    use stream2 =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.Sample1WithNCover.cob.xml")

    let doc2 = XDocument.Load stream2

    let merge = AltCover.OpenCover.Merge [ doc1; doc2 ]

    let lines =
      [ """<CoverageSession xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">"""
        """  <Summary numSequencePoints="?" visitedSequencePoints="0" numBranchPoints="?" visitedBranchPoints="0" sequenceCoverage="0" branchCoverage="0" maxCyclomaticComplexity="0" minCyclomaticComplexity="0" visitedClasses="0" numClasses="?" visitedMethods="0" numMethods="?" minCrapScore="0" maxCrapScore="0" />"""
        """  <Modules />"""
        """</CoverageSession>""" ]

    let expected = String.Join(Environment.NewLine, lines)
    test <@ merge.ToString() = expected.ToString() @>

  [<Test>]
  let MergePassesSingleOpenCover () =
    use stream1 =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.Compressible.xml")

    let doc1 = XDocument.Load stream1

    let merge = AltCover.OpenCover.Merge [ doc1 ]
    test <@ merge.ToString() = doc1.ToString() @>

  [<Test>]
  let MergeCombinesSummaryCoverage () =
    use stream1 =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.HandRolledMonoCoverage.xml")

    let doc1 = XDocument.Load stream1

    use stream2 =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.Sample4FullTracking.xml")

    let doc2 = XDocument.Load stream2

    let merge =
      AltCover.OpenCover.Merge [ doc1
                                 doc2
                                 doc1
                                 doc2 ]

    let summary = merge.Root.Element(XName.Get "Summary")

    //printfn "%A" merge

    test
      <@ summary.ToString() = "<Summary numSequencePoints=\"35\" visitedSequencePoints=\"21\" numBranchPoints=\"5\" visitedBranchPoints=\"5\" sequenceCoverage=\"60.00\" branchCoverage=\"100.00\" maxCyclomaticComplexity=\"7\" minCyclomaticComplexity=\"1\" visitedClasses=\"5\" numClasses=\"8\" visitedMethods=\"9\" numMethods=\"13\" minCrapScore=\"1.00\" maxCrapScore=\"14.11\" />" @>

  // TODO -- recursive validation

  [<Test>]
  let MergeCombinesRepeatCoverage () =
    use stream1 =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.Sample4.Prepare.xml")

    let doc1 = XDocument.Load stream1

    // relabelled data from a different build -- but it still merges togother plausibly
    use stream2 =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Api.Tests.Sample4FullTracking.xml")

    let doc2 = XDocument.Load stream2

    let merge = AltCover.OpenCover.Merge [ doc1; doc2; doc1; doc2 ]
    let summary = merge.Root.Element(XName.Get "Summary")

    //printfn "%A" merge

    test
      <@ summary
        .ToString()
        .Replace("minCrapScore=\"1.12\"", "minCrapScore=\"1.13\"") = "<Summary numSequencePoints=\"41\" visitedSequencePoints=\"11\" numBranchPoints=\"5\" visitedBranchPoints=\"4\" sequenceCoverage=\"26.83\" branchCoverage=\"80.00\" maxCyclomaticComplexity=\"11\" minCyclomaticComplexity=\"1\" visitedClasses=\"4\" numClasses=\"8\" visitedMethods=\"7\" numMethods=\"12\" minCrapScore=\"1.13\" maxCrapScore=\"87.20\" />" @>

// TODO -- recursive validation

#if SOURCEMAP
  let SolutionDir () = SolutionRoot.location

  let internal mangleFile (f: String) =
    f
      .Replace(@"C:\Users\steve\Documents\GitHub\altcover", SolutionRoot.location)
      .Replace('\\', Path.DirectorySeparatorChar)

  [<Test>]
  let NCoverFindsFiles () =
    use stream =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("altcover.api.tests.core.GenuineNCover158.Xml")

    let doc = XDocument.Load stream

    [ ("seqpnt", "document")
      ("module", "name") ]
    |> Seq.map (fun (k, v) -> (XName.Get k, XName.Get v))
    |> Seq.iter
         (fun (k, v) ->
           doc.Descendants k
           |> Seq.iter
                (fun x ->
                  let old = x.Attribute(v).Value
                  x.Attribute(v).Value <- mangleFile old))

    let rewrite = AltCover.RenderToHtml.Action doc
    test <@ rewrite |> Seq.map fst |> Seq.toList = [ "Program.fs" ] @>

  [<Test>]
  let OpenCoverFindsFiles () =
    use stream =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("altcover.api.tests.core.Compressible.xml")

    let doc = XDocument.Load stream

    [ ("File", "fullPath") ]
    |> Seq.map (fun (k, v) -> (XName.Get k, XName.Get v))
    |> Seq.iter
         (fun (k, v) ->
           doc.Descendants k
           |> Seq.iter
                (fun x ->
                  let old = x.Attribute(v).Value
                  x.Attribute(v).Value <- mangleFile old))

    let rewrite = AltCover.RenderToHtml.Action doc
    test <@ rewrite |> Seq.map fst |> Seq.toList = [ "Filter.fs" ] @>
#endif