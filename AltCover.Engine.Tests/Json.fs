namespace Tests

open System
open System.Collections.Generic
open System.IO
open System.IO.Compression
open System.Reflection
open System.Text.RegularExpressions
open System.Xml
open System.Xml.Linq
open System.Xml.Schema

open AltCover
open Microsoft.FSharp.Reflection
open Mono.Options

#nowarn "25" // partial pattern match
#nowarn "3559" // TODO

module Json =

  [<Test>]
  let NCoverShouldGeneratePlausibleJson () =
    Runner.init ()

    let resource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("GenuineNCover158.Xml", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

    let baseline = XDocument.Load(stream)
    // fix up file path
    let exe =
      Path.Combine(SolutionRoot.location, "Samples/Sample19", "ConsoleApplication1.exe")

    baseline.Root.Descendants(XName.Get "module")
    |> Seq.iter (fun e -> e.Attribute(XName.Get "name").Value <- exe)

    let unique =
      Path.Combine(
        Assembly.GetExecutingAssembly().Location
        |> Path.GetDirectoryName,
        Guid.NewGuid().ToString()
        + "/GenuineNCover158.json"
      )

    Json.path.Value <- Some unique

    unique
    |> Path.GetDirectoryName
    |> Directory.CreateDirectory
    |> ignore

    try
      Assert.That(unique |> File.Exists, Is.False)

      let result =
        Json.xmlToJson baseline ReportFormat.NCover

      let resource2 =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find _.EndsWith("GenuineNCover158.json", StringComparison.Ordinal)

      use stream2 =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)

      use reader = new StreamReader(stream2)
      let expected = reader.ReadToEnd()
      //printfn "%s" result
      let result1 =
        result.Replace("\r", String.Empty).Replace("\n", String.Empty)

      let expected1 =
        expected.Replace("\r", String.Empty).Replace("\n", String.Empty)

      testEqualValue result1 expected1
    finally
      Json.path.Value <- None

  [<Test>]
  let OpenCoverShouldGeneratePlausibleJson () =
    let dummy =
      SortedDictionary<string, NativeJson.Documents>()

    dummy.Add("\"\\\b\f\n\r\tA<>\u2012", SortedDictionary<string, NativeJson.Classes>())

    let escaped =
      NativeJson.toText(dummy).Replace("\r", String.Empty).Replace("\n", String.Empty)

    let expectedEscapes =
      """{ "\"\\\b\f\n\r\tA\u003C\u003E\u2012": {  }}"""

    Assert.That(escaped, Is.EqualTo expectedEscapes)
    test <@ escaped = expectedEscapes @>

    Runner.init ()

    let resource =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find _.EndsWith("Sample4FullTracking.xml", StringComparison.Ordinal)

    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)

    let baseline = XDocument.Load(stream)

    let unique =
      Path.Combine(
        Assembly.GetExecutingAssembly().Location
        |> Path.GetDirectoryName,
        Guid.NewGuid().ToString() + "/OpenCover.json"
      )

    Json.path.Value <- Some unique

    unique
    |> Path.GetDirectoryName
    |> Directory.CreateDirectory
    |> ignore

    try
      let result =
        Json.xmlToJson baseline ReportFormat.OpenCover

      let resource2 =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find _.EndsWith("OpenCover.json", StringComparison.Ordinal)

      use stream2 =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(resource2)

      use reader = new StreamReader(stream2)
      let expected = reader.ReadToEnd()
      //printfn "%s" result
      let result1 =
        result
          .Replace('\r', '\u00FF')
          .Replace('\n', '\u00FF')
          .Replace("\u00FF\u00FF", "\u00FF")
          .Trim([| '\u00FF' |])

      let expected1 =
        expected
          .Replace('\r', '\u00FF')
          .Replace('\n', '\u00FF')
          .Replace("\u00FF\u00FF", "\u00FF")
          .Trim([| '\u00FF' |])

      testEqualValue result1 expected1
    finally
      Json.path.Value <- None