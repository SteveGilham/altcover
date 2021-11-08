namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Linq
open System.Xml.Linq
open System.Xml.XPath

open AltCover.XmlExtensions

[<RequireQualifiedAccess>]
module CoverageFormats =
  [<SuppressMessage("Microsoft.Naming",
                    "CA1704:IdentifiersShouldBeSpelledCorrectly",
                    Justification = "Lcov is a name")>]
  let ConvertToLcov (document: XDocument) stream =
    let format = XmlUtilities.discoverFormat document
    AltCover.LCov.convertReport document format stream

  [<SuppressMessage("Microsoft.Naming",
                    "CA1704:IdentifiersShouldBeSpelledCorrectly",
                    Justification = "Cobertura is a name")>]
  let ConvertToCobertura (document: XDocument) =
    let format = XmlUtilities.discoverFormat document
    AltCover.Cobertura.convertReport document format

  let ConvertToJson (document: XDocument) =
    let format = XmlUtilities.discoverFormat document
    Json.xmlToJson document format

  [<SuppressMessage("Gendarme.Rules.Maintainability",
                    "AvoidUnnecessarySpecializationRule",
                    Justification = "AvoidSpeculativeGenerality too")>]
  [<SuppressMessage("Microsoft.Design",
                    "CA1011:ConsiderPassingBaseTypesAsParameters",
                    Justification = "AvoidSpeculativeGenerality too")>]
  let ConvertFromNCover (document: XDocument) (assemblies: string seq) =
    let reporter, rewritten = AltCover.OpenCover.reportGenerator ()
    let visitors = [ reporter ]
    let identities = Dictionary<string, XElement>()

    document.Descendants(XName.Get "module")
    |> Seq.iter
         (fun n ->
           let key =
             n.Attribute(XName.Get "assemblyIdentity").Value

           identities.Add(key, n))

    let paths = Dictionary<string, string>()

    assemblies
    |> Seq.iter
         (fun p ->
           let a =
             XmlUtilities.assemblyNameWithFallback p (Path.GetFileNameWithoutExtension p)

           paths.Add(p |> Path.GetFullPath, a))

    let usefulAssemblies =
      assemblies
      |> Seq.map Path.GetFullPath
      |> Seq.filter (fun p -> identities.ContainsKey paths.[p])
      |> Seq.map (fun p -> { AssemblyPath = p; Destinations = [] })

    // ensure default state
    AltCover.Main.init ()
    // this switches branch recording off
    AltCover.CoverageParameters.theReportFormat <- Some AltCover.ReportFormat.NCover
    AltCover.Visitor.visit visitors usefulAssemblies

    let parse s =
      Int32.TryParse(
        s,
        System.Globalization.NumberStyles.Integer,
        System.Globalization.CultureInfo.InvariantCulture
      )
      |> snd

    let rewrite =
      use stash = new MemoryStream()
      stash |> rewritten
      stash.Position <- 0L
      XDocument.Load stash

    // Match modules
    rewrite.Descendants(XName.Get "Module")
    |> Seq.iter
         (fun target ->
           let path =
             target.Descendants(XName.Get "ModulePath")
             |> Seq.map (fun n -> n.Value)
             |> Seq.head
             |> Path.GetFullPath

           let identity = paths.[path]
           let source = identities.[identity]
           let files = Dictionary<string, string>()

           target
             .Descendants(XName.Get "File")
             .OfType<XElement>()
           |> Seq.iter
                (fun f ->
                  files.Add(
                    f.Attribute(XName.Get "fullPath").Value.Replace('\\', '/'),
                    f.Attribute(XName.Get "uid").Value
                  ))

           // Copy sequence points across
           source.Descendants(XName.Get "seqpnt")
           |> Seq.iter
                (fun s ->
                  let sl = s.Attribute(XName.Get "line").Value
                  let sc = s.Attribute(XName.Get "column").Value
                  let el = s.Attribute(XName.Get "endline").Value
                  let ec = s.Attribute(XName.Get "endcolumn").Value
                  let key = s.Attribute(XName.Get "document").Value.Replace('\\', '/')
                  let uid = files.[key]
                  let vc =
                    parse <| s.Attribute(XName.Get "visitcount").Value

                  let xpath =
                    ".//SequencePoint[@sl='"
                    + sl
                    + "' and @sc='"
                    + sc
                    + "' and @el='"
                    + el
                    + "' and @ec='"
                    + ec
                    + "' and @fileid='"
                    + uid
                    + "']"

                  xpath
                  |> target.XPathSelectElement
                  |> Option.ofObj
                  |> Option.iter
                       (fun sp ->
                         let v =
                           parse <| sp.Attribute(XName.Get "vc").Value

                         let visits = (max 0 v) + (max 0 vc)

                         sp.Attribute(XName.Get "vc").Value <- visits.ToString(
                           System.Globalization.CultureInfo.InvariantCulture
                         ))))

    // This is done at generation time now
    // There is no filtering done here
    // rewrite.Descendants(XName.Get "Class")
    // |> Seq.filter (fun c -> c.Descendants(XName.Get "Method") |> Seq.isEmpty)
    // |> Seq.toList // reify before making changes
    // |> Seq.iter (fun c -> c.Remove())

    let dec = rewrite.Declaration
    dec.Encoding <- "utf-8"
    dec.Standalone <- null

    OpenCover.PostProcess rewrite BranchOrdinal.Offset
    rewrite

  [<SuppressMessage("Gendarme.Rules.Maintainability",
                    "AvoidUnnecessarySpecializationRule",
                    Justification = "AvoidSpeculativeGenerality too")>]
  [<SuppressMessage("Microsoft.Design",
                    "CA1011:ConsiderPassingBaseTypesAsParameters",
                    Justification = "AvoidSpeculativeGenerality too")>]
  let ConvertToNCover (document: XDocument) =
    let transform =
      XmlUtilities.loadTransform "OpenCoverToNCover"

    let rewrite = XDocument()

    do
      use output = rewrite.CreateWriter()
      use source = document.CreateReader()
      transform.Transform(source, output)

    rewrite.Descendants(XName.Get "method")
    |> Seq.iter
         (fun m ->
           let c = m.Attribute(XName.Get "class").Value
           m.SetAttribute("class", c.Replace('/', '+'))
           let name = m.Attribute(XName.Get "name").Value

           let lead =
             name.Substring(
               name.LastIndexOf("::", StringComparison.Ordinal)
               + 2
             )

           m.SetAttribute("name", lead.Substring(0, charIndexOf lead '(')))

    rewrite.Descendants(XName.Get "module")
    |> Seq.iter
         (fun m ->
           let path = m.Attribute(XName.Get "name").Value
           let info = System.IO.FileInfo path
           m.SetAttribute("name", info.Name)
           let assembly = m.Attribute(XName.Get "assembly").Value

           m.SetAttribute(
             "assemblyIdentity",
             XmlUtilities.assemblyNameWithFallback path assembly
           ))

    let thread = System.Threading.Thread.CurrentThread
    let culture = thread.CurrentCulture

    try
      thread.CurrentCulture <- CultureInfo.InvariantCulture

      rewrite.Descendants(XName.Get "coverage")
      |> Seq.iter
           (fun c ->
             let now =
               DateTime.UtcNow.ToLongDateString()
               + ":"
               + DateTime.UtcNow.ToLongTimeString()

             c.SetAttribute("startTime", now)
             c.SetAttribute("measureTime", now))
    finally
      thread.CurrentCulture <- culture

    rewrite