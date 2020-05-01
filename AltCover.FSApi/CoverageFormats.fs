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

/// <summary>
/// <para type="description">Conversions of NCover and OpenCover format data.</para>
/// </summary>
[<RequireQualifiedAccess>]
module CoverageFormats =
  /// <summary>
  /// <para type="description">Takes either OpenCover or classic NCover format input as an `XDocument`, as an argument or from the object pipeline. Writes the Lcov report to a file.</para>
  /// </summary>
  /// <param name="document">The report to convert.</param>
  /// <param name="stream">The output is written here.</param>
  [<SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
                    Justification="Lcov is a name")>]
  let ConvertToLcov (document:XDocument) stream =
    let format = XmlUtilities.discoverFormat document
    AltCover.LCov.convertReport document format stream

  /// <summary>
  /// <para type="synopsis">Creates a Cobertura format report from other report formats.</para>
  /// <para type="description">Takes either OpenCover or classic NCover format input as an `XDocument`, as an argument or from the object pipeline.</para>
  /// <para type="description">Writes the Cobertura report to the object pipeline as an `XDocument`, and optionally to a file.</para>
  /// </summary>
  /// <param name="document">The report to convert.</param>
  /// <returns>The converted document</returns>
  [<SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
                    Justification="Cobertura is a name")>]
  let ConvertToCobertura (document:XDocument) =
    let format = XmlUtilities.discoverFormat document
    AltCover.Cobertura.convertReport document format

  /// <summary>
  /// <para type="synopsis">Converts classic NCover format and returns OpenCover format.</para>
  /// <para type="description">The classic NCover format input either may be as an `XDocument` from the object pipeline or from a file.</para>
  /// <para type="description">Writes the OpenCover format report to the pipeline as an `XDocument`, and, optionally, to a file.  The report will contain data for the assemblies listed as the `-Assembly` argument and that are in the NCover input.</para>
  /// </summary>
  /// <param name="document">The report to convert.</param>
  /// <param name="assemblies">The assemblies contributing to the report.</param>
  /// <returns>The converted document</returns>
  [<SuppressMessage(
    "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
    Justification = "AvoidSpeculativeGenerality too")>]
  [<SuppressMessage("Microsoft.Design", "CA1011:ConsiderPassingBaseTypesAsParameters",
    Justification = "AvoidSpeculativeGenerality too")>]
  [<SuppressMessage("Microsoft.Maintainability", "CA1506:AvoidExcessiveClassCoupling",
     Justification="That's XML + code reuse for you.")>]
  let ConvertFromNCover (document:XDocument) (assemblies : string seq) =
    let reporter, rewrite = AltCover.OpenCover.reportGenerator()
    let visitors = [ reporter ]
    let identities = Dictionary<string, XElement>()
    document.Descendants(XName.Get "module")
    |> Seq.iter (fun n ->
         let key = n.Attribute(XName.Get "assemblyIdentity").Value
         identities.Add(key, n))

    let paths = Dictionary<string, string>()
    assemblies
    |> Seq.iter (fun p ->
         let a =
           XmlUtilities.assemblyNameWithFallback p (Path.GetFileNameWithoutExtension p)
         paths.Add(p, a))
    let usefulAssemblies =
      assemblies
      |> Seq.filter (fun p -> identities.ContainsKey paths.[p])
      |> Seq.map (fun p -> (p, []))

    // ensure default state
    AltCover.Main.init()
    // this switches branch recording off
    AltCover.CoverageParameters.theReportFormat <- Some AltCover.Base.ReportFormat.NCover
    AltCover.Visitor.visit visitors usefulAssemblies

    let parse s =
      Int32.TryParse
        (s, System.Globalization.NumberStyles.Integer,
         System.Globalization.CultureInfo.InvariantCulture) |> snd
    // Match modules
    rewrite.Descendants(XName.Get "Module")
    |> Seq.iter (fun target ->
         let path =
           target.Descendants(XName.Get "ModulePath")
           |> Seq.map (fun n -> n.Value)
           |> Seq.head

         let identity = paths.[path]
         let source = identities.[identity]
         let files = Dictionary<string, string>()
         target.Descendants(XName.Get "File").OfType<XElement>()
         |> Seq.iter
              (fun f ->
                files.Add
                  (f.Attribute(XName.Get "fullPath").Value,
                   f.Attribute(XName.Get "uid").Value))

         // Copy sequence points across
         source.Descendants(XName.Get "seqpnt")
         |> Seq.iter (fun s ->
              let sl = s.Attribute(XName.Get "line").Value
              let sc = s.Attribute(XName.Get "column").Value
              let el = s.Attribute(XName.Get "endline").Value
              let ec = s.Attribute(XName.Get "endcolumn").Value
              let uid = files.[s.Attribute(XName.Get "document").Value]
              let vc = parse <| s.Attribute(XName.Get "visitcount").Value
              let xpath =
                ".//SequencePoint[@sl='" + sl + "' and @sc='" + sc + "' and @el='" + el
                + "' and @ec='" + ec + "' and @fileid='" + uid + "']"
              let sp = Extensions.XPathSelectElement(target, xpath)
              let v = parse <| sp.Attribute(XName.Get "vc").Value
              let visits = (max 0 v) + (max 0 vc)
              sp.Attribute(XName.Get "vc").Value <- visits.ToString
                                                      (System.Globalization.CultureInfo.InvariantCulture)))

    rewrite.Descendants(XName.Get "Class")
    |> Seq.filter (fun c -> c.Descendants(XName.Get "Method") |> Seq.isEmpty)
    |> Seq.toList // reify before making changes
    |> Seq.iter (fun c -> c.Remove())

    let dec = rewrite.Declaration
    dec.Encoding <- "utf-8"
    dec.Standalone <- null

    OpenCoverUtilities.PostProcess rewrite Ordinal.Offset
    rewrite

  /// <summary>
  /// <para type="synopsis">Converts OpenCover format to NCover format.</para>
  /// <para type="description">Takes the OpenCover input either as an ``XDocument`` from the object pipeline or from a file.</para>
  /// <para type="description">Writes the classic NCover report to the pipeline as an ``XDocument``, and, optionally, to a file.</para>
  /// </summary>
  /// <param name="document">The report to convert.</param>
  /// <returns>The converted document</returns>
  [<SuppressMessage(
    "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
    Justification = "AvoidSpeculativeGenerality too")>]
  [<SuppressMessage("Microsoft.Design", "CA1011:ConsiderPassingBaseTypesAsParameters",
    Justification = "AvoidSpeculativeGenerality too")>]
  let ConvertToNCover(document:XDocument) =
    let transform = XmlUtilities.loadTransform "OpenCoverToNCover"
    let rewrite = XDocument()
    do use output = rewrite.CreateWriter()
       use source = document.CreateReader()
       transform.Transform(source, output)

    rewrite.Descendants(XName.Get "method")
    |> Seq.iter (fun m ->
         let c = m.Attribute(XName.Get "class").Value
         m.SetAttribute("class", c.Replace('/', '+'))
         let name = m.Attribute(XName.Get "name").Value
         let lead = name.Substring(name.LastIndexOf("::", StringComparison.Ordinal) + 2)
         m.SetAttribute("name", lead.Substring(0, lead.IndexOf('('))))

    rewrite.Descendants(XName.Get "module")
    |> Seq.iter (fun m ->
         let path = m.Attribute(XName.Get "name").Value
         let info = System.IO.FileInfo path
         m.SetAttribute("name", info.Name)
         let assembly = m.Attribute(XName.Get "assembly").Value
         m.SetAttribute
           ("assemblyIdentity", XmlUtilities.assemblyNameWithFallback path assembly))

    let thread = System.Threading.Thread.CurrentThread
    let culture = thread.CurrentCulture
    try
      thread.CurrentCulture <- CultureInfo.InvariantCulture
      rewrite.Descendants(XName.Get "coverage")
      |> Seq.iter (fun c ->
           let now =
             DateTime.UtcNow.ToLongDateString() + ":" + DateTime.UtcNow.ToLongTimeString()
           c.SetAttribute("startTime", now)
           c.SetAttribute("measureTime", now))
    finally
      thread.CurrentCulture <- culture
    rewrite