namespace AltCover

#if !NET472

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Reflection
open System.Xml
open System.Xml.Linq
open System.Xml.Schema
open System.Xml.XPath

[<assembly: SuppressMessage("Microsoft.Naming",
                            "CA1724:TypeNamesShouldNotMatchNamespaces",
                            Scope = "type",
                            Target = "AltCover.Configuration",
                            Justification = "No conflict here")>]
()

module Configuration =

  let private defaultDocument () =
    let doc = XDocument()
    doc.Add(XElement(XName.Get "AltCover.Visualizer"))
    doc

  [<SuppressMessage("Gendarme.Rules.Exceptions",
                    "DoNotSwallowErrorsCatchingNonSpecificExceptionsRule",
                    Justification = "need to exhaustively list the espected ones")>]
  [<SuppressMessage("Microsoft.Design",
                    "CA1031:DoNotCatchGeneralExceptionTypes",
                    Justification = "ditto, ditto")>]
  let private ensureFile () =
    let profileDir =
      Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData)

    let dir =
      Directory.CreateDirectory(Path.Combine(profileDir, ".altcover"))

    let file =
      Path.Combine(dir.FullName, "Visualizer.xml")

    if file |> File.Exists |> not then
      (file, defaultDocument ())
    else
      try
        let doc = XDocument.Load(file)

        try
          let schemas = XmlSchemaSet()

          use str =
            Assembly
              .GetExecutingAssembly()
              .GetManifestResourceStream("AltCover.UICommon.config.xsd")

          use xr = new StreamReader(str)
          use xsd = xr |> XmlReader.Create

          schemas.Add(String.Empty, xsd) |> ignore
          doc.Validate(schemas, null)
          (file, doc)
        with xx -> // DoNotSwallowErrorsCatchingNonSpecificExceptionsRule
          let nl = Environment.NewLine
          printfn "%A%s%s%A" xx nl nl doc
          (file, defaultDocument ())
      with x -> // DoNotSwallowErrorsCatchingNonSpecificExceptionsRule
        printfn "%A" x
        (file, defaultDocument ())

  [<SuppressMessage("Microsoft.Naming",
                    "CA1704:IdentifiersShouldBeSpelledCorrectly",
                    Justification = "Dir is a well known abbrev, and 's' is OK")>]
  let SaveSchemaDir (s: string) =
    let file, config = ensureFile ()

    let node =
      config.XPathSelectElements("AltCover.Visualizer")
      |> Seq.toList
      |> Seq.head

    if (match (node.Attribute(XName.Get "GSettingsSchemaDir"), String.IsNullOrWhiteSpace s) with
        | (null, false) ->
            node.Add(XAttribute(XName.Get "GSettingsSchemaDir", s))
            true
        | (a, false) ->
            a.Value <- s
            true
        | (null, true) -> false
        | (a, true) ->
            a.Remove()
            true) then
      config.Save file

  let SaveFont (font: string) =
    let file, config = ensureFile ()

    config.XPathSelectElements("//Font")
    |> Seq.toList
    |> Seq.iter (fun x -> x.Remove())

    let inject = XElement(XName.Get "Font", font)

    match config.XPathSelectElements("//CoveragePath")
          |> Seq.toList with
    | [] -> (config.FirstNode :?> XElement).AddFirst(inject)
    | x :: _ -> inject |> x.AddAfterSelf

    config.Save file

  let ReadFont () =
    let _, config = ensureFile ()

    match config.XPathSelectElements("//Font") |> Seq.toList with
    | [] -> "Monospace Normal 10"
    | x :: _ -> x.FirstNode.ToString()

  [<SuppressMessage("Microsoft.Naming",
                    "CA1704:IdentifiersShouldBeSpelledCorrectly",
                    Justification = "Compiler generated, also Dir is a well known abbrev")>]
  let ReadSchemaDir () =
    let _, config = ensureFile ()

    let node =
      config.XPathSelectElements("AltCover.Visualizer")
      |> Seq.toList
      |> Seq.head

    match node.Attribute(XName.Get "GSettingsSchemaDir") with
    | null -> String.Empty
    | a -> a.Value

  let SaveFolder (path: string) =
    let file, config = ensureFile ()

    match config.XPathSelectElements("//CoveragePath")
          |> Seq.toList with
    | [] ->
        (config.FirstNode :?> XElement)
          .AddFirst(XElement(XName.Get "CoveragePath", path))
    | x :: _ ->
        x.RemoveAll()
        x.Add path

    config.Save file

  let ReadFolder () =
    let _, config = ensureFile ()

    match config.XPathSelectElements("//CoveragePath")
          |> Seq.toList with
    | [] -> System.IO.Directory.GetCurrentDirectory()
    | x :: _ -> x.FirstNode.ToString()

  let SaveCoverageFiles (coverageFiles: string seq) =
    let file, config = ensureFile ()

    config.XPathSelectElements("//RecentlyOpened")
    |> Seq.toList
    |> Seq.iter (fun x -> x.Remove())

    let inject = config.FirstNode :?> XElement

    coverageFiles
    |> Seq.iter (fun path -> inject.Add(XElement(XName.Get "RecentlyOpened", path)))

    config.Save file

  [<SuppressMessage("Microsoft.Naming",
                    "CA1704:IdentifiersShouldBeSpelledCorrectly",
                    Justification = "Compiler generated")>]
  let ReadCoverageFiles sink =
    let _, config = ensureFile ()

    config.XPathSelectElements("//RecentlyOpened")
    |> Seq.map (fun n -> n.FirstNode.ToString())
    |> Seq.filter (fun n -> File.Exists(n))
    |> Seq.toList
    |> sink

  [<SuppressMessage("Microsoft.Naming",
                    "CA1704:IdentifiersShouldBeSpelledCorrectly",
                    Justification = "Compiler generated")>]
  let SaveGeometry location size =
    let file, config = ensureFile ()

    config.XPathSelectElements("//Geometry")
    |> Seq.toList
    |> Seq.iter (fun x -> x.Remove())

    let (x, y) = location ()
    let (width, height) = size ()

    let element =
      XElement(
        XName.Get "Geometry",
        XAttribute(XName.Get "x", x),
        XAttribute(XName.Get "y", y),
        XAttribute(XName.Get "width", width),
        XAttribute(XName.Get "height", height)
      )

    match config.XPathSelectElements("//RecentlyOpened")
          |> Seq.toList with
    | [] -> (config.FirstNode :?> XElement).Add element
    | x :: _ -> x.AddBeforeSelf element

    config.Save file

  let ReadGeometry position =
    let _, config = ensureFile ()

    let attribute (x: XElement) a =
      x.Attribute(XName.Get a).Value
      |> Double.TryParse
      |> snd

    config.XPathSelectElements("//Geometry")
    |> Seq.iter
         (fun e ->
           let width =
             Math.Max(attribute e "width" |> int, 600)

           let height =
             Math.Max(attribute e "height" |> int, 450)

           let x = attribute e "x" |> int
           let y = attribute e "y" |> int
           position (width, height) (x, y))

  let ClearGeometry () =
    let file, config = ensureFile ()

    config.XPathSelectElements("//Geometry")
    |> Seq.toList
    |> Seq.iter (fun f -> f.Remove())

    config.Save file

#endif
