namespace AltCover

open System
open System.IO
open System.Xml.Linq
open Augment
open System.Globalization

// based on the sample file at https://raw.githubusercontent.com/jenkinsci/cobertura-plugin/master/src/test/resources/hudson/plugins/cobertura/coverage-with-data.xml
[<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1704",
    Justification="'Cobertura' is jargon")>]
module internal Cobertura =
  let internal path : Option<string> ref = ref None

  module internal I =

    let internal setRate hits total (rate : string) (target : XElement) =
      if total > 0 then
        let ratio = (float hits) / (float total)
        target.SetAttributeValue(rate.X, String.Format(CultureInfo.InvariantCulture,
                                                       "{0:0.##}", ratio))

    [<System.Diagnostics.CodeAnalysis.SuppressMessage(
      "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
      Justification = "AvoidSpeculativeGenerality too")>]
    let internal addSources (report : XDocument) (target : XElement) (tag : string)
                            (attribute : string) =
      report.Descendants(tag.X)
      |> Seq.map (fun s -> s.Attribute(attribute.X).Value |> Path.GetDirectoryName)
      |> Seq.fold (fun s f -> s |> Set.add f) Set.empty<String>
      |> Seq.sort
      |> Seq.iter (fun f ->
           target.Descendants("sources".X)
           |> Seq.iter (fun s -> s.Add(XElement("source".X, XText(f)))))

    let internal nCover (report : XDocument) (packages : XElement) =
      let processSeqPnts (method : XElement) (lines : XElement) =
        method.Descendants("seqpnt".X)
        |> Seq.filter (fun s -> s.Attribute("excluded".X).Value <> "true")
        |> Seq.fold (fun (h, t) s ->
             let vc = s.Attribute("visitcount".X)

             let vx =
               if isNull vc then "0" else vc.Value

             let line =
               XElement
                 ("line".X, XAttribute("number".X, s.Attribute("line".X).Value),
                  XAttribute("hits".X, vx), XAttribute("branch".X, "false"))
             lines.Add line
             (h + (if vx = "0" then 0 else 1), t + 1)) (0, 0)

      let processMethod (methods : XElement) (hits, total) (key, (signature, method)) =
        let mtx =
          XElement
            ("method".X, XAttribute("name".X, key), XAttribute("signature".X, signature))
        methods.Add(mtx)
        let lines = XElement("lines".X)
        mtx.Add(lines)
        let (mHits, mTotal) = processSeqPnts method lines
        setRate mHits mTotal "line-rate" mtx
        setRate 1 1 "branch-rate" mtx
        setRate 1 1 "complexity" mtx
        (hits + mHits, total + mTotal)

      let sortMethod (n : String) (methods : XElement) (method : XElement seq) =
        method
        |> Seq.map (fun m ->
             let key, signature =
               let fna = m.Attribute("fullname".X)
               if fna |> isNull then
                 (m.Attribute("class".X).Value + "." + m.Attribute("name".X).Value,
                  String.Empty)
               else
                 let fn = fna.Value.Split([| ' '; '(' |]) |> Array.toList
                 (fn.[1].Substring(n.Length + 1), fn.[0] + " " + fn.[2])
             (key, (signature, m)))
        |> LCov.sortByFirst
        |> Seq.fold (processMethod methods) (0, 0)

      let processClass (classes : XElement) (hits, total) ((name, signature), method) =
        let ``class`` =
          XElement
            ("class".X, XAttribute("name".X, name), XAttribute("filename".X, signature))
        classes.Add(``class``)
        let methods = XElement("methods".X)
        ``class``.Add(methods)
        let (mHits, mTotal) = sortMethod name methods method
        setRate mHits mTotal "line-rate" ``class``
        setRate 1 1 "branch-rate" ``class``
        setRate 1 1 "complexity" ``class``
        (hits + mHits, total + mTotal)

      let extractClasses (``module`` : XElement) classes =
        ``module``.Descendants("method".X)
        |> Seq.filter (fun m -> m.Attribute("excluded".X).Value <> "true")
        |> Seq.groupBy (fun method ->
             (method.Attribute("class".X).Value,
              method.Descendants("seqpnt".X)
              |> Seq.map (fun s -> s.Attribute("document".X).Value)
              |> Seq.head))
        |> LCov.sortByFirst
        |> Seq.fold (processClass classes) (0, 0)

      let processModule (hits, total) (``module`` : XElement) =
        let package =
          XElement("package".X, XAttribute("name".X, ``module``.Attribute("name".X).Value))
        packages.Add(package)
        let classes = XElement("classes".X)
        package.Add(classes)
        let (cHits, cTotal) = extractClasses ``module`` classes
        setRate cHits cTotal "line-rate" package
        setRate 1 1 "branch-rate" package
        setRate 1 1 "complexity" package
        (hits + cHits, total + cTotal)

      let (hits, total) = report.Descendants("module".X) |> Seq.fold processModule (0, 0)
      let p = packages.Parent
      setRate hits total "line-rate" p
      setRate 1 1 "branch-rate" p
      p.Attribute("lines-valid".X).Value <- total.ToString(CultureInfo.InvariantCulture)
      p.Attribute("lines-covered".X).Value <- hits.ToString(CultureInfo.InvariantCulture)

      addSources report packages.Parent "seqpnt" "document"

    let internal openCover (report : XDocument) (packages : XElement) =
      let extract (owner : XElement) (target : XElement) =
        let summary = owner.Descendants("Summary".X) |> Seq.head

        let valueOf (name : string) =
          summary.Attribute(name.X).Value
          |> Int32.TryParse
          |> snd

        let b = valueOf "numBranchPoints"
        let bv = valueOf "visitedBranchPoints"
        let s = valueOf "numSequencePoints"
        let sv = valueOf "visitedSequencePoints"

        setRate sv s "line-rate" target
        setRate bv b "branch-rate" target
        if target.Name.LocalName.Equals("coverage", StringComparison.Ordinal) then
          let copyup (name : string) (value : int) =
            target.Attribute(name.X).Value <- value.ToString(CultureInfo.InvariantCulture)

          copyup "lines-valid" s
          copyup "lines-covered" sv
          copyup "branches-valid" b
          copyup "branches-covered" bv
          target.Attribute("complexity".X).Value <- (summary.Attribute
                                                       ("maxCyclomaticComplexity".X)).Value

      let doBranch bec bev uspid (line : XElement) =
        let pc = Math.Round(100.0 * (float bev) / (float bec)) |> int
        line.SetAttributeValue
          ("condition-coverage".X, sprintf "%d%% (%d/%d)" pc bev bec)
        let cc = XElement("conditions".X)
        line.Add cc
        let co =
          XElement
            ("condition".X, XAttribute("number".X, uspid), XAttribute("type".X, "jump"),
             XAttribute("coverage".X, sprintf "%d%%" pc))
        cc.Add co

      let copySeqPnt (lines : XElement) (s : XElement) =
        let vc = s.Attribute("vc".X)

        let vx =
          (vc
           |> Option.nullable
           |> Option.getOrElse (XAttribute("dummy".X, "0"))).Value

        let bec =
          s.Attribute("bec".X).Value
          |> Int32.TryParse
          |> snd

        let bev =
          s.Attribute("bev".X).Value
          |> Int32.TryParse
          |> snd

        let line =
          XElement
            ("line".X, XAttribute("number".X, s.Attribute("sl".X).Value),
             XAttribute("hits".X, vx),
             XAttribute
               ("branch".X,
                (if bec = 0 then "false" else "true")))

        if bec > 0 then
          let uspid = s.Attribute("uspid".X).Value // KISS approach
          doBranch bec bev uspid line
        lines.Add line

      let addMethod (methods : XElement) (key, signature) =
        let mtx =
          XElement
            ("method".X, XAttribute("name".X, key), XAttribute("signature".X, signature))
        methods.Add(mtx)
        let lines = XElement("lines".X)
        mtx.Add(lines)
        (mtx, lines)

      let provideAttributeValue (element : XElement) (name : string) v =
        v + (element.Attribute(name.X).Value
             |> Int32.TryParse
             |> snd)

      let processMethod (methods : XElement) (b, bv, s, sv, c, cv)
          (key, (signature, method)) =
        let ccplex = 0 |> provideAttributeValue method "cyclomaticComplexity"
        let mtx, lines = addMethod (methods : XElement) (key, signature)
        extract method mtx
        mtx.Add(XAttribute("complexity".X, ccplex))
        method.Descendants("SequencePoint".X) |> Seq.iter (copySeqPnt lines)
        let summary = method.Descendants("Summary".X) |> Seq.head
        (b |> provideAttributeValue summary "numBranchPoints",
         bv |> provideAttributeValue summary "visitedBranchPoints",
         s |> provideAttributeValue summary "numSequencePoints",
         sv |> provideAttributeValue summary "visitedSequencePoints", c + 1, cv + ccplex)

      let arrangeMethods (name : String) (methods : XElement) (methodSet : XElement seq) =
        methodSet
        |> Seq.map (fun method ->
             let fn =
               (method.Descendants("Name".X) |> Seq.head).Value.Split([| ' '; '(' |])
               |> Array.toList
             let key = fn.[1].Substring(name.Length + 2)
             let signature = fn.[0] + " " + fn.[2]
             (key, (signature, method)))
        |> LCov.sortByFirst
        |> Seq.filter (fun (_, (_, mt)) ->
             mt.Descendants("SequencePoint".X)
             |> Seq.isEmpty
             |> not)
        |> Seq.fold (processMethod methods) (0, 0, 0, 0, 0, 0)

      let processClass (classes : XElement) (cvcum, ccum) ((name, source), methodSet) =
        let ``class`` =
          XElement("class".X, XAttribute("name".X, name), XAttribute("filename".X, source))
        classes.Add(``class``)
        let methods = XElement("methods".X)
        ``class``.Add(methods)
        let (b, bv, s, sv, c, cv) = arrangeMethods name methods methodSet
        setRate sv s "line-rate" ``class``
        setRate bv b "branch-rate" ``class``
        setRate cv c "complexity" ``class``
        (cv + cvcum, c + ccum)

      let processModule files classes (``module`` : XElement) =
        ``module``.Descendants("Method".X)
        |> Seq.filter (fun m ->
             m.Descendants("FileRef".X)
             |> Seq.isEmpty
             |> not)
        |> Seq.groupBy (fun method ->
             ((method.Parent.Parent.Descendants("FullName".X) |> Seq.head).Value,
              method.Descendants("FileRef".X)
              |> Seq.map (fun s -> files |> Map.find (s.Attribute("uid".X).Value))
              |> Seq.head))
        |> LCov.sortByFirst
        |> Seq.fold (processClass classes) (0, 0)

      let lookUpFiles (``module`` : XElement) =
        ``module``.Descendants("File".X)
        |> Seq.fold (fun m x ->
             m |> Map.add (x.Attribute("uid".X).Value) (x.Attribute("fullPath".X).Value))
             Map.empty
      report.Descendants("Module".X)
      |> Seq.filter (fun m ->
           m.Descendants("Class".X)
           |> Seq.isEmpty
           |> not)
      |> Seq.iter (fun ``module`` ->
           let mname =
             ``module``.Descendants("ModuleName".X)
             |> Seq.map (fun x -> x.Value)
             |> Seq.head

           let package = XElement("package".X, XAttribute("name".X, mname))
           let files = lookUpFiles ``module``
           packages.Add(package)
           let classes = XElement("classes".X)
           package.Add(classes)
           extract ``module`` package
           let (cv, c) = processModule files classes ``module``
           setRate cv c "complexity" package)
      extract (report.Descendants("CoverageSession".X) |> Seq.head) packages.Parent
      addSources report packages.Parent "File" "fullPath"

  let internal convertReport (report : XDocument) (format : Base.ReportFormat) =
    let rewrite = XDocument(XDeclaration("1.0", "utf-8", "no"), [||])
    let doctype =
      XDocumentType
        ("coverage", null, "http://cobertura.sourceforge.net/xml/coverage-04.dtd", null)
    rewrite.Add(doctype)
    let element =
      XElement
        ("coverage".X, XAttribute("line-rate".X, 0), XAttribute("branch-rate".X, 0),
          XAttribute("lines-covered".X, 0), XAttribute("lines-valid".X, 0),
          XAttribute("branches-covered".X, 0), XAttribute("branches-valid".X, 0),
          XAttribute("complexity".X, 1),
          XAttribute("version".X, AssemblyVersionInformation.AssemblyVersion),
          XAttribute
            ("timestamp".X,
            int
              ((DateTime.UtcNow - DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)).TotalSeconds)))
    rewrite.Add(element)
    element.Add(XElement("sources".X))
    let packages = XElement("packages".X)
    element.Add(packages)

    match format with
    | Base.ReportFormat.NCover -> I.nCover report packages
    | _ -> I.openCover report packages

    // lines reprise
    packages.Descendants("class".X)
    |> Seq.iter (fun c ->
          let reprise = XElement("lines".X)
          c.Add reprise
          let lines =
            c.Descendants("line".X)
            |> Seq.sortBy (fun l ->
                l.Attribute("number".X).Value
                |> Int32.TryParse
                |> snd)
            |> Seq.toList
          lines
          |> List.iter (fun l ->
              let copy = XElement(l)
              reprise.Add copy))
    rewrite

  let internal summary (report : XDocument) (format : Base.ReportFormat) result =
    let rewrite = convertReport report format
    rewrite.Save(!path |> Option.get)
    result