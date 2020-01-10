namespace AltCover

open System
open System.IO
open System.Xml.Linq
open Augment
open System.Globalization

// based on the sample file at https://raw.githubusercontent.com/jenkinsci/cobertura-plugin/master/src/test/resources/hudson/plugins/cobertura/coverage-with-data.xml

module internal Cobertura =
  let internal path : Option<string> ref = ref None
  let X = OpenCover.X

  let SetRate hits total rate (target : XElement) =
    if total > 0 then
      let ratio = (float hits) / (float total)
      target.SetAttributeValue(X rate, String.Format("{0:0.##}", ratio))

  let AddSources (report : XDocument) (target : XElement) tag attribute =
    report.Descendants(X tag)
    |> Seq.map (fun s -> s.Attribute(X attribute).Value |> Path.GetDirectoryName)
    |> Seq.fold (fun s f -> s |> Set.add f) Set.empty<String>
    |> Seq.sort
    |> Seq.iter (fun f ->
         target.Descendants(X "sources")
         |> Seq.iter (fun s -> s.Add(XElement(X "source", XText(f)))))

  let internal NCover (report : XDocument) (packages : XElement) =
    let ProcessSeqPnts (method : XElement) (lines : XElement) =
      method.Descendants(X "seqpnt")
      |> Seq.filter (fun s -> s.Attribute(X "excluded").Value <> "true")
      |> Seq.fold (fun (h, t) s ->
           let vc = s.Attribute(X "visitcount")

           let vx =
             if isNull vc then "0" else vc.Value

           let line =
             XElement
               (X "line", XAttribute(X "number", s.Attribute(X "line").Value),
                XAttribute(X "hits", vx), XAttribute(X "branch", "false"))
           lines.Add line
           (h + (if vx = "0" then 0 else 1), t + 1)) (0, 0)

    let ProcessMethod (methods : XElement) (hits, total) (key, (signature, method)) =
      let mtx =
        XElement
          (X "method", XAttribute(X "name", key), XAttribute(X "signature", signature))
      methods.Add(mtx)
      let lines = XElement(X "lines")
      mtx.Add(lines)
      let (mHits, mTotal) = ProcessSeqPnts method lines
      SetRate mHits mTotal "line-rate" mtx
      SetRate 1 1 "branch-rate" mtx
      SetRate 1 1 "complexity" mtx
      (hits + mHits, total + mTotal)

    let SortMethod (n : String) (methods : XElement) (method : XElement seq) =
      method
      |> Seq.map (fun m ->
           let key, signature =
             let fna = m.Attribute(X "fullname")
             if fna |> isNull then
               (m.Attribute(X "class").Value + "." + m.Attribute(X "name").Value,
                String.Empty)
             else
               let fn = fna.Value.Split([| ' '; '(' |]) |> Array.toList
               (fn.[1].Substring(n.Length + 1), fn.[0] + " " + fn.[2])
           (key, (signature, m)))
      |> LCov.SortByFirst
      |> Seq.fold (ProcessMethod methods) (0, 0)

    let ProcessClass (classes : XElement) (hits, total) ((name, signature), method) =
      let ``class`` =
        XElement
          (X "class", XAttribute(X "name", name), XAttribute(X "filename", signature))
      classes.Add(``class``)
      let methods = XElement(X "methods")
      ``class``.Add(methods)
      let (mHits, mTotal) = SortMethod name methods method
      SetRate mHits mTotal "line-rate" ``class``
      SetRate 1 1 "branch-rate" ``class``
      SetRate 1 1 "complexity" ``class``
      (hits + mHits, total + mTotal)

    let ExtractClasses (``module`` : XElement) classes =
      ``module``.Descendants(X "method")
      |> Seq.filter (fun m -> m.Attribute(X "excluded").Value <> "true")
      |> Seq.groupBy (fun method ->
           (method.Attribute(X "class").Value,
            method.Descendants(X "seqpnt")
            |> Seq.map (fun s -> s.Attribute(X "document").Value)
            |> Seq.head))
      |> LCov.SortByFirst
      |> Seq.fold (ProcessClass classes) (0, 0)

    let ProcessModule (hits, total) (``module`` : XElement) =
      let package =
        XElement(X "package", XAttribute(X "name", ``module``.Attribute(X "name").Value))
      packages.Add(package)
      let classes = XElement(X "classes")
      package.Add(classes)
      let (cHits, cTotal) = ExtractClasses ``module`` classes
      SetRate cHits cTotal "line-rate" package
      SetRate 1 1 "branch-rate" package
      SetRate 1 1 "complexity" package
      (hits + cHits, total + cTotal)

    let (hits, total) = report.Descendants(X "module") |> Seq.fold ProcessModule (0, 0)
    let p = packages.Parent
    SetRate hits total "line-rate" p
    SetRate 1 1 "branch-rate" p
    p.Attribute(X "lines-valid").Value <- total.ToString(CultureInfo.InvariantCulture)
    p.Attribute(X "lines-covered").Value <- hits.ToString(CultureInfo.InvariantCulture)

    AddSources report packages.Parent "seqpnt" "document"

  let internal OpenCover (report : XDocument) (packages : XElement) =
    let extract (owner : XElement) (target : XElement) =
      let summary = owner.Descendants(X "Summary") |> Seq.head

      let valueOf name =
        summary.Attribute(X name).Value
        |> Int32.TryParse
        |> snd

      let b = valueOf "numBranchPoints"
      let bv = valueOf "visitedBranchPoints"
      let s = valueOf "numSequencePoints"
      let sv = valueOf "visitedSequencePoints"

      SetRate sv s "line-rate" target
      SetRate bv b "branch-rate" target
      if target.Name.LocalName.Equals("coverage", StringComparison.Ordinal) then
        let copyup name (value : int) =
          target.Attribute(X name).Value <- value.ToString(CultureInfo.InvariantCulture)

        copyup "lines-valid" s
        copyup "lines-covered" sv
        copyup "branches-valid" b
        copyup "branches-covered" bv
        target.Attribute(X "complexity").Value <- (summary.Attribute
                                                     (X "maxCyclomaticComplexity")).Value

    let doBranch bec bev uspid (line : XElement) =
      let pc = Math.Round(100.0 * (float bev) / (float bec)) |> int
      line.SetAttributeValue
        (X "condition-coverage", sprintf "%d%% (%d/%d)" pc bev bec)
      let cc = XElement(X "conditions")
      line.Add cc
      let co =
        XElement
          (X "condition", XAttribute(X "number", uspid), XAttribute(X "type", "jump"),
           XAttribute(X "coverage", sprintf "%d%%" pc))
      cc.Add co

    let ProcessSeqPnt (lines : XElement) (s : XElement) =
      let vc = s.Attribute(X "vc")

      let vx =
        (vc
         |> Option.nullable
         |> Option.getOrElse (XAttribute(X "dummy", "0"))).Value

      let bec =
        s.Attribute(X "bec").Value
        |> Int32.TryParse
        |> snd

      let bev =
        s.Attribute(X "bev").Value
        |> Int32.TryParse
        |> snd

      let line =
        XElement
          (X "line", XAttribute(X "number", s.Attribute(X "sl").Value),
           XAttribute(X "hits", vx),
           XAttribute
             (X "branch",
              (if bec = 0 then "false" else "true")))

      if bec > 0 then
        let uspid = s.Attribute(X "uspid").Value // KISS approach
        doBranch bec bev uspid line
      lines.Add line

    let AddMethod (methods : XElement) (key, signature) =
      let mtx =
        XElement
          (X "method", XAttribute(X "name", key), XAttribute(X "signature", signature))
      methods.Add(mtx)
      let lines = XElement(X "lines")
      mtx.Add(lines)
      (mtx, lines)

    let AddAttributeValue (element : XElement) name v =
      v + (element.Attribute(X name).Value
           |> Int32.TryParse
           |> snd)

    let ProcessMethod (methods : XElement) (b, bv, s, sv, c, cv)
        (key, (signature, method)) =
      let ccplex = 0 |> AddAttributeValue method "cyclomaticComplexity"
      let mtx, lines = AddMethod (methods : XElement) (key, signature)
      extract method mtx
      mtx.Add(XAttribute(X "complexity", ccplex))
      method.Descendants(X "SequencePoint") |> Seq.iter (ProcessSeqPnt lines)
      let summary = method.Descendants(X "Summary") |> Seq.head
      (b |> AddAttributeValue summary "numBranchPoints",
       bv |> AddAttributeValue summary "visitedBranchPoints",
       s |> AddAttributeValue summary "numSequencePoints",
       sv |> AddAttributeValue summary "visitedSequencePoints", c + 1, cv + ccplex)

    let ArrangeMethods (name : String) (methods : XElement) (methodSet : XElement seq) =
      methodSet
      |> Seq.map (fun method ->
           let fn =
             (method.Descendants(X "Name") |> Seq.head).Value.Split([| ' '; '(' |])
             |> Array.toList
           let key = fn.[1].Substring(name.Length + 2)
           let signature = fn.[0] + " " + fn.[2]
           (key, (signature, method)))
      |> LCov.SortByFirst
      |> Seq.filter (fun (_, (_, mt)) ->
           mt.Descendants(X "SequencePoint")
           |> Seq.isEmpty
           |> not)
      |> Seq.fold (ProcessMethod methods) (0, 0, 0, 0, 0, 0)

    let ProcessClass (classes : XElement) (cvcum, ccum) ((name, source), methodSet) =
      let ``class`` =
        XElement(X "class", XAttribute(X "name", name), XAttribute(X "filename", source))
      classes.Add(``class``)
      let methods = XElement(X "methods")
      ``class``.Add(methods)
      let (b, bv, s, sv, c, cv) = ArrangeMethods name methods methodSet
      SetRate sv s "line-rate" ``class``
      SetRate bv b "branch-rate" ``class``
      SetRate cv c "complexity" ``class``
      (cv + cvcum, c + ccum)

    let ProcessModule files classes (``module`` : XElement) =
      ``module``.Descendants(X "Method")
      |> Seq.filter (fun m ->
           m.Descendants(X "FileRef")
           |> Seq.isEmpty
           |> not)
      |> Seq.groupBy (fun method ->
           ((method.Parent.Parent.Descendants(X "FullName") |> Seq.head).Value,
            method.Descendants(X "FileRef")
            |> Seq.map (fun s -> files |> Map.find (s.Attribute(X "uid").Value))
            |> Seq.head))
      |> LCov.SortByFirst
      |> Seq.fold (ProcessClass classes) (0, 0)

    let lookUpFiles (``module`` : XElement) =
      ``module``.Descendants(X "File")
      |> Seq.fold (fun m x ->
           m |> Map.add (x.Attribute(X "uid").Value) (x.Attribute(X "fullPath").Value))
           Map.empty
    report.Descendants(X "Module")
    |> Seq.filter (fun m ->
         m.Descendants(X "Class")
         |> Seq.isEmpty
         |> not)
    |> Seq.iter (fun ``module`` ->
         let mname =
           ``module``.Descendants(X "ModuleName")
           |> Seq.map (fun x -> x.Value)
           |> Seq.head

         let package = XElement(X "package", XAttribute(X "name", mname))
         let files = lookUpFiles ``module``
         packages.Add(package)
         let classes = XElement(X "classes")
         package.Add(classes)
         extract ``module`` package
         let (cv, c) = ProcessModule files classes ``module``
         SetRate cv c "complexity" package)
    extract (report.Descendants(X "CoverageSession") |> Seq.head) packages.Parent
    AddSources report packages.Parent "File" "fullPath"

  let ConvertReport (report : XDocument) (format : Base.ReportFormat) =
    let rewrite = XDocument(XDeclaration("1.0", "utf-8", "no"), [||])
    let doctype =
      XDocumentType
        ("coverage", null, "http://cobertura.sourceforge.net/xml/coverage-04.dtd", null)
    rewrite.Add(doctype)
    let element =
      XElement
        (X "coverage", XAttribute(X "line-rate", 0), XAttribute(X "branch-rate", 0),
         XAttribute(X "lines-covered", 0), XAttribute(X "lines-valid", 0),
         XAttribute(X "branches-covered", 0), XAttribute(X "branches-valid", 0),
         XAttribute(X "complexity", 1),
         XAttribute(X "version", AssemblyVersionInformation.AssemblyVersion),
         XAttribute
           (X "timestamp",
            int
              ((DateTime.UtcNow - DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)).TotalSeconds)))
    rewrite.Add(element)
    element.Add(XElement(X "sources"))
    let packages = XElement(X "packages")
    element.Add(packages)

    match format with
    | Base.ReportFormat.NCover -> NCover report packages
    | _ -> OpenCover report packages

    // lines reprise
    packages.Descendants(X "class")
    |> Seq.iter (fun c ->
         let reprise = XElement(X "lines")
         c.Add reprise
         let lines =
           c.Descendants(X "line")
           |> Seq.sortBy (fun l ->
                l.Attribute(X "number").Value
                |> Int32.TryParse
                |> snd)
           |> Seq.toList
         lines
         |> List.iter (fun l ->
              let copy = XElement(l)
              reprise.Add copy))
    rewrite

  let internal Summary (report : XDocument) (format : Base.ReportFormat) result =
    let rewrite = ConvertReport report format
    rewrite.Save(!path |> Option.get)
    result