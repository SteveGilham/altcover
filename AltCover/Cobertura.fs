namespace AltCover

open System
open System.Xml.Linq

// based on the sample file at https://raw.githubusercontent.com/jenkinsci/cobertura-plugin/master/src/test/resources/hudson/plugins/cobertura/coverage-with-data.xml

module Cobertura =
  let internal path : Option<string> ref = ref None
  let X = OpenCover.X

  let SetRate hits total rate (target:XElement) =
     if total > 0 then target.SetAttributeValue(X rate, (float hits)/(float total))

  let internal NCover (report:XDocument) (packages:XElement) =
    let ProcessSeqPnts (``method``:XElement) (lines:XElement) =
       ``method``.Descendants(X "seqpnt")
       |> Seq.fold (fun (h,t) s -> let vc = s.Attribute(X "visitcount")
                                   let vx = if isNull vc then "0" else vc.Value
                                   let line = XElement(X "line",
                                               XAttribute(X "number", s.Attribute(X "line").Value),
                                               XAttribute(X "hits", vx),
                                               XAttribute(X "branch", "false"))
                                   lines.Add line
                                   (h + (if vx = "0" then 0 else 1), t + 1)) (0,0)

    let ProcessMethod (methods:XElement) (hits, total) (key, (signature, ``method``)) =
      let mtx = XElement(X "method",
                         XAttribute(X "name", key),
                         XAttribute(X "signature", signature))
      methods.Add(mtx)
      let lines = XElement(X "lines")
      mtx.Add(lines)
      let (mHits, mTotal) = ProcessSeqPnts ``method`` lines
      SetRate mHits mTotal "line-rate" mtx
      (hits + mHits, total + mTotal)

    let SortMethod (n:String) (methods:XElement) (``method``: XElement seq) =
      ``method``
      |> Seq.map(fun m -> let fn = m.Attribute(X "fullname").Value.Split([| ' '; '(' |])
                                    |> Array.toList
                          let key = fn.[1].Substring(n.Length + 1)
                          let signature = fn.[0] + " " + fn.[2]
                          (key, (signature, m)))
      |> LCov.SortByFirst
      |> Seq.fold (ProcessMethod methods) (0,0)

    let ProcessClass (classes:XElement) (hits, total) ((name,signature),``method``) =
      let ``class`` = XElement(X "class",
                        XAttribute(X "name", name),
                        XAttribute(X "filename", signature))
      classes.Add(``class``)
      let methods = XElement(X "methods")
      ``class``.Add(methods)
      let (mHits, mTotal) = SortMethod name methods ``method``
      SetRate mHits mTotal "line-rate" ``class``
      (hits + mHits, total + mTotal)

    let ExtractClasses (``module``:XElement) classes =
      ``module``.Descendants(X "method")
      |> Seq.groupBy(fun ``method`` -> (``method``.Attribute(X "class").Value,
                                        ``method``.Descendants(X "seqpnt")
                                        |> Seq.map (fun s -> s.Attribute(X "document").Value)
                                        |> Seq.head))
      |> LCov.SortByFirst
      |> Seq.fold (ProcessClass classes) (0,0)

    let ProcessModule  (hits, total) (``module``:XElement) =
      let package = XElement(X "package",
                                XAttribute(X "name", ``module``.Attribute(X "name").Value))
      packages.Add(package)
      let classes = XElement(X "classes")
      package.Add(classes)
      let (cHits, cTotal) = ExtractClasses ``module`` classes
      SetRate cHits cTotal "line-rate" package
      (hits + cHits, total + cTotal)

    let (hits, total) = report.Descendants(X "module")
                       |> Seq.fold ProcessModule (0,0)
    SetRate hits total "line-rate" packages.Parent
    packages.Parent.SetAttributeValue(X "branch-rate", null)

  let internal OpenCover (report:XDocument)  (packages:XElement) =
    let extract (owner:XElement) (target:XElement) =
        let summary = owner.Descendants(X "Summary") |> Seq.head
        let b = summary.Attribute(X "numBranchPoints").Value |> Int32.TryParse |> snd
        let bv = summary.Attribute(X "visitedBranchPoints").Value |> Int32.TryParse |> snd
        let s = summary.Attribute(X "numSequencePoints").Value |> Int32.TryParse |> snd
        let sv = summary.Attribute(X "visitedSequencePoints").Value |> Int32.TryParse |> snd
        SetRate sv s "line-rate" target
        SetRate bv b "branch-rate" target

    let doBranch bec bev (line:XElement) =
        let pc = Math.Round(100.0 * (float bev)/ (float bec)) |> int
        line.SetAttributeValue(X "condition-coverage",
                                sprintf "%d%% (%d/%d)" pc bev bec)
        let cc = XElement(X "conditions")
        line.Add cc
        let co = XElement(X "condition",
                        XAttribute(X "number", 0),
                        XAttribute(X "type", "jump"),
                        XAttribute(X "coverage", sprintf "%d%%" pc))
        cc.Add co

    let ProcessSeqPnt (lines:XElement) (s:XElement) =
        let vc = s.Attribute(X "vc")
        let vx = if isNull vc then "0" else vc.Value
        let bec = s.Attribute(X "bec").Value |> Int32.TryParse |> snd
        let bev = s.Attribute(X "bev").Value |> Int32.TryParse |> snd
        let line = XElement(X "line",
                        XAttribute(X "number", s.Attribute(X "sl").Value),
                        XAttribute(X "hits", vx),
                        XAttribute(X "branch", if bec = 0 then "false" else "true"))

        if bec > 0 then doBranch bec bev line
        lines.Add line

    let ProcessMethod (methods:XElement) (b,bv,s,sv) (key, (signature, ``method``)) =
      let mtx = XElement(X "method",
                          XAttribute(X "name", key),
                          XAttribute(X "signature", signature))
      extract ``method`` mtx
      methods.Add(mtx)
      let lines = XElement(X "lines")
      mtx.Add(lines)
      ``method``.Descendants(X "SequencePoint")
      |> Seq.iter(ProcessSeqPnt lines)
      let summary = ``method``.Descendants(X "Summary") |> Seq.head
      ( b + (summary.Attribute(X "numBranchPoints").Value |> Int32.TryParse |> snd),
        bv + (summary.Attribute(X "visitedBranchPoints").Value |> Int32.TryParse |> snd),
        s + (summary.Attribute(X "numSequencePoints").Value |> Int32.TryParse |> snd),
        sv + (summary.Attribute(X "visitedSequencePoints").Value |> Int32.TryParse |> snd))

    let ArrangeMethods (name:String) (methods:XElement) (methodSet:XElement seq) =
      methodSet
      |> Seq.map(fun ``method`` -> let fn = (``method``.Descendants(X "Name")
                                             |> Seq.head).Value.Split([| ' '; '(' |])
                                             |> Array.toList
                                   let key = fn.[1].Substring(name.Length + 2)
                                   let signature = fn.[0] + " " + fn.[2]
                                   (key, (signature, ``method``)))
      |> LCov.SortByFirst
      |> Seq.filter (fun (_,(_,mt)) -> mt.Descendants(X "SequencePoint") |> Seq.isEmpty |> not)
      |> Seq.fold(ProcessMethod methods) (0,0,0,0)

    let ProcessClass (classes:XElement) ((name, source), methodSet) =
      let ``class`` = XElement(X "class",
                                  XAttribute(X "name", name),
                                  XAttribute(X "filename", source))
      classes.Add(``class``)
      let methods = XElement(X "methods")
      ``class``.Add(methods)
      let (b,bv,s,sv) = ArrangeMethods name methods methodSet
      SetRate sv s "line-rate" ``class``
      SetRate bv b "branch-rate" ``class``

    let ProcessModule files classes (``module``:XElement) =
      ``module``.Descendants(X "Method")
      |> Seq.filter(fun m -> m.Descendants(X "FileRef") |> Seq.isEmpty |> not)
      |> Seq.groupBy(fun ``method`` -> ((``method``.Parent.Parent.Descendants(X "FullName")
                                        |> Seq.head).Value,
                                        ``method``.Descendants(X "FileRef")
                                        |> Seq.map (fun s -> files
                                                             |> Map.find (s.Attribute(X "uid").Value))
                                        |> Seq.head))
      |> LCov.SortByFirst
      |> Seq.iter (ProcessClass classes)

    let lookUpFiles (``module``:XElement) =
       ``module``.Descendants(X "File")
       |> Seq.fold(fun m x -> m |>
                               Map.add (x.Attribute(X "uid").Value)
                                       (x.Attribute(X "fullPath").Value))
                                       Map.empty
    report.Descendants(X "Module")
    |> Seq.filter(fun m -> m.Descendants(X "Class") |> Seq.isEmpty |> not)
    |> Seq.iter (fun ``module`` -> let mname = ``module``.Descendants(X "ModuleName")
                                               |> Seq.map (fun x -> x.Value)
                                               |> Seq.head
                                   let package = XElement(X "package",
                                                          XAttribute(X "name", mname))
                                   let files = lookUpFiles ``module``
                                   packages.Add(package)
                                   let classes = XElement(X "classes")
                                   package.Add(classes)

                                   extract ``module`` package
                                   ProcessModule files classes ``module``)

    extract (report.Descendants(X "CoverageSession") |> Seq.head) packages.Parent

  let internal Summary (report:XDocument) (format:Base.ReportFormat) result =
    let rewrite = XDocument(XDeclaration("1.0", "utf-8", "yes"), [||])
    let element = XElement(X "coverage",
                            XAttribute(X "line-rate", 0),
                            XAttribute(X "branch-rate", 0),
                            XAttribute(X "version", AssemblyVersionInformation.AssemblyVersion),
                            XAttribute(X "timestamp",
                                       int((DateTime.UtcNow -
                                            DateTime(1970,1,1,0,0,0,DateTimeKind.Utc)).TotalSeconds))
                )

    rewrite.Add(element)
    let packages = XElement(X "packages")
    element.Add(packages)

    match format with
    | Base.ReportFormat.NCover -> NCover report packages
    | _ -> OpenCover report packages

    // lines reprise
    packages.Descendants(X "class")
    |> Seq.iter(fun c -> let reprise = XElement(X "lines")
                         c.Add reprise
                         let lines = c.Descendants(X "line")
                                     |> Seq.sortBy(fun l -> l.Attribute(X "number").Value
                                                            |> Int32.TryParse |> snd)
                                     |> Seq.toList
                         lines
                         |> List.iter (fun l -> let copy = XElement(l)
                                                reprise.Add copy))

    rewrite.Save(!path |> Option.get)
    result