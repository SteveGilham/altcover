namespace AltCover

open System
open System.Xml.Linq

// based on the sample file at https://raw.githubusercontent.com/jenkinsci/cobertura-plugin/master/src/test/resources/hudson/plugins/cobertura/coverage-with-data.xml
// TODO -- refactor away from the arrow anti-pattern

module Cobertura =
  let internal path : Option<string> ref = ref None
  let X = OpenCover.X

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
      if mTotal > 0 then mtx.SetAttributeValue(X "line-rate", (float mHits)/(float mTotal))
      (hits + mHits, total + mTotal)

    let SortMethod (n:String) (methods:XElement) (``method``: XElement seq) =
      ``method``
      |> Seq.map(fun m -> let fn = m.Attribute(X "fullname").Value.Split([| ' '; '(' |]) 
                                    |> Array.toList
                          let key = fn.[1].Substring(n.Length + 1)
                          let signature = fn.[0] + " " + fn.[2]
                          (key, (signature, m)))
      |> Seq.sortBy fst
      |> Seq.fold (ProcessMethod methods) (0,0)

    let ProcessClass (classes:XElement) (hits, total) ((name,signature),``method``) =
      let ``class`` = XElement(X "class",
                        XAttribute(X "name", name),
                        XAttribute(X "filename", signature))
      classes.Add(``class``)
      let methods = XElement(X "methods")
      ``class``.Add(methods)
      let (mHits, mTotal) = SortMethod name methods ``method``
      if mTotal > 0 then ``class``.SetAttributeValue(X "line-rate", (float mHits)/(float mTotal))
      (hits + mHits, total + mTotal)

    let ExtractClasses (``module``:XElement) classes =
      ``module``.Descendants(X "method")
      |> Seq.groupBy(fun ``method`` -> (``method``.Attribute(X "class").Value,
                                        ``method``.Descendants(X "seqpnt")
                                        |> Seq.map (fun s -> s.Attribute(X "document").Value)
                                        |> Seq.head))
      |> Seq.sortBy fst
      |> Seq.fold (ProcessClass classes) (0,0)

    let ProcessModule  (hits, total) (``module``:XElement) = 
      let package = XElement(X "package",
                                XAttribute(X "name", ``module``.Attribute(X "name").Value))
      packages.Add(package)
      let classes = XElement(X "classes")
      package.Add(classes)
      let (cHits, cTotal) = ExtractClasses ``module`` classes
      if cTotal > 0 then package.SetAttributeValue(X "line-rate", (float cHits)/(float cTotal))
      (hits + cHits, total + cTotal)

    let (hits, total) = report.Descendants(X "module")
                       |> Seq.fold ProcessModule (0,0)
    if total > 0 then packages.Parent.SetAttributeValue(X "line-rate", (float hits)/(float total))
    packages.Parent.SetAttributeValue(X "branch-rate", null)

  let internal OpenCover (report:XDocument)  (packages:XElement) =
    let extract (owner:XElement) (target:XElement) =
        let summary = owner.Descendants(X "Summary") |> Seq.head
        let b = summary.Attribute(X "numBranchPoints").Value |> Int32.TryParse |> snd
        let bv = summary.Attribute(X "visitedBranchPoints").Value |> Int32.TryParse |> snd
        let s = summary.Attribute(X "numSequencePoints").Value |> Int32.TryParse |> snd
        let sv = summary.Attribute(X "visitedSequencePoints").Value |> Int32.TryParse |> snd
        if s > 0 then target.SetAttributeValue(X "line-rate", (float sv)/(float s))
        if b > 0 then target.SetAttributeValue(X "branch-rate", (float bv)/(float b))
    report.Descendants(X "Module")
    |> Seq.filter(fun m -> m.Descendants(X "Class") |> Seq.isEmpty |> not)
    |> Seq.iter (fun m -> let mname = m.Descendants(X "ModuleName")
                                      |> Seq.map (fun x -> x.Value)
                                      |> Seq.head
                          let package = XElement(X "package",
                                                 XAttribute(X "name", mname))
                          let files = m.Descendants(X "File")
                                      |> Seq.fold(fun m x -> m |>
                                                             Map.add (x.Attribute(X "uid").Value) (x.Attribute(X "fullPath").Value)) Map.empty
                          packages.Add(package)
                          let classes = XElement(X "classes")
                          package.Add(classes)

                          extract m package

                          m.Descendants(X "Method")
                          |> Seq.filter(fun m -> m.Descendants(X "FileRef") |> Seq.isEmpty |> not)
                          |> Seq.groupBy(fun mx -> ((mx.Parent.Parent.Descendants(X "FullName") |> Seq.head).Value,
                                                    mx.Descendants(X "FileRef")
                                                    |> Seq.map (fun s -> files
                                                                         |> Map.find (s.Attribute(X "uid").Value))
                                                    |> Seq.head))
                          |> Seq.sortBy fst
                          |> Seq.iter (fun ((n,s),mx) -> let cx = XElement(X "class",
                                                                           XAttribute(X "name", n),
                                                                           XAttribute(X "filename", s))
                                                         classes.Add(cx)
                                                         let mxx = XElement(X "methods")
                                                         cx.Add(mxx)
                                                         let q = mx
                                                                 |> Seq.map(fun mt -> let fn = (mt.Descendants(X "Name") |> Seq.head).Value.Split([| ' '; '(' |]) |> Array.toList
                                                                                      let key = fn.[1].Substring(n.Length + 2)
                                                                                      let signa = fn.[0] + " " + fn.[2]
                                                                                      (key, (signa, mt)))
                                                                 |> Seq.sortBy fst
                                                                 |> Seq.filter (fun (_,(_,mt)) -> mt.Descendants(X "SequencePoint") |> Seq.isEmpty |> not)
                                                                 |> Seq.fold(fun (b,bv,s,sv) (key, (signa, mt)) -> let mtx = XElement(X "method",
                                                                                                                                      XAttribute(X "name", key),
                                                                                                                                      XAttribute(X "signature", signa))
                                                                                                                   extract mt mtx
                                                                                                                   mxx.Add(mtx)
                                                                                                                   let lines = XElement(X "lines")
                                                                                                                   mtx.Add(lines)
                                                                                                                   mt.Descendants(X "SequencePoint")
                                                                                                                   |> Seq.iter(fun s -> let vc = s.Attribute(X "vc")
                                                                                                                                        let vx = if isNull vc then "0" else vc.Value
                                                                                                                                        let bec = s.Attribute(X "bec").Value |> Int32.TryParse |> snd
                                                                                                                                        let bev = s.Attribute(X "bev").Value |> Int32.TryParse |> snd
                                                                                                                                        let line = XElement(X "line",
                                                                                                                                                       XAttribute(X "number", s.Attribute(X "sl").Value),
                                                                                                                                                       XAttribute(X "hits", vx),
                                                                                                                                                       XAttribute(X "branch", if bec = 0 then "false" else "true"))
                                                                                                                                        let doBranch () =
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

                                                                                                                                        if bec > 0 then doBranch()

                                                                                                                                        lines.Add line
                                                                                                                              )
                                                                                                                   let summary = mt.Descendants(X "Summary") |> Seq.head
                                                                                                                   ( b + (summary.Attribute(X "numBranchPoints").Value |> Int32.TryParse |> snd),
                                                                                                                     bv + (summary.Attribute(X "visitedBranchPoints").Value |> Int32.TryParse |> snd),
                                                                                                                     s + (summary.Attribute(X "numSequencePoints").Value |> Int32.TryParse |> snd),
                                                                                                                     sv + (summary.Attribute(X "visitedSequencePoints").Value |> Int32.TryParse |> snd))) (0,0,0,0)
                                                         let (b,bv,s,sv) = q
                                                         if s > 0 then cx.SetAttributeValue(X "line-rate", (float sv)/(float s))
                                                         if b > 0 then cx.SetAttributeValue(X "branch-rate", (float bv)/(float b))
                                                                                                                   )
    )

    extract (report.Descendants(X "CoverageSession") |> Seq.head) packages.Parent

  let internal Summary (report:XDocument) (format:Base.ReportFormat) result =
    let rewrite = XDocument(XDeclaration("1.0", "utf-8", "yes"), [||])
    let element = XElement(X "coverage",
                            XAttribute(X "line-rate", 0),
                            XAttribute(X "branch-rate", 0),
                            XAttribute(X "version", AssemblyVersionInformation.AssemblyVersion),
                            XAttribute(X "timestamp", int((DateTime.UtcNow - DateTime(1970,1,1,0,0,0,DateTimeKind.Utc)).TotalSeconds))
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