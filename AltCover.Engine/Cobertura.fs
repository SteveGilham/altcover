﻿namespace AltCover

open System
open System.Collections.Generic
open System.IO
open System.Xml.Linq
open System.Globalization

open AltCover.Shared
open System.Diagnostics.CodeAnalysis

// based on the sample file at https://raw.githubusercontent.com/jenkinsci/cobertura-plugin/master/src/test/resources/hudson/plugins/cobertura/coverage-with-data.xml
[<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming",
                                                  "CA1704",
                                                  Justification = "'Cobertura' is jargon")>]
module internal Cobertura =
  let internal path: Option<string> ref =
    ref None

  let internal packages: string list ref =
    ref List.empty<string>

  module internal I =

    let internal setRate hits total (rate: string) (target: XElement) =

      let ratio =
        if total > 0 then
          (float hits) / (float total)
        else
          0.0

      target.SetAttributeValue(
        rate.X,
        String.Format(CultureInfo.InvariantCulture, "{0:0.##}", ratio)
      )

    [<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Performance",
                                                      "AvoidUncalledPrivateCodeRule",
                                                      Justification =
                                                        "Temporary to validate")>]
    let splitPath path =
      let rec facets path bits =
        let npath = Path.GetDirectoryName path

        let facet =
          if String.IsNullOrWhiteSpace npath then
            path
          else
            (Path.GetFileName path)

        let wfacet = facet :: bits

        if String.IsNullOrWhiteSpace npath then
          wfacet
        else
          facets npath wfacet

      if String.IsNullOrWhiteSpace path then
        [ String.Empty ]
      else
        facets (path.Replace('\\', '/')) []

    [<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Globalization",
                                                      "PreferStringComparisonOverrideRule",
                                                      Justification =
                                                        "Compiler generated comparisons")>]
    let grouping path =
      match path with
      | [ "\\" ]
      | [ "/" ] -> "/"
      | "\\" :: x :: _
      | "/" :: x :: _ -> "/" + x
      | [] -> String.Empty
      | _ -> path |> Seq.head

    [<SuppressMessage("Gendarme.Rules.Exceptions",
                      "InstantiateArgumentExceptionCorrectlyRule",
                      Justification = "Compiler inlines")>]
    [<SuppressMessage("Gendarme.Rules.Performance",
                      "AvoidRepetitiveCallsToPropertiesRule",
                      Justification = "Compiler inlines")>]
    let extractSource (values: ((string * string seq) * string list) seq) =
      let starter = values |> Seq.head

      match Seq.length values with
      | 1 -> starter |> fst
      | _ -> // extract longest common prefix
        let files =
          values |> Seq.map fst |> Seq.collect snd

        let l =
          values
          |> Seq.map (fun (_, split) -> Seq.length split)
          |> Seq.min

        let parts =
          values
          |> Seq.map (fun (_, split) -> split |> Seq.take l |> Seq.toArray)

        let tokens =
          (snd starter)
          |> Seq.mapi (fun i facet -> (i, facet))
          |> Seq.take l

        let prefix =
          tokens
          |> Seq.takeWhile (fun (i, facet) ->
            parts
            |> Seq.map (fun x -> x[i])
            |> Seq.forall (fun x -> x.Equals(facet, StringComparison.Ordinal)))
          |> Seq.map snd
          |> Seq.toArray
          |> Path.Combine

        let trimmed = prefix.Length
        let source = starter |> fst |> fst

        (source.Substring(0, trimmed).Trim('\\')
         + String([| Path.DirectorySeparatorChar |]),
         files)

    [<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Maintainability",
                                                      "AvoidUnnecessarySpecializationRule",
                                                      Justification =
                                                        "AvoidSpeculativeGenerality too")>]
    let internal addSources
      (report: XDocument)
      (target: XElement)
      (tag: string)
      (attribute: string)
      =
      let files =
        report.Descendants(tag.X)
        |> Seq.map (fun s -> s.Attribute(attribute.X).Value)

      let table =
        Dictionary<string option, string>()

      let rawsources =
        files
        |> Seq.filter (fun a ->
          let fail =
            a |> String.IsNullOrWhiteSpace |> not

          if fail then
            table[Option.ofObj a] <- String.Empty

          fail)
        |> Seq.map (fun a -> a, Path.GetDirectoryName a)
        |> Seq.filter (fun (a, d) ->
          let fail =
            d |> String.IsNullOrWhiteSpace |> not

          if fail then
            table[Option.ofObj a] <- a

          fail)
        |> Seq.groupBy snd
        |> Seq.map (fun (a, s) -> a, s |> Seq.map fst)
        |> Seq.sortBy fst // seq of (directory, files full names)

      let packaged =
        packages.Value
        |> Seq.map (fun x -> x, Seq.empty<string>)

      let groupable = // seq of ((directory, files full names), facets)
        Seq.concat [ packaged; rawsources ]
        |> Seq.map (fun x -> (x, x |> fst |> splitPath))

      let groups = // seq of (root, seq of ((directory, files full names), facets))
        groupable |> Seq.groupBy (snd >> grouping)

      let results = // throws away everything but the sources
        groups |> Seq.map (snd >> extractSource)

      results
      |> Seq.sortBy fst
      |> Seq.iter (fun f ->
        target.Descendants("sources".X)
        |> Seq.iter _.Add(XElement("source".X, XText(fst f))))

      results // TODO - make look-up table
      |> Seq.iter (fun (p, files) ->
        let prefix =
          1 + p.Replace('\\', '/').Trim('/').Length

        files
        |> Seq.iter (fun f -> table[Some f] <- f.Substring(prefix))

      )

      table

    let internal nCover (report: XDocument) (packages: XElement) =

      let sources =
        addSources report packages.Parent "seqpnt" "document"

      let processSeqPnts document (method: XElement) (lines: XElement) =
        method.Descendants("seqpnt".X)
        |> Seq.filter (fun s ->
          s.Attribute("excluded".X).Value != "true"
          && s.Attribute("document".X).Value == document)
        |> Seq.fold
          (fun (h, t) s ->
            let vc = s.Attribute("visitcount".X)

            let vx = if isNull vc then "0" else vc.Value

            let line =
              XElement(
                "line".X,
                XAttribute("number".X, s.Attribute("line".X).Value),
                XAttribute("hits".X, vx),
                XAttribute("branch".X, "false")
              )

            lines.Add line
            (h + (if vx == "0" then 0 else 1), t + 1))
          (0, 0)

      let processMethod
        document
        (methods: XElement)
        (hits, total)
        (key, (signature, method))
        =
        let mtx =
          XElement(
            "method".X,
            XAttribute("name".X, key),
            XAttribute("signature".X, signature)
          )

        methods.Add(mtx)
        let lines = XElement("lines".X)
        mtx.Add(lines)

        let (mHits, mTotal) =
          processSeqPnts document method lines

        setRate mHits mTotal "line-rate" mtx
        setRate 1 1 "branch-rate" mtx
        setRate 1 1 "complexity" mtx
        (hits + mHits, total + mTotal)

      let sortMethod (document: String) (methods: XElement) (method: XElement seq) =
        method
        |> Seq.map (fun m ->
          let key, signature =
            let fna = m.Attribute("fullname".X)

            if fna |> isNull then
              (m.Attribute("name".X).Value, "ReturnType (Arguments)")
            else
              let fn = fna.Value
              let cname = m.Attribute("class".X).Value
              let mname = m.Attribute("name".X).Value

              let indexmname =
                mname.Split('`') |> Seq.head

              let classAt =
                fn.IndexOf(cname, StringComparison.Ordinal)

              let returnType = fn.Substring(0, classAt)

              let methodAt =
                fn.IndexOf(indexmname, classAt + cname.Length, StringComparison.Ordinal)

              let argsAt = methodAt + indexmname.Length
              let args = fn.Substring(argsAt)
              let signature = returnType + args
              (mname, signature)

          (key, (signature, m)))
        |> LCov.sortByFirst id
        |> Seq.fold (processMethod document methods) (0, 0)

      let processClass
        (classes: XElement)
        (hits, total)
        ((name, document: string), method)
        =
        let filename =
          sources[Option.ofObj document]

        let ``class`` =
          XElement(
            "class".X,
            XAttribute("name".X, name),
            XAttribute("filename".X, filename)
          )

        classes.Add(``class``)
        let methods = XElement("methods".X)
        ``class``.Add(methods)

        let (mHits, mTotal) =
          sortMethod document methods method

        setRate mHits mTotal "line-rate" ``class``
        setRate 1 1 "branch-rate" ``class``
        setRate 1 1 "complexity" ``class``
        (hits + mHits, total + mTotal)

      let extractClasses (``module``: XElement) classes =
        ``module``.Descendants("method".X)
        |> Seq.filter (fun m ->
          m.Attribute("excluded".X).Value != "true"
          && m.Descendants("seqpnt".X) |> Seq.isEmpty |> not)

        |> Seq.collect (fun method ->
          let cname =
            method.Attribute("class".X).Value

          method.Descendants("seqpnt".X)
          |> Seq.map _.Attribute("document".X).Value
          |> Seq.distinct
          |> Seq.sort
          |> Seq.map (fun d -> (cname, d), method))
        |> Seq.groupBy fst
        |> Seq.map (fun (k, s) -> k, s |> Seq.map (fun (k, m) -> m))
        |> Seq.sortBy (fun ((c, d), _) -> c + "\u0000" + d) // short classes sort first
        |> Seq.fold (processClass classes) (0, 0)

      let processModule (hits, total) (``module``: XElement) =
        let package =
          XElement(
            "package".X,
            XAttribute("name".X, ``module``.Attribute("name".X).Value)
          )

        packages.Add(package)
        let classes = XElement("classes".X)
        package.Add(classes)

        let (cHits, cTotal) =
          extractClasses ``module`` classes

        setRate cHits cTotal "line-rate" package
        setRate 1 1 "branch-rate" package
        setRate 1 1 "complexity" package
        (hits + cHits, total + cTotal)

      let (hits, total) =
        report.Descendants("module".X)
        |> Seq.fold processModule (0, 0)

      let p = packages.Parent
      setRate hits total "line-rate" p
      setRate 1 1 "branch-rate" p
      p.Attribute("lines-valid".X).Value <- total.ToString(CultureInfo.InvariantCulture)
      p.Attribute("lines-covered".X).Value <- hits.ToString(CultureInfo.InvariantCulture)

    let internal openCover (report: XDocument) (packages: XElement) =

      let sources =
        addSources report packages.Parent "File" "fullPath"

      let extract (owner: XElement) (target: XElement) =
        let summary =
          owner.Elements("Summary".X) |> Seq.head

        let valueOf (name: string) =
          summary.Attribute(name.X).Value
          |> Int32.TryParse
          |> snd

        let branches =
          owner.Descendants("BranchPoint".X)

        let b = branches |> Seq.length

        let bv =
          branches
          |> Seq.filter (fun x -> x.Attribute("vc".X).Value != "0")
          |> Seq.length

        let s = valueOf "numSequencePoints"
        let sv = valueOf "visitedSequencePoints"

        setRate sv s "line-rate" target
        setRate bv b "branch-rate" target

        if target.Name.LocalName.Equals("coverage", StringComparison.Ordinal) then
          let copyup (name: string) (value: int) =
            target.Attribute(name.X).Value <- value.ToString(CultureInfo.InvariantCulture)

          copyup "lines-valid" s
          copyup "lines-covered" sv
          copyup "branches-valid" b
          copyup "branches-covered" bv

          target.Attribute("complexity".X).Value <-
            (summary.Attribute("maxCyclomaticComplexity".X)).Value

      let doBranch bec bev uspid (line: XElement) =
        let pc =
          Math.Round(100.0 * (float bev) / (float bec))
          |> int

        line.SetAttributeValue("condition-coverage".X, sprintf "%d%% (%d/%d)" pc bev bec)
        let cc = XElement("conditions".X)
        line.Add cc

        let co =
          XElement(
            "condition".X,
            XAttribute("number".X, uspid),
            XAttribute("type".X, "jump"),
            XAttribute("coverage".X, sprintf "%d%%" pc)
          )

        cc.Add co

      let copySeqPnt (lines: XElement) (s: int * XElement seq) =
        let rep = s |> snd |> Seq.head

        let (vcn, bec, bev) =
          s
          |> snd
          |> Seq.fold
            (fun (v, c, n) sp ->
              (v
               + (sp.Attribute("vc".X).Value
                  |> Int32.TryParse
                  |> snd),
               c
               + (sp.Attribute("bec".X).Value
                  |> Int32.TryParse
                  |> snd),
               n
               + (sp.Attribute("bev".X).Value
                  |> Int32.TryParse
                  |> snd)))
            (0, 0, 0)

        let vx =
          vcn.ToString(CultureInfo.InvariantCulture)

        let line =
          XElement(
            "line".X,
            XAttribute("number".X, rep.Attribute("sl".X).Value),
            XAttribute("hits".X, vx),
            XAttribute("branch".X, (if bec = 0 then "false" else "true"))
          )

        if bec > 0 then
          let uspid = rep.Attribute("uspid".X).Value
          doBranch bec bev uspid line

        lines.Add line

      let addMethod (methods: XElement) (key, signature) =
        let mtx =
          XElement(
            "method".X,
            XAttribute("name".X, key),
            XAttribute("signature".X, signature)
          )

        methods.Add(mtx)
        let lines = XElement("lines".X)
        mtx.Add(lines)
        (mtx, lines)

      let provideAttributeValue (element: XElement) (name: string) v =
        v
        + (element.Attribute(name.X).Value
           |> Int32.TryParse
           |> snd)

      let processMethod
        fileid
        (methods: XElement)
        (b, bv, s, sv, c, cv)
        (key, (signature, method))
        =
        let ccplex =
          0
          |> provideAttributeValue method "cyclomaticComplexity"

        let mtx, lines =
          addMethod (methods: XElement) (key, signature)

        extract method mtx
        mtx.Add(XAttribute("complexity".X, ccplex))

        method.Descendants("SequencePoint".X)
        |> Seq.filter (fun s -> s.Attribute("fileid".X).Value == fileid)
        |> Seq.groupBy (fun b -> b.Attribute("sl".X).Value |> Int32.TryParse |> snd) //_ is ambiguous???
        |> Seq.sortBy fst
        |> Seq.iter (copySeqPnt lines)

        let summary =
          method.Elements("Summary".X) |> Seq.head

        (b
         |> provideAttributeValue summary "numBranchPoints",
         bv
         |> provideAttributeValue summary "visitedBranchPoints",
         s
         |> provideAttributeValue summary "numSequencePoints",
         sv
         |> provideAttributeValue summary "visitedSequencePoints",
         c + 1,
         cv + ccplex)

      let arrangeMethods
        (name: String)
        fileid
        (methods: XElement)
        (methodSet: XElement seq)
        =
        methodSet
        |> Seq.map (fun method ->
          let fn =
            (method.Descendants("Name".X) |> Seq.head).Value

          let cplus = name + "::"

          let marker =
            fn.IndexOf(cplus, StringComparison.Ordinal)

          let returntype = fn.Substring(0, marker)
          let start = marker + cplus.Length
          let argsAt = fn.IndexOf('(', start)
          let args = fn.Substring(argsAt)
          let signature = returntype + args

          let key =
            fn.Substring(start, argsAt - start)

          (key, (signature, method)))
        |> LCov.sortByFirst id
        |> Seq.filter (fun (_, (_, mt)) ->
          mt.Descendants("SequencePoint".X)
          |> Seq.isEmpty
          |> not)
        |> Seq.fold (processMethod fileid methods) (0, 0, 0, 0, 0, 0)

      let processClass
        (files: Map<string, string>)
        (classes: XElement)
        (cvcum, ccum)
        ((name, fileid), methodSet)
        =
        let document = files |> Map.find fileid

        let filename =
          sources[Option.ofObj document]

        let ``class`` =
          XElement(
            "class".X,
            XAttribute("name".X, name),
            XAttribute("filename".X, filename)
          )

        classes.Add(``class``)
        let methods = XElement("methods".X)
        ``class``.Add(methods)

        let (b, bv, s, sv, c, cv) =
          arrangeMethods name fileid methods methodSet

        setRate sv s "line-rate" ``class``
        setRate bv b "branch-rate" ``class``
        setRate cv c "complexity" ``class``
        (cv + cvcum, c + ccum)

      let processModule files classes (``module``: XElement) =
        ``module``.Descendants("Method".X)
        |> Seq.filter (fun m ->
          m.Descendants("SequencePoint".X)
          |> Seq.isEmpty
          |> not
          || m.Descendants("BranchPoint".X)
             |> Seq.isEmpty
             |> not)
        |> Seq.collect (fun method ->
          let cname =
            (method.Parent.Parent.Descendants("FullName".X)
             |> Seq.head)
              .Value

          [ method.Descendants("SequencePoint".X)
            method.Descendants("BranchPoint".X) ]
          |> Seq.concat
          |> Seq.map _.Attribute("fileid".X).Value
          |> Seq.distinct
          |> Seq.sort
          |> Seq.map (fun d -> (cname, d), method))
        |> Seq.groupBy fst
        |> Seq.map (fun (k, s) -> k, s |> Seq.map (fun (k, m) -> m))
        |> Seq.sortBy (fun ((c, d), _) -> c + "\u0000" + (files |> Map.find d)) // short classes sort first
        |> Seq.fold (processClass files classes) (0, 0)

      let lookUpFiles (``module``: XElement) =
        ``module``.Descendants("File".X)
        |> Seq.fold
          (fun m x ->
            m
            |> Map.add (x.Attribute("uid".X).Value) (x.Attribute("fullPath".X).Value))
          Map.empty

      report.Descendants("Module".X)
      |> Seq.filter (_.Descendants("Class".X) >> Seq.isEmpty >> not)
      |> Seq.iter (fun ``module`` ->
        let mname =
          ``module``.Descendants("ModuleName".X)
          |> Seq.map _.Value
          |> Seq.head

        let package =
          XElement("package".X, XAttribute("name".X, mname))

        let files = lookUpFiles ``module``
        packages.Add(package)
        let classes = XElement("classes".X)
        package.Add(classes)
        extract ``module`` package

        let (cv, c) =
          processModule files classes ``module``

        setRate cv c "complexity" package)

      extract
        (report.Descendants("CoverageSession".X)
         |> Seq.head)
        packages.Parent

  let internal convertReport (report: XDocument) (format: ReportFormat) =
    let rewrite =
      XDocument(XDeclaration("1.0", "utf-8", "no"), [||])

    let doctype =
      XDocumentType(
        "coverage",
        null,
        "http://cobertura.sourceforge.net/xml/coverage-04.dtd",
        null
      )

    rewrite.Add(doctype)

    let element =
      XElement(
        "coverage".X,
        XAttribute("line-rate".X, 0),
        XAttribute("branch-rate".X, 0),
        XAttribute("lines-covered".X, 0),
        XAttribute("lines-valid".X, 0),
        XAttribute("branches-covered".X, 0),
        XAttribute("branches-valid".X, 0),
        XAttribute("complexity".X, 1),
        XAttribute("version".X, AssemblyVersionInformation.AssemblyVersion),
        XAttribute(
          "timestamp".X,
          int (
            (DateTime.UtcNow
             - DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc))
              .TotalSeconds
          )
        )
      )

    rewrite.Add(element)
    element.Add(XElement("sources".X))
    let packages = XElement("packages".X)
    element.Add(packages)

    (match format with
     | ReportFormat.NCover -> I.nCover
     | _ -> I.openCover)
      report
      packages // Maybe later

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

  let convertJson document =
    let x =
      document
      |> NativeJson.jsonToXml
      |> NativeJson.orderXml

    convertReport x ReportFormat.OpenCover

  let internal summary (report: DocumentType) (format: ReportFormat) result =
    let rewrite =
      match report with
      | Unknown -> null
      | XML document -> convertReport document format
      | JSON modules -> convertJson modules

    rewrite
    |> Option.ofObj
    |> Option.iter _.Save(path.Value |> Option.get)

    (result, 0uy, String.Empty)