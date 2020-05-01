namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.Globalization
open System.IO
open System.Linq
open System.Xml
open System.Xml.Linq
open System.Xml.Schema
open System.Xml.XPath

open Mono.Cecil
open AltCover.XmlExtensions

[<RequireQualifiedAccess>]
module OpenCoverUtilities =

  [<SuppressMessage(
    "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
    Justification = "AvoidSpeculativeGenerality too")>]
  let private compressMethod withinSequencePoint sameSpan (m : XElement) =
    let sp = m.Descendants(XName.Get "SequencePoint")
             |> Seq.toList
    let bp = m.Descendants(XName.Get "BranchPoint")
             |> Seq.toList
    if sp
       |> List.isEmpty
       |> not
       && bp
          |> List.isEmpty
          |> not
    then
      let tail = XElement(XName.Get "SequencePoint")
      tail.Add(XAttribute (XName.Get "offset", Int32.MaxValue.ToString(CultureInfo.InvariantCulture)))
      let interleave =
        List.concat
          [ sp
            bp
            [ tail ] ]
        |> List.sortBy (fun x ->
             x.Attribute(XName.Get "offset").Value
             |> Int32.TryParse
             |> snd)
      interleave
      |> Seq.fold (fun (s : XElement, bs : XElement list) x ->
           match x.Name.LocalName with
           | "SequencePoint" ->
               let bx =
                 if withinSequencePoint then
                   let next =
                     x.Attribute(XName.Get "offset").Value
                     |> Int32.TryParse
                     |> snd

                   let (kill, keep) =
                     bs
                     |> List.partition (fun b ->
                          b.Attribute(XName.Get "offsetend").Value
                          |> Int32.TryParse
                          |> snd < next)

                   kill |> Seq.iter (fun b -> b.Remove())
                   keep
                 else
                   bs

               let by =
                 if sameSpan then
                   let (kill, keep) =
                     bx
                     |> List.groupBy
                          (fun b ->
                            (b.Attribute(XName.Get "offset").Value,
                             b.Attribute(XName.Get "offsetchain").Value,
                             b.Attribute(XName.Get "offsetend").Value))
                     |> List.fold (fun (ki, ke) (_, bz) ->
                          let totalVisits =
                            bz
                            |> Seq.sumBy (fun b ->
                                 b.Attribute(XName.Get "vc").Value
                                 |> Int32.TryParse
                                 |> snd)

                          let h = bz |> Seq.head
                          h.SetAttribute
                            ("vc", totalVisits.ToString(CultureInfo.InvariantCulture))
                          (List.concat
                            [ ki
                              bz
                              |> Seq.tail
                              |> Seq.toList ], h :: ke)) ([], [])
                   kill |> Seq.iter (fun b -> b.Remove())
                   keep
                 else
                   bx

               // Fix up what remains
               by
               |> List.rev // because the list will have been built up in reverse order
               |> Seq.mapi (fun i b -> (i, b))
               |> Seq.groupBy (fun (_, b) -> b.Attribute(XName.Get "offset").Value)
               |> Seq.iter (fun (_, paths) ->
                    paths // assume likely ranges for these numbers!
                    |> Seq.sortBy (fun (n, p) ->
                         n + 100 * (p.Attribute(XName.Get "offsetend").Value
                                    |> Int32.TryParse
                                    |> snd))
                    |> Seq.iteri (fun i (_, p) ->
                         p.SetAttribute
                           ("path", (i + 1).ToString(CultureInfo.InvariantCulture))))
               s.SetAttribute("bec", by.Length.ToString(CultureInfo.InvariantCulture))
               s.SetAttribute("bev", "0")
               (x, [])
           | _ -> (s, x :: bs)) (sp.Head, [])
      |> ignore

  let internal copyFillMethodPoint (m : XElement) (sp : XElement seq) =
    let attr = m.Attribute(XName.Get ("type", "http://www.w3.org/2001/XMLSchema-instance"))
    if attr |> isNull
    then m.Add(XAttribute(XName.Get ("type", "http://www.w3.org/2001/XMLSchema-instance"), "SequencePoint"))
    else attr.Value <- "SequencePoint"

    sp
    |> Seq.take 1
    |> Seq.collect (fun p -> p.Attributes())
    |> Seq.iter (fun a -> m.SetAttribute(a.Name.LocalName, a.Value))

  let internal visitCount (nodes : XElement seq) =
    nodes
    |> Seq.filter (fun s ->
         Int64.TryParse
           (s.Attribute(XName.Get "vc").Value, NumberStyles.Integer, CultureInfo.InvariantCulture)
         |> snd
         <> 0L)
    |> Seq.length

  /// <summary>
  /// <para type="synopsis">Updates summary and related coverage-derived data based on visit counts.</para>
  /// </summary>
  /// <param name="document">The input report</param>
  /// <param name="ordinal">How branches are indexed.  If offset values are available, those are preferred.</param>
  [<SuppressMessage("Microsoft.Globalization", "CA1308:NormalizeStringsToUppercase",
      Justification="No Turkish I's involved here, just specific XML tags")>]
  [<SuppressMessage("Microsoft.Design", "CA1011:ConsiderPassingBaseTypesAsParameters",
    Justification = "AvoidSpeculativeGenerality too")>]
  let PostProcess(document : XDocument) (ordinal: AltCover.Ordinal) =
    let orderAttr = ordinal.ToString().ToLowerInvariant()
    let scoreToString raw =
      (sprintf "%.2f" raw).TrimEnd([| '0' |]).TrimEnd([| '.' |])

    let stringToScore (node : XElement) name =
      node.Attribute(XName.Get name).Value
      |> (fun d -> Double.TryParse(d, NumberStyles.Number, CultureInfo.InvariantCulture))
      |> snd

    let percentCover visits points =
      if points = 0
      then "0"
      else ((float (visits * 100)) / (float points)) |> scoreToString

    let setSummary (x : XContainer) pointVisits branchVisits methodVisits classVisits
        ptcover brcover minCrap maxCrap =
      x.Descendants(XName.Get "Summary")
      |> Seq.tryHead
      |> Option.iter (fun s ->
            let minc =
              (if minCrap = Double.MaxValue then 0.0 else minCrap)
              |> scoreToString

            let maxc =
              (if maxCrap = Double.MinValue then 0.0 else maxCrap)
              |> scoreToString

            s.SetAttribute("visitedSequencePoints", sprintf "%d" pointVisits)
            s.SetAttribute("visitedBranchPoints", sprintf "%d" branchVisits)
            s.SetAttribute("visitedMethods", sprintf "%d" methodVisits)
            classVisits
            |> Option.iter
                (fun cvc -> s.SetAttribute("visitedClasses", sprintf "%d" cvc))
            s.SetAttribute("branchCoverage", brcover)
            s.SetAttribute("sequenceCoverage", ptcover)
            s.SetAttribute("minCrapScore", minc)
            s.SetAttribute("maxCrapScore", maxc))

    let computeBranchExitCount (doc : XDocument) (sp : XElement seq) bp =
      let tail = XElement(XName.Get "SequencePoint")
      tail.SetAttribute
        (orderAttr, Int32.MaxValue.ToString(CultureInfo.InvariantCulture))
      let nodes =
        List.concat
          [ sp
            |> Seq.toList
            [ tail ]
            bp
            |> Seq.toList ]

      let interleave =
        nodes
        |> Seq.sortBy (fun x ->
              x.Attribute(XName.Get orderAttr).Value
              |> Int32.TryParse
              |> snd)

      interleave
      |> Seq.fold (fun ((bev, bec), sq : XElement) x ->
            match x.Name.LocalName with
            | "SequencePoint" ->
                sq.SetAttribute("bec", sprintf "%d" bec)
                sq.SetAttribute("bev", sprintf "%d" bev)
                ((0, 0), x)
            | _ ->
                (((bev + (if x.Attribute(XName.Get "vc").Value = "0" then 0 else 1), bec + 1), sq)))
            ((0, 0), nodes.[0])
      |> ignore

    let crapScore (method : XElement) =
      let coverage =
        let cover = stringToScore method "sequenceCoverage"
        if cover > 0.0 then cover else stringToScore method "branchCoverage"

      let complexity = stringToScore method "cyclomaticComplexity"

      let raw =
        (Math.Pow(complexity, 2.0) * Math.Pow((1.0 - (coverage / 100.0)), 3.0)
          + complexity)
      let score = raw |> scoreToString
      method.SetAttribute("crapScore", score)
      raw

    let updateMethod (vb, vs, vm, pt, br, minc, maxc) (method : XElement) =
      let sp = method.Descendants(XName.Get "SequencePoint")
      let bp = method.Descendants(XName.Get "BranchPoint")
      let mp =
        method.Descendants(XName.Get "MethodPoint")
        |> Seq.toList
      let count = sp.Count()
      let rawCount = bp.Count()

      // inconsistent name to shut Gendarme up
      let numBranches = rawCount + Math.Sign(count + rawCount)
      if count > 0 then
        mp |> Seq.iter (fun m -> copyFillMethodPoint m sp)
      let pointVisits = visitCount sp
      let b0 = visitCount bp
      let branchVisits = b0 + Math.Sign b0
      if pointVisits > 0 || b0 > 0 then
        let fillMethod() =
          let cover = percentCover pointVisits count
          let bcover = percentCover branchVisits numBranches
          method.SetAttribute("visited", "true")
          method.SetAttribute("sequenceCoverage", cover)
          method.SetAttribute("branchCoverage", bcover)
          let raw = crapScore method
          setSummary method pointVisits branchVisits 1 None cover bcover raw raw
          computeBranchExitCount method.Document sp bp
          (vb + branchVisits, vs + pointVisits, vm + 1, pt + count, br + numBranches,
            Math.Min(minc, raw), Math.Max(maxc, raw))
        fillMethod()
      else
        (vb, vs, vm, pt + count, br + numBranches, minc, maxc)

    let updateClass (vb, vs, vm, vc, pt, br, minc0, maxc0) (``class`` : XElement) =
      let (cvb, cvs, cvm, cpt, cbr, minc, maxc) =
        ``class``.Descendants(XName.Get "Method")
        |> Seq.fold updateMethod
              (0, 0, 0, 0, 0, Double.MaxValue, Double.MinValue)

      let cover = percentCover cvs cpt
      let bcover = percentCover cvb cbr

      let cvc =
        if cvm > 0 then 1 else 0
      setSummary ``class`` cvs cvb cvm (Some cvc) cover bcover minc maxc
      (vb + cvb, vs + cvs, vm + cvm, vc + cvc, pt + cpt, br + cbr,
        Math.Min(minc, minc0), Math.Max(maxc, maxc0))

    let updateModule (vb, vs, vm, vc, pt, br, minc0, maxc0)
                     (``module`` : XElement) =
      let (cvb, cvs, cvm, cvc, cpt, cbr, minc, maxc) =
        ``module``.Descendants(XName.Get "Class")
        |> Seq.fold updateClass
              (0, 0, 0, 0, 0, 0, Double.MaxValue, Double.MinValue)

      let cover = percentCover cvs cpt
      let bcover = percentCover cvb cbr
      setSummary ``module`` cvs cvb cvm (Some cvc) cover bcover minc maxc
      (vb + cvb, vs + cvs, vm + cvm, vc + cvc, pt + cpt, br + cbr,
        Math.Min(minc, minc0), Math.Max(maxc, maxc0))

    let (vb, vs, vm, vc, pt, br, minc, maxc) =
      document.Descendants(XName.Get "Module")
      |> Seq.fold updateModule
            (0, 0, 0, 0, 0, 0, Double.MaxValue, Double.MinValue)

    let cover = percentCover vs pt
    let bcover = percentCover vb br
    setSummary document vs vb vm (Some vc) cover bcover minc maxc

  let internal summary() =
    XElement
      (XName.Get "Summary", XAttribute(XName.Get "numSequencePoints", 0),
        XAttribute(XName.Get "visitedSequencePoints", 0), XAttribute(XName.Get "numBranchPoints", 0),
        XAttribute(XName.Get "visitedBranchPoints", 0), XAttribute(XName.Get "sequenceCoverage", 0),
        XAttribute(XName.Get "branchCoverage", 0), XAttribute(XName.Get "maxCyclomaticComplexity", 0),
        XAttribute(XName.Get "minCyclomaticComplexity", 0), XAttribute(XName.Get "visitedClasses", 0),
        XAttribute(XName.Get "numClasses", 0), XAttribute(XName.Get "visitedMethods", 0),
        XAttribute(XName.Get "numMethods", 0), XAttribute(XName.Get "minCrapScore", 0),
        XAttribute(XName.Get "maxCrapScore", 0))

  let internal fixFormatMethod (m:XElement) (file:AssemblyDefinition) =
    // visited attribute <Method visited="false" cyclomaticComplexity="1" nPathComplexity="0" sequenceCoverage="0" branchCoverage="0" isConstructor="false" isStatic="true" isGetter="false" isSetter="false" crapScore="0">
    let a = m.Attributes() |> Seq.toList
    m.RemoveAttributes()
    let filterVisted = fun (x:XElement) -> x.Attribute(XName.Get "vc").Value <> "0"
    let seqpts = m.Descendants(XName.Get "SequencePoint") |> Seq.toList
    let brapts = m.Descendants(XName.Get "BranchPoint") |> Seq.toList
    let visited = Seq.concat [ seqpts
                               m.Descendants(XName.Get "MethodPoint") |> Seq.toList
                               brapts ]
                  |> Seq.exists filterVisted
    let boolString b =
      if b then "true" else "false"
    let v = XAttribute(XName.Get "visited", boolString visited)
    m.Add((v::a) |> List.toArray)

    // npath complexity
    let np = brapts
             |> List.groupBy (fun bp -> bp.Attribute(XName.Get "sl").Value)
             |> Seq.fold (fun np0 (_, b) -> OpenCover.safeMultiply (Seq.length b) np0) 1
    m.SetAttributeValue(XName.Get "nPathComplexity", np)

    let declaringTypeName = (m.AncestorsAndSelf(XName.Get "Class") |> Seq.head).Descendants(XName.Get "FullName")
                            |> Seq.head
    let declaringType = declaringTypeName.Value |> file.MainModule.GetType

    // value in method <MetadataToken>100663297</MetadataToken>
    let methodFullName = (m.Descendants(XName.Get "Name") |> Seq.head).Value
    let methodDef = declaringType.Methods
                    |> Seq.tryFind(fun n -> n.FullName = methodFullName)
    methodDef
    |> Option.iter(fun x -> let token = m.Descendants(XName.Get "MetadataToken") |> Seq.head
                            token.Value <- x.MetadataToken.ToUInt32().
                                             ToString(CultureInfo.InvariantCulture))
    // xsi:type in <MethodPoint xsi:type="SequencePoint" vc="0" uspid="0" ordinal="0" offset="2" sl="59" sc="16" el="59" ec="17" bec="0" bev="0" fileid="1" />
    //  instead of xmlns:p8="xsi" <MethodPoint vc="0" uspid="0" p8:type="SequencePoint" ordinal="0" offset="0" sc="0" sl="59" ec="1" el="59" bec="0" bev="0" fileid="1" xmlns:p8="xsi" />
    // Fix offset, sc, ec in <MethodPoint />
    let debugInfo = methodDef
                    |> Option.map (fun md -> md.DebugInformation)
                    |> Option.filter (fun dbg -> dbg |> isNull |> not)

    m.Descendants(XName.Get "MethodPoint")
    |> Seq.tryHead
    |> Option.iter (fun x -> let a = x.Attributes()
                                     |> Seq.filter (fun s -> s.Name.ToString().Contains("{") |> not)
                                     |> Seq.cast<obj>
                                     |> Seq.toArray
                             x.RemoveAttributes()
                             x.Add(XAttribute(XName.Get("type",
                                                        "http://www.w3.org/2001/XMLSchema-instance"),
                                                        "SequencePoint"))
                             x.Add a
                             debugInfo
                             |> Option.iter (fun dbg ->
                                               dbg.SequencePoints
                                               |> Seq.filter (fun s -> s.IsHidden |> not)
                                               |> Seq.tryHead
                                               |> Option.iter (fun s -> x.Attribute(XName.Get "sc").Value <- s.StartColumn.ToString(CultureInfo.InvariantCulture)
                                                                        x.Attribute(XName.Get "ec").Value <- s.EndColumn.ToString(CultureInfo.InvariantCulture)
                                                                        x.Attribute(XName.Get "offset").Value <- s.Offset.ToString(CultureInfo.InvariantCulture))))

    // Fix sequence points as best we can
    debugInfo
    |> Option.iter (fun d -> let sp = d.SequencePoints
                                      |> Seq.filter (fun s -> s.IsHidden |> not)
                                      |> Seq.toList

                             seqpts
                             |> List.fold (fun (dbgpts : Cil.SequencePoint list, offset : int) xmlpt ->
                                               let (ok, line) = Int32.TryParse(xmlpt.Attribute(XName.Get "sl").Value, NumberStyles.Integer, CultureInfo.InvariantCulture)
                                               let maybeDbg = dbgpts |> List.tryHead

                                               let filter (d:Cil.SequencePoint) = d.StartLine <= line
                                               let next = if (ok && maybeDbg
                                                                    |> Option.filter filter
                                                                    |> Option.isSome)
                                                          then let dbgline = maybeDbg |> Option.get
                                                               let offset' = dbgline.Offset
                                                               (dbgpts |> List.skipWhile filter, offset')
                                                          else (dbgpts, offset)

                                               let a = xmlpt.Attributes() |> Seq.toArray
                                               xmlpt.RemoveAttributes()
                                               xmlpt.Add(a |> Array.take 3)
                                               xmlpt.SetAttribute("offset", (snd next).ToString(CultureInfo.InvariantCulture))
                                               xmlpt.Add(a |> Array.skip 3)
                                               next) (sp, 0)
                             |> ignore

                             sp
                             |> Seq.map (fun s -> s.Document.Url)
                             |> Seq.tryFind File.Exists
                             |> Option.map File.ReadAllLines
                             |> Option.iter (fun f -> seqpts
                                                      |> Seq.iter (fun point ->
                                                                       let (ok, index) = Int32.TryParse(point.Attribute(XName.Get "sl").Value, NumberStyles.Integer, CultureInfo.InvariantCulture)
                                                                       if ok then
                                                                         let line = f.[index - 1]
                                                                         let cols = line.Length + 1
                                                                         point.SetAttribute("ec", cols.ToString(CultureInfo.InvariantCulture))
                                                                         point.SetAttribute("sc", (cols - line.TrimStart().Length).ToString(CultureInfo.InvariantCulture))

                                                      )
                             )
    )

  let private hash = new System.Security.Cryptography.SHA1CryptoServiceProvider()

  let internal fixFormatModule (m:XElement) (files:string array) =
    // supply empty module level  <Summary numSequencePoints="0" visitedSequencePoints="0" numBranchPoints="0" visitedBranchPoints="0" sequenceCoverage="0" branchCoverage="0" maxCyclomaticComplexity="0" minCyclomaticComplexity="0" visitedClasses="0" numClasses="0" visitedMethods="0" numMethods="0" minCrapScore="0" maxCrapScore="0" />
    (m.Elements() |> Seq.head).AddBeforeSelf(summary())
    let modulePath = m.Element(XName.Get "ModulePath")
    let assemblyFileName = modulePath.Value
    let assemblyPath = files |> Seq.tryFind (fun f -> assemblyFileName = Path.GetFileName f)
    let hashFile sPath =
      use stream = File.OpenRead sPath
      stream
      |> hash.ComputeHash
      |> BitConverter.ToString

    assemblyPath
    |> Option.filter File.Exists
    // fill in path  <ModulePath>...\Sample4.dll</ModulePath>
    // fix hash in <Module hash="42-08-CA-1A-A6-25-CE-DA-DD-18-DC-D5-9A-BF-1B-BF-00-1D-E5-9B">
    // visited attribute <Method visited="false" cyclomaticComplexity="1" nPathComplexity="0" sequenceCoverage="0" branchCoverage="0" isConstructor="false" isStatic="true" isGetter="false" isSetter="false" crapScore="0">
    // value in method <MetadataToken>100663297</MetadataToken>
    // xsi:type in <MethodPoint xsi:type="SequencePoint" vc="0" uspid="0" ordinal="0" offset="2" sl="59" sc="16" el="59" ec="17" bec="0" bev="0" fileid="1" />
    //  instead of xmlns:p8="xsi" <MethodPoint vc="0" uspid="0" p8:type="SequencePoint" ordinal="0" offset="0" sc="0" sl="59" ec="1" el="59" bec="0" bev="0" fileid="1" xmlns:p8="xsi" />
    // Fix offset, sc, ec in <MethodPoint />
    |> Option.iter(fun path -> modulePath.Value <- (Path.GetFullPath path)
                               m.Attribute(XName.Get "hash").Value <- hashFile path
                               use def = AssemblyDefinition.ReadAssembly path
                               def.MainModule.ReadSymbols()
                               m.Descendants(XName.Get "Method")
                               |> Seq.iter (fun m2 -> fixFormatMethod m2 def))

  /// <summary>
  /// <para type="synopsis">Fills in gaps in `coverlet``'s OpenCover dialect.</para>
  /// <para type="description">Adds summary data and other items to report in ``coverlet``'s OpenCover dialect, particularly giving somewhat meaningful start and end column values for its line-based paradigm, as well as npath coverage and branch exits.</para>
  /// </summary>
  /// <param name="document">The input report</param>
  /// <returns>The filled-in report</returns>
  let FormatFromCoverlet (report:XDocument) (files:string array) =
    let rewrite = XDocument(report)

    // attributes in <CoverageSession xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">

    rewrite.Descendants(XName.Get "CoverageSession")
    |> Seq.iter(fun session -> session.RemoveAttributes()
                               session.Add(XAttribute(XNamespace.Xmlns + "xsd",
                                            "http://www.w3.org/2001/XMLSchema"),
                                           XAttribute(XNamespace.Xmlns + "xsi",
                                            "http://www.w3.org/2001/XMLSchema-instance")))

    // supply empty module level  <Summary numSequencePoints="0" visitedSequencePoints="0" numBranchPoints="0" visitedBranchPoints="0" sequenceCoverage="0" branchCoverage="0" maxCyclomaticComplexity="0" minCyclomaticComplexity="0" visitedClasses="0" numClasses="0" visitedMethods="0" numMethods="0" minCrapScore="0" maxCrapScore="0" />
    // fill in path  <ModulePath>...\Sample4.dll</ModulePath>
    // visited attribute <Method visited="false" cyclomaticComplexity="1" nPathComplexity="0" sequenceCoverage="0" branchCoverage="0" isConstructor="false" isStatic="true" isGetter="false" isSetter="false" crapScore="0">
    // value in method <MetadataToken>100663297</MetadataToken>
    // xsi:type in <MethodPoint xsi:type="SequencePoint" vc="0" uspid="0" ordinal="0" offset="2" sl="59" sc="16" el="59" ec="17" bec="0" bev="0" fileid="1" />
    //  instead of xmlns:p8="xsi" <MethodPoint vc="0" uspid="0" p8:type="SequencePoint" ordinal="0" offset="0" sc="0" sl="59" ec="1" el="59" bec="0" bev="0" fileid="1" xmlns:p8="xsi" />
    // Fix offset, sc, ec in <MethodPoint />
    rewrite.Descendants(XName.Get "Module")
    |> Seq.iter (fun m -> fixFormatModule m files)

    // TODO list
    // Fix offset, sc, ec in <SequencePoint vc="1" uspid="1" ordinal="0" offset="0" sl="47" sc="21" el="47" ec="26" bec="0" bev="0" fileid="1" />

    PostProcess rewrite Ordinal.SL

    rewrite

  /// <summary>
  /// <para type="synopsis">Removes compiler-generated hidden branches from OpenCover.</para>
  /// <para type="description">Takes output from the OpenCover program, and adjust from OpenCover's liberal idea of significant branches towards AltCover's more restricted approach -- chose either or both of `sameSpan` to unify branches that go from the same start, and take the same trajectory to the same end (OpenCover issue #786 being one instance of this) and `withinSequencePoint` to remove branches interior to a statement (compiler generated things like stashing of lambdas, the hidden conditional `Dispose()` after a `using`, or inside F# inlines -- OpenCover issues #657, #807 being instances of this).</para>
  /// <para type="description">Either takes an `XDocument` from the pipeline or from a file; emits the result as an `XDocument` to the pipeline and optionally to a file.</para>
  /// </summary>
  /// <param name="document">The input report</param>
  /// <param name="withinSequencePoint">Whether to hide branches that terminatwe inside the same sequence point as they begin</param>
  /// <param name="sameSpan">Whether to treat branches between the same points as being the same branch</param>
  /// <returns>The filled-in report</returns>
  let CompressBranching (document : XDocument) withinSequencePoint sameSpan =
    // Validate
    let xmlDocument = new XDocument(document)
    let schemas = XmlUtilities.loadSchema AltCover.Base.ReportFormat.OpenCover
    xmlDocument.Validate(schemas, null)
    // Get all the methods
    xmlDocument.Descendants(XName.Get "Method")
    |> Seq.iter (compressMethod withinSequencePoint sameSpan)
    // tidy up here
    PostProcess xmlDocument Ordinal.Offset
    xmlDocument

[<assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
  MessageId="Api", Justification="It's an API, damn it.")>]
[<assembly: SuppressMessage("Microsoft.Performance", "CA1810:InitializeReferenceTypeStaticFieldsInline",
  Scope="member", Target="<StartupCode$AltCover-FSApi>.$OpenCover.#.cctor()",
  Justification="Compiler Generated")>]
[<assembly: SuppressMessage("Microsoft.Performance", "CA1823:AvoidUnusedPrivateFields",
  Scope="member", Target="AltCover.OpenCoverUtilities+setSummary@159D.#scoreToString",
  Justification="Compiler Generated")>]
()