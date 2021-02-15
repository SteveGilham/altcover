namespace AltCover

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.Globalization
open System.IO
open System.Linq
open System.Xml.Linq
open System.Xml.Schema

open Mono.Cecil
open AltCover.XmlExtensions

[<RequireQualifiedAccess>]
module OpenCover =

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

  [<SuppressMessage("Microsoft.Globalization", "CA1308:NormalizeStringsToUppercase",
      Justification="No Turkish I's involved here, just specific XML tags")>]
  [<SuppressMessage("Microsoft.Design", "CA1011:ConsiderPassingBaseTypesAsParameters",
    Justification = "AvoidSpeculativeGenerality too")>]
  let PostProcess(document : XDocument) (ordinal: BranchOrdinal) =
    let orderAttr = ordinal.ToString().ToLowerInvariant()
    let counts = Dictionary<string, Dictionary<int, PointVisit>>()
    PostProcess.action orderAttr counts ReportFormat.OpenCover (XmlAbstraction.XDoc document)

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
             |> Seq.fold (fun np0 (_, b) -> AltCover.OpenCover.safeMultiply (Seq.length b) np0) 1
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
                    |> Option.filter (isNull >> not)

    m.Descendants(XName.Get "MethodPoint")
    |> Seq.tryHead
    |> Option.iter (fun x -> let a = x.Attributes()
                                     |> Seq.filter (fun s -> charIndexOf (s.Name.ToString()) '{' < 0)
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

    PostProcess rewrite BranchOrdinal.SL

    rewrite

  let CompressBranching (document : XDocument) withinSequencePoint sameSpan =
    // Validate
    let xmlDocument = XDocument(document)
    let schemas = XmlUtilities.loadSchema AltCover.ReportFormat.OpenCover
    xmlDocument.Validate(schemas, null)
    // Get all the methods
    xmlDocument.Descendants(XName.Get "Method")
    |> Seq.iter (compressMethod withinSequencePoint sameSpan)
    // tidy up here
    PostProcess xmlDocument BranchOrdinal.Offset
    xmlDocument

  let JsonToXml (document:string) =
    document |> NativeJson.fromJsonText |> NativeJson.jsonToXml  |> NativeJson.orderXml

[<assembly: SuppressMessage("Microsoft.Performance", "CA1810:InitializeReferenceTypeStaticFieldsInline",
  Scope="member", Target="<StartupCode$AltCover-Toolkit>.$OpenCover.#.cctor()",
  Justification="Compiler Generated")>]
()