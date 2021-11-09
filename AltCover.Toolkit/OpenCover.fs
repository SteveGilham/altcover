namespace AltCover

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.Globalization
open System.IO
open System.Xml.Linq
open System.Xml.Schema

open Mono.Cecil
open AltCover.XmlExtensions

[<RequireQualifiedAccess>]
module OpenCover =

  [<SuppressMessage("Gendarme.Rules.Maintainability",
                    "AvoidUnnecessarySpecializationRule",
                    Justification = "AvoidSpeculativeGenerality too")>]
  let private compressMethod withinSequencePoint sameSpan (m: XElement) =
    let sp =
      m.Descendants(XName.Get "SequencePoint")
      |> Seq.toList

    let bp =
      m.Descendants(XName.Get "BranchPoint")
      |> Seq.toList

    if sp |> List.isEmpty |> not
       && bp |> List.isEmpty |> not then
      let tail = XElement(XName.Get "SequencePoint")

      tail.Add(
        XAttribute(
          XName.Get "offset",
          Int32.MaxValue.ToString(CultureInfo.InvariantCulture)
        )
      )

      let interleave =
        List.concat [ sp; bp; [ tail ] ]
        |> List.sortBy
             (fun x ->
               x.Attribute(XName.Get "offset").Value
               |> Int32.TryParse
               |> snd)

      interleave
      |> Seq.fold
           (fun (s: XElement, bs: XElement list) x ->
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
                       |> List.partition
                            (fun b ->
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
                       |> List.fold
                            (fun (ki, ke) (_, bz) ->
                              let totalVisits =
                                bz
                                |> Seq.sumBy
                                     (fun b ->
                                       b.Attribute(XName.Get "vc").Value
                                       |> Int32.TryParse
                                       |> snd)

                              let h = bz |> Seq.head

                              h.SetAttribute(
                                "vc",
                                totalVisits.ToString(CultureInfo.InvariantCulture)
                              )

                              (List.concat [ ki
                                             bz |> Seq.tail |> Seq.toList ],
                               h :: ke))
                            ([], [])

                     kill |> Seq.iter (fun b -> b.Remove())
                     keep
                   else
                     bx

                 // Fix up what remains
                 by
                 |> List.rev // because the list will have been built up in reverse order
                 |> Seq.mapi (fun i b -> (i, b))
                 |> Seq.groupBy (fun (_, b) -> b.Attribute(XName.Get "offset").Value)
                 |> Seq.iter
                      (fun (_, paths) ->
                        paths // assume likely ranges for these numbers!
                        |> Seq.sortBy
                             (fun (n, p) ->
                               n
                               + 100
                                 * (p.Attribute(XName.Get "offsetend").Value
                                    |> Int32.TryParse
                                    |> snd))
                        |> Seq.iteri
                             (fun i (_, p) ->
                               p.SetAttribute(
                                 "path",
                                 (i + 1).ToString(CultureInfo.InvariantCulture)
                               )))

                 s.SetAttribute("bec", by.Length.ToString(CultureInfo.InvariantCulture))
                 s.SetAttribute("bev", "0")
                 (x, [])
             | _ -> (s, x :: bs))
           (sp.Head, [])
      |> ignore

  [<SuppressMessage("Microsoft.Globalization",
                    "CA1308:NormalizeStringsToUppercase",
                    Justification = "No Turkish I's involved here, just specific XML tags")>]
  [<SuppressMessage("Microsoft.Design",
                    "CA1011:ConsiderPassingBaseTypesAsParameters",
                    Justification = "AvoidSpeculativeGenerality too")>]
  let PostProcess (document: XDocument) (ordinal: BranchOrdinal) =
    let orderAttr = ordinal.ToString().ToLowerInvariant()

    let counts =
      Dictionary<string, Dictionary<int, PointVisit>>()

    PostProcess.action
      orderAttr
      counts
      ReportFormat.OpenCover
      (XmlAbstraction.XDoc document)

  let internal summary () =
    XElement(
      XName.Get "Summary",
      XAttribute(XName.Get "numSequencePoints", 0),
      XAttribute(XName.Get "visitedSequencePoints", 0),
      XAttribute(XName.Get "numBranchPoints", 0),
      XAttribute(XName.Get "visitedBranchPoints", 0),
      XAttribute(XName.Get "sequenceCoverage", 0),
      XAttribute(XName.Get "branchCoverage", 0),
      XAttribute(XName.Get "maxCyclomaticComplexity", 0),
      XAttribute(XName.Get "minCyclomaticComplexity", 0),
      XAttribute(XName.Get "visitedClasses", 0),
      XAttribute(XName.Get "numClasses", 0),
      XAttribute(XName.Get "visitedMethods", 0),
      XAttribute(XName.Get "numMethods", 0),
      XAttribute(XName.Get "minCrapScore", 0),
      XAttribute(XName.Get "maxCrapScore", 0)
    )

  let internal fixFormatMethod (m: XElement) (file: AssemblyDefinition) =
    // visited attribute <Method visited="false" cyclomaticComplexity="1" nPathComplexity="0" sequenceCoverage="0" branchCoverage="0" isConstructor="false" isStatic="true" isGetter="false" isSetter="false" crapScore="0">
    let a = m.Attributes() |> Seq.toList
    m.RemoveAttributes()

    let filterVisted =
      fun (x: XElement) -> x.Attribute(XName.Get "vc").Value <> "0"

    let seqpts =
      m.Descendants(XName.Get "SequencePoint")
      |> Seq.toList

    let brapts =
      m.Descendants(XName.Get "BranchPoint")
      |> Seq.toList

    let visited =
      Seq.concat [ seqpts
                   m.Descendants(XName.Get "MethodPoint")
                   |> Seq.toList
                   brapts ]
      |> Seq.exists filterVisted

    let boolString b = if b then "true" else "false"

    let v =
      XAttribute(XName.Get "visited", boolString visited)

    m.Add((v :: a) |> List.toArray)

    // npath complexity
    let np =
      brapts
      |> List.groupBy (fun bp -> bp.Attribute(XName.Get "sl").Value)
      |> Seq.fold (fun np0 (_, b) -> AltCover.OpenCover.safeMultiply (Seq.length b) np0) 1

    m.SetAttributeValue(XName.Get "nPathComplexity", np)

    let declaringTypeName =
      (m.AncestorsAndSelf(XName.Get "Class") |> Seq.head)
        .Descendants(XName.Get "FullName")
      |> Seq.head

    let declaringType =
      declaringTypeName.Value |> file.MainModule.GetType

    // value in method <MetadataToken>100663297</MetadataToken>
    let methodFullName =
      (m.Descendants(XName.Get "Name") |> Seq.head)
        .Value

    let methodDef =
      declaringType.Methods
      |> Seq.tryFind (fun n -> n.FullName = methodFullName)

    methodDef
    |> Option.iter
         (fun x ->
           let token =
             m.Descendants(XName.Get "MetadataToken")
             |> Seq.head

           token.Value <-
             x
               .MetadataToken
               .ToUInt32()
               .ToString(CultureInfo.InvariantCulture))
    // xsi:type in <MethodPoint xsi:type="SequencePoint" vc="0" uspid="0" ordinal="0" offset="2" sl="59" sc="16" el="59" ec="17" bec="0" bev="0" fileid="1" />
    //  instead of xmlns:p8="xsi" <MethodPoint vc="0" uspid="0" p8:type="SequencePoint" ordinal="0" offset="0" sc="0" sl="59" ec="1" el="59" bec="0" bev="0" fileid="1" xmlns:p8="xsi" />
    // Fix offset, sc, ec in <MethodPoint />
    let debugInfo =
      methodDef
      |> Option.map (fun md -> md.DebugInformation)
      |> Option.filter (isNull >> not)

    m.Descendants(XName.Get "MethodPoint")
    |> Seq.tryHead
    |> Option.iter
         (fun x ->
           let a =
             x.Attributes()
             |> Seq.filter (fun s -> charIndexOf (s.Name.ToString()) '{' < 0)
             |> Seq.cast<obj>
             |> Seq.toArray

           x.RemoveAttributes()

           x.Add(
             XAttribute(
               XName.Get("type", "http://www.w3.org/2001/XMLSchema-instance"),
               "SequencePoint"
             )
           )

           x.Add a

           debugInfo
           |> Option.iter
                (fun dbg ->
                  dbg.SequencePoints
                  |> Seq.filter (fun s -> s.IsHidden |> not)
                  |> Seq.tryHead
                  |> Option.iter
                       (fun s ->
                         x.Attribute(XName.Get "sc").Value <- s.StartColumn.ToString(
                           CultureInfo.InvariantCulture
                         )
                         x.Attribute(XName.Get "ec").Value <- s.EndColumn.ToString(
                           CultureInfo.InvariantCulture
                         )
                         x.Attribute(XName.Get "offset").Value <- s.Offset.ToString(
                           CultureInfo.InvariantCulture
                         ))))

    // Fix sequence points as best we can
    debugInfo
    |> Option.iter
         (fun d ->
           let sp =
             d.SequencePoints
             |> Seq.filter (fun s -> s.IsHidden |> not)
             |> Seq.toList

           seqpts
           |> List.fold
                (fun (dbgpts: Cil.SequencePoint list, offset: int) xmlpt ->
                  let (ok, line) =
                    Int32.TryParse(
                      xmlpt.Attribute(XName.Get "sl").Value,
                      NumberStyles.Integer,
                      CultureInfo.InvariantCulture
                    )

                  let maybeDbg = dbgpts |> List.tryHead

                  let filter (d: Cil.SequencePoint) = d.StartLine <= line

                  let next =
                    if (ok
                        && maybeDbg |> Option.filter filter |> Option.isSome) then
                      let dbgline = maybeDbg |> Option.get
                      let offset' = dbgline.Offset
                      (dbgpts |> List.skipWhile filter, offset')
                    else
                      (dbgpts, offset)

                  let a = xmlpt.Attributes() |> Seq.toArray
                  xmlpt.RemoveAttributes()
                  xmlpt.Add(a |> Array.take 3)

                  xmlpt.SetAttribute(
                    "offset",
                    (snd next).ToString(CultureInfo.InvariantCulture)
                  )

                  xmlpt.Add(a |> Array.skip 3)
                  next)
                (sp, 0)
           |> ignore

           sp
           |> Seq.map (fun s -> s.Document.Url)
           |> Seq.tryFind File.Exists
           |> Option.map File.ReadAllLines
           |> Option.iter
                (fun f ->
                  seqpts
                  |> Seq.iter
                       (fun point ->
                         let (ok, index) =
                           Int32.TryParse(
                             point.Attribute(XName.Get "sl").Value,
                             NumberStyles.Integer,
                             CultureInfo.InvariantCulture
                           )

                         if ok then
                           let line = f.[index - 1]
                           let cols = line.Length + 1

                           point.SetAttribute(
                             "ec",
                             cols.ToString(CultureInfo.InvariantCulture)
                           )

                           point.SetAttribute(
                             "sc",
                             (cols - line.TrimStart().Length)
                               .ToString(CultureInfo.InvariantCulture)
                           )

                         )))

  let private hash =
    sha1Hash()

  let internal fixFormatModule (m: XElement) (files: string array) =
    // supply empty module level  <Summary numSequencePoints="0" visitedSequencePoints="0" numBranchPoints="0" visitedBranchPoints="0" sequenceCoverage="0" branchCoverage="0" maxCyclomaticComplexity="0" minCyclomaticComplexity="0" visitedClasses="0" numClasses="0" visitedMethods="0" numMethods="0" minCrapScore="0" maxCrapScore="0" />
    (m.Elements() |> Seq.head)
      .AddBeforeSelf(summary ())

    let modulePath = m.Element(XName.Get "ModulePath")
    let assemblyFileName = modulePath.Value

    let assemblyPath =
      files
      |> Seq.tryFind (fun f -> assemblyFileName = Path.GetFileName f)

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
    |> Option.iter
         (fun path ->
           modulePath.Value <- (Path.GetFullPath path)
           m.Attribute(XName.Get "hash").Value <- hashFile path

           use def = AssemblyDefinition.ReadAssembly path
           def.MainModule.ReadSymbols()

           m.Descendants(XName.Get "Method")
           |> Seq.iter (fun m2 -> fixFormatMethod m2 def))

  let FormatFromCoverlet (report: XDocument) (files: string array) =
    let rewrite = XDocument(report)

    // attributes in <CoverageSession xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">

    rewrite.Descendants(XName.Get "CoverageSession")
    |> Seq.iter
         (fun session ->
           session.RemoveAttributes()

           session.Add(
             XAttribute(XNamespace.Xmlns + "xsd", "http://www.w3.org/2001/XMLSchema"),
             XAttribute(
               XNamespace.Xmlns + "xsi",
               "http://www.w3.org/2001/XMLSchema-instance"
             )
           ))

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

  let CompressBranching (document: XDocument) withinSequencePoint sameSpan =
    // Validate
    let xmlDocument = XDocument(document)

    let schemas =
      XmlUtilities.loadSchema AltCover.ReportFormat.OpenCover

    xmlDocument.Validate(schemas, null)
    // Get all the methods
    xmlDocument.Descendants(XName.Get "Method")
    |> Seq.iter (compressMethod withinSequencePoint sameSpan)
    // tidy up here
    PostProcess xmlDocument BranchOrdinal.Offset
    xmlDocument

  //-----------------------------------------------------------------------------------------

  let blankOpenCover () =
    use reader = // fsharplint:disable-next-line  RedundantNewKeyword
      new StringReader(
        """<CoverageSession xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"><Summary numSequencePoints="?" visitedSequencePoints="0" numBranchPoints="?" visitedBranchPoints="0" sequenceCoverage="0" branchCoverage="0" maxCyclomaticComplexity="0" minCyclomaticComplexity="0" visitedClasses="0" numClasses="?" visitedMethods="0" numMethods="?" minCrapScore="0" maxCrapScore="0" /><Modules /></CoverageSession>"""
      )

    XDocument.Load(reader)

  [<NoComparison; NoEquality>]
  type TrackedMethod =
    { Hash: string
      Token: string
      Name: string
      Strategy: string list
      Entry: string list
      Exit: string list
      Uid: int }

  let percent v n =
    if n = 0 then
      "0"
    else
      let v1 = v |> float
      let n1 = n |> float

      (v1 * 100.0 / n1)
        .ToString("F2", CultureInfo.InvariantCulture)

  [<NoEquality; NoComparison; ExcludeFromCodeCoverage>]
  type Summary =
    { NumSequencePoints: int
      VisitedSequencePoints: int
      NumBranchPoints: int
      VisitedBranchPoints: int
      MaxCyclomaticComplexity: int
      MinCyclomaticComplexity: int Option
      VisitedClasses: int
      NumClasses: int
      VisitedMethods: int
      NumMethods: int
      MinCrapScore: float Option
      MaxCrapScore: float }
    member this.SequenceCoverage =
      percent this.VisitedSequencePoints this.NumSequencePoints

    member this.BranchCoverage =
      percent this.VisitedBranchPoints this.NumBranchPoints

    member this.Xml =
      XElement(
        XName.Get "Summary",
        XAttribute(XName.Get "numSequencePoints", this.NumSequencePoints),
        XAttribute(XName.Get "visitedSequencePoints", this.VisitedSequencePoints),
        XAttribute(XName.Get "numBranchPoints", this.NumBranchPoints),
        XAttribute(XName.Get "visitedBranchPoints", this.VisitedBranchPoints),
        XAttribute(XName.Get "sequenceCoverage", this.SequenceCoverage),
        XAttribute(XName.Get "branchCoverage", this.BranchCoverage),
        XAttribute(XName.Get "maxCyclomaticComplexity", this.MaxCyclomaticComplexity),
        XAttribute(
          XName.Get "minCyclomaticComplexity",
          (Option.defaultValue 1 this.MinCyclomaticComplexity)
        ),
        XAttribute(XName.Get "visitedClasses", this.VisitedClasses),
        XAttribute(XName.Get "numClasses", this.NumClasses),
        XAttribute(XName.Get "visitedMethods", this.VisitedMethods),
        XAttribute(XName.Get "numMethods", this.NumMethods),
        XAttribute(
          XName.Get "minCrapScore",
          (Option.defaultValue 1.0 this.MinCrapScore)
            .ToString("F2", CultureInfo.InvariantCulture)
        ),
        XAttribute(
          XName.Get "maxCrapScore",
          this.MaxCrapScore.ToString("F2", CultureInfo.InvariantCulture)
        )
      )

    member this.Add(other: Summary) =
      let maybeMin a b compare =
        match (a, b) with
        | (None, _) -> b
        | (_, None) -> a
        | (Some aa, Some bb) -> compare (aa, bb) |> Some

      { NumSequencePoints = this.NumSequencePoints + other.NumSequencePoints
        VisitedSequencePoints =
          this.VisitedSequencePoints
          + other.VisitedSequencePoints
        NumBranchPoints = this.NumBranchPoints + other.NumBranchPoints
        VisitedBranchPoints =
          this.VisitedBranchPoints
          + other.VisitedBranchPoints
        MaxCyclomaticComplexity =
          Math.Max(this.MaxCyclomaticComplexity, other.MaxCyclomaticComplexity)
        MinCyclomaticComplexity =
          maybeMin this.MinCyclomaticComplexity other.MinCyclomaticComplexity Math.Min
        VisitedClasses = this.VisitedClasses + other.VisitedClasses
        NumClasses = this.NumClasses + other.NumClasses
        VisitedMethods = this.VisitedMethods + other.VisitedMethods
        NumMethods = this.NumMethods + other.NumMethods
        MinCrapScore = maybeMin other.MinCrapScore this.MinCrapScore Math.Min
        MaxCrapScore = Math.Max(this.MaxCrapScore, other.MaxCrapScore) }

    static member Create() =
      { NumSequencePoints = 0
        VisitedSequencePoints = 0
        NumBranchPoints = 0
        VisitedBranchPoints = 0
        MaxCyclomaticComplexity = 0
        MinCyclomaticComplexity = None
        VisitedClasses = 0
        NumClasses = 0
        VisitedMethods = 0
        NumMethods = 0
        MinCrapScore = None
        MaxCrapScore = 0.0 }

  let attributeOrEmpty name (x: XElement) =
    x.Attribute(XName.Get name)
    |> Option.ofObj
    |> Option.map (fun m -> m.Value)
    |> Option.defaultValue String.Empty

  let mergeFiles (files: XElement seq) =
    files
    |> Seq.map (fun x -> x.Attribute(XName.Get "fullPath").Value)
    |> Seq.distinct
    |> Seq.mapi (fun i name -> name, (i + 1))
    |> Map.ofSeq

  let mergeCounts (records: XElement seq) =
    let mmin, total =
      records
      |> Seq.fold
           (fun (low, total) item ->
             let vc =
               item
               |> (attributeOrEmpty "vc")
               |> Int32.TryParse
               |> snd

             (Math.Min(vc, low), total + Math.Max(vc, 0)))
           (0, 0)

    [ total; mmin ]
    |> List.filter (fun n -> n <> 0)
    |> List.tryHead
    |> Option.defaultValue 0

  [<SuppressMessage("Gendarme.Rules.Maintainability",
                    "AvoidUnnecessarySpecializationRule",
                    Justification = "AvoidSpeculativeGenerality too")>]
  let mapFile (files: Map<string, int>) (a: XAttribute) (modu: XElement) =
    let oldfile = a.Value

    let source =
      modu.Descendants(XName.Get "File")
      |> Seq.find (fun f -> f.Attribute(XName.Get "uid").Value = oldfile)

    Map.find (source.Attribute(XName.Get "fullPath").Value) files

  //  hash, token
  let mapTracking (tracked: Map<string * string, TrackedMethod>) (x: XElement) =
    let key = attributeOrEmpty "uid" x

    let tm =
      x.Ancestors(XName.Get "CoverageSession")
      |> Seq.collect (fun c -> c.Descendants(XName.Get "TrackedMethod"))
      |> Seq.find (fun t -> (attributeOrEmpty "uid" t) = key)

    let token = attributeOrEmpty "token" tm
    let hash = attributeOrEmpty "hash" tm.Parent.Parent
    let tm = Map.find (hash, token) tracked
    let result = XElement(x)
    result.SetAttributeValue(XName.Get "uid", tm.Uid)
    result

  let fixPoint files modu tracked (group: XElement seq) =
    let r = group |> Seq.head
    let mc = group |> mergeCounts
    let np0 = XElement r
    np0.SetAttributeValue(XName.Get "vc", mc)

    group
    |> Seq.map (fun x -> x.Attribute(XName.Get "fileid"))
    |> Seq.filter (isNull >> not)
    |> Seq.tryHead
    |> Option.iter
         (fun a ->
           let newfile = mapFile files a modu
           np0.SetAttributeValue(XName.Get "fileid", newfile))

    let times =
      group
      |> Seq.collect (fun p -> p.Descendants(XName.Get "Time"))
      |> Seq.groupBy (attributeOrEmpty "time")
      |> Seq.map (fun (t, x) -> t, x |> mergeCounts)
      |> Seq.filter (fun (_, x) -> x > 0)
      |> Seq.map
           (fun (t, x) ->
             XElement(
               XName.Get "Time",
               XAttribute(XName.Get "time", t),
               XAttribute(XName.Get "vc", x)
             ))

    np0.Elements(XName.Get "Times")
    |> Seq.toList
    |> Seq.iter (fun t -> t.Remove())

    let tracks =
      group
      |> Seq.collect (fun p -> p.Descendants(XName.Get "TrackedMethodRef"))
      |> Seq.map (mapTracking tracked)
      |> Seq.groupBy (attributeOrEmpty "uid")
      |> Seq.map (fun (t, x) -> t, x |> mergeCounts)
      |> Seq.filter (fun (_, x) -> x > 0)
      |> Seq.map
           (fun (t, x) ->
             XElement(
               XName.Get "TrackedMethodRef",
               XAttribute(XName.Get "uid", t),
               XAttribute(XName.Get "vc", x)
             ))

    np0.Elements(XName.Get "TrackedMethodRefs")
    |> Seq.toList
    |> Seq.iter (fun t -> t.Remove())

    if times |> Seq.isEmpty |> not then
      let t = XElement(XName.Get "Times")
      times |> Seq.toArray |> t.Add
      np0.Add t

    if tracks |> Seq.isEmpty |> not then
      let t = XElement(XName.Get "TrackedMethodRefs")
      times |> Seq.toArray |> t.Add
      np0.Add t

    (mc, np0)
  (*
OpenCover on Tests.AltCoverRunnerTests/PostprocessShouldRestoreDegenerateOpenCoverState@2619-4::Invoke
<SequencePoint vc="5" uspid="10914" ordinal="6" offset="96" sl="2626" sc="12" el="2629" ec="16" bec="2" bev="1" fileid="876" />
<BranchPoint vc="5" uspid="10917" ordinal="0" offset="116" sl="2626" path="0" offsetchain="118" offsetend="122" fileid="876" />
AltCover on ditto
<SequencePoint vc="5" uspid="8578" ordinal="6" offset="96" sl="2626" sc="12" el="2629" ec="16" bec="2" bev="1" fileid="37" />
<BranchPoint vc="5" uspid="1687" ordinal="0" offset="116" sl="2626" path="0" offsetchain="118" offsetend="122" fileid="37" />
coverlet on Tests.AltCoverRunnerTests/PostprocessShouldRestoreDegenerateOpenCoverState@2619-4::Invoke -- empty class
coverlet on Tests.AltCoverRunnerTests/PostprocessShouldRestoreDegenerateOpenCoverState
<SequencePoint vc="5" uspid="2626" ordinal="41" sl="2626" sc="1" el="2626" ec="2" bec="0" bev="0" fileid="37" /> (flattened, lambda branch lost)
<BranchPoint vc="1" uspid="2612" ordinal="3" path="1" offset="301" offsetend="338" sl="2612" fileid="37" />(sample branch)
*)

  let mergePoints files modu tracked (sps: ('a * XElement seq) seq) =
    let (vc, np) =
      sps
      |> Seq.fold
           (fun (count, points) group ->
             let g = group |> snd
             let (mc, np0) = fixPoint files modu tracked g
             (count + if mc > 0 then 1 else 0), np0 :: points)
           (0, [])

    (vc, np |> Seq.toArray)

  let branchEntryHeuristic newsps newbps =
    let lines =
      newsps
      |> Seq.groupBy (attributeOrEmpty "sl")
      |> Seq.map
           (fun (l, x) ->
             l |> Int32.TryParse |> snd,
             x
             |> Seq.sortBy ((attributeOrEmpty "sc") >> Int32.TryParse >> snd)
             |> Seq.head)

    let branches =
      newbps
      |> Seq.map
           (fun b ->
             (b
              |> (attributeOrEmpty "sl")
              |> Int32.TryParse
              |> snd),
             b)

    let interleave =
      [ lines; branches ]
      |> Seq.concat
      |> Seq.sortBy
           (fun (n, x) ->
             (n <<< 1)
             + (if x.Name.LocalName = "SequencePoint" then
                  0
                else
                  1))
      |> Seq.map snd
      |> Seq.toList

    interleave
    |> Seq.fold
         (fun (bev, sq: XElement) x ->
           match x.Name.LocalName with
           | "SequencePoint" ->
               sq.SetAttributeValue(XName.Get "bev", bev)
               (0, x)
           | _ ->
               let visited =
                 attributeOrEmpty "vc" x |> Int32.TryParse |> snd

               let delta = if visited > 0 then 1 else 0
               (bev + delta, sq))
         (0, interleave |> Seq.head)
    |> ignore

  let mergeMethods
    (files: Map<string, int>)
    (tracked: Map<string * string, TrackedMethod>)
    (methods: (string * XElement seq) seq)
    : (Summary * XElement array) =
    let metadataToken (group: string * XElement seq) =
      let mt = XElement(XName.Get "MetadataToken")
      group
      |> snd
      |> Seq.map (fun m -> m.Element(XName.Get "MetadataToken").Value)
      |> Seq.tryFind (String.IsNullOrWhiteSpace >> not)
      |> Option.iter (fun t -> mt.Value <- t)
      mt

    let fileRef modu (group: string * XElement seq) =
        group
        |> snd
        |> Seq.collect (fun x -> x.Elements(XName.Get "FileRef"))
        |> Seq.map (fun f -> f.Attribute(XName.Get "uid"))
        |> Seq.filter (isNull >> not)
        |> Seq.tryHead
        |> Option.map
            (fun a ->
              let fr0 = XElement(XName.Get "FileRef")
              let newfile = mapFile files a modu
              fr0.SetAttributeValue(XName.Get "uid", newfile)
              fr0)

    let sequencePoints modu (group: string * XElement seq) =
      let sp = XElement(XName.Get "SequencePoints")
      let sps =
        group
        |> snd
        |> Seq.collect (fun m -> m.Descendants(XName.Get "SequencePoint"))
        |> Seq.groupBy
            (fun s ->
              s
              |> attributeOrEmpty "sl"
              |> Int32.TryParse
              |> snd,
              s
              |> attributeOrEmpty "sc"
              |> Int32.TryParse
              |> snd)

      let (vs, newsps) = mergePoints files modu tracked sps
      newsps
      |> Seq.iter
          (fun s ->
            s.SetAttributeValue(XName.Get "bec", 0)
            s.SetAttributeValue(XName.Get "bev", 0))

      sp.Add newsps
      (sp, newsps, vs)

    let branchPoints modu (group: string * XElement seq) =
        let bp = XElement(XName.Get "BranchPoints")

        let bps =
          group
          |> snd
          |> Seq.collect (fun m -> m.Descendants(XName.Get "BranchPoint"))
          |> Seq.groupBy
              (fun s ->
                (s
                  |> attributeOrEmpty "sl"
                  |> Int32.TryParse
                  |> snd,
                  s
                  |> attributeOrEmpty "offset"
                  |> Int32.TryParse
                  |> snd),
                s
                |> attributeOrEmpty "endoffset"
                |> Int32.TryParse
                |> snd)

        let (vb, newbps) = mergePoints files modu tracked bps
        bp.Add newbps
        (bp, newbps, vb)

    let methodPoints  modu (group: string * XElement seq) =
      let mpn = XName.Get "MethodPoint"

      let methodPoints =
        group
        |> snd
        |> Seq.collect (fun x -> x.Elements mpn)

      let mpv = methodPoints |> mergeCounts

      let mp =
        methodPoints
        |> Seq.tryHead
        |> Option.map
            (fun r ->
              let mp0 = XElement r
              mp0.SetAttributeValue(XName.Get "vc", mpv)

              r.Attribute(XName.Get "fileid")
              |> Option.ofObj
              |> Option.iter
                    (fun a ->
                      let newfile = mapFile files a modu
                      mp0.SetAttributeValue(XName.Get "fileid", newfile))

              mp0)
      (mp, mpv)

    let s, x =
      methods
      |> Seq.fold
           (fun (ss: Summary, xx) group ->
             let rep = group |> snd |> Seq.head

             let sm = XElement(XName.Get "Summary")
             let mt = metadataToken group

             let name = XElement(XName.Get "Name", fst group)
             let modu = rep.Parent.Parent.Parent.Parent

             let fr = fileRef modu group
             let (sp, newsps, vs) = sequencePoints modu group
             let (bp, newbps, vb) = branchPoints modu group

             // bec, bev heuristic
             if newsps |> Seq.isEmpty |> not
                && newbps |> Seq.isEmpty |> not then
               branchEntryHeuristic newsps newbps

             let (mp, mpv) = methodPoints modu group

             let merge = XElement(XName.Get "Method")

             [ Some sm
               Some mt
               Some name
               fr
               Some sp
               Some bp
               mp ]
             |> Seq.choose id
             |> merge.Add

             rep.Attributes()
             |> Seq.map XAttribute
             |> Seq.toArray
             |> merge.Add

             merge.SetAttributeValue(
               XName.Get "visited",
               if mpv > 0 then "true" else "false"
             )

             merge.SetAttributeValue(
               XName.Get "sequenceCoverage",
               percent vs newsps.Length
             )

             merge.SetAttributeValue(XName.Get "branchCoverage", percent vb newbps.Length)

             let cc =
               attributeOrEmpty "cyclomaticComplexity" rep
               |> Int32.TryParse
               |> snd

             let ratio =
               if newsps.Length = 0 then
                 0.0
               else
                 (float vs) / (float newsps.Length)

             let crap =
               Math.Pow(float cc, 2.0)
               * Math.Pow((1.0 - ratio), 3.0)
               + (float cc)

             let summary =
               { NumSequencePoints = newsps.Length
                 VisitedSequencePoints = vs
                 NumBranchPoints = newbps.Length
                 VisitedBranchPoints = vb
                 MaxCyclomaticComplexity = cc
                 MinCyclomaticComplexity = Some cc
                 VisitedClasses = 0
                 NumClasses = 0
                 VisitedMethods = if mpv > 0 then 1 else 0
                 NumMethods = 1
                 MinCrapScore = Some crap
                 MaxCrapScore = crap }

             summary.Xml.Attributes()
             |> Seq.map XAttribute
             |> Seq.toArray
             |> sm.Add

             (ss.Add summary, merge :: xx))
           (Summary.Create(), [])

    (s, x |> List.toArray)

  let mergeClasses
    (files: Map<string, int>)
    (tracked: Map<string * string, TrackedMethod>)
    (classes: (string * XElement seq) seq)
    : (Summary * XElement array) =

    let s, x =
      classes
      |> Seq.fold
           (fun (ss: Summary, xx) c ->
             let sm = XElement(XName.Get "Summary")
             let mm = XElement(XName.Get "Methods")

             let mc =
               XElement(XName.Get "Class", sm, XElement(XName.Get "FullName", fst c), mm)

             let methods =
               c
               |> snd
               |> Seq.collect (fun m -> m.Descendants(XName.Get "Method"))
               |> Seq.filter (fun x -> x.Attribute(XName.Get "skippedDueTo") |> isNull)
               |> Seq.groupBy (fun x -> x.Element(XName.Get "Name").Value)

             let (msum, merged) = mergeMethods files tracked methods
             mm.Add merged

             let classVisit =
               { msum with
                   NumClasses = 1
                   VisitedClasses = if msum.VisitedMethods > 0 then 1 else 0 }

             classVisit.Xml.Attributes()
             |> Seq.map XAttribute
             |> Seq.toArray
             |> sm.Add

             // skip trivial classes
             if mm.Descendants() |> Seq.isEmpty then
               (ss, xx)
             else
               (ss.Add(classVisit), mc :: xx))
           (Summary.Create(), [])

    (s, x |> List.toArray)

  let mergeModules
    (files: Map<string, int>)
    (tracked: Map<string * string, TrackedMethod>)
    (modules: XElement seq)
    =
    let findFiles (f:XElement) =
      let foundFiles =
        modules
        |> Seq.collect (fun m -> m.Descendants(XName.Get "File"))
        |> Seq.map (attributeOrEmpty "fullPath")
        |> Seq.distinct
        |> Seq.filter (String.IsNullOrWhiteSpace >> not)
        |> Seq.sort

      foundFiles
      |> Seq.iter
            (fun file ->
              XElement(
                XName.Get "File",
                XAttribute(XName.Get "uid", Map.find file files),
                XAttribute(XName.Get "fullPath", file)
              )
              |> f.Add)

    let findClasses (c:XElement) =
      let classes =
        modules
        |> Seq.collect (fun m -> m.Descendants(XName.Get "Class"))
        |> Seq.filter (fun x -> x.Attribute(XName.Get "skippedDueTo") |> isNull)
        |> Seq.groupBy (fun x -> x.Element(XName.Get "FullName").Value)

      let msummary, merged = mergeClasses files tracked classes
      c.Add merged
      msummary

    let r = modules |> Seq.head

    let merge = XElement(XName.Get "Module")

    let hash = XName.Get "hash"
    let hashValue = r.Attribute(hash).Value
    merge.Add(XAttribute(hash, hashValue))
    let summary = XElement(XName.Get "Summary")
    merge.Add summary

    [ "ModulePath"
      "ModuleTime"
      "ModuleName" ]
    |> List.iter
         (fun n ->
           let x = XName.Get n
           merge.Add(XElement(x, r.Element(x).Value)))

    let f = XElement(XName.Get "Files")
    merge.Add f
    findFiles f

    let c = XElement(XName.Get "Classes")
    merge.Add c
    let msummary = findClasses c

    msummary.Xml.Attributes()
    |> Seq.map XAttribute
    |> Seq.toArray
    |> summary.Add

    let t = XElement(XName.Get "TrackedMethods")
    merge.Add t

    let trackedMethods =
      modules
      |> Seq.collect (fun m -> m.Descendants(XName.Get "TrackedMethod"))
      |> Seq.groupBy (attributeOrEmpty "token")
      |> Seq.map
           (fun grouped ->
             let key = (hashValue, fst grouped)
             let data = Map.find key tracked

             XElement(
               XName.Get "TrackedMethod",
               XAttribute(XName.Get "uid", data.Uid),
               XAttribute(XName.Get "token", data.Token),
               XAttribute(XName.Get "name", data.Name),
               XAttribute(XName.Get "strategy", String.Join(";", data.Strategy)),
               XAttribute(XName.Get "entry", String.Join(";", data.Entry)),
               XAttribute(XName.Get "exit", String.Join(";", data.Exit))
             ))

    trackedMethods |> Seq.toArray |> t.Add

    (msummary, merge)

  let accumulate rvalue svalue =
    [ svalue; rvalue ]
    |> Seq.filter (String.IsNullOrWhiteSpace >> not)
    |> Seq.head

  let mergeTrackedMethods index records =
    records
    |> Seq.fold
         (fun state r ->
           { state with
               Hash = accumulate r.Hash state.Hash
               Token = accumulate r.Token state.Token
               Name = accumulate r.Name state.Name
               Strategy = r.Strategy @ state.Strategy
               Entry = r.Entry @ state.Entry
               Exit = r.Exit @ state.Exit
               Uid = index + 1 })
         { Hash = String.Empty
           Token = String.Empty
           Name = String.Empty
           Strategy = []
           Entry = []
           Exit = []
           Uid = index + 1 }

  let mergeDocuments (inputs: XDocument list) =
    let doc = blankOpenCover ()
    let hash = XName.Get "hash"

    let tracked =
      inputs
      |> List.collect
           (fun x ->
             x.Descendants(XName.Get "TrackedMethod")
             |> Seq.toList)
      |> List.map
           (fun x ->
             { Hash = x.Parent.Parent |> attributeOrEmpty "hash"
               Token = x |> attributeOrEmpty "token"
               Name = x |> attributeOrEmpty "name"
               Strategy =
                 (x |> attributeOrEmpty "strategy").Split(';')
                 |> Seq.toList
               Entry =
                 (x |> attributeOrEmpty "entry").Split(';')
                 |> Seq.toList
               Exit =
                 (x |> attributeOrEmpty "exit").Split(';')
                 |> Seq.toList
               Uid = 0 })
      |> List.filter
           (fun t ->
             (t.Name |> String.IsNullOrWhiteSpace
              || t.Hash |> String.IsNullOrWhiteSpace
              || t.Token |> String.IsNullOrWhiteSpace)
             |> not)
      |> List.groupBy (fun t -> (t.Hash, t.Token))
      |> List.mapi (fun i (k, v) -> (k, mergeTrackedMethods i v))
      |> Map.ofList

    let files =
      inputs
      |> Seq.collect (fun m -> m.Descendants(XName.Get "File"))
      |> mergeFiles

    let (summary, modules) =
      inputs
      |> List.collect (fun x -> x.Descendants(XName.Get "Module") |> Seq.toList)
      |> List.filter (fun x -> x.Attribute(XName.Get "skippedDueTo") |> isNull)
      |> List.groupBy (fun x -> x.Attribute(hash).Value)
      |> List.map (snd >> (mergeModules files tracked))
      |> List.fold
           (fun (states: Summary, statem) (locals, localm) ->
             states.Add(locals), localm :: statem)
           (Summary.Create(), [])

    doc
      .Root
      .Element(XName.Get "Modules")
      .Add(modules |> List.toArray)

    let sm = doc.Root.Element(XName.Get "Summary")

    summary.Xml.Attributes()
    |> Seq.map XAttribute
    |> Seq.iter (fun a -> sm.SetAttributeValue(a.Name, a.Value))

    doc

  let Merge (documents: XDocument seq) =
    let inputs =
      documents
      |> Seq.filter
           (fun x ->
             try
               let format = XmlUtilities.discoverFormat x
               format = ReportFormat.OpenCover
             with :? XmlSchemaValidationException -> false)
      |> Seq.toList

    match inputs with
    | [] -> blankOpenCover ()
    | [ x ] -> XDocument x
    | _ -> mergeDocuments inputs

  [<System.Runtime.CompilerServices.MethodImplAttribute(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)>]
  let JsonToXml (document: string) =
    let json =
      document
      |> NativeJson.fromJsonText
    let xml =
      json
      |> NativeJson.jsonToXml
      |> NativeJson.orderXml

    // heuristic for coverlet vs altcover
    let sp = json.Values
             |> Seq.collect (fun m -> m.Values)
             |> Seq.collect (fun d -> d.Values)
             |> Seq.collect (fun c -> c.Values)
             |> Seq.filter (fun m -> m.SeqPnts.IsNotNull)
             |> Seq.collect (fun m -> m.SeqPnts)
             |> Seq.isEmpty |> not
    PostProcess xml (if sp then BranchOrdinal.Offset else BranchOrdinal.SL)
    xml

[<assembly: SuppressMessage("Microsoft.Performance",
                            "CA1810:InitializeReferenceTypeStaticFieldsInline",
                            Scope = "member",
                            Target = "<StartupCode$AltCover-Toolkit>.$OpenCover.#.cctor()",
                            Justification = "Compiler Generated")>]
()