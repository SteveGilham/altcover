namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.Globalization
open System.IO
open System.Linq
open System.Xml.Linq

open Mono.Cecil

[<ExcludeFromCodeCoverage>]
type internal Exclusion =
  | Nothing
  | ByType
  | ByMethod

// // <summary>
// // State object passed from visit to visit
// // </summary>
[<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
type internal OpenCoverContext =
  { Stack : XElement list
    Excluded : Exclusion
    Files : Map<string, int>
    Index : int
    MethodSeq : int
    MethodBr : int
    MethodCC : int option list
    ClassSeq : int
    ClassBr : int
    ClassCC : (int * int) list
    ModuleSeq : int
    ModuleBr : int
    ModuleMethods : int
    ModuleClasses : int
    ModuleCC : (int * int) list
    TotalMethods : int
    TotalClasses : int
    TotalSeq : int
    TotalBr : int }
  static member Build() =
    { Stack = List.empty<XElement>
      Excluded = Nothing
      Files = Map.empty<string, int>
      Index = 0
      MethodSeq = 0
      MethodBr = 0
      MethodCC = []
      ClassSeq = 0
      ClassBr = 0
      ClassCC = []
      ModuleSeq = 0
      ModuleBr = 0
      ModuleMethods = 0
      ModuleClasses = 0
      ModuleCC = []
      TotalMethods = 0
      TotalClasses = 0
      TotalSeq = 0
      TotalBr = 0 }

module internal OpenCover =
  let internal safeMultiply x y =
    try
      Checked.op_Multiply x <| Math.Max(1, y)
    with :? OverflowException -> Int32.MaxValue

  module internal I =
    let internal setChain (xbranch : XElement) branch =
      let chain = branch.Target.Tail |> List.map (fun i -> i.Offset)
      xbranch.SetAttributeValue
        ("offsetchain".X,
         match chain with
         | [] -> null
         | l ->
             String.Join
               (" ", l |> Seq.map (fun i -> i.ToString(CultureInfo.InvariantCulture))))

  [<SuppressMessage("Microsoft.Maintainability", "CA1506",
                    Justification = "partitioned into closures")>]
  let internal reportGenerator() =
    // The internal state of the document is mutated by the
    // operation of the visitor.  Everything else should now be pure
    let document = XDocument(XDeclaration("1.0", "utf-8", "yes"), [||])
    let summary() =
      XElement
        ("Summary".X, XAttribute("numSequencePoints".X, 0),
         XAttribute("visitedSequencePoints".X, 0), XAttribute("numBranchPoints".X, 0),
         XAttribute("visitedBranchPoints".X, 0), XAttribute("sequenceCoverage".X, 0),
         XAttribute("branchCoverage".X, 0), XAttribute("maxCyclomaticComplexity".X, 0),
         XAttribute("minCyclomaticComplexity".X, 0), XAttribute("visitedClasses".X, 0),
         XAttribute("numClasses".X, 0), XAttribute("visitedMethods".X, 0),
         XAttribute("numMethods".X, 0), XAttribute("minCrapScore".X, 0),
         XAttribute("maxCrapScore".X, 0))

    let startVisit(s : OpenCoverContext) =
      let element =
        XElement
          ("CoverageSession".X,
           XAttribute(XNamespace.Xmlns + "xsd", "http://www.w3.org/2001/XMLSchema"),
           XAttribute
             (XNamespace.Xmlns + "xsi", "http://www.w3.org/2001/XMLSchema-instance"))
      document.Add(element)
      element.Add(summary())
      let modules = XElement("Modules".X)
      element.Add(modules)
      { s with Stack = modules :: s.Stack }

    let visitModule (s : OpenCoverContext) (moduleDef : ModuleDefinition)
                    (included : Inspections) =
      let instrumented = included.IsInstrumented
      let element = XElement("Module".X)
      if not instrumented
      then element.SetAttributeValue("skippedDueTo".X, "Filter")
      else element.Add(summary())
      element.SetAttributeValue("hash".X, KeyStore.hashFile moduleDef.FileName)
      let head = s.Stack |> Seq.head
      head.Add(element)
      element.Add(XElement("ModulePath".X, moduleDef.FileName |> Path.GetFullPath))
      element.Add(XElement("ModuleTime".X, File.GetLastWriteTimeUtc moduleDef.FileName))
      element.Add(XElement("ModuleName".X, moduleDef.Assembly.Name.Name))
      if instrumented then element.Add(XElement("Files".X))
      let classes = XElement("Classes".X)
      element.Add(classes)
      if CoverageParameters.trackingNames
         |> Seq.isEmpty
         |> not
      then element.Add(XElement("TrackedMethods".X))
      { s with
          Stack = classes :: s.Stack
          Excluded = Nothing
          ModuleSeq = 0
          ModuleBr = 0
          ModuleMethods = 0
          ModuleClasses = 0
          ClassCC = [] }

    let visitType (s : OpenCoverContext) (typeDef : TypeDefinition)
                  (included : Inspections) =
      let instrumented =  included.IsInstrumented
      let methods = XElement("Methods".X)
      if included <> Inspections.TrackOnly then
        let element = XElement("Class".X)
        if not instrumented then element.SetAttributeValue("skippedDueTo".X, "Filter")
        let head = s.Stack |> Seq.head
        head.Add(element)
        element.Add(summary())
        element.Add(XElement("FullName".X, typeDef.FullName))
        element.Add(methods)
      { s with
          Stack =
            if instrumented then methods :: s.Stack else s.Stack
          Excluded =
            if instrumented then Nothing else ByType
          ClassSeq = 0
          ClassBr = 0
          MethodCC = [] }

    let boolString b =
      if b then "true" else "false"

    let methodElement (methodDef : MethodDefinition) =
      let cc = Gendarme.cyclomaticComplexity methodDef
      (cc,
       XElement
         ("Method".X, XAttribute("skippedDueTo".X, "Filter"),
          XAttribute("visited".X, "false"), XAttribute("cyclomaticComplexity".X, cc),
          XAttribute("nPathComplexity".X, "0"), XAttribute("sequenceCoverage".X, "0"),
          XAttribute("branchCoverage".X, "0"),
          XAttribute("isConstructor".X, boolString methodDef.IsConstructor),
          XAttribute("isStatic".X, boolString methodDef.IsStatic),
          XAttribute("isGetter".X, boolString methodDef.IsGetter),
          XAttribute("isSetter".X, boolString methodDef.IsSetter),
          XAttribute("crapScore".X, 0)))

    let addMethodContent (element : XElement) (methodDef : MethodDefinition) instrumented =
      element.Add(summary())
      element.Add
        (XElement("MetadataToken".X, methodDef.MetadataToken.ToUInt32().ToString()))
      element.Add(XElement("Name".X, methodDef.FullName))
      if instrumented then element.Add(XElement("FileRef".X))
      let seqpnts = XElement("SequencePoints".X)
      element.Add(seqpnts)
      element.Add(XElement("BranchPoints".X))
      element.Add(XElement("MethodPoint".X))
      seqpnts

    let visitMethod (s : OpenCoverContext) (methodDef : MethodDefinition) included =
      if s.Excluded = Nothing && included <> Inspections.TrackOnly then
        let instrumented = included.IsInstrumented
        let cc, element = methodElement methodDef
        if instrumented then element.SetAttributeValue("skippedDueTo".X, "File")
        let head = s.Stack |> Seq.head
        head.Add element
        let seqpnts = addMethodContent element methodDef instrumented
        { s with
            Stack =
              if instrumented then seqpnts :: s.Stack else s.Stack
            Excluded =
              if instrumented then Nothing else ByMethod
            Index = -1
            MethodSeq = 0
            MethodBr = 0
            MethodCC = Some cc :: s.MethodCC }
      else
        s

    let methodPointElement (codeSegment : SeqPnt) ref i vc =
      XElement
        ("SequencePoint".X, XAttribute("vc".X, int vc), XAttribute("uspid".X, i),
         XAttribute("ordinal".X, 0), XAttribute("offset".X, codeSegment.Offset),
         XAttribute("sl".X, codeSegment.StartLine),
         XAttribute("sc".X, codeSegment.StartColumn),
         XAttribute("el".X, codeSegment.EndLine),
         XAttribute("ec".X, codeSegment.EndColumn), XAttribute("bec".X, 0),
         XAttribute("bev".X, 0), XAttribute("fileid".X, ref))

    let recordFile (s : OpenCoverContext) file =
      if s.Files.ContainsKey file then
        s.Files, s.Files.Item file
      else
        let index = s.Files.Count + 1
        s.Files.Add(file, index), index

    let visitCodeSegment (s : OpenCoverContext) (codeSegment : SeqPnt) i vc =
      let fileset, ref =
        recordFile s (codeSegment.Document |> Visitor.sourceLinkMapping)
      let element = methodPointElement codeSegment ref i vc
      let head = s.Stack |> Seq.head
      if head.IsEmpty then head.Add(element) else head.FirstNode.AddBeforeSelf(element)
      { s with
          Files = fileset
          Index = ref
          MethodSeq = s.MethodSeq + 1 }

    let visitMethodPoint (s : OpenCoverContext) (codeSegment' : SeqPnt option) i included
        vc =
      let element = (s.Stack |> Seq.head).Parent

      let attr =
        element.Attribute("skippedDueTo".X)
        |> Option.ofObj
        |> Option.map (fun a -> a.Value)
      if included && attr = Some "File" then
        element.SetAttributeValue("skippedDueTo".X, null)
      match (included, codeSegment', s.Excluded) with
      | (true, Some codeSegment, Nothing) -> visitCodeSegment s codeSegment i vc
      | _ -> s

    let visitGoTo s branch =
      let doc = branch.SequencePoint.Document.Url
      let fileset, ref = recordFile s doc
      let fileid = fileset.Item doc
      (XElement
        ("BranchPoint".X, XAttribute("vc".X, int branch.VisitCount),
         XAttribute("uspid".X, branch.Uid), XAttribute("ordinal".X, 0),
         XAttribute("offset".X, branch.Offset),
         XAttribute("sl".X, branch.SequencePoint.StartLine),
         XAttribute("path".X, branch.Path), XAttribute("offsetchain".X, 0),
         XAttribute("offsetend".X, branch.Target.Head.Offset),
         XAttribute("fileid".X, fileid)), fileset, ref)

    let visitBranchPoint s branch =
      if s.Excluded = Nothing && branch.Included
         && branch.Representative = Reporting.Representative then
        let branches = s.Stack.Head.Parent.Descendants("BranchPoints".X) |> Seq.head
        let (xbranch, fileset, ref) = visitGoTo s branch
        I.setChain xbranch branch
        if branches.IsEmpty then
          branches.Add(xbranch)
        else
          branches.LastNode.AddAfterSelf(xbranch)
        { s with
            Files = fileset
            Index = ref
            MethodBr = s.MethodBr + 1 }
      else
        s

    let limitMethodCC count stack =
      if count > 0 then stack else None :: stack.Tail

    let passOnClassExclusion excluded =
      if excluded = ByMethod then Nothing else excluded

    let handleOrdinals (method : XElement) =
      let sp = method.Descendants("SequencePoint".X) |> Seq.toList
      sp |> Seq.iteri (fun i x -> x.SetAttributeValue("ordinal".X, i))
      let bp = method.Descendants("BranchPoint".X) |> Seq.toList
      bp |> Seq.iteri (fun i x -> x.SetAttributeValue("ordinal".X, i))
      if sp
         |> List.isEmpty
         |> not
         && bp
            |> List.isEmpty
            |> not then
        let tail = XElement("SequencePoint".X, XAttribute("offset".X, Int32.MaxValue))

        let interleave =
          List.concat
            [ sp
              bp
              [ tail ] ]
          |> List.sortBy (fun x ->
               x.Attribute("offset".X).Value
               |> Int32.TryParse
               |> snd)

        let (np, _, _) =
          interleave
          |> Seq.fold (fun (np0, bec, sq : XElement) x ->
               match x.Name.LocalName with
               | "SequencePoint" ->
                   sq.SetAttributeValue("bec".X, bec)
                   (safeMultiply np0 bec, 0, x)
               | _ -> (np0, bec + 1, sq)) (1, 0, sp.Head)

        method.SetAttributeValue("nPathComplexity".X, np)
      else if bp
              |> List.isEmpty
              |> not then
        let np =
          bp
          |> List.groupBy (fun bp -> bp.Attribute("offset".X).Value)
          |> Seq.fold (fun np0 (_, b) -> safeMultiply (Seq.length b) np0) 1
        method.SetAttributeValue("nPathComplexity".X, np)

    let addTracking (s : OpenCoverContext) (m : MethodDefinition) t =
      t
      |> Option.iter (fun (uid, strategy) ->
           let classes = s.Stack |> Seq.find (fun x -> x.Name.LocalName = "Classes")
           let tracked = classes.Parent.Elements("TrackedMethods".X)
           tracked
           |> Seq.iter (fun t ->
                t.Add
                  (XElement
                    ("TrackedMethod".X, XAttribute("uid".X, uid),
                     XAttribute("token".X, m.MetadataToken.ToUInt32().ToString()),
                     XAttribute("name".X, m.FullName), XAttribute("strategy".X, strategy)))))

    let addMethodSummary (s : OpenCoverContext) cc (summary : XElement) =
      summary.SetAttributeValue("numSequencePoints".X, s.MethodSeq)
      summary.SetAttributeValue
        ("numBranchPoints".X,
         s.MethodBr + // make the number agree with OpenCover
         if s.MethodSeq > 0 || s.MethodBr > 0 then 1 else 0)
      summary.SetAttributeValue("maxCyclomaticComplexity".X, cc)
      summary.SetAttributeValue("minCyclomaticComplexity".X, cc)
      summary.SetAttributeValue
        ("numMethods".X,
         (if s.MethodSeq > 0 || s.MethodBr > 0 then 1 else 0))

    let visitAfterMethodIncluded(s : OpenCoverContext) =
      let head, tail = s.Stack.Split
      let skipped = head.Parent.Attributes("skippedDueTo".X).Any()
      head.Parent.Elements("FileRef".X)
      |> Seq.toList
      |> Seq.iter (fun fileref ->
           if s.Index < 0
           then fileref.Remove()
           else fileref.Add(XAttribute("uid".X, s.Index)))
      let cc = Option.defaultValue 1 s.MethodCC.Head
      let method = head.Parent
      if skipped then
        method.Elements("Summary".X)
        |> Seq.toList
        |> Seq.iter (fun x -> x.Remove())
      else
        method.Elements("Summary".X) |> Seq.iter (addMethodSummary s cc)
      handleOrdinals method
      (tail, skipped)

    let updateClassCountsByMethod (s : OpenCoverContext) (tail : XElement list) =
      { s with
          Stack = tail
          ClassSeq = s.ClassSeq + s.MethodSeq
          ClassBr =
            s.ClassBr + s.MethodBr + // make the number agree with OpenCover
            if s.MethodSeq > 0 || s.MethodBr > 0 then 1 else 0
          MethodCC = limitMethodCC (s.MethodSeq + s.MethodBr) s.MethodCC }

    let visitAfterMethod (s : OpenCoverContext) methodDef track included =
      addTracking s methodDef track
      if s.Excluded = Nothing && included <> Inspections.TrackOnly then
        let tail, skipped = visitAfterMethodIncluded s
        if skipped |> not then
          updateClassCountsByMethod s tail
        else
          { s with Stack = tail }
      else
        { s with Excluded = passOnClassExclusion s.Excluded }

    let visitAfterType(s : OpenCoverContext) =
      let head, tail = s.Stack.Split

      let (min, max), methods =
        s.MethodCC
        |> Seq.map (fun q ->
             (q |> Option.defaultValue 1, q |> Option.defaultValue 0),
             (if q.IsSome then 1 else 0))
        |> Seq.fold (fun state pair ->
             let cc = fst state
             let cc2 = fst pair
             (Math.Min(fst cc, fst cc2), Math.Max(snd cc, snd cc2)),
             snd state + snd pair) ((1, 0), 0)

      let classes =
        if s.ClassSeq > 0 || s.ClassBr > 0 then 1 else 0

      head.Parent.Elements("Summary".X)
      |> Seq.iter (fun summary ->
           summary.SetAttributeValue("numSequencePoints".X, s.ClassSeq)
           summary.SetAttributeValue("numBranchPoints".X, s.ClassBr)
           summary.SetAttributeValue("maxCyclomaticComplexity".X, max)
           summary.SetAttributeValue("minCyclomaticComplexity".X, Math.Min(min, max))
           summary.SetAttributeValue("numClasses".X, classes)
           summary.SetAttributeValue("numMethods".X, methods))

      if head.Descendants("Method".X) |> Seq.isEmpty &&
         //head.Attributes("skippedDueTo".X) |> Seq.isEmpty &&
         s.Excluded = Nothing
      then head.Parent.Remove()

      { s with
          Stack =
            if s.Excluded = Nothing then tail else s.Stack
          ModuleSeq = s.ModuleSeq + s.ClassSeq
          ModuleBr = s.ModuleBr + s.ClassBr
          ClassCC = (Math.Max(1, min), max) :: s.ClassCC
          ModuleMethods = methods + s.ModuleMethods
          ModuleClasses = classes + s.ModuleClasses
          Excluded = Nothing }

    let visitAfterModule(s : OpenCoverContext) =
      let head, tail = s.Stack.Split
      let min, max =
        s.ClassCC
        |> Seq.fold
             (fun state pair ->
               Math.Min(fst state, fst pair), Math.Max(snd state, snd pair)) (1, 0)
      head.Parent.Elements("Summary".X)
      |> Seq.iter (fun summary ->
           summary.SetAttributeValue("numSequencePoints".X, s.ModuleSeq)
           summary.SetAttributeValue("numBranchPoints".X, s.ModuleBr)
           summary.SetAttributeValue("maxCyclomaticComplexity".X, max)
           summary.SetAttributeValue("minCyclomaticComplexity".X, min)
           summary.SetAttributeValue("numClasses".X, s.ModuleClasses)
           summary.SetAttributeValue("numMethods".X, s.ModuleMethods))
      let files = head.Parent.Elements("Files".X)
      s.Files
      |> Map.toSeq
      |> Seq.sortBy snd
      |> Seq.iter (fun (k, v) ->
           files
           |> Seq.iter
                (fun f ->
                  f.Add
                    (XElement
                      ("File".X, XAttribute("uid".X, v), XAttribute("fullPath".X, k)))))
      { s with
          Stack = tail
          TotalSeq = s.TotalSeq + s.ModuleSeq
          TotalBr = s.TotalBr + s.ModuleBr
          ModuleCC = (min, max) :: s.ModuleCC
          TotalClasses = s.ModuleClasses + s.TotalClasses
          TotalMethods = s.ModuleMethods + s.TotalMethods }

    let afterAll(s : OpenCoverContext) =
      let head = s.Stack |> Seq.head
      let min, max =
        s.ModuleCC
        |> Seq.fold
             (fun state pair ->
               Math.Min(fst state, fst pair), Math.Max(snd state, snd pair)) (1, 0)
      head.Parent.Elements("Summary".X)
      |> Seq.iter (fun summary ->
           summary.SetAttributeValue("numSequencePoints".X, s.TotalSeq)
           summary.SetAttributeValue("numBranchPoints".X, s.TotalBr)
           summary.SetAttributeValue("maxCyclomaticComplexity".X, max)
           summary.SetAttributeValue("minCyclomaticComplexity".X, Math.Max(1, min))
           summary.SetAttributeValue("numClasses".X, s.TotalClasses)
           summary.SetAttributeValue("numMethods".X, s.TotalMethods))
      s

    // split for gendarme
    let reportVisitor (s : OpenCoverContext) (node : Node) =
      match node with
      | Start _ -> startVisit s
      | Node.Module(moduleDef, included) -> visitModule s moduleDef included
      | Node.Type(typeDef, included, _) -> visitType s typeDef included
      | Node.Method(methodDef, included, _, _) -> visitMethod s methodDef included
      | MethodPoint(_, codeSegment, i, included, vc) ->
          visitMethodPoint s codeSegment i included vc
      | BranchPoint b -> visitBranchPoint s b
      | AfterMethod(methodDef, included, track) ->
          visitAfterMethod s methodDef track included
      | AfterType _ -> visitAfterType s
      | AfterModule _ -> visitAfterModule s
      | Finish -> afterAll s
      | _ -> s

    let result = Visitor.encloseState reportVisitor (OpenCoverContext.Build())
    (result, document)