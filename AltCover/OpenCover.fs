namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.Globalization
open System.IO
open System.Xml.Linq

open Mono.Cecil

open AltCover.Augment

[<ExcludeFromCodeCoverage>]
type internal Exclusion =
  | Nothing
  | ByType
  | ByMethod

/// <summary>
/// State object passed from visit to visit
/// </summary>
[<ExcludeFromCodeCoverage; NoComparison>]
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
  let internal X name = XName.Get name

  let internal setChain (xbranch : XElement) (chain : int list) =
    xbranch.SetAttributeValue
      (X "offsetchain",
       match chain with
       | [] -> null
       | l ->
         String.Join
           (" ", l |> Seq.map (fun i -> i.ToString(CultureInfo.InvariantCulture))))

  let SafeMultiply x y =
    try
      Checked.op_Multiply x <| Math.Max(1, y)
    with :? OverflowException -> Int32.MaxValue

  let internal ReportGenerator() =
    // The internal state of the document is mutated by the
    // operation of the visitor.  Everything else should now be pure
    let document = XDocument(XDeclaration("1.0", "utf-8", "yes"), [||])
    let Summary() =
      XElement
        (X "Summary",
         XAttribute(X "numSequencePoints", 0),
         XAttribute(X "visitedSequencePoints", 0),
         XAttribute(X "numBranchPoints", 0),
         XAttribute(X "visitedBranchPoints", 0),
         XAttribute(X "sequenceCoverage", 0),
         XAttribute(X "branchCoverage", 0),
         XAttribute(X "maxCyclomaticComplexity", 0),
         XAttribute(X "minCyclomaticComplexity", 0),
         XAttribute(X "visitedClasses", 0),
         XAttribute(X "numClasses", 0),
         XAttribute(X "visitedMethods", 0),
         XAttribute(X "numMethods", 0),
         XAttribute(X "minCrapScore", 0),
         XAttribute(X "maxCrapScore", 0))

    let StartVisit(s : OpenCoverContext) =
      let element =
        XElement
          (X "CoverageSession",
           XAttribute(XNamespace.Xmlns + "xsd", "http://www.w3.org/2001/XMLSchema"),
           XAttribute
             (XNamespace.Xmlns + "xsi", "http://www.w3.org/2001/XMLSchema-instance"))
      document.Add(element)
      element.Add(Summary())
      let modules = XElement(X "Modules")
      element.Add(modules)
      { s with Stack = modules :: s.Stack }

    let VisitModule (s : OpenCoverContext) (moduleDef : ModuleDefinition) included =
      let instrumented = Visitor.IsInstrumented included
      let element = XElement(X "Module")
      if not instrumented then element.SetAttributeValue(X "skippedDueTo", "Filter")
      else element.Add(Summary())
      element.SetAttributeValue(X "hash", KeyStore.HashFile moduleDef.FileName)
      let head = s.Stack |> Seq.head
      head.Add(element)
      element.Add(XElement(X "ModulePath", moduleDef.FileName))
      element.Add(XElement(X "ModuleTime", File.GetLastWriteTimeUtc moduleDef.FileName))
      element.Add(XElement(X "ModuleName", moduleDef.Assembly.Name.Name))
      if instrumented then element.Add(XElement(X "Files"))
      let classes = XElement(X "Classes")
      element.Add(classes)
      if Visitor.TrackingNames
         |> Seq.isEmpty
         |> not
      then element.Add(XElement(X "TrackedMethods"))
      { s with Stack = classes :: s.Stack
               Excluded = Nothing
               ModuleSeq = 0
               ModuleBr = 0
               ModuleMethods = 0
               ModuleClasses = 0
               ClassCC = [] }

    let VisitType (s : OpenCoverContext) (typeDef : TypeDefinition) included =
      let instrumented = Visitor.IsInstrumented included
      let methods = XElement(X "Methods")
      if included <> Inspect.TrackOnly then
        let element = XElement(X "Class")
        if not instrumented then element.SetAttributeValue(X "skippedDueTo", "Filter")
        let head = s.Stack |> Seq.head
        head.Add(element)
        element.Add(Summary())
        element.Add(XElement(X "FullName", typeDef.FullName))
        element.Add(methods)
      { s with Stack =
                 if instrumented then methods :: s.Stack
                 else s.Stack
               Excluded =
                 if instrumented then Nothing
                 else ByType
               ClassSeq = 0
               ClassBr = 0
               MethodCC = [] }

    let boolString b =
      if b then "true"
      else "false"

    let methodElement (methodDef : MethodDefinition) =
      let cc = Gendarme.CyclomaticComplexity methodDef
      (cc,
       XElement
         (X "Method", XAttribute(X "skippedDueTo", "Filter"),
          XAttribute(X "visited", "false"),
          XAttribute(X "cyclomaticComplexity", cc),
          XAttribute(X "nPathComplexity", "0"),
          XAttribute(X "sequenceCoverage", "0"),
          XAttribute(X "branchCoverage", "0"),
          XAttribute(X "isConstructor", boolString methodDef.IsConstructor),
          XAttribute(X "isStatic", boolString methodDef.IsStatic),
          XAttribute(X "isGetter", boolString methodDef.IsGetter),
          XAttribute(X "isSetter", boolString methodDef.IsSetter),
          XAttribute(X "crapScore", 0)))

    let addMethodContent (element : XElement) (methodDef : MethodDefinition) instrumented =
      element.Add(Summary())
      element.Add
        (XElement(X "MetadataToken", methodDef.MetadataToken.ToUInt32().ToString()))
      element.Add(XElement(X "Name", methodDef.FullName))
      if instrumented then element.Add(XElement(X "FileRef"))
      let seqpnts = XElement(X "SequencePoints")
      element.Add(seqpnts)
      element.Add(XElement(X "BranchPoints"))
      element.Add(XElement(X "MethodPoint"))
      seqpnts

    let VisitMethod (s : OpenCoverContext) (methodDef : MethodDefinition) included =
      if s.Excluded = Nothing && included <> Inspect.TrackOnly then
        let instrumented = Visitor.IsInstrumented included
        let cc, element = methodElement methodDef
        if instrumented then element.SetAttributeValue(X "skippedDueTo", null)
        let head = s.Stack |> Seq.head
        head.Add element
        let seqpnts = addMethodContent element methodDef instrumented
        { s with Stack =
                   if instrumented then seqpnts :: s.Stack
                   else s.Stack
                 Excluded =
                   if instrumented then Nothing
                   else ByMethod
                 Index = -1
                 MethodSeq = 0
                 MethodBr = 0
                 MethodCC = Some cc :: s.MethodCC }
      else s

    let MethodPointElement (codeSegment : SeqPnt) ref i =
      XElement
        (X "SequencePoint",
         XAttribute(X "vc", 0),
         XAttribute(X "uspid", i),
         XAttribute(X "ordinal", 0),
         XAttribute(X "offset", codeSegment.Offset),
         XAttribute(X "sl", codeSegment.StartLine),
         XAttribute(X "sc", codeSegment.StartColumn),
         XAttribute(X "el", codeSegment.EndLine),
         XAttribute(X "ec", codeSegment.EndColumn),
         XAttribute(X "bec", 0),
         XAttribute(X "bev", 0),
         XAttribute(X "fileid", ref))

    let RecordFile (s : OpenCoverContext) file =
      if s.Files.ContainsKey file then s.Files, s.Files.Item file
      else
        let index = s.Files.Count + 1
        s.Files.Add(file, index), index

    let VisitCodeSegment (s : OpenCoverContext) (codeSegment : SeqPnt) i =
      if s.Excluded = Nothing then
        let fileset, ref = RecordFile s codeSegment.Document
        let element = MethodPointElement codeSegment ref i
        let head = s.Stack |> Seq.head
        if head.IsEmpty then head.Add(element)
        else head.FirstNode.AddBeforeSelf(element)
        { s with Files = fileset
                 Index = ref
                 MethodSeq = s.MethodSeq + 1 }
      else s

    let VisitMethodPoint (s : OpenCoverContext) (codeSegment' : SeqPnt option) i =
      match codeSegment' with
      | Some codeSegment -> VisitCodeSegment s codeSegment i
      | None -> s

    let VisitGoTo s branch =
      let fileset, ref = RecordFile s branch.Document
      let fileid = fileset.Item branch.Document
      (XElement
         (X "BranchPoint",
          XAttribute(X "vc", 0),
          XAttribute(X "uspid", branch.Uid),
          XAttribute(X "ordinal", 0),
          XAttribute(X "offset", branch.Offset),
          XAttribute(X "sl", branch.StartLine),
          XAttribute(X "path", branch.Path),
          XAttribute(X "offsetchain", 0),
          XAttribute(X "offsetend", branch.Target.Head),
          XAttribute(X "fileid", fileid)), fileset, ref)

    let VisitBranchPoint s branch =
      if s.Excluded = Nothing then
        let branches = s.Stack.Head.Parent.Descendants(X "BranchPoints") |> Seq.head
        let (xbranch, fileset, ref) = VisitGoTo s branch
        setChain xbranch branch.Target.Tail
        if branches.IsEmpty then branches.Add(xbranch)
        else branches.LastNode.AddAfterSelf(xbranch)
        { s with Files = fileset
                 Index = ref
                 MethodBr = s.MethodBr + 1 }
      else s

    let limitMethodCC count stack =
      if count > 0 then stack
      else None :: stack.Tail

    let passOnClassExclusion excluded =
      if excluded = ByMethod then Nothing
      else excluded

    let handleOrdinals (method : XElement) =
      let sp = method.Descendants(X "SequencePoint") |> Seq.toList
      sp |> Seq.iteri (fun i x -> x.SetAttributeValue(X "ordinal", i))
      let bp = method.Descendants(X "BranchPoint") |> Seq.toList
      bp |> Seq.iteri (fun i x -> x.SetAttributeValue(X "ordinal", i))
      if sp
         |> List.isEmpty
         |> not
         && bp
            |> List.isEmpty
            |> not
      then
        let interleave =
          List.concat [ sp; bp ]
          |> List.sortBy (fun x ->
               x.Attribute(X "offset").Value
               |> Int32.TryParse
               |> snd)

        let (np, _, _) =
          interleave
          |> Seq.fold (fun (np0, bec, sq : XElement) x ->
               match x.Name.LocalName with
               | "SequencePoint" ->
                 sq.SetAttributeValue(X "bec", bec)
                 (SafeMultiply np0 bec, 0, x)
               | _ -> (np0, bec + 1, sq)) (1, 0, sp.Head)

        method.SetAttributeValue(X "nPathComplexity", np)
      else if bp
              |> List.isEmpty
              |> not
      then
        let np =
          bp
          |> List.groupBy (fun bp -> bp.Attribute(X "offset").Value)
          |> Seq.fold (fun np0 (_, b) -> SafeMultiply (Seq.length b) np0) 1
        method.SetAttributeValue(X "nPathComplexity", np)

    let AddTracking (s : OpenCoverContext) (m : MethodDefinition) t =
      t
      |> Option.iter
           (fun (uid, strategy) ->
           let classes = s.Stack |> Seq.find (fun x -> x.Name.LocalName = "Classes")
           let tracked = classes.Parent.Elements(X "TrackedMethods")
           tracked
           |> Seq.iter
                (fun t ->
                t.Add
                  (XElement
                     (X "TrackedMethod",
                      XAttribute(X "uid", uid),
                      XAttribute(X "token", m.MetadataToken.ToUInt32().ToString()),
                      XAttribute(X "name", m.FullName),
                      XAttribute(X "strategy", strategy)))))

    let AddMethodSummary (s : OpenCoverContext) cc (summary : XElement) =
      summary.SetAttributeValue(X "numSequencePoints", s.MethodSeq)
      summary.SetAttributeValue(X "numBranchPoints",
                                s.MethodBr + // make the number agree with OpenCover
                                             if s.MethodSeq > 0 || s.MethodBr > 0 then 1
                                             else 0)
      summary.SetAttributeValue(X "maxCyclomaticComplexity", cc)
      summary.SetAttributeValue(X "minCyclomaticComplexity", cc)
      summary.SetAttributeValue(X "numMethods",
                                if s.MethodSeq > 0 || s.MethodBr > 0 then 1
                                else 0)

    let VisitAfterMethodIncluded(s : OpenCoverContext) =
      let head, tail = Augment.Split s.Stack
      head.Parent.Elements(X "FileRef")
      |> Seq.toList
      |> Seq.iter (fun fileref ->
           if s.Index < 0 then fileref.Remove()
           else fileref.Add(XAttribute(X "uid", s.Index)))
      let cc = Option.getOrElse 1 s.MethodCC.Head
      let method = head.Parent
      method.Elements(X "Summary") |> Seq.iter (AddMethodSummary s cc)
      handleOrdinals method
      tail

    let UpdateClassCountsByMethod (s : OpenCoverContext) (tail : XElement list) =
      { s with Stack = tail
               ClassSeq = s.ClassSeq + s.MethodSeq
               ClassBr =
                 s.ClassBr + s.MethodBr + // make the number agree with OpenCover
                                          if s.MethodSeq > 0 || s.MethodBr > 0 then 1
                                          else 0
               MethodCC = limitMethodCC (s.MethodSeq + s.MethodBr) s.MethodCC }

    let VisitAfterMethod (s : OpenCoverContext) methodDef track included =
      AddTracking s methodDef track
      if s.Excluded = Nothing && included <> Inspect.TrackOnly then
        let tail = VisitAfterMethodIncluded s
        UpdateClassCountsByMethod s tail
      else { s with Excluded = passOnClassExclusion s.Excluded }

    let VisitAfterType(s : OpenCoverContext) =
      let head, tail = Augment.Split s.Stack

      let (min, max), methods =
        s.MethodCC
        |> Seq.map (fun q ->
             (q |> Option.getOrElse 1, q |> Option.getOrElse 0),
             if q.IsSome then 1
             else 0)
        |> Seq.fold (fun state pair ->
             let cc = fst state
             let cc2 = fst pair
             (Math.Min(fst cc, fst cc2), Math.Max(snd cc, snd cc2)), snd state + snd pair)
             ((1, 0), 0)

      let classes =
        if s.ClassSeq > 0 || s.ClassBr > 0 then 1
        else 0

      head.Parent.Elements(X "Summary")
      |> Seq.iter (fun summary ->
           summary.SetAttributeValue(X "numSequencePoints", s.ClassSeq)
           summary.SetAttributeValue(X "numBranchPoints", s.ClassBr)
           summary.SetAttributeValue(X "maxCyclomaticComplexity", max)
           summary.SetAttributeValue(X "minCyclomaticComplexity", Math.Min(min, max))
           summary.SetAttributeValue(X "numClasses", classes)
           summary.SetAttributeValue(X "numMethods", methods))
      { s with Stack =
                 if s.Excluded = Nothing then tail
                 else s.Stack
               ModuleSeq = s.ModuleSeq + s.ClassSeq
               ModuleBr = s.ModuleBr + s.ClassBr
               ClassCC = (Math.Max(1, min), max) :: s.ClassCC
               ModuleMethods = methods + s.ModuleMethods
               ModuleClasses = classes + s.ModuleClasses
               Excluded = Nothing }

    let VisitAfterModule(s : OpenCoverContext) =
      let head, tail = Augment.Split s.Stack
      let min, max =
        s.ClassCC
        |> Seq.fold
             (fun state pair ->
             Math.Min(fst state, fst pair), Math.Max(snd state, snd pair)) (1, 0)
      head.Parent.Elements(X "Summary")
      |> Seq.iter (fun summary ->
           summary.SetAttributeValue(X "numSequencePoints", s.ModuleSeq)
           summary.SetAttributeValue(X "numBranchPoints", s.ModuleBr)
           summary.SetAttributeValue(X "maxCyclomaticComplexity", max)
           summary.SetAttributeValue(X "minCyclomaticComplexity", min)
           summary.SetAttributeValue(X "numClasses", s.ModuleClasses)
           summary.SetAttributeValue(X "numMethods", s.ModuleMethods))
      let files = head.Parent.Elements(X "Files")
      s.Files
      |> Map.toSeq
      |> Seq.sortBy snd
      |> Seq.iter
           (fun (k, v) ->
           files
           |> Seq.iter
                (fun f ->
                f.Add
                  (XElement(X "File",
                            XAttribute(X "uid", v),
                            XAttribute(X "fullPath", k)))))
      { s with Stack = tail
               TotalSeq = s.TotalSeq + s.ModuleSeq
               TotalBr = s.TotalBr + s.ModuleBr
               ModuleCC = (min, max) :: s.ModuleCC
               TotalClasses = s.ModuleClasses + s.TotalClasses
               TotalMethods = s.ModuleMethods + s.TotalMethods }

    let AfterAll(s : OpenCoverContext) =
      let head = s.Stack |> Seq.head
      let min, max =
        s.ModuleCC
        |> Seq.fold
             (fun state pair ->
             Math.Min(fst state, fst pair), Math.Max(snd state, snd pair)) (1, 0)
      head.Parent.Elements(X "Summary")
      |> Seq.iter (fun summary ->
           summary.SetAttributeValue(X "numSequencePoints", s.TotalSeq)
           summary.SetAttributeValue(X "numBranchPoints", s.TotalBr)
           summary.SetAttributeValue(X "maxCyclomaticComplexity", max)
           summary.SetAttributeValue(X "minCyclomaticComplexity", min)
           summary.SetAttributeValue(X "numClasses", s.TotalClasses)
           summary.SetAttributeValue(X "numMethods", s.TotalMethods))
      s

    let ReportVisitor (s : OpenCoverContext) (node : Node) =
      match node with
      | Start _ -> StartVisit s
      | Node.Module(moduleDef, included) -> VisitModule s moduleDef included
      | Node.Type(typeDef, included) -> VisitType s typeDef included
      | Node.Method(methodDef, included, _) -> VisitMethod s methodDef included
      | MethodPoint(_, codeSegment, i, _) -> VisitMethodPoint s codeSegment i
      | BranchPoint b -> VisitBranchPoint s b
      | AfterMethod(methodDef, included, track) ->
        VisitAfterMethod s methodDef track included
      | AfterType _ -> VisitAfterType s
      | AfterModule _ -> VisitAfterModule s
      | Finish -> AfterAll s
      | _ -> s

    let result = Visitor.EncloseState ReportVisitor (OpenCoverContext.Build())
    (result, document)