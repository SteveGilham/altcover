namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Xml.Linq

open Mono.Cecil
open Mono.Cecil.Cil

open AltCover.Augment

module OpenCover =

  [<ExcludeFromCodeCoverage>]
  type internal Exclusion =
  | Nothing
  | Type
  | Method

  /// <summary>
  /// State object passed from visit to visit
  /// </summary>
  [<ExcludeFromCodeCoverage>]
  type internal Context = { Stack : XElement list
                            Excluded : Exclusion
                            Files : Map<string, int>
                            Index : int
                            MethodSeq : int
                            MethodCC : int option list
                            ClassSeq : int
                            ClassCC : (int * int) list
                            ModuleSeq : int
                            ModuleMethods : int
                            ModuleClasses : int
                            ModuleCC : (int * int) list
                            TotalMethods : int
                            TotalClasses : int
                            TotalSeq : int}
  with static member Build () =
                    { Stack = List.empty<XElement>
                      Excluded = Nothing
                      Files = Map.empty<string, int>
                      Index = 0
                      MethodSeq = 0
                      MethodCC = []
                      ClassSeq = 0
                      ClassCC = []
                      ModuleSeq = 0
                      ModuleMethods = 0
                      ModuleClasses = 0
                      ModuleCC = []
                      TotalMethods = 0
                      TotalClasses = 0
                      TotalSeq = 0}
  let internal ReportGenerator () =

    // The internal state of the document is mutated by the
    // operation of the visitor.  Everything else should now be pure
    let document = XDocument(XDeclaration("1.0", "utf-8", "yes"), [||])

    let X name =
      XName.Get name

    let Summary () =
        XElement(X "Summary",
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
                XAttribute(X "numMethods", 0))

    let StartVisit (s : Context) =
          let element = XElement(X "CoverageSession",
                                    XAttribute(XNamespace.Xmlns + "xsd", "http://www.w3.org/2001/XMLSchema"),
                                    XAttribute(XNamespace.Xmlns + "xsi", "http://www.w3.org/2001/XMLSchema-instance")
          )
          document.Add(element)
          element.Add(Summary())
          let modules = XElement(X "Modules")
          element.Add(modules)
          {s with Stack = modules :: s.Stack }

    let VisitModule (s : Context) (moduleDef:ModuleDefinition) included =
          let element = XElement(X "Module")
          if not included then element.SetAttributeValue(X "skippedDueTo", "Filter")
                          else element.Add(Summary())
          element.SetAttributeValue(X "hash", KeyStore.HashFile moduleDef.FileName)
          let head = s.Stack |> Seq.head
          head.Add(element)

          element.Add(XElement(X "ModulePath", moduleDef.FileName))
          element.Add(XElement(X "ModuleTime", File.GetLastWriteTimeUtc moduleDef.FileName))
          element.Add(XElement(X "ModuleName", moduleDef.Assembly.Name.Name))
          if included then element.Add(XElement(X "Files"))
          let classes = XElement(X "Classes")
          element.Add(classes)
          if Visitor.TrackingNames |> Seq.isEmpty |> not then
            element.Add(XElement(X "TrackedMethods"))
          {s with Stack = classes :: s.Stack
                  Excluded = Nothing
                  ModuleSeq = 0
                  ModuleMethods = 0
                  ModuleClasses = 0
                  ClassCC = []}

    let VisitType (s : Context) (typeDef:TypeDefinition) included =
          let element = XElement(X "Class")
          if not included then element.SetAttributeValue(X "skippedDueTo", "Filter")
          let head = s.Stack |> Seq.head
          head.Add(element)
          element.Add(Summary())
          element.Add(XElement(X "FullName", typeDef.FullName))
          let methods = XElement(X "Methods")
          element.Add(methods)
          {s with Stack = if included then methods :: s.Stack else s.Stack
                  Excluded = if included then Nothing else Type
                  ClassSeq = 0
                  MethodCC = []}

    let boolString b = if b then "true" else "false"

    let methodElement (methodDef:MethodDefinition) =
         let cc = Gendarme.CyclomaticComplexity methodDef
         (cc, XElement(X "Method",
                           XAttribute(X "skippedDueTo", "Filter"),
                           XAttribute(X "visited", "false"),
                           XAttribute(X "cyclomaticComplexity", cc),
                           XAttribute(X "nPathComplexity", "0"),
                           XAttribute(X "sequenceCoverage", "0"),
                           XAttribute(X "branchCoverage", "0"),
                           XAttribute(X "isConstructor", boolString methodDef.IsConstructor),
                           XAttribute(X "isStatic", boolString methodDef.IsStatic),
                           XAttribute(X "isGetter", boolString methodDef.IsGetter),
                           XAttribute(X "isSetter", boolString methodDef.IsSetter)))

    let VisitMethod  (s : Context) (methodDef:MethodDefinition) included =
       if s.Excluded = Nothing then
          let cc, element = methodElement methodDef
          if included then element.SetAttributeValue(X "skippedDueTo", null)
          let head = s.Stack |> Seq.head
          head.Add element
          element.Add(Summary())
          element.Add(XElement(X "MetadataToken", methodDef.MetadataToken.ToUInt32().ToString()))
          element.Add(XElement(X "Name", methodDef.FullName))
          element.Add(XElement(X "FileRef"))
          let seqpnts = XElement(X "SequencePoints")
          element.Add(seqpnts)
          element.Add(XElement(X "BranchPoints"))
          element.Add(XElement(X "MethodPoint"))
          {s with Stack = if included then seqpnts :: s.Stack else s.Stack
                  Excluded = if included then Nothing else Method
                  Index = -1
                  MethodSeq = 0
                  MethodCC = Some cc :: s.MethodCC}
       else s

    let MethodPointElement (codeSegment:Cil.SequencePoint) end' ref i =
      XElement(X "SequencePoint",
        XAttribute(X "vc", 0),
        XAttribute(X "uspid", i),
        XAttribute(X "ordinal", 0),
        XAttribute(X "offset", codeSegment.Offset),
        XAttribute(X "sl", codeSegment.StartLine),
        XAttribute(X "sc", codeSegment.StartColumn),
        XAttribute(X "el", fst end'),
        XAttribute(X "ec", snd end'),
        XAttribute(X "bec", 0),
        XAttribute(X "bev", 0),
        XAttribute(X "fileid", ref))

    let VisitMethodPoint (s : Context) (codeSegment:Cil.SequencePoint) i included =
       if s.Excluded = Nothing then
          // quick fix for .mdb lack of end line/column information
          let end' = match (codeSegment.EndLine, codeSegment.EndColumn) with
                     | (-1, _) -> (codeSegment.StartLine, codeSegment.StartColumn + 1)
                     | endPair -> endPair
          let file = codeSegment.Document.Url
          let fileset, ref = if s.Files.ContainsKey file then
                                s.Files, s.Files.Item file
                             else
                                let index = s.Files.Count + 1
                                s.Files.Add (file, index), index
          let element = MethodPointElement codeSegment end' ref i
          let head = s.Stack |> Seq.head
          if head.IsEmpty then head.Add(element)
          else head.FirstNode.AddBeforeSelf(element)
          { s with Files = fileset
                   Index = ref
                   MethodSeq = s.MethodSeq + 1}
       else s

    let VisitMethodPoint' (s : Context) (codeSegment':Cil.SequencePoint option) i included =
      match codeSegment' with
      | Some codeSegment ->  VisitMethodPoint s codeSegment i included
      | None -> s

    let limitMethodCC count stack =
        if count > 0
        then stack
        else None :: stack.Tail

    let passOnClassExclusion excluded =
        if excluded = Method then Nothing else excluded

    let handleSequencePoints (``method``:XElement) =
        let sp = ``method``.Descendants(X "SequencePoint")
        sp |> Seq.iteri(fun i x -> x.SetAttributeValue(X "ordinal", i))

    let AddTracking (s : Context) (m:MethodDefinition) t =
      t |>
      Option.iter(fun (uid,strategy) ->
                    let classes = s.Stack |> Seq.skip 2 |> Seq.head
                    let tracked = classes.Parent.Elements(X "TrackedMethods")
                    tracked
                    |> Seq.iter (fun t -> t.Add(XElement(X "TrackedMethod",
                                                         XAttribute(X "uid", uid),
                                                         XAttribute(X "token", m.MetadataToken.ToUInt32().ToString()),
                                                         XAttribute(X "name", m.FullName),
                                                         XAttribute(X "strategy", strategy)))))

    let VisitAfterMethodIncluded (s : Context) =
        let head,tail = Augment.Split s.Stack
        head.Parent.Elements(X "FileRef")
        |> Seq.toList
        |> Seq.iter (fun fileref ->  if s.Index < 0
                                     then fileref.Remove()
                                     else fileref.Add(XAttribute(X "uid", s.Index)))
        let cc = Option.getOrElse 1 s.MethodCC.Head
        let ``method`` = head.Parent
        ``method``.Elements(X "Summary")
        |> Seq.iter(fun summary -> summary.SetAttributeValue(X "numSequencePoints", s.MethodSeq)
                                   summary.SetAttributeValue(X "maxCyclomaticComplexity", cc)
                                   summary.SetAttributeValue(X "minCyclomaticComplexity", cc)
                                   summary.SetAttributeValue(X "numMethods", if s.MethodSeq > 0 then 1 else 0))
        handleSequencePoints ``method``
        tail

    let VisitAfterMethod (s : Context) methodDef track =
      AddTracking s methodDef track
      if s.Excluded = Nothing then
        let tail = VisitAfterMethodIncluded s
        {s with Stack = tail
                ClassSeq = s.ClassSeq + s.MethodSeq
                MethodCC = limitMethodCC s.MethodSeq s.MethodCC}
      else { s with Excluded = passOnClassExclusion s.Excluded }

    let VisitAfterType (s : Context) =
        let head, tail = Augment.Split s.Stack
        let (min, max), methods = s.MethodCC
                                  |> Seq.map (fun q -> (q |> Option.getOrElse 1, q |> Option.getOrElse 0), if q.IsSome then 1 else 0)
                                  |> Seq.fold (fun state pair ->
                                     let cc = fst state
                                     let cc2 = fst pair
                                     (Math.Min (fst cc, fst cc2), Math.Max (snd cc, snd cc2)), snd state + snd pair)
                                     ((1,0), 0)
        let classes = if s.ClassSeq > 0 then 1 else 0
        head.Parent.Elements(X "Summary")
        |> Seq.iter(fun summary -> summary.SetAttributeValue(X "numSequencePoints", s.ClassSeq)
                                   summary.SetAttributeValue(X "maxCyclomaticComplexity", max)
                                   summary.SetAttributeValue(X "minCyclomaticComplexity", Math.Min(min, max))
                                   summary.SetAttributeValue(X "numClasses", classes)
                                   summary.SetAttributeValue(X "numMethods", methods))
        {s with Stack = if s.Excluded = Nothing then tail else s.Stack
                ModuleSeq = s.ModuleSeq + s.ClassSeq
                ClassCC = ((if min = 0 then 1 else min), max) :: s.ClassCC
                ModuleMethods = methods + s.ModuleMethods
                ModuleClasses = classes + s.ModuleClasses
                Excluded = Nothing }

    let VisitAfterModule (s : Context) =
        let head,tail = Augment.Split s.Stack
        let min,max = s.ClassCC
                      |> Seq.fold (fun state pair -> Math.Min (fst state, fst pair), Math.Max (snd state, snd pair)) (1,0)
        head.Parent.Elements(X "Summary")
        |> Seq.iter(fun summary -> summary.SetAttributeValue(X "numSequencePoints", s.ModuleSeq)
                                   summary.SetAttributeValue(X "maxCyclomaticComplexity", max )
                                   summary.SetAttributeValue(X "minCyclomaticComplexity", min)
                                   summary.SetAttributeValue(X "numClasses", s.ModuleClasses)
                                   summary.SetAttributeValue(X "numMethods", s.ModuleMethods))

        let files = head.Parent.Elements(X "Files")
        s.Files
        |> Map.toSeq
        |> Seq.sortBy snd
        |> Seq.iter (fun (k,v) -> files |> Seq.iter (fun f -> f.Add(XElement(X "File",
                                                                             XAttribute(X "uid", v),
                                                                             XAttribute(X "fullPath", k)))))
        {s with Stack = tail
                TotalSeq = s.TotalSeq + s.ModuleSeq
                ModuleCC = (min, max) :: s.ModuleCC
                TotalClasses = s.ModuleClasses + s.TotalClasses
                TotalMethods = s.ModuleMethods + s.TotalMethods}

    let AfterAll (s : Context) =
      let head = s.Stack |> Seq.head
      let min,max = s.ModuleCC
                    |> Seq.fold (fun state pair -> Math.Min (fst state, fst pair), Math.Max (snd state, snd pair)) (1,0)
      head.Parent.Elements(X "Summary")
      |> Seq.iter(fun summary -> summary.SetAttributeValue(X "numSequencePoints", s.TotalSeq)
                                 summary.SetAttributeValue(X "maxCyclomaticComplexity", max )
                                 summary.SetAttributeValue(X "minCyclomaticComplexity", min)
                                 summary.SetAttributeValue(X "numClasses", s.TotalClasses)
                                 summary.SetAttributeValue(X "numMethods", s.TotalMethods))
      s

    let ReportVisitor (s : Context) (node:Node) =
      match node with
      | Start _ -> StartVisit s
      | Node.Module (moduleDef, included) -> VisitModule s moduleDef included
      | Node.Type (typeDef, included) -> VisitType s typeDef included
      | Node.Method (methodDef, included, _) -> VisitMethod s methodDef included
      | MethodPoint (_, codeSegment,  i, included) -> VisitMethodPoint' s codeSegment i included
      | AfterMethod (methodDef, _, track) -> VisitAfterMethod s methodDef track
      | AfterType _ ->   VisitAfterType s
      | AfterModule _ ->  VisitAfterModule s
      | Finish -> AfterAll s
      | _ -> s

    let result = Visitor.EncloseState ReportVisitor (Context.Build())
    (result, document)