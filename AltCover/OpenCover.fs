namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Xml.Linq

open Mono.Cecil
open Mono.Cecil.Cil

#if WEAKNAMETESTS
#else
open AltCover.Augment
#endif

module OpenCover =

#if WEAKNAMETESTS
#else

  /// <summary>
  /// State object passed from visit to visit
  /// </summary>
  [<ExcludeFromCodeCoverage>]
  type internal Context = { Stack : XElement list
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
#endif

  // OpenCover uses Gendarme to compute Cyclomatic Complexity values.  Reimplement that algorithm here
  let mask = [  0xFFFF6C3FCUL
                0x1B0300000000FFE0UL
                0x400100FFF800UL
                0xDE0UL ]

  let FindFirstUnconditionalBranchTarget (ins:Cil.Instruction) =
     Seq.unfold (fun (state:Cil.Instruction) -> if isNull state then None else Some (state, state.Next)) ins
     |> Seq.tryFind (fun i -> i.OpCode.FlowControl = FlowControl.Branch)
     |> Option.map (fun i -> i.Operand :?> Cil.Instruction)

  let AccumulateSwitchTargets (ins:Cil.Instruction) (targets:System.Collections.Generic.HashSet<Cil.Instruction>) =
    let cases = ins.Operand :?> Cil.Instruction[]
    cases |> Seq.iter(fun target -> if target <> ins.Next then target |> targets.Add |> ignore)

    // add 'default' branch (if one exists)
    let next = ins.Next
    if next.OpCode.FlowControl = FlowControl.Branch then
      let operand = next.Operand :?> Cil.Instruction
      match cases |> Seq.head |> FindFirstUnconditionalBranchTarget with
      | Some unc when unc = operand -> ()
      | _ -> operand |> targets.Add |> ignore

  let ``detect ternary pattern`` code =
    let index = int code
    if mask |> Seq.skip (index >>> 6) |> Seq.head &&& (1UL <<< (index &&& 63)) = 0UL then 0 else 1

  let SwitchCyclomaticComplexity (instructions:Cil.Instruction seq) =
    let targets = System.Collections.Generic.HashSet<Cil.Instruction>()
    let fast = instructions
               |> Seq.fold (fun c i ->
                                match i.OpCode.FlowControl with
                                | FlowControl.Branch ->
                                    let previous = i.Previous
                                    c + if previous |> isNull |> not then
                                          do if previous.OpCode.FlowControl = FlowControl.Cond_Branch then
                                              match previous.Operand with
                                              | :? (Cil.Instruction array) -> ()
                                              | :? Cil.Instruction as branch ->
                                                 if targets.Contains branch then i |> targets.Add |> ignore
                                              | _ -> ()
                                          ``detect ternary pattern`` previous.OpCode.Code else 0
                                | FlowControl.Cond_Branch ->
                                    if i.OpCode = OpCodes.Switch then
                                      AccumulateSwitchTargets i targets
                                      c
                                    else
                                      let branch = i.Operand :?> Cil.Instruction
                                      let previous = branch.Previous
                                      c + if previous |> isNull |> not &&
                                             previous.Previous.OpCode.Code <> OpCodes.Switch.Code &&
                                             branch |> targets.Contains |> not
                                          then 1 else 0
                                | _ -> c ) 1
    fast + targets.Count

  let CyclomaticComplexity (m:MethodDefinition) =
    if m.HasBody then
        let instructions = m.Body.Instructions
                           |> Seq.cast<Cil.Instruction>
        match instructions |> Seq.tryFind (fun i -> i.OpCode = OpCodes.Switch) with
        | None ->
           instructions
            |> Seq.fold (fun c i -> match i.OpCode.FlowControl with
                                    | FlowControl.Cond_Branch ->
                                      c + 1
                                    | FlowControl.Branch ->
                                      let previous = i.Previous
                                      c + if previous |> isNull |> not then
                                             ``detect ternary pattern`` previous.OpCode.Code else 0
                                    | _ -> c ) 1
        | _ -> SwitchCyclomaticComplexity instructions
    else 1

#if WEAKNAMETESTS
#else

  let internal ReportGenerator () =

    // The internal state of the document is mutated by the
    // operation of the visitor.  Everything else should now be pure
    let document = XDocument(XDeclaration("1.0", "utf-8", "yes"), [||])

    let X name =
      XName.Get(name)

    let StartVisit (s : Context) =
          let element = XElement(X "CoverageSession")
          document.Add(element)
          element.Add(XElement(X "Summary"))
          let modules = XElement(X "Modules")
          element.Add(modules)
          {s with Stack = modules :: s.Stack }

    let VisitModule (s : Context) (moduleDef:ModuleDefinition) included =
          let element = XElement(X "Module",
                          XAttribute(X "hash", KeyStore.HashFile moduleDef.FileName))
          if not included then element.SetAttributeValue(X "skippedDueTo", "Filter")
          let head = s.Stack |> Seq.head
          head.Add(element)
          element.Add(XElement(X "Summary"))
          element.Add(XElement(X "ModulePath", moduleDef.FileName))
          element.Add(XElement(X "ModuleTime", File.GetLastWriteTimeUtc moduleDef.FileName))
          element.Add(XElement(X "ModuleName", moduleDef.Assembly.Name.Name))
          element.Add(XElement(X "Files"))
          let classes = XElement(X "Classes")
          element.Add(classes)
          {s with Stack = classes :: s.Stack
                  ModuleSeq = 0
                  ModuleMethods = 0
                  ModuleClasses = 0
                  ClassCC = []}

    let VisitType (s : Context) (typeDef:TypeDefinition) included =
          let element = XElement(X "Class")
          if not included then element.SetAttributeValue(X "skippedDueTo", "Filter")
          let head = s.Stack |> Seq.head
          head.Add(element)
          element.Add(XElement(X "Summary"))
          element.Add(XElement(X "FullName", typeDef.FullName))
          let methods = XElement(X "Methods")
          element.Add(methods)
          {s with Stack = methods :: s.Stack
                  ClassSeq = 0
                  MethodCC = []}

    let boolString b = if b then "true" else "false"

    let methodElement (methodDef:MethodDefinition) =
         let cc = CyclomaticComplexity methodDef
         (cc, XElement(X "Method",
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
          let cc, element = methodElement methodDef
          if not included then element.SetAttributeValue(X "skippedDueTo", "Filter")
          let head = s.Stack |> Seq.head
          head.Add element
          element.Add(XElement(X "Summary"))
          element.Add(XElement(X "MetadataToken", methodDef.MetadataToken.ToUInt32().ToString()))
          element.Add(XElement(X "Name", methodDef.FullName))
          element.Add(XElement(X "FileRef"))
          let seqpnts = XElement(X "SequencePoints")
          element.Add(seqpnts)
          element.Add(XElement(X "BranchPoints"))
          element.Add(XElement(X "MethodPoint"))
          {s with Stack = seqpnts :: s.Stack
                  Index = -1
                  MethodSeq = 0
                  MethodCC = Some cc :: s.MethodCC}

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

    let VisitAfterMethod (s : Context) =
      let head,tail = Augment.Split s.Stack
      let fileref = head.Parent.Descendants(X "FileRef") |> Seq.head
      if s.Index < 0 then
        fileref.Remove() // TODO method point
      else fileref.Add(XAttribute(X "uid", s.Index))
      let summary = head.Parent.Descendants(X "Summary") |> Seq.head
      let cc = Option.getOrElse 1 s.MethodCC.Head
      summary.Add(XAttribute(X "numSequencePoints", s.MethodSeq))
      summary.Add(XAttribute(X "visitedSequencePoints", 0))
      summary.Add(XAttribute(X "numBranchPoints", 0))
      summary.Add(XAttribute(X "visitedBranchPoints", 0))
      summary.Add(XAttribute(X "sequenceCoverage", 0))
      summary.Add(XAttribute(X "branchCoverage", 0))
      summary.Add(XAttribute(X "maxCyclomaticComplexity", cc))
      summary.Add(XAttribute(X "minCyclomaticComplexity", cc))
      summary.Add(XAttribute(X "visitedClasses", 0))
      summary.Add(XAttribute(X "numClasses", 0))
      summary.Add(XAttribute(X "visitedMethods", 0))
      summary.Add(XAttribute(X "numMethods", if s.MethodSeq > 0 then 1 else 0))
      head.Descendants(X "SequencePoint")
      |> Seq.iteri(fun i x -> x.SetAttributeValue(X "ordinal", i))
      {s with Stack = tail
              ClassSeq = s.ClassSeq + s.MethodSeq
              MethodCC = if s.MethodSeq > 0 then s.MethodCC 
                         else None :: s.MethodCC.Tail}

    let VisitAfterType (s : Context) =
      let head,tail = Augment.Split s.Stack
      let summary = head.Parent.Descendants(X "Summary") |> Seq.head
      let (min, max), methods = s.MethodCC
                                |> Seq.map (fun q -> (q |> Option.getOrElse 1, q |> Option.getOrElse 0), if q.IsSome then 1 else 0)
                                |> Seq.fold (fun state pair -> 
                                   let cc = fst state
                                   let cc2 = fst pair
                                   (Math.Min (fst cc, fst cc2), Math.Max (snd cc, snd cc2)), snd state + snd pair)
                                   ((1,0), 0)
      let classes = if s.ClassSeq > 0 then 1 else 0
      summary.Add(XAttribute(X "numSequencePoints", s.ClassSeq))
      summary.Add(XAttribute(X "visitedSequencePoints", 0))
      summary.Add(XAttribute(X "numBranchPoints", 0))
      summary.Add(XAttribute(X "visitedBranchPoints", 0))
      summary.Add(XAttribute(X "sequenceCoverage", 0))
      summary.Add(XAttribute(X "branchCoverage", 0))
      summary.Add(XAttribute(X "maxCyclomaticComplexity", max))
      summary.Add(XAttribute(X "minCyclomaticComplexity", Math.Min(min, max)))
      summary.Add(XAttribute(X "visitedClasses", 0))
      summary.Add(XAttribute(X "numClasses", classes))
      summary.Add(XAttribute(X "visitedMethods", 0))
      summary.Add(XAttribute(X "numMethods", methods))
      {s with Stack = tail
              ModuleSeq = s.ModuleSeq + s.ClassSeq
              ClassCC = ((if min = 0 then 1 else min), max) :: s.ClassCC
              ModuleMethods = methods + s.ModuleMethods
              ModuleClasses = classes + s.ModuleClasses}

    let VisitAfterModule (s : Context) =
      let head,tail = Augment.Split s.Stack
      let summary = head.Parent.Descendants(X "Summary") |> Seq.head
      let min,max = s.ClassCC
                    |> Seq.fold (fun state pair -> Math.Min (fst state, fst pair), Math.Max (snd state, snd pair)) (1,0)
      summary.Add(XAttribute(X "numSequencePoints", s.ModuleSeq))
      summary.Add(XAttribute(X "visitedSequencePoints", 0))
      summary.Add(XAttribute(X "numBranchPoints", 0))
      summary.Add(XAttribute(X "visitedBranchPoints", 0))
      summary.Add(XAttribute(X "sequenceCoverage", 0))
      summary.Add(XAttribute(X "branchCoverage", 0))
      summary.Add(XAttribute(X "maxCyclomaticComplexity", max ))
      summary.Add(XAttribute(X "minCyclomaticComplexity", min))
      summary.Add(XAttribute(X "visitedClasses", 0))
      summary.Add(XAttribute(X "numClasses", s.ModuleClasses))
      summary.Add(XAttribute(X "visitedMethods", 0))
      summary.Add(XAttribute(X "numMethods", s.ModuleMethods))

      let files = head.Parent.Descendants(X "Files") |> Seq.head
      s.Files
      |> Map.toSeq
      |> Seq.sortBy snd
      |> Seq.iter (fun (k,v) -> files.Add(XElement(X "File",
                                                  XAttribute(X "uid", v),
                                                  XAttribute(X "fullPath", k))))
      {s with Stack = tail
              TotalSeq = s.TotalSeq + s.ModuleSeq
              ModuleCC = (min, max) :: s.ModuleCC
              TotalClasses = s.ModuleClasses + s.TotalClasses
              TotalMethods = s.ModuleMethods + s.TotalMethods}

    let AfterAll (s : Context) =
      let head = s.Stack |> Seq.head
      let summary = head.Parent.Descendants(X "Summary") |> Seq.head
      let min,max = s.ModuleCC
                    |> Seq.fold (fun state pair -> Math.Min (fst state, fst pair), Math.Max (snd state, snd pair)) (1,0)
      summary.Add(XAttribute(X "numSequencePoints", s.TotalSeq))
      summary.Add(XAttribute(X "visitedSequencePoints", 0))
      summary.Add(XAttribute(X "numBranchPoints", 0))
      summary.Add(XAttribute(X "visitedBranchPoints", 0))
      summary.Add(XAttribute(X "sequenceCoverage", 0))
      summary.Add(XAttribute(X "branchCoverage", 0))
      summary.Add(XAttribute(X "maxCyclomaticComplexity", max ))
      summary.Add(XAttribute(X "minCyclomaticComplexity", min))
      summary.Add(XAttribute(X "visitedClasses", 0))
      summary.Add(XAttribute(X "numClasses", s.TotalClasses))
      summary.Add(XAttribute(X "visitedMethods", 0))
      summary.Add(XAttribute(X "numMethods", s.TotalMethods))
      s

    let ReportVisitor (s : Context) (node:Node) =
      match node with
      | Start _ -> StartVisit s
      | Module (moduleDef, _ , included) -> VisitModule s moduleDef included
      | Type (typeDef, _, included) -> VisitType s typeDef included
      | Method (methodDef, _, included) -> VisitMethod s methodDef included
      | MethodPoint (_, codeSegment,  i, included) ->
        VisitMethodPoint s codeSegment i included
      | AfterMethod _ -> VisitAfterMethod s
      | AfterType _ ->   VisitAfterType s
      | AfterModule _ ->  VisitAfterModule s
      | Finish -> AfterAll s
      | _ -> s

    let result = Visitor.EncloseState ReportVisitor (Context.Build())
    (result, document)
#endif