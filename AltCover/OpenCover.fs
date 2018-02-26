namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Xml.Linq

open Mono.Cecil

module OpenCover =

  /// <summary>
  /// State object passed from visit to visit
  /// </summary>
  [<ExcludeFromCodeCoverage>]
  type internal Context = { Stack : XElement list
                            Files : Map<string, int>
                            Index : int
                            MethodSeq : int
                            ClassSeq : int
                            ModuleSeq : int
                            TotalSeq : int}
  with static member Build () =
                    { Stack = List.empty<XElement>
                      Files = Map.empty<string, int>
                      Index = 0
                      MethodSeq = 0
                      ClassSeq = 0
                      ModuleSeq = 0
                      TotalSeq = 0}

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
                  Files = Map.empty<string, int>
                  ModuleSeq = 0}

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
                  ClassSeq = 0}

    let boolString b = if b then "true" else "false"

    let methodElement (methodDef:MethodDefinition) =
         XElement(X "Method",
                           XAttribute(X "visited", "false"),
                           XAttribute(X "cyclomaticComplexity", "0"),
                           XAttribute(X "nPathComplexity", "0"),
                           XAttribute(X "sequenceCoverage", "0"),
                           XAttribute(X "branchCoverage", "0"),
                           XAttribute(X "isConstructor", boolString methodDef.IsConstructor),
                           XAttribute(X "isStatic", boolString methodDef.IsStatic),
                           XAttribute(X "isGetter", boolString methodDef.IsGetter),
                           XAttribute(X "isSetter", boolString methodDef.IsSetter))

    let VisitMethod  (s : Context) (methodDef:MethodDefinition) included =
          let element = methodElement methodDef
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
                  MethodSeq = 0}

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
          let element = XElement(X "SequencePoint",
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
      summary.Add(XAttribute(X "numSequencePoints", s.MethodSeq))
      head.Descendants(X "SequencePoint")
      |> Seq.iteri(fun i x -> x.SetAttributeValue(X "ordinal", i))
      {s with Stack = tail
              ClassSeq = s.ClassSeq + s.MethodSeq}

    let VisitAfterType (s : Context) =
      let head,tail = Augment.Split s.Stack
      let summary = head.Parent.Descendants(X "Summary") |> Seq.head
      summary.Add(XAttribute(X "numSequencePoints", s.ClassSeq))
      {s with Stack = tail
              ModuleSeq = s.ModuleSeq + s.ClassSeq}

    let VisitAfterModule (s : Context) =
      let head,tail = Augment.Split s.Stack
      let summary = head.Parent.Descendants(X "Summary") |> Seq.head
      summary.Add(XAttribute(X "numSequencePoints", s.ModuleSeq))
      let files = head.Parent.Descendants(X "Files") |> Seq.head
      s.Files
      |> Map.toSeq
      |> Seq.sortBy snd
      |> Seq.iter (fun (k,v) -> files.Add(XElement(X "File",
                                                  XAttribute(X "uid", v),
                                                  XAttribute(X "fullPath", k))))
      {s with Stack = tail
              TotalSeq = s.TotalSeq + s.ModuleSeq}

    let AfterAll (s : Context) =
      let head = s.Stack |> Seq.head
      let summary = head.Parent.Descendants(X "Summary") |> Seq.head
      summary.Add(XAttribute(X "numSequencePoints", s.TotalSeq))
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