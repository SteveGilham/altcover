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
                            Index : int }
  with static member Build () =
                    { Stack = List.empty<XElement>
                      Files = Map.empty<string, int>
                      Index = 0}

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

    let VisitModule (s : Context) (head:XElement) (moduleDef:ModuleDefinition) =
          let element = XElement(X "Module",
                          XAttribute(X "hash", KeyStore.HashFile moduleDef.FileName));
          head.Add(element)
          element.Add(XElement(X "Summary"))
          element.Add(XElement(X "ModulePath", moduleDef.FileName))
          element.Add(XElement(X "ModuleTime", File.GetLastWriteTimeUtc moduleDef.FileName))
          element.Add(XElement(X "ModuleName", moduleDef.Assembly.Name.Name))
          element.Add(XElement(X "Files"))
          let classes = XElement(X "Classes")
          element.Add(classes)
          {s with Stack = classes :: s.Stack
                  Files = Map.empty<string, int>}

    let VisitType (s : Context) (head:XElement) (typeDef:TypeDefinition) =
          let element = XElement(X "Class");
          head.Add(element)
          element.Add(XElement(X "Summary"))
          element.Add(XElement(X "FullName", Naming.FullTypeName typeDef))
          let methods = XElement(X "Methods")
          element.Add(methods)
          {s with Stack = methods :: s.Stack }

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

    let VisitMethod  (s : Context) (head:XElement) (methodDef:MethodDefinition) included =
          let element = methodElement methodDef
          head.Add element
          element.Add(XElement(X "Summary"))
          element.Add(XElement(X "MetadataToken", methodDef.MetadataToken.ToUInt32().ToString()))
          element.Add(XElement(X "Name", Naming.FullMethodName methodDef))
          element.Add(XElement(X "FileRef"))
          let seqpnts = XElement(X "SequencePoints")
          element.Add(seqpnts)
          element.Add(XElement(X "BranchPoints"))
          element.Add(XElement(X "MethodPoint"))
          {s with Stack = seqpnts :: s.Stack
                  Index = -1}

    let VisitMethodPoint (s : Context) (head:XElement) (codeSegment:Cil.SequencePoint) included =
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
                          XAttribute(X "sl", codeSegment.StartLine),
                          XAttribute(X "sc", codeSegment.StartColumn),
                          XAttribute(X "el", fst end'),
                          XAttribute(X "ec", snd end'),
                          XAttribute(X "fileid", ref))
          if head.IsEmpty then head.Add(element)
          else head.FirstNode.AddBeforeSelf(element)
          { s with Files = fileset
                   Index = ref }

    let ReportVisitor (s : Context) (node:Node) =
      let head = if s.Stack |> List.isEmpty then null else (s.Stack).Head
      let tail = if s.Stack |> List.isEmpty then [] else (s.Stack).Tail
      match node with
      | Start _ -> StartVisit s
      | Module (moduleDef,_ , _) -> VisitModule s head moduleDef
      | Type (typeDef,_, _) -> VisitType s head typeDef
      | Method (methodDef, _, included) -> VisitMethod s head methodDef included
      | MethodPoint (_, codeSegment,  _, included) ->
        VisitMethodPoint s head codeSegment included

      | AfterMethod _ ->
          let fileref = head.Parent.Descendants(X "FileRef") |> Seq.head
          if s.Index < 0 then
            fileref.Remove() // TODO method point

          else fileref.Add(XAttribute(X "uid", s.Index))
          {s with Stack = tail}

      | AfterType _ ->
          {s with Stack = tail}

      | AfterModule _ ->
          let files = head.Parent.Descendants(X "Files") |> Seq.head
          s.Files
          |> Map.toSeq
          |> Seq.sortBy snd
          |> Seq.iter (fun (k,v) -> files.Add(XElement(X "File",
                                                       XAttribute(X "uid", v),
                                                       XAttribute(X "fullPath", k))))
          {s with Stack = tail}

      | Finish -> s

      | _ -> s

    let result = Visitor.EncloseState ReportVisitor (Context.Build())
    (result, document)