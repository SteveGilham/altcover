namespace AltCover

open System
open System.Xml.Linq

module Report =

  let internal ReportGenerator () =

    let data = XProcessingInstruction(
                   "xml-stylesheet",
                   "type='text/xsl' href='coverage.xsl'") :> Object

    // The internal state of the document is mutated by the
    // operation of the visitor.  Everything else should now be pure
    let document = XDocument(XDeclaration("1.0", "utf-8", "yes"), [|data|])

    let X name =
      XName.Get(name)

    let ReportVisitor (s : list<XElement>) (node:Node) =
      let head = if List.isEmpty s then null else s.Head
      match node with
      | Start _ ->
          let element = XElement(X "coverage",
                          XAttribute(X "profilerVersion", 0),
                          XAttribute(X "driverVersion", 0),
                          XAttribute(X "startTime", DateTime.MaxValue.ToString("o", System.Globalization.CultureInfo.InvariantCulture)),
                          XAttribute(X "measureTime", DateTime.MinValue.ToString("o", System.Globalization.CultureInfo.InvariantCulture)))
          document.Add(element)
          element :: s

      | Module (moduleDef, _) ->
          let element = XElement(X "module",
                          XAttribute(X "moduleId", moduleDef.Mvid.ToString()),
                          XAttribute(X "name", moduleDef.Name),
                          XAttribute(X "assembly", moduleDef.Assembly.Name.Name),
                          XAttribute(X "assemblyIdentity", moduleDef.Assembly.Name.FullName));
          head.Add(element)
          element :: s

      | Method (methodDef, included) ->
          let element = XElement(X "method",
                          XAttribute(X "name", methodDef.Name),
                          //// Mono.Cecil emits names in the form outer/inner rather than outer+inner
                          XAttribute(X "class", Naming.FullTypeName methodDef.DeclaringType),
                          XAttribute(X "metadataToken", methodDef.MetadataToken.ToUInt32().ToString()),
                          XAttribute(X "excluded", if included then "false" else "true"),
                          XAttribute(X "instrumented", if included then "true" else "false"),
                          XAttribute(X "fullname", Naming.FullMethodName methodDef)
                              )

          head.Add(element)
          element :: s

      | MethodPoint (_, codeSegment, _, included) ->
          // quick fix for .mdb lack of end line/column information
          let end' = match (codeSegment.EndLine, codeSegment.EndColumn) with
                     | (-1, _) -> (codeSegment.StartLine, codeSegment.StartColumn + 1)
                     | endPair -> endPair
          let element = XElement(X "seqpnt",
                          XAttribute(X "visitcount", 0),
                          XAttribute(X "line", codeSegment.StartLine),
                          XAttribute(X "column", codeSegment.StartColumn),
                          XAttribute(X "endline", fst end'),
                          XAttribute(X "endcolumn", snd end'),
                          XAttribute(X "excluded", (not included).ToString().ToLowerInvariant()),
                          XAttribute(X "document", codeSegment.Document.Url));
          if head.IsEmpty then head.Add(element)
          else head.FirstNode.AddBeforeSelf(element)
          s

      | AfterMethod _ ->
          if head.IsEmpty then head.Remove()
          s.Tail

      | AfterModule _ ->
          s.Tail

      | Finish -> s

      | _ -> s

    let result = Visitor.EncloseState ReportVisitor List.empty<XElement>
    (result, document)