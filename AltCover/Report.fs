namespace AltCover

open System
open System.Xml.Linq
open Mono.Cecil

module internal Report =

  let internal ReportGenerator() =
    let data =
      XProcessingInstruction(
        "xml-stylesheet",
        """type="text/xsl" href="coverage.xsl" """) :> Object
    // The internal state of the document is mutated by the
    // operation of the visitor.  Everything else should now be pure
    let document = XDocument(XDeclaration("1.0", "utf-8", "yes"), [| data |])
    let X name = XName.Get(name)

    let ToExcluded included =
      if included then "false"
      else "true"

    let StartVisit(s : list<XElement>) =
      let element =
        XElement
          (X "coverage",
           XAttribute
             (X "profilerVersion",
              "AltCover "
              + System.Diagnostics.FileVersionInfo.GetVersionInfo(
                  System.Reflection.Assembly.GetExecutingAssembly().Location).FileVersion),
           XAttribute(X "driverVersion", 0),
           XAttribute
             (X "startTime",
              DateTime.MaxValue.ToString
                ("o", System.Globalization.CultureInfo.InvariantCulture)),
           XAttribute
             (X "measureTime",
              DateTime.MinValue.ToString
                ("o", System.Globalization.CultureInfo.InvariantCulture)))
      document.Add(element)
      element :: s

    let VisitModule (s : list<XElement>) (head : XElement) (moduleDef : ModuleDefinition)
        included =
      let element =
        XElement
          (X "module", XAttribute(X "moduleId", moduleDef.Mvid.ToString()),
           XAttribute(X "name", moduleDef.Name),
           XAttribute(X "assembly", moduleDef.Assembly.Name.Name),
           XAttribute(X "assemblyIdentity", moduleDef.Assembly.Name.FullName),
           XAttribute(X "excluded", ToExcluded included))
      head.Add(element)
      element :: s

    let VisitMethod (s : list<XElement>) (head : XElement) (methodDef : MethodDefinition)
        included =
      let element =
        XElement
          (X "method", XAttribute(X "name", methodDef.Name),
           //// Mono.Cecil emits names in the form outer/inner rather than outer+inner
           XAttribute(X "class", Naming.FullTypeName methodDef.DeclaringType),
           XAttribute(X "metadataToken", methodDef.MetadataToken.ToUInt32().ToString()),
           XAttribute(X "excluded", ToExcluded included),
           XAttribute(X "instrumented",
                      if included then "true"
                      else "false"),
           XAttribute(X "fullname", Naming.FullMethodName methodDef))
      head.Add(element)
      element :: s

    let VisitMethodPoint (s : list<XElement>) (head : XElement)
        (codeSegment' : SeqPnt option) included =
      match codeSegment' with
      | Some codeSegment ->
        let element =
          XElement
            (X "seqpnt", XAttribute(X "visitcount", 0),
             XAttribute(X "line", codeSegment.StartLine),
             XAttribute(X "column", codeSegment.StartColumn),
             XAttribute(X "endline", codeSegment.EndLine),
             XAttribute(X "endcolumn", codeSegment.EndColumn),
             XAttribute(X "excluded", ToExcluded included),
             XAttribute(X "document", codeSegment.Document))
        if head.IsEmpty then head.Add(element)
        else head.FirstNode.AddBeforeSelf(element)
      | None -> ()
      s

    let ReportVisitor (s : list<XElement>) (node : Node) =
      let head =
        if List.isEmpty s then null
        else s.Head

      let tail =
        if List.isEmpty s then []
        else s.Tail

      match node with
      | Start _ -> StartVisit s
      | Module(moduleDef, included) ->
        VisitModule s head moduleDef (Visitor.IsInstrumented included)
      | Method(methodDef, included, _) ->
        VisitMethod s head methodDef (Visitor.IsInstrumented included)
      | MethodPoint(_, codeSegment, _, included) ->
        VisitMethodPoint s head codeSegment included
      | AfterMethod _ ->
        if head.IsEmpty then head.Remove()
        tail
      | AfterModule _ -> tail
      | Finish -> s
      | _ -> s

    let result = Visitor.EncloseState ReportVisitor List.empty<XElement>
    (result, document)