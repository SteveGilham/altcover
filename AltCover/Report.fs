namespace AltCover

open System
open System.Xml.Linq
open Mono.Cecil

open Augment

module internal Report =

  let internal ReportGenerator() =
    let data =
      XProcessingInstruction("xml-stylesheet", """type="text/xsl" href="coverage.xsl" """) :> Object
    // The internal state of the document is mutated by the
    // operation of the visitor.  Everything else should now be pure
    let document = XDocument(XDeclaration("1.0", "utf-8", "yes"), [| data |])

    let ToExcluded included =
      if included then "false" else "true"

    let StartVisit(s : list<XElement>) =
      let element =
        XElement
          ("coverage".X,
           XAttribute
             ("profilerVersion".X,
              "AltCover "
              + (System.Diagnostics.FileVersionInfo.GetVersionInfo
                   (System.Reflection.Assembly.GetExecutingAssembly().Location)).FileVersion),
           XAttribute("driverVersion".X, 0),
           XAttribute
             ("startTime".X,
              DateTime.MaxValue.ToString
                ("o", System.Globalization.CultureInfo.InvariantCulture)),
           XAttribute
             ("measureTime".X,
              DateTime.MinValue.ToString
                ("o", System.Globalization.CultureInfo.InvariantCulture)))
      document.Add(element)
      element :: s

    let VisitModule (s : list<XElement>) (head : XElement) (moduleDef : ModuleDefinition)
        included =
      let element =
        XElement
          ("module".X, XAttribute("moduleId".X, moduleDef.Mvid.ToString()),
           XAttribute("name".X, moduleDef.Name),
           XAttribute("assembly".X, moduleDef.Assembly.Name.Name),
           XAttribute("assemblyIdentity".X, moduleDef.Assembly.Name.FullName),
           XAttribute("excluded".X, ToExcluded included))
      head.Add(element)
      element :: s

    let VisitMethod (s : list<XElement>) (head : XElement) (methodDef : MethodDefinition)
        included =
      let element =
        XElement
          ("method".X, XAttribute("name".X, methodDef.Name),
           //// Mono.Cecil emits names in the form outer/inner rather than outer+inner
           XAttribute("class".X, Naming.FullTypeName methodDef.DeclaringType),
           XAttribute("metadataToken".X, methodDef.MetadataToken.ToUInt32().ToString()),
           XAttribute("excluded".X, ToExcluded included),
           XAttribute
             ("instrumented".X,
              (if included then "true" else "false")),
           XAttribute("fullname".X, Naming.FullMethodName methodDef))
      head.Add(element)
      element :: s

    let VisitMethodPoint (s : list<XElement>) (head : XElement)
        (codeSegment' : SeqPnt option) included vc =
      match codeSegment' with
      | Some codeSegment ->
          let element =
            XElement
              ("seqpnt".X, XAttribute("visitcount".X, int vc),
               XAttribute("line".X, codeSegment.StartLine),
               XAttribute("column".X, codeSegment.StartColumn),
               XAttribute("endline".X, codeSegment.EndLine),
               XAttribute("endcolumn".X, codeSegment.EndColumn),
               XAttribute("excluded".X, ToExcluded included),
               XAttribute("document".X, codeSegment.Document |> Visitor.SourceLinkMapping))
          if head.IsEmpty then
            head.Add(element)
          else
            head.FirstNode.AddBeforeSelf(element)
      | None -> ()
      s

    let ReportVisitor (s : list<XElement>) (node : Node) =
      let head =
        if List.isEmpty s then null else s.Head

      let tail =
        if List.isEmpty s then [] else s.Tail

      match node with
      | Start _ -> StartVisit s
      | Module(moduleDef, included) ->
          VisitModule s head moduleDef (included.IsInstrumented)
      | Method(methodDef, included, _, _) ->
          VisitMethod s head methodDef (included.IsInstrumented)
      | MethodPoint(_, codeSegment, _, included, vc) ->
          VisitMethodPoint s head codeSegment included vc
      | AfterMethod _ ->
          if head.IsEmpty then head.Remove()
          tail
      | AfterModule _ -> tail
      | Finish -> s
      | _ -> s

    let result = Visitor.EncloseState ReportVisitor List.empty<XElement>
    (result, document)