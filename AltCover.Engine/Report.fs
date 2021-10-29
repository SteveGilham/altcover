namespace AltCover

open System
open System.Xml.Linq
open Mono.Cecil

module internal Report =

  let internal reportGenerator () =
    let data =
      XProcessingInstruction("xml-stylesheet", """type="text/xsl" href="coverage.xsl" """)
      :> Object
    // The internal state of the document is mutated by the
    // operation of the visitor.  Everything else should now be pure
    let document =
      XDocument(XDeclaration("1.0", "utf-8", "yes"), [| data |])

    let toExcluded included = if included then "false" else "true"

    let startVisit (s: list<XElement>) =
      let element =
        XElement(
          "coverage".X,
          XAttribute(
            "profilerVersion".X,
            "AltCover "
            + (System.Diagnostics.FileVersionInfo.GetVersionInfo(
                System
                  .Reflection
                  .Assembly
                  .GetExecutingAssembly()
                  .Location
            ))
              .FileVersion
          ),
          XAttribute("driverVersion".X, 0),
          XAttribute(
            "startTime".X,
            DateTime.MaxValue.ToString(
              "o",
              System.Globalization.CultureInfo.InvariantCulture
            )
          ),
          XAttribute(
            "measureTime".X,
            DateTime.MinValue.ToString(
              "o",
              System.Globalization.CultureInfo.InvariantCulture
            )
          )
        )

      document.Add(element)
      element :: s

    let visitModule (s: list<XElement>) (head: XElement) (moduleDef: ModuleEntry) =
      let def = moduleDef.Module

      let element =
        XElement(
          "module".X,
          XAttribute("moduleId".X, def.Mvid.ToString()),
          XAttribute("name".X, def.FileName |> canonicalPath),
          XAttribute("assembly".X, def.Assembly.Name.Name),
          XAttribute("assemblyIdentity".X, def.Assembly.Name.FullName),
          XAttribute("excluded".X, toExcluded moduleDef.Inspection.IsInstrumented)
        )

      head.Add(element)

      // embed support <altcover.file document="{@fullPath}" embed="{@altcover.embed}" />
      moduleDef.Module
      |> ProgramDatabase.getModuleDocuments
      |> Seq.iter (fun d -> let key = d.Url
                                      |> Visitor.sourceLinkMapping
                            d
                            |> Metadata.getSource
                            |> Option.iter (fun s -> let x = XElement("altcover.file".X,
                                                                      XAttribute("document".X, key),
                                                                      XAttribute("embed".X, s))
                                                     element.Add x))

      element :: s

    let visitMethod
      (s: list<XElement>)
      (head: XElement)
      (methodDef: MethodDefinition)
      included
      =
      let element =
        XElement(
          "method".X,
          XAttribute("name".X, methodDef.Name),
          // /// Mono.Cecil emits names in the form outer/inner rather than outer+inner
          XAttribute("class".X, Naming.fullTypeName methodDef.DeclaringType),
          XAttribute("metadataToken".X, methodDef.MetadataToken.ToUInt32().ToString()),
          XAttribute("excluded".X, toExcluded included),
          XAttribute("instrumented".X, (if included then "true" else "false")),
          XAttribute("fullname".X, Naming.fullMethodName methodDef)
        )

      (head.Elements("altcover.file".X) |> Seq.tryHead
       |> Option.map (fun x -> x.AddBeforeSelf)
       |> Option.defaultValue head.Add) [| element |]
      element :: s

    let visitMethodPoint (s: list<XElement>) (head: XElement) (e: StatementEntry) =
      match e.SeqPnt with
      | Some codeSegment ->
          let element =
            XElement(
              "seqpnt".X,
              XAttribute("visitcount".X, int e.DefaultVisitCount),
              XAttribute("line".X, codeSegment.StartLine),
              XAttribute("column".X, codeSegment.StartColumn),
              XAttribute("endline".X, codeSegment.EndLine),
              XAttribute("endcolumn".X, codeSegment.EndColumn),
              XAttribute("excluded".X, toExcluded e.Interesting),
              XAttribute("document".X, codeSegment.Document.Url |> Visitor.sourceLinkMapping)
            )

          if head.IsEmpty then
            head.Add(element)
          else
            head.FirstNode.AddBeforeSelf(element)
      | None -> ()

      s

    let reportVisitor (s: list<XElement>) (node: Node) =
      let head = if List.isEmpty s then null else s.Head

      let tail = if List.isEmpty s then [] else s.Tail

      match node with
      | Start _ -> startVisit s
      | Module m -> visitModule s head m
      | Method m -> visitMethod s head m.Method (m.Inspection.IsInstrumented)
      | MethodPoint m -> visitMethodPoint s head m
      | AfterMethod _ ->
          if head.IsEmpty then head.Remove()
          tail
      | AfterModule _ -> tail
      | Finish -> s
      | _ -> s

    let result =
      Visitor.encloseState reportVisitor List.empty<XElement>

    (result, (fun (s: System.IO.Stream) -> document.Save s)) // fsharplint:disable-line