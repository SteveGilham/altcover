namespace AltCover

open System
open System.Xml.Linq

module Report =

  let internal ReportGenerator () =
    let initialState = List.empty<XElement>
    let data = new XProcessingInstruction(
                   "xml-stylesheet",
                   "type='text/xsl' href='coverage.xsl'") :> Object
                   
    // The internal state of the document is mutated by the 
    // operation of the visitor.  Everything else should now be pure
    let document = new XDocument(new XDeclaration("1.0", "utf-8", "yes"), [|data|])
    
    let X name =
      XName.Get(name)
    
    let ReportVisitor (s : list<XElement>) (node:Node) = 
      match node with
      | Start _ -> 
          let element = new XElement(X "coverage",
                          new XAttribute(X "profilerVersion", 0),
                          new XAttribute(X "driverVersion", 0),
                          new XAttribute(X "startTime", DateTime.MaxValue.ToString("o", System.Globalization.CultureInfo.InvariantCulture)),
                          new XAttribute(X "measureTime", DateTime.MinValue.ToString("o", System.Globalization.CultureInfo.InvariantCulture)))
          document.Add(element)
          element :: s

      | Module (moduleDef, _) ->
          let element = new XElement(X "module",
                          new XAttribute(X "moduleId", moduleDef.Mvid.ToString()),
                          new XAttribute(X "name", moduleDef.Name),
                          new XAttribute(X "assembly", moduleDef.Assembly.Name.Name),
                          new XAttribute(X "assemblyIdentity", moduleDef.Assembly.Name.FullName));
          s.Head.Add(element)   
          element :: s     

      | Method (methodDef, included) ->
          let element = new XElement(X "method",
                          new XAttribute(X "name", methodDef.Name),
                          //// Mono.Cecil emits names in the form outer/inner rather than outer+inner
                          new XAttribute(X "class", methodDef.DeclaringType.FullName.Replace('/', '+')), 
                          new XAttribute(X "metadataToken", methodDef.MetadataToken.ToUInt32().ToString()),                         
                          new XAttribute(X "excluded", if included then "false" else "true"),
                          new XAttribute(X "instrumented", if included then "true" else "false"),
                          new XAttribute(X "fullname", Naming.FullMethodName methodDef)
                              )

          s.Head.Add(element)   
          element :: s       
          
      | MethodPoint (_, codeSegment, _, included) ->
          let element = new XElement(X "seqpnt",
                          new XAttribute(X "visitcount", 0),
                          new XAttribute(X "line", codeSegment.StartLine),
                          new XAttribute(X "column", codeSegment.StartColumn),
                          new XAttribute(X "endline", codeSegment.EndLine),
                          new XAttribute(X "endcolumn", codeSegment.EndColumn),
                          new XAttribute(X "excluded", (not included).ToString().ToLowerInvariant()),
                          new XAttribute(X "document", codeSegment.Document.Url));
          if s.Head.IsEmpty then s.Head.Add(element)
          else s.Head.FirstNode.AddBeforeSelf(element)                
          s         
          
      | AfterMethod _ ->
          let m = s.Head
          if m.IsEmpty then m.Remove()
          s.Tail
          
      | AfterModule _ ->
          s.Tail
          
      | Finish -> s

      | _ -> s
      
    let result = Visitor.EncloseState ReportVisitor List.empty<XElement>  
    (result, document)