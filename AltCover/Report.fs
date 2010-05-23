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

      | Module (moduleDef, moduleId,_,_) ->
          let element = new XElement(X "module",
                          new XAttribute(X "moduleId", moduleId),
                          new XAttribute(X "name", moduleDef.Name),
                          new XAttribute(X "assembly", moduleDef.Assembly.Name.Name),
                          new XAttribute(X "assemblyIdentity", moduleDef.Assembly.Name.FullName));
          s.Head.Add(element)   
          element :: s     

      | Method (methodDef, included, _) ->
          let element = new XElement(X "method",
                          new XAttribute(X "name", methodDef.Name),
                          new XAttribute(X "excluded", (not included).ToString().ToLowerInvariant()), //TODO -- replace lowering
                          new XAttribute(X "instrumented", included.ToString().ToLowerInvariant()),
                          //// Mono.Cecil emits names in the form outer/inner rather than outer+inner
                          new XAttribute(X "class", methodDef.DeclaringType.FullName.Replace('/', '+')),
                          new XAttribute(X "fullname", methodDef.FullName.Replace('/', '+')));
          s.Head.Add(element)   
          element :: s       
          
      | MethodPoint (_, codeSegment, _, included) ->          
          let element = new XElement(X "seqpnt",
                          new XAttribute(X "visitcount", 0),
                          new XAttribute(X "line", codeSegment.Line),
                          new XAttribute(X "column", codeSegment.Column),
                          new XAttribute(X "endline", codeSegment.EndLine),
                          new XAttribute(X "endcolumn", codeSegment.EndColumn),
                          new XAttribute(X "excluded", (not included).ToString().ToLowerInvariant()),
                          new XAttribute(X "document", codeSegment.Document));
          s.Head.Add(element)               
          s         
          
      | AfterMethod _ ->
          let m = s.Head
          let c = m.Descendants()
                  |> Seq.cast
                  |> Seq.length
          if c = 0 then m.Remove()
          s.Tail
          
      | AfterModule _ ->
          s.Tail
          
      | Finish -> s

      | _ -> s
      
    let result = Visitor.EncloseState ReportVisitor List.empty<XElement>  
    (result, document)