namespace AltCover

open System
open System.Xml.Linq

module Report =

  let mutable private document : XDocument = null
  let ReportDocument () = document

  let internal ReportGenerator () =
    let stack = ref List.empty<XElement>
    
    let X name =
      XName.Get(name)
    
    let ReportVisitor (node:Node) = 
      match node with
      | Start _ -> 
          let element = new XElement(X "coverage",
                          new XAttribute(X "profilerVersion", 0),
                          new XAttribute(X "driverVersion", 0),
                          new XAttribute(X "startTime", DateTime.MaxValue.ToString("o", System.Globalization.CultureInfo.InvariantCulture)),
                          new XAttribute(X "measureTime", DateTime.MinValue.ToString("o", System.Globalization.CultureInfo.InvariantCulture)))

          let data : array<Object> = [|new XProcessingInstruction("xml-stylesheet", "type='text/xsl' href='coverage.xsl'");
                                       element|]
          document <- new XDocument(new XDeclaration("1.0", "utf-8", "yes"), data)
          stack := element :: !stack

      | Module (moduleDef, moduleId,_,_) ->
          let element = new XElement(X "module",
                          new XAttribute(X "moduleId", moduleId + 1),
                          new XAttribute(X "name", moduleDef.Name),
                          new XAttribute(X "assembly", moduleDef.Assembly.Name.Name),
                          new XAttribute(X "assemblyIdentity", moduleDef.Assembly.Name.FullName));
          stack.Value.Head.Add(element)              
          stack := element :: !stack
          
      | Method (methodDef, included, _) ->
          let element = new XElement(X "method",
                          new XAttribute(X "name", methodDef.Name),
                          new XAttribute(X "excluded", (not included).ToString().ToLowerInvariant()),
                          new XAttribute(X "instrumented", included.ToString().ToLowerInvariant()),
                          //// Mono.Cecil emits names in the form outer/inner rather than outer+inner
                          new XAttribute(X "class", methodDef.DeclaringType.FullName.Replace('/', '+')),
                          new XAttribute(X "fullname", methodDef.FullName.Replace('/', '+')));
      
          stack.Value.Head.Add(element)              
          stack := element :: !stack
          
      | MethodPoint (_, codeSegment, _, included) ->          
          let element = new XElement(X "seqpnt",
                          new XAttribute(X "visitcount", 0),
                          new XAttribute(X "line", codeSegment.Line),
                          new XAttribute(X "column", codeSegment.Column),
                          new XAttribute(X "endline", codeSegment.EndLine),
                          new XAttribute(X "endcolumn", codeSegment.EndColumn),
                          new XAttribute(X "excluded", (not included).ToString().ToLowerInvariant()),
                          new XAttribute(X "document", codeSegment.Document));
      
          stack.Value.Head.Add(element)              

      | AfterMethod _ ->
          let m = stack.Value.Head
          let c = m.Descendants()
                  |> Seq.cast
                  |> Seq.length
          if c = 0 then m.Remove()
          stack := stack.Value.Tail
          
      | AfterModule _ ->
          stack := stack.Value.Tail
          
      | Finish ->
          () // TODO
          
      | _ -> ()
      
    ReportVisitor