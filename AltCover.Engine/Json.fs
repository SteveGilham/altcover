namespace AltCover

open System
open System.IO
open System.Xml.Linq
open System.Globalization

open Manatee.Json

// based on the sample file at https://raw.githubusercontent.com/jenkinsci/cobertura-plugin/master/src/test/resources/hudson/plugins/cobertura/coverage-with-data.xml
[<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1704",
    Justification="'Json' is jargon")>]
module internal Json =
  let internal path : Option<string> ref = ref None

  let simpleAttributeToValue (a:XAttribute) =
    let value = a.Value
    let b,v = Double.TryParse value
    if b then JsonValue v
    else
      let b2, v2 = Boolean.TryParse value
      if b2 then JsonValue v2
      else JsonValue value

  let simpleElementToJSon (xElement : XElement) =
    let element = JsonObject()
    if xElement.HasAttributes
    then
      xElement.Attributes()
      |> Seq.iter(fun (a:XAttribute) ->
        if a.Name.ToString().StartsWith("{", StringComparison.Ordinal) |> not
        then
         let attribute = simpleAttributeToValue a
         element.Add (a.Name.LocalName, attribute))
    JsonValue element

  let addMethodSeqpnts (mjson:JsonValue) (m:XElement) =
    let seqpnts = JsonArray()
    m.Descendants(XName.Get "seqpnt")
    |> Seq.iter(fun sp ->
      let spjson = simpleElementToJSon sp
      seqpnts.Add spjson
    )
    if seqpnts.Count > 0
    then mjson.Object.Add("seqpnt", JsonValue seqpnts)

  let addModuleMethods (mjson:JsonValue) (m:XElement) =
    let methods = JsonArray()
    m.Descendants(XName.Get "method")
    |> Seq.iter(fun m2 ->
      let m2json = simpleElementToJSon m2
      addMethodSeqpnts m2json m
      methods.Add m2json
    )
    if methods.Count > 0
    then mjson.Object.Add("method", JsonValue methods)

  let addTerminalGroup group item (json:JsonValue) (x:XElement) =
    let items = JsonArray()
    x.Descendants(XName.Get group)
    |> Seq.collect (fun f -> f.Descendants(XName.Get item))
    |> Seq.iter(fun m2 ->
      let m2json = simpleElementToJSon m2
      items.Add m2json
    )
    if items.Count > 0
    then json.Object.Add(item, JsonValue items)

  let addMethodPoints group item (mjson:JsonValue) (m:XElement) =
    let points = JsonArray()
    m.Descendants(XName.Get group)
    |> Seq.collect (fun c -> c.Descendants(XName.Get item))
    |> Seq.iter(fun x ->
      let point = simpleElementToJSon x
      points.Add point
      addTerminalGroup "Times" "Time"  point x
      addTerminalGroup "TrackedMethodRefs" "TrackedMethodRef"  point x
    )
    if points.Count > 0
    then mjson.Object.Add(item, JsonValue points)

  let addClassMethods (mjson:JsonValue) (m:XElement) =
    let methods = JsonArray()
    m.Descendants(XName.Get "Methods")
    |> Seq.collect (fun c -> c.Descendants(XName.Get "Method"))
    |> Seq.iter(fun x ->
      let ``method`` = simpleElementToJSon x
      methods.Add ``method``
      [
        "Summary"
        "FileRef"
        "MethodPoint"
      ]
      |> Seq.iter (fun name ->
      x.Elements(XName.Get name)
      |> Seq.iter (fun s -> let js = simpleElementToJSon s
                            ``method``.Object.Add(name, JsonValue js)))

      [
        "MetadataToken"
        "Name"
      ]
      |> Seq.iter  (fun tag ->
        x.Elements(XName.Get tag)
        |> Seq.iter (fun s -> let js = s.Value
                              ``method``.Object.Add(tag, JsonValue js)))
      addMethodPoints "SequencePoints" "SequencePoint"  ``method`` x
      addMethodPoints "BranchPoints" "BranchPoint" ``method`` x
    )
    if methods.Count > 0
    then mjson.Object.Add("Method", JsonValue methods)

  let addModuleClasses (mjson:JsonValue) (m:XElement) =
    let classes = JsonArray()
    m.Descendants(XName.Get "Classes")
    |> Seq.collect (fun c -> c.Descendants(XName.Get "Class"))
    |> Seq.iter(fun c ->
      let ``class`` = simpleElementToJSon c
      classes.Add ``class``
      c.Elements(XName.Get "Summary")
      |> Seq.iter (fun s -> let js = simpleElementToJSon s
                            ``class``.Object.Add("Summary", JsonValue js))

      [
        "FullName"
      ]
      |> Seq.iter  (fun tag ->
        c.Elements(XName.Get tag)
        |> Seq.iter (fun s -> let js = s.Value
                              ``class``.Object.Add(tag, JsonValue js)))
      addClassMethods ``class`` c
    )
    if classes.Count > 0
    then mjson.Object.Add("Class", JsonValue classes)

  let ncoverToJson report =
    let json = simpleElementToJSon report
    let modules = JsonArray()
    report.Descendants(XName.Get "module")
    |> Seq.iter(fun m ->
      let mjson = simpleElementToJSon m
      addModuleMethods mjson m
      modules.Add mjson
    )

    if modules.Count > 0
    then json.Object.Add("module", JsonValue modules)

    let jo = JsonObject()
    jo.Add("coverage", json)
    let jv = JsonValue jo
    jv

  let opencoverToJson report =
    let json = simpleElementToJSon report
    report.Elements(XName.Get "Summary")
    |> Seq.iter (fun s -> let js = simpleElementToJSon s
                          json.Object.Add("Summary", JsonValue js))

    let modules = JsonArray()
    report.Descendants(XName.Get "Module")
    |> Seq.iter(fun m ->
      let mjson = simpleElementToJSon m
      m.Elements(XName.Get "Summary")
      |> Seq.iter (fun s -> let js = simpleElementToJSon s
                            mjson.Object.Add("Summary", JsonValue js))

      [
        "FullName"
        "ModulePath"
        "ModuleTime"
        "ModuleName"
      ]
      |> Seq.iter  (fun tag ->
        m.Elements(XName.Get tag)
        |> Seq.iter (fun s -> let js = s.Value
                              mjson.Object.Add(tag, JsonValue js)))

      addTerminalGroup "Files" "File" mjson m
      addModuleClasses mjson m
      addTerminalGroup "TrackedMethods" "TrackedMethod" mjson m

      modules.Add mjson
    )
    if modules.Count > 0
    then json.Object.Add("Module", JsonValue modules)

    let jo = JsonObject()
    jo.Add("CoverageSession", json)
    let jv = JsonValue jo
    jv

  let internal convertReport (report : XDocument) (format:ReportFormat) (stream : Stream) =
    doWithStream (fun () -> new StreamWriter(stream)) (fun writer ->
        (report.Root
        |> (match format with
            | ReportFormat.NCover -> ncoverToJson
            | _ -> opencoverToJson)).ToString() // ready minified
        |> writer.Write)

  let internal summary (report : XDocument) (format : ReportFormat) result =
    doWithStream(fun () -> File.OpenWrite(!path |> Option.get))
      (convertReport report format)
    (result, 0uy, String.Empty)