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

  let internal simpleAttributeToValue (a:XAttribute) =
    let value = a.Value
    let b,v = Double.TryParse value
    if b then JsonValue v
    else
      let b2, v2 = Boolean.TryParse value
      if b2 then JsonValue v2
      else JsonValue value

  let internal mappedElementToJSon mappings (xElement : XElement) =
    let element = JsonObject()
    if xElement.HasAttributes
    then
      xElement.Attributes()
      |> Seq.iter(fun (a:XAttribute) ->
        if a.Name.ToString().StartsWith("{", StringComparison.Ordinal) |> not
        then
          let map = mappings
                    |> Seq.tryFind (fun (n,_) -> n = a.Name.LocalName)
                    |> Option.map snd
                    |> Option.defaultValue (simpleAttributeToValue >> Some)
          map a
          |> Option.iter (fun v -> element.Add (a.Name.LocalName, v)))
    JsonValue element

  let internal simpleElementToJSon (xElement : XElement) =
    mappedElementToJSon [] xElement

  let internal addMethodSeqpnts (mjson:JsonValue) (m:XContainer) =
    let seqpnts = JsonArray()
    m.Descendants(XName.Get "seqpnt")
    |> Seq.iter(fun sp ->
      let spjson = simpleElementToJSon sp
      seqpnts.Add spjson
    )
    if seqpnts.Count > 0
    then mjson.Object.Add("seqpnt", JsonValue seqpnts)

  let internal addModuleMethods (mjson:JsonValue) (m:XElement) =
    let methods = JsonArray()
    m.Descendants(XName.Get "method")
    |> Seq.iter(fun m2 ->
      let m2json = simpleElementToJSon m2
      addMethodSeqpnts m2json m
      methods.Add m2json
    )
    if methods.Count > 0
    then mjson.Object.Add("method", JsonValue methods)

  [<System.Diagnostics.CodeAnalysis.SuppressMessage(
    "Gendarme.Rules.Smells", "AvoidLongParameterListsRule",
    Justification = "Should the need ecerarise...")>]
  let internal addGenericGroup mappings group item f (json:JsonValue) (x:XContainer) =
    let items = JsonArray()
    x.Descendants(XName.Get group)
    |> Seq.collect (fun f -> f.Descendants(XName.Get item))
    |> Seq.iter(fun x ->
      let json = mappedElementToJSon mappings x
      items.Add json
      f json x
    )
    if items.Count > 0
    then json.Object.Add(item, JsonValue items)

  let internal addTerminalGroup mappings group item (json:JsonValue) (x:XContainer) =
    addGenericGroup mappings group item (fun _ _ -> ()) json x

  let internal formatSingleTime (t:String) =
    let formatTimeValue l =
      let million = 1000000L
      let rem = (l/million)*million
      let time = DateTime(rem, DateTimeKind.Utc)
      (time.ToString("yyyy-MM-dd HH:mm:ss.f", DateTimeFormatInfo.InvariantInfo)
        + (l % million).ToString("D6", CultureInfo.InvariantCulture).TrimEnd('0'))

    [t]
    |> Seq.map Int64.TryParse
    |> Seq.filter fst
    |> Seq.map (snd >> formatTimeValue  >> JsonValue)
    |> Seq.tryHead

  let internal formatTimeValue (a:XAttribute) =
    formatSingleTime a.Value

  let internal formatTimeList (a:XAttribute) =
    a.Value.Split([|';'|], StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map formatSingleTime
    |> Seq.choose id
    |> JsonArray
    |> JsonValue
    |> Some

  let internal formatOffsetChain (a:XAttribute) =
    a.Value.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map Int32.TryParse
    |> Seq.filter fst
    |> Seq.map (snd >> float >> JsonValue)
    |> JsonArray
    |> JsonValue
    |> Some

  let internal addMethodPoints mappings group item (json:JsonValue) (x:XContainer) =
    addGenericGroup mappings group item (fun point x ->
      addTerminalGroup ["time", formatTimeValue ]"Times" "Time"  point x
      addTerminalGroup [] "TrackedMethodRefs" "TrackedMethodRef"  point x) json x

  let internal addIntermediateGroup group item perItem (json:JsonValue) (x:XContainer) =
    let items = JsonArray()
    x.Descendants(XName.Get group)
    |> Seq.collect (fun c -> c.Descendants(XName.Get item))
    |> Seq.iter (perItem items)
    if items.Count > 0
    then json.Object.Add(item, JsonValue items)

  let internal addClassMethods (mjson:JsonValue) (m:XContainer) =
    addIntermediateGroup "Methods" "Method" (fun items x ->
      let ``method`` = simpleElementToJSon x
      items.Add ``method``
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
      addMethodPoints [] "SequencePoints" "SequencePoint"  ``method`` x
      addMethodPoints ["offsetchain", formatOffsetChain ] "BranchPoints" "BranchPoint" ``method`` x
    ) mjson m

  let internal addModuleClasses (mjson:JsonValue) (m:XContainer) =
    addIntermediateGroup "Classes" "Class" (fun items c ->
      let ``class`` = simpleElementToJSon c
      items.Add ``class``
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
    ) mjson m

  let internal topLevelToJson coverage ``module`` f report =
    let json = simpleElementToJSon report
    let modules = JsonArray()
    f json report modules
    if modules.Count > 0
    then json.Object.Add(``module``, JsonValue modules)

    let jo = JsonObject()
    jo.Add(coverage, json)
    let jv = JsonValue jo
    jv

  let ncoverToJson report =
    topLevelToJson "coverage" "module" (fun _ x modules ->
      x.Descendants(XName.Get "module")
      |> Seq.iter(fun m ->
        let mjson = simpleElementToJSon m
        addModuleMethods mjson m
        modules.Add mjson
      )) report

  let opencoverToJson report =
    topLevelToJson "CoverageSession" "Module" (fun json x modules ->
    x.Elements(XName.Get "Summary")
    |> Seq.iter (fun s -> let js = simpleElementToJSon s
                          json.Object.Add("Summary", JsonValue js))

    x.Descendants(XName.Get "Module")
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

      addTerminalGroup [] "Files" "File" mjson m
      addModuleClasses mjson m
      addTerminalGroup [
        "entry", formatTimeList
        "exit", formatTimeList ] "TrackedMethods" "TrackedMethod" mjson m

      modules.Add mjson
    )) report

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