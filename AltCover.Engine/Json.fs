namespace AltCover

open System
open System.IO
open System.Xml.Linq
open System.Globalization
open System.Text

open Manatee.Json

// based on the sample file at https://raw.githubusercontent.com/jenkinsci/cobertura-plugin/master/src/test/resources/hudson/plugins/cobertura/coverage-with-data.xml
[<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1704",
    Justification="'Json' is jargon")>]
module internal Json =
  let internal path : Option<string> ref = ref None

  // --- Generic XML to JSON helpers ---

  let internal appendChar (builder:StringBuilder) (c:Char) =
    builder.Append(c) |> ignore

  [<System.Diagnostics.CodeAnalysis.SuppressMessage(
    "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
    Justification = "AvoidSpeculativeGenerality too")>]
  let internal escapeString (builder:StringBuilder) (s:String) =
    s
    |> Seq.iter (fun c ->
      if Char.IsControl c || c = '\\' || c = '&'
      then
        match c with
        | '"' -> builder.Append("\\\"")
        | '\\' -> builder.Append("\\\\")
        | '\b' -> builder.Append("\\b")
        | '\f' -> builder.Append("\\f")
        | '\n' -> builder.Append("\\n")
        | '\r' -> builder.Append("\\r")
        | '\t' -> builder.Append("\\t")
        | _ -> builder.Append("\\u").Append(((int)c).ToString("X4", CultureInfo.InvariantCulture))
        |> ignore
      else appendChar builder c )

  let internal appendSimpleValue (builder:StringBuilder) (value:string) =
    let b,v = Double.TryParse value
    if b then builder.AppendFormat(CultureInfo.InvariantCulture, "{0}", v) |> ignore
    else
      let b2, v2 = Boolean.TryParse value
      if b2 then builder.Append(if v2 then "true" else "false") |> ignore
      else
        appendChar builder '"'
        escapeString builder value
        appendChar builder '"'

  let internal appendSimpleAttributeValue (builder:StringBuilder) (a:XAttribute) =
    appendSimpleValue builder a.Value

  let internal appendSequence builder topComma (sequence:(StringBuilder -> unit)seq)  =
    sequence
    |> Seq.fold (fun comma item ->
       if comma
       then appendChar builder ','
       item builder
       true
      ) topComma

  let internal appendMappedElement
    (builder:StringBuilder)
    (topComma:bool)
    (mappings: (string * (XAttribute -> JsonValue)) list)
    (xElement : XElement) =

    xElement.Attributes()
    |> Seq.filter (fun a -> a.Name.ToString().StartsWith("{", StringComparison.Ordinal) |> not)
    |> Seq.map (fun a -> (fun (b:StringBuilder) ->
          let local = a.Name.LocalName
          b.Append("\"")
           .Append(local) // known safe
           .Append("\":") |> ignore
          let mapped = mappings
                       |> Seq.tryFind (fun (n,_) -> n = local)
                       |> Option.map snd
                       |> Option.map (fun f ->
            (fun (v:XAttribute) ->
              builder.Append((f v).ToString()) |> ignore))
          a |>
          Option.defaultValue (appendSimpleAttributeValue builder) mapped ))
    |> (appendSequence builder topComma)

  let internal appendSimpleElement builder (xElement : XElement) =
    appendMappedElement builder false [] xElement

  // --- Manatee remnant ---

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

  // --- OpenCover ---

  [<System.Diagnostics.CodeAnalysis.SuppressMessage(
    "Gendarme.Rules.Smells", "AvoidLongParameterListsRule",
    Justification = "Should the need ever arise...")>]
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

  let internal appendInternalElements next group key
    (builder:StringBuilder) (comma:bool) (x:XContainer) =
    let mutable first = true

    x.Elements(XName.Get group)
    |> Seq.collect(fun xx -> xx.Elements(XName.Get key))
    |> Seq.map (fun xx -> (fun (b:StringBuilder) ->
      if first
      then
        if comma
        then appendChar b ','
        b.Append("\"")
         .Append(key)
         .Append("\":[")
         |> ignore
      first <- false
      appendChar b '{'
      let c2 = appendSimpleElement b xx
      next b c2 xx |> ignore
      appendChar b '}'
    ))

    |> appendSequence builder false // (sequence:(StringBuilder -> unit)seq)
    |> ignore

    let result = first |> not
    if result
    then builder.Append ("]")|> ignore
    result

  let internal appendItemSummary (builder:StringBuilder) comma (x:XContainer) =
    let mutable topComma = comma
    x.Elements(XName.Get "Summary")
    |> Seq.tryHead
    |> Option.iter (fun s -> if topComma then appendChar builder ',' |> ignore
                             appendSimpleElement (builder.Append("\"Summary\":{")) s |> ignore
                             appendChar builder '}'
                             topComma <- true)
    topComma

  let openCoverSink _ _ _ = false

  let internal appendPointTracking (builder:StringBuilder) comma (x:XContainer) =
    let mutable topComma = appendInternalElements openCoverSink "Times" "Time" builder comma x
    appendInternalElements openCoverSink "TrackedMethodRefs" "TrackedMethodRef" builder topComma x

  let internal appendMethodPoints (builder:StringBuilder) comma (x:XContainer) =
    let mutable topComma = comma
    [
      "Summary"
      "FileRef"
      "MethodPoint"
    ]
    |> Seq.iter (fun name ->
      x.Elements(XName.Get name)
      |> Seq.tryHead
      |> Option.iter (fun s -> if topComma then appendChar builder ',' |> ignore
                               appendSimpleElement (
                                 builder.Append('"')
                                        .Append(name)
                                        .Append( "\":{")) s |> ignore
                               appendChar builder '}'
                               topComma <- true))

    [
      "MetadataToken"
      "Name"
    ]
    |> Seq.iter  (fun tag ->
        x.Elements(XName.Get tag)
        |> Seq.tryHead
        |> Option.iter (fun s -> if topComma then appendChar builder ',' |> ignore
                                 appendSimpleValue
                                   (builder.Append('"')
                                           .Append(tag)
                                           .Append("\":"))
                                   s.Value
                                 topComma <- true))
    topComma <- appendInternalElements appendPointTracking "SequencePoints" "SequencePoint" builder topComma x
    appendInternalElements appendPointTracking "BranchPoints" "BranchPoint" builder topComma x

  let internal appendClassMethods (builder:StringBuilder) comma (x:XContainer) =
    let mutable topComma = appendItemSummary builder comma x
    [
        "FullName"
    ]
    |> Seq.iter  (fun tag ->
        x.Elements(XName.Get tag)
        |> Seq.tryHead
        |> Option.iter (fun s -> if topComma then appendChar builder ',' |> ignore
                                 appendSimpleValue
                                   (builder.Append('"')
                                           .Append(tag)
                                           .Append("\":"))
                                   s.Value
                                 topComma <- true))
    topComma <- appendInternalElements appendMethodPoints "Methods" "Method" builder topComma x

  let internal appendModuleInternals (builder:StringBuilder) comma (x:XContainer) =
    let mutable topComma = appendItemSummary builder comma x
    [
        "FullName"
        "ModulePath"
        "ModuleTime"
        "ModuleName"
    ]
    |> Seq.iter  (fun tag ->
        x.Elements(XName.Get tag)
        |> Seq.tryHead
        |> Option.iter (fun s -> if topComma then appendChar builder ',' |> ignore
                                 appendSimpleValue
                                   (builder.Append('"')
                                           .Append(tag)
                                           .Append("\":"))
                                   s.Value
                                 topComma <- true))

    topComma <- appendInternalElements openCoverSink "Files" "File" builder comma x
    topComma <- appendInternalElements appendClassMethods "Classes" "Class" builder topComma x
    appendInternalElements openCoverSink "TrackedMethods" "TrackedMethod" builder topComma x

    //  addTerminalGroup [
    //    "entry", formatTimeList
    //    "exit", formatTimeList ] "TrackedMethods" "TrackedMethod" mjson m

  let internal appendSessionModules (builder:StringBuilder) comma (x:XContainer) =
    appendInternalElements appendModuleInternals "Modules" "Module" builder comma x

  let opencoverToJson report =
    let builder = StringBuilder()
    builder.Append("{\"CoverageSession\":{") |> ignore
    let mutable topComma = appendSimpleElement builder report
    topComma <- appendItemSummary builder topComma report
    appendSessionModules builder topComma report |> ignore
    builder.Append("}}") // return value

  // --- NCover ---

  let internal appendNCoverElements next key
    (builder:StringBuilder) (comma:bool) (x:XContainer) =
    let mutable first = true

    x.Elements(XName.Get key)
    |> Seq.map (fun xx -> (fun (b:StringBuilder) ->
      if first
      then
        if comma
        then appendChar b ','
        b.Append("\"")
         .Append(key)
         .Append("\":[")
         |> ignore
      first <- false
      appendChar b '{'
      let c2 = appendSimpleElement b xx
      next b c2 xx |> ignore
      appendChar b '}'
    ))

    |> appendSequence builder false // (sequence:(StringBuilder -> unit)seq)
    |> ignore

    let result = first |> not
    if result
    then builder.Append ("]")|> ignore
    result

  let internal appendMethodSeqpnts (builder:StringBuilder) comma (x:XContainer) =
    appendNCoverElements (fun _ _ _ -> false) "seqpnt" builder comma x

  let internal appendModuleMethods (builder:StringBuilder) comma (x:XContainer) =
    appendNCoverElements appendMethodSeqpnts "method" builder comma x

  let internal appendCoverageModules (builder:StringBuilder) comma (x:XContainer) =
    appendNCoverElements appendModuleMethods "module" builder comma x

  let ncoverToJson report =
    let builder = StringBuilder()
    builder.Append("{\"coverage\":{") |> ignore
    let topComma = appendSimpleElement builder report
    appendCoverageModules builder topComma report |> ignore
    builder.Append("}}") // return value

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