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

  let internal appendSimpleAttributeValue (builder:StringBuilder) (a:XAttribute) =
    let value = a.Value
    let b,v = Double.TryParse value
    if b then builder.AppendFormat(CultureInfo.InvariantCulture, "{0}", v) |> ignore
    else
      let b2, v2 = Boolean.TryParse value
      if b2 then builder.Append(if v2 then "true" else "false") |> ignore
      else
        appendChar builder '"'
        escapeString builder value
        appendChar builder '"'

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

  let internal topLevelToJson (coverage:string) (``module``:string) f report =
    let builder = StringBuilder()
    builder.Append("{\"")
           .Append(coverage) // known safe
           .Append("\":{") |> ignore
    let topComma = appendSimpleElement builder report

    let modules = JsonArray()
    let comma2 = f builder topComma report modules

    if modules.Count > 0
    then
      if comma2
      then appendChar builder ','
      appendChar builder '"'
      builder.Append(``module``)
             .Append("\":[")
             |> ignore
      let mutable comma = false
      modules
      |> Seq.iter(fun m ->
        if comma
        then appendChar builder ','
        builder.Append(m.ToString()) |> ignore
        comma <- true)
      appendChar builder ']'
    builder.Append("}}") // return the builder

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

    if first |> not
    then builder.Append ("]")|> ignore
    true

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

    //topLevelToJson "coverage" "module" (fun builder comma x modules ->
    //  x.Descendants(XName.Get "module")
    //  |> Seq.map (fun m -> (fun b ->
    //    appendChar b '{'
    //    appendSimpleElement b m |> ignore // TODO comma
    //    let mjson = JsonValue(JsonObject())
    //    b.Append ((addModuleMethods mjson m).ToString()) |> ignore
    //  ))
    //  |> appendSequence builder comma) report

  let opencoverToJson report =
    topLevelToJson "CoverageSession" "Module" (fun builder topComma x modules ->
    x.Elements(XName.Get "Summary")
    |> Seq.iter (fun s -> if topComma then appendChar builder ',' |> ignore
                          appendSimpleElement (builder.Append("\"Summary\":{")) s |> ignore
                          appendChar builder '}')
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
    )
    true ) report

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