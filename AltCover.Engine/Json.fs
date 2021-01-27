namespace AltCover

open System
open System.IO
open System.Xml.Linq
open System.Globalization
open System.Text

[<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1704",
    Justification="'Json' is jargon")>]
module internal Json =
  let internal path : Option<string> ref = ref None

  // --- Generic XML to JSON helpers ---

  [<Sealed>]
  type private BuildWriter() =
    inherit TextWriter(CultureInfo.InvariantCulture)
    member val Builder:StringBuilder = null with get, set
    member self.Clear() = let temp = self.Builder
                          self.Builder <- null
                          temp
    override self.Encoding = Encoding.Unicode // pointless but required
    [<System.Diagnostics.CodeAnalysis.SuppressMessage(
      "Gendarme.Rules.Exceptions", "UseObjectDisposedExceptionRule",
      Justification="Would be meaningless")>]
    override self.Write(value:Char) =
        value
        |> self.Builder.Append
        |> ignore
    [<System.Diagnostics.CodeAnalysis.SuppressMessage(
      "Gendarme.Rules.Exceptions", "UseObjectDisposedExceptionRule",
      Justification="Would be meaningless")>]
    override self.Write(value:String) =
        value
        |> self.Builder.Append
        |> ignore

  let private escapeString (builder:TextWriter) (s:String) =
    System.Text.Encodings.Web.JavaScriptEncoder.Default.Encode(builder, s)

  let private appendSimpleValue (builder:BuildWriter) (value:string) =
    let b,v = Double.TryParse value
    if b then builder.Builder.AppendFormat(CultureInfo.InvariantCulture, "{0}", v) |> ignore
    else
      let b2, v2 = Boolean.TryParse value
      if b2 then builder.Write(if v2 then "true" else "false") |> ignore
      else
        builder.Write '"'
        escapeString builder value
        builder.Write '"'

  let private appendSequence (builder:BuildWriter) topComma (sequence:(BuildWriter -> unit)seq)  =
    sequence
    |> Seq.fold (fun comma item ->
       if comma
       then builder.Write ','
       item builder
       true
      ) topComma

  let private appendSingleTime (builder:TextWriter) (value:string) =
    builder.Write '"'
    value
    |> Int64.TryParse
    |> snd
    |> BitConverter.GetBytes
    |> Convert.ToBase64String
    |> escapeString builder
    builder.Write '"'

  let private formatTimeList (builder:BuildWriter) (value:string) =
    builder.Write '['
    value.Split([|';'|], StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map (fun v -> fun b -> appendSingleTime b v)
    |> appendSequence builder false
    |> ignore
    builder.Write ']'

  let private specialValues = [
                                 ("time", appendSingleTime)
                                 ("offsetchain", (fun builder value ->
                                    builder.Write '['
                                    value.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
                                    |> Seq.filter (Int32.TryParse >> fst)
                                    |> Seq.map (fun v -> fun b -> v |> builder.Write)
                                    |> appendSequence builder false
                                    |> ignore
                                    builder.Write ']'))
                                 ("entry", formatTimeList)
                                 ("exit", formatTimeList)
                                ]
                                |> Map.ofSeq

  let private appendMappedElement
    (builder:BuildWriter)
    (topComma:bool)
    (xElement : XElement) =

    xElement.Attributes()
    |> Seq.filter (fun a -> a.Name.ToString().StartsWith("{", StringComparison.Ordinal) |> not)
    |> Seq.map (fun a -> (fun (b:BuildWriter) ->
          let local = a.Name.LocalName
          b.Builder.Append("\"")
           .Append(local) // known safe
           .Append("\":") |> ignore
          let mapped = specialValues
                       |> Map.tryFind local
                       |> Option.defaultValue appendSimpleValue

          mapped builder a.Value))
    |> (appendSequence builder topComma)

  let private appendSimpleElement builder (xElement : XElement) =
    appendMappedElement builder false xElement

  [<System.Diagnostics.CodeAnalysis.SuppressMessage(
    "Gendarme.Rules.Correctness", "EnsureLocalDisposalRule",
    Justification="Would be meaningless")>]
  [<System.Diagnostics.CodeAnalysis.SuppressMessage(
    "Microsoft.Reliability", "CA2000:Dispose objects before losing scope",
    Justification="Would be meaningless")>]
  let private applyHeadline report (tag:string) =
    let builder = new BuildWriter()
    builder.Builder <- StringBuilder()
    builder.Write tag
    (builder, appendSimpleElement builder report)

  let private close (builder:BuildWriter) =
    builder.Write("}}")
    builder.Clear()// return value

  // --- OpenCover ---

  [<System.Diagnostics.CodeAnalysis.SuppressMessage(
    "Gendarme.Rules.Smells", "AvoidLongParameterListsRule",
    Justification = "Should the need ever arise...")>]
  let private appendInternalElements next group key
    (builder:BuildWriter) (comma:bool) (x:XContainer) =
    let mutable first = true

    x.Elements(XName.Get group)
    |> Seq.collect(fun xx -> xx.Elements(XName.Get key))
    |> Seq.map (fun xx -> (fun (b:BuildWriter) ->
      if first
      then
        if comma
        then builder.Write ','
        b.Builder.Append("\"")
         .Append(key)
         .Append("\":[")
         |> ignore
      first <- false
      builder.Write '{'
      let c2 = appendSimpleElement b xx
      next b c2 xx |> ignore
      builder.Write '}'
    ))

    |> appendSequence builder false // (sequence:(StringBuilder -> unit)seq)
    |> ignore

    let result = first |> not
    if result
    then builder.Write ']'
    result

  let private appendItemSummary (builder:BuildWriter) comma (x:XContainer) =
    let mutable topComma = comma
    x.Elements(XName.Get "Summary")
    |> Seq.tryHead
    |> Option.iter (fun s -> if topComma then builder.Write ',' |> ignore
                             builder.Write("\"Summary\":{")
                             appendSimpleElement builder s |> ignore
                             builder.Write '}'
                             topComma <- true)
    topComma

  let openCoverSink _ _ _ = false

  let private appendPointTracking (builder:BuildWriter) comma (x:XContainer) =
    let mutable topComma = appendInternalElements openCoverSink "Times" "Time" builder comma x
    appendInternalElements openCoverSink "TrackedMethodRefs" "TrackedMethodRef" builder topComma x

  let private appendMethodPoints (builder:BuildWriter) comma (x:XContainer) =
    let mutable topComma = comma
    [
      "Summary"
      "FileRef"
      "MethodPoint"
    ]
    |> Seq.iter (fun name ->
      x.Elements(XName.Get name)
      |> Seq.tryHead
      |> Option.iter (fun s -> if topComma then builder.Write ',' |> ignore
                               builder.Builder.Append('"')
                                        .Append(name)
                                        .Append( "\":{") |> ignore
                               appendSimpleElement builder s |> ignore
                               builder.Write '}'
                               topComma <- true))

    [
      "MetadataToken"
      "Name"
    ]
    |> Seq.iter  (fun tag ->
        x.Elements(XName.Get tag)
        |> Seq.tryHead
        |> Option.iter (fun s -> if topComma then builder.Write ',' |> ignore
                                 builder.Builder.Append('"')
                                           .Append(tag)
                                           .Append("\":") |> ignore
                                 appendSimpleValue builder s.Value
                                 topComma <- true))
    topComma <- appendInternalElements appendPointTracking "SequencePoints" "SequencePoint" builder topComma x
    appendInternalElements appendPointTracking "BranchPoints" "BranchPoint" builder topComma x

  let private appendClassMethods (builder:BuildWriter) comma (x:XContainer) =
    let mutable topComma = appendItemSummary builder comma x
    [
        "FullName"
    ]
    |> Seq.iter  (fun tag ->
        x.Elements(XName.Get tag)
        |> Seq.tryHead
        |> Option.iter (fun s -> if topComma then builder.Write ',' |> ignore
                                 builder.Builder.Append('"')
                                           .Append(tag)
                                           .Append("\":") |> ignore
                                 appendSimpleValue builder s.Value
                                 topComma <- true))
    topComma <- appendInternalElements appendMethodPoints "Methods" "Method" builder topComma x

  let private appendModuleInternals (builder:BuildWriter) comma (x:XContainer) =
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
        |> Option.iter (fun s -> if topComma then builder.Write ',' |> ignore
                                 builder.Builder.Append('"')
                                           .Append(tag)
                                           .Append("\":") |> ignore
                                 appendSimpleValue builder s.Value
                                 topComma <- true))

    topComma <- appendInternalElements openCoverSink "Files" "File" builder comma x
    topComma <- appendInternalElements appendClassMethods "Classes" "Class" builder topComma x
    appendInternalElements openCoverSink "TrackedMethods" "TrackedMethod" builder topComma x

  let private appendSessionModules (builder:BuildWriter) comma (x:XContainer) =
    appendInternalElements appendModuleInternals "Modules" "Module" builder comma x

  let opencoverToJson report =
    let (builder,comma) = applyHeadline report "{\"CoverageSession\":{"
    let topComma = appendItemSummary builder comma report
    appendSessionModules builder topComma report |> ignore
    close builder

  // --- NCover ---

  let private appendNCoverElements next key
    (builder:BuildWriter) (comma:bool) (x:XContainer) =
    let mutable first = true

    x.Elements(XName.Get key)
    |> Seq.map (fun xx -> (fun (b:BuildWriter) ->
      if first
      then
        if comma
        then builder.Write ','
        b.Builder.Append("\"")
         .Append(key)
         .Append("\":[")
         |> ignore
      first <- false
      builder.Write '{'
      let c2 = appendSimpleElement b xx
      next b c2 xx |> ignore
      builder.Write '}'
    ))

    |> appendSequence builder false // (sequence:(StringBuilder -> unit)seq)
    |> ignore

    let result = first |> not
    if result
    then builder.Builder.Append ("]")|> ignore
    result

  let private appendMethodSeqpnts (builder:BuildWriter) comma (x:XContainer) =
    appendNCoverElements (fun _ _ _ -> false) "seqpnt" builder comma x

  let private appendModuleMethods (builder:BuildWriter) comma (x:XContainer) =
    appendNCoverElements appendMethodSeqpnts "method" builder comma x

  let private appendCoverageModules (builder:BuildWriter) comma (x:XContainer) =
    appendNCoverElements appendModuleMethods "module" builder comma x

  let ncoverToJson report =
    let (builder,comma) = applyHeadline report "{\"coverage\":{"
    appendCoverageModules builder comma report |> ignore
    close builder

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