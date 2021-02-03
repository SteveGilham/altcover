namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Xml.Linq
open System.Globalization
open System.Text
open System.Text.Json

open Mono.Cecil
open Mono.Cecil.Rocks

[<SuppressMessage("Microsoft.Naming", "CA1704",
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
    |> NativeJson.FromTracking
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
  [<SuppressMessage("Gendarme.Rules.Exceptions",
    "InstantiateArgumentExceptionCorrectlyRule",
    Justification="Inlined library code")>]
  [<SuppressMessage("Gendarme.Rules.Performance",
    "AvoidRepetitiveCallsToPropertiesRule",
    Justification="Inlined library code")>]
  [<SuppressMessage("Microsoft.Usage", "CA2208:InstantiateArgumentExceptionsCorrectly",
    Justification="Inlined library code")>]
  let internal lineVisits (seqpnts:NativeJson.SeqPnt seq) =
    (seqpnts
      |> Seq.maxBy (fun sp -> sp.VC)).VC

  let getMethodRecord (modul:NativeJson.Documents) (doc:string) (cname:string) (mname:string) =
    let classes = match modul.TryGetValue doc with
                  | true, c -> c
                  | _ -> let c = NativeJson.Classes()
                         modul.Add(doc, c)
                         c
    let methods = match classes.TryGetValue cname with
                  | true, m -> m
                  | _ -> let m = NativeJson.Methods()
                         classes.Add(cname, m)
                         m
    match methods.TryGetValue mname with
    | true, m -> m // possible??
    | _ -> let m = NativeJson.Method.Create(None)
           methods.Add(mname, m)
           m

  let maybeAssembly path =
    Some path
    |> Option.filter File.Exists
    |> Option.map (fun p -> let def = AssemblyDefinition.ReadAssembly p
                            ProgramDatabase.readSymbols def
                            def)

  let ncoverToJson (report: XElement) =
    let json = NativeJson.Modules()
    report.Descendants(XName.Get "module")
    |> Seq.iter (fun x ->
      let path = x.Attribute(XName.Get "name").Value
      let modul = NativeJson.Documents()
      let counts = System.Collections.Generic.Dictionary<string, int>()
      let def = maybeAssembly path
      try
        x.Descendants(XName.Get "method")
        |> Seq.iteri (fun index m ->
          let mname = m.Attribute(XName.Get "name").Value
          let cname = m.Attribute(XName.Get "class").Value.Replace('+', '/')
          let outerclass = cname.Split('/') |> Seq.head
          let _, excluded = m.Attribute(XName.Get "excluded").Value
                            |> Boolean.TryParse

          let mutable docname = String.Empty
          if not excluded
          then
            let sp = NativeJson.SeqPnts()
            m.Descendants(XName.Get "seqpnt")
            |> Seq.iter(fun s ->
              let _, excluded = s.Attribute(XName.Get "excluded").Value
                                |> Boolean.TryParse

              let parse n = s.Attribute(XName.Get n).Value
                            |> Int32.TryParse
                            |> snd
              if not excluded
              then
                if String.IsNullOrWhiteSpace docname
                then docname <- s.Attribute(XName.Get "document").Value
                {
                    NativeJson.SeqPnt.VC = parse "visitcount"
                    NativeJson.SeqPnt.SL = parse "line"
                    NativeJson.SeqPnt.SC = parse "column"
                    NativeJson.SeqPnt.EL = parse "endline"
                    NativeJson.SeqPnt.EC = parse "endcolumn"
                    NativeJson.SeqPnt.Offset = 0
                    NativeJson.SeqPnt.Id = 0
                    NativeJson.SeqPnt.Times = null
                    NativeJson.SeqPnt.Tracks = null
                } |> sp.Add )
            if sp.Count > 0
            then
              // try to find the method in the assembly
              let td = def
                       |> Option.map (fun a -> a.MainModule.GetAllTypes()
                                               |> Seq.tryFind(fun t -> t.FullName = cname))
                       |> Option.flatten

              let md = td
                       |> Option.map(fun t -> t.Methods
                                              |> Seq.tryFind(fun m -> m.Name = mname &&
                                                                      m.DebugInformation.HasSequencePoints &&
                                                                      (let pt = m.DebugInformation.SequencePoints
                                                                                |> Seq.head
                                                                       pt.StartLine = sp.[0].SL &&
                                                                       pt.StartColumn = sp.[0].SC) ))
                       |> Option.flatten

              let truemd = md
                           |> Option.map (Visitor.I.containingMethods >> Seq.last)

              let basename = sprintf "%s::%s" cname mname
              let _, count = counts.TryGetValue basename
              let index = count + 1
              counts.[basename] <- index
              let synth = sprintf "ReturnType%d %s(Argument List%d)" index basename index
              let methodName = truemd
                               |> Option.map (fun m -> m.FullName)
                               |> Option.defaultValue synth
              let className = truemd
                              |> Option.map (fun m -> m.DeclaringType.FullName)
                              |> Option.defaultValue outerclass
              let m = getMethodRecord modul docname className methodName
              m.SeqPnts.AddRange sp
              m.SeqPnts
              |> Seq.groupBy (fun s -> s.SL)
              |> Seq.iter (fun (l,ss) -> m.Lines.[l] <- lineVisits ss)
        )
      finally
        def
        |> Option.iter(fun d -> d.Dispose())
      if modul.Count > 0
      then json.Add(path |> Path.GetFileName, modul)
    )
    let temp = StringBuilder()

    JsonSerializer.Serialize(json, NativeJson.options)
    |> temp.Append

  let internal convertReport (report : XDocument) (format:ReportFormat) (stream : Stream) =
    doWithStream (fun () -> new StreamWriter(stream)) (fun writer ->
        (report.Root
        |> (match format with
            | ReportFormat.NCover -> ncoverToJson
            | _ -> opencoverToJson)).ToString()// Maybe later
        |> writer.Write)

  let internal summary (report : DocumentType) (format : ReportFormat) result =
    doWithStream(fun () -> File.OpenWrite(!path |> Option.get))
      (match report with
       | XML document -> convertReport document format
       | _ -> ignore)
    (result, 0uy, String.Empty)