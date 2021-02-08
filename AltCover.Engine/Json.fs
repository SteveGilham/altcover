namespace AltCover

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Reflection
open System.Xml.Linq
open System.Globalization
open System.Text

open Manatee.Json.Serialization

open Mono.Cecil
open Mono.Cecil.Rocks

type internal AltDictionarySerializer() =
  static member Encode<'k, 'v>(context:SerializationContext) =
    let o = Manatee.Json.JsonObject()

    let encode (kvp:KeyValuePair<'k, 'v>) =
      let key = String.Format(CultureInfo.InvariantCulture, "{0}", kvp.Key)
      let value = kvp.Value
      printfn "Encoding %A => %A (%A)" key value (value.GetType().FullName)
      context.Push(value.GetType(), typeof<'v>, key, value)
      printfn "Pushed %A" <| context.Source.GetType()
      let value2 = if value.GetType() = typeof<int32>
                   then
                    printfn "Encoding int %A" value
                    let c = value.GetType().GetRuntimeMethods()
                            |> Seq.find (fun m -> m.Name = "System.IConvertible.ToDouble")
                    printfn "with %A" c
                    let v = c.Invoke(value, [| null |]) :?> float
                    printfn "Getting %A" v
                    Manatee.Json.JsonValue v
                   else if value.GetType() = typeof<String>
                        then Manatee.Json.JsonValue(value.ToString())
                        else context.RootSerializer.Serialize(context)
      printfn "Got value"
      context.Pop()
      printfn "Popped"
      o.Add(key, value2)
      printfn "Added"

    context.Source :?> System.Collections.IDictionary
    |> Seq.cast<KeyValuePair<'k,'v>>
    |> Seq.iter encode
    Manatee.Json.JsonValue(o)

  interface ISerializer with
    member this.Handles(context:SerializationContextBase) =
      let b =
        if context.InferredType.GetTypeInfo().IsGenericType
        then context.InferredType.GetGenericTypeDefinition().GetInterfaces()
             |> Seq.exists(fun t -> t.Name = "IDictionary`2" &&
                                    t.Namespace = "System.Collections.Generic")
        else false
      printfn "Do we handle %A => %A (%A)" context.InferredType.FullName b context.CurrentLocation
      b

    member this.Serialize(context:SerializationContext) =
      //AltDictionarySerializer.Encode(context) by reflection to frob the generic types
      printfn "ADS on %A" <| context.Source.GetType().FullName
      let typeargs = context.Source.GetType().GetTypeInfo().GenericTypeArguments
      printfn "typeargs = %A" typeargs
      try
        let info = typeof<AltDictionarySerializer>
                      .GetMethod("Encode", BindingFlags.Static ||| BindingFlags.NonPublic)
                      .MakeGenericMethod(typeargs)
        printfn "Invoking with %A" typeargs
        let r = info.Invoke(null, [| context :> obj |]) :?> Manatee.Json.JsonValue
        printfn "Invoked with %A" typeargs
        r
      with
      | x -> printfn "Blew %A with %s" typeargs x.Message
             reraise()

    member this.Deserialize(context:DeserializationContext) =
      Object()
    member val ShouldMaintainReferences = false

[<SuppressMessage("Microsoft.Naming", "CA1704",
    Justification="'Json' is jargon")>]
module internal Json =
  let internal path : Option<string> ref = ref None

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

  [<System.Diagnostics.CodeAnalysis.SuppressMessage(
    "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
    Justification = "AvoidSpeculativeGenerality too")>]
  let internal getMethodRecords (modul:NativeJson.Documents) (doc:string) (cname:string) (mname:string) =
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
    | true, m -> (methods, m)
    | _ -> let m = NativeJson.Method.Create(None)
           methods.Add(mname, m)
           (methods, m)

  [<System.Diagnostics.CodeAnalysis.SuppressMessage(
    "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
    Justification = "AvoidSpeculativeGenerality too")>]
  let internal getMethodRecord (modul:NativeJson.Documents) (doc:string) (cname:string) (mname:string) =
    getMethodRecords modul doc cname mname
    |> snd

  [<SuppressMessage(
    "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
    Justification = "AvoidSpeculativeGenerality too")>]
  [<SuppressMessage(
    "Gendarme.Rules.Smells", "AvoidLongParameterListsRule",
    Justification = "Long enough but no longer")>]
  let internal updateMethodRecord (modul:NativeJson.Documents)
                         (doc:string) (cname:string) (mname:string)
                         (update: (Nullable<int>*NativeJson.Times*NativeJson.Times)) =
    let methods, m0 = getMethodRecords modul doc cname mname
    let tid, entry, exit = update
    if tid.HasValue && tid <> m0.TId
    then
      let next = { m0 with TId = tid
                           Entry = entry
                           Exit = exit }
      methods.[mname] <- next
      next
    else
      m0

  let internal maybeAssembly path =
    Some path
    |> Option.filter File.Exists
    |> Option.map (fun p -> let def = AssemblyDefinition.ReadAssembly p
                            ProgramDatabase.readSymbols def
                            def)

  let internal maybeDispose (def:AssemblyDefinition option) =
    def
    |> Option.iter(fun d -> d.Dispose())

  // try to find the method in the assembly
  [<SuppressMessage(
    "Gendarme.Rules.Smells", "AvoidLongParameterListsRule",
    Justification = "Long enough but no longer")>]
  let internal maybeNames (def:AssemblyDefinition option) (fallbackm:string) (fallbackc:string)
                 (sp : NativeJson.SeqPnt option) (cname:string) (mname:string) =
      let td = def
                |> Option.map (fun a -> a.MainModule.GetAllTypes()
                                        |> Seq.tryFind(fun t -> t.FullName = cname))
                |> Option.flatten

      let md = td
                |> Option.map(fun t -> t.Methods
                                      |> Seq.tryFind(fun m -> m.FullName = mname ||
                                                              (m.Name = mname &&
                                                               (let dbg = m.DebugInformation
                                                                dbg.HasSequencePoints &&
                                                                Option.isSome sp &&
                                                                (let pt = dbg.SequencePoints
                                                                          |> Seq.head
                                                                 pt.StartLine = sp.Value.SL &&
                                                                 pt.StartColumn = sp.Value.SC)))))
                |> Option.flatten

      let truemd = md
                    |> Option.map (Visitor.I.containingMethods >> Seq.last)

      let methodName = truemd
                        |> Option.map (fun m -> m.FullName)
                        |> Option.defaultValue fallbackm
      let className = truemd
                      |> Option.map (fun m -> m.DeclaringType.FullName)
                      |> Option.defaultValue fallbackc
      (className, methodName)

  [<SuppressMessage(
    "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
    Justification = "AvoidSpeculativeGenerality too")>]
  let internal ncoverToJson (report: XElement) =
    let json = NativeJson.Modules()
    report.Descendants(XName.Get "module")
    |> Seq.iter (fun x ->
      let path = x.Attribute(XName.Get "name").Value
      let modul = NativeJson.Documents()
      let counts = System.Collections.Generic.Dictionary<string, int>()
      let def = maybeAssembly path
      try
        x.Descendants(XName.Get "method")
        |> Seq.iter (fun m ->
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

              let parse n =
                s.Attribute(XName.Get n)
                |> Option.ofObj
                |> Option.map (fun a -> a.Value
                                        |> Int32.TryParse
                                        |> snd)
                |> Option.defaultValue 0

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
              let basename = sprintf "%s::%s" cname mname
              let _, count = counts.TryGetValue basename
              let index = count + 1
              counts.[basename] <- index
              let synth = sprintf "ReturnType%d %s(Argument List%d)" index basename index
              let (className, methodName) = maybeNames def synth outerclass (sp |> Seq.head |> Some) cname mname
              let m = getMethodRecord modul docname className methodName
              m.SeqPnts.AddRange sp
              m.SeqPnts
              |> Seq.groupBy (fun s -> s.SL)
              |> Seq.iter (fun (l,ss) -> m.Lines.[l] <- lineVisits ss)
        )
      finally
        maybeDispose def
      if modul.Count > 0
      then json.Add(path |> Path.GetFileName, modul)
    )
    json

  [<System.Diagnostics.CodeAnalysis.SuppressMessage(
    "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
    Justification = "AvoidSpeculativeGenerality too")>]
  let internal opencoverToJson (report: XElement) =
    let json = NativeJson.Modules()
    report.Descendants(XName.Get "Module")
    |> Seq.iter (fun x ->
      let path = (x.Elements(XName.Get "ModulePath") |> Seq.head).Value
      let modul = NativeJson.Documents()
      let files = System.Collections.Generic.Dictionary<string, string>()
      x.Descendants(XName.Get "File")
      |> Seq.iter (fun x -> files.Add(x.Attribute(XName.Get "uid").Value,
                                      x.Attribute(XName.Get "fullPath").Value) )

      let tracked = System.Collections.Generic.Dictionary<string, int * NativeJson.Times * NativeJson.Times>()
      x.Descendants(XName.Get "TrackedMethod")
      |> Seq.iter (fun x -> tracked.Add(x.Attribute(XName.Get "name").Value,
                                        (x.Attribute(XName.Get "uid").Value
                                         |> Int32.TryParse |> snd,
                                         (let e = NativeJson.Times()
                                          x.Attribute(XName.Get "entry").Value.Split(';')
                                          |> Seq.map (Int64.TryParse >> snd >> NativeJson.FromTracking)
                                          |> e.AddRange
                                          e),
                                         (let e = NativeJson.Times()
                                          x.Attribute(XName.Get "exit").Value.Split(';')
                                          |> Seq.map (Int64.TryParse >> snd >> NativeJson.FromTracking)
                                          |> e.AddRange
                                          e)) ))

      let def = maybeAssembly path
      try
        x.Descendants(XName.Get "Method")
        |> Seq.iter (fun m ->
          let mname = (m.Elements(XName.Get "Name") |> Seq.head).Value
          let cname = (m.Parent.Parent.Elements(XName.Get "FullName") |> Seq.head).Value.Replace('+', '/')
          let outerclass = cname.Split('/') |> Seq.head

          let mutable docname = String.Empty
          let updateDocname (s:XElement) =
            if String.IsNullOrWhiteSpace docname
            then docname <- files.[s.Attribute(XName.Get "fileid").Value]

          let sp = NativeJson.SeqPnts()
          m.Descendants(XName.Get "SequencePoint")
          |> Seq.iter(fun s ->
            let parse n =
              s.Attribute(XName.Get n)
              |> Option.ofObj
              |> Option.map (fun a -> a.Value
                                      |> Int32.TryParse
                                      |> snd)
              |> Option.defaultValue 0

            updateDocname s
            {
                NativeJson.SeqPnt.VC = parse "vc"
                NativeJson.SeqPnt.SL = parse "sl"
                NativeJson.SeqPnt.SC = parse "sc"
                NativeJson.SeqPnt.EL = parse "el"
                NativeJson.SeqPnt.EC = parse "ec"
                NativeJson.SeqPnt.Offset = parse "offset"
                NativeJson.SeqPnt.Id = parse "uspid"
                NativeJson.SeqPnt.Times = let t = s.Descendants(XName.Get "Time")
                                          if t |> isNull || t |> Seq.isEmpty
                                          then null
                                          else
                                            let t2 = NativeJson.Times()
                                            t
                                            |> Seq.map (fun x -> x.Attribute(XName.Get "time").Value
                                                                |> Int64.TryParse
                                                                |> snd
                                                                |> NativeJson.FromTracking)
                                            |> t2.AddRange
                                            t2

                NativeJson.SeqPnt.Tracks = let t = s.Descendants(XName.Get "TrackedMethodRef")
                                           if t |> isNull || t |> Seq.isEmpty
                                           then null
                                           else
                                             let t2 = NativeJson.Tracks()
                                             t
                                             |> Seq.map (fun x -> x.Attribute(XName.Get "uid").Value
                                                                  |> Int32.TryParse
                                                                  |> snd)
                                             |>t2.AddRange
                                             t2

            } |> sp.Add )

          let bp = NativeJson.Branches()
          m.Descendants(XName.Get "BranchPoint")
          |> Seq.iter(fun s ->
            let parse n =
              s.Attribute(XName.Get n)
              |> Option.ofObj
              |> Option.map (fun a -> a.Value
                                      |> Int32.TryParse
                                      |> snd)
              |> Option.defaultValue 0

            updateDocname s
            let offset = parse "offset"
            {
                NativeJson.BranchInfo.Hits = parse "vc"
                NativeJson.BranchInfo.Line = parse "sl"
                NativeJson.BranchInfo.EndOffset = parse "offsetend"
                NativeJson.BranchInfo.Path = bp
                                             |> Seq.filter (fun k -> k.Offset = offset)
                                             |> Seq.length
                NativeJson.BranchInfo.Ordinal = uint bp.Count
                NativeJson.BranchInfo.Offset = offset
                NativeJson.BranchInfo.Id = parse "uspid"
                NativeJson.BranchInfo.Times =
                                          let t = s.Descendants(XName.Get "Time")
                                          if t |> isNull || t |> Seq.isEmpty
                                          then null
                                          else
                                            let t2 = NativeJson.Times()
                                            t
                                            |> Seq.map (fun x -> x.Attribute(XName.Get "time").Value
                                                                |> Int64.TryParse
                                                                |> snd
                                                                |> NativeJson.FromTracking)
                                            |> t2.AddRange
                                            t2

                NativeJson.BranchInfo.Tracks =
                                           let t = s.Descendants(XName.Get "TrackedMethodRef")
                                           if t |> isNull || t |> Seq.isEmpty
                                           then null
                                           else
                                             let t2 = NativeJson.Tracks()
                                             t
                                             |> Seq.map (fun x -> x.Attribute(XName.Get "uid").Value
                                                                  |> Int32.TryParse
                                                                  |> snd)
                                             |>t2.AddRange
                                             t2

            } |> bp.Add )

          if sp.Count > 0 || bp.Count > 0
          then
            let (className, methodName) = maybeNames def mname outerclass None cname mname
            let (tid, entry, exit) = if tracked.ContainsKey mname
                                     then
                                       let (tid0, entry, exit) = tracked.[mname]
                                       (Nullable<int>(tid0), entry, exit)
                                     else
                                       (System.Nullable(), null, null)

            updateMethodRecord modul docname className mname (tid, entry, exit)
            |> ignore
            let m = getMethodRecord modul docname className methodName
            if sp.Count > 0 then
              m.SeqPnts.AddRange sp
              m.SeqPnts
              |> Seq.groupBy (fun s -> s.SL)
              |> Seq.iter (fun (l,ss) -> m.Lines.[l] <- lineVisits ss)
            if bp.Count > 0 then
              m.Branches.AddRange bp
        )
      finally
        maybeDispose def
      if modul.Count > 0
      then
        json.Add(path |> Path.GetFileName, modul)
    )
    json

  let internal convertReport (report : XDocument) (format:ReportFormat) (stream : Stream) =
    doWithStream (fun () -> new StreamWriter(stream)) (fun writer ->
        // SerializerFactory.RemoveSerializer<NativeJson.Modules>()
        SerializerFactory.AddSerializer(AltDictionarySerializer())
        let s = JsonSerializer()
        let o = JsonSerializerOptions()
        o.PropertySelectionStrategy <- PropertySelectionStrategy.ReadAndWrite
        s.Options <- o
        ((report.Root
         |> (match format with
             | ReportFormat.NCover -> ncoverToJson
             | _ -> opencoverToJson))
         |> s.Serialize<NativeJson.Modules>).GetIndentedString()
         // tabs instead of 2 space indents -- OK
         // Dictionary style -- not good
        |> writer.Write)

  let internal summary (report : DocumentType) (format : ReportFormat) result =
    match report with
    | XML document ->
      doWithStream(fun () -> File.OpenWrite(!path |> Option.get))
       (convertReport document format)
    | _ -> ()
    (result, 0uy, String.Empty)