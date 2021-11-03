namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Xml.Linq

open Mono.Cecil
open Mono.Cecil.Rocks

[<SuppressMessage("Microsoft.Naming", "CA1704", Justification = "'Json' is jargon")>]
module internal Json =
  let internal path : Option<string> ref = ref None

  // --- NCover ---

  let internal lineVisits (seqpnts: NativeJson.SeqPnt seq) =
    seqpnts
    |> Seq.fold (fun m sp -> Math.Max(m, sp.VC)) 0

  [<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Maintainability",
                                                    "AvoidUnnecessarySpecializationRule",
                                                    Justification = "AvoidSpeculativeGenerality too")>]
  let internal getMethodRecords
    (modul: NativeJson.Documents)
    (doc: string * string)
    (cname: string)
    (mname: string)
    =
    let classes =
      match modul.TryGetValue (fst doc) with
      | true, c -> c
      | _ ->
          let c = NativeJson.Classes()
          modul.Add(fst doc, c)
          NativeJson.injectEmbed c (snd doc)
          c

    let methods =
      match classes.TryGetValue cname with
      | true, m -> m
      | _ ->
          let m = NativeJson.Methods()
          classes.Add(cname, m)
          m

    match methods.TryGetValue mname with
    | true, m -> (methods, m)
    | _ ->
        let m = NativeJson.Method.Create(None)
        methods.Add(mname, m)
        (methods, m)

  [<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Maintainability",
                                                    "AvoidUnnecessarySpecializationRule",
                                                    Justification = "AvoidSpeculativeGenerality too")>]
  let internal getMethodRecord
    (modul: NativeJson.Documents)
    (doc: string * string)
    (cname: string)
    (mname: string)
    =
    getMethodRecords modul doc cname mname |> snd

  [<SuppressMessage("Gendarme.Rules.Maintainability",
                    "AvoidUnnecessarySpecializationRule",
                    Justification = "AvoidSpeculativeGenerality too")>]
  [<SuppressMessage("Gendarme.Rules.Smells",
                    "AvoidLongParameterListsRule",
                    Justification = "Long enough but no longer")>]
  let internal updateMethodRecord
    (modul: NativeJson.Documents)
    (doc: string * string)
    (cname: string)
    (mname: string)
    (update: (Nullable<int> * NativeJson.Times * NativeJson.Times))
    =
    let methods, m0 = getMethodRecords modul doc cname mname
    let tid, entry, exit = update

    if tid.HasValue && tid <> m0.TId then
      let next =
        { m0 with
            TId = tid
            Entry = entry
            Exit = exit }

      methods.[mname] <- next
      next
    else
      m0

  let internal maybeAssembly path =
    Some path
    |> Option.filter File.Exists
    |> Option.map
         (fun p ->
           let def = AssemblyDefinition.ReadAssembly p
           ProgramDatabase.readSymbols def
           def)

  let internal maybeDispose (def: AssemblyDefinition option) =
    def |> Option.iter (fun d -> d.Dispose())

  // try to find the method in the assembly
  [<SuppressMessage("Gendarme.Rules.Smells",
                    "AvoidLongParameterListsRule",
                    Justification = "Long enough but no longer")>]
  let internal maybeNames
    (def: AssemblyDefinition option)
    (fallbackm: string)
    (fallbackc: string)
    (sp: NativeJson.SeqPnt option)
    (cname: string)
    (mname: string)
    =
    let td =
      def
      |> Option.map
           (fun a ->
             a.MainModule.GetAllTypes()
             |> Seq.tryFind (fun t -> t.FullName = cname))
      |> Option.flatten

    let md =
      td
      |> Option.map
           (fun t ->
             t.Methods
             |> Seq.tryFind
                  (fun m ->
                    m.FullName = mname
                    || (m.Name = mname
                        && (let dbg = m.DebugInformation

                            dbg.HasSequencePoints
                            && Option.isSome sp
                            && (let pt = dbg.SequencePoints |> Seq.head

                                pt.StartLine = sp.Value.SL
                                && pt.StartColumn = sp.Value.SC)))))
      |> Option.flatten

    let truemd =
      md
      |> Option.map (Visitor.I.containingMethods >> Seq.last)

    let methodName =
      truemd
      |> Option.map (fun m -> m.FullName)
      |> Option.defaultValue fallbackm

    let className =
      truemd
      |> Option.map (fun m -> m.DeclaringType.FullName)
      |> Option.defaultValue fallbackc

    (className, methodName)

  [<SuppressMessage("Gendarme.Rules.Maintainability",
                    "AvoidUnnecessarySpecializationRule",
                    Justification = "AvoidSpeculativeGenerality too")>]
  let internal ncoverToJson (report: XElement) =
    let json = NativeJson.Modules()

    report.Descendants(XName.Get "module")
    |> Seq.iter
         (fun x ->
           let path = x.Attribute(XName.Get "name").Value
           let modul = NativeJson.Documents()

           let counts =
             System.Collections.Generic.Dictionary<string, int>()
           let embeds =
             System.Collections.Generic.Dictionary<string, string>()

           let def = maybeAssembly path

           try
             x.Descendants(XName.Get "method")
             |> Seq.iter
                  (fun m ->
                    let mname = m.Attribute(XName.Get "name").Value

                    let cname =
                      m
                        .Attribute(XName.Get "class")
                        .Value.Replace('+', '/')

                    let outerclass = cname.Split('/') |> Seq.head

                    let _, excluded =
                      m.Attribute(XName.Get "excluded").Value
                      |> Boolean.TryParse

                    let jmethods = System.Collections.Generic.Dictionary<string, NativeJson.Method>()

                    if not excluded then
                      let basename = sprintf "%s::%s" cname mname
                      let _, count = counts.TryGetValue basename
                      let index = count + 1
                      counts.[basename] <- index

                      let synth =
                        sprintf "ReturnType%d %s(Argument List%d)" index basename index

                      let spoints = m.Descendants(XName.Get "seqpnt")

                      let makeSeqPnt (x:XElement) =
                        let parse n =
                          x.Attribute(XName.Get n)
                          |> Option.ofObj
                          |> Option.map (fun a -> a.Value |> Int32.TryParse |> snd)
                          |> Option.defaultValue 0

                        { NativeJson.SeqPnt.VC = parse "visitcount"
                          NativeJson.SeqPnt.SL = parse "line"
                          NativeJson.SeqPnt.SC = parse "column"
                          NativeJson.SeqPnt.EL = parse "endline"
                          NativeJson.SeqPnt.EC = parse "endcolumn"
                          NativeJson.SeqPnt.Offset = parse "offset"
                          NativeJson.SeqPnt.Id = 0
                          NativeJson.SeqPnt.Times = null
                          NativeJson.SeqPnt.Tracks = null }

                      let sp = spoints
                               |> Seq.tryHead
                               |> Option.map makeSeqPnt

                      let (className, methodName) =
                        maybeNames
                          def
                          synth
                          outerclass
                          sp
                          cname
                          mname

                      spoints
                      |> Seq.iter
                           (fun s ->
                             let _, excluded =
                               s.Attribute(XName.Get "excluded").Value
                               |> Boolean.TryParse

                             if not excluded then
                                let doc = s.Attribute(XName.Get "document").Value
                                let embed =
                                  let (ok, result) = embeds.TryGetValue(doc)
                                  if ok then result
                                  else
                                    let found = s.Ancestors("module".X)
                                                |> Seq.collect (fun m -> m.Descendants("altcover.file".X))
                                                |> Seq.filter (fun f -> f.Attribute(XName.Get "document").Value = doc)
                                                |> Seq.tryHead
                                                |> Option.map (fun f -> f.Attribute(XName.Get "embed").Value)
                                                |> Option.filter (String.IsNullOrWhiteSpace >> not)
                                                |> Option.defaultValue String.Empty
                                    embeds.Add(doc, found)
                                    found

                                let docname = (doc, embed)
                                let jmethod =
                                  getMethodRecord modul docname className methodName
                                jmethods.[doc] <- jmethod

                                s
                                |> makeSeqPnt
                                |> jmethod.SeqPnts.Add)

                      jmethods.Values
                      |> Seq.iter (fun jm -> jm.SeqPnts
                                             |> Seq.groupBy (fun s -> s.SL)
                                             |> Seq.iter (fun (l, ss) -> jm.Lines.[l] <- lineVisits ss)))
           finally
             maybeDispose def

           if modul.Count > 0 then
             json.Add(path |> Path.GetFileName, modul))

    json

  [<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Maintainability",
                                                    "AvoidUnnecessarySpecializationRule",
                                                    Justification = "AvoidSpeculativeGenerality too")>]
  let internal opencoverToJson (report: XElement) =
    let json = NativeJson.Modules()

    report.Descendants(XName.Get "Module")
    |> Seq.iter
         (fun x ->
           let path =
             (x.Elements(XName.Get "ModulePath") |> Seq.head)
               .Value

           let modul = NativeJson.Documents()

           let files =
             System.Collections.Generic.Dictionary<string, (string*string)>()

           x.Descendants(XName.Get "File")
           |> Seq.iter
                (fun x ->
                  let document = x.Attribute(XName.Get "fullPath").Value

                  let embed = x.Attribute(XName.Get "altcover.embed")
                              |> Option.ofObj
                              |> Option.map (fun e -> e.Value)
                              |> Option.defaultValue String.Empty

                  files.Add(
                    x.Attribute(XName.Get "uid").Value,
                    (document, embed)

                  ))

           let tracked =
             System.Collections.Generic.Dictionary<string, int * NativeJson.Times * NativeJson.Times>
               ()

           x.Descendants(XName.Get "TrackedMethod")
           |> Seq.iter
                (fun x ->
                  tracked.Add(
                    x.Attribute(XName.Get "name").Value,
                    (x.Attribute(XName.Get "uid").Value
                     |> Int32.TryParse
                     |> snd,
                     (let e = NativeJson.Times()

                      x.Attribute(XName.Get "entry").Value.Split(';')
                      |> Seq.map (Int64.TryParse >> snd >> NativeJson.fromTracking)
                      |> e.AddRange

                      e),
                     (let e = NativeJson.Times()

                      x.Attribute(XName.Get "exit").Value.Split(';')
                      |> Seq.map (Int64.TryParse >> snd >> NativeJson.fromTracking)
                      |> e.AddRange

                      e))
                  ))

           let def = maybeAssembly path

           try
             x.Descendants(XName.Get "Method")
             |> Seq.iter
                  (fun m ->
                    let mname =
                      (m.Elements(XName.Get "Name") |> Seq.head).Value

                    let cname =
                      (m.Parent.Parent.Elements(XName.Get "FullName")
                       |> Seq.head)
                        .Value.Replace('+', '/')

                    let outerclass = cname.Split('/') |> Seq.head
                    let jmethods = System.Collections.Generic.Dictionary<string, NativeJson.Method>()

                    m.Descendants(XName.Get "SequencePoint")
                    |> Seq.iter
                         (fun s ->
                           let parse n =
                             s.Attribute(XName.Get n)
                             |> Option.ofObj
                             |> Option.map (fun a -> a.Value |> Int32.TryParse |> snd)
                             |> Option.defaultValue 0

                           let docname = files.[s.Attribute(XName.Get "fileid").Value]
                           let sp =
                             { NativeJson.SeqPnt.VC = parse "vc"
                               NativeJson.SeqPnt.SL = parse "sl"
                               NativeJson.SeqPnt.SC = parse "sc"
                               NativeJson.SeqPnt.EL = parse "el"
                               NativeJson.SeqPnt.EC = parse "ec"
                               NativeJson.SeqPnt.Offset = parse "offset"
                               NativeJson.SeqPnt.Id = parse "uspid"
                               NativeJson.SeqPnt.Times =
                                 let t = s.Descendants(XName.Get "Time")

                                 if t |> isNull || t |> Seq.isEmpty then
                                   null
                                 else
                                   let t2 = NativeJson.Times()

                                   t
                                   |> Seq.map
                                        (fun x ->
                                          x.Attribute(XName.Get "time").Value
                                          |> Int64.TryParse
                                          |> snd
                                          |> NativeJson.fromTracking)
                                   |> t2.AddRange

                                   t2

                               NativeJson.SeqPnt.Tracks =
                                 let t =
                                   s.Descendants(XName.Get "TrackedMethodRef")

                                 if t |> isNull || t |> Seq.isEmpty then
                                   null
                                 else
                                   let t2 = NativeJson.Tracks()

                                   t
                                   |> Seq.map
                                        (fun x ->
                                          x.Attribute(XName.Get "uid").Value
                                          |> Int32.TryParse
                                          |> snd)
                                   |> t2.AddRange

                                   t2

                             }
                           let (className, methodName) =
                              maybeNames def mname outerclass None cname mname

                           let (tid, entry, exit) =
                             if tracked.ContainsKey mname then
                               let (tid0, entry, exit) = tracked.[mname]
                               (Nullable<int>(tid0), entry, exit)
                             else
                               (System.Nullable(), null, null)

                           updateMethodRecord modul docname className mname (tid, entry, exit)
                           |> ignore

                           let jmethod =
                             getMethodRecord modul docname className methodName
                           jmethods.[fst docname] <- jmethod
                           jmethod.SeqPnts.Add sp
                           )

                    let bp = NativeJson.Branches()

                    m.Descendants(XName.Get "BranchPoint")
                    |> Seq.iter
                         (fun s ->
                           let parse n =
                             s.Attribute(XName.Get n)
                             |> Option.ofObj
                             |> Option.map (fun a -> a.Value |> Int32.TryParse |> snd)
                             |> Option.defaultValue 0

                           let docname = files.[s.Attribute(XName.Get "fileid").Value]
                           let offset = parse "offset"

                           let jbp =
                             { NativeJson.BranchInfo.Hits = parse "vc"
                               NativeJson.BranchInfo.Line = parse "sl"
                               NativeJson.BranchInfo.EndOffset = parse "offsetend"
                               NativeJson.BranchInfo.Path =
                                 bp
                                 |> Seq.filter (fun k -> k.Offset = offset)
                                 |> Seq.length
                               NativeJson.BranchInfo.Ordinal = uint bp.Count
                               NativeJson.BranchInfo.Offset = offset
                               NativeJson.BranchInfo.Id = parse "uspid"
                               NativeJson.BranchInfo.Times =
                                 let t = s.Descendants(XName.Get "Time")

                                 if t |> isNull || t |> Seq.isEmpty then
                                   null
                                 else
                                   let t2 = NativeJson.Times()

                                   t
                                   |> Seq.map
                                        (fun x ->
                                          x.Attribute(XName.Get "time").Value
                                          |> Int64.TryParse
                                          |> snd
                                          |> NativeJson.fromTracking)
                                   |> t2.AddRange

                                   t2

                               NativeJson.BranchInfo.Tracks =
                                 let t =
                                   s.Descendants(XName.Get "TrackedMethodRef")

                                 if t |> isNull || t |> Seq.isEmpty then
                                   null
                                 else
                                   let t2 = NativeJson.Tracks()

                                   t
                                   |> Seq.map
                                        (fun x ->
                                          x.Attribute(XName.Get "uid").Value
                                          |> Int32.TryParse
                                          |> snd)
                                   |> t2.AddRange

                                   t2

                             }
                           let (className, methodName) =
                              maybeNames def mname outerclass None cname mname

                           let (tid, entry, exit) =
                             if tracked.ContainsKey mname then
                               let (tid0, entry, exit) = tracked.[mname]
                               (Nullable<int>(tid0), entry, exit)
                             else
                               (System.Nullable(), null, null)

                           updateMethodRecord modul docname className mname (tid, entry, exit)
                           |> ignore

                           let jmethod =
                             getMethodRecord modul docname className methodName
                           jmethods.[fst docname] <- jmethod
                           jmethod.Branches.Add jbp
                           bp.Add jbp // for counting paths
                           )
                    jmethods.Values
                      |> Seq.iter (fun jm -> jm.SeqPnts
                                             |> Seq.groupBy (fun s -> s.SL)
                                             |> Seq.iter (fun (l, ss) -> jm.Lines.[l] <- lineVisits ss)))
           finally
             maybeDispose def

           if modul.Count > 0 then
             json.Add(path |> Path.GetFileName, modul))

    json

  [<SuppressMessage("Gendarme.Rules.Performance",
                    "AvoidUncalledPrivateCodeRule",
                    Justification = "An internal API")>]
  let internal xmlToJson (report: XDocument) (format: ReportFormat) =
    (report.Root
     |> (match format with
         | ReportFormat.NCover -> ncoverToJson
         | _ -> opencoverToJson))
    |> NativeJson.toText