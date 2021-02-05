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
    | true, m -> m
    | _ -> let m = NativeJson.Method.Create(None)
           methods.Add(mname, m)
           m

  let maybeAssembly path =
    Some path
    |> Option.filter File.Exists
    |> Option.map (fun p -> let def = AssemblyDefinition.ReadAssembly p
                            ProgramDatabase.readSymbols def
                            def)

  [<System.Diagnostics.CodeAnalysis.SuppressMessage(
    "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
    Justification = "AvoidSpeculativeGenerality too")>]
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
              // try to find the method in the assembly
              let td = def
                       |> Option.map (fun a -> a.MainModule.GetAllTypes()
                                               |> Seq.tryFind(fun t -> t.FullName = cname))
                       |> Option.flatten

              let md = td
                       |> Option.map(fun t -> t.Methods
                                              |> Seq.tryFind(fun m -> let dbg = m.DebugInformation
                                                                      m.Name = mname &&
                                                                      dbg.HasSequencePoints &&
                                                                      (let pt = dbg.SequencePoints
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
    JsonSerializer.Serialize(json, NativeJson.options)

  [<System.Diagnostics.CodeAnalysis.SuppressMessage(
    "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
    Justification = "AvoidSpeculativeGenerality too")>]
  let opencoverToJson (report: XElement) =
    let json = NativeJson.Modules()
    report.Descendants(XName.Get "Module")
    |> Seq.iter (fun x ->
      let path = (x.Elements(XName.Get "ModulePath") |> Seq.head).Value
      let modul = NativeJson.Documents()
      let files = System.Collections.Generic.Dictionary<string, string>()
      x.Descendants(XName.Get "File")
      |> Seq.iter (fun x -> files.Add(x.Attribute(XName.Get "uid").Value,
                                      x.Attribute(XName.Get "fullPath").Value) )
      let def = maybeAssembly path

      try
        x.Descendants(XName.Get "Method")
        |> Seq.iter (fun m ->
          let mname = (m.Elements(XName.Get "Name") |> Seq.head).Value
          let cname = (m.Parent.Parent.Elements(XName.Get "FullName") |> Seq.head).Value.Replace('+', '/')
          let outerclass = cname.Split('/') |> Seq.head

          let mutable docname = String.Empty

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

            if String.IsNullOrWhiteSpace docname
            then docname <- files.[s.Attribute(XName.Get "fileid").Value]
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

            if String.IsNullOrWhiteSpace docname
            then docname <- files.[s.Attribute(XName.Get "fileid").Value]
            {
                NativeJson.BranchInfo.Hits = parse "vc"
                NativeJson.BranchInfo.Line = parse "sl"
                NativeJson.BranchInfo.EndOffset = parse "offsetend"
                NativeJson.BranchInfo.Path = 0 // TODO
                NativeJson.BranchInfo.Ordinal = 0u // TODO
                NativeJson.BranchInfo.Offset = parse "offset"
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
            // try to find the method in the assembly
            let td = def
                      |> Option.map (fun a -> a.MainModule.GetAllTypes()
                                              |> Seq.tryFind(fun t -> t.FullName = cname))
                      |> Option.flatten

            let md = td
                      |> Option.map(fun t -> t.Methods
                                            |> Seq.tryFind(fun m -> m.FullName = mname))
                      |> Option.flatten

            let truemd = md
                         |> Option.map (Visitor.I.containingMethods >> Seq.last)

            let methodName = truemd
                              |> Option.map (fun m -> m.FullName)
                              |> Option.defaultValue mname
            let className = truemd
                            |> Option.map (fun m -> m.DeclaringType.FullName)
                            |> Option.defaultValue outerclass
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
        def
        |> Option.iter(fun d -> d.Dispose())
      if modul.Count > 0
      then
        json.Add(path |> Path.GetFileName, modul)
    )
    JsonSerializer.Serialize(json, NativeJson.options)

  let internal convertReport (report : XDocument) (format:ReportFormat) (stream : Stream) =
    doWithStream (fun () -> new StreamWriter(stream)) (fun writer ->
        (report.Root
        |> (match format with
            | ReportFormat.NCover -> ncoverToJson
            | _ -> opencoverToJson))
        |> writer.Write)

  let internal summary (report : DocumentType) (format : ReportFormat) result =
    doWithStream(fun () -> File.OpenWrite(!path |> Option.get))
      (match report with
       | XML document -> convertReport document format
       | _ -> ignore)
    (result, 0uy, String.Empty)