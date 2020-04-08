namespace AltCover

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.Globalization
open System.IO
open System.IO.Compression
open System.Text
open System.Xml
open System.Xml.Linq

open Mono.Cecil
open Mono.Options
open Augment
open AltCover.Base

[<ExcludeFromCodeCoverage; NoComparison>]
type TeamCityFormat =
  | Default
  | [<SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
    Justification="TeamCity notation")>]
    R
  | [<SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
    Justification="TeamCity notation")>]
    B
  | RPlus
  | BPlus
  [<SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
    Justification="trivial usage")>]
  static member Factory s =
    let (|Select|_|) (pattern : String) offered =
      if offered
         |> String.IsNullOrWhiteSpace
         |> not
         && pattern.Equals
              (String
                (offered
                 |> Seq.sort
                 |> Seq.toArray), StringComparison.OrdinalIgnoreCase) then
        Some offered
      else
        None

    match s with
    | Select "B" _ -> B
    | Select "+" _
    | Select "+B" _ -> BPlus
    | Select "R" _ -> R
    | Select "+R" _ -> RPlus
    | _ -> Default

module internal Runner =

  let mutable internal recordingDirectory : Option<string> = None
  let mutable internal workingDirectory : Option<string> = None
  let internal executable : Option<string> ref = ref None
  let internal collect = ref false // ddFlag
  let mutable internal threshold : Option<int> = None
  let mutable internal output : Option<string> = None
  let internal summary = StringBuilder()
  let mutable internal summaryFormat = TeamCityFormat.Default

  let internal init() =
    CommandLine.error <- []
    CommandLine.dropReturnCode := false
    recordingDirectory <- None
    workingDirectory <- None
    executable := None
    LCov.path := None
    Cobertura.path := None
    collect := false
    threshold <- None
    output <- None
    summaryFormat <- Default
    summary.Clear() |> ignore

  module internal I =

    let internal write line =
      [ summary.AppendLine >> ignore
        Output.info ]
      |> Seq.iter (fun f -> f line)

    let internal writeSummary key vc nc pc =
      let line =
        CommandLine.Format.Local(key, vc, nc, pc)
      write line

    let private totalTC =
      "##teamcity[buildStatisticValue key='CodeCoverageAbs{0}Total' value='{1}']"
    let private coverTC =
      "##teamcity[buildStatisticValue key='CodeCoverageAbs{0}Covered' value='{1}']"

    let internal writeTC template what value =
      let line = String.Format(CultureInfo.InvariantCulture, template, what, value)
      write line

    [<System.Diagnostics.CodeAnalysis.SuppressMessage(
      "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
      Justification = "AvoidSpeculativeGenerality too")>]
    let internal nCoverSummary(report : XDocument) =
      let makepc v n =
        if n = 0 then
          "n/a"
        else
          Math.Round((float v) * 100.0 / (float n), 2)
              .ToString(CultureInfo.InvariantCulture)

      let summarise v n key =
        let pc = makepc v n
        writeSummary key v n pc

      let methods =
        report.Descendants("method".X)
        |> Seq.filter (fun m -> m.Attribute("excluded".X).Value = "false")
        |> Seq.toList

      let classes =
        methods
        |> Seq.groupBy (fun m -> m.Attribute("class".X).Value)
        |> Seq.toList

      let isVisited (x : XElement) =
        let v = x.Attribute("visitcount".X)
        (v
         |> isNull
         |> not)
        && (v.Value <> "0")

      let vclasses =
        classes
        |> Seq.filter (fun (_, ms) ->
             ms |> Seq.exists (fun m -> m.Descendants("seqpnt".X) |> Seq.exists isVisited))
        |> Seq.length

      let vmethods =
        methods
        |> Seq.filter (fun m -> m.Descendants("seqpnt".X) |> Seq.exists isVisited)
        |> Seq.length

      let points =
        report.Descendants("seqpnt".X)
        |> Seq.filter (fun m -> m.Attribute("excluded".X).Value = "false")
        |> Seq.toList

      let vpoints =
        points
        |> Seq.filter isVisited
        |> Seq.length

      let emitSummary() =
        if [ Default; BPlus; RPlus ] |> Seq.exists (fun x -> x = summaryFormat) then
          summarise vclasses classes.Length "VisitedClasses"
          summarise vmethods methods.Length "VisitedMethods"
          summarise vpoints points.Length "VisitedPoints"

        if [ B; R; BPlus; RPlus ] |> Seq.exists (fun x -> x = summaryFormat) then
          writeTC totalTC "C" classes.Length
          writeTC coverTC "C" vclasses
          writeTC totalTC "M" methods.Length
          writeTC coverTC "M" vmethods
          writeTC totalTC "S" points.Length
          writeTC coverTC "S" vpoints

      emitSummary()

      makepc vpoints points.Length

    [<System.Diagnostics.CodeAnalysis.SuppressMessage(
      "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
      Justification = "AvoidSpeculativeGenerality too")>]
    let internal altSummary(report : XDocument) =
      "Alternative"
      |> CommandLine.resources.GetString
      |> write

      let classes =
        report.Descendants("Class".X)
        |> Seq.filter (fun c -> c.Attribute("skippedDueTo".X) |> isNull)
        |> Seq.filter (fun c ->
             c.Descendants("Method".X)
             |> Seq.isEmpty
             |> not)
        |> Seq.toList

      let vclasses =
        classes
        |> Seq.filter (fun c ->
             c.Descendants("Method".X)
             |> Seq.exists (fun m -> m.Attribute("visited".X).Value = "true"))
        |> Seq.length

      let nc = classes.Length

      let pc =
        if nc = 0 then
          "n/a"
        else
          Math.Round((float vclasses) * 100.0 / (float nc), 2)
              .ToString(CultureInfo.InvariantCulture)
      writeSummary "AltVC" vclasses nc pc

      let methods =
        classes
        |> Seq.collect (fun c -> c.Descendants("Method".X))
        |> Seq.filter (fun c -> c.Attribute("skippedDueTo".X) |> isNull)
        |> Seq.toList

      let vm =
        methods
        |> Seq.filter (fun m -> m.Attribute("visited".X).Value = "true")
        |> Seq.length

      let nm = methods.Length

      let pm =
        if nm = 0 then
          "n/a"
        else
          Math.Round((float vm) * 100.0 / (float nm), 2)
              .ToString(CultureInfo.InvariantCulture)
      writeSummary "AltVM" vm nm pm

    let internal openCoverSummary(report : XDocument) =
      let summary = report.Descendants("Summary".X) |> Seq.head

      let summarise go (visit : string) (number : string) (precalc : string option) key =
        let vc = summary.Attribute(visit.X).Value
        let nc = summary.Attribute(number.X).Value

        let pc =
          match precalc with
          | None ->
              if nc = "0" then
                "n/a"
              else
                let vc1 =
                  vc
                  |> Int32.TryParse
                  |> snd
                  |> float

                let nc1 =
                  nc
                  |> Int32.TryParse
                  |> snd
                  |> float

                Math.Round(vc1 * 100.0 / nc1, 2).ToString(CultureInfo.InvariantCulture)
          | Some x -> summary.Attribute(x.X).Value
        if go then writeSummary key vc nc pc
        (vc, nc, pc)

      let go = [ Default; BPlus; RPlus ] |> Seq.exists (fun x -> x = summaryFormat)
      let (vc, nc, _) =
        summarise go "visitedClasses" "numClasses" None "VisitedClasses"
      let (vm, nm, _) =
        summarise go "visitedMethods" "numMethods" None "VisitedMethods"
      let (vs, ns, covered) =
        summarise go "visitedSequencePoints" "numSequencePoints" (Some "sequenceCoverage")
          "VisitedPoints"
      let (vb, nb, _) =
        summarise go "visitedBranchPoints" "numBranchPoints" (Some "branchCoverage")
          "VisitedBranches"
      if go then
        write String.Empty
        altSummary report

      if [ B; R; BPlus; RPlus ] |> Seq.exists (fun x -> x = summaryFormat) then
        writeTC totalTC "C" nc
        writeTC coverTC "C" vc
        writeTC totalTC "M" nm
        writeTC coverTC "M" vm
        writeTC totalTC "S" ns
        writeTC coverTC "S" vs
        let tag =
          match summaryFormat with
          | R
          | RPlus -> "R"
          | _ -> "B"
        writeTC totalTC tag nb
        writeTC coverTC tag vb

      covered

    let internal invariantParseDouble d =
      Double.TryParse(d, NumberStyles.Number, CultureInfo.InvariantCulture)

    let internal standardSummary (report : XDocument) (format : Base.ReportFormat) result =
      let covered =
        report
        |> match format with
           | Base.ReportFormat.NCover -> nCoverSummary
           | _ -> openCoverSummary
        |> invariantParseDouble

      let value =
        match covered with
        | (false, _) -> 0.0
        | (_, x) -> x

      match threshold with
      | None -> result
      | Some x ->
          let f = float x
          if f <= value then result else Math.Ceiling(f - value) |> int

    let mutable internal summaries : (XDocument -> Base.ReportFormat -> int -> int) list =
      []

    let internal addLCovSummary() =
      summaries <- LCov.summary :: summaries
    let internal addCoberturaSummary() =
      summaries <- Cobertura.summary :: summaries

  // "Public"
  let internal validateThreshold x =
    let (q, n) =
      Int32.TryParse(if (String.IsNullOrWhiteSpace(x)) then "!" else x)

    let ok = q && (n >= 0) && (n <= 100)
    if ok |> not then
      CommandLine.error <-
        CommandLine.Format.Local("InvalidValue",
           "--threshold", x) :: CommandLine.error
    (ok, n)

  let internal declareOptions() =
    I.summaries <- []
    I.summaries <- I.standardSummary :: I.summaries
    [ ("r|recorderDirectory=",
       (fun x ->
         if CommandLine.validateDirectory "--recorderDirectory" x then
           if Option.isSome recordingDirectory then
             CommandLine.error <-
               CommandLine.Format.Local("MultiplesNotAllowed",
                  "--recorderDirectory") :: CommandLine.error
           else
             recordingDirectory <- Some(Path.GetFullPath x)))
      ("w|workingDirectory=",
       (fun x ->
         if CommandLine.validateDirectory "--workingDirectory" x then
           if Option.isSome workingDirectory then
             CommandLine.error <-
               CommandLine.Format.Local("MultiplesNotAllowed",
                  "--workingDirectory") :: CommandLine.error
           else
             workingDirectory <- Some(Path.GetFullPath x)))
      ("x|executable=",
       (fun x ->
         if CommandLine.validatePath "--executable" x then
           if Option.isSome !executable then
             CommandLine.error <-
               CommandLine.Format.Local("MultiplesNotAllowed", "--executable")
               :: CommandLine.error
           else
             executable := Some x))
      (CommandLine.ddFlag "collect" collect)
      ("l|lcovReport=",
       (fun x ->
         if CommandLine.validatePath "--lcovReport" x then
           if Option.isSome !LCov.path then
             CommandLine.error <-
               CommandLine.Format.Local("MultiplesNotAllowed", "--lcovReport")
               :: CommandLine.error
           else
             LCov.path := x
                          |> Path.GetFullPath
                          |> Some
             I.addLCovSummary()))
      ("t|threshold=",
       (fun x ->
         let ok, n = validateThreshold x
         if ok then
           if Option.isSome threshold then
             CommandLine.error <-
               CommandLine.Format.Local("MultiplesNotAllowed", "--threshold")
               :: CommandLine.error
           else
             threshold <- Some n))
      ("c|cobertura=",
       (fun x ->
         if CommandLine.validatePath "--cobertura" x then
           if Option.isSome !Cobertura.path then
             CommandLine.error <-
               CommandLine.Format.Local("MultiplesNotAllowed", "--cobertura")
               :: CommandLine.error
           else
             Cobertura.path := x
                               |> Path.GetFullPath
                               |> Some
             I.addCoberturaSummary()))
      ("o|outputFile=",
       (fun x ->
         if CommandLine.validatePath "--outputFile" x then
           if Option.isSome output then
             CommandLine.error <-
               CommandLine.Format.Local("MultiplesNotAllowed", "--outputFile")
               :: CommandLine.error
           else
             output <-
               x
               |> Path.GetFullPath
               |> Some))
      (CommandLine.ddFlag "dropReturnCode" CommandLine.dropReturnCode)
      ("teamcity:",
       (fun x ->
         if summaryFormat = Default then
           summaryFormat <-
             if String.IsNullOrWhiteSpace x then B else TeamCityFormat.Factory x
           if summaryFormat = Default then
             CommandLine.error <-
               CommandLine.Format.Local("InvalidValue", "--teamcity", x)
               :: CommandLine.error
         else
           CommandLine.error <-
             CommandLine.Format.Local("MultiplesNotAllowed", "--teamcity")
             :: CommandLine.error))
      ("?|help|h", (fun x -> CommandLine.help <- not (isNull x)))

      ("<>",
       (fun x ->
         CommandLine.error <-
           CommandLine.Format.Local( "InvalidValue",
              "AltCover", x) :: CommandLine.error)) ] // default end stop
    |> List.fold
         (fun (o : OptionSet) (p, a) ->
           o.Add(p, CommandLine.resources.GetString(p), new System.Action<string>(a)))
         (OptionSet())

  let internal requireRecorderTest recordingDirectory success fail =
    match recordingDirectory with
    | None ->
        CommandLine.error <-
          (CommandLine.resources.GetString "recorderRequired") :: CommandLine.error
        fail
    | Some path ->
        let dll = Path.Combine(path, "AltCover.Recorder.g.dll")
        if File.Exists dll then
          success
        else
          CommandLine.error <-
            CommandLine.Format.Local("recorderNotFound", dll)
            :: CommandLine.error
          fail

  let internal setRecordToFile report =
    doWithStream (fun () ->
      let binpath = report + ".acv"
      File.Create(binpath)) ignore

  module internal J =
    let internal requireExe(parse : Either<string * OptionSet, string list * OptionSet>) =
      match parse with
      | Right(l, options) ->
          match (!executable, !collect) with
          | (None, false)
          | (Some _, true) ->
              CommandLine.error <-
                (CommandLine.resources.GetString "executableRequired") :: CommandLine.error
              Left("UsageError", options)
          | (None, _) -> Right([], options)
          | (Some exe, _) -> Right(exe :: l, options)
      | fail -> fail

    let internal requireRecorder(parse : Either<string * OptionSet, string list * OptionSet>) =
      match parse with
      | Right(_, options) ->
          requireRecorderTest recordingDirectory parse (Left("UsageError", options))
      | fail -> fail

    let internal requireWorker(parse : Either<string * OptionSet, string list * OptionSet>) =
      match parse with
      | Right _ ->
          match workingDirectory with
          | None -> workingDirectory <- Directory.GetCurrentDirectory() |> Some
          | _ -> ()
          parse
      | fail -> fail

    // mocking point
    [<System.Diagnostics.CodeAnalysis.SuppressMessage(
        "Gendarme.Rules.Performance", "AvoidUncalledPrivateCodeRule",
        Justification = "Unit test accessor")>]
    let mutable internal recorderName = "AltCover.Recorder.g.dll"

    [<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Correctness",
           "EnsureLocalDisposalRule",
           Justification="Tuple return confusing Gendarme -- TODO")>]
    let internal recorderInstance() =
      let recorderPath = Path.Combine(Option.get recordingDirectory, recorderName)
      let definition = AssemblyDefinition.ReadAssembly recorderPath
      (definition, definition.MainModule.GetType("AltCover.Recorder.Instance"))

    let internal getMethod (t : TypeDefinition) (name : string) =
      t.Methods
      |> Seq.filter (fun m -> m.Name = name)
      |> Seq.head

    let internal getFirstOperandAsString(m : MethodDefinition) =
      m.Body.Instructions
      |> Seq.filter (fun i -> i.OpCode = Cil.OpCodes.Ldstr)
      |> Seq.map (fun i -> i.Operand :?> string)
      |> Seq.head

    let internal getFirstOperandAsNumber(m : MethodDefinition) =
      m.Body.Instructions
      |> Seq.filter (fun i -> i.OpCode = Cil.OpCodes.Ldc_I4)
      |> Seq.map (fun i -> i.Operand :?> int)
      |> Seq.head

    let internal payloadBase(rest : string list) =
      CommandLine.doPathOperation
        (fun () ->
          CommandLine.processTrailingArguments rest
            (DirectoryInfo(Option.get workingDirectory))) 255 true

    let internal runProcess report (payload : string list -> int) (args : string list) =
      setRecordToFile report
      "Beginning run..." |> CommandLine.writeResource
      let result = payload args
      "Getting results..." |> CommandLine.writeResource
      result

    let internal collectResults (hits : Dictionary<string, Dictionary<int, Base.PointVisit>>)
        report =
      let timer = System.Diagnostics.Stopwatch()
      timer.Start()
      let visits =
        Directory.GetFiles
          (Path.GetDirectoryName(report), Path.GetFileName(report) + ".*.acv")
        |> Seq.fold (fun before f ->
             timer.Restart()
             let length = FileInfo(f).Length.ToString("#,#", CultureInfo.CurrentCulture)
             sprintf "... %s (%sb)" f length |> Output.info
             use fileStream = File.OpenRead f
             use results = new DeflateStream(fileStream, CompressionMode.Decompress)
             use formatter = new System.IO.BinaryReader(results)

             let rec sink hitcount =
               let hit =
                 try
                   let id = formatter.ReadString()
                   let strike = formatter.ReadInt32()
                   let tag = formatter.ReadByte() |> int
                   Some
                     (id, strike,
                      match enum tag with
                      | Base.Tag.Time -> Base.Time <| formatter.ReadInt64()
                      | Base.Tag.Call -> Base.Call <| formatter.ReadInt32()
                      | Base.Tag.Both ->
                          let time = formatter.ReadInt64()
                          let call = formatter.ReadInt32()
                          Base.Both
                            { Time = time
                              Call = call }
                      | Base.Tag.Table ->
                          let t = Dictionary<string, Dictionary<int, PointVisit>>()

                          let rec ``module``() =
                            let m = formatter.ReadString()
                            if String.IsNullOrEmpty m then
                              ()
                            else
                              if m
                                 |> t.ContainsKey
                                 |> not
                              then t.Add(m, Dictionary<int, PointVisit>())
                              let points = formatter.ReadInt32()

                              let rec sequencePoint pts =
                                if pts > 0 then
                                  let p = formatter.ReadInt32()
                                  let n = formatter.ReadInt64()
                                  if p
                                     |> t.[m].ContainsKey
                                     |> not
                                  then t.[m].Add(p, PointVisit.Create())
                                  let pv = t.[m].[p]
                                  pv.Count <- pv.Count + n
                                  let rec tracking() =
                                    let track = formatter.ReadByte() |> int
                                    match enum track with
                                    | Tag.Time ->
                                        pv.Tracks.Add(Time <| formatter.ReadInt64())
                                        tracking()
                                    | Tag.Call ->
                                        pv.Tracks.Add(Call <| formatter.ReadInt32())
                                        tracking()
                                    | Tag.Both ->
                                        pv.Tracks.Add
                                          (let time = formatter.ReadInt64()
                                           let call = formatter.ReadInt32()
                                           Base.Both
                                             { Time = time
                                               Call = call })
                                        tracking()
                                    // Expect never to happen                                    | Tag.Table -> ``module``()
                                    | _ -> sequencePoint (pts - 1)
                                  tracking()
                                else
                                  ``module``()
                              sequencePoint points
                          ``module``()
                          Table t
                      | _ -> Null)
                 with :? EndOfStreamException -> None
               match hit with
               | Some tuple ->
                   let (key, hitPointId, visit) = tuple

                   let increment =
                     if key
                        |> String.IsNullOrWhiteSpace
                        |> not
                        || ((String.IsNullOrEmpty key) && hitPointId = 0
                            && visit.GetType().ToString() = "AltCover.Base.Track+Table") then
                       Base.Counter.addVisit hits key hitPointId visit
                     else
                       0L
                   sink (hitcount + increment)
               | None -> hitcount

             let after = sink before
             timer.Stop()
             if after > before then
               let delta = after - before
               let interval = timer.Elapsed
               let rate = (float delta) / interval.TotalSeconds
               CommandLine.writeResourceWithFormatItems "%d visits recorded in %A (%A visits/sec)"
                 [| delta :> obj
                    interval
                    rate |] false
             after) 0L
      timer.Stop()
      CommandLine.writeResourceWithFormatItems "%d visits recorded" [| visits |] (visits = 0L)

    let internal monitorBase (hits : Dictionary<string, Dictionary<int, Base.PointVisit>>)
        report (payload : string list -> int) (args : string list) =
      let result =
        if !collect then 0 else runProcess report payload args
      collectResults hits report
      result

    let internal copyFillMethodPoint (mp : XmlElement seq) sp =
      mp
      |> Seq.iter (fun m ->
           m.SetAttribute
             ("type", "http://www.w3.org/2001/XMLSchema-instance", "SequencePoint")
           |> ignore
           sp
           |> Seq.cast<XmlElement>
           |> Seq.take 1
           |> Seq.collect (fun p -> p.Attributes |> Seq.cast<XmlAttribute>)
           |> Seq.iter (fun a -> m.SetAttribute(a.Name, a.Value)))

    [<System.Diagnostics.CodeAnalysis.SuppressMessage(
      "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
      Justification = "AvoidSpeculativeGenerality too")>]
    let internal lookUpVisitsByToken token (dict : Dictionary<int, Base.PointVisit>) =
      let (ok, index) =
        Int32.TryParse
          (token, System.Globalization.NumberStyles.Integer,
           System.Globalization.CultureInfo.InvariantCulture)
      match dict.TryGetValue(if ok then index else -1) with
      | (false, _) -> PointVisit.Create()
      | (_, pair) -> pair

    let internal fillMethodPoint (mp : XmlElement seq) (method : XmlElement)
        (dict : Dictionary<int, Base.PointVisit>) =
      let token =
        use elements = method.GetElementsByTagName("MetadataToken")
        elements
        |> Seq.cast<XmlElement>
        |> Seq.map (fun m -> m.InnerText)
        |> Seq.head

      let vc = (lookUpVisitsByToken token dict).Total()
      mp
      |> Seq.iter (fun m ->
           m.SetAttribute("vc", vc.ToString(CultureInfo.InvariantCulture))
           m.SetAttribute("uspid", token)
           m.SetAttribute("ordinal", "0")
           m.SetAttribute("offset", "0"))

    let visitCount nodes =
      nodes
      |> Seq.cast<XmlElement>
      |> Seq.filter (fun s ->
           Int64.TryParse
             (s.GetAttribute("vc"), NumberStyles.Integer, CultureInfo.InvariantCulture)
           |> snd
           <> 0L)
      |> Seq.length

    [<System.Diagnostics.CodeAnalysis.SuppressMessage(
      "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
      Justification = "AvoidSpeculativeGenerality too")>]
    let internal tryGetValue (d : Dictionary<'a, 'b>) (key : 'a) =
      match d with
      | null -> (false, Unchecked.defaultof<'b>)
      | _ -> d.TryGetValue key

    let internal postProcess (counts : Dictionary<string, Dictionary<int, Base.PointVisit>>)
        format (document : XmlDocument) =
      match format with
      | Base.ReportFormat.OpenCoverWithTracking
      | Base.ReportFormat.OpenCover ->
          let scoreToString raw =
            (sprintf "%.2f" raw).TrimEnd([| '0' |]).TrimEnd([| '.' |])

          let stringToScore (node : XmlElement) name =
            node.GetAttribute(name)
            |> I.invariantParseDouble
            |> snd

          let percentCover visits points =
            if points = 0
            then "0"
            else ((float (visits * 100)) / (float points)) |> scoreToString

          let setSummary (x : XmlElement) pointVisits branchVisits methodVisits classVisits
              ptcover brcover minCrap maxCrap =
            use elements = x.GetElementsByTagName("Summary")
            elements
            |> Seq.cast<XmlElement>
            |> Seq.tryHead
            |> Option.iter (fun s ->
                 let minc =
                   (if minCrap = Double.MaxValue then 0.0 else minCrap)
                   |> scoreToString

                 let maxc =
                   (if maxCrap = Double.MinValue then 0.0 else maxCrap)
                   |> scoreToString

                 s.SetAttribute("visitedSequencePoints", sprintf "%d" pointVisits)
                 s.SetAttribute("visitedBranchPoints", sprintf "%d" branchVisits)
                 s.SetAttribute("visitedMethods", sprintf "%d" methodVisits)
                 classVisits
                 |> Option.iter
                      (fun cvc -> s.SetAttribute("visitedClasses", sprintf "%d" cvc))
                 s.SetAttribute("branchCoverage", brcover)
                 s.SetAttribute("sequenceCoverage", ptcover)
                 s.SetAttribute("minCrapScore", minc)
                 s.SetAttribute("maxCrapScore", maxc))

          let computeBranchExitCount (doc : XmlDocument) (sp : XmlNodeList) bp =
            let tail = doc.CreateElement("SequencePoint")
            tail.SetAttribute
              ("offset", Int32.MaxValue.ToString(CultureInfo.InvariantCulture))
            let nodes =
              List.concat
                [ sp
                  |> Seq.cast<XmlElement>
                  |> Seq.toList
                  [ tail ]
                  bp
                  |> Seq.cast<XmlElement>
                  |> Seq.toList ]

            let interleave =
              nodes
              |> Seq.sortBy (fun x ->
                   x.GetAttribute("offset")
                   |> Int32.TryParse
                   |> snd)

            interleave
            |> Seq.fold (fun (bev, sq : XmlElement) x ->
                 match x.Name with
                 | "SequencePoint" ->
                     sq.SetAttribute("bev", sprintf "%d" bev)
                     (0, x)
                 | _ ->
                     (bev + (if x.GetAttribute("vc") = "0" then 0 else 1), sq))
                 (0, nodes.[0])
            |> ignore

          let crapScore (method : XmlElement) =
            let coverage =
              let cover = stringToScore method "sequenceCoverage"
              if cover > 0.0 then cover else stringToScore method "branchCoverage"

            let complexity = stringToScore method "cyclomaticComplexity"

            let raw =
              (Math.Pow(complexity, 2.0) * Math.Pow((1.0 - (coverage / 100.0)), 3.0)
               + complexity)
            let score = raw |> scoreToString
            method.SetAttribute("crapScore", score)
            raw

          let updateMethod (dict : Dictionary<int, Base.PointVisit>)
              (vb, vs, vm, pt, br, minc, maxc) (method : XmlElement) =
            use sp = method.GetElementsByTagName("SequencePoint")
            use bp = method.GetElementsByTagName("BranchPoint")
            let mp =
              use elements = method.GetElementsByTagName("MethodPoint")
              elements |> Seq.cast<XmlElement> |> Seq.toList
            let count = sp.Count
            let rawCount = bp.Count

            // inconsistent name to shut Gendarme up
            let numBranches = rawCount + Math.Sign(count + rawCount)
            if count > 0 then
              copyFillMethodPoint mp sp
            else
              fillMethodPoint mp method dict
            let pointVisits = visitCount sp
            let b0 = visitCount bp
            let branchVisits = b0 + Math.Sign b0
            if pointVisits > 0 || b0 > 0 then
              let fillMethod() =
                let cover = percentCover pointVisits count
                let bcover = percentCover branchVisits numBranches
                method.SetAttribute("visited", "true")
                method.SetAttribute("sequenceCoverage", cover)
                method.SetAttribute("branchCoverage", bcover)
                let raw = crapScore method
                setSummary method pointVisits branchVisits 1 None cover bcover raw raw
                computeBranchExitCount method.OwnerDocument sp bp
                (vb + branchVisits, vs + pointVisits, vm + 1, pt + count, br + numBranches,
                 Math.Min(minc, raw), Math.Max(maxc, raw))
              fillMethod()
            else
              (vb, vs, vm, pt + count, br + numBranches, minc, maxc)

          let updateClass (dict : Dictionary<int, Base.PointVisit>)
              (vb, vs, vm, vc, pt, br, minc0, maxc0) (``class`` : XmlElement) =
            let (cvb, cvs, cvm, cpt, cbr, minc, maxc) =
              use elements = ``class``.GetElementsByTagName("Method")
              elements
              |> Seq.cast<XmlElement>
              |> Seq.fold (updateMethod dict)
                   (0, 0, 0, 0, 0, Double.MaxValue, Double.MinValue)

            let cover = percentCover cvs cpt
            let bcover = percentCover cvb cbr

            let cvc =
              if cvm > 0 then 1 else 0
            setSummary ``class`` cvs cvb cvm (Some cvc) cover bcover minc maxc
            (vb + cvb, vs + cvs, vm + cvm, vc + cvc, pt + cpt, br + cbr,
             Math.Min(minc, minc0), Math.Max(maxc, maxc0))

          let updateModule (counts : Dictionary<string, Dictionary<int, Base.PointVisit>>)
              (vb, vs, vm, vc, pt, br, minc0, maxc0) (``module`` : XmlElement) =
            let dict =
              match (tryGetValue counts) <| ``module``.GetAttribute("hash") with
              | (false, _) -> Dictionary<int, Base.PointVisit>()
              | (true, d) -> d

            let (cvb, cvs, cvm, cvc, cpt, cbr, minc, maxc) =
              use elements = ``module``.GetElementsByTagName("Class")
              elements
              |> Seq.cast<XmlElement>
              |> Seq.fold (dict |> updateClass)
                   (0, 0, 0, 0, 0, 0, Double.MaxValue, Double.MinValue)

            let cover = percentCover cvs cpt
            let bcover = percentCover cvb cbr
            setSummary ``module`` cvs cvb cvm (Some cvc) cover bcover minc maxc
            (vb + cvb, vs + cvs, vm + cvm, vc + cvc, pt + cpt, br + cbr,
             Math.Min(minc, minc0), Math.Max(maxc, maxc0))

          let (vb, vs, vm, vc, pt, br, minc, maxc) =
            use elements = document.DocumentElement.SelectNodes("//Module")
            elements
            |> Seq.cast<XmlElement>
            |> Seq.fold (updateModule counts)
                 (0, 0, 0, 0, 0, 0, Double.MaxValue, Double.MinValue)

          let cover = percentCover vs pt
          let bcover = percentCover vb br
          setSummary document.DocumentElement vs vb vm (Some vc) cover bcover minc maxc
      | _ -> ()

    let internal point (pt : XmlElement) items outername innername attribute =
      match items with
      | [] -> ()
      | _ ->
          let outer = pt.OwnerDocument.CreateElement(outername)
          outer
          |> pt.AppendChild
          |> ignore
          items
          |> Seq.choose id
          |> Seq.countBy id
          |> Seq.sortBy fst
          |> Seq.iter (fun (t, n) ->
               let inner = pt.OwnerDocument.CreateElement(innername)
               inner
               |> outer.AppendChild
               |> ignore
               inner.SetAttribute(attribute, t.ToString())
               inner.SetAttribute("vc", sprintf "%d" n))

    let internal pointProcess (pt : XmlElement) tracks =
      let (times, calls) =
        tracks
        |> Seq.map (fun t ->
             match t with
             | Base.Time x -> (Some x, None)
             | Base.Both b -> (Some b.Time, Some b.Call)
             | Base.Call y -> (None, Some y)
             | _ -> (None, None))
        |> Seq.toList
        |> List.unzip
      point pt times "Times" "Time" "time"
      point pt calls "TrackedMethodRefs" "TrackedMethodRef" "uid"

    let internal writeReportBase (hits : Dictionary<string, Dictionary<int, Base.PointVisit>>)
        report =
      AltCover.Base.Counter.doFlush (postProcess hits report) pointProcess true hits report
    // mocking points
    [<System.Diagnostics.CodeAnalysis.SuppressMessage(
        "Gendarme.Rules.Performance", "AvoidUncalledPrivateCodeRule",
        Justification = "Unit test accessor")>]
    let mutable internal getPayload = payloadBase
    [<System.Diagnostics.CodeAnalysis.SuppressMessage(
        "Gendarme.Rules.Performance", "AvoidUncalledPrivateCodeRule",
        Justification = "Unit test accessor")>]
    let mutable internal getMonitor = monitorBase
    [<System.Diagnostics.CodeAnalysis.SuppressMessage(
        "Gendarme.Rules.Performance", "AvoidUncalledPrivateCodeRule",
        Justification = "Unit test accessor")>]
    let mutable internal doReport = writeReportBase

    let internal doSummaries (document : XDocument) (format : Base.ReportFormat) result =
      let code =
        I.summaries |> List.fold (fun r summary -> summary document format r) result
      if (code > 0 && code <> result) then
        CommandLine.writeErrorResourceWithFormatItems "threshold"
          [| code :> obj
             (Option.get threshold) :> obj |]
      code

    let internal loadReport report =
      if File.Exists report then XDocument.Load report else XDocument()

  // "Public"
  let internal doCoverage arguments options1 =
    let check1 =
      declareOptions()
      |> CommandLine.parseCommandLine(arguments |> Array.skip 1)
      |> CommandLine.processHelpOption
      |> J.requireExe
      |> J.requireRecorder
      |> J.requireWorker
    match check1 with
    | Left(intro, options) ->
        CommandLine.handleBadArguments false arguments
          { Intro = intro
            Options = options1
            Options2 = options }
        255
    | Right(rest, _) ->
        let value =
          CommandLine.doPathOperation (fun () ->
            let pair = J.recorderInstance()
            use assembly = fst pair
            let instance = snd pair

            let report =
              (J.getMethod instance "get_ReportFile")
              |> J.getFirstOperandAsString
              |> Path.GetFullPath

            let format =
              (J.getMethod instance "get_CoverageFormat") |> J.getFirstOperandAsNumber
            let hits = Dictionary<string, Dictionary<int, Base.PointVisit>>()
            let payload = J.getPayload
            let result = J.getMonitor hits report payload rest
            let format' = enum format
            let delta = J.doReport hits format' report output
            CommandLine.writeResourceWithFormatItems
              "Coverage statistics flushing took {0:N} seconds" [| delta.TotalSeconds |]
              false

            // And tidy up after everything's done
            File.Delete(report + ".acv")
            Directory.GetFiles
              (Path.GetDirectoryName(report), Path.GetFileName(report) + ".*.acv")
            |> Seq.iter File.Delete
            let document = J.loadReport report
            J.doSummaries document format' result) 255 true
        CommandLine.reportErrors "Collection" false
        value

[<assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", Scope="member", Target="AltCover.TeamCityFormat+Tags.#B", MessageId="B", Justification="TeamCity notation")>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", Scope="member", Target="AltCover.TeamCityFormat+Tags.#R", MessageId="R", Justification="TeamCity notation")>]
()