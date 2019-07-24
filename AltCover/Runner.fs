namespace AltCover

open System
open System.Collections.Generic
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

[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage; NoComparison>]
type TeamCityFormat =
  | Default
  | R
  | B
  | RPlus
  | BPlus
  static member Factory s =
    let (|Select|_|) (pattern : String) offered =
      if offered
          |> String.IsNullOrWhiteSpace
          |> not
          && pattern.Equals(String (offered |> Seq.sort |> Seq.toArray),
                            StringComparison.OrdinalIgnoreCase)
      then Some offered
      else None

    match s with
    | Select "B" _ -> B
    | Select "+" _
    | Select "+B" _ -> BPlus
    | Select "R" _-> R
    | Select "+R" _ -> RPlus
    | _ ->  Default

module internal Runner =

  let mutable internal recordingDirectory : Option<string> = None
  let mutable internal workingDirectory : Option<string> = None
  let internal executable : Option<string> ref = ref None
  let internal collect = ref false
  let mutable internal threshold : Option<int> = None
  let mutable internal output : Option<string> = None
  let internal Summary = StringBuilder()
  let mutable internal SummaryFormat = TeamCityFormat.Default

  let init() =
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
    SummaryFormat <- Default
    Summary.Clear() |> ignore

  let X = OpenCover.X

  let Write line =
    [Summary.AppendLine >> ignore; Output.Info]
    |> Seq.iter(fun f -> f line)

  let WriteSummary key vc nc pc =
    let line = String.Format(CultureInfo.CurrentCulture,
                              CommandLine.resources.GetString key, vc, nc, pc)
    Write line

  let TCtotal = "##teamcity[buildStatisticValue key='CodeCoverageAbs{0}Total' value='{1}']"
  let TCcover = "##teamcity[buildStatisticValue key='CodeCoverageAbs{0}Covered' value='{1}']"

  let WriteTC template what value =
    let line = String.Format(CultureInfo.InvariantCulture,
                              template, what, value)
    Write line

  let NCoverSummary(report : XDocument) =
    let makepc v n =
      if n = 0 then "n/a"
      else
        Math.Round((float v) * 100.0 / (float n), 2)
            .ToString(CultureInfo.InvariantCulture)

    let summarise v n key =
      let pc = makepc v n
      WriteSummary key v n pc

    let methods =
      report.Descendants(X "method")
      |> Seq.filter (fun m -> m.Attribute(X "excluded").Value = "false")
      |> Seq.toList

    let classes =
      methods
      |> Seq.groupBy (fun m -> m.Attribute(X "class").Value)
      |> Seq.toList

    let isVisited (x : XElement) =
      let v = x.Attribute(X "visitcount")
      (v
       |> isNull
       |> not)
      && (v.Value <> "0")

    let vclasses =
      classes
      |> Seq.filter
           (fun (_, ms) ->
           ms |> Seq.exists (fun m -> m.Descendants(X "seqpnt") |> Seq.exists isVisited))
      |> Seq.length

    let vmethods =
      methods
      |> Seq.filter (fun m -> m.Descendants(X "seqpnt") |> Seq.exists isVisited)
      |> Seq.length

    let points =
      report.Descendants(X "seqpnt")
      |> Seq.filter (fun m -> m.Attribute(X "excluded").Value = "false")
      |> Seq.toList

    let vpoints =
      points
      |> Seq.filter isVisited
      |> Seq.length

    let emitSummary () =
      if [Default; BPlus; RPlus] |> Seq.exists (fun x -> x = SummaryFormat) then
        summarise vclasses classes.Length "VisitedClasses"
        summarise vmethods methods.Length "VisitedMethods"
        summarise vpoints points.Length "VisitedPoints"

      if [B; R; BPlus; RPlus] |> Seq.exists (fun x -> x = SummaryFormat) then
        WriteTC TCtotal "C" classes.Length
        WriteTC TCcover "C" vclasses
        WriteTC TCtotal "M" methods.Length
        WriteTC TCcover "M" vmethods
        WriteTC TCtotal "S" points.Length
        WriteTC TCcover "S" vpoints
    emitSummary()

    makepc vpoints points.Length

  let AltSummary(report : XDocument) =
    "Alternative"
    |> CommandLine.resources.GetString
    |> Write

    let classes =
      report.Descendants(X "Class")
      |> Seq.filter (fun c -> c.Attribute(X "skippedDueTo") |> isNull)
      |> Seq.filter (fun c ->
           c.Descendants(X "Method")
           |> Seq.isEmpty
           |> not)
      |> Seq.toList

    let vclasses =
      classes
      |> Seq.filter
           (fun c ->
           c.Descendants(X "Method")
           |> Seq.exists (fun m -> m.Attribute(X "visited").Value = "true"))
      |> Seq.length

    let nc = classes.Length

    let pc =
      if nc = 0 then "n/a"
      else
        Math.Round((float vclasses) * 100.0 / (float nc), 2)
            .ToString(CultureInfo.InvariantCulture)
    WriteSummary "AltVC" vclasses nc pc

    let methods =
      classes
      |> Seq.collect (fun c -> c.Descendants(X "Method"))
      |> Seq.filter (fun c -> c.Attribute(X "skippedDueTo") |> isNull)
      |> Seq.toList

    let vm =
      methods
      |> Seq.filter (fun m -> m.Attribute(X "visited").Value = "true")
      |> Seq.length

    let nm = methods.Length

    let pm =
      if nm = 0 then "n/a"
      else
        Math.Round((float vm) * 100.0 / (float nm), 2)
            .ToString(CultureInfo.InvariantCulture)
    WriteSummary "AltVM" vm nm pm

  let OpenCoverSummary(report : XDocument) =
    let summary = report.Descendants(X "Summary") |> Seq.head

    let summarise go visit number precalc key =
      let vc = summary.Attribute(X visit).Value
      let nc = summary.Attribute(X number).Value

      let pc =
        match precalc with
        | None ->
          if nc = "0" then "n/a"
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
        | Some x -> summary.Attribute(X x).Value
      if go then WriteSummary key vc nc pc
      (vc, nc, pc)

    let go = [Default; BPlus; RPlus] |> Seq.exists (fun x -> x = SummaryFormat)
    let (vc, nc, _) = summarise go "visitedClasses" "numClasses" None "VisitedClasses"
    let (vm, nm, _) = summarise go "visitedMethods" "numMethods" None "VisitedMethods"
    let (vs, ns, covered) =
      summarise go "visitedSequencePoints" "numSequencePoints" (Some "sequenceCoverage")
          "VisitedPoints"
    let (vb, nb, _) = summarise go "visitedBranchPoints" "numBranchPoints" (Some "branchCoverage")
                        "VisitedBranches"
    if go then
      Write String.Empty
      AltSummary report

    if [B; R; BPlus; RPlus] |> Seq.exists (fun x -> x = SummaryFormat) then
      WriteTC TCtotal "C" nc
      WriteTC TCcover "C" vc
      WriteTC TCtotal "M" nm
      WriteTC TCcover "M" vm
      WriteTC TCtotal "S" ns
      WriteTC TCcover "S" vs
      let tag = match SummaryFormat with
                | R
                | RPlus -> "R"
                | _ -> "B"
      WriteTC TCtotal tag nb
      WriteTC TCcover tag vb

    covered

  let InvariantParseDouble d = Double.TryParse(d, NumberStyles.Number, CultureInfo.InvariantCulture)

  let StandardSummary (report : XDocument) (format : Base.ReportFormat) result =
    let covered =
      report
      |> match format with
         | Base.ReportFormat.NCover -> NCoverSummary
         | _ -> OpenCoverSummary
      |> InvariantParseDouble

    let value =
      match covered with
      | (false, _) -> 0.0
      | (_, x) -> x

    match threshold with
    | None -> result
    | Some x ->
      let f = float x
      if f <= value then result
      else Math.Ceiling(f - value) |> int

  let mutable internal Summaries : (XDocument -> Base.ReportFormat -> int -> int) list =
    []

  let internal ValidateThreshold x =
    let (q, n) =
      Int32.TryParse(if (String.IsNullOrWhiteSpace(x)) then "!"
                     else x)

    let ok = q && (n >= 0) && (n <= 100)
    if ok |> not then
      CommandLine.error <- String.Format
                             (CultureInfo.CurrentCulture,
                              CommandLine.resources.GetString "InvalidValue",
                              "--threshold", x) :: CommandLine.error
    (ok, n)

  let internal DeclareOptions() =
    Summaries <- []
    Summaries <- StandardSummary :: Summaries
    [ ("r|recorderDirectory=",
       (fun x ->
       if CommandLine.ValidateDirectory "--recorderDirectory" x then
         if Option.isSome recordingDirectory then
           CommandLine.error <- String.Format
                                  (CultureInfo.CurrentCulture,
                                   CommandLine.resources.GetString "MultiplesNotAllowed",
                                   "--recorderDirectory") :: CommandLine.error
         else recordingDirectory <- Some(Path.GetFullPath x)))
      ("w|workingDirectory=",
       (fun x ->
       if CommandLine.ValidateDirectory "--workingDirectory" x then
         if Option.isSome workingDirectory then
           CommandLine.error <- String.Format
                                  (CultureInfo.CurrentCulture,
                                   CommandLine.resources.GetString "MultiplesNotAllowed",
                                   "--workingDirectory") :: CommandLine.error
         else workingDirectory <- Some(Path.GetFullPath x)))
      ("x|executable=",
       (fun x ->
       if CommandLine.ValidatePath "--executable" x then
         if Option.isSome !executable then
           CommandLine.error <- String.Format
                                  (CultureInfo.CurrentCulture,
                                   CommandLine.resources.GetString "MultiplesNotAllowed",
                                   "--executable") :: CommandLine.error
         else executable := Some x))
      (CommandLine.ddFlag "collect" collect)
      ("l|lcovReport=",
       (fun x ->
       if CommandLine.ValidatePath "--lcovReport" x then
         if Option.isSome !LCov.path then
           CommandLine.error <- String.Format
                                  (CultureInfo.CurrentCulture,
                                   CommandLine.resources.GetString "MultiplesNotAllowed",
                                   "--lcovReport") :: CommandLine.error
         else
           LCov.path := x
                        |> Path.GetFullPath
                        |> Some
           Summaries <- LCov.Summary :: Summaries))
      ("t|threshold=",
       (fun x ->
       let ok, n = ValidateThreshold x
       if ok then
         if Option.isSome threshold then
           CommandLine.error <- String.Format
                                  (CultureInfo.CurrentCulture,
                                   CommandLine.resources.GetString "MultiplesNotAllowed",
                                   "--threshold") :: CommandLine.error
         else threshold <- Some n))
      ("c|cobertura=",
       (fun x ->
       if CommandLine.ValidatePath "--cobertura" x then
         if Option.isSome !Cobertura.path then
           CommandLine.error <- String.Format
                                  (CultureInfo.CurrentCulture,
                                   CommandLine.resources.GetString "MultiplesNotAllowed",
                                   "--cobertura") :: CommandLine.error
         else
           Cobertura.path := x
                             |> Path.GetFullPath
                             |> Some
           Summaries <- Cobertura.Summary :: Summaries))
      ("o|outputFile=",
       (fun x ->
       if CommandLine.ValidatePath "--outputFile" x then
         if Option.isSome output then
           CommandLine.error <- String.Format
                                  (CultureInfo.CurrentCulture,
                                   CommandLine.resources.GetString "MultiplesNotAllowed",
                                   "--outputFile") :: CommandLine.error
         else
           output <- x
                     |> Path.GetFullPath
                     |> Some))
      (CommandLine.ddFlag "dropReturnCode" CommandLine.dropReturnCode)
      ("teamcity:",
        fun x -> if SummaryFormat = Default then
                    SummaryFormat <- if String.IsNullOrWhiteSpace x
                                     then B
                                     else TeamCityFormat.Factory x
                    if SummaryFormat = Default then
                       CommandLine.error <- String.Format
                                              (CultureInfo.CurrentCulture,
                                                CommandLine.resources.GetString "InvalidValue",
                                                "--teamcity", x) :: CommandLine.error
                 else
                   CommandLine.error <- String.Format
                                      (CultureInfo.CurrentCulture,
                                       CommandLine.resources.GetString "MultiplesNotAllowed",
                                       "--teamcity") :: CommandLine.error)
      ("?|help|h", (fun x -> CommandLine.help <- not (isNull x)))

      ("<>",
       (fun x ->
       CommandLine.error <- String.Format
                              (CultureInfo.CurrentCulture,
                               CommandLine.resources.GetString "InvalidValue", "AltCover",
                               x) :: CommandLine.error)) ] // default end stop
    |> List.fold
         (fun (o : OptionSet) (p, a) ->
         o.Add(p, CommandLine.resources.GetString(p), new System.Action<string>(a)))
         (OptionSet())

  let internal RequireExe(parse : Either<string * OptionSet, string list * OptionSet>) =
    match parse with
    | Right(l, options) ->
      match (!executable, !collect) with
      | (None, false) | (Some _, true) ->
        CommandLine.error <- (CommandLine.resources.GetString "executableRequired")
                             :: CommandLine.error
        Left("UsageError", options)
      | (None, _) -> Right([], options)
      | (Some exe, _) -> Right(exe :: l, options)
    | fail -> fail

  let internal RequireRecorderTest recordingDirectory success fail =
    match recordingDirectory with
    | None ->
      CommandLine.error <- (CommandLine.resources.GetString "recorderRequired")
                           :: CommandLine.error
      fail
    | Some path ->
      let dll = Path.Combine(path, "AltCover.Recorder.g.dll")
      if File.Exists dll then success
      else
        CommandLine.error <- String.Format
                               (CultureInfo.CurrentCulture,
                                CommandLine.resources.GetString "recorderNotFound", dll)
                             :: CommandLine.error
        fail

  let internal RequireRecorder(parse : Either<string * OptionSet, string list * OptionSet>) =
    match parse with
    | Right(_, options) ->
      RequireRecorderTest recordingDirectory parse (Left("UsageError", options))
    | fail -> fail

  let internal RequireWorker(parse : Either<string * OptionSet, string list * OptionSet>) =
    match parse with
    | Right _ ->
      match workingDirectory with
      | None -> workingDirectory <- Directory.GetCurrentDirectory() |> Some
      | _ -> ()
      parse
    | fail -> fail

  // mocking point
  let mutable internal RecorderName = "AltCover.Recorder.g.dll"

  let RecorderInstance() =
    let recorderPath = Path.Combine(Option.get recordingDirectory, RecorderName)
    let definition = AssemblyDefinition.ReadAssembly recorderPath
    (definition, definition.MainModule.GetType("AltCover.Recorder.Instance"))

  let GetMethod (t : TypeDefinition) (name : string) =
    t.Methods
    |> Seq.filter (fun m -> m.Name = name)
    |> Seq.head

  let GetFirstOperandAsString(m : MethodDefinition) =
    m.Body.Instructions
    |> Seq.filter (fun i -> i.OpCode = Cil.OpCodes.Ldstr)
    |> Seq.map (fun i -> i.Operand :?> string)
    |> Seq.head

  let GetFirstOperandAsNumber(m : MethodDefinition) =
    m.Body.Instructions
    |> Seq.filter (fun i -> i.OpCode = Cil.OpCodes.Ldc_I4)
    |> Seq.map (fun i -> i.Operand :?> int)
    |> Seq.head

  let PayloadBase(rest : string list) =
    CommandLine.doPathOperation
      (fun () ->
      CommandLine.ProcessTrailingArguments rest
        (DirectoryInfo(Option.get workingDirectory))) 255 true
  let WriteResource = CommandLine.resources.GetString >> Output.Info
  let WriteResourceWithFormatItems s x warn =
    String.Format(CultureInfo.CurrentCulture, s |> CommandLine.resources.GetString, x)
    |> (Output.WarnOn warn)
  let WriteErrorResourceWithFormatItems s x =
    String.Format(CultureInfo.CurrentCulture, s |> CommandLine.resources.GetString, x)
    |> Output.Error

  let internal SetRecordToFile report =
    LCov.DoWith (fun () ->
      let binpath = report + ".acv"
      File.Create(binpath)) ignore

  let internal RunProcess report (payload : string list -> int) (args : string list) =
    SetRecordToFile report
    "Beginning run..." |> WriteResource
    let result = payload args
    "Getting results..." |> WriteResource
    result

  let internal CollectResults (hits : Dictionary<string, Dictionary<int, Base.PointVisit>>)
      report =
    let timer = System.Diagnostics.Stopwatch()
    timer.Start()
    let visits =
      Directory.GetFiles
        (Path.GetDirectoryName(report), Path.GetFileName(report) + ".*.acv")
      |> Seq.fold (fun before f ->
           timer.Restart()
           let length = FileInfo(f).Length.ToString("#,#", CultureInfo.CurrentUICulture)
           sprintf "... %s (%sb)" f length |> Output.Info
           use results = new DeflateStream(File.OpenRead f, CompressionMode.Decompress)
           use formatter = new System.IO.BinaryReader(results)

           let rec sink hitcount =
             let hit =
               try
                 let id = formatter.ReadString()
                 let strike = formatter.ReadInt32()
                 let tag = formatter.ReadByte() |> int
                 Some(id, strike,
                      match enum tag with
                      | Base.Tag.Time -> Base.Time <| formatter.ReadInt64()
                      | Base.Tag.Call -> Base.Call <| formatter.ReadInt32()
                      | Base.Tag.Both ->
                        Base.Both(formatter.ReadInt64(), formatter.ReadInt32())
                      | Base.Tag.Table ->
                          let t = Dictionary<string, Dictionary<int, PointVisit>>()
                          let rec ``module`` () =
                            let m = formatter.ReadString()
                            if String.IsNullOrEmpty m
                            then ()
                            else
                              if m |> t.ContainsKey |> not
                              then t.Add(m, Dictionary<int, PointVisit>())
                              let points = formatter.ReadInt32()
                              let rec sequencePoint pts =
                                if pts > 0 then
                                  let p = formatter.ReadInt32()
                                  let n = formatter.ReadInt64()
                                  if p |> t.[m].ContainsKey |> not
                                  then t.[m].Add(p, PointVisit.Create())
                                  let pv = t.[m].[p]
                                  pv.Count <- pv.Count + n
                                  let rec tracking () =
                                    let track = formatter.ReadByte() |> int
                                    match enum track with
                                    | Tag.Time -> pv.Tracks.Add (Time <| formatter.ReadInt64())
                                                  tracking ()
                                    | Tag.Call -> pv.Tracks.Add (Call <| formatter.ReadInt32())
                                                  tracking ()
                                    | Tag.Both -> pv.Tracks.Add (Both (formatter.ReadInt64(), formatter.ReadInt32()))
                                                  tracking ()
// Expect never to happen                                    | Tag.Table -> ``module``()
                                    | _ -> sequencePoint (pts - 1)
                                  tracking()
                                else ``module``()
                              sequencePoint points
                          ``module`` ()
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
                    || (key = String.Empty &&
                        hitPointId = 0 &&
                        visit.GetType().ToString() = "AltCover.Base.Track+Table")
                 then
                   Base.Counter.AddVisit hits key hitPointId visit
                 else 0L
               sink (hitcount + increment)
             | None -> hitcount

           let after = sink before
           timer.Stop()
           if after > before then
             let delta = after - before
             let interval = timer.Elapsed
             let rate = (float delta) / interval.TotalSeconds
             WriteResourceWithFormatItems "%d visits recorded in %A (%A visits/sec)"
               [| delta :> obj
                  interval
                  rate |] false
           after) 0L
    timer.Stop()
    WriteResourceWithFormatItems "%d visits recorded" [| visits |] (visits = 0L)

  let internal MonitorBase (hits : Dictionary<string, Dictionary<int, Base.PointVisit>>)
      report (payload : string list -> int) (args : string list) =
    let result =
      if !collect then 0
      else RunProcess report payload args
    CollectResults hits report
    result

  let internal CopyFillMethodPoint (mp : XmlElement seq) sp =
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

  let internal LookUpVisitsByToken token (dict : Dictionary<int, Base.PointVisit>) =
    let (ok, index) =
      Int32.TryParse
        (token, System.Globalization.NumberStyles.Integer,
         System.Globalization.CultureInfo.InvariantCulture)
    match dict.TryGetValue(if ok then index
                           else -1) with
    | (false, _) -> PointVisit.Create()
    | (_, pair) -> pair

  let internal FillMethodPoint (mp : XmlElement seq) (method : XmlElement)
      (dict : Dictionary<int, Base.PointVisit>) =
    let token =
      method.GetElementsByTagName("MetadataToken")
      |> Seq.cast<XmlElement>
      |> Seq.map (fun m -> m.InnerText)
      |> Seq.head

    let vc = (LookUpVisitsByToken token dict).Total ()
    mp
    |> Seq.iter (fun m ->
         m.SetAttribute
           ("vc", vc.ToString(CultureInfo.InvariantCulture))
         m.SetAttribute("uspid", token)
         m.SetAttribute("ordinal", "0")
         m.SetAttribute("offset", "0"))

  let VisitCount nodes =
    nodes
    |> Seq.cast<XmlElement>
    |> Seq.filter (fun s -> Int64.TryParse
                              (s.GetAttribute("vc"),
                               NumberStyles.Integer,
                               CultureInfo.InvariantCulture)
                            |> snd
                            <> 0L)
    |> Seq.length

  let internal TryGetValue (d : Dictionary<'a, 'b>) (key : 'a) =
    match d with
    | null -> (false, Unchecked.defaultof<'b>)
    | _ -> d.TryGetValue key

  let internal PostProcess (counts : Dictionary<string, Dictionary<int, Base.PointVisit>>)
      format (document : XmlDocument) =
    match format with
    | Base.ReportFormat.OpenCoverWithTracking | Base.ReportFormat.OpenCover ->
      let scoreToString raw = (sprintf "%.2f" raw).TrimEnd([| '0' |]).TrimEnd([| '.' |])
      let stringToScore (node:XmlElement) name =
            node.GetAttribute(name)
            |> InvariantParseDouble
            |> snd

      let percentCover visits points =
        if points = 0 then "0"
        else ((float (visits * 100)) / (float points)) |> scoreToString

      let setSummary (x : XmlElement) pointVisits branchVisits methodVisits classVisits
          ptcover brcover minCrap maxCrap =
        x.GetElementsByTagName("Summary")
        |> Seq.cast<XmlElement>
        |> Seq.tryHead
        |> Option.iter (fun s ->
             let minc =
               (if minCrap = Double.MaxValue then 0.0
                else minCrap)
               |> scoreToString

             let maxc =
               (if maxCrap = Double.MinValue then 0.0
                else maxCrap)
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
        tail.SetAttribute("offset", Int32.MaxValue.ToString(CultureInfo.InvariantCulture))
        let nodes =
          List.concat [ sp
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
               (bev + (if x.GetAttribute("vc") = "0" then 0
                       else 1), sq)) (0, nodes.[0])
        |> ignore

      let crapScore (method : XmlElement) =
        let coverage =
          let cover = stringToScore method "sequenceCoverage"
          if cover > 0.0 then cover
          else stringToScore method "branchCoverage"

        let complexity = stringToScore method "cyclomaticComplexity"

        let raw =
          (Math.Pow(complexity, 2.0) * Math.Pow((1.0 - (coverage / 100.0)), 3.0)
           + complexity)
        let score = raw |> scoreToString
        method.SetAttribute("crapScore", score)
        raw

      let updateMethod (dict : Dictionary<int, Base.PointVisit>)
          (vb, vs, vm, pt, br, minc, maxc) (method : XmlElement) =
        let sp = method.GetElementsByTagName("SequencePoint")
        let bp = method.GetElementsByTagName("BranchPoint")
        let mp = method.GetElementsByTagName("MethodPoint") |> Seq.cast<XmlElement>
        let count = sp.Count
        let rawCount = bp.Count

        // inconsistent name to shut Gendarme up
        let numBranches = rawCount + Math.Sign(count + rawCount)
        if count > 0 then CopyFillMethodPoint mp sp
        else FillMethodPoint mp method dict
        let pointVisits = VisitCount sp
        let b0 = VisitCount bp
        let branchVisits = b0 + Math.Sign b0
        if pointVisits > 0 || b0 > 0 then
          let FillMethod() =
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
          FillMethod()
        else (vb, vs, vm, pt + count, br + numBranches, minc, maxc)

      let updateClass (dict : Dictionary<int, Base.PointVisit>)
          (vb, vs, vm, vc, pt, br, minc0, maxc0) (``class`` : XmlElement) =
        let (cvb, cvs, cvm, cpt, cbr, minc, maxc) =
          ``class``.GetElementsByTagName("Method")
          |> Seq.cast<XmlElement>
          |> Seq.fold (updateMethod dict)
               (0, 0, 0, 0, 0, Double.MaxValue, Double.MinValue)

        let cover = percentCover cvs cpt
        let bcover = percentCover cvb cbr

        let cvc =
          if cvm > 0 then 1
          else 0
        setSummary ``class`` cvs cvb cvm (Some cvc) cover bcover minc maxc
        (vb + cvb, vs + cvs, vm + cvm, vc + cvc, pt + cpt, br + cbr, Math.Min(minc, minc0),
         Math.Max(maxc, maxc0))

      let updateModule (counts : Dictionary<string, Dictionary<int, Base.PointVisit>>)
          (vb, vs, vm, vc, pt, br, minc0, maxc0) (``module`` : XmlElement) =
        let dict =
          match (TryGetValue counts) <| ``module``.GetAttribute("hash") with
          | (false, _) -> Dictionary<int, Base.PointVisit>()
          | (true, d) -> d

        let (cvb, cvs, cvm, cvc, cpt, cbr, minc, maxc) =
          ``module``.GetElementsByTagName("Class")
          |> Seq.cast<XmlElement>
          |> Seq.fold (dict |> updateClass)
               (0, 0, 0, 0, 0, 0, Double.MaxValue, Double.MinValue)

        let cover = percentCover cvs cpt
        let bcover = percentCover cvb cbr
        setSummary ``module`` cvs cvb cvm (Some cvc) cover bcover minc maxc
        (vb + cvb, vs + cvs, vm + cvm, vc + cvc, pt + cpt, br + cbr, Math.Min(minc, minc0),
         Math.Max(maxc, maxc0))

      let (vb, vs, vm, vc, pt, br, minc, maxc) =
        document.DocumentElement.SelectNodes("//Module")
        |> Seq.cast<XmlElement>
        |> Seq.fold (updateModule counts)
             (0, 0, 0, 0, 0, 0, Double.MaxValue, Double.MinValue)

      let cover = percentCover vs pt
      let bcover = percentCover vb br
      setSummary document.DocumentElement vs vb vm (Some vc) cover bcover minc maxc
    | _ -> ()

  let internal Point (pt : XmlElement) items outername innername attribute =
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

  let internal PointProcess (pt : XmlElement) tracks =
    let (times, calls) =
      tracks
      |> Seq.map (fun t ->
           match t with
           | Base.Time x -> (Some x, None)
           | Base.Both(x, y) -> (Some x, Some y)
           | Base.Call y -> (None, Some y)
           | _ -> (None, None))
      |> Seq.toList
      |> List.unzip
    Point pt times "Times" "Time" "time"
    Point pt calls "TrackedMethodRefs" "TrackedMethodRef" "uid"

  let internal WriteReportBase (hits : Dictionary<string, Dictionary<int, Base.PointVisit>>)
      report =
    AltCover.Base.Counter.DoFlush (PostProcess hits report) PointProcess true hits report
  // mocking points
  let mutable internal GetPayload = PayloadBase
  let mutable internal GetMonitor = MonitorBase
  let mutable internal DoReport = WriteReportBase

  let DoSummaries (document : XDocument) (format : Base.ReportFormat) result =
    let code = Summaries |> List.fold (fun r summary -> summary document format r) result
    if (code > 0 && code <> result) then
      WriteErrorResourceWithFormatItems "threshold" [| code :> obj
                                                       (Option.get threshold) :> obj |]
    code

  let LoadReport report =
    if File.Exists report then XDocument.Load report
    else XDocument()

  let DoCoverage arguments options1 =
    let check1 =
      DeclareOptions()
      |> CommandLine.ParseCommandLine(arguments |> Array.skip 1)
      |> CommandLine.ProcessHelpOption
      |> RequireExe
      |> RequireRecorder
      |> RequireWorker
    match check1 with
    | Left(intro, options) ->
      CommandLine.HandleBadArguments false arguments intro options1 options
      255
    | Right(rest, _) ->
      let value =
        CommandLine.doPathOperation (fun () ->
          let pair = RecorderInstance()
          use assembly = fst pair
          let instance = snd pair

          let report =
            (GetMethod instance "get_ReportFile")
            |> GetFirstOperandAsString
            |> Path.GetFullPath

          let format =
            (GetMethod instance "get_CoverageFormat") |> GetFirstOperandAsNumber
          let hits = Dictionary<string, Dictionary<int, Base.PointVisit>>()
          let payload = GetPayload
          let result = GetMonitor hits report payload rest
          let format' = enum format
          let delta = DoReport hits format' report output
          WriteResourceWithFormatItems "Coverage statistics flushing took {0:N} seconds"
            [| delta.TotalSeconds |] false

          // And tidy up after everything's done
          File.Delete(report + ".acv")
          Directory.GetFiles
            (Path.GetDirectoryName(report), Path.GetFileName(report) + ".*.acv")
          |> Seq.iter File.Delete
          let document = LoadReport report
          DoSummaries document format' result) 255 true
      CommandLine.ReportErrors "Collection" false
      value