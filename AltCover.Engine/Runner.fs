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

[<ExcludeFromCodeCoverage; NoComparison>]
type internal SummaryFormat =
  | Default
  | [<SuppressMessage("Microsoft.Naming",
                      "CA1704:IdentifiersShouldBeSpelledCorrectly",
                      Justification = "Consistent notation")>] N
  | [<SuppressMessage("Microsoft.Naming",
                      "CA1704:IdentifiersShouldBeSpelledCorrectly",
                      Justification = "Consistent notation")>] O
  | [<SuppressMessage("Microsoft.Naming",
                      "CA1704:IdentifiersShouldBeSpelledCorrectly",
                      Justification = "Consistent notation")>] C
  | [<SuppressMessage("Microsoft.Naming",
                      "CA1704:IdentifiersShouldBeSpelledCorrectly",
                      Justification = "TeamCity notation")>] R
  | [<SuppressMessage("Microsoft.Naming",
                      "CA1704:IdentifiersShouldBeSpelledCorrectly",
                      Justification = "TeamCity notation")>] B
  | Many of SummaryFormat list
  static member ToList x =
    match x with
    | Many l -> l
    | f -> [ f ]

  static member Factory(s: string) =
    let expanded =
      match s with
      | x when String.IsNullOrWhiteSpace x -> "B"
      | "+" -> "BOC"
      | _ -> s.Replace("+", "OC").ToUpperInvariant()

    try
      expanded
      |> Seq.distinct
      |> Seq.fold
           (fun state c ->
             match (c, state) with
             | ('N', _)
             | (_, N) -> N
             | ('B', _) -> Many(B :: (SummaryFormat.ToList state))
             | ('R', _) -> Many(R :: (SummaryFormat.ToList state))
             | ('O', _) -> Many(O :: (SummaryFormat.ToList state))
             | ('C', _) -> Many(C :: (SummaryFormat.ToList state))
             | _ -> s |> FormatException |> raise)
           (Many [])
    with :? FormatException -> Default

type internal Threshold =
  { Statements: uint8
    Branches: uint8
    Methods: uint8
    Crap: uint8
    AltMethods: uint8
    AltCrap: uint8 }
  static member Default() =
    { Statements = 0uy
      Branches = 0uy
      Methods = 0uy
      Crap = 0uy
      AltMethods = 0uy
      AltCrap = 0uy }

  static member Create(x: string) =
    let chars = x.ToUpperInvariant() |> Seq.toList

    let rec partition data result =
      match data with
      | [] -> result
      | _ ->
          let h =
            data
            |> List.takeWhile (Char.IsDigit >> not)
            |> List.toArray

          let t =
            data |> List.skipWhile (Char.IsDigit >> not)

          let h2 =
            t |> List.takeWhile Char.IsDigit |> List.toArray

          let t2 = t |> List.skipWhile Char.IsDigit
          partition t2 ((String(h), String(h2)) :: result)

    let parse top f t x =
      let part, v =
        Byte.TryParse(
          if (String.IsNullOrWhiteSpace(x)) then
            "!"
          else
            x
        )

      if part && v <= top then
        (part, f t v)
      else
        (false, t)

    let parts =
      partition chars []
      |> List.fold
           (fun (ok, t) (h, h2) ->
             let fail t _ = (false, t)

             let defaultMapper =
               parse 100uy (fun t v -> { t with Statements = v })

             let mapper =
               match (ok, h) with // can't say String.Empty
               | (true, x) when x.Length = 0 -> defaultMapper
               | (true, "S") -> defaultMapper
               | (true, "B") -> parse 100uy (fun t v -> { t with Branches = v })
               | (true, "M") -> parse 100uy (fun t v -> { t with Methods = v })
               | (true, "C") -> parse 255uy (fun t v -> { t with Crap = v })
               | (true, "AM") -> parse 100uy (fun t v -> { t with AltMethods = v })
               | (true, "AC") -> parse 255uy (fun t v -> { t with AltCrap = v })
               | _ -> fail

             mapper t h2)
           (true, Threshold.Default())

    parts
    |> (fun (a, b) -> (a, (if a then Some b else None)))

  static member Validate(x: string) =
    let result = Threshold.Create x

    if not (fst result) then
      CommandLine.error <-
        CommandLine.Format.Local("InvalidValue", "--threshold", x)
        :: CommandLine.error

    result

module internal Runner =

  let mutable internal recordingDirectory : Option<string> = None
  let mutable internal workingDirectory : Option<string> = None
  let internal executable : Option<string> ref = ref None
  let internal collect = ref false // ddFlag
  let mutable internal threshold : Threshold option = None
  let mutable internal output : Option<string> = None
  let internal summary = StringBuilder()
  let mutable internal summaryFormat = SummaryFormat.Default

  let internal init () =
    CommandLine.verbosity <- 0
    CommandLine.error <- []
    CommandLine.dropReturnCode := false
    recordingDirectory <- None
    workingDirectory <- None
    executable := None
    LCov.path := None
    Cobertura.path := None
    Json.path := None
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
      let line =
        String.Format(CultureInfo.InvariantCulture, template, what, value)

      write line

    [<SuppressMessage("Gendarme.Rules.Exceptions",
                      "InstantiateArgumentExceptionCorrectlyRule",
                      Justification = "Library method inlined")>]
    [<SuppressMessage("Microsoft.Usage",
                      "CA2208:InstantiateArgumentExceptionsCorrectly",
                      Justification = "Library method inlined")>]
    let internal contains o l = l |> Seq.contains o

    [<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Maintainability",
                                                      "AvoidUnnecessarySpecializationRule",
                                                      Justification = "AvoidSpeculativeGenerality too")>]
    let internal nCoverSummary (report: XDocument) =
      let makepc v n =
        if n = 0 then
          "n/a"
        else
          Math
            .Round((float v) * 100.0 / (float n), 2)
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

      let isVisited (x: XElement) =
        let v = x.Attribute("visitcount".X)
        (v |> isNull |> not) && (v.Value <> "0")

      let vclasses =
        classes
        |> Seq.filter
             (fun (_, ms) ->
               ms
               |> Seq.exists (fun m -> m.Descendants("seqpnt".X) |> Seq.exists isVisited))
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
        points |> Seq.filter isVisited |> Seq.length

      let l = SummaryFormat.ToList summaryFormat

      let emitSummary () =
        if summaryFormat = Default || l |> contains O then
          summarise vclasses classes.Length "VisitedClasses"
          summarise vmethods methods.Length "VisitedMethods"
          summarise vpoints points.Length "VisitedPoints"

        if l |> contains B || l |> contains R then
          writeTC totalTC "C" classes.Length
          writeTC coverTC "C" vclasses
          writeTC totalTC "M" methods.Length
          writeTC coverTC "M" vmethods
          writeTC totalTC "S" points.Length
          writeTC coverTC "S" vpoints

      emitSummary ()

      [ (makepc vpoints points.Length)
        "0" // branches
        (makepc vmethods methods.Length)
        (makepc vmethods methods.Length)
        "0" // crap
        "0" ] // altcrap

    [<SuppressMessage("Gendarme.Rules.Exceptions",
                      "InstantiateArgumentExceptionCorrectlyRule",
                      Justification = "In inlined library code")>]
    [<SuppressMessage("Gendarme.Rules.Performance",
                      "AvoidRepetitiveCallsToPropertiesRule",
                      Justification = "In inlined library code")>]
    [<SuppressMessage("Microsoft.Usage",
                      "CA2208:InstantiateArgumentExceptionsCorrectly",
                      Justification = "In inlined library code")>]
    let internal emitAltCrapScore go (methods: XElement seq) =
      let value =
        (methods
         |> Seq.map (fun m -> m.Attribute("crapScore".X))
         |> Seq.filter (fun a -> a.IsNotNull)
         |> Seq.map (fun d -> d.Value.InvariantParseDouble() |> snd)
         |> Seq.max)

      if go then
        CommandLine.Format.Local("altMaxCrap", value)
        |> write

      value.ToString(CultureInfo.InvariantCulture)

    [<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Maintainability",
                                                      "AvoidUnnecessarySpecializationRule",
                                                      Justification = "AvoidSpeculativeGenerality too")>]
    let internal altSummary go extra (report: XDocument) =
      "Alternative"
      |> CommandLine.resources.GetString
      |> if go || extra then write else ignore

      let classes =
        report.Descendants("Class".X)
        |> Seq.filter (fun c -> c.Attribute("skippedDueTo".X) |> isNull)
        |> Seq.filter (fun c -> c.Descendants("Method".X) |> Seq.isEmpty |> not)
        |> Seq.toList

      let vclasses =
        classes
        |> Seq.filter
             (fun c ->
               c.Descendants("Method".X)
               |> Seq.exists (fun m -> m.Attribute("visited".X).Value = "true"))
        |> Seq.length

      let nc = classes.Length

      let pc =
        if nc = 0 then
          "n/a"
        else
          Math
            .Round((float vclasses) * 100.0 / (float nc), 2)
            .ToString(CultureInfo.InvariantCulture)

      if go then
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

      let amv =
        if nm = 0 then
          "0.0"
        else
          Math
            .Round((float vm) * 100.0 / (float nm), 2)
            .ToString(CultureInfo.InvariantCulture)

      let pm = if nm = 0 then "n/a" else amv
      if go then writeSummary "AltVM" vm nm pm

      let acv =
        if nm > 0 then
          emitAltCrapScore extra methods
        else
          "0.0"

      (amv, acv)

    [<SuppressMessage("Gendarme.Rules.Exceptions",
                      "InstantiateArgumentExceptionCorrectlyRule",
                      Justification = "Library method inlined")>]
    [<SuppressMessage("Microsoft.Usage",
                      "CA2208:InstantiateArgumentExceptionsCorrectly",
                      Justification = "Library method inlined")>]
    [<SuppressMessage("Gendarme.Rules.Maintainability",
                      "AvoidComplexMethodsRule",
                      Justification = "TODO: refactor even more")>]
    let private makeOpenCoverSummary (report: XDocument) (summary: XElement) =

      let summarise go (visit: string) (number: string) (precalc: string option) key =
        let vc = summary.Attribute(visit.X).Value
        let nc = summary.Attribute(number.X).Value

        let pc =
          match precalc with
          | None ->
              if nc = "0" then
                "n/a"
              else
                let vc1 = vc |> Int32.TryParse |> snd |> float

                let nc1 = nc |> Int32.TryParse |> snd |> float

                Math
                  .Round(vc1 * 100.0 / nc1, 2)
                  .ToString(CultureInfo.InvariantCulture)
          | Some x -> summary.Attribute(x.X).Value

        if go then writeSummary key vc nc pc
        (vc, nc, pc)

      let l =
        (SummaryFormat.ToList summaryFormat)
        |> Seq.distinct

      let go =
        summaryFormat = Default || l |> Seq.contains O

      let (vc, nc, _) =
        summarise go "visitedClasses" "numClasses" None "VisitedClasses"

      let (vm, nm, mcovered) =
        summarise go "visitedMethods" "numMethods" None "VisitedMethods"

      let (vs, ns, covered) =
        summarise
          go
          "visitedSequencePoints"
          "numSequencePoints"
          (Some "sequenceCoverage")
          "VisitedPoints"

      let (vb, nb, bcovered) =
        summarise
          go
          "visitedBranchPoints"
          "numBranchPoints"
          (Some "branchCoverage")
          "VisitedBranches"

      let crap = summary.Attribute("maxCrapScore".X)

      let crapvalue =
        crap
        |> Option.ofObj
        |> Option.map (fun a -> a.Value)
        |> Option.defaultValue "0.0"

      let extra =
        summaryFormat = Default || l |> Seq.contains C

      if extra then
        if crap.IsNotNull then
          CommandLine.Format.Local("maxCrap", crapvalue)
          |> write

      if go || extra then write String.Empty

      let (altmcovered, altcrapvalue) = altSummary go extra report

      if l |> Seq.contains B || l |> Seq.contains R then
        writeTC totalTC "C" nc
        writeTC coverTC "C" vc
        writeTC totalTC "M" nm
        writeTC coverTC "M" vm
        writeTC totalTC "S" ns
        writeTC coverTC "S" vs

        l
        |> Seq.iter
             (fun f ->
               let tag =
                 match f with
                 | R -> "R"
                 | B -> "B"
                 | _ -> String.Empty

               if tag |> String.IsNullOrEmpty |> not then
                 writeTC totalTC tag nb
                 writeTC coverTC tag vb)

      [ covered
        bcovered
        mcovered
        altmcovered
        crapvalue
        altcrapvalue ]

    let internal openCoverSummary (report: XDocument) =
      report.Root.Elements("Summary".X)
      |> Seq.tryHead
      |> Option.map (makeOpenCoverSummary report)
      |> Option.defaultValue []

    [<SuppressMessage("Gendarme.Rules.Exceptions",
                      "InstantiateArgumentExceptionCorrectlyRule",
                      Justification = "Library method inlined")>]
    [<SuppressMessage("Microsoft.Usage",
                      "CA2208:InstantiateArgumentExceptionsCorrectly",
                      Justification = "Library method inlined")>]
    let internal jsonSummary (json: NativeJson.Modules) =
      let summarise go (vc: int) (nc: int) key =

        let pc =
          if nc = 0 then
            "n/a"
          else
            let vc1 = vc |> float

            let nc1 = nc |> float

            Math
              .Round(vc1 * 100.0 / nc1, 2)
              .ToString(CultureInfo.InvariantCulture)

        if go then writeSummary key vc nc pc
        pc

      let l =
        (SummaryFormat.ToList summaryFormat)
        |> Seq.distinct

      let go =
        summaryFormat = Default || l |> Seq.contains O

      let mutable vc = 0
      let mutable nc = 0
      let mutable vm = 0
      let mutable nm = 0
      let mutable vs = 0
      let mutable ns = 0
      let mutable vb = 0
      let mutable nb = 0

      json.Values
      |> Seq.iter
           (fun modul ->
             let mutable cn = []
             let mutable cv = []

             modul.Values
             |> Seq.iter
                  (fun doc ->
                    doc
                    |> Seq.iter
                         (fun cnv ->
                           let mutable visited = false
                           cn <- cnv.Key :: cn

                           cnv.Value.Values
                           |> Seq.iter
                                (fun m ->
                                  nb <- nb + m.Branches.Count

                                  let b =
                                    m.Branches
                                    |> Seq.filter (fun branch -> branch.Hits > 0)
                                    |> Seq.length

                                  vb <- vb + b
                                  visited <- visited || b > 0

                                  ns <- ns + m.Lines.Count

                                  let s =
                                    m.Lines
                                    |> Seq.filter (fun line -> line.Value > 0)
                                    |> Seq.length

                                  vs <- vs + s
                                  visited <- visited || s > 0

                                  nm <- nm + 1
                                  vm <- vm.Increment(s > 0 || b > 0))

                           if visited then cv <- cnv.Key :: cv)

                    )

             vc <- vc + (cv |> Seq.distinct |> Seq.length)
             nc <- nc + (cn |> Seq.distinct |> Seq.length))

      summarise go vc nc "VisitedClasses" |> ignore
      let mcovered = summarise go vm nm "VisitedMethods"
      let covered = summarise go vs ns "VisitedPoints"
      let bcovered = summarise go vb nb "VisitedBranches"

      let extra =
        summaryFormat = Default || l |> Seq.contains C

      if go || extra then write String.Empty

      if l |> Seq.contains B || l |> Seq.contains R then
        writeTC totalTC "C" nc
        writeTC coverTC "C" vc
        writeTC totalTC "M" nm
        writeTC coverTC "M" vm
        writeTC totalTC "S" ns
        writeTC coverTC "S" vs

        l
        |> Seq.iter
             (fun f ->
               let tag =
                 match f with
                 | R -> "R"
                 | B -> "B"
                 | _ -> String.Empty

               if tag |> String.IsNullOrEmpty |> not then
                 writeTC totalTC tag nb
                 writeTC coverTC tag vb)

      [ covered
        bcovered
        mcovered
        "n/a"
        "n/a"
        "n/a" ]

    [<SuppressMessage("Gendarme.Rules.Exceptions",
                      "InstantiateArgumentExceptionCorrectlyRule",
                      Justification = "Inlined library code")>]
    [<SuppressMessage("Microsoft.Usage",
                      "CA2208:InstantiateArgumentExceptionsCorrectly",
                      Justification = "Inlined library code")>]
    let internal standardSummary
      (reportDocument: DocumentType)
      (format: ReportFormat)
      result
      =
      let covered =
        match reportDocument with
        | Unknown -> [] //(result, 0uy, String.Empty)
        | XML report ->
            report
            |> match format with
               | ReportFormat.NCover -> nCoverSummary
               | _ -> openCoverSummary
        | JSON jtext -> jsonSummary jtext // (result, 0uy, String.Empty) // TODO

      let best = (result, 0uy, String.Empty)

      let possibles =
        match threshold with
        | None -> [ best ]
        | Some t ->
            let found =
              covered
              |> List.map (fun d -> d.InvariantParseDouble())

            let ceil (f: float) (value: float) =
              if f <= value && value > 0.0 && f > 0.0 then
                None
              else
                Math.Ceiling(f - value) |> int |> Some

            let sink _ : int option = None

            let funs =
              [ (ceil (float t.Statements), t.Statements, "Statements")
                (if format = ReportFormat.NCover then
                   sink
                 else
                   ceil (float t.Branches)),
                t.Branches,
                "Branches"
                (ceil (float t.Methods), t.Methods, "Methods")
                (if format = ReportFormat.NCover then
                   sink
                 else
                   ceil (float t.AltMethods)),
                t.AltMethods,
                "AltMethods"
                (if format = ReportFormat.NCover || t.Crap = 0uy then
                   sink
                 else
                   (fun c -> ceil c (float t.Crap))),
                t.Crap,
                "Crap"
                (if format = ReportFormat.NCover || t.AltCrap = 0uy then
                   sink
                 else
                   (fun c -> ceil c (float t.AltCrap))),
                t.AltCrap,
                "AltCrap" ]

            List.zip found funs
            |> List.filter (fst >> fst)
            |> List.map
                 (fun (c, (f, x, y)) ->
                   match c |> snd |> f with
                   | Some q -> Some(q, x, y)
                   | None -> None)
            |> List.filter Option.isSome
            |> List.map Option.get
            |> List.filter (fun (a, _, _) -> a >= 0)

      match possibles with
      | [] -> best
      | _ -> possibles |> List.maxBy (fun (a, _, _) -> a)

    let mutable internal summaries : (DocumentType -> ReportFormat -> int -> (int * byte * string)) list =
      []

    let internal addLCovSummary () = summaries <- LCov.summary :: summaries

    let internal addCoberturaSummary () =
      summaries <- Cobertura.summary :: summaries

    let internal initSummary () = summaries <- [ standardSummary ]

  // "Public"
  let internal declareOptions () =
    I.initSummary ()

    [ ("r|recorderDirectory=",
       (fun x ->
         if CommandLine.validateDirectory "--recorderDirectory" x then
           if Option.isSome recordingDirectory then
             CommandLine.error <-
               CommandLine.Format.Local("MultiplesNotAllowed", "--recorderDirectory")
               :: CommandLine.error
           else
             recordingDirectory <- Some(canonicalDirectory x)))
      ("w|workingDirectory=",
       (fun x ->
         if CommandLine.validateDirectory "--workingDirectory" x then
           if Option.isSome workingDirectory then
             CommandLine.error <-
               CommandLine.Format.Local("MultiplesNotAllowed", "--workingDirectory")
               :: CommandLine.error
           else
             workingDirectory <- Some(canonicalDirectory x)))
      ("x|executable=",
       (fun x ->
         if CommandLine.validatePath "--executable" x then
           if Option.isSome executable.Value then
             CommandLine.error <-
               CommandLine.Format.Local("MultiplesNotAllowed", "--executable")
               :: CommandLine.error
           else
             executable := Some x))
      (CommandLine.ddFlag "collect" collect)
      ("l|lcovReport=",
       (fun x ->
         if CommandLine.validatePath "--lcovReport" x then
           if Option.isSome LCov.path.Value then
             CommandLine.error <-
               CommandLine.Format.Local("MultiplesNotAllowed", "--lcovReport")
               :: CommandLine.error
           else
             LCov.path := x |> canonicalPath |> Some
             I.addLCovSummary ()))
      ("t|threshold=",
       (fun x ->
         let ok, t = Threshold.Validate x

         if ok then
           if Option.isSome threshold then
             CommandLine.error <-
               CommandLine.Format.Local("MultiplesNotAllowed", "--threshold")
               :: CommandLine.error
           else
             threshold <- t))
      ("c|cobertura=",
       (fun x ->
         if CommandLine.validatePath "--cobertura" x then
           if Option.isSome Cobertura.path.Value then
             CommandLine.error <-
               CommandLine.Format.Local("MultiplesNotAllowed", "--cobertura")
               :: CommandLine.error
           else
             Cobertura.path := x |> canonicalPath |> Some
             I.addCoberturaSummary ()))
      ("o|outputFile=",
       (fun x ->
         if CommandLine.validatePath "--outputFile" x then
           if Option.isSome output then
             CommandLine.error <-
               CommandLine.Format.Local("MultiplesNotAllowed", "--outputFile")
               :: CommandLine.error
           else
             output <- x |> canonicalPath |> Some))
      (CommandLine.ddFlag "dropReturnCode" CommandLine.dropReturnCode)
      ("summary|teamcity:",
       (fun x ->
         if summaryFormat = Default then
           summaryFormat <-
             if String.IsNullOrWhiteSpace x then
               B
             else
               SummaryFormat.Factory x

           if summaryFormat = Default then
             CommandLine.error <-
               CommandLine.Format.Local("InvalidValue", "--summary", x)
               :: CommandLine.error
         else
           CommandLine.error <-
             CommandLine.Format.Local("MultiplesNotAllowed", "--summary")
             :: CommandLine.error))
      ("q", (fun _ -> CommandLine.verbosity <- CommandLine.verbosity + 1))
      ("?|help|h", (fun x -> CommandLine.help <- not (isNull x)))

      ("<>",
       (fun x ->
         CommandLine.error <-
           CommandLine.Format.Local("InvalidValue", "AltCover", x)
           :: CommandLine.error)) ] // default end stop
    |> List.fold
         (fun (o: OptionSet) (p, a) ->
           o.Add(p, CommandLine.resources.GetString(p), new System.Action<string>(a)))
         (OptionSet())

  let internal requireRecorderTest recordingDirectory success fail =
    match recordingDirectory with
    | None ->
        CommandLine.error <-
          (CommandLine.resources.GetString "recorderRequired")
          :: CommandLine.error

        fail
    | Some path ->
        let dll =
          Path.Combine(path, "AltCover.Recorder.g.dll")

        if File.Exists dll then
          success
        else
          CommandLine.error <-
            CommandLine.Format.Local("recorderNotFound", dll)
            :: CommandLine.error

          fail

  let internal setRecordToFile report =
    doWithStream
      (fun () ->
        let binpath = report + ".acv"
        File.Create(binpath))
      ignore

  module internal J =
    let internal requireExe (parse: Either<string * OptionSet, string list * OptionSet>) =
      match parse with
      | Right (l, options) ->
          match (executable.Value, collect.Value) with
          | (None, false)
          | (Some _, true) ->
              CommandLine.error <-
                (CommandLine.resources.GetString "executableRequired")
                :: CommandLine.error

              Left("UsageError", options)
          | (None, _) -> Right([], options)
          | (Some exe, _) -> Right(exe :: l, options)
      | fail -> fail

    let internal requireRecorder
      (parse: Either<string * OptionSet, string list * OptionSet>)
      =
      match parse with
      | Right (_, options) ->
          requireRecorderTest recordingDirectory parse (Left("UsageError", options))
      | fail -> fail

    let internal requireWorker
      (parse: Either<string * OptionSet, string list * OptionSet>)
      =
      match parse with
      | Right _ ->
          match workingDirectory with
          | None -> workingDirectory <- Directory.GetCurrentDirectory() |> Some
          | _ -> ()

          parse
      | fail -> fail

    // mocking point
    [<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Performance",
                                                      "AvoidUncalledPrivateCodeRule",
                                                      Justification = "Unit test accessor")>]
    let mutable internal recorderName = "AltCover.Recorder.g.dll"

    [<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Correctness",
                                                      "EnsureLocalDisposalRule",
                                                      Justification = "Tuple return confusing Gendarme -- TODO")>]
    let internal recorderInstance () =
      let recorderPath =
        Path.Combine(Option.get recordingDirectory, recorderName)

      let definition =
        AssemblyDefinition.ReadAssembly recorderPath

      (definition, definition.MainModule.GetType("AltCover.Recorder.Instance"))

    let internal getMethod (t: TypeDefinition) (name: string) =
      t.Methods
      |> Seq.filter (fun m -> m.Name = name)
      |> Seq.head

    let internal getFirstOperandAsString (m: MethodDefinition) =
      m.Body.Instructions
      |> Seq.filter (fun i -> i.OpCode = Cil.OpCodes.Ldstr)
      |> Seq.map (fun i -> i.Operand :?> string)
      |> Seq.head

    let internal getFirstOperandAsNumber (m: MethodDefinition) =
      m.Body.Instructions
      |> Seq.filter (fun i -> i.OpCode = Cil.OpCodes.Ldc_I4)
      |> Seq.map (fun i -> i.Operand :?> int)
      |> Seq.head

    let internal payloadBase (rest: string list) =
      CommandLine.doPathOperation
        (fun () ->
          CommandLine.processTrailingArguments
            rest
            (DirectoryInfo(Option.get workingDirectory)))
        255
        true

    let internal runProcess report (payload: string list -> int) (args: string list) =
      setRecordToFile report
      "Beginning run..." |> CommandLine.writeResource

      let result = payload args
      "Getting results..." |> CommandLine.writeResource
      result

    let internal collectResults
      (hits: Dictionary<string, Dictionary<int, PointVisit>>)
      report
      =
      let timer = System.Diagnostics.Stopwatch()
      timer.Start()

      let visits =
        Directory.GetFiles(
          Path.GetDirectoryName(report),
          Path.GetFileName(report) + ".*.acv"
        )
        |> Seq.fold
             (fun before f ->
               timer.Restart()

               let length =
                 FileInfo(f)
                   .Length.ToString("#,#", CultureInfo.CurrentCulture)

               sprintf "... %s (%sb)" f length |> Output.info
               use fileStream = File.OpenRead f

               use results =
                 new DeflateStream(fileStream, CompressionMode.Decompress)

               use formatter = new System.IO.BinaryReader(results)

               let rec sink hitcount =
                 let hit =
                   try
                     let id = formatter.ReadString()
                     let strike = formatter.ReadInt32()
                     let tag = formatter.ReadByte() |> int

                     Some(
                       id,
                       strike,
                       match enum tag with
                       | Tag.Time -> Time <| formatter.ReadInt64()
                       | Tag.Call -> Call <| formatter.ReadInt32()
                       | Tag.Both ->
                           let time = formatter.ReadInt64()
                           let call = formatter.ReadInt32()
                           Both { Time = time; Call = call }
                       | Tag.Table ->
                           let t =
                             Dictionary<string, Dictionary<int, PointVisit>>()

                           let rec ``module`` () =
                             let m = formatter.ReadString()

                             if String.IsNullOrEmpty m then
                               ()
                             else
                               if m |> t.ContainsKey |> not then
                                 t.Add(m, Dictionary<int, PointVisit>())

                               let points = formatter.ReadInt32()

                               let rec sequencePoint pts =
                                 if pts > 0 then
                                   let p = formatter.ReadInt32()
                                   let n = formatter.ReadInt64()

                                   if p |> t.[m].ContainsKey |> not then
                                     t.[m].Add(p, PointVisit.Create())

                                   let pv = t.[m].[p]
                                   pv.Count <- pv.Count + n

                                   let rec tracking () =
                                     let track = formatter.ReadByte() |> int

                                     match enum track with
                                     | Tag.Time ->
                                         pv.Tracks.Add(Time <| formatter.ReadInt64())
                                         tracking ()
                                     | Tag.Call ->
                                         pv.Tracks.Add(Call <| formatter.ReadInt32())
                                         tracking ()
                                     | Tag.Both ->
                                         pv.Tracks.Add(
                                           let time = formatter.ReadInt64()
                                           let call = formatter.ReadInt32()
                                           Both { Time = time; Call = call }
                                         )

                                         tracking ()
                                     // Expect never to happen                                    | Tag.Table -> ``module``()
                                     | _ -> sequencePoint (pts - 1)

                                   tracking ()
                                 else
                                   ``module`` ()

                               sequencePoint points

                           ``module`` ()
                           Table t
                       | _ -> Null
                     )
                   with :? EndOfStreamException -> None

                 match hit with
                 | Some tuple ->
                     let (key, hitPointId, visit) = tuple

                     let increment =
                       if key |> String.IsNullOrWhiteSpace |> not
                          || ((String.IsNullOrEmpty key)
                              && hitPointId = 0
                              && visit.GetType().ToString() = "AltCover.Track+Table") then
                         Counter.addVisit hits key hitPointId visit
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

                 CommandLine.writeResourceWithFormatItems
                   "%d visits recorded in %A (%A visits/sec)"
                   [| delta :> obj; interval; rate |]
                   false

               after)
             0L

      timer.Stop()

      CommandLine.writeResourceWithFormatItems
        "%d visits recorded"
        [| visits |]
        (visits = 0L)

    let internal monitorBase
      (hits: Dictionary<string, Dictionary<int, PointVisit>>)
      report
      (payload: string list -> int)
      (args: string list)
      =
      let result =
        if collect.Value then
          0
        else
          runProcess report payload args

      collectResults hits report
      result

    let internal postProcess
      (counts: Dictionary<string, Dictionary<int, PointVisit>>)
      format
      (document: XmlDocument)
      =
      PostProcess.action "sl" counts format (XmlAbstraction.XmlDoc document)

    let internal point (pt: XmlElement) items outername innername attribute =
      match items with
      | [] -> ()
      | _ ->
          let outer =
            pt.OwnerDocument.CreateElement(outername)

          outer |> pt.AppendChild |> ignore

          items
          |> Seq.choose id
          |> Seq.countBy id
          |> Seq.sortBy fst
          |> Seq.iter
               (fun (t, n) ->
                 let inner =
                   pt.OwnerDocument.CreateElement(innername)

                 inner |> outer.AppendChild |> ignore
                 inner.SetAttribute(attribute, t.ToString())
                 inner.SetAttribute("vc", sprintf "%d" n))

    let internal extractTracks tracks =
      tracks
      |> Seq.map
           (fun t ->
             match t with
             | Time x -> (Some x, None)
             | Both b -> (Some b.Time, Some b.Call)
             | Call y -> (None, Some y)
             | _ -> (None, None))
      |> Seq.toList
      |> List.unzip

    let internal pointProcess (pt: XmlElement) tracks =
      let (times, calls) = extractTracks tracks
      point pt times "Times" "Time" "time"
      point pt calls "TrackedMethodRefs" "TrackedMethodRef" "uid"

    [<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Maintainability",
                                                      "AvoidUnnecessarySpecializationRule",
                                                      Justification = "AvoidSpeculativeGenerality too")>]
    let updateNativeJsonMethod
      (hits: Dictionary<string, Dictionary<int, PointVisit>>)
      (visits: Dictionary<int, PointVisit>)
      (m: NativeJson.Method)
      =
      if m.TId.IsNotNull then
        let tidValue = m.TId.Value
        let e1, entries = hits.TryGetValue Track.Entry

        if e1 then
          let e2, entrypoint = entries.TryGetValue tidValue

          if e2 then
            entrypoint.Tracks
            |> Seq.iter
                 (fun t ->
                   match t with
                   | Time tx -> tx |> NativeJson.fromTracking |> m.Entry.Add
                   | _ -> ())

        let e3, exits = hits.TryGetValue Track.Exit

        if e3 then
          let e4, exitpoint = exits.TryGetValue tidValue

          if e4 then
            exitpoint.Tracks
            |> Seq.iter
                 (fun t ->
                   match t with
                   | Time tx -> tx |> NativeJson.fromTracking |> m.Exit.Add
                   | _ -> ())

      let fillTracks tracks calls =
        let ntrack =
          if tracks |> isNull && calls |> Seq.isEmpty |> not then
            NativeJson.Tracks()
          else
            tracks

        if calls |> Seq.isEmpty |> not then
          ntrack.AddRange calls

        ntrack

      let fillTimes (times: NativeJson.Times) (visits: int64 seq) =
        let ntimes =
          if times |> isNull && visits |> Seq.isEmpty |> not then
            NativeJson.Times()
          else
            times

        if visits |> Seq.isEmpty |> not then
          ntimes.AddRange(visits |> Seq.map NativeJson.fromTracking)

        ntimes

      let sps =
        m.SeqPnts
        |> Seq.map
             (fun sp ->
               let b, count = visits.TryGetValue sp.Id

               if b then
                 let (times', calls') = extractTracks count.Tracks
                 let times = times' |> Seq.choose id
                 let calls = calls' |> Seq.choose id

                 { sp with
                     VC = (int <| count.Total()) + Math.Max(0, sp.VC)
                     Tracks = fillTracks sp.Tracks calls
                     Times = fillTimes sp.Times times }
               else
                 sp)
        |> Seq.toList

      m.SeqPnts.Clear()
      m.SeqPnts.AddRange sps

      let bps =
        m.Branches
        |> Seq.map
             (fun bp ->
               let b, count =
                 visits.TryGetValue(bp.Id ||| Counter.branchFlag)

               if b then
                 let (times', calls') = extractTracks count.Tracks
                 let times = times' |> Seq.choose id
                 let calls = calls' |> Seq.choose id

                 { bp with
                     Hits = (int <| count.Total()) + Math.Max(0, bp.Hits)
                     Tracks = fillTracks bp.Tracks calls
                     Times = fillTimes bp.Times times }
               else
                 bp)
        |> Seq.toList

      m.Branches.Clear()
      m.Branches.AddRange bps

      m.SeqPnts
      |> Seq.groupBy (fun s -> s.SL)
      |> Seq.iter (fun (l, ss) -> m.Lines.[l] <- Json.lineVisits ss)

    [<SuppressMessage("Gendarme.Rules.Correctness",
                      "EnsureLocalDisposalRule",
                      Justification = "The reader must not close the stream")>]
    [<SuppressMessage("Microsoft.Reliability",
                      "CA2000:DisposeObjectsBeforeLosingScope",
                      Justification = "The reader must not close the stream")>]
    let writeNativeJsonReport
      (hits: Dictionary<string, Dictionary<int, PointVisit>>)
      _
      (file: Stream)
      output
      =
      let flushStart = DateTime.UtcNow
      // do work here
      let jsonText =
        let reader = new StreamReader(file) // DO NOT DISPOSE
        reader.ReadToEnd()

      let json = NativeJson.fromJsonText jsonText

      // do magic here
      json
      |> Seq.iter
           (fun kvp ->
             let key = kvp.Key
             let b, visits = hits.TryGetValue key

             if b then
               kvp.Value.Values
               |> Seq.collect (fun doc -> doc.Values)
               |> Seq.collect (fun c -> c.Values)
               |> Seq.iter (updateNativeJsonMethod hits visits))

      let encoded = NativeJson.serializeToUtf8Bytes json

      if Option.isSome output then
        use outputFile =
          new FileStream(
            output.Value,
            FileMode.OpenOrCreate,
            FileAccess.Write,
            FileShare.None,
            4096,
            FileOptions.SequentialScan
          )

        outputFile.Write(encoded, 0, encoded.Length)
      else
        file.Seek(0L, SeekOrigin.Begin) |> ignore
        file.SetLength 0L
        file.Write(encoded, 0, encoded.Length)

      TimeSpan(DateTime.UtcNow.Ticks - flushStart.Ticks)

    let internal writeReportBase
      (hits: Dictionary<string, Dictionary<int, PointVisit>>)
      format
      report
      =
      let reporter (arg: string option) =
        let (container, file) = Zip.openUpdate report

        try
          if format = ReportFormat.NativeJson
             || format = ReportFormat.NativeJsonWithTracking then
            writeNativeJsonReport hits format file arg
          else
            AltCover.Counter.doFlushStream
              (postProcess hits format)
              pointProcess
              true
              hits
              format
              file
              arg
        finally
          file.Dispose()

          if container.IsNotNull then
            container.Dispose()

      reporter

    // mocking points
    [<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Performance",
                                                      "AvoidUncalledPrivateCodeRule",
                                                      Justification = "Unit test accessor")>]
    let mutable internal getPayload = payloadBase

    [<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Performance",
                                                      "AvoidUncalledPrivateCodeRule",
                                                      Justification = "Unit test accessor")>]
    let mutable internal getMonitor = monitorBase

    [<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Performance",
                                                      "AvoidUncalledPrivateCodeRule",
                                                      Justification = "Unit test accessor")>]
    let mutable internal doReport = writeReportBase

    let internal doSummaries (document: DocumentType) (format: ReportFormat) result =
      let (code, t, f) =
        I.summaries
        |> List.fold
             (fun (r, t, f) summary ->
               let rx, t2, f2 = summary document format r

               if rx > r then
                 (rx, t2, f2)
               else
                 (r, t, f))
             (result, 0uy, String.Empty)

      if (code > 0 && result = 0 && code <> result) then
        CommandLine.writeErrorResourceWithFormatItems f [| code :> obj; t :> obj |]

      code

  // "Public"
  let internal doCoverage arguments options1 =
    let check1 =
      declareOptions ()
      |> CommandLine.parseCommandLine (arguments |> Array.skip 1)
      |> CommandLine.processHelpOption
      |> J.requireExe
      |> J.requireRecorder
      |> J.requireWorker

    match check1 with
    | Left (intro, options) ->
        CommandLine.handleBadArguments
          false
          arguments
          { Intro = intro
            Options = options1
            Options2 = options }

        255
    | Right (rest, _) ->
        CommandLine.applyVerbosity ()

        let value =
          CommandLine.doPathOperation
            (fun () ->
              let pair = J.recorderInstance ()
              let instance = snd pair

              let report =
                (J.getMethod instance "get_ReportFile")
                |> J.getFirstOperandAsString
                |> canonicalPath

              let format =
                (J.getMethod instance "get_CoverageFormat")
                |> J.getFirstOperandAsNumber
                |> enum

              let hits =
                Dictionary<string, Dictionary<int, PointVisit>>()

              let payload = J.getPayload
              let result = J.getMonitor hits report payload rest
              let delta = J.doReport hits format report output

              CommandLine.writeResourceWithFormatItems
                "Coverage statistics flushing took {0:N} seconds"
                [| delta.TotalSeconds |]
                false

              // And tidy up after everything's done
              File.Delete(report + ".acv")

              Directory.GetFiles(
                Path.GetDirectoryName(report),
                Path.GetFileName(report) + ".*.acv"
              )
              |> Seq.iter File.Delete

              let document = DocumentType.LoadReport format report
              J.doSummaries document format result)
            255
            true

        CommandLine.reportErrors "Collection" false
        value

[<assembly: SuppressMessage("Microsoft.Naming",
                            "CA1704:IdentifiersShouldBeSpelledCorrectly",
                            Scope = "member",
                            Target = "AltCover.TeamCityFormat+Tags.#B",
                            MessageId = "B",
                            Justification = "TeamCity notation")>]
[<assembly: SuppressMessage("Microsoft.Naming",
                            "CA1704:IdentifiersShouldBeSpelledCorrectly",
                            Scope = "member",
                            Target = "AltCover.TeamCityFormat+Tags.#R",
                            MessageId = "R",
                            Justification = "TeamCity notation")>]
()