namespace AltCover

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.Globalization
open System.IO
open System.Linq
open System.Xml.XPath
open GuiCommon

module HandlerCommon =
  let DoRowActivation
    (methodPath: XPathNavigator)
    (window: IVisualizerWindow)
    (noSource: unit -> unit)
    (showSource: Source -> int -> unit)
    =
    let points =
      [ "seqpnt"; "branch" ]
      |> List.map
           (fun tag ->
             methodPath.SelectChildren(tag, String.Empty)
             |> Seq.cast<XPathNavigator>)
      |> Seq.concat

    let allpoints =
      [ [ methodPath ] |> List.toSeq; points ]
      |> Seq.concat

    let document =
      allpoints
      |> Seq.map (fun p -> p.GetAttribute("document", String.Empty))
      |> Seq.tryFind (String.IsNullOrWhiteSpace >> not)

    let line =
      allpoints
      |> Seq.map (fun p -> p.GetAttribute("line", String.Empty))
      |> Seq.tryFind (String.IsNullOrWhiteSpace >> not)

    if document |> Option.isNone || line |> Option.isNone then
      noSource ()
    else
      let filename = Option.get document
      window.Title <- "AltCover.Visualizer - " + filename
      // get embed if any & fold it in here
      let embed = GuiCommon.Embed methodPath filename
      let info = GetSource(filename).MakeEmbedded filename embed

      let lineNumber =
          Int32.TryParse(line |> Option.get) |> snd

      showSource info lineNumber

  let private filterCoverage lines (n: CodeTag) =
    n.Line > 0
    && n.EndLine > 0
    && n.Line <= lines
    && n.EndLine <= lines

  let private (|Select|_|) (pattern: String) offered =
    if
      (fst offered) |> String.IsNullOrWhiteSpace |> not
      && pattern.StartsWith
        (
          fst offered,
          StringComparison.Ordinal
        )
    then
      Some offered
    else
      None

  let private selectStyle because excluded =
    match (because, excluded) with
    | Select "author declared (" _ -> Exemption.Declared
    | Select "tool-generated: " _ -> Exemption.Automatic
    | Select "static analysis: " _ -> Exemption.StaticAnalysis
    | (_, true) -> Exemption.Excluded
    | _ -> Exemption.None

  let private coverageToTag lineOnly (n: XPathNavigator) =
    let excluded =
      Boolean.TryParse(n.GetAttribute("excluded", String.Empty))
      |> snd

    let visitcount =
      Int32.TryParse(n.GetAttribute("visitcount", String.Empty))
      |> snd

    let line = n.GetAttribute("line", String.Empty)
    let column = n.GetAttribute("column", String.Empty)
    let endline = n.GetAttribute("endline", String.Empty)

    let endcolumn =
      n.GetAttribute("endcolumn", String.Empty)
    // Extension behaviour for textual signalling for three lines
    n.MoveToParent() |> ignore

    let because =
      n.GetAttribute("excluded-because", String.Empty)

    { Style =
        match Exemption.OfInt visitcount with
        | Exemption.None -> selectStyle because excluded
        | Exemption.Declared -> Exemption.Declared
        | Exemption.Automatic -> Exemption.Automatic
        | Exemption.StaticAnalysis -> Exemption.StaticAnalysis
        | Exemption.Excluded -> Exemption.Excluded
        | _ -> Exemption.Visited
      LineOnly = lineOnly
      VisitCount = visitcount
      Line = Int32.TryParse(line) |> snd
      Column = (Int32.TryParse(column) |> snd)
      EndLine = Int32.TryParse(endline) |> snd
      EndColumn = (Int32.TryParse(endcolumn) |> snd) }

  [<SuppressMessage("Gendarme.Rules.Exceptions",
                    "InstantiateArgumentExceptionCorrectlyRule",
                    Justification = "Inlined library code")>]
  [<SuppressMessage("Microsoft.Usage",
                    "CA2208:InstantiateArgumentExceptionsCorrectly",
                    Justification = "Ditto, ditto")>]
  let TagCoverage (methodPath: XPathNavigator) (file: Source) (sourceLines: int) =
    let lineOnly =
      methodPath.Select("//coverage[@lineonly]")
      |> Seq.cast<XPathNavigator>
      |> Seq.isEmpty
      |> not

    methodPath.Select("//seqpnt[@document='" + file.FullName + "']") // TODO encapsulate more
    |> Seq.cast<XPathNavigator>
    |> Seq.map (coverageToTag lineOnly)
    |> Seq.filter (filterCoverage sourceLines)
    |> Seq.sortByDescending (fun t -> t.VisitCount)
    |> Seq.toList

  [<SuppressMessage("Microsoft.Naming",
                    "CA1702:CompoundWordsShouldBeCasedCorrectly",
                    Justification = "Why is this different to 'TagBranches'???")>]
  let TagLines (visited: 'Tag) (notVisited: 'Tag) (tags: CodeTag list) =
    tags
    |> List.groupBy (fun t -> t.Line)
    |> List.map
         (fun (l, t) ->
           let total =
             t
             |> Seq.sumBy
                  (fun tag ->
                    if tag.VisitCount <= 0 then
                      0
                    else
                      tag.VisitCount)

           (l,
            if total > 0 then
              visited
            else
              notVisited))

  let private parseIntegerAttribute (element: XPathNavigator) (attribute: string) =
    let text =
      element.GetAttribute(attribute, String.Empty)

    let number =
      Int32.TryParse(text, NumberStyles.None, CultureInfo.InvariantCulture)

    if (fst number) then
      snd number
    else
      if not <| String.IsNullOrEmpty(text) then
        System.Diagnostics.Debug.WriteLine(
          "ParseIntegerAttribute : '"
          + attribute
          + "' with value '"
          + text
        )

      0

  let TagBranches (methodPath: XPathNavigator) (file: Source) =
    (methodPath.Select("//branch[@document='" + file.FullName + "']") // TODO
     |> Seq.cast<XPathNavigator>
     |> Seq.groupBy (fun n -> n.GetAttribute("line", String.Empty))
     |> Seq.toList
     |> Seq.map
          (fun n ->
            let line =
              parseIntegerAttribute ((snd n) |> Seq.head) "line"

            let num = (snd n) |> Seq.length

            let v =
              (snd n)
              |> Seq.filter (fun x -> x.GetAttribute("visitcount", String.Empty) <> "0")
              |> Seq.length

            line, (v, num)))
      .ToDictionary(fst, snd)

  let UpdateCoverageFiles (window: IVisualizerWindow) path add =
    let casematch =
      match System.Environment.GetEnvironmentVariable("OS") with
      | "Windows_NT" -> StringComparison.OrdinalIgnoreCase
      | _ -> StringComparison.Ordinal

    let files =
      window.CoverageFiles
      |> List.filter (fun n -> not (n.Equals(path, casematch)))
      |> Seq.truncate (9)
      |> Seq.toList

    window.CoverageFiles <-
      (if add then (path :: files) else files)
      |> Seq.distinctBy
           (fun n ->
             match casematch with
             | StringComparison.Ordinal -> n
             | _ -> n.ToUpperInvariant())
      |> Seq.toList

  [<SuppressMessage("Gendarme.Rules.Maintainability",
                    "AvoidUnnecessarySpecializationRule",
                    Justification = "Avoid speculative generality too")>]
  let IconForBranches
    (icons: Icons<'TIcon>)
    (branches: Dictionary<int, int * int>)
    line
    (setToolTip: string -> unit)
    =
    let counts = branches.TryGetValue line

    let (|AllVisited|_|) (b, (v, num)) =
      if b |> not || v <> num then
        None
      else
        Some()

    if fst counts then
      let v, num = snd counts

      setToolTip
      <| Resource.Format("branchesVisited", [ v; num ])

    (match counts with
     | (false, _) -> icons.Blank
     | (_, (0, _)) -> icons.RedBranch
     | AllVisited -> icons.Branched
     | _ -> icons.Branch)
      .Force()

  // Fill in the menu from the memory cache
  let PopulateMenu
    (items: 'TMenuItem seq)
    (newItems: string seq)
    (clear: 'TMenuItem -> unit)
    (set: string -> 'TMenuItem -> unit)
    =
    // blank the whole menu
    items |> Seq.iter clear
    // fill in with the items we have
    Seq.zip newItems items
    |> Seq.iter (fun (name, item) -> set name item)

    items.Any()