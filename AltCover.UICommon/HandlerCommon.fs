namespace AltCover.Visualizer

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Xml.XPath
open AltCover
open AltCover.Visualizer.GuiCommon

module HandlerCommon =
  let DoRowActivation (methodPath:XPathNavigator) (window:IVisualizerWindow)
    noSource showSource =
      let points = [ "seqpnt"; "branch"]
                   |> List.map (fun tag -> methodPath.SelectChildren(tag, String.Empty) |> Seq.cast<XPathNavigator>)
                   |> Seq.concat
      let allpoints = [[methodPath] |> List.toSeq; points] |> Seq.concat
      let document = allpoints
                     |> Seq.map (fun p -> p.GetAttribute("document", String.Empty))
                     |> Seq.tryFind (fun d -> d |> String.IsNullOrWhiteSpace |> not)
      let line = allpoints
                 |> Seq.map (fun p -> p.GetAttribute("line", String.Empty))
                 |> Seq.tryFind (fun d -> d |> String.IsNullOrWhiteSpace |> not)
      if document |> Option.isNone ||
         line |> Option.isNone then
        noSource()
      else
        let filename = Option.get document
        window.Title <- "AltCover.Visualizer - " + filename
        let info = GetSource(filename)
        let current = new FileInfo(window.CoverageFiles.Head)
        if (not <| info.Exists) then
          Messages.MissingSourceThisFileMessage window.ShowMessageOnGuiThread current info
        else if (info.Outdated current.LastWriteTimeUtc) then
          Messages.OutdatedCoverageThisFileMessage window.ShowMessageOnGuiThread current info
        else
          let lineNumber = Int32.TryParse(line |> Option.get) |> snd
          showSource info lineNumber

  let private filterCoverage lines (n : CodeTag) =
    n.Line > 0 && n.EndLine > 0 && n.Line <= lines && n.EndLine <= lines

  let private (|Select|_|) (pattern : String) offered =
    if (fst offered)
        |> String.IsNullOrWhiteSpace
        |> not
        && pattern.StartsWith(fst offered, StringComparison.Ordinal) then
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

  let private coverageToTag(n : XPathNavigator) =
    let excluded = Boolean.TryParse(n.GetAttribute("excluded", String.Empty)) |> snd
    let visitcount = Int32.TryParse(n.GetAttribute("visitcount", String.Empty)) |> snd
    let line = n.GetAttribute("line", String.Empty)
    let column = n.GetAttribute("column", String.Empty)
    let endline = n.GetAttribute("endline", String.Empty)
    let endcolumn = n.GetAttribute("endcolumn", String.Empty)
    // Extension behaviour for textual signalling for three lines
    n.MoveToParent() |> ignore
    let because = n.GetAttribute("excluded-because", String.Empty)
    { Style =
        match Exemption.OfInt visitcount with
        | Exemption.None -> selectStyle because excluded
        | Exemption.Declared -> Exemption.Declared
        | Exemption.Automatic -> Exemption.Automatic
        | Exemption.StaticAnalysis -> Exemption.StaticAnalysis
        | Exemption.Excluded -> Exemption.Excluded
        | _ -> Exemption.Visited
      VisitCount = visitcount
      Line = Int32.TryParse(line) |> snd
      Column = (Int32.TryParse(column) |> snd)
      EndLine = Int32.TryParse(endline) |> snd
      EndColumn = (Int32.TryParse(endcolumn) |> snd) }

  [<SuppressMessage("Gendarme.Rules.Exceptions", "InstantiateArgumentExceptionCorrectlyRule",
                    Justification="Inlined library code")>]
  [<SuppressMessage("Microsoft.Usage", "CA2208:InstantiateArgumentExceptionsCorrectly",
                    Justification="Ditto, ditto")>]
  let TagCoverage (methodPath:XPathNavigator) (fileName:string) (sourceLines:int) =
    methodPath.Select("//seqpnt[@document='" + fileName + "']")
    |> Seq.cast<XPathNavigator>
    |> Seq.map coverageToTag
    |> Seq.filter (filterCoverage sourceLines)
    |> Seq.sortByDescending (fun t -> t.VisitCount)
    |> Seq.toList