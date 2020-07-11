namespace AltCover.Visualizer

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Xml.XPath
open AltCover.Augment
open AltCover.Visualizer.GuiCommon

module HandlerCommon =
  let DoRowActivation (m:XPathNavigator) (vWindow:IVisualizerWindow)
    noSource showSource =
      let points = [ "seqpnt"; "branch"]
                   |> List.map (fun tag -> m.SelectChildren(tag, String.Empty) |> Seq.cast<XPathNavigator>)
                   |> Seq.concat
      let allpoints = [[m] |> List.toSeq; points] |> Seq.concat
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
        vWindow.Title <- "AltCover.Visualizer - " + filename
        let info = GetSource(filename)
        let current = new FileInfo(vWindow.CoverageFiles.Head)
        if (not <| info.Exists) then
          Messages.MissingSourceThisFileMessage vWindow.ShowMessageOnGuiThread current info
        else if (info.Outdated current.LastWriteTimeUtc) then
          Messages.OutdatedCoverageThisFileMessage vWindow.ShowMessageOnGuiThread current info
        else
          let lineNumber = Int32.TryParse(line |> Option.get) |> snd
          showSource info lineNumber