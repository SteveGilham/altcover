namespace AltCover.Visualizer

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Xml.XPath
open AltCover.Augment
open AltCover.Visualizer.GuiCommon

type IVisualizerWindow =
  interface
  abstract member Title : string with get, set
  abstract member CoverageFiles : string list with get, set
  abstract member ShowMessageOnGuiThread : MessageType -> string -> unit
  end