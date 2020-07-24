namespace AltCover

open System.Diagnostics.CodeAnalysis

type IVisualizerWindow =
  interface
  abstract member Title : string with get, set
  abstract member CoverageFiles : string list with get, set
  [<SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
                    Justification="Compiler generated")>]
  abstract member ShowMessageOnGuiThread : MessageType -> string -> unit
  end