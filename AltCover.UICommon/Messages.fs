namespace AltCover.Visualizer

open System.IO

open GuiCommon

type MessageType =
  | Info = 0
  | Warning = 1
  //| Question = 2
  | Error = 3
  //| Other = 4

type Display = MessageType -> string -> unit

module Messages =
  let InvalidCoverageFileMessage (display : Display) (file : InvalidFile) =
    let message =
      Resource.Format( "InvalidFile",
                          [ file.File.FullName
                            file.Fault.Message ])
    display MessageType.Error message

  [<System.Diagnostics.CodeAnalysis.SuppressMessage(
    "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
    Justification = "AvoidSpeculativeGenerality too")>]
  let private showMessageResourceFileWarning rn (display : Display) (file : FileInfo) (source : Source) =
    let message = // rely of the format to drop the source file if not needed
      Resource.Format(rn, [file.FullName; source.FullName])
    display MessageType.Warning message

  let OutdatedCoverageFileMessage (display : Display) (file : FileInfo) =
    showMessageResourceFileWarning "CoverageOutOfDate" display file
      (Source.Dummy)

  let MissingSourceFileMessage (display : Display) (file : FileInfo) =
    showMessageResourceFileWarning "MissingSourceFile" display file
      (Source.Dummy)

  let OutdatedCoverageThisFileMessage (display : Display) (file : FileInfo)
      (source : Source) =
    showMessageResourceFileWarning "CoverageOutOfDateThisFile" display file source

  let MissingSourceThisFileMessage (display : Display) (file : FileInfo) (source : Source) =
    showMessageResourceFileWarning "MissingSourceThisFile" display file source