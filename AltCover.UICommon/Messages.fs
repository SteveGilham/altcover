namespace AltCover

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
  let InvalidCoverageFileMessage (display: Display) (file: InvalidFile) =
    let message =
      Resource.Format(
        "InvalidFile",
        [ file.File.FullName
          file.Fault.Message ]
      )

    display MessageType.Error message