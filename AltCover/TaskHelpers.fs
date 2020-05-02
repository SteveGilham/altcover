namespace AltCover

open System

module internal ApiStore =
  let mutable internal store = String.Empty
  let private writeToStore s = store <- s
  let internal logToStore =
    FSApi.Logging.Primitive { Primitive.Logging.Create() with Info = writeToStore }

  let internal getStringValue s =
    writeToStore String.Empty
    logToStore.Apply()
    [| s |]
    |> Main.effectiveMain
    |> ignore
    store