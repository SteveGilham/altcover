namespace AltCover

open System

module internal TaskIO =
  type AltCover.OptionApi.LoggingOptions with
    member internal self.Apply() =
      Output.error <- self.Error
      Output.warn <- self.Warn
      Output.info <- self.Info
      Output.echo <- self.Echo

  let mutable internal store = String.Empty
  let private writeToStore s = store <- s
  let internal logToStore =
    OptionApi.LoggingOptions.Primitive { Primitive.LoggingOptions.Create() with Info = writeToStore }

  let internal getStringValue s =
    writeToStore String.Empty
    logToStore.Apply()
    [| s |]
    |> Main.effectiveMain
    |> ignore
    store

  let internal colourize name =
    let ok, colour = Enum.TryParse<ConsoleColor>(name, true)
    if ok then Console.ForegroundColor <- colour