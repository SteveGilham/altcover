open Avalonia
open Avalonia.Controls
open Avalonia.Logging
open Avalonia.Logging.Serilog

open AltCover.Avalonia
open Mono.Options

let BuildAvaloniaApp() =
  AppBuilderBase<AppBuilder>.Configure<App>().UsePlatformDetect()
    .LogToDebug(LogEventLevel.Warning)

[<EntryPoint>]
let main arguments =
  let options =
    [ ("g|geometry",
       (fun _ ->
         Persistence.clearGeometry()
         Persistence.save <- false))
      ("r|recentFiles", (fun _ -> Persistence.saveCoverageFiles [])) ]
    |> List.fold
         (fun (o : OptionSet) (p, a) ->
           o.Add(p, UICommon.GetResourceString p, new System.Action<string>(a)))
         (OptionSet())
  options.Parse(arguments) |> ignore
  AppBuilder.Configure<App>().UsePlatformDetect().Start<MainWindow>()
  0