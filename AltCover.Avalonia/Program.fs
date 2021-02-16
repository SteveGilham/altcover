namespace AltCover

open Avalonia
open Avalonia.Controls
open Avalonia.Logging

open Mono.Options

module VisualizerMain =
  let BuildAvaloniaApp() =
    AppBuilderBase<AppBuilder>.Configure<App>().UsePlatformDetect()
      .LogToTrace(LogEventLevel.Warning)

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
             o.Add(p, Resource.GetResourceString p, new System.Action<string>(a)))
           (OptionSet())
    options.Parse(arguments) |> ignore
    BuildAvaloniaApp().StartWithClassicDesktopLifetime(arguments)