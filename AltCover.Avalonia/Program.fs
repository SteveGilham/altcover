namespace AltCover

open Avalonia
open Avalonia.Controls
open Avalonia.Logging

open Mono.Options

module VisualizerMain =
#if !NETSTANDARD2_0 // no AppBuilder here
  let BuildAvaloniaApp () =
    AppBuilderBase<AppBuilder>
      .Configure<App>()
      .UsePlatformDetect()
      .LogToTrace(LogEventLevel.Warning)
#endif

  [<EntryPoint>]
  let main arguments =
    let options =
      [ ("g|geometry",
         (fun _ ->
           Persistence.clearGeometry ()
           Persistence.save <- false))
        ("r|recentFiles", (fun _ -> Persistence.saveCoverageFiles [])) ]
      |> List.fold
           (fun (o: OptionSet) (p, a) ->
             o.Add(p, Resource.GetResourceString p, new System.Action<string>(a)))
           (OptionSet())

    options.Parse(arguments) |> ignore

#if !NETSTANDARD2_0
    BuildAvaloniaApp()
      .StartWithClassicDesktopLifetime(arguments)
#else
    0
#endif