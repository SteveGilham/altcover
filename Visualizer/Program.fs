open Avalonia
open Avalonia.Controls
open Avalonia.Logging
open Avalonia.Logging.Serilog

open AltCover.Avalonia

let BuildAvaloniaApp() =
    AppBuilderBase<AppBuilder>.Configure<App>().UsePlatformDetect().LogToDebug(LogEventLevel.Warning)

[<EntryPoint>]
let main _ =
    AppBuilder.Configure<App>()
        .UsePlatformDetect()
        .Start<MainWindow>()
    0