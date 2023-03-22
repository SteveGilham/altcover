namespace AltCover

open Avalonia
open Avalonia.Markup.Xaml
#if AVALONIA11
open Avalonia.Styling
open Avalonia.Themes.Simple
open Avalonia.Themes.Fluent
#endif

type App() =
  inherit Application()

  override this.Initialize() =
#if AVALONIA11
    let style = Styles()
    style.Add <| SimpleTheme()
    this.Styles.Add style
#endif
    AvaloniaXamlLoader.Load(this)

  override this.OnFrameworkInitializationCompleted() =
    let life =
      (this.ApplicationLifetime
      :?> Avalonia.Controls.ApplicationLifetimes.IClassicDesktopStyleApplicationLifetime)

    life.MainWindow <- MainWindow()
    base.OnFrameworkInitializationCompleted()