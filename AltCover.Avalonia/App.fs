namespace AltCover.Avalonia

open Avalonia
open Avalonia.Markup.Xaml

type App() =
  inherit Application()
  override this.Initialize() =
    AvaloniaXamlLoader.Load(this)

  override this.OnFrameworkInitializationCompleted() =
    let life = (this.ApplicationLifetime :?> Avalonia.Controls.ApplicationLifetimes.IClassicDesktopStyleApplicationLifetime)
    life.MainWindow <- MainWindow()
    base.OnFrameworkInitializationCompleted()