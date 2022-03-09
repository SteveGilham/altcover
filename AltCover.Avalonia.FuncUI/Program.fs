﻿namespace AltCover.Avalonia.FuncUI

open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.FuncUI

/// This is your application you can ose the initialize method to load styles
/// or handle Life Cycle events of your application
type App() =
  inherit Application()

  override this.Initialize() =
    this.Styles.Load "avares://Avalonia.Themes.Default/DefaultTheme.xaml"
    this.Styles.Load "avares://Avalonia.Themes.Default/Accents/BaseDark.xaml"
    this.Styles.Load "avares://AltCover.Avalonia.FuncUI/Styles.xaml"

  override this.OnFrameworkInitializationCompleted() =
    match this.ApplicationLifetime with
    | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
      desktopLifetime.MainWindow <- Shell.MainWindow()
    | _ -> ()

module Program =

  [<EntryPoint>]
  let main (args: string []) =
#if NETSTANDARD2_0 // no AppBuilder here
    args.Length
#else
    AppBuilder
      .Configure<App>()
      .UsePlatformDetect()
      .UseSkia()
      .StartWithClassicDesktopLifetime(args)
#endif