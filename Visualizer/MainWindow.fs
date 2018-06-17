namespace AltCover.Avalonia

open Avalonia.Controls
open Avalonia.Markup.Xaml

type MainWindow () as this =
    inherit Window()

    do this.InitializeComponent()
    member this.InitializeComponent() =
        AvaloniaXamlLoader.Load(this)