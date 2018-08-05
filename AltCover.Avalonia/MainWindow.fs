namespace AltCover.Avalonia

open System
open System.Globalization
open System.Resources

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Html
open Avalonia.Markup.Xaml
open ReactiveUI

module UICommon =

    let GetResourceString (key:string) =
        let executingAssembly = System.Reflection.Assembly.GetExecutingAssembly()
        let resources = new ResourceManager("AltCover.Visualizer.Strings", executingAssembly)
        resources.GetString(key)

type MainWindowViewModel  () =
    inherit ReactiveObject()

    let mutable exit = ReactiveCommand.Create(new Action(fun _ -> Application.Current.Exit()))

    member this.Exit
        with get() =
            exit

type MainWindow () as this =
    inherit Window()

    let mutable armed = false

    do this.InitializeComponent()

    member this.InitializeComponent() =
        AvaloniaXamlLoader.Load(this)
        this.DataContext <- MainWindowViewModel()

        this.Title <- "AltCover.Visualizer"
        this.FindControl<TabItem>("Visualizer").Header <- UICommon.GetResourceString "Visualizer"

        ["Open"; "Refresh"; "Font"; "Exit"]
        |> Seq.iter (fun n -> let item = this.FindControl<TextBlock>(n + "Text")
                              item.Text <- UICommon.GetResourceString n)

        this.FindControl<TabItem>("About").Header <- UICommon.GetResourceString "About"
        this.FindControl<TextBlock>("Program").Text <- "AltCover.Visualizer " + AssemblyVersionInformation.AssemblyFileVersion
        this.FindControl<TextBlock>("Description").Text <- UICommon.GetResourceString "ProgramDescription"

        let copyright = AssemblyVersionInformation.AssemblyCopyright
        this.FindControl<TextBlock>("Copyright").Text <- copyright

        let link = this.FindControl<HtmlLabel>("Link")
        link.Text <- """<center><a href="http://www.github.com/SteveGilham/altcover">""" +
                      UICommon.GetResourceString "WebsiteLabel" +
                      "</a></center>"

        link.PointerPressed |> Event.add (fun _ -> armed <- true)
        link.PointerLeave |> Event.add (fun _ -> armed <- false)
        link.PointerReleased
        |> Event.add (fun _ -> ())

        // Windows -- Process Start (url)
        // Mac -- ("open", url)
        // *nix -- ("xdg-open", url)
        //Application.Instance.Open("http://www.github.com/SteveGilham/altcover"))

        this.FindControl<TextBlock>("License").Text <- UICommon.GetResourceString "AboutDialog.License"
        this.FindControl<TextBlock>("MIT").Text <- String.Format(CultureInfo.InvariantCulture,
                                                                 UICommon.GetResourceString "License",
                                                                 copyright)
