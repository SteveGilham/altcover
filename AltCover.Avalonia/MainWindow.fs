namespace AltCover

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Linq
open System.Xml.XPath

open GuiCommon

open AltCover.FontSupport
open Avalonia.Controls
open Avalonia.Controls.Presenters
open Avalonia.Markup.Xaml
open Avalonia.Media
open Avalonia.Media.Imaging
open Avalonia.Threading

type Thickness = Avalonia.Thickness

type MainWindow() as this =
  inherit Window()
  let mutable armed = false
  let mutable justOpened = String.Empty
  let mutable coverageFiles : string list = []
  let ofd = OpenFileDialog()
  let iconMaker (x: Stream) = new Bitmap(x)
  let icons = Icons(iconMaker)

  let getIcon name =
    let iconstype = icons.GetType()
    let named = iconstype.GetProperty(name)
    let value = named.GetValue(icons) :?> Lazy<Bitmap>
    value.Force()

  let visited = SolidColorBrush.Parse "#0000CD" // "#F5F5F5" // Medium Blue on White Smoke
  let declared = SolidColorBrush.Parse "#FFA500" // "#F5F5F5" // Orange on White Smoke
  let staticAnalysis = SolidColorBrush.Parse "#000000" // "#F5F5F5" // Black on White Smoke
  let automatic = SolidColorBrush.Parse "#FFD700" // "#F5F5F5" // Gold on White Smoke
  let notVisited = SolidColorBrush.Parse "#DC143C" // "#F5F5F5"// Crimson on White Smoke
  let excluded = SolidColorBrush.Parse "#87CEEB" // "#F5F5F5" // Sky Blue on White Smoke

  let makeTreeNode name icon =
    let tree = new Image()
    tree.Source <- icons.TreeExpand.Force()
    tree.Margin <- Thickness.Parse("2")
    let text = new TextBlock()
    text.Text <- name
    text.Margin <- Thickness.Parse("2")
    let image = new Image()
    image.Source <- icon
    image.Margin <- Thickness.Parse("2")
    let display = new StackPanel()
    display.Orientation <- Avalonia.Layout.Orientation.Horizontal
    display.Children.Add tree
    display.Children.Add image
    display.Children.Add text
    display.Tag <- name
    display

  do
    this.InitializeComponent()
    this.Show()

  member private this.DisplayMessage (status: MessageType) message =
    let caption =
      match status with
      | MessageType.Info -> Resource.GetResourceString "LoadInfo"
      | MessageType.Warning -> Resource.GetResourceString "LoadWarning"
      | _ -> Resource.GetResourceString "LoadError"

    this.ShowMessageBox status caption message

  member private this.ShowMessageBox (status: MessageType) caption message =
    Dispatcher.UIThread.Post
      (fun _ ->
        this.FindControl<Image>("Status").Source <- (match status with
                                                     | MessageType.Info -> icons.Info
                                                     | MessageType.Warning -> icons.Warn
                                                     | _ -> icons.Error)
          .Force()

        this.FindControl<TextBlock>("Caption").Text <- caption
        this.FindControl<TextBox>("Message").Text <- message
        this.FindControl<StackPanel>("MessageBox").IsVisible <- true
        this.FindControl<Menu>("Menu").IsVisible <- false
        this.FindControl<DockPanel>("Grid").IsVisible <- false)

  // Fill in the menu from the memory cache
  member private this.PopulateMenu() =
    let listitem = this.FindControl<MenuItem>("List")
    let items = listitem.Items.OfType<MenuItem>()

    let active =
      HandlerCommon.PopulateMenu
        items
        coverageFiles
        (fun (i: MenuItem) ->
          i.IsVisible <- false
          i.Header <- String.Empty)
        (fun name item ->
          item.IsVisible <- true
          item.Header <- name)

    listitem.IsEnabled <- active

    this.FindControl<Image>("ListImage").Source <- (if active then
                                                      icons.MRU
                                                    else
                                                      icons.MRUInactive)
      .Force()

    active

  member private this.UpdateMRU path add =
    HandlerCommon.UpdateCoverageFiles this path add
    let active = this.PopulateMenu()
    Persistence.saveCoverageFiles coverageFiles
    let menu = this.FindControl<MenuItem>("Refresh")
    menu.IsEnabled <- active

    menu.Icon <-
      (if active then
         icons.RefreshActive
       else
         icons.Refresh)
        .Force()

  member private this.HideAboutBox _ =
    this.FindControl<StackPanel>("AboutBox").IsVisible <- false
    this.FindControl<Menu>("Menu").IsVisible <- true
    this.FindControl<DockPanel>("Grid").IsVisible <- true

  member private this.UpdateTextFonts (text: TextPresenter) text2 =
    [ text; text2 ]
    |> List.iter
         (fun t ->
           let (_, logfont) =
             LogFont.TryParse(Persistence.readFont ())

           t.FontFamily <- FontFamily(logfont.faceName)
           t.FontSize <- float logfont.height
           t.FontWeight <- enum logfont.weight

           t.FontStyle <-
             match logfont.italic with
             | 0uy -> FontStyle.Normal
             | 255uy -> FontStyle.Italic
             | _ -> FontStyle.Oblique)

  member private this.PrepareDoubleTap
    (context: CoverageTreeContext<List<TreeViewItem>, TreeViewItem>)
    (xpath: XPathNavigator)
    =
    let visibleName =
      (context.Row.Header :?> StackPanel).Tag.ToString()

    let tagByCoverage (buff: TextPresenter) (lines: FormattedTextLine list) (n: CodeTag) =
      let start =
        (n.Column - 1)
        + (lines
           |> Seq.take (n.Line - 1)
           |> Seq.sumBy (fun l -> l.Length))

      // coverlet-like case w/o column data
      let ec =
        if n.LineOnly then
          lines.[n.EndLine - 1].Length
        else
          n.EndColumn

      let finish =
        (ec - 1)
        + (lines
           |> Seq.take (n.EndLine - 1)
           |> Seq.sumBy (fun l -> l.Length))

      FormattedTextStyleSpan(
        start,
        finish - start,
        match n.Style with
        | Exemption.Visited -> visited
        | Exemption.Automatic -> automatic
        | Exemption.Declared -> declared
        | Exemption.Excluded -> excluded
        | Exemption.StaticAnalysis -> staticAnalysis
        | _ -> notVisited
      )

    let markBranches
      (root: XPathNavigator)
      (stack: StackPanel)
      (lines: FormattedTextLine list)
      (filename: string)
      =
      let branches = HandlerCommon.TagBranches root filename

      let h = (lines |> Seq.head).Height
      let pad = (h - 16.0) / 2.0
      let margin = Thickness(0.0, pad)

      Dispatcher.UIThread.Post
        (fun _ ->
          stack.Children.Clear()

          for l in 1 .. lines.Length do
            let pic = new Image()

            let pix =
              HandlerCommon.IconForBranches
                icons
                branches
                l
                (fun text -> ToolTip.SetTip(pic, text))

            pic.Source <- pix
            pic.Margin <- margin
            stack.Children.Add pic)

    let markCoverage
      (root: XPathNavigator)
      (textBox: TextPresenter)
      (text2: TextPresenter)
      (lines: FormattedTextLine list)
      filename
      =
      let tags =
        HandlerCommon.TagCoverage root filename lines.Length

      let formats =
        tags |> List.map (tagByCoverage textBox lines)

      let linemark =
        tags
        |> HandlerCommon.TagLines visited notVisited
        |> List.map
             (fun (l, tag) ->
               let start =
                 (l - 1) * (7 + Environment.NewLine.Length)

               FormattedTextStyleSpan(start, 7, tag))

      (formats, linemark)

    context.Row.DoubleTapped
    |> Event.add
         (fun _ ->
           let text =
             this.FindControl<TextPresenter>("Source")

           let text2 = this.FindControl<TextPresenter>("Lines")

           let scroller =
             this.FindControl<ScrollViewer>("Coverage")

           let noSource () =
             this.DisplayMessage MessageType.Info
             <| String.Format(
               System.Globalization.CultureInfo.CurrentCulture,
               Resource.GetResourceString "No source location",
               visibleName
             )

           let showSource (info: Source) (line: int) =
             try
               this.UpdateTextFonts text text2
               text.Text <- File.ReadAllText info.FullName

               let textLines =
                 text.FormattedText.GetLines() |> Seq.toList

               text2.Text <-
                 String.Join(
                   Environment.NewLine,
                   textLines
                   // Font limitation or Avalonia limitation?
                   // character \u2442 just shows as a box.
                   |> Seq.mapi (fun i _ -> sprintf "%6d " (1 + i))
                 )

               let sample = textLines |> Seq.head
               let depth = sample.Height * float (line - 1)
               let root = xpath.Clone()
               root.MoveToRoot()

               let (formats, linemark) =
                 markCoverage root text text2 textLines info.FullName

               let stack = this.FindControl<StackPanel>("Branches")
               root.MoveToRoot()
               markBranches root stack textLines info.FullName

               async {
                 Threading.Thread.Sleep(300)

                 Dispatcher.UIThread.Post
                   (fun _ ->
                     text.FormattedText.Spans <- formats
                     text.Tag <- formats
                     text.InvalidateVisual()
                     text2.FormattedText.Spans <- linemark
                     text2.Tag <- linemark
                     text2.InvalidateVisual()

                     let midpoint = scroller.Viewport.Height / 2.0

                     if (depth > midpoint) then
                       scroller.Offset <- scroller.Offset.WithY(depth - midpoint))
               }
               |> Async.Start

             with x ->
               let caption = Resource.GetResourceString "LoadError"
               this.ShowMessageBox MessageType.Error caption x.Message

           HandlerCommon.DoRowActivation xpath this noSource showSource)

  member this.InitializeComponent() =
    AvaloniaXamlLoader.Load(this)
    Persistence.readGeometry this
    coverageFiles <- Persistence.readCoverageFiles ()
    this.PopulateMenu() |> ignore // no refresh at this point
    ofd.Directory <- Persistence.readFolder ()
    ofd.Title <- Resource.GetResourceString "Open Coverage File"
    ofd.AllowMultiple <- false

    let filterBits =
      (Resource.GetResourceString "SelectXml")
        .Split([| '|' |])
      |> Seq.map
           (fun f ->
             let entry = f.Split([| '%' |])
             let filter = FileDialogFilter()
             filter.Name <- entry |> Seq.head
             filter.Extensions <- List(entry |> Seq.tail)
             filter)

    ofd.Filters <- List(filterBits)
    this.Title <- "AltCover.Visualizer"

    let p = Environment.OSVersion.Platform |> int
    let isWindows = p <= 3

    let respondToFont font =
      font.ToString() |> Persistence.saveFont

      let text =
        this.FindControl<TextPresenter>("Source")

      let text2 = this.FindControl<TextPresenter>("Lines")
      this.UpdateTextFonts text text2

      [ text; text2 ]
      |> Seq.iter
           (fun t ->
             let tmp = t.Text
             t.Text <- String.Empty
             t.Text <- tmp

             t.FormattedText.Spans <-
               match t.Tag with
               | :? (list<FormattedTextStyleSpan>) as l -> l
               | _ -> [])

      let h =
        (text.FormattedText.GetLines() |> Seq.head).Height

      let pad = (h - 16.0) / 2.0
      let margin = Thickness(0.0, pad)

      this.FindControl<StackPanel>("Branches").Children
      |> Seq.cast<Image>
      |> Seq.iter (fun pic -> pic.Margin <- margin)

    let fontItem = this.FindControl<MenuItem>("Font")

    if isWindows then
      fontItem.IsVisible <- true

      fontItem.Click
      |> Event.add
           (fun _ ->
             let hwnd = this.PlatformImpl.Handle.Handle

             Fonts.SelectWin32(Persistence.readFont (), hwnd)
             |> Option.ofObj
             |> Option.iter respondToFont)
    else if Fonts.Wish().Any() then
      fontItem.IsVisible <- true

      fontItem.Click
      |> Event.add
           (fun _ ->
             Persistence.readFont ()
             |> Fonts.SelectWish
             |> Option.ofObj
             |> Option.iter respondToFont)

    [ "open"
      "refresh"
      "font"
      "showAbout"
      "exit" ]
    |> Seq.iter
         (fun n ->
           let cap =
             n.First().ToString().ToUpper() + n.Substring(1)

           let raw =
             Resource.GetResourceString(n + "Button.Label")

           let keytext = raw.Split('|')

           let menu = this.FindControl<MenuItem> cap
           // Why is this Shift inverted?  This turns into Alt+key.  TODO raise issue
           let hotkey =
             Avalonia.Input.KeyGesture.Parse("Alt+Shift+" + keytext.[0])

           menu.HotKey <- hotkey

           let item =
             this.FindControl<Primitives.AccessText>(cap + "Text")

           item.Text <- keytext.[1]

           )

    this.FindControl<MenuItem>("Exit").Click
    |> Event.add
         (fun _ ->
           if Persistence.save then
             Persistence.saveGeometry this

           let l =
             Avalonia.Application.Current.ApplicationLifetime
             :?> Avalonia.Controls.ApplicationLifetimes.IClassicDesktopStyleApplicationLifetime

           l.Shutdown())

    this.FindControl<MenuItem>("ShowAbout").Click
    |> Event.add
         (fun _ ->
           this.FindControl<StackPanel>("AboutBox").IsVisible <- true
           this.FindControl<Menu>("Menu").IsVisible <- false
           this.FindControl<DockPanel>("Grid").IsVisible <- false)

    let openFile = new Event<String option>()

    this.FindControl<MenuItem>("Open").Click
    |> Event.add
         (fun _ ->
           this.HideAboutBox()

           async {
             (ofd.ShowAsync(this)
              |> Async.AwaitTask
              |> Async.RunSynchronously)
             |> Option.ofObj
             |> Option.map (fun x -> x.FirstOrDefault() |> Option.ofObj)
             |> Option.iter openFile.Trigger
           }
           |> Async.Start)

    let click =
      openFile.Publish
      |> Event.choose id
      |> Event.map
           (fun n ->
             ofd.Directory <- Path.GetDirectoryName n

             if Persistence.save then
               Persistence.saveFolder ofd.Directory

             justOpened <- n
             -1)

    let select =
      this
        .FindControl<MenuItem>("List")
        .Items.OfType<MenuItem>()
      |> Seq.mapi (fun n (i: MenuItem) -> i.Click |> Event.map (fun _ -> n))
    // The sum of all these events -- we have explicitly selected a file
    let fileSelection = select |> Seq.fold Event.merge click

    let refresh =
      this.FindControl<MenuItem>("Refresh").Click
      |> Event.map (fun _ -> 0)

    let makeNewRow name (anIcon: Lazy<Bitmap>) =
      let row = TreeViewItem()
      row.HorizontalAlignment <- Avalonia.Layout.HorizontalAlignment.Left

      row.LayoutUpdated
      |> Event.add
           (fun _ ->
             let remargin (t: TreeViewItem) =
               if t.HeaderPresenter.IsNotNull then
                 let hp =
                   t.HeaderPresenter :?> Avalonia.Controls.Presenters.ContentPresenter

                 let grid = hp.Parent :?> Grid
                 grid.Margin <- Thickness(float t.Level * 4.0, 0.0, 0.0, 0.0)

             remargin row

             row.Items.OfType<TreeViewItem>()
             |> Seq.iter remargin)

      row.Tapped
      |> Event.add
           (fun evt ->
             row.IsExpanded <- not row.IsExpanded
             let items = (row.Header :?> StackPanel).Children
             items.RemoveAt(0)
             let mark = Image()

             mark.Source <-
               if row.Items.OfType<Object>().Any() then
                 if row.IsExpanded then
                   icons.TreeCollapse.Force()
                 else
                   icons.TreeExpand.Force()
               else
                 icons.Blank.Force()

             mark.Margin <- Thickness.Parse("2")
             items.Insert(0, mark)
             evt.Handled <- true)

      row.Items <- List<TreeViewItem>()
      row.Header <- makeTreeNode name <| anIcon.Force()
      row

    select
    |> Seq.fold Event.merge refresh
    |> Event.add this.HideAboutBox

    Event.merge fileSelection refresh
    |> Event.add
         (fun index ->
           let mutable auxModel =
             { Model = List<TreeViewItem>()
               Row = null }

           let environment =
             { Icons = icons
               GetFileInfo =
                 fun i ->
                   FileInfo(
                     if i < 0 then
                       justOpened
                     else
                       coverageFiles.[i]
                   )
               Display = this.DisplayMessage
               UpdateMRUFailure = fun info -> this.UpdateMRU info.FullName false
               UpdateUISuccess =
                 fun info ->
                   let tree = this.FindControl<TreeView>("Tree")
                   this.Title <- "AltCover.Visualizer"

                   tree.Items.OfType<IDisposable>()
                   |> Seq.iter (fun x -> x.Dispose())

                   let t1 =
                     this.FindControl<TextPresenter>("Source")

                   let t2 = this.FindControl<TextPresenter>("Lines")

                   [ t1; t2 ]
                   |> Seq.iter
                        (fun t ->
                          t.Text <- String.Empty
                          t.FormattedText.Spans <- []
                          t.Tag <- t.FormattedText.Spans)

                   this
                     .FindControl<StackPanel>("Branches")
                     .Children.Clear()

                   tree.Items <- auxModel.Model
                   this.UpdateMRU info.FullName true
               SetXmlNode =
                 fun name ->
                   let model = auxModel.Model

                   { Model = model
                     Row =
                       let row = makeNewRow name icons.Xml
                       model.Add row
                       row }
               AddNode =
                 fun context icon name ->
                   { context with
                       Row =
                         let row = makeNewRow name icon
                         (context.Row.Items :?> List<TreeViewItem>).Add row
                         row }
               Map = this.PrepareDoubleTap }

           Dispatcher.UIThread.Post
             (fun _ -> CoverageFileTree.DoSelected environment index))
    this.FindControl<TextBlock>("Program").Text <- "AltCover.Visualizer "
                                                   + AssemblyVersionInformation.AssemblyFileVersion

    this.FindControl<TextBlock>("Description").Text <- Resource.GetResourceString
                                                         "aboutVisualizer.Comments"

    let copyright =
      AssemblyVersionInformation.AssemblyCopyright

    this.FindControl<TextBlock>("Copyright").Text <- String.Format(
      CultureInfo.InvariantCulture,
      Resource.GetResourceString "aboutVisualizer.Copyright",
      copyright
    )

    let link = this.FindControl<TextBlock>("Link")
    link.Text <- Resource.GetResourceString "aboutVisualizer.WebsiteLabel"
    let linkButton = this.FindControl<Button>("LinkButton")

    linkButton.Click
    |> Event.add
         (fun _ ->
           Avalonia.Dialogs.AboutAvaloniaDialog.OpenBrowser
             "http://www.github.com/SteveGilham/altcover")
    this.FindControl<TabItem>("AboutDetails").Header <- Resource.GetResourceString
                                                          "AboutDialog.About"

    this.FindControl<TabItem>("License").Header <- Resource.GetResourceString
                                                     "AboutDialog.License"

    this.FindControl<TextBlock>("MIT").Text <- String.Format(
      CultureInfo.InvariantCulture,
      Resource.GetResourceString "License",
      copyright
    )

    this.Closing
    |> Event.add
         (fun e ->
           if this.FindControl<DockPanel>("Grid").IsVisible
              |> not then
             this.FindControl<StackPanel>("AboutBox").IsVisible <- false
             this.FindControl<StackPanel>("MessageBox").IsVisible <- false
             this.FindControl<Menu>("Menu").IsVisible <- true
             this.FindControl<DockPanel>("Grid").IsVisible <- true
             e.Cancel <- true)

    // MessageBox
    let okButton =
      this.FindControl<Button>("DismissMessageBox")

    okButton.Content <- "OK"

    okButton.Click
    |> Event.add
         (fun _ ->
           this.FindControl<StackPanel>("MessageBox").IsVisible <- false
           this.FindControl<Menu>("Menu").IsVisible <- true
           this.FindControl<DockPanel>("Grid").IsVisible <- true)

    // AboutBox
    let okButton2 =
      this.FindControl<Button>("DismissAboutBox")

    okButton2.Content <- "OK"

    okButton2.Click
    |> Event.add
         (fun _ ->
           this.FindControl<StackPanel>("AboutBox").IsVisible <- false
           this.FindControl<Menu>("Menu").IsVisible <- true
           this.FindControl<DockPanel>("Grid").IsVisible <- true)

  interface IVisualizerWindow with
    member self.CoverageFiles
      with get () = coverageFiles
      and set (value) = coverageFiles <- value

    member self.Title
      with get () = self.Title
      and set (value) = self.Title <- value

    member self.ShowMessageOnGuiThread mtype message = self.DisplayMessage mtype message