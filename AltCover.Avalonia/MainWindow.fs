namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Linq
open System.Xml.XPath

open GuiCommon

open AltCover.FontSupport
open Avalonia.Controls
#if AVALONIA11
#else
open Avalonia.Controls.Presenters
#endif
open Avalonia.Markup.Xaml
open Avalonia.Media
open Avalonia.Media.Imaging
open Avalonia.Threading

type Thickness = Avalonia.Thickness

[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidUnneededFieldInitializationRule",
                            Scope = "member",
                            Target = "AltCover.MainWindow::.ctor()",
                            Justification = "Initialization specifies type")>]
()

type AboutBox() as this =
  inherit Window()
  do this.InitializeComponent()

  member this.InitializeComponent() =
    AvaloniaXamlLoader.Load(this)
    this.CanResize <- false

    this.Title <- Resource.GetResourceString "aboutVisualizer.Title"

    this.FindControl<TextBlock>("Program").Text <-
      "AltCover.Visualizer "
      + AssemblyVersionInformation.AssemblyPackageVersion

    this.FindControl<TextBlock>("Description").Text <-
      String.Format(
        Globalization.CultureInfo.CurrentCulture,
        Resource.GetResourceString("aboutVisualizer.Comments"),
#if AVALONIA11
        "AvaloniaUI v11"
#else
        "AvaloniaUI"
#endif
      )

    let copyright =
      AssemblyVersionInformation.AssemblyCopyright

    this.FindControl<TextBlock>("Copyright").Text <- copyright

    let vslink =
      this.FindControl<TextBlock>("VSLink")

    vslink.Text <- Resource.GetResourceString "aboutVisualizer.Copyright"

#if AVALONIA11
#else
    let vsLinkButton =
      this.FindControl<Button>("VSLinkButton")

    vsLinkButton.Click
    |> Event.add (fun _ ->
      Avalonia.Dialogs.AboutAvaloniaDialog.OpenBrowser
        "https://learn.microsoft.com/en-us/visualstudio/designers/the-visual-studio-image-library?view=vs-2022")
#endif

    let link =
      this.FindControl<TextBlock>("Link")

    link.Text <- Resource.GetResourceString "aboutVisualizer.WebsiteLabel"

#if AVALONIA11
#else
    let linkButton =
      this.FindControl<Button>("LinkButton")

    linkButton.Click
    |> Event.add (fun _ ->
      Avalonia.Dialogs.AboutAvaloniaDialog.OpenBrowser
        "http://www.github.com/SteveGilham/altcover")
#endif

    this.FindControl<TabItem>("AboutDetails").Header <-
      Resource.GetResourceString "AboutDialog.About"

    this.FindControl<TabItem>("License").Header <-
      Resource.GetResourceString "AboutDialog.License"

    this.FindControl<TextBlock>("MIT").Text <-
      String.Format(
        CultureInfo.InvariantCulture,
        Resource.GetResourceString "License",
        copyright
      )

    // AboutBox
    let aboutClose =
      this.FindControl<Button>("DismissAboutBox")

    aboutClose.Click
    |> Event.add (fun _ -> this.Close())

    aboutClose.Content <- "OK"

#if AVALONIA11
type TextLine =
  { Start: int
    End: int
    Brush: (SolidColorBrush * SolidColorBrush) option }
#endif

type MainWindow() as this =
  inherit Window()
  let mutable armed = false
  let mutable justOpened = String.Empty

  [<NonSerialized>]
  let mutable coverageFiles: string list = []

  [<NonSerialized>]
#if AVALONIA11
  let ofd =
    Avalonia.Platform.Storage.FilePickerOpenOptions()
#else
  let ofd = OpenFileDialog()
#endif

  // fsharplint:disable-next-line  RedundantNewKeyword
  let iconMaker (x: Stream) = new Bitmap(x)

  [<NonSerialized>]
  let icons = Icons(iconMaker)

  let getIcon name =
    let iconstype = icons.GetType()
    let named = iconstype.GetProperty(name)

    let value =
      named.GetValue(icons) :?> Lazy<Bitmap>

    value.Force()

  [<NonSerialized>]
  let visited =
    SolidColorBrush.Parse "#0000CD" // "#cefdce" // Medium Blue on Pale Green

  [<NonSerialized>]
  let visitedBG =
    SolidColorBrush.Parse "#cefdce" // Medium Blue on Pale Green

  [<NonSerialized>]
  let declared =
    SolidColorBrush.Parse "#FF8C00" // "#FFFFFF" // Dark Orange on White

  [<NonSerialized>]
  let declaredBG =
    SolidColorBrush.Parse "#FFFFFF" // Dark Orange on White

  [<NonSerialized>]
  let staticAnalysis =
    SolidColorBrush.Parse "#F5F5F5" // "#000000" // White Smoke on Black

  [<NonSerialized>]
  let staticAnalysisBG =
    SolidColorBrush.Parse "#000000" // White Smoke on Black

  [<NonSerialized>]
  let automatic =
    SolidColorBrush.Parse "#808080" // "#FFD700"// Grey on Gold

  [<NonSerialized>]
  let automaticBG =
    SolidColorBrush.Parse "#FFD700" // Grey on Gold

  [<NonSerialized>]
  let notVisited =
    SolidColorBrush.Parse "#DC143C" // "#FFFFFF" // Crimson on White

  [<NonSerialized>]
  let notVisitedBG =
    SolidColorBrush.Parse "#FFFFFF" // Crimson on White

  [<NonSerialized>]
  let excluded =
    SolidColorBrush.Parse "#87CEEB" // "#F5F5F5" // Sky Blue on White Smoke

  [<NonSerialized>]
  let excludedBG =
    SolidColorBrush.Parse "#F5F5F5" // Sky Blue on White Smoke

#if AVALONIA11
  [<SuppressMessage("Gendarme.Rules.Portability",
                    "NewLineLiteralRule",
                    Justification = "That is kind of the point...")>]
  [<TailCall>]
  let rec linesOfString (s: string) (start: int) (lines: TextLine list) =
    let rn = s.IndexOf("\r\n", start)

    if rn >= 0 then
      linesOfString
        s
        (rn + 2)
        ({ Start = start
           End = rn + 2
           Brush = None }
         :: lines)
    else
      let r = s.IndexOf('\r', start)

      if r >= 0 then
        linesOfString
          s
          (r + 1)
          ({ Start = start
             End = r + 1
             Brush = None }
           :: lines)
      else
        let n = s.IndexOf('\n', start)

        if n >= 0 then
          linesOfString
            s
            (n + 1)
            ({ Start = start
               End = n + 1
               Brush = None }
             :: lines)
        else
          ({ Start = start
             End = s.Length + 1
             Brush = None }
           :: lines)
          |> List.rev
#endif

  let makeTreeNode pc leaf name icon =
    let text = TextBlock()
    text.Text <- name
    text.Margin <- Thickness.Parse("2")

    let note = TextBlock()
    note.Text <- pc
    note.HorizontalAlignment <- Avalonia.Layout.HorizontalAlignment.Right
    note.VerticalAlignment <- Avalonia.Layout.VerticalAlignment.Bottom
    text.VerticalAlignment <- Avalonia.Layout.VerticalAlignment.Bottom
    note.Margin <- Thickness.Parse("2")

    let logfont =
      LogFont.TryParse(Persistence.readFont ()) |> snd

    note.FontFamily <- FontFamily(logfont.faceName)

    let image = Image()
    image.Source <- icon
    image.Margin <- Thickness.Parse("2")
    let display = StackPanel()
    display.Orientation <- Avalonia.Layout.Orientation.Horizontal
    display.Children.Add image
    display.Children.Add note
    display.Children.Add text
    display.Tag <- name
    display

  do
    this.InitializeComponent()
    this.Show()

  [<SuppressMessage("Gendarme.Rules.Smells",
                    "AvoidSwitchStatementsRule",
                    Justification = "This is FP, not OO")>]
  member private this.DisplayMessage (status: MessageType) message =
    let caption =
      match status with
      | MessageType.Info -> Resource.GetResourceString "LoadInfo"
      | MessageType.Warning -> Resource.GetResourceString "LoadWarning"
      | _ -> Resource.GetResourceString "LoadError"

    this.ShowMessageBox status caption message

  [<SuppressMessage("Gendarme.Rules.Smells",
                    "AvoidSwitchStatementsRule",
                    Justification = "This is FP, not OO")>]
  member private this.ShowMessageBox (status: MessageType) caption message =
#if AVALONIA11
    ()
#else
    let dlg =
      MessageBox.Avalonia.DTO.MessageBoxCustomParamsWithImage()

    dlg.WindowIcon <- WindowIcon(icons.VIcon)

    dlg.Icon <-
      (match status with
       | MessageType.Info -> icons.Info
       | MessageType.Warning -> icons.Warn
       | _ -> icons.Error)
        .Force()

    dlg.ContentMessage <- message
    dlg.ContentTitle <- caption

    let ok =
      MessageBox.Avalonia.Models.ButtonDefinition()

    ok.Name <- "OK"
    dlg.ButtonDefinitions <- [ ok ]
    dlg.WindowStartupLocation <- WindowStartupLocation.CenterOwner
    dlg.SizeToContent <- SizeToContent.Height

    let mbox =
      MessageBox.Avalonia.MessageBoxManager.GetMessageBoxCustomWindow(dlg)

    // can we get this to tidy up after itself??
    mbox.ShowDialog(this) |> ignore
#endif

  // Fill in the menu from the memory cache
  member private this.PopulateMenu() =
    let listitem =
      this.FindControl<MenuItem>("List")

    let items =
      listitem.Items.OfType<MenuItem>()

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

    this.FindControl<Image>("ListImage").Source <-
      (if active then
         icons.MRU
       else
         icons.MRUInactive)
        .Force()

    active

  member private this.UpdateMRU path add =
    HandlerCommon.UpdateCoverageFiles this path add
    let active = this.PopulateMenu()
    Persistence.saveCoverageFiles coverageFiles

    let menu =
      this.FindControl<MenuItem>("Refresh")

    menu.IsEnabled <- active

    menu.Icon <-
      (if active then
         icons.RefreshActive
       else
         icons.Refresh)
        .Force()

#if AVALONIA11
  member private this.UpdateTextFonts (text: TextBlock) text2 =
#else
  member private this.UpdateTextFonts (text: TextPresenter) text2 =
#endif
    [ text; text2 ]
    |> List.iter (fun t ->
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

  [<SuppressMessage("Gendarme.Rules.Exceptions",
                    "DoNotSwallowErrorsCatchingNonSpecificExceptionsRule",
                    Justification = "Problem is displayed to the user")>]
  member private this.CatchAllAndWarn f =
    try
      f ()
    with x ->
      let caption =
        Resource.GetResourceString "LoadError"

      this.ShowMessageBox MessageType.Error caption x.Message

  member private this.PrepareDoubleTap
    (context: CoverageTreeContext<List<TreeViewItem>, TreeViewItem>)
    (xpath: XPathNavigator)
    =
    let visibleName =
      (context.Row.Header :?> StackPanel).Tag.ToString()

#if AVALONIA11
    let tagByCoverage (buff: TextBlock) (lines: TextLine list) (n: CodeTag) =
#else
    let tagByCoverage (buff: TextPresenter) (lines: FormattedTextLine list) (n: CodeTag) =
#endif
      // rubber hits the road -- TODO
#if !AVALONIA11
      let start =
        (n.Column - 1)
        + (lines
           |> Seq.take (n.Line - 1)
           |> Seq.sumBy _.Length)

      let ec =
        if n.LineOnly then
          // coverlet-like case w/o column data
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
#else
      let start =
        (n.Column - 1) + lines.[n.Line - 1].Start

      let finishLine = lines.[n.EndLine - 1]

      let finish =
        if n.LineOnly then
          // coverlet-like case w/o column data
          finishLine.End - 1
        else
          finishLine.Start + n.EndColumn - 1

      ()
#endif

    let markBranches
#if AVALONIA11
      (lineHeight: float)
#endif
      (root: XPathNavigator)
      (stack: StackPanel)
#if AVALONIA11
      (lines: TextLine list)
#else
      (lines: FormattedTextLine list)
#endif
      (file: Source)
      =
      let branches =
        HandlerCommon.TagBranches root file

#if AVALONIA11
      let pad = (lineHeight - 16.0) / 2.0
#else
      let h = (lines |> Seq.head).Height
      let pad = (h - 16.0) / 2.0
#endif
      let margin = Thickness(0.0, pad)

      Dispatcher.UIThread.Post(fun _ ->
        stack.Children.Clear()

        for l in 1 .. lines.Length do
          let pic = Image()

          let pix =
            HandlerCommon.IconForBranches icons branches l (fun text ->
              ToolTip.SetTip(pic, text))

          pic.Source <- pix
          pic.Margin <- margin
          stack.Children.Add pic)

    let markCoverage
      (root: XPathNavigator)
#if AVALONIA11
      (textBox: TextBlock)
      (text2: TextBlock)
      (lines: TextLine list)
#else
      (textBox: TextPresenter)
      (text2: TextPresenter)
      (lines: FormattedTextLine list)
#endif
      info
      =
      let tags =
        HandlerCommon.TagCoverage root info lines.Length

      let formats =
        tags |> List.map (tagByCoverage textBox lines)

      let linemark =
        tags
#if AVALONIA11
        |> HandlerCommon.TagLines (visited, visitedBG) (notVisited, notVisitedBG)
#else
        |> HandlerCommon.TagLines visited notVisited
#endif
        |> List.map (fun (l, tag) ->
          let start =
            (l - 1) * (7 + Environment.NewLine.Length)

          // mark the line number column
#if !AVALONIA11
          FormattedTextStyleSpan(start, 7, tag))
#else
          { Start = start
            End = start + 7
            Brush = Some tag })
#endif

      (formats, linemark)

    context.Row.DoubleTapped
    |> Event.add (fun _ ->
      let text =
#if AVALONIA11
        this.FindControl<TextBlock>("Source")
#else
        this.FindControl<TextPresenter>("Source")
#endif

      let text2 =
#if AVALONIA11
        this.FindControl<TextBlock>("Lines")
#else
        this.FindControl<TextPresenter>("Lines")
#endif

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
        this.CatchAllAndWarn(fun () ->
          this.UpdateTextFonts text text2
          text.Text <- info.ReadAllText().Replace('\t', '\u2192')

          // get line count
#if !AVALONIA11
          let textLines =
            text.FormattedText.GetLines() |> Seq.toList
#else
          let t =
            Typeface(text.FontFamily, text.FontStyle, text.FontWeight, text.FontStretch)

          let format =
            FormattedText(
              text.Text,
              CultureInfo.InvariantCulture,
              FlowDirection.LeftToRight,
              t,
              text.FontSize,
              this.Foreground
            )

          let textLines = linesOfString text.Text 0 []
#endif

#if !AVALONIA11
          text2.Text <-
            String.Join(
              Environment.NewLine,
              textLines
              |> Seq.mapi (fun i _ -> sprintf "%6d " (1 + i))
            )
            + Environment.NewLine

          let sample = textLines |> Seq.head
          let depth = sample.Height * float (line - 1)

          //// get line height
          //let h = (textLines |> Seq.head).Height
          //let depth = h * float (line - 1)
#else

          let numbers =
            textLines
            |> Seq.mapi (fun i _ -> sprintf "%6d " (1 + i))

          numbers
          |> Seq.iter (fun l ->
            let line = l + Environment.NewLine

            let run =
              Avalonia.Controls.Documents.Run(line)

            text2.Inlines.Add run)

          let h =
            format.Height
            / (textLines |> List.length |> float)

          let depth = h * float (line - 1)
#endif
          let root = xpath.Clone()
          root.MoveToRoot()

          let (formats, linemark) =
            markCoverage root text text2 textLines info

          let stack =
            this.FindControl<StackPanel>("Branches")

          root.MoveToRoot()
#if AVALONIA11
          markBranches h root stack textLines info
#else
          markBranches root stack textLines info
#endif

          async {
            Threading.Thread.Sleep(300)

            Dispatcher.UIThread.Post(fun _ ->
              // apply formatting on UI thread -- TODO
#if !AVALONIA11
              text.FormattedText.Spans <- formats
#endif
              text.Tag <- formats
              text.InvalidateVisual()

#if !AVALONIA11
              text2.FormattedText.Spans <- linemark
#else
              linemark
              |> Seq.iter (fun mark ->
                let index =
                  mark.Start / (7 + Environment.NewLine.Length)

                let span = text2.Inlines.[index]

                match mark.Brush with
                | None -> ()
                | Some(f, b) ->
                  span.Foreground <- f
                  span.Background <- b)
#endif
              text2.Tag <- linemark
              text2.InvalidateVisual()

              let midpoint =
                scroller.Viewport.Height / 2.0

              if (depth > midpoint) then
                scroller.Offset <- scroller.Offset.WithY(depth - midpoint))
          }
          |> Async.Start)

      HandlerCommon.DoRowActivation xpath this noSource showSource)

  [<SuppressMessage("Gendarme.Rules.Smells",
                    "AvoidLongMethodsRule",
                    Justification = "work to be done here")>]
  member this.InitializeComponent() =
    AvaloniaXamlLoader.Load(this)
    Persistence.readGeometry this
    coverageFiles <- Persistence.readCoverageFiles ()
    this.PopulateMenu() |> ignore // no refresh at this point
#if !AVALONIA11
    ofd.Directory <- Persistence.readFolder ()
#else
    let where = Persistence.readFolder ()

    //if Directory.Exists where then
    //  ofd.SuggestedStartLocation <-
    //    new Avalonia.Platform.Storage.FileIO.BclStorageFolder(where)
#endif

    ofd.Title <- Resource.GetResourceString "Open Coverage File"
    ofd.AllowMultiple <- false

    let filterBits =
      (Resource.GetResourceString "SelectXml").Split([| '|' |])
      |> Seq.map (fun f ->
        let entry = f.Split([| '%' |])
#if !AVALONIA11
        let filter = FileDialogFilter()
        filter.Name <- entry |> Seq.head
        filter.Extensions <- List(entry |> Seq.tail)
#else
        let filter =
          Avalonia.Platform.Storage.FilePickerFileType(entry |> Seq.head)

        filter.Patterns <- List(entry |> Seq.tail |> Seq.map (fun x -> "*." + x))
#endif
        filter)

#if AVALONIA11
    ofd.FileTypeFilter <- List(filterBits)
#else
    ofd.Filters <- List(filterBits)
#endif
    this.Title <- "AltCover.Visualizer"

    let p =
      Environment.OSVersion.Platform |> int

    let isWindows = p <= 3

    let respondToFont font =
      font.ToString() |> Persistence.saveFont

      let text =
#if AVALONIA11
        this.FindControl<TextBlock>("Source")
#else
        this.FindControl<TextPresenter>("Source")
#endif

      let text2 =
#if AVALONIA11
        this.FindControl<TextBlock>("Lines")
#else
        this.FindControl<TextPresenter>("Lines")
#endif

      this.UpdateTextFonts text text2

      [ text; text2 ]
      |> Seq.iter (fun t ->
        let tmp = t.Text
        t.Text <- String.Empty
        t.Text <- tmp

      // reapply saved formatting on UI thread --TODO
#if !AVALONIA11

        t.FormattedText.Spans <-
          match t.Tag with
          | :? (list<FormattedTextStyleSpan>) as l -> l
          | _ -> []
#endif
      )

      // recompute line height for new font
#if !AVALONIA11
      let h =
        (text.FormattedText.GetLines() |> Seq.head).Height
#else
      let tf =
        Typeface(text.FontFamily, text.FontStyle, text.FontWeight, text.FontStretch)

      let format =
        FormattedText(
          text.Text,
          CultureInfo.InvariantCulture,
          FlowDirection.LeftToRight,
          tf,
          text.FontSize,
          this.Foreground
        )

      let textLines = linesOfString text.Text 0 []

      // first guess
      let h =
        format.Height
        / (textLines |> List.length |> float)
#endif

      let pad = (h - 16.0) / 2.0
      let margin = Thickness(0.0, pad)

      this.FindControl<StackPanel>("Branches").Children
      |> Seq.cast<Image>
      |> Seq.iter (fun pic -> pic.Margin <- margin)

    let fontItem =
      this.FindControl<MenuItem>("Font")

    if isWindows then
      fontItem.IsVisible <- true

#if AVALONIA11
#else
      fontItem.Click
      |> Event.add (fun _ ->

        let hwnd = this.PlatformImpl.Handle.Handle

        Fonts.SelectWin32(Persistence.readFont (), hwnd)
        |> Option.ofObj
        |> Option.iter respondToFont)
#endif
    else if Fonts.Wish().Any() then
      fontItem.IsVisible <- true

      fontItem.Click
      |> Event.add (fun _ ->
        Persistence.readFont ()
        |> Fonts.SelectWish
        |> Option.ofObj
        |> Option.iter respondToFont)

    [ "open"
      "refresh"
      "font"
      "showAbout"
      "exit" ]
    |> Seq.iter (fun n ->
      let cap =
        n.First().ToString().ToUpper(CultureInfo.InvariantCulture)
        + n.Substring(1)

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
    |> Event.add (fun _ ->
      if Persistence.save then
        Persistence.saveGeometry this

      let l =
        Avalonia.Application.Current.ApplicationLifetime
        :?> Avalonia.Controls.ApplicationLifetimes.IClassicDesktopStyleApplicationLifetime

      l.Shutdown())

    this.FindControl<MenuItem>("ShowAbout").Click
    |> Event.add (fun _ -> AboutBox().ShowDialog(this) |> ignore)

#if AVALONIA11
    let openFile =
      Event<Avalonia.Platform.Storage.IStorageFile option>()
#else
    let openFile = Event<String option>()
#endif

    this.FindControl<MenuItem>("Open").Click
    |> Event.add (fun _ ->
      async {
#if AVALONIA11
        (use s =
          this.StorageProvider.OpenFilePickerAsync(ofd)
#else
        (use s = ofd.ShowAsync(this)
#endif

         s |> Async.AwaitTask |> Async.RunSynchronously)
        |> Option.ofObj
        |> Option.map (_.FirstOrDefault() >> Option.ofObj)
        |> Option.iter openFile.Trigger
      }
      |> Async.Start)

    let click =
      openFile.Publish
      |> Event.choose id
      |> Event.map (fun n ->
#if AVALONIA11
        //use temp = ofd.SuggestedStartLocation
        //let ok, where = n.TryGetUri()
        //let path = where.AbsolutePath

        //if ok then
        //  ofd.SuggestedStartLocation <-
        //    new Avalonia.Platform.Storage.FileIO.BclStorageFolder(
        //      Path.GetDirectoryName path)
#else
        ofd.Directory <- Path.GetDirectoryName n
#endif

        if Persistence.save then
#if AVALONIA11
          //let ok, where =
          //  ofd.SuggestedStartLocation.TryGetUri()

          //if ok then
          //  Persistence.saveFolder where.AbsolutePath
          ()
#else
          Persistence.saveFolder ofd.Directory
#endif

#if AVALONIA11
        //justOpened <- path
#else
        justOpened <- n
#endif
        -1)

    let select =
      this.FindControl<MenuItem>("List").Items.OfType<MenuItem>()
      |> Seq.mapi (fun n (i: MenuItem) -> i.Click |> Event.map (fun _ -> n))
    // The sum of all these events -- we have explicitly selected a file
    let fileSelection =
      select |> Seq.fold Event.merge click

    let refresh =
      this.FindControl<MenuItem>("Refresh").Click
      |> Event.map (fun _ -> 0)

    let makeNewRow note leaf name (anIcon: Lazy<Bitmap>) =
      let row = TreeViewItem()
      let l = List<TreeViewItem>()

      if not leaf then
        let dummy = TreeViewItem()

        // duplicated
        let image = Image()
        image.Source <- icons.Progress.Force()
        image.Margin <- Thickness.Parse("2")
        let display = StackPanel()
        display.Orientation <- Avalonia.Layout.Orientation.Horizontal
        display.Children.Add image

        dummy.Header <- display
        l.Add dummy
        row.Tag <- New
      else
        row.Tag <- Expanded

#if AVALONIA11
      //"AvaloniaUI v11"
#else
      row.Items <- l
#endif

      row.Tapped
      |> Event.add (fun x ->
        match row.Tag :?> CoverageRowState with
        | New ->
          row.Tag <- Expanded
          l.RemoveAt(0)
        | Unexpanded a ->
          row.Tag <- Expanded
          a ()
          l.RemoveAt(0)
        | _ -> ())

      row.HorizontalAlignment <- Avalonia.Layout.HorizontalAlignment.Left

      row.LayoutUpdated
      |> Event.add (fun _ ->
        let remargin (t: TreeViewItem) =
          if t.HeaderPresenter.IsNotNull then
            let hp =
#if AVALONIA11
              t.HeaderPresenter
#else
              t.HeaderPresenter :?> Avalonia.Controls.Presenters.ContentPresenter
#endif

            let grid = hp.Parent :?> Grid
            grid.Margin <- Thickness(float t.Level * 4.0, 0.0, 0.0, 0.0)

        remargin row

        row.Items.OfType<TreeViewItem>()
        |> Seq.iter remargin)

      row.Header <- makeTreeNode note leaf name <| anIcon.Force()
      row

    Event.merge fileSelection refresh
    |> Event.add (fun index ->
      let mutable auxModel =
        { Model = List<TreeViewItem>()
          Row = nullObject }

      let addNode =
        fun
            leaf
            (context: CoverageTreeContext<List<TreeViewItem>, TreeViewItem>)
            icon
            pc
            name
            (tip: string option) ->
          let newrow = makeNewRow pc leaf name icon

#if AVALONIA11

#else
          (context.Row.Items :?> List<TreeViewItem>).Add newrow
#endif
          tip
          |> Option.iter (fun text -> ToolTip.SetTip(newrow, text))

          { context with Row = newrow }

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
          UpdateMRUFailure =
            fun info ->
              Dispatcher.UIThread.Post(fun _ ->
                let tree =
                  this.FindControl<TreeView>("Tree")

#if AVALONIA11

#else
                tree.Items <- Enumerable.Empty<TreeViewItem>()
#endif
                tree.InvalidateVisual()
                this.UpdateMRU info.FullName false)
          UpdateUISuccess =
            fun info ->
              let tree =
                this.FindControl<TreeView>("Tree")

              this.Title <- "AltCover.Visualizer"

              tree.Items.OfType<IDisposable>()
              |> Seq.iter _.Dispose()

              let t1 =
#if AVALONIA11
                this.FindControl<TextBlock>("Source")
#else
                this.FindControl<TextPresenter>("Source")
#endif

              let t2 =
#if AVALONIA11
                this.FindControl<TextBlock>("Lines")
#else
                this.FindControl<TextPresenter>("Lines")
#endif

              [ t1; t2 ]
              |> Seq.iter (fun t ->
                t.Text <- String.Empty
                // clear format stashes -- TODO
#if !AVALONIA11
                let ft = t.FormattedText
                ft.Spans <- []
                t.Tag <- ft.Spans
#endif
              )

              this.FindControl<StackPanel>("Branches").Children.Clear()

#if AVALONIA11

#else
              tree.Items <- auxModel.Model
#endif
              tree.InvalidateVisual()

              if info.IsNotNull then
                this.UpdateMRU info.FullName true
          SetXmlNode =
            fun pc name icon tip ->
              let tree =
                this.FindControl<TreeView>("Tree")

#if AVALONIA11

#else
              tree.Items <- Enumerable.Empty<TreeViewItem>()
#endif
              tree.InvalidateVisual()

              let model = auxModel.Model
              model.Clear()
              let row = makeNewRow pc false name icon
              model.Add row

              if tip |> String.IsNullOrWhiteSpace |> not then
                ToolTip.SetTip(row, tip)

              { Model = model; Row = row }
          TreeUIDispatch = Dispatcher.UIThread.Post
          AddNode = (addNode false)
          AddLeafNode = (addNode true)
          OnRowExpanded =
            (fun (row: TreeViewItem) (action: unit -> unit) ->
              row.Tag <- Unexpanded action)
          Map = this.PrepareDoubleTap }

      async { CoverageFileTree.DoSelected environment index }
      |> Async.Start)

  interface IVisualizerWindow with
    member self.CoverageFiles
      with get () = coverageFiles
      and set (value) = coverageFiles <- value

    member self.Title
      with get () = self.Title
      and set (value) = self.Title <- value

    member self.ShowMessageOnGuiThread mtype message = self.DisplayMessage mtype message

#if AVALONIA11
[<assembly: SuppressMessage("Gendarme.Rules.Smells",
                            "AvoidLargeClassesRule",
                            Scope = "type", // TypeDefinition
                            Target = "AltCover.MainWindow",
                            Justification = "God Object, alas")>]
#endif
[<assembly: SuppressMessage("Gendarme.Rules.Smells",
                            "AvoidSwitchStatementsRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "AltCover.MainWindow/tagByCoverage@423::Invoke(Avalonia.Controls.Presenters.TextPresenter,Microsoft.FSharp.Collections.FSharpList`1<Avalonia.Media.FormattedTextLine>,AltCover.GuiCommon/CodeTag)",
                            Justification = "Wrong paradigm")>]
()