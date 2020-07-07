namespace AltCover.Avalonia

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Linq
open System.Xml.XPath

open AltCover.Augment
open AltCover.Visualizer
open AltCover.Visualizer.GuiCommon

open Avalonia
open Avalonia.Controls
open Avalonia.Markup.Xaml
open Avalonia.Media
open Avalonia.Media.Imaging
open Avalonia.Threading

module Persistence =
  let mutable internal save = true

  let internal saveSchemaDir = Configuration.SaveSchemaDir
  let internal saveFont = Configuration.SaveFont
  let internal readFont = Configuration.ReadFont
  let internal readSchemaDir = Configuration.ReadSchemaDir
  let internal readFolder = Configuration.ReadFolder
  let internal saveFolder = Configuration.SaveFolder
  let internal saveCoverageFiles = Configuration.SaveCoverageFiles
  let readCoverageFiles() =
    let mutable l = []
    Configuration.ReadCoverageFiles (fun files -> l <- files)
    l
  let saveGeometry (w : Window) =
    Configuration.SaveGeometry (fun () -> w.Position.X, w.Position.Y)
                               (fun () -> w.Width, w.Height)

  let readGeometry (w : Window) =
    Configuration.ReadGeometry (fun (width,height) (x,y) -> w.Height <- float height
                                                            w.Width <- float width
                                                            let monitor = w.Screens.All
                                                                          |> Seq.filter (fun s -> let tl = s.WorkingArea.TopLeft
                                                                                                  let br = s.WorkingArea.BottomRight
                                                                                                  x >= tl.X && x <= br.X &&
                                                                                                     y >= tl.Y && y <= br.Y)
                                                                          |> Seq.tryHead
                                                                          |> Option.defaultValue w.Screens.Primary
                                                            let bounds = monitor.WorkingArea
                                                            let x' = Math.Min(Math.Max(x, bounds.TopLeft.X), bounds.BottomRight.X - width)
                                                            let y' = Math.Min(Math.Max(y, bounds.TopLeft.Y), bounds.BottomRight.Y - height)
                                                            w.Position <- PixelPoint(x', y'))
  let clearGeometry = Configuration.ClearGeometry

type TextTag =
  { Foreground : string
    Background : string }

  static member Make a b =
    { Foreground = a
      Background = b }

  static member Visited = TextTag.Make "#0000CD" "#F5F5F5" // Medium Blue on White Smoke
  // "#404040" "#cefdce" // Dark on Pale Green
  static member Declared = TextTag.Make "#FFA500" "#F5F5F5" // Orange on White Smoke
  // "#FFA500" "#FFFFFF" // Orange on White
  static member StaticAnalysis = TextTag.Make "#708090" "#F5F5F5" // Slate Grey on White Smoke
  // "#808080" "#F5F5F5" // Grey on White Smoke
  static member Automatic = TextTag.Make "#FFD700" "#F5F5F5" // Gold on White Smoke
  // "#808080" "#FFFF00" // Grey on Yellow
  static member NotVisited = TextTag.Make "#DC143C" "#F5F5F5"// Crimson on White Smoke
  //"#ff0000" "#FFFFFF" // Red on White
  static member Excluded = TextTag.Make "#87CEEB" "#F5F5F5" // Sky Blue on White Smoke
  // "#87CEEB" "#FFFFFF" // Sky Blue on white
  static member Partial = TextTag.Make "#000000" "#F5F5F5" // Black on White Smoke
  // "#404040" "#FFFF00" // Dark on Yellow

// Range colouring information
type internal ColourTag =
  { style : TextTag
    vc : int
    line : int
    column : int
    endline : int
    endcolumn : int }

type MainWindow() as this =
  inherit Window()
  let mutable armed = false
  let mutable justOpened = String.Empty
  let mutable coverageFiles : string list = []
  let ofd = OpenFileDialog()
  let icons = Icons(fun x -> new Bitmap(x))

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
    display.Orientation <- Layout.Orientation.Horizontal
    display.Children.Add tree
    display.Children.Add image
    display.Children.Add text
    display.Tag <- name
    display

  do
    this.InitializeComponent()
    this.Show()

  member private this.DisplayMessage (status : MessageType) message =
    let caption = match status with
                  | MessageType.Warning -> Resource.GetResourceString "LoadWarning"
                  | _ -> Resource.GetResourceString "LoadError"
    this.ShowMessageBox status caption message

  member private this.ShowMessageBox (status : MessageType) caption message =
    Dispatcher.UIThread.Post(fun _ ->
      this.FindControl<Image>("Status").Source <- (match status with
                                                   | MessageType.Info -> icons.Info
                                                   | MessageType.Warning -> icons.Warn
                                                   | _ -> icons.Error).Force()
      this.FindControl<TextBlock>("Caption").Text <- caption
      this.FindControl<TextBox>("Message").Text <- message
      this.FindControl<StackPanel>("MessageBox").IsVisible <- true
      this.FindControl<Menu>("Menu").IsVisible <- false
      this.FindControl<DockPanel>("Grid").IsVisible <- false)

  // Fill in the menu from the memory cache
  member private this.PopulateMenu() =
    let listitem = this.FindControl<MenuItem>("List")
    let items = listitem.Items.OfType<MenuItem>()
    // blank the whole menu
    items
    |> Seq.iter (fun (i : MenuItem) ->
         i.IsVisible <- false
         i.Header <- String.Empty)
    // fill in with the items we have
    Seq.zip coverageFiles items
    |> Seq.iter (fun (name, item) ->
         item.IsVisible <- true
         item.Header <- name)
    // set or clear the menu
    listitem.IsEnabled <- coverageFiles.Any()
    this.FindControl<Image>("ListImage").Source <- (if coverageFiles.Any() then
                                                      icons.MRU
                                                    else
                                                      icons.MRUInactive).Force()

  member private this.UpdateMRU path add =
    let casematch =
      match System.Environment.GetEnvironmentVariable("OS") with
      | "Windows_NT" -> StringComparison.OrdinalIgnoreCase
      | _ -> StringComparison.Ordinal

    let files =
      coverageFiles
      |> List.filter (fun n -> not (n.Equals(path, casematch)))
      |> Seq.truncate (9)
      |> Seq.toList

    coverageFiles <-
      (if add then (path :: files) else files)
      |> Seq.distinctBy (fun n ->
           match casematch with
           | StringComparison.Ordinal -> n
           | _ -> n.ToUpperInvariant())
      |> Seq.toList
    this.PopulateMenu()
    Persistence.saveCoverageFiles coverageFiles
    this.FindControl<MenuItem>("Refresh").IsEnabled <- coverageFiles.Any()
    this.FindControl<Image>("RefreshImage").Source <- (if coverageFiles.Any() then
                                                         icons.Refresh
                                                       else
                                                         icons.RefreshInactive).Force()
  member private this.HideAboutBox _ =
    this.FindControl<StackPanel>("AboutBox").IsVisible <- false
    this.FindControl<Menu>("Menu").IsVisible <- true
    this.FindControl<DockPanel>("Grid").IsVisible <- true

  member private this.PrepareDoubleTap
    (context:CoverageTreeContext<List<TreeViewItem>,TreeViewItem>)
    (xpath: XPathNavigator) =
        let visbleName = (context.Row.Header :?> StackPanel).Tag.ToString()

        let (|Select|_|) (pattern : String) offered =
          if (fst offered)
             |> String.IsNullOrWhiteSpace
             |> not
             && pattern.StartsWith(fst offered, StringComparison.Ordinal) then
            Some offered
          else
            None

        let selectStyle because excluded =
          match (because, excluded) with
          | Select "author declared (" _ -> TextTag.Declared
          | Select "tool-generated: " _ -> TextTag.Automatic
          | Select "static analysis: " _ -> TextTag.StaticAnalysis
          | (_, true) -> TextTag.Excluded
          | _ -> TextTag.NotVisited

        let coverageToTag(n : XPathNavigator) =
          let excluded = Boolean.TryParse(n.GetAttribute("excluded", String.Empty)) |> snd
          let visitcount = Int32.TryParse(n.GetAttribute("visitcount", String.Empty)) |> snd
          let line = n.GetAttribute("line", String.Empty)
          let column = n.GetAttribute("column", String.Empty)
          let endline = n.GetAttribute("endline", String.Empty)
          let endcolumn = n.GetAttribute("endcolumn", String.Empty)
          // Extension behaviour for textual signalling for three lines
          n.MoveToParent() |> ignore
          let because = n.GetAttribute("excluded-because", String.Empty)
          { style =
              if visitcount = 0 then selectStyle because excluded else TextTag.Visited
            vc = visitcount
            line = Int32.TryParse(line) |> snd
            column = (Int32.TryParse(column) |> snd)
            endline = Int32.TryParse(endline) |> snd
            endcolumn = (Int32.TryParse(endcolumn) |> snd) }

        let filterCoverage lines (n : ColourTag) =
          n.line > 0 && n.endline > 0 && n.line <= lines && n.endline <= lines
        let tagByCoverage (buff : TextBlock) (lines : FormattedTextLine list) (n : ColourTag) =
          let start = (n.column - 1) + (lines |> Seq.take (n.line - 1) |> Seq.sumBy (fun l -> l.Length))
          let finish = (n.endcolumn - 1) + (lines |> Seq.take (n.endline - 1) |> Seq.sumBy (fun l -> l.Length))
          FormattedTextStyleSpan(start, finish - start,
                        SolidColorBrush.Parse n.style.Foreground)
        let parseIntegerAttribute (element : XPathNavigator) (attribute : string) =
          let text = element.GetAttribute(attribute, String.Empty)
          let number = Int32.TryParse(text, NumberStyles.None, CultureInfo.InvariantCulture)
          if (fst number) then
            snd number
          else
            if not <| String.IsNullOrEmpty(text) then
              System.Diagnostics.Debug.WriteLine
                ("ParseIntegerAttribute : '" + attribute + "' with value '" + text)
            0

      //// bound by current line length in case we're looking from stale coverage
      //let line = buff.GetIterAtLine(n.line - 1)
      //let from =
      //  if line.CharsInLine = 0 then line
      //  else buff.GetIterAtLineOffset(n.line - 1, Math.Min(n.column, line.CharsInLine) - 1)
      //let endline = buff.GetIterAtLine(n.endline - 1)
      //let until =
      //  if endline.CharsInLine = 0 then endline
      //  else buff.GetIterAtLineOffset(n.endline - 1, Math.Min(n.endcolumn, endline.CharsInLine) - 1)
      //buff.ApplyTag(tag, from, until)
        let markBranches (root : XPathNavigator) (stack : StackPanel) (lines : FormattedTextLine list) (filename:string) =
          let branches = new Dictionary<int, int * int>()

          root.Select("//method")
          |> Seq.cast<XPathNavigator>
          |> Seq.filter (fun n ->
               let f = n.Clone()
               f.MoveToFirstChild()
               && filename.Equals
                    (f.GetAttribute("document", String.Empty),
                     StringComparison.Ordinal))
          |> Seq.collect (fun n -> n.Select("./branch") |> Seq.cast<XPathNavigator>)
          |> Seq.cast<XPathNavigator>
          |> Seq.groupBy (fun n -> n.GetAttribute("line", String.Empty))
          |> Seq.toList
          |> Seq.iter (fun n ->
               let line = parseIntegerAttribute ((snd n) |> Seq.head) "line"
               let num = (snd n) |> Seq.length
               let v =
                 (snd n)
                 |> Seq.filter (fun x -> x.GetAttribute("visitcount", String.Empty) <> "0")
                 |> Seq.length
               branches.Add(line, (v, num)))

          let h = (lines |> Seq.head).Height
          let pad = (h - 16.0)/2.0
          let margin = Thickness(0.0, pad)

          Dispatcher.UIThread.Post(fun _ ->
            for l in 1 .. lines.Length do
              let counts = branches.TryGetValue l

              let (|AllVisited|_|) (b, (v, num)) =
                if b |> not
                   || v <> num then
                  None
                else
                  Some()

              let pix =
                match counts with
                | (false, _) -> icons.Blank.Force()
                | (_, (0, _)) -> icons.RedBranch.Force()
                | AllVisited -> icons.Branched.Force()
                | _ -> icons.Branch.Force()

              let pic = new Image()
              pic.Source <- pix
              pic.Margin <- margin
              stack.Children.Add pic
              if fst counts then
                let v, num = snd counts
                ToolTip.SetTip(pic,
                  Resource.Format("branchesVisited", [v; num])))

        let markCoverage (root : XPathNavigator) textBox (text2: TextBlock) (lines : FormattedTextLine list) filename =
          let lc = lines.Length
          let tags = root.Select("//seqpnt[@document='" + filename + "']")
                     |> Seq.cast<XPathNavigator>
                     |> Seq.map coverageToTag
                     |> Seq.filter (filterCoverage lc)
                     |> Seq.sortByDescending (fun t -> t.vc)
                     |> Seq.toList

          let formats = tags
                        |> List.map (tagByCoverage textBox lines)

          let linemark = tags
                         |> List.groupBy (fun t -> t.line)
                         |> List.map (fun (l, t) -> let total = t |> Seq.sumBy (fun tag -> if tag.vc <= 0
                                                                                           then 0
                                                                                           else tag.vc)
                                                    let style = if total > 0
                                                                then TextTag.Visited
                                                                else TextTag.NotVisited
                                                    let start = (l - 1) * (7 + Environment.NewLine.Length)
                                                    FormattedTextStyleSpan(start, 7,
                                                                           SolidColorBrush.Parse style.Foreground))

          let linetext = String.Join (Environment.NewLine,
                                      lines
                                      // Font limitation or Avalonia limitation? -- character \u2442 just shows as a box.
                                      |> Seq.mapi (fun i _ -> sprintf "%6d " (1 + i)))

          Dispatcher.UIThread.Post(fun _ -> textBox.FormattedText.Spans <- formats
                                            text2.Text <- linetext
                                            text2.FormattedText.Spans <- linemark)

        context.Row.DoubleTapped
        |> Event.add (fun _ ->
             let text = this.FindControl<TextBlock>("Source")
             let text2 = this.FindControl<TextBlock>("Lines")
             let scroller = this.FindControl<ScrollViewer>("Coverage")
             let points =
               xpath.SelectChildren("seqpnt", String.Empty) |> Seq.cast<XPathNavigator>
             if Seq.isEmpty points then
               let caption = Resource.GetResourceString "LoadInfo"
               this.ShowMessageBox MessageType.Info caption
               <| String.Format
                    (System.Globalization.CultureInfo.CurrentCulture,
                     Resource.GetResourceString "No source location", visbleName)
             else
               let point = points |> Seq.head
               let path = point.GetAttribute("document", String.Empty)
               let info = FileInfo(path)
               let source = info |> File
               let current = new FileInfo(coverageFiles.Head)
               if (not info.Exists) then
                 Messages.MissingSourceThisFileMessage (this.DisplayMessage) current source
               else if (info.LastWriteTimeUtc > current.LastWriteTimeUtc) then
                 Messages.OutdatedCoverageThisFileMessage (this.DisplayMessage) current source
               else
                 let line =
                   point.GetAttribute("line", String.Empty)
                   |> Int32.TryParse
                   |> snd
                 try
                   // TODO -- font  size control too
                   text.Text <- File.ReadAllText path

                   [ text; text2 ]
                   |> List.iter (fun t ->
                         t.FontFamily <- FontFamily(Persistence.readFont())
                         t.FontSize <- 16.0
                         t.FontStyle <- FontStyle.Normal)

                   let textLines = text.FormattedText.GetLines() |> Seq.toList
                   let sample = textLines |> Seq.head
                   let depth = sample.Height * float (line - 1)
                   let root = xpath.Clone()
                   root.MoveToRoot()
                   markCoverage root text text2 textLines path
                   let stack = this.FindControl<StackPanel>("Branches")
                   root.MoveToRoot()
                   markBranches root stack textLines path

                   Dispatcher.UIThread.Post(fun _ ->
                                       let midpoint = scroller.Viewport.Height / 2.0
                                       if (depth > midpoint)
                                       then scroller.Offset <- scroller.Offset.WithY(depth - midpoint))

                 with x ->
                   let caption = Resource.GetResourceString "LoadError"
                   this.ShowMessageBox MessageType.Error caption x.Message)

  member this.InitializeComponent() =
    AvaloniaXamlLoader.Load(this)
    Persistence.readGeometry this
    coverageFiles <- Persistence.readCoverageFiles()
    this.PopulateMenu()
    ofd.Directory <- Persistence.readFolder()
    ofd.Title <- Resource.GetResourceString "Open Coverage File"
    ofd.AllowMultiple <- false
    let filterBits =
      (Resource.GetResourceString "SelectXml").Split([| '|' |])
      |> Seq.map (fun f ->
           let entry = f.Split([| '%' |])
           let filter = FileDialogFilter()
           filter.Name <- entry |> Seq.head
           filter.Extensions <- List(entry |> Seq.tail)
           filter)
    ofd.Filters <- List(filterBits)
    this.Title <- "AltCover.Visualizer"
    [ "Open"; "Refresh"; "Font"; "About"; "Exit" ]
    |> Seq.iter (fun n ->
         let item = this.FindControl<TextBlock>(n + "Text")
         item.Text <- Resource.GetResourceString n)
    this.FindControl<MenuItem>("Exit").Click
    |> Event.add (fun _ ->
         if Persistence.save then Persistence.saveGeometry this
         let l = Application.Current.ApplicationLifetime :?> Avalonia.Controls.ApplicationLifetimes.IClassicDesktopStyleApplicationLifetime
         l.Shutdown())
    this.FindControl<MenuItem>("About").Click
    |> Event.add (fun _ ->
         this.FindControl<StackPanel>("AboutBox").IsVisible <- true
         this.FindControl<Menu>("Menu").IsVisible <- false
         this.FindControl<DockPanel>("Grid").IsVisible <- false)
    let openFile = new Event<String option>()
    this.FindControl<MenuItem>("Open").Click
    |> Event.add (fun _ ->
         this.HideAboutBox()
         async {
           (ofd.ShowAsync(this)
            |> Async.AwaitTask
            |> Async.RunSynchronously).FirstOrDefault()
           |> Option.ofObj
           |> openFile.Trigger
         }
         |> Async.Start)
    let click =
      openFile.Publish
      |> Event.choose id
      |> Event.map (fun n ->
           ofd.Directory <- Path.GetDirectoryName n
           if Persistence.save then Persistence.saveFolder ofd.Directory
           justOpened <- n
           -1)

    let select =
      this.FindControl<MenuItem>("List").Items.OfType<MenuItem>()
      |> Seq.mapi (fun n (i : MenuItem) -> i.Click |> Event.map (fun _ -> n))
    // The sum of all these events -- we have explicitly selected a file
    let fileSelection = select |> Seq.fold Event.merge click
    let refresh = this.FindControl<MenuItem>("Refresh").Click |> Event.map (fun _ -> 0)
    let makeNewRow name (anIcon:Lazy<Bitmap>) =
      let row = TreeViewItem()
      row.HorizontalAlignment <- Layout.HorizontalAlignment.Left
      row.LayoutUpdated
      |> Event.add (fun _ -> let remargin (t:TreeViewItem) =
                               if t.HeaderPresenter.IsNotNull
                               then let hp = t.HeaderPresenter :?> Avalonia.Controls.Presenters.ContentPresenter
                                    let grid = hp.Parent :?> Grid
                                    grid.Margin <- Thickness(float t.Level * 4.0, 0.0, 0.0, 0.0)

                             remargin row
                             row.Items.OfType<TreeViewItem>()
                             |> Seq.iter remargin)
      row.Tapped |> Event.add (fun evt -> row.IsExpanded <- not row.IsExpanded
                                          let items = (row.Header :?> StackPanel).Children
                                          items.RemoveAt(0)
                                          let mark = Image()
                                          mark.Source <- if row.Items.OfType<Object>().Any()
                                                         then if row.IsExpanded
                                                              then icons.TreeCollapse.Force()
                                                              else icons.TreeExpand.Force()
                                                         else icons.Blank.Force()
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
    |> Event.add (fun index ->
      let mutable auxModel =
        {
          Model = List<TreeViewItem>()
          Row = null
        }
      let environment =
        {
          Icons = icons
          GetFileInfo = fun i -> FileInfo(if i < 0
                                          then justOpened
                                          else coverageFiles.[i])
          Display = this.DisplayMessage
          UpdateMRUFailure = fun info -> this.UpdateMRU info.FullName false
          UpdateUISuccess = fun info -> let tree = this.FindControl<TreeView>("Tree")
                                        tree.Items.OfType<IDisposable>() |> Seq.iter (fun x -> x.Dispose())
                                        this.FindControl<TextBlock>("Source").Text <- String.Empty
                                        this.FindControl<TextBlock>("Lines").Text <- String.Empty
                                        this.FindControl<StackPanel>("Branches").Children.Clear()
                                        tree.Items <- auxModel.Model
                                        this.UpdateMRU info.FullName true
          SetXmlNode = fun name -> let model = auxModel.Model
                                   // mappings.Clear()
                                   {
                                      Model = model
                                      Row = let row = makeNewRow name icons.Xml
                                            model.Add row
                                            row
                                   }
          AddNode = fun context icon name ->
                                 { context with
                                       Row = let row = makeNewRow name icon
                                             (context.Row.Items :?> List<TreeViewItem>).Add row
                                             row }
          Map = this.PrepareDoubleTap
        }

      Dispatcher.UIThread.Post(fun _ -> CoverageFileTree.DoSelected environment index))

    this.FindControl<TextBlock>("Program").Text <- "AltCover.Visualizer "
                                                   + AssemblyVersionInformation.AssemblyFileVersion
    this.FindControl<TextBlock>("Description").Text <- Resource.GetResourceString
                                                         "aboutVisualizer.Comments"
    let copyright = AssemblyVersionInformation.AssemblyCopyright
    this.FindControl<TextBlock>("Copyright").Text <- String.Format
                                                 (CultureInfo.InvariantCulture,
                                                  Resource.GetResourceString "aboutVisualizer.Copyright",
                                                  copyright)

    let link = this.FindControl<TextBlock>("Link")
    link.Text <- Resource.GetResourceString "aboutVisualizer.WebsiteLabel"
    let linkButton = this.FindControl<Button>("LinkButton")
    linkButton.Click |> Event.add (fun _ -> Avalonia.Dialogs.AboutAvaloniaDialog.OpenBrowser
                                              "http://www.github.com/SteveGilham/altcover")

    this.FindControl<TabItem>("AboutDetails").Header <- Resource.GetResourceString "About"
    this.FindControl<TabItem>("License").Header <- Resource.GetResourceString
                                                     "AboutDialog.License"
    this.FindControl<TextBlock>("MIT").Text <- String.Format
                                                 (CultureInfo.InvariantCulture,
                                                  Resource.GetResourceString "License",
                                                  copyright)
    this.Closing
    |> Event.add (fun e ->
         if this.FindControl<DockPanel>("Grid").IsVisible |> not then
           this.FindControl<StackPanel>("AboutBox").IsVisible <- false
           this.FindControl<StackPanel>("MessageBox").IsVisible <- false
           this.FindControl<Menu>("Menu").IsVisible <- true
           this.FindControl<DockPanel>("Grid").IsVisible <- true
           e.Cancel <- true)

    // MessageBox
    let okButton = this.FindControl<Button>("DismissMessageBox")
    okButton.Content <- "OK"
    okButton.Click
    |> Event.add (fun _ ->
         this.FindControl<StackPanel>("MessageBox").IsVisible <- false
         this.FindControl<Menu>("Menu").IsVisible <- true
         this.FindControl<DockPanel>("Grid").IsVisible <- true)

    // AboutBox
    let okButton2 = this.FindControl<Button>("DismissAboutBox")
    okButton2.Content <- "OK"
    okButton2.Click
    |> Event.add (fun _ ->
         this.FindControl<StackPanel>("AboutBox").IsVisible <- false
         this.FindControl<Menu>("Menu").IsVisible <- true
         this.FindControl<DockPanel>("Grid").IsVisible <- true)