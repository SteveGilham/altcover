namespace AltCover

open System
open System.Collections.Generic
open System.IO
open System.Linq
open System.Xml.XPath

open GuiCommon

open Gdk
open Gtk

open Mono.Options
open System.Diagnostics.CodeAnalysis

module private Gui =

  // --------------------------  General Purpose ---------------------------
  let icons = Icons(fun x -> new Pixbuf(x))

  // --------------------------  Persistence ---------------------------

  // -------------------------- Message Boxes ---------------------------

  // -------------------------- UI set-up  ---------------------------
  let private initializeHandler () =
    let handler = Handler()

    [ "mainWindow"
      "fileOpenMenu"
      "aboutVisualizer" ]
#if !NET472
    |> List.iter
         (fun name ->
           use b =
             new Builder(
               System
                 .Reflection
                 .Assembly
                 .GetExecutingAssembly()
                 .GetManifestResourceStream("AltCover.Visualizer.Visualizer3.glade"),
               name
             )

           b.Autoconnect handler)
#else
    |> List.iter
         (fun name ->
           use xml =
             new Glade.XML("AltCover.Visualizer.Visualizer.glade", name)

           xml.Autoconnect(handler))
#endif

    handler.coverageFiles <- []

    handler

  let private setDefaultText (h: Handler) =
    [ (h.codeView.Buffer, 100)
      (h.lineView.Buffer, 7) ]
    |> Seq.iter
         (fun (b, n) ->
           b.Text <- String(' ', n)
           b.ApplyTag("baseline", b.StartIter, b.EndIter))

  // Fill in the menu from the memory cache
  let private populateMenu (handler: Handler) =
    let items =
      handler.fileOpenMenu.AllChildren
      |> Seq.cast<MenuItem>

    let active =
      HandlerCommon.PopulateMenu
        items
        handler.coverageFiles
        (fun (i: MenuItem) ->
          i.Visible <- false
          (i.Child :?> Label).Text <- String.Empty)
        (fun name item ->
          item.Visible <- true
          (item.Child :?> Label).Text <- name)
    // set or clear the menu
    handler.openButton.Menu <-
      if active then
        handler.fileOpenMenu :> Widget
      else
        null

    active

  let private prepareAboutDialog (handler: Handler) =
#if !NET472
    handler.aboutVisualizer.TransientFor <- handler.mainWindow
#else
    AboutDialog.SetUrlHook(fun _ link -> Browser.ShowUrl(Uri link))
    |> ignore

    LinkButton.SetUriHook(fun _ link -> Browser.ShowUrl(Uri link))
    |> ignore

    handler.aboutVisualizer.ActionArea.Children.OfType<Button>()
    |> Seq.iter
         (fun w ->
           let t = Resource.GetResourceString w.Label

           if t |> String.IsNullOrWhiteSpace |> not then
             w.Label <- t)
#endif

    handler.aboutVisualizer.Title <- Resource.GetResourceString("aboutVisualizer.Title")

    handler.aboutVisualizer.Modal <- true
    handler.aboutVisualizer.WindowPosition <- WindowPosition.Mouse

    handler.aboutVisualizer.Version <-
      System.AssemblyVersionInformation.AssemblyFileVersion

    handler.aboutVisualizer.Copyright <-
      Resource.Format(
        "aboutVisualizer.Copyright",
        [ System.AssemblyVersionInformation.AssemblyCopyright ]
      )

    handler.aboutVisualizer.License <-
      Resource.Format("License", [ System.AssemblyVersionInformation.AssemblyCopyright ])

    handler.aboutVisualizer.Comments <-
      Resource.GetResourceString("aboutVisualizer.Comments")

    handler.aboutVisualizer.WebsiteLabel <-
      Resource.GetResourceString("aboutVisualizer.WebsiteLabel")

  let private prepareTreeView (handler: Handler) =
    [| icons.Assembly
       icons.Namespace
       icons.Class
       icons.Method |]
    |> Seq.iteri
         (fun i x -> // this line number
           let column = new Gtk.TreeViewColumn()
           let cell = new Gtk.CellRendererText()
           let icon = new Gtk.CellRendererPixbuf()
           column.PackStart(icon, true)
           column.PackEnd(cell, true)

           handler.classStructureTree.AppendColumn(column)
           |> ignore

           column.AddAttribute(cell, "text", 2 * i)
           column.AddAttribute(icon, "pixbuf", 1 + (2 * i)))

    handler.classStructureTree.Model <-
      new TreeStore(
        typeof<string>,
        typeof<Gdk.Pixbuf>,
        typeof<string>,
        typeof<Gdk.Pixbuf>,
        typeof<string>,
        typeof<Gdk.Pixbuf>,
        typeof<string>,
        typeof<Gdk.Pixbuf>,
        typeof<string>,
        typeof<Gdk.Pixbuf>
      )

    handler.auxModel <-
      new TreeStore(
        typeof<string>,
        typeof<Gdk.Pixbuf>,
        typeof<string>,
        typeof<Gdk.Pixbuf>,
        typeof<string>,
        typeof<Gdk.Pixbuf>,
        typeof<string>,
        typeof<Gdk.Pixbuf>,
        typeof<string>,
        typeof<Gdk.Pixbuf>
      )

#if !NET472
  [<AutoSerializable(false); Sealed>]
  [<SuppressMessage("Gendarme.Rules.Correctness",
                    "DisposableFieldsShouldBeDisposedRule",
                    Justification = "Whole program lifetime")>]
  [<SuppressMessage("Gendarme.Rules.Maintainability",
                    "VariableNamesShouldNotMatchFieldNamesRule",
                    Justification = "Compiler generated")>]
  type FileOpenDialog(dialog: FileChooserDialog) =
    member self.Run() =
      dialog.SetCurrentFolder(Persistence.readFolder ())
      |> ignore

      try
        dialog.Run()
      finally
        dialog.Hide()

    member self.FileName = dialog.Filename

    [<SuppressMessage("Gendarme.Rules.Design",
                      "AvoidPropertiesWithoutGetAccessorRule",
                      Justification = "No use case exists")>]
    member self.InitialDirectory
      with set (value) = dialog.SetCurrentFolder(value) |> ignore

    interface IDisposable with
      [<SuppressMessage("Gendarme.Rules.Design",
                        "DoNotDeclareVirtualMethodsInSealedTypeRule",
                        Justification = "Compiler generated virtual")>]
      member self.Dispose() = ()

  [<SuppressMessage("Gendarme.Rules.Correctness",
                    "EnsureLocalDisposalRule",
                    Justification = "Value is returned")>]
  let private prepareOpenFileDialog (handler: Handler) =
    let openFileDialog =
      new FileChooserDialog(
        Resource.GetResourceString "OpenFile",
        handler.mainWindow,
        FileChooserAction.Open,
        Resource.GetResourceString "OpenFile.Open",
        ResponseType.Ok,
        Resource.GetResourceString "OpenFile.Cancel",
        ResponseType.Cancel,
        null
      )

    let data =
      Resource
        .GetResourceString("SelectXml")
        .Split([| '|' |])

    data
    |> Seq.iter
         (fun t ->
           let filter = new FileFilter()
           let data = t.Split([| '%' |])
           filter.Name <- data.[0]
           filter.AddPattern("*." + data.[1])
           openFileDialog.AddFilter filter)

    new FileOpenDialog(openFileDialog)
#else
  type FileOpenDialog = System.Windows.Forms.OpenFileDialog

  [<SuppressMessage("Microsoft.Reliability",
                    "CA2000:DisposeObjectsBeforeLosingScope",
                    Justification = "'openFileDialog' is returned")>]
  [<SuppressMessage("Microsoft.Globalization",
                    "CA1303:Do not pass literals as localized parameters",
                    Justification = "It's furniture, not user visible text")>]
  let private prepareOpenFileDialog _ =
    let openFileDialog =
      new System.Windows.Forms.OpenFileDialog()

    openFileDialog.InitialDirectory <- Persistence.readFolder ()

    openFileDialog.Filter <-
      Resource
        .GetResourceString("SelectXml")
        .Replace("%", "|*.")

    openFileDialog.FilterIndex <- 0
    openFileDialog.RestoreDirectory <- false
    openFileDialog
#endif
  // -------------------------- Tree View ---------------------------
  let mappings =
    new Dictionary<TreePath, XPathNavigator>()
  // -------------------------- Event handling  ---------------------------
#if !NET472
  type TheTreeModel = ITreeModel
#else
  type TheTreeModel = TreeModel
#endif

  let private doSelected (handler: Handler) doUpdateMRU index =
    let environment =
      { Icons = icons
        GetFileInfo =
          fun i ->
            FileInfo(
              if i < 0 then
                handler.justOpened
              else
                handler.coverageFiles.[i]
            )
        Display =
          (handler :> IVisualizerWindow)
            .ShowMessageOnGuiThread
        UpdateMRUFailure =
          fun info ->
            Handler.InvokeOnGuiThread(fun () -> doUpdateMRU handler info.FullName false)
        UpdateUISuccess =
          fun info ->
            let updateUI (theModel: TheTreeModel) (info: FileInfo) () =
              // File is good so enable the refresh button
              handler.refreshButton.Sensitive <- true
              // Do real UI work here
              handler.auxModel <- handler.classStructureTree.Model :?> TreeStore
              handler.classStructureTree.Model <- theModel
              setDefaultText handler
              handler.mainWindow.Title <- "AltCover.Visualizer"
              doUpdateMRU handler info.FullName true
            ////ShowMessage h.mainWindow (sprintf "%s\r\n>%A" info.FullName handler.coverageFiles) MessageType.Info
            Handler.InvokeOnGuiThread(updateUI handler.auxModel info)
        SetXmlNode =
          fun name ->
            let model = handler.auxModel
            model.Clear()
            mappings.Clear()

            let topRow =
              model.AppendValues(name, icons.Xml.Force())

            { Model = model; Row = topRow }
        AddNode =
          fun context icon name ->
            { context with
                Row =
                  context.Model.AppendValues(
                    context.Row,
                    [| name :> obj; icon.Force() :> obj |]
                  ) }
        Map = fun context xpath -> mappings.Add(context.Model.GetPath context.Row, xpath) }

    async { CoverageFileTree.DoSelected environment index }
    |> Async.Start

  type OpenFileDialogFactory = Handler -> FileOpenDialog

  let private handleOpenClicked
    (handler: Handler)
    (openFileDialogFactory: OpenFileDialogFactory)
    =
    use openFileDialog = openFileDialogFactory handler

    let makeSelection (ofd: FileOpenDialog) x =

#if !NET472
      if Enum.ToObject(typeof<ResponseType>, ofd.Run()) :?> ResponseType = ResponseType.Ok then
#else
      if ofd.ShowDialog() = System.Windows.Forms.DialogResult.OK then
#endif
        let file = FileInfo(ofd.FileName)
        let dir = file.Directory.FullName
        ofd.InitialDirectory <- dir

        if Persistence.save then
          Persistence.saveFolder dir

        Some file
      else
        None

    handler.openButton.Clicked
    |> Event.map (makeSelection openFileDialog)
    |> Event.choose id
    |> Event.map
         (fun info ->
           handler.justOpened <- info.FullName
           -1)

  [<SuppressMessage("Microsoft.Reliability",
                    "CA2000:DisposeObjectsBeforeLosingScope",
                    Justification = "'tag' is subsumed")>]
  let private applyTag font (buffer: TextBuffer) (style: string, fg, bg) =
    let tag = new TextTag(style)
    tag.Font <- font
    tag.Foreground <- fg
    tag.Background <- bg
    buffer.TagTable.Add(tag)

  [<SuppressMessage("Microsoft.Reliability",
                    "CA2000:DisposeObjectsBeforeLosingScope",
                    Justification = "All added to the text buffer")>]
  let private initializeTextBuffer bg (buff: TextBuffer) =
    let font = Persistence.readFont ()

    [ ("baseline", "#808080", bg)
      ("visited", "#0000CD", "#cefdce") // Medium Blue on Pale Green
      ("declared", "#FF8C00", "#FFFFFF") // Dark Orange on White
      ("static", "#F5F5F5", "#000000") // White Smoke on Black
      ("automatic", "#808080", "#FFD700") // Grey on Gold
      ("notVisited", "#DC143C", "#FFFFFF") // Crimson on White
      ("excluded", "#87CEEB", "#F5F5F5") ] // Sky Blue on White Smoke
    |> Seq.iter (fun x -> applyTag font buff x |> ignore)

  let private setDefaultTags (h: Handler) =
    initializeTextBuffer "#f5f5f5" h.codeView.Buffer
    initializeTextBuffer "#ffffff" h.lineView.Buffer

  [<SuppressMessage("Microsoft.Reliability",
                    "CA2000:DisposeObjectsBeforeLosingScope",
                    Justification = "IDisposables are added to the TextView")>]
  let private markBranches
    (root: XPathNavigator)
    (lineView: TextView)
    (filename: string)
    =
    let buff = lineView.Buffer
    let branches = HandlerCommon.TagBranches root filename

    for l in 1 .. buff.LineCount do
      let image = new Image()

      let pix =
        HandlerCommon.IconForBranches
          icons
          branches
          l
          (fun text -> image.TooltipText <- text)

      let mutable i = buff.GetIterAtLineOffset(l - 1, 7)
      let a = new TextChildAnchor()
      buff.InsertChildAnchor(&i, a)
      image.Pixbuf <- pix
      image.Visible <- true
      lineView.AddChildAtAnchor(image, a)

  let private tagByCoverage (buff: TextBuffer) (n: CodeTag) =
    // bound by current line length in case we're looking from stale coverage
    let line = buff.GetIterAtLine(n.Line - 1)
    let chars = line.CharsInLine

    let from =
      if chars = 0 then
        line
      else
        buff.GetIterAtLineOffset(n.Line - 1, Math.Max(Math.Min(n.Column, chars) - 1, 0))

    let endline = buff.GetIterAtLine(n.EndLine - 1)
    let endchars = endline.CharsInLine

    // coverlet-like case w/o column data
    let ec =
      if n.LineOnly then
        endchars
      else
        n.EndColumn

    let until =
      if endchars = 0 then
        endline
      else
        buff.GetIterAtLineOffset(n.EndLine - 1, Math.Min(ec, endchars) - 1)

    let tag =
      match n.Style with
      | Exemption.Visited -> "visited"
      | Exemption.Declared -> "declared"
      | Exemption.Automatic -> "automatic"
      | Exemption.StaticAnalysis -> "static"
      | Exemption.Excluded -> "excluded"
      | _ -> "notVisited"

    buff.ApplyTag(tag, from, until)

  let private markCoverage
    (root: XPathNavigator)
    (buff: TextBuffer)
    (buff2: TextBuffer)
    filename
    =
    let tags =
      HandlerCommon.TagCoverage root filename buff.LineCount

    tags |> Seq.iter (tagByCoverage buff)

    tags
    |> HandlerCommon.TagLines "visited" "notVisited"
    |> List.iter
         (fun (l, tag) ->
           let start = buff2.GetIterAtLine(l - 1)
           let finish = buff2.GetIterAtLineOffset(l - 1, 7)
           buff2.ApplyTag(tag, start, finish))

  let internal lineHeights (h: Handler) =
    let v = h.codeView
    let l = h.lineView
    let buf = v.Buffer
    let lines = buf.LineCount
    let icode = buf.GetIterAtLine(1)
    let (pcode, _h1) = v.GetLineYrange icode
    let iline = l.Buffer.GetIterAtLine(1)
    let (pline, _h2) = l.GetLineYrange iline
    (pcode, pline)

  let internal balanceLines h =
    let (pcode, pline) = lineHeights h

    if pline > pcode then
      h.codeView.PixelsAboveLines <- pline - pcode
    else
      h.lineView.PixelsBelowLines <- pcode - pline

  let internal scrollToRow (h: Handler) =
    let v = h.codeView
    let scroller = h.sourceScroller
    let va = scroller.Vadjustment
    let lines = v.Buffer.LineCount

    let pages = va.Upper / va.PageSize // total document size
    let ``lines per page`` = (float lines) / pages

    let pagedepth =
      float (h.activeRow - 1) / ``lines per page``

    let adjust =
      if pagedepth > 0.5 then
        let pageshift = (pagedepth - 0.5)
        Math.Min(va.Upper - va.PageSize, pageshift * va.PageSize)
      else
        0.0

    Handler.InvokeOnGuiThread
      (fun () ->
        balanceLines h
        va.Value <- adjust)

#if !NET472
  // fsharplint:disable-next-line RedundantNewKeyword
  let latch = new Threading.ManualResetEvent false
#endif

  let private onRowActivated (handler: Handler) (activation: RowActivatedArgs) =
    let hitFilter (activated: RowActivatedArgs) (path: TreePath) =
      activated.Path.Compare(path) = 0

    let hits =
      mappings.Keys |> Seq.filter (hitFilter activation)

    if not (Seq.isEmpty hits) then
      let m = mappings.[Seq.head hits]

      let noSource () =
        let message =
          Resource.Format(
            "No source location",
            [ (activation.Column.Cells.[1] :?> Gtk.CellRendererText)
                .Text.Replace("<", "&lt;")
                .Replace(">", "&gt;") ]
          )

        (handler :> IVisualizerWindow)
          .ShowMessageOnGuiThread
          AltCover.MessageType.Info
          message

      let showSource (info: Source) (line: int) =
        let buff = handler.codeView.Buffer
        let buff2 = handler.lineView.Buffer
        let pathname = info.FullName

        buff.Text <- info.ReadAllText()

        buff2.Text <-
          String.Join(
            Environment.NewLine,
            seq { 1 .. buff.LineCount }
            |> Seq.map (fun i -> sprintf "%6d " i)
          )

        [ handler.codeView; handler.lineView ]
        |> Seq.iter
             (fun v ->
               v.PixelsAboveLines <- 0
               v.PixelsInsideWrap <- 0
               v.PixelsBelowLines <- 0)

        [ buff; buff2 ]
        |> List.iter (fun b -> b.ApplyTag("baseline", b.StartIter, b.EndIter))

        let root = m.Clone()
        root.MoveToRoot()
        markBranches root handler.lineView pathname
        markCoverage root buff buff2 pathname
        handler.activeRow <- line
        handler.codeView.CursorVisible <- false
        handler.viewport1.QueueDraw()

        async {
          Threading.Thread.Sleep(300)
          scrollToRow handler
        }
        |> Async.Start

      HandlerCommon.DoRowActivation m handler noSource showSource

  [<SuppressMessage("Microsoft.Reliability",
                    "CA2000:DisposeObjectsBeforeLosingScope",
                    Justification = "IDisposables are added to other widgets")>]
  let private addLabelWidget g (button: ToolButton, resource) =
    let keytext =
      (resource |> Resource.GetResourceString)
        .Split('|')

    let key =
      Keyval.FromName(keytext.[0].Substring(0, 1))
      |> int
      |> enum<Gdk.Key>

    button.AddAccelerator(
      "clicked",
      g,
      AccelKey(key, ModifierType.Mod1Mask, AccelFlags.Visible)
    )

    let label = new TextView()
    let buffer = label.Buffer
    let tag0 = new TextTag("baseline")
    tag0.Justification <- Justification.Center
    tag0.Background <- "#FFFFFF"

    let tt = buffer.TagTable
    tt.Add tag0 |> ignore
    let tag = new TextTag("underline")
    tag.Underline <- Pango.Underline.Single
    tt.Add tag |> ignore

    let start = keytext.[1].IndexOf('_')
    buffer.Text <- keytext.[1].Replace("_", String.Empty)
    buffer.ApplyTag("baseline", buffer.StartIter, buffer.EndIter)

    buffer.ApplyTag(
      "underline",
      buffer.GetIterAtLineOffset(0, start),
      buffer.GetIterAtLineOffset(0, start + 1)
    )

    label.CursorVisible <- false
    label.Editable <- false
    button.Label <- null
    button.LabelWidget <- label

  [<SuppressMessage("Microsoft.Reliability",
                    "CA2000:DisposeObjectsBeforeLosingScope",
                    Justification = "IDisposables are added to other widgets")>]
  let private setToolButtons (h: Handler) =
    let g = new AccelGroup()
    h.mainWindow.AddAccelGroup(g)
#if !NET472
    h.toolbar1.ToolbarStyle <- ToolbarStyle.Both
    let prov = new CssProvider()
    let nl = Environment.NewLine

    let style =
      nl
      + "* {"
      + nl
      + "     background-color: white;"
      + nl
      + "  }"
      + nl

    prov.LoadFromData(style) |> ignore
    h.toolbar1.StyleContext.AddProvider(prov, UInt32.MaxValue)
#endif

    [ (h.openButton :> ToolButton, "openButton.Label")
      (h.refreshButton, "refreshButton.Label")
      (h.fontButton, "fontButton.Label")
      (h.showAboutButton, "showAboutButton.Label")
      (h.exitButton, "exitButton.Label") ]
    |> Seq.iter (addLabelWidget g)

    h.fileOpenMenu.AllChildren
    |> Seq.cast<MenuItem>
    |> Seq.iteri
         (fun n (i: MenuItem) ->
           let c = ((n + 1) % 10) |> char

           let key =
             Keyval.FromName(String [| '0' + c |])
             |> int
             |> enum<Gdk.Key>

           i.AddAccelerator(
             "activate",
             g,
             AccelKey(key, ModifierType.Mod1Mask, AccelFlags.Visible)
           ))

  let private prepareGui () =
    let handler = initializeHandler ()
    setToolButtons handler
    prepareAboutDialog handler
    prepareTreeView handler
    Persistence.readGeometry handler.mainWindow
    Persistence.readCoverageFiles handler
    populateMenu handler |> ignore // no refresh at this point
    handler.separator1.Expand <- true
    handler.separator1.Homogeneous <- false
    handler.codeView.Editable <- false
#if NET472
    let whiteSmoke = Color(245uy, 245uy, 245uy)

    seq { 0 .. 4 }
    |> Seq.iter
         (fun i ->
           let state = i |> enum
           handler.viewport1.ModifyBg(state, whiteSmoke)
           handler.codeView.ModifyBase(state, whiteSmoke)
           handler.codeView.ModifyBg(state, whiteSmoke))
#else
    let prov = new CssProvider()

    let nl = Environment.NewLine

    let style =
      nl
      + "* {"
      + nl
      + "     background-color: whiteSmoke;"
      + nl
      + "  }"
      + nl

    prov.LoadFromData(style) |> ignore
    handler.viewport1.StyleContext.AddProvider(prov, UInt32.MaxValue)
    handler.codeView.StyleContext.AddProvider(prov, UInt32.MaxValue)
#endif
    handler.lineView.Editable <- false
    setDefaultTags handler
    setDefaultText handler

    handler.refreshButton.Sensitive <- false

    handler.exitButton.Clicked
    |> Event.add
         (fun _ ->
           if Persistence.save then
             Persistence.saveGeometry handler.mainWindow

           Application.Quit())
    // Initialize graphics and begin
    let vicon = icons.VIcon
    handler.mainWindow.Icon <- vicon
    handler.aboutVisualizer.Icon <- vicon
    handler.aboutVisualizer.Logo <- icons.Logo.Force()
    handler.mainWindow.ShowAll()
    handler

  let parseCommandLine arguments =
    let options =
      [ ("g|geometry",
         (fun _ ->
           Persistence.clearGeometry ()
           Persistence.save <- false))
#if !NET472
        ("schemadir:", Persistence.saveSchemaDir)
#endif
        ("r|recentFiles", (fun _ -> Persistence.saveCoverageFiles [])) ]
      |> List.fold
           (fun (o: OptionSet) (p, a) ->
             o.Add(p, Resource.GetResourceString p, new System.Action<string>(a)))
           (OptionSet())

    options.Parse(arguments) |> ignore

  [<EntryPoint; STAThread>]
  let internal main arguments =
    parseCommandLine arguments
    Application.Init()

    let handler = prepareGui ()
#if !NET472
    handler.codeView.Drawn
    |> Event.add (fun _ -> latch.Set() |> ignore)

    let schemaDir = Persistence.readSchemaDir ()

    if schemaDir |> String.IsNullOrWhiteSpace |> not then
      Environment.SetEnvironmentVariable("GSETTINGS_SCHEMA_DIR", schemaDir)
#endif

    handler.mainWindow.DeleteEvent
    |> Event.add
         (fun args ->
           if Persistence.save then
             Persistence.saveGeometry handler.mainWindow

           Application.Quit()
           args.RetVal <- true)

    handler.showAboutButton.Clicked
    |> Event.add
         (fun args ->
           ignore <| handler.aboutVisualizer.Run()
           handler.aboutVisualizer.Hide())
    // The Open event
    let click =
      handleOpenClicked handler prepareOpenFileDialog

    // Selecting an item from the menu
    let select =
      handler.fileOpenMenu.AllChildren
      |> Seq.cast<MenuItem>
      |> Seq.mapi (fun n (i: MenuItem) -> i.Activated |> Event.map (fun _ -> n))

    // The sum of all these events -- we have explicitly selected a file
    let fileSelection = select |> Seq.fold Event.merge click

    let updateMRU (h: Handler) path add =
      HandlerCommon.UpdateCoverageFiles h path add
      let active = populateMenu h
      Persistence.saveCoverageFiles h.coverageFiles
      handler.refreshButton.Sensitive <- active

    // Now mix in selecting the file currently loaded
    let refresh =
      handler.refreshButton.Clicked
      |> Event.map (fun _ -> 0)

    Event.merge fileSelection refresh
    |> Event.add (doSelected handler updateMRU)

    handler.fontButton.Clicked
    |> Event.add
         (fun x ->
           let format = Resource.GetResourceString "SelectFont"
#if !NET472
           use selector =
             new FontChooserDialog(format, handler.mainWindow)

           selector.Font <- Persistence.readFont ()

           if Enum.ToObject(typeof<ResponseType>, selector.Run()) :?> ResponseType = ResponseType.Ok then
             let font = selector.Font
#else
           use selector = new FontSelectionDialog(format)

           selector.SetFontName(Persistence.readFont ())
           |> ignore

           if Enum.ToObject(typeof<ResponseType>, selector.Run()) :?> ResponseType = ResponseType.Ok then
             let font = selector.FontName
#endif

             Persistence.saveFont (font)

             [ handler.codeView; handler.lineView ]
             |> Seq.iter
                  (fun v ->
                    v.Buffer.TagTable.Foreach(fun tag -> tag.Font <- font)
                    v.PixelsAboveLines <- 0
                    v.PixelsBelowLines <- 0)

             handler.viewport1.QueueDraw()

             async {
               Threading.Thread.Sleep(300)
               Handler.InvokeOnGuiThread(fun () -> balanceLines handler)
             }
             |> Async.Start

#if !NET472
           ) // implicit Dispose()
#else
           selector.Destroy())
#endif
    // Tree selection events and such
    handler.classStructureTree.RowActivated
    |> Event.add (onRowActivated handler)

    Application.Run()
    0 // needs an int return

#if NET472
[<assembly: SuppressMessage("Microsoft.Performance",
                            "CA1810:InitializeReferenceTypeStaticFieldsInline",
                            Scope = "member",
                            Target = "<StartupCode$AltCover-Visualizer>.$Visualizer.#.cctor()",
                            Justification = "Compiler generated")>]
[<assembly: SuppressMessage("Microsoft.Reliability",
                            "CA2000:Dispose objects before losing scope",
                            Scope = "member",
                            Target = "AltCover.Gui+prepareTreeView@141.#Invoke(System.Int32,System.Lazy`1<Gdk.Pixbuf>)",
                            Justification = "Added to GUI widget tree")>]
[<assembly: SuppressMessage("Microsoft.Usage",
                            "CA2208:InstantiateArgumentExceptionsCorrectly",
                            Scope = "member",
                            Target = "AltCover.Persistence.#readCoverageFiles(AltCover.Visualizer.Handler)",
                            Justification = "Inlined library code")>]
()
#endif