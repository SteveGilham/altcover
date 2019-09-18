namespace AltCover.Visualizer

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Linq
open System.Reflection
open System.Resources
open System.Xml
open System.Xml.Linq
open System.Xml.Schema
open System.Xml.XPath

open AltCover.Augment
open AltCover.Visualizer.Extensions
open AltCover.Visualizer.GuiCommon

open Gdk
open Gtk
#if NETCOREAPP2_1
#else
open Glade
open Microsoft.Win32
#endif

open Mono.Options

type internal Handler() =
  class
#if NETCOREAPP2_1
    [<Builder.Object; DefaultValue(true)>]
    val mutable toolbar1 : Toolbar
#endif

    [<
#if NETCOREAPP2_1
      Builder.Object;
#else
      Widget;
#endif
      DefaultValue(true)>]
    val mutable mainWindow : Window

    [<
#if NETCOREAPP2_1
      Builder.Object;
#else
      Widget;
#endif
      DefaultValue(true)>]
    val mutable openButton : MenuToolButton

    [<
#if NETCOREAPP2_1
      Builder.Object;
#else
      Widget;
#endif
      DefaultValue(true)>]
    val mutable separator1 : SeparatorToolItem

    [<
#if NETCOREAPP2_1
      Builder.Object;
#else
      Widget;
#endif
      DefaultValue(true)>]
    val mutable exitButton : ToolButton

    [<
#if NETCOREAPP2_1
      Builder.Object;
#else
      Widget;
#endif
      DefaultValue(true)>]
    val mutable refreshButton : ToolButton

    [<
#if NETCOREAPP2_1
      Builder.Object;
#else
      Widget;
#endif
      DefaultValue(true)>]
    val mutable fontButton : ToolButton

    [<
#if NETCOREAPP2_1
      Builder.Object;
#else
      Widget;
#endif
      DefaultValue(true)>]
    val mutable showAboutButton : ToolButton

    [<
#if NETCOREAPP2_1
      Builder.Object;
#else
      Widget;
#endif
      DefaultValue(true)>]
    val mutable aboutVisualizer : AboutDialog

    [<
#if NETCOREAPP2_1
      Builder.Object;
#else
      Widget;
#endif
      DefaultValue(true)>]
    val mutable fileOpenMenu : Menu

    [<
#if NETCOREAPP2_1
      Builder.Object;
#else
      Widget;
#endif
      DefaultValue(true)>]
    val mutable classStructureTree : TreeView

    [<
#if NETCOREAPP2_1
      Builder.Object;
#else
      Widget;
#endif
      DefaultValue(true)>]
    val mutable codeView : TextView

    [<DefaultValue(true)>]
    val mutable coverageFiles : string list

    [<DefaultValue(true)>]
    val mutable justOpened : string

    [<DefaultValue(true)>]
    val mutable baseline : TextTag

    [<DefaultValue(true)>]
    val mutable activeRow : int
  end

module Persistence =
  let mutable internal save = true

#if NETCOREAPP2_1

  let private DefaultDocument() =
    let doc = XDocument()
    doc.Add(XElement(XName.Get "AltCover.Visualizer"))
    doc

  let private EnsureFile() =
    let profileDir = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData)
    let dir = Directory.CreateDirectory(Path.Combine(profileDir, ".altcover"))
    let file = Path.Combine(dir.FullName, "Visualizer.xml")
    if file
       |> File.Exists
       |> not
    then (file, DefaultDocument())
    else
      try
        let doc = XDocument.Load(file)
        try
          let schemas = new XmlSchemaSet()
          use xsd =
            new StreamReader(Assembly.GetExecutingAssembly()
                                     .GetManifestResourceStream("AltCover.Visualizer.config.xsd"))
          schemas.Add(String.Empty, XmlReader.Create xsd) |> ignore
          doc.Validate(schemas, null)
          (file, doc)
        with xx ->
          printfn "%A\r\n\r\n%A" xx doc
          (file, DefaultDocument())
      with x ->
        printfn "%A" x
        (file, DefaultDocument())

  let internal saveFont (font : string) =
    let file, config = EnsureFile()
    config.XPathSelectElements("//Font")
    |> Seq.toList
    |> Seq.iter (fun x -> x.Remove())
    let inject = XElement(XName.Get "Font", font)
    match config.XPathSelectElements("//CoveragePath") |> Seq.toList with
    | [] -> (config.FirstNode :?> XElement).AddFirst(inject)
    | x :: _ -> inject |> x.AddAfterSelf
    config.Save file

  let internal readFont() =
    let _, config = EnsureFile()
    match config.XPathSelectElements("//Font") |> Seq.toList with
    | [] -> "Monospace 10"
    | x :: _ -> x.FirstNode.ToString()

  let internal saveFolder (path : string) =
    let file, config = EnsureFile()
    match config.XPathSelectElements("//CoveragePath") |> Seq.toList with
    | [] ->
      (config.FirstNode :?> XElement).AddFirst(XElement(XName.Get "CoveragePath", path))
    | x :: _ ->
      x.RemoveAll()
      x.Add path
    config.Save file

  let internal readFolder() =
    let _, config = EnsureFile()
    match config.XPathSelectElements("//CoveragePath") |> Seq.toList with
    | [] -> System.IO.Directory.GetCurrentDirectory()
    | x :: _ -> x.FirstNode.ToString()

  let internal saveCoverageFiles (coverageFiles : string list) =
    let file, config = EnsureFile()
    config.XPathSelectElements("//RecentlyOpened")
    |> Seq.toList
    |> Seq.iter (fun x -> x.Remove())
    let inject = config.FirstNode :?> XElement
    coverageFiles
    |> Seq.iter (fun path -> inject.Add(XElement(XName.Get "RecentlyOpened", path)))
    config.Save file

  let internal readCoverageFiles (handler : Handler) =
    let _, config = EnsureFile()

    let files =
      config.XPathSelectElements("//RecentlyOpened")
      |> Seq.map (fun n -> n.FirstNode.ToString())
      |> Seq.toList
    handler.coverageFiles <- files

  let saveGeometry (w : Window) =
    let file, config = EnsureFile()
    config.XPathSelectElements("//Geometry")
    |> Seq.toList
    |> Seq.iter (fun x -> x.Remove())
    let (x, y) = w.GetPosition()
    let (width, height) = w.GetSize()
    let element =
      XElement
        (XName.Get "Geometry",
         XAttribute(XName.Get "x", x),
         XAttribute(XName.Get "y", y),
         XAttribute(XName.Get "width", width),
         XAttribute(XName.Get "height", height))

    match config.XPathSelectElements("//RecentlyOpened") |> Seq.toList with
    | [] -> (config.FirstNode :?> XElement).Add element
    | x :: _ -> x.AddBeforeSelf element
    config.Save file

  let readGeometry (w : Window) =
    let _, config = EnsureFile()

    let attribute (x : XElement) a =
      x.Attribute(XName.Get a).Value
      |> Double.TryParse
      |> snd
    config.XPathSelectElements("//Geometry")
    |> Seq.iter (fun e ->
         let width = Math.Max(attribute e "width" |> int, 600)
         let height = Math.Max(attribute e "height" |> int, 450)
         let bounds = w.Display.PrimaryMonitor.Geometry
         let x = Math.Min(Math.Max(attribute e "x" |> int, 0), bounds.Width - width)
         let y = Math.Min(Math.Max(attribute e "y" |> int, 0), bounds.Height - height)
         w.DefaultHeight <- height
         w.DefaultWidth <- width
         w.Move(x, y))

  let clearGeometry() =
    let file, config = EnsureFile()
    config.XPathSelectElements("//Geometry")
    |> Seq.toList
    |> Seq.iter (fun f -> f.Remove())
    config.Save file
#else
  let internal geometry = "SOFTWARE\\AltCover\\Visualizer\\Geometry"
  let internal recent = "SOFTWARE\\AltCover\\Visualizer\\Recently Opened"
  let internal coveragepath = "SOFTWARE\\AltCover\\Visualizer"

  let internal saveFolder (path : string) =
    use key = Registry.CurrentUser.CreateSubKey(coveragepath)
    key.SetValue("path", path)

  let internal readFolder() =
    use key = Registry.CurrentUser.CreateSubKey(coveragepath)
    key.GetValue("path", System.IO.Directory.GetCurrentDirectory()) :?> string

  let internal saveFont (font : string) =
    use key = Registry.CurrentUser.CreateSubKey(coveragepath)
    key.SetValue("font", font)

  let internal readFont() =
    use key = Registry.CurrentUser.CreateSubKey(coveragepath)
    key.GetValue("font", "Monospace Normal 10") :?> string

  let internal saveGeometry (w : Window) =
    use key = Registry.CurrentUser.CreateSubKey(geometry)
    let (x, y) = w.GetPosition()
    key.SetValue("x", x)
    key.SetValue("y", y)
    let (width, height) = w.GetSize()
    key.SetValue("width", width)
    key.SetValue("height", height)

  let internal readGeometry (w : Window) =
    use key = Registry.CurrentUser.CreateSubKey(geometry)
    let width = Math.Max(key.GetValue("width", 600) :?> int, 600)
    let height = Math.Max(key.GetValue("height", 450) :?> int, 450)
    let bounds = System.Windows.Forms.Screen.PrimaryScreen.WorkingArea
    let x = Math.Min(Math.Max(key.GetValue("x", (bounds.Width - width) / 2) :?> int, 0), bounds.Width - width)
    let y = Math.Min(Math.Max(key.GetValue("y", (bounds.Height - height) / 2) :?> int, 0), bounds.Height - height)
    w.DefaultHeight <- height
    w.DefaultWidth <- width
    w.Move(x, y)

  let internal readCoverageFiles (handler : Handler) =
    use fileKey = Registry.CurrentUser.CreateSubKey(recent)
    let KeyToValue (key : RegistryKey) (n : string) = key.GetValue(n, String.Empty)

    let names =
      fileKey.GetValueNames()
      |> Array.filter (fun (s : string) -> s.Length = 1 && Char.IsDigit(s.Chars(0)))
      |> Array.sortBy (fun s -> Int32.TryParse(s) |> snd)

    let files =
      names
      |> Array.map (KeyToValue fileKey)
      |> Seq.cast<string>
      |> Seq.filter (fun n -> not (String.IsNullOrWhiteSpace(n)))
      |> Seq.filter (fun n -> File.Exists(n))
      |> Seq.toList

    handler.coverageFiles <- files

  let saveCoverageFiles files =
    // Update the recent files menu and registry store from memory cache
    // with new most recent file
    let RegDeleteKey (key : RegistryKey) (name : string) = key.DeleteValue(name)
    let RegSetKey (key : RegistryKey) (index : int) (name : string) = key.SetValue(index.ToString(), name)
    use fileKey = Registry.CurrentUser.CreateSubKey(recent)
    fileKey.GetValueNames() |> Seq.iter (RegDeleteKey fileKey)
    files |> Seq.iteri (RegSetKey fileKey)

  let clearGeometry () =
    do
      use k1 = Registry.CurrentUser.CreateSubKey(geometry)
      ()
    Registry.CurrentUser.DeleteSubKeyTree(geometry)
#endif

module Gui =
  // --------------------------  General Purpose ---------------------------
  // Safe event dispatch => GUI update
  let private InvokeOnGuiThread(action : unit -> unit) =
    Gtk.Application.Invoke(fun (o : obj) (e : EventArgs) -> action())

  let private GetResourceString(key : string) =
    let executingAssembly = System.Reflection.Assembly.GetExecutingAssembly()
    let resources = ResourceManager("AltCover.Visualizer.Resource", executingAssembly)
    resources.GetString(key)

  let private XmlIcon =
    lazy (new Pixbuf(Assembly.GetExecutingAssembly()
                             .GetManifestResourceStream("AltCover.Visualizer.XMLFile_16x.png")))
  let private AssemblyIcon =
    lazy (new Pixbuf(Assembly.GetExecutingAssembly()
                             .GetManifestResourceStream("AltCover.Visualizer.Assembly_6212.png")))
  let private NamespaceIcon =
    lazy (new Pixbuf(Assembly.GetExecutingAssembly()
                             .GetManifestResourceStream("AltCover.Visualizer.Namespace_16x.png")))
  let private ClassIcon =
    lazy (new Pixbuf(Assembly.GetExecutingAssembly()
                             .GetManifestResourceStream("AltCover.Visualizer.class_16xLG.png")))
  let private MethodIcon =
    lazy (new Pixbuf(Assembly.GetExecutingAssembly()
                             .GetManifestResourceStream("AltCover.Visualizer.method_16xLG.png")))
  let branched =
    lazy (new Pixbuf(Assembly.GetExecutingAssembly()
                             .GetManifestResourceStream("AltCover.Visualizer.Branch_12x_16x_grn.png")))
  let branch =
    lazy (new Pixbuf(Assembly.GetExecutingAssembly()
                             .GetManifestResourceStream("AltCover.Visualizer.Branch_12x_16x_ylw.png")))
  let redbranch =
    lazy (new Pixbuf(Assembly.GetExecutingAssembly()
                             .GetManifestResourceStream("AltCover.Visualizer.Branch_12x_16x_red.png")))
  let blank =
    lazy (new Pixbuf(Assembly.GetExecutingAssembly()
                             .GetManifestResourceStream("AltCover.Visualizer.Blank_12x_16x.png")))
  // --------------------------  Persistence ---------------------------
  // -------------------------- Tree View ---------------------------
  let Mappings = new Dictionary<TreePath, XPathNavigator>()

  let private PopulateClassNode (model : TreeStore) (row : TreeIter)
      (nodes : seq<MethodKey>) =
    let ApplyToModel (theModel : TreeStore) (theRow : TreeIter) (x : MethodKey) =
      let fullname = x.m.GetAttribute("fullname", String.Empty)

      let args =
        if String.IsNullOrEmpty(fullname) || x.name.IndexOf('(') > 0 then String.Empty
        else
          let bracket = fullname.IndexOf('(')
          if bracket < 0 then String.Empty
          else fullname.Substring(bracket)

      let displayname = x.name + args

      let offset =
        match displayname.LastIndexOf("::", StringComparison.Ordinal) with
        | -1 -> 0
        | o -> o + 2

      let newrow =
        theModel.AppendValues(theRow,
                              [| displayname.Substring(offset) :> obj
                                 MethodIcon.Force() :> obj |])

      Mappings.Add(theModel.GetPath(newrow), x.m)

    let methods = nodes |> Seq.toArray
    Array.sortInPlaceWith MethodNameCompare methods
    methods |> Array.iter (ApplyToModel model row)

  let private PopulateNamespaceNode (model : TreeStore) (row : TreeIter)
      (nodes : seq<MethodKey>) =
    let ApplyToModel (theModel : TreeStore) (theRow : TreeIter)
        (group : string * seq<MethodKey>) =
      let name = fst group

      let newrow =
        theModel.AppendValues(theRow,
                              [| name :> obj
                                 ClassIcon.Force() :> obj |])
      PopulateClassNode theModel newrow (snd group)
      newrow

    let isNested (name : string) n =
      name.StartsWith(n + "+", StringComparison.Ordinal)
      || name.StartsWith(n + "/", StringComparison.Ordinal)
    nodes
    |> Seq.groupBy (fun x -> x.classname)
    |> Seq.sortBy fst
    |> Seq.fold (fun stack c ->
         let name = fst c
         let restack = stack |> List.filter (fst >> (isNested name))

         let pr =
           match restack with
           | [] -> row
           | (_, r) :: _ -> r

         let nr = ApplyToModel model pr c
         (name, nr) :: restack) []
    |> ignore

  let private PopulateAssemblyNode (model : TreeStore) (row : TreeIter)
      (node : XPathNavigator) =
    // within the <module> we have <method> nodes with name="get_module" class="AltCover.Coverage.CoverageSchema.coverage"
    let ApplyToModel (theModel : TreeStore) (theRow : TreeIter)
        (group : string * seq<MethodKey>) =
      let name = fst group

      let newrow =
        theModel.AppendValues(theRow,
                              [| name :> obj
                                 NamespaceIcon.Force() :> obj |])
      PopulateNamespaceNode theModel newrow (snd group)

    let methods =
      node.SelectChildren("method", String.Empty)
      |> Seq.cast<XPathNavigator>
      |> Seq.map (fun m ->
           let classfullname = m.GetAttribute("class", String.Empty)
           let lastdot = classfullname.LastIndexOf('.')
           { m = m
             spacename =
               if lastdot < 0 then String.Empty
               else classfullname.Substring(0, lastdot)
             classname =
               if lastdot < 0 then classfullname
               else classfullname.Substring(1 + lastdot)
             name = m.GetAttribute("name", String.Empty) })
      |> Seq.groupBy (fun x -> x.spacename)
      |> Seq.sortBy fst

    methods |> Seq.iter (ApplyToModel model row)

  // -------------------------- Message Boxes ---------------------------
  let private ShowMessage (parent : Window) (message : string) (messageType : MessageType) =
    use md =
      new MessageDialog(parent, DialogFlags.Modal ||| DialogFlags.DestroyWithParent,
                        messageType, ButtonsType.Close, message)
    md.Icon <- parent.Icon
    md.Title <- "AltCover.Visualizer"
    md.Run() |> ignore
#if NETCOREAPP2_1
    // implicit Dispose()
#else
    md.Destroy()
#endif

  let private ShowMessageOnGuiThread (parent : Window) (severity : MessageType) message =
    let SendMessageToWindow() = ShowMessage parent message severity
    InvokeOnGuiThread(SendMessageToWindow)

  let private InvalidCoverageFileMessage (parent : Window) (x : InvalidFile) =
    let format = GetResourceString("InvalidFile")
    let message =
      String.Format
        (System.Globalization.CultureInfo.CurrentCulture, format, x.File.FullName,
         x.Fault.Message)
    ShowMessageOnGuiThread parent MessageType.Error message

  let private OutdatedCoverageFileMessage (parent : Window) (x : FileInfo) =
    let format = GetResourceString("CoverageOutOfDate")
    let message =
      String.Format(System.Globalization.CultureInfo.CurrentCulture, format, x.FullName)
    ShowMessageOnGuiThread parent MessageType.Warning message

  let private MissingSourceFileMessage (parent : Window) (x : FileInfo) =
    let format = GetResourceString("MissingSourceFile")
    let message =
      String.Format(System.Globalization.CultureInfo.CurrentCulture, format, x.FullName)
    ShowMessageOnGuiThread parent MessageType.Warning message

  let private OutdatedCoverageThisFileMessage (parent : Window) (c : FileInfo)
      (s : Source) =
    let format = GetResourceString("CoverageOutOfDateThisFile")
    let message =
      String.Format
        (System.Globalization.CultureInfo.CurrentCulture, format, c.FullName, s.FullName)
    ShowMessageOnGuiThread parent MessageType.Warning message

  let private MissingSourceThisFileMessage (parent : Window) (c : FileInfo) (s : Source) =
    let format = GetResourceString("MissingSourceThisFile")
    let message =
      String.Format
        (System.Globalization.CultureInfo.CurrentCulture, format, c.FullName, s.FullName)
    ShowMessageOnGuiThread parent MessageType.Warning message

  // -------------------------- UI set-up  ---------------------------
  let private InitializeHandler() =
    let handler = Handler()
    [ "mainWindow"; "fileOpenMenu"; "aboutVisualizer" ]
#if NETCOREAPP2_1
    |> List.iter (fun name ->
         use b =
           new Builder(Assembly.GetExecutingAssembly()
                               .GetManifestResourceStream("AltCover.Visualizer.Visualizer3.glade"),
                       name)
         b.Autoconnect handler)
#else
    |> List.iter (fun name -> let xml = new Glade.XML("AltCover.Visualizer.Visualizer.glade", name)
                              xml.Autoconnect(handler))
#endif

    handler.coverageFiles <- []
    handler

  // Fill in the menu from the memory cache
  let private populateMenu (handler : Handler) =
    let items = handler.fileOpenMenu.AllChildren |> Seq.cast<MenuItem>
    // blank the whole menu
    items
    |> Seq.iter (fun (i : MenuItem) ->
         i.Visible <- false
         (i.Child :?> Label).Text <- String.Empty)
    // fill in with the items we have
    Seq.zip handler.coverageFiles items
    |> Seq.iter (fun (name, item) ->
         item.Visible <- true
         (item.Child :?> Label).Text <- name)
    // set or clear the menu
    handler.openButton.Menu <- if handler.coverageFiles.IsEmpty then null
                               else handler.fileOpenMenu :> Widget

  let private PrepareAboutDialog(handler : Handler) =
    let ShowUrl(link : string) =
      match System.Environment.GetEnvironmentVariable("OS") with
      | "Windows_NT" -> System.Diagnostics.Process.Start(link) |> ignore
      // TODO -- other OS types
      | _ -> ShowMessage handler.aboutVisualizer link MessageType.Info
    // The first gets the display right, the second the browser launch

#if NETCOREAPP2_1
    handler.aboutVisualizer.TransientFor <- handler.mainWindow
#else
    AboutDialog.SetUrlHook(fun _ link -> ShowUrl link) |> ignore
    LinkButton.SetUriHook(fun _ link -> ShowUrl link) |> ignore
    handler.aboutVisualizer.ActionArea.Children.OfType<Button>()
    |> Seq.iter (fun w ->
         let t = GetResourceString w.Label
         if t
            |> String.IsNullOrWhiteSpace
            |> not
         then w.Label <- t)
#endif

    handler.aboutVisualizer.Title <- GetResourceString("aboutVisualizer.Title")
    handler.aboutVisualizer.Modal <- true
    handler.aboutVisualizer.WindowPosition <- WindowPosition.Mouse
    handler.aboutVisualizer.Version <- System.AssemblyVersionInformation.AssemblyFileVersion
    handler.aboutVisualizer.Copyright <- String.Format
                                           (System.Globalization.CultureInfo.CurrentCulture,
                                            GetResourceString("aboutVisualizer.Copyright"),
                                            System.AssemblyVersionInformation.AssemblyCopyright)
    handler.aboutVisualizer.License <- String.Format
                                         (System.Globalization.CultureInfo.CurrentCulture,
                                          handler.aboutVisualizer.License,
                                          System.AssemblyVersionInformation.AssemblyCopyright)
    handler.aboutVisualizer.Comments <- GetResourceString("aboutVisualizer.Comments")
    handler.aboutVisualizer.WebsiteLabel <- GetResourceString
                                              ("aboutVisualizer.WebsiteLabel")

  let private PrepareTreeView(handler : Handler) =
    [| AssemblyIcon; NamespaceIcon; ClassIcon; MethodIcon |]
    |> Seq.iteri (fun i x ->
         let column = new Gtk.TreeViewColumn()
         let cell = new Gtk.CellRendererText()
         let icon = new Gtk.CellRendererPixbuf()
         column.PackStart(icon, true)
         column.PackEnd(cell, true)
         handler.classStructureTree.AppendColumn(column) |> ignore
         column.AddAttribute(cell, "text", 2 * i)
         column.AddAttribute(icon, "pixbuf", 1 + (2 * i)))

#if NETCOREAPP2_1
  let private PrepareOpenFileDialog(handler : Handler) =
    let openFileDialog =
      new FileChooserDialog(GetResourceString "OpenFile", handler.mainWindow,
                            FileChooserAction.Open, GetResourceString "OpenFile.Open",
                            ResponseType.Ok, GetResourceString "OpenFile.Cancel",
                            ResponseType.Cancel, null)
    let data = GetResourceString("SelectXml").Split([| '|' |])
    let filter = new FileFilter()
    filter.Name <- data.[0]
    filter.AddPattern data.[1]
    openFileDialog.AddFilter filter

    let filter = new FileFilter()
    filter.Name <- data.[2]
    filter.AddPattern data.[3]
    openFileDialog.AddFilter filter
    openFileDialog
#else
  [<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Reliability", "CA2000:DisposeObjectsBeforeLosingScope",
                                                    Justification = "'openFileDialog' is returned")>]
  let private PrepareOpenFileDialog() =
    let openFileDialog = new System.Windows.Forms.OpenFileDialog()
    openFileDialog.InitialDirectory <- Persistence.readFolder()
    openFileDialog.Filter <- GetResourceString("SelectXml")
    openFileDialog.FilterIndex <- 0
    openFileDialog.RestoreDirectory <- false
    openFileDialog
#endif

  // -------------------------- Event handling  ---------------------------
  let private HandleOpenClicked (handler : Handler)
#if NETCOREAPP2_1

      (openFileDialogFactory : Handler -> FileChooserDialog) =
    let openFileDialog = openFileDialogFactory handler
#else
                                (openFileDialogFactory : unit -> System.Windows.Forms.OpenFileDialog) =
    use openFileDialog = openFileDialogFactory ()
#endif

#if NETCOREAPP2_1
    let MakeSelection (ofd : FileChooserDialog) x =
      openFileDialog.SetCurrentFolder(Persistence.readFolder()) |> ignore
      try
        if Enum.ToObject(typeof<ResponseType>, ofd.Run()) :?> ResponseType = ResponseType.Ok then
          let file = new FileInfo(ofd.Filename)
          let dir = file.Directory.FullName
#else
    let MakeSelection (ofd : System.Windows.Forms.OpenFileDialog) x =
        if ofd.ShowDialog() = System.Windows.Forms.DialogResult.OK then
          let file = new FileInfo(ofd.FileName)
          let dir = file.Directory.FullName
          ofd.InitialDirectory <- dir
#endif
          if Persistence.save then Persistence.saveFolder dir
          Some file
        else None
#if NETCOREAPP2_1
      finally
        ofd.Hide()
#endif

    handler.openButton.Clicked
    |> Event.map (MakeSelection openFileDialog)
    |> Event.choose id
    |> Event.map (fun info ->
         handler.justOpened <- info.FullName
         -1)

  [<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Reliability",
                                                    "CA2000:DisposeObjectsBeforeLosingScope",
                                                    Justification = "'baseline' is returned")>]
  let private InitializeTextBuffer(buff : TextBuffer) =
    let Tag (buffer : TextBuffer) (style : string, fg, bg) =
      let tag = new TextTag(style)
      tag.Foreground <- fg
      tag.Background <- bg
      buffer.TagTable.Add(tag)

    let baseline = new TextTag("baseline")
    baseline.Font <- Persistence.readFont()
    baseline.Foreground <- "#c0c0c0"
    buff.TagTable.Add(baseline) |> ignore
    [ (// Last declared type is last layer painted
       "visited", "#404040", "#cefdce") // "#98FB98") ; // Dark on Pale Green
      ("declared", "#FFA500", "#FFFFFF") // Orange on White
      ("static", "#808080", "#F5F5F5") // Grey on White Smoke
      ("automatic", "#808080", "#FFFF00") // Grey on Yellow
      ("notVisited", "#ff0000", "#FFFFFF") // Red on White
      ("excluded", "#87CEEB", "#FFFFFF") // Sky Blue on white
                                         ]
    |> Seq.iter (fun x -> Tag buff x |> ignore)
    baseline

  let private ParseIntegerAttribute (element : XPathNavigator) (attribute : string) =
    let text = element.GetAttribute(attribute, String.Empty)
    let number = Int32.TryParse(text, NumberStyles.None, CultureInfo.InvariantCulture)
    if (fst number) then snd number
    else
      if not <| String.IsNullOrEmpty(text) then
        System.Diagnostics.Debug.WriteLine
          ("ParseIntegerAttribute : '" + attribute + "' with value '" + text)
      0

  [<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Reliability",
                                                    "CA2000:DisposeObjectsBeforeLosingScope",
                                                    Justification = "IDisposables are added to the TextView")>]
  let private MarkBranches (root : XPathNavigator) (codeView : TextView)
      (filename : string) =
    let buff = codeView.Buffer
    let branches = new Dictionary<int, int * int>()
    root.Select("//method")
    |> Seq.cast<XPathNavigator>
    |> Seq.filter
         (fun n ->
         let f = n.Clone()
         f.MoveToFirstChild()
         && filename.Equals
              (f.GetAttribute("document", String.Empty),
               StringComparison.OrdinalIgnoreCase))
    |> Seq.collect (fun n -> n.Select("./branch") |> Seq.cast<XPathNavigator>)
    |> Seq.groupBy (fun n -> n.GetAttribute("line", String.Empty))
    |> Seq.iter (fun n ->
         let line = ParseIntegerAttribute ((snd n) |> Seq.head) "line"
         let num = (snd n) |> Seq.length

         let v =
           (snd n)
           |> Seq.filter (fun x -> x.GetAttribute("visitcount", String.Empty) <> "0")
           |> Seq.length
         branches.Add(line, (v, num)))
    for l in 1..buff.LineCount do
      let counts = branches.TryGetValue l

      let (|AllVisited|_|) (b, (v, num)) =
        if b
           |> not
           || v <> num then None
        else Some()

      let pix =
        match counts with
        | (false, _) -> blank
        | (_, (0, _)) -> redbranch
        | AllVisited -> branched
        | _ -> branch

      let mutable i = buff.GetIterAtLine(l - 1)
      let a = new TextChildAnchor()
      buff.InsertChildAnchor(&i, a)
      let image = new Image(pix.Force())
      image.Visible <- true
      codeView.AddChildAtAnchor(image, a)
      if fst counts then
        let v, num = snd counts
        image.TooltipText <- String.Format
                               (System.Globalization.CultureInfo.CurrentCulture,
                                GetResourceString "branchesVisited", v, num)

  let internal (|Select|_|) (pattern : String) offered =
    if (fst offered)
       |> String.IsNullOrWhiteSpace
       |> not
       && pattern.StartsWith(fst offered, StringComparison.Ordinal)
    then Some offered
    else None

  let private SelectStyle because excluded =
    match (because, excluded) with
    | Select "author declared (" _ -> Exemption.Declared
    | Select "tool-generated: " _ -> Exemption.Automatic
    | Select "static analysis: " _ -> Exemption.StaticAnalysis
    | (_, true) -> Exemption.Excluded
    | _ -> 0

  let private CoverageToTag(n : XPathNavigator) =
    let excluded = Boolean.TryParse(n.GetAttribute("excluded", String.Empty)) |> snd
    let visitcount = Int32.TryParse(n.GetAttribute("visitcount", String.Empty)) |> snd
    let line = n.GetAttribute("line", String.Empty)
    let column = n.GetAttribute("column", String.Empty)
    let endline = n.GetAttribute("endline", String.Empty)
    let endcolumn = n.GetAttribute("endcolumn", String.Empty)
    // Extension behaviour for textual signalling for three lines
    n.MoveToParent() |> ignore
    let because = n.GetAttribute("excluded-because", String.Empty)
    let fallback = SelectStyle because excluded
    { visitcount =
        if visitcount = 0 then fallback
        else visitcount
      line = Int32.TryParse(line) |> snd
      column = (Int32.TryParse(column) |> snd) + 1
      endline = Int32.TryParse(endline) |> snd
      endcolumn = (Int32.TryParse(endcolumn) |> snd) + 1 }

  let private FilterCoverage (buff : TextBuffer) (n : CodeTag) =
    let lc = buff.LineCount
    n.line > 0 && n.endline > 0 && n.line <= lc && n.endline <= lc

  let private TagByCoverage (buff : TextBuffer) (n : CodeTag) =
    // bound by current line length in case we're looking from stale coverage
    let line = buff.GetIterAtLine(n.line - 1)

    let from =
      if line.CharsInLine = 0 then line
      else buff.GetIterAtLineOffset(n.line - 1, Math.Min(n.column, line.CharsInLine) - 1)

    let endline = buff.GetIterAtLine(n.endline - 1)

    let until =
      if endline.CharsInLine = 0 then endline
      else
        buff.GetIterAtLineOffset
          (n.endline - 1, Math.Min(n.endcolumn, endline.CharsInLine) - 1)

    let tag =
      match n.visitcount with
      | 0 -> "notVisited"
      | Exemption.Declared -> "declared"
      | Exemption.Automatic -> "automatic"
      | Exemption.StaticAnalysis -> "static"
      | Exemption.Excluded -> "excluded"
      | _ -> "visited"

    buff.ApplyTag(tag, from, until)

  let private MarkCoverage (root : XPathNavigator) buff filename =
    root.Select("//seqpnt[@document='" + filename + "']")
    |> Seq.cast<XPathNavigator>
    |> Seq.map CoverageToTag
    |> Seq.filter (FilterCoverage buff)
    |> Seq.iter (TagByCoverage buff)

  let internal ScrollToRow (h : Handler) _ =
    let buff = h.codeView.Buffer
    if buff
       |> isNull
       |> not
       && h.activeRow > 0
    then
      let iter = buff.GetIterAtLine(h.activeRow - 1)
      let mark = buff.CreateMark("line", iter, false)
      h.codeView.ScrollToMark(mark, 0.0, true, 0.0, 0.3)
      buff.DeleteMark("line")

  let latch = new Threading.ManualResetEvent false

  let private OnRowActivated (handler : Handler) (activation : RowActivatedArgs) =
    let HitFilter (activated : RowActivatedArgs) (path : TreePath) =
      activated.Path.Compare(path) = 0
    let hits = Mappings.Keys |> Seq.filter (HitFilter activation)
    if not (Seq.isEmpty hits) then
      let m = Mappings.[Seq.head hits]
      let points = m.SelectChildren("seqpnt", String.Empty) |> Seq.cast<XPathNavigator>
      if Seq.isEmpty points then
        let noSource() =
          let message =
            String.Format
              (CultureInfo.CurrentCulture, GetResourceString "No source location",
               (activation.Column.Cells.[1] :?> Gtk.CellRendererText)
                 .Text.Replace("<", "&lt;").Replace(">", "&gt;"))
          ShowMessageOnGuiThread handler.mainWindow MessageType.Info message
        noSource()
      else
        let child = points |> Seq.head
        let filename = child.GetAttribute("document", String.Empty)
        handler.mainWindow.Title <- "AltCover.Visualizer - " + filename
        let info = GetSource(filename)
        let current = new FileInfo(handler.coverageFiles.Head)
        if (not <| info.Exists) then
          MissingSourceThisFileMessage handler.mainWindow current info
        else if (info.Outdated current.LastWriteTimeUtc) then
          OutdatedCoverageThisFileMessage handler.mainWindow current info
        else
          let showSource() =
            let buff = handler.codeView.Buffer
            buff.Text <- info.ReadAllText()
            buff.ApplyTag("baseline", buff.StartIter, buff.EndIter)
            let line = child.GetAttribute("line", String.Empty)
            let root = m.Clone()
            root.MoveToRoot()
            MarkBranches root handler.codeView filename
            MarkCoverage root buff filename
            handler.activeRow <- Int32.TryParse(line) |> snd
            handler.codeView.CursorVisible <- false
            handler.codeView.QueueDraw()
#if NETCOREAPP2_1
            async {
              Threading.Thread.Sleep(300)
              ScrollToRow handler ()
            }
            |> Async.Start
#else
            ScrollToRow handler ()
#endif

          showSource()

  [<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Reliability",
                                                    "CA2000:DisposeObjectsBeforeLosingScope",
                                                    Justification = "IDisposables are added to other widgets")>]
  let private AddLabelWidget g (button : ToolButton, resource) =
    let keytext = (resource |> GetResourceString).Split('\u2028')

    let key =
      Keyval.FromName(keytext.[0].Substring(0, 1))
      |> int
      |> enum<Gdk.Key>

    button.AddAccelerator
      ("clicked", g, new AccelKey(key, ModifierType.Mod1Mask, AccelFlags.Visible))

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
    buffer.ApplyTag
      ("underline", buffer.GetIterAtLineOffset(0, start),
       buffer.GetIterAtLineOffset(0, start + 1))

    label.CursorVisible <- false
    label.Editable <- false
    button.Label <- null
    button.LabelWidget <- label

  [<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Reliability",
                                                    "CA2000:DisposeObjectsBeforeLosingScope",
                                                    Justification = "IDisposables are added to other widgets")>]
  let private SetToolButtons(h : Handler) =
    let g = new AccelGroup()
    h.mainWindow.AddAccelGroup(g)
#if NETCOREAPP2_1
    h.toolbar1.ToolbarStyle <- ToolbarStyle.Both
    let prov = new CssProvider()
    let style = """
* {
    background-color: white;
}
"""
    prov.LoadFromData(style) |> ignore
    h.toolbar1.StyleContext.AddProvider(prov, UInt32.MaxValue)
#endif

    [ (h.openButton :> ToolButton, "openButton.Label")
      (h.refreshButton, "refreshButton.Label")
      (h.fontButton, "fontButton.Label")
      (h.showAboutButton, "showAboutButton.Label")
      (h.exitButton, "exitButton.Label") ]
    |> Seq.iter (AddLabelWidget g)
    h.fileOpenMenu.AllChildren
    |> Seq.cast<MenuItem>
    |> Seq.iteri
         (fun n (i : MenuItem) ->
         let c = ((n + 1) % 10) |> char

         let key =
           Keyval.FromName(String [| '0' + c |])
           |> int
           |> enum<Gdk.Key>
         i.AddAccelerator
           ("activate", g, new AccelKey(key, ModifierType.Mod1Mask, AccelFlags.Visible)))

  let private PrepareGui() =
    let handler = InitializeHandler()
    SetToolButtons handler
    PrepareAboutDialog handler
    PrepareTreeView handler
    Persistence.readGeometry handler.mainWindow
    Persistence.readCoverageFiles handler
    populateMenu handler
    handler.separator1.Expand <- true
    handler.separator1.Homogeneous <- false
    handler.codeView.Editable <- false
    handler.baseline <- InitializeTextBuffer handler.codeView.Buffer
    handler.refreshButton.Sensitive <- false
    handler.exitButton.Clicked
    |> Event.add (fun _ ->
         if Persistence.save then Persistence.saveGeometry handler.mainWindow
         Application.Quit())
    // Initialize graphics and begin
    handler.mainWindow.Icon <- new Pixbuf(Assembly.GetExecutingAssembly()
                                                  .GetManifestResourceStream("AltCover.Visualizer.VIcon.ico"))
    handler.aboutVisualizer.Icon <- new Pixbuf(Assembly.GetExecutingAssembly()
                                                       .GetManifestResourceStream("AltCover.Visualizer.VIcon.ico"))
    handler.aboutVisualizer.Logo <- new Pixbuf(Assembly.GetExecutingAssembly()
                                                       .GetManifestResourceStream("AltCover.Visualizer.logo.png"))
    handler.mainWindow.ShowAll()
    handler

  let ParseCommandLine arguments =
    let options =
      [ ("g|geometry",
         (fun _ ->
         Persistence.clearGeometry()
         Persistence.save <- false))
        ("r|recentFiles", (fun _ -> Persistence.saveCoverageFiles [])) ]
      |> List.fold
           (fun (o : OptionSet) (p, a) ->
           o.Add(p, GetResourceString p, new System.Action<string>(a))) (OptionSet())
    options.Parse(arguments) |> ignore

  [<EntryPoint; STAThread>]
  let internal Main arguments =
    ParseCommandLine arguments
    Application.Init()
    let handler = PrepareGui()
#if NETCOREAPP2_1
    handler.codeView.Drawn |> Event.add (fun _ -> latch.Set() |> ignore)
#endif

    handler.mainWindow.DeleteEvent
    |> Event.add (fun args ->
         if Persistence.save then Persistence.saveGeometry handler.mainWindow
         Application.Quit()
         args.RetVal <- true)
    handler.showAboutButton.Clicked
    |> Event.add (fun args ->
         ignore <| handler.aboutVisualizer.Run()
         handler.aboutVisualizer.Hide())
    // The Open event
    let click = HandleOpenClicked handler PrepareOpenFileDialog

    // Selecting an item from the menu
    let select =
      handler.fileOpenMenu.AllChildren
      |> Seq.cast<MenuItem>
      |> Seq.mapi (fun n (i : MenuItem) -> i.Activated |> Event.map (fun _ -> n))

    // The sum of all these events -- we have explicitly selected a file
    let fileSelection = select |> Seq.fold Event.merge click

    let updateMRU (h : Handler) path add =
      let casematch =
        match System.Environment.GetEnvironmentVariable("OS") with
        | "Windows_NT" -> StringComparison.OrdinalIgnoreCase
        | _ -> StringComparison.Ordinal

      let files =
        h.coverageFiles
        |> List.filter (fun n -> not (n.Equals(path, casematch)))
        |> Seq.truncate (9)
        |> Seq.toList

      h.coverageFiles <- (if add then (path :: files)
                          else files)
                         |> Seq.distinctBy (fun n ->
                              match casematch with
                              | StringComparison.Ordinal -> n
                              | _ -> n.ToUpperInvariant())
                         |> Seq.toList
      populateMenu h
      Persistence.saveCoverageFiles h.coverageFiles
      handler.refreshButton.Sensitive <- h.coverageFiles.Any()

    // Now mix in selecting the file currently loaded
    let refresh = handler.refreshButton.Clicked |> Event.map (fun _ -> 0)
    Event.merge fileSelection refresh
    |> Event.add (fun index ->
         let h = handler
         async {
           let current =
             FileInfo(if index < 0 then h.justOpened
                      else h.coverageFiles.[index])
           match CoverageFile.LoadCoverageFile current with
           | Left failed ->
             InvalidCoverageFileMessage h.mainWindow failed
             InvokeOnGuiThread(fun () -> updateMRU h current.FullName false)
           | Right coverage ->
             // check if coverage is newer that the source files
             let sourceFiles =
               coverage.Document.CreateNavigator().Select("//seqpnt/@document")
               |> Seq.cast<XPathNavigator>
               |> Seq.map (fun x -> x.Value)
               |> Seq.distinct

             let missing =
               sourceFiles
               |> Seq.map GetSource
               |> Seq.filter (fun f -> not f.Exists)

             if not (Seq.isEmpty missing) then
               MissingSourceFileMessage h.mainWindow current
             let newer =
               sourceFiles
               |> Seq.map GetSource
               |> Seq.filter
                    (fun f -> f.Exists && f.Outdated current.LastWriteTimeUtc)
             // warn if not
             if not (Seq.isEmpty newer) then
               OutdatedCoverageFileMessage h.mainWindow current
             let model =
               new TreeStore(typeof<string>, typeof<Gdk.Pixbuf>, typeof<string>,
                             typeof<Gdk.Pixbuf>, typeof<string>, typeof<Gdk.Pixbuf>,
                             typeof<string>, typeof<Gdk.Pixbuf>, typeof<string>,
                             typeof<Gdk.Pixbuf>)
             Mappings.Clear()
             let toprow = model.AppendValues(current.Name, XmlIcon.Force())

             let ApplyToModel (theModel : TreeStore) theRow
                 (group : XPathNavigator * string) =
               let name = snd group

               let newrow =
                 theModel.AppendValues(theRow,
                                       [| name :> obj
                                          AssemblyIcon.Force() :> obj |])
               PopulateAssemblyNode theModel newrow (fst group)

             let assemblies =
               coverage.Document.CreateNavigator().Select("//module")
               |> Seq.cast<XPathNavigator>
             assemblies
             |> Seq.map
                  (fun node ->
                  (node,
                   node.GetAttribute("assemblyIdentity", String.Empty).Split(',')
                   |> Seq.head))
             |> Seq.sortBy snd
             |> Seq.iter (ApplyToModel model toprow)
             let UpdateUI (theModel :
#if NETCOREAPP2_1
                                       ITreeModel
#else
                                       TreeModel
#endif

                          ) (info : FileInfo) () =
               // File is good so enable the refresh button
               h.refreshButton.Sensitive <- true
               // Do real UI work here
               h.classStructureTree.Model <- theModel
               h.codeView.Buffer.Clear()
               h.mainWindow.Title <- "AltCover.Visualizer"
               updateMRU h info.FullName true
             ////ShowMessage h.mainWindow (sprintf "%s\r\n>%A" info.FullName h.coverageFiles) MessageType.Info
             InvokeOnGuiThread(UpdateUI model current)
         }
         |> Async.Start)
    handler.fontButton.Clicked
    |> Event.add (fun x ->
         let executingAssembly = System.Reflection.Assembly.GetExecutingAssembly()
         let resources =
           ResourceManager("AltCover.Visualizer.Resource", executingAssembly)
         let format = resources.GetString("SelectFont")
#if NETCOREAPP2_1
         use selector = new FontChooserDialog(format, handler.mainWindow)
         selector.Font <- Persistence.readFont()
         if Enum.ToObject(typeof<ResponseType>, selector.Run()) :?> ResponseType = ResponseType.Ok then
           let font = selector.Font
#else
         let selector = new FontSelectionDialog(format)
         selector.SetFontName(Persistence.readFont()) |> ignore
         if Enum.ToObject(typeof<ResponseType>, selector.Run()) :?> ResponseType = ResponseType.Ok then
           let font = selector.FontName
#endif

           Persistence.saveFont (font)
           handler.baseline.Font <- font
           handler.codeView.QueueDraw()
#if NETCOREAPP2_1
         ) // implicit Dispose()
#else
         selector.Destroy())
#endif
    // Tree selection events and such
    handler.classStructureTree.RowActivated |> Event.add (OnRowActivated handler)
    Application.Run()
    0 // needs an int return