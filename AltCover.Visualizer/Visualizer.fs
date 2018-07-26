namespace AltCover.Visualizer

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Linq
open System.Reflection
open System.Resources
open System.Xml.Linq
open System.Xml.XPath

open AltCover.Augment
open AltCover.Visualizer.Extensions

open Gdk
open Gtk
open Glade

open Microsoft.Win32

open Mono.Options

type internal Handler() = class
       [<Widget; DefaultValue(true)>]
       val mutable mainWindow : Window

       [<Widget; DefaultValue(true)>]
       val mutable openButton : MenuToolButton

       [<Widget; DefaultValue(true)>]
       val mutable refreshButton : ToolButton

       [<Widget; DefaultValue(true)>]
       val mutable fontButton : ToolButton

       [<Widget; DefaultValue(true)>]
       val mutable showAboutButton : ToolButton

       [<Widget; DefaultValue(true)>]
       val mutable aboutVisualizer : AboutDialog

       [<Widget; DefaultValue(true)>]
       val mutable fileOpenMenu : Menu

       [<Widget; DefaultValue(true)>]
       val mutable classStructureTree : TreeView

       [<Widget; DefaultValue(true)>]
       val mutable codeView : TextView

       [<DefaultValue(true)>]
       val mutable coverageFiles : string list

       [<DefaultValue(true)>]
       val mutable justOpened : string
end

module Gui =

 // Binds class name and XML
 type internal MethodKey = {
         m : XPathNavigator;
         spacename : string;
         classname : string;
         name : string }

 // Range colouring information
 type internal CodeTag = {
        visitcount : int;
        line : int;
        column : int;
        endline : int;
        endcolumn : int }

 // --------------------------  General Purpose ---------------------------

 // Safe event dispatch => GUI update
 let private InvokeOnGuiThread (action: unit -> unit) =
        Gtk.Application.Invoke(fun (o:obj) (e:EventArgs) -> action())

 let private GetResourceString (key:string) =
   let executingAssembly = System.Reflection.Assembly.GetExecutingAssembly()
   let resources = new ResourceManager("AltCover.Visualizer.Resource", executingAssembly)
   resources.GetString(key)

 let private AssemblyIcon = lazy ( new Pixbuf(Assembly.GetExecutingAssembly().GetManifestResourceStream("AltCover.Visualizer.Assembly_6212.png")) )
 let private NamespaceIcon = lazy ( new Pixbuf(Assembly.GetExecutingAssembly().GetManifestResourceStream("AltCover.Visualizer.brackets_Curly_16xLG.png")) )
 let private ClassIcon = lazy ( new Pixbuf(Assembly.GetExecutingAssembly().GetManifestResourceStream("AltCover.Visualizer.class_16xLG.png")) )
 let private MethodIcon = lazy ( new Pixbuf(Assembly.GetExecutingAssembly().GetManifestResourceStream("AltCover.Visualizer.method_16xLG.png")) )

 // --------------------------  Persistence ---------------------------

 let private geometry = "SOFTWARE\\AltCover\\Visualizer\\Geometry"
 let private recent = "SOFTWARE\\AltCover\\Visualizer\\Recently Opened"
 let private coveragepath = "SOFTWARE\\AltCover\\Visualizer"
 let mutable private save = true

 let private saveFolder (path:string) =
    use key = Registry.CurrentUser.CreateSubKey(coveragepath)
    key.SetValue("path", path)

 let private readFolder () =
    use key = Registry.CurrentUser.CreateSubKey(coveragepath)
    key.GetValue("path", System.IO.Directory.GetCurrentDirectory()) :?> string

 let private saveFont (font:string) =
    use key = Registry.CurrentUser.CreateSubKey(coveragepath)
    key.SetValue("font", font)

 let private readFont () =
    use key = Registry.CurrentUser.CreateSubKey(coveragepath)
    key.GetValue("font", "Monospace Normal 10") :?> string

 let private saveGeometry (w:Window) =
    use key = Registry.CurrentUser.CreateSubKey(geometry)
    let (x,y) = w.GetPosition()
    key.SetValue("x", x)
    key.SetValue("y", y)

    let (width, height) = w.GetSize()
    key.SetValue("width", width)
    key.SetValue("height", height)

 let private readGeometry (w:Window) =
   use key = Registry.CurrentUser.CreateSubKey(geometry)
   let width = Math.Max(key.GetValue("width", 600) :?> int, 600)
   let height = Math.Max(key.GetValue("height", 450) :?> int, 450)
   let bounds = System.Windows.Forms.Screen.PrimaryScreen.WorkingArea
   let x = Math.Min(Math.Max(key.GetValue("x", (bounds.Width - width) /2) :?> int, 0), bounds.Width - width)
   let y = Math.Min(Math.Max(key.GetValue("y", (bounds.Height - height) /2) :?> int, 0), bounds.Height - height)
   w.DefaultHeight <- height
   w.DefaultWidth <- width
   w.Move(x, y)

 let private readCoverageFiles (handler:Handler) =
   use fileKey = Registry.CurrentUser.CreateSubKey(recent)

   let KeyToValue (key:RegistryKey) (n:string) =
        key.GetValue(n, String.Empty)

   let names = fileKey.GetValueNames()
                |> Array.filter (fun (s:string) -> s.Length = 1 && Char.IsDigit(s.Chars(0)))
                |> Array.sortBy (fun s -> Int32.TryParse(s) |> snd)
   let files = names
               |> Array.map (KeyToValue fileKey)
               |> Seq.cast<string>
               |> Seq.filter (fun n -> not (String.IsNullOrWhiteSpace(n)))
               |> Seq.filter (fun n -> File.Exists(n))
               |> Seq.toList
   handler.coverageFiles <- files

 // -------------------------- Method Name Handling ---------------------------

 let private MethodNameCompare (leftKey:MethodKey) (rightKey:MethodKey) =
    let HandleSpecialName (name:string) =
        if name.StartsWith("get_", StringComparison.Ordinal) ||
           name.StartsWith("set_", StringComparison.Ordinal)
        then (name.Substring(4), true) else (name, false)

    let x = leftKey.name
    let y = rightKey.name
    let (left, specialLeft) = HandleSpecialName x
    let (right, specialRight) = HandleSpecialName y

    let sort = String.CompareOrdinal(left, right)
    let specialCase = (0 = sort) && specialLeft && specialRight

    if 0 = sort then
        if specialCase then
            String.CompareOrdinal(x, y)
        else let l1 = leftKey.m.GetAttribute("fullname", String.Empty)
             let r1 = rightKey.m.GetAttribute("fullname", String.Empty)
             String.CompareOrdinal(l1, r1)
    else sort

 // -------------------------- Tree View ---------------------------

 let Mappings = new Dictionary<TreePath, XPathNavigator>()

 let private PopulateClassNode (model:TreeStore) (row:TreeIter) (nodes: seq<MethodKey>) =
    let ApplyToModel (theModel:TreeStore) (theRow:TreeIter) (x:MethodKey) =
        let fullname = x.m.GetAttribute("fullname", String.Empty)
        let args = if String.IsNullOrEmpty(fullname) then String.Empty
                   else let bracket = fullname.IndexOf('(')
                        if bracket < 0 then String.Empty
                        else fullname.Substring(bracket)
        let displayname = x.name + args
        let newrow = theModel.AppendValues(theRow, [| displayname :> obj ; MethodIcon.Force() :> obj |])
        Mappings.Add(theModel.GetPath(newrow), x.m)

    let methods = nodes |> Seq.toArray
    Array.sortInPlaceWith MethodNameCompare methods
    methods |> Array.iter (ApplyToModel model row)

 let private PopulateNamespaceNode (model:TreeStore) (row:TreeIter) (nodes: seq<MethodKey>) =
    let ApplyToModel (theModel:TreeStore) (theRow:TreeIter) (group : string * seq<MethodKey>) =
        let name = fst group
        let newrow = theModel.AppendValues(theRow, [| name :> obj ; ClassIcon.Force() :> obj |])
        PopulateClassNode theModel newrow (snd group)
    nodes |> Seq.groupBy (fun x -> x.classname)
          |> Seq.sortBy (fun x -> fst x)
          |> Seq.iter (ApplyToModel model row)
    ()

 let private PopulateAssemblyNode (model:TreeStore) (row:TreeIter) (node:XPathNavigator) =
    // within the <module> we have <method> nodes with name="get_module" class="AltCover.Coverage.CoverageSchema.coverage"
    let ApplyToModel (theModel:TreeStore) (theRow:TreeIter) (group : string * seq<MethodKey>) =
        let name = fst group
        let newrow = theModel.AppendValues(theRow, [| name :> obj ; NamespaceIcon.Force() :> obj |])
        PopulateNamespaceNode theModel newrow (snd group)

    let methods = node.SelectChildren("method", String.Empty)
                  |> Seq.cast<XPathNavigator>
                  |> Seq.map (fun m -> let classfullname = m.GetAttribute("class", String.Empty)
                                       let lastdot = classfullname.LastIndexOf('.')
                                       { m = m
                                         spacename = if lastdot < 0 then String.Empty else classfullname.Substring(0, lastdot)
                                         classname = if lastdot < 0 then classfullname else classfullname.Substring(1 + lastdot)
                                         name = m.GetAttribute("name", String.Empty) } )
                  |> Seq.groupBy (fun x -> x.spacename)
                  |> Seq.sortBy (fun x -> fst x)

    methods |> Seq.iter (ApplyToModel model row)

 // -------------------------- Message Boxes ---------------------------

 let private ShowMessage (parent:Window) (message:string) (messageType:MessageType) =
        use md = new MessageDialog (parent,
                                    DialogFlags.Modal ||| DialogFlags.DestroyWithParent,
                                    messageType,
                                    ButtonsType.Close,
                                    message);
        md.Icon <- parent.Icon
        md.Title <- parent.Title
        md.Run() |> ignore
        md.Destroy()

 let private ShowMessageOnGuiThread (parent:Window) (severity:MessageType) message =
   let SendMessageToWindow  () =
     ShowMessage parent message severity
   InvokeOnGuiThread(SendMessageToWindow)

 let private InvalidCoverageFileMessage (parent:Window) (x : InvalidFile) =
   let format = GetResourceString("InvalidFile")
   let message = String.Format(System.Globalization.CultureInfo.CurrentCulture,
                               format,
                               x.File.FullName,
                               x.Fault.Message)
   ShowMessageOnGuiThread parent MessageType.Error message

 let private OutdatedCoverageFileMessage (parent:Window) (x : FileInfo) =
   let format = GetResourceString("CoverageOutOfDate")
   let message = String.Format(System.Globalization.CultureInfo.CurrentCulture,
                               format,
                               x.FullName)
   ShowMessageOnGuiThread parent MessageType.Warning message

 let private MissingSourceFileMessage (parent:Window) (x : FileInfo) =
   let format = GetResourceString("MissingSourceFile")
   let message = String.Format(System.Globalization.CultureInfo.CurrentCulture,
                               format,
                               x.FullName)
   ShowMessageOnGuiThread parent MessageType.Warning message

 let private OutdatedCoverageThisFileMessage (parent:Window) (c : FileInfo) (s:FileInfo) =
   let format = GetResourceString("CoverageOutOfDateThisFile")
   let message = String.Format(System.Globalization.CultureInfo.CurrentCulture,
                               format,
                               c.FullName,
                               s.FullName)
   ShowMessageOnGuiThread parent MessageType.Warning message

 let private MissingSourceThisFileMessage (parent:Window) (c : FileInfo) (s:FileInfo) =
   let format = GetResourceString("MissingSourceThisFile")
   let message = String.Format(System.Globalization.CultureInfo.CurrentCulture,
                               format,
                               c.FullName,
                               s.FullName)
   ShowMessageOnGuiThread parent MessageType.Warning message

 // -------------------------- UI set-up  ---------------------------

 let private InitializeHandler () =
   let handler = new Handler()

   ["mainWindow"; "fileOpenMenu"; "aboutVisualizer"]
   |> List.map (fun name -> new Glade.XML ("AltCover.Visualizer.Visualizer.glade", name))
   |> List.iter (fun xml -> xml.Autoconnect(handler))
   handler

 // Fill in the menu from the memory cache
 let private populateMenu (handler:Handler) =
   let items = handler.fileOpenMenu.AllChildren
               |> Seq.cast<MenuItem>

   // blank the whole menu
   items
   |> Seq.iter (fun (i:MenuItem) -> i.Visible <- false
                                    (i.Child:?> Label).Text <- String.Empty)
   // fill in with the items we have
   Seq.zip handler.coverageFiles items
   |> Seq.iter (fun (p:string * MenuItem) ->
                          (snd p).Visible <- true
                          (((snd p).Child) :?> Label).Text <- fst p)

   // set or clear the menu
   handler.openButton.Menu <- if handler.coverageFiles.IsEmpty then null else handler.fileOpenMenu :> Widget
   handler.openButton.Label <- GetResourceString("openButton.Label")

 let private PrepareAboutDialog (handler:Handler) =
   let ShowUrl (link:string) =
     match System.Environment.GetEnvironmentVariable("OS") with
     | "Windows_NT" -> System.Diagnostics.Process.Start(link) |> ignore
     // TODO -- other OS types
     | _ -> ShowMessage handler.aboutVisualizer link MessageType.Info

   // The first gets the display right, the second the browser launch
   AboutDialog.SetUrlHook(fun _ link -> ShowUrl link) |> ignore
   LinkButton.SetUriHook(fun _ link -> ShowUrl link) |> ignore

   handler.aboutVisualizer.ActionArea.Children.OfType<Button>()
   |> Seq.iter(fun w -> let t = GetResourceString w.Label
                        if t |> String.IsNullOrWhiteSpace |> not then
                          w.Label <- t )

   handler.aboutVisualizer.Title <- GetResourceString("aboutVisualizer.Title")
   handler.aboutVisualizer.Parent <- handler.mainWindow
   handler.aboutVisualizer.Version <- System.Diagnostics.FileVersionInfo.GetVersionInfo(
                                      System.Reflection.Assembly.GetExecutingAssembly().Location).FileVersion
   handler.aboutVisualizer.WindowPosition <- WindowPosition.Mouse
   handler.aboutVisualizer.Version <- System.AssemblyVersionInformation.AssemblyFileVersion
   handler.aboutVisualizer.Copyright <- String.Format(System.Globalization.CultureInfo.CurrentCulture,
                                                      GetResourceString("aboutVisualizer.Copyright"),
                                                      System.AssemblyVersionInformation.AssemblyCopyright)
   handler.aboutVisualizer.License <- String.Format(System.Globalization.CultureInfo.CurrentCulture,
                                                    handler.aboutVisualizer.License,
                                                      System.AssemblyVersionInformation.AssemblyCopyright)
   handler.aboutVisualizer.Comments <- GetResourceString("aboutVisualizer.Comments")
   handler.aboutVisualizer.WebsiteLabel <- GetResourceString("aboutVisualizer.WebsiteLabel")

 let private PrepareTreeView (handler:Handler) =
   [| AssemblyIcon ; NamespaceIcon ; ClassIcon ; MethodIcon |]
   |> Seq.iteri (fun i x -> let column = new Gtk.TreeViewColumn ()
                            let cell = new Gtk.CellRendererText()
                            let icon = new Gtk.CellRendererPixbuf()
                            column.PackStart(icon, true)
                            column.PackEnd(cell, true)
                            handler.classStructureTree.AppendColumn(column) |> ignore
                            column.AddAttribute(cell, "text", 2*i)
                            column.AddAttribute(icon, "pixbuf", 1+(2*i)) )

 [<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Reliability", "CA2000:DisposeObjectsBeforeLosingScope",
    Justification = "'openFileDialog' is returned")>]
 let private PrepareOpenFileDialog () =
   let openFileDialog = new System.Windows.Forms.OpenFileDialog();
   openFileDialog.InitialDirectory <- readFolder ()
   openFileDialog.Filter <- GetResourceString("SelectXml")
   openFileDialog.FilterIndex <- 0
   openFileDialog.RestoreDirectory <- false
   openFileDialog

 // -------------------------- Event handling  ---------------------------

 let private HandleOpenClicked (handler:Handler) (openFileDialogFactory:unit -> System.Windows.Forms.OpenFileDialog) =
   use openFileDialog = openFileDialogFactory()
   let MakeSelection (ofd:System.Windows.Forms.OpenFileDialog) x =
        if ofd.ShowDialog() = System.Windows.Forms.DialogResult.OK then
                                                let file = new FileInfo(ofd.FileName)
                                                ofd.InitialDirectory <- file.Directory.FullName
                                                if save then saveFolder ofd.InitialDirectory
                                                Some( file )
        else None

   handler.openButton.Clicked
                 |> Event.map (MakeSelection openFileDialog)
                 |> Event.choose id
                 |> Event.map (fun info -> handler.justOpened <- info.FullName
                                           -1)

 [<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Reliability", "CA2000:DisposeObjectsBeforeLosingScope",
    Justification = "'baseline' is returned")>]
 let private InitializeTextBuffer (buff:TextBuffer) =
    let Tag (buffer:TextBuffer) (style:string, fg, bg) =
                        let tag = new TextTag(style)
                        tag.Foreground <- fg
                        tag.Background <- bg
                        buffer.TagTable.Add(tag)

    let baseline = new TextTag("baseline")
    baseline.Font <- readFont()
    baseline.Foreground <- "#c0c0c0"
    buff.TagTable.Add(baseline)

    // Last declared type is last layer painted
    [  ("visited", "#404040", "#cefdce") // "#98FB98") ; // Dark on Pale Green
       ("branched", "#404040", "#ADFF2F") ; // Grey on Green Yellow
       ("declared", "#FFA500", "#FFFFFF") ; // Orange on White
       ("static", "#808080", "#F5F5F5") ; // Grey on White Smoke
       ("automatic", "#808080", "#FFFF00") ; // Grey on Yellow
       ("notVisited", "#ff0000", "#FFFFFF") ; // Red on White
       ("excluded", "#87CEEB", "#FFFFFF") ; // Sky Blue on white
       ("allBranches", "#FFFFFF", "#cefdce")
       ("someBranches", "#FFFFFF", "#FFFF00")
       ("noBranches", "#FFFFFF", "#FF0000")
       ]
        |> Seq.iter (Tag buff)

 let private ParseIntegerAttribute (element:XPathNavigator) (attribute:string) =
   let text = element.GetAttribute(attribute, String.Empty)
   let number = Int32.TryParse(text, NumberStyles.None, CultureInfo.InvariantCulture)
   if (fst number) then
    snd number
   else
     if not <| String.IsNullOrEmpty(text) then System.Diagnostics.Debug.WriteLine("ParseIntegerAttribute : '" + attribute + "' with value '" + text)
     0

 [<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Reliability",
    "CA2000:DisposeObjectsBeforeLosingScope",
    Justification = "IDisposables are added to the TextView")>]
 let private MarkBranches (root:XPathNavigator) (codeView:TextView) (filename:string) =
    let buff = codeView.Buffer
    let branches = new Dictionary<int,int*int>()
    let branch = new Pixbuf(Assembly.GetExecutingAssembly().GetManifestResourceStream(
                                "AltCover.Visualizer.Branch_12x_16x.png"))
    let blank = new Pixbuf(Assembly.GetExecutingAssembly().GetManifestResourceStream(
                                "AltCover.Visualizer.Blank_12x_16x.png"))

    root.Select("//method")
                    |> Seq.cast<XPathNavigator>
                    |> Seq.filter(fun n -> let f = n.Clone()
                                           f.MoveToFirstChild() && filename.Equals(
                                            f.GetAttribute("document", String.Empty),
                                            StringComparison.OrdinalIgnoreCase))

                    |> Seq.collect(fun n -> n.Select("./branch") |> Seq.cast<XPathNavigator>)
                    |> Seq.groupBy(fun n -> n.GetAttribute("line", String.Empty))
                    |> Seq.iter (fun n -> let line = ParseIntegerAttribute ((snd n) |> Seq.head) "line"
                                          let num = (snd n) |> Seq.length
                                          let v = (snd n)
                                                  |> Seq.filter (fun x -> x.GetAttribute("visitcount", String.Empty) <> "0")
                                                  |> Seq.length
                                          branches.Add(line, (v,num)))
    for l in 1 .. buff.LineCount do
      let pix = if branches.ContainsKey l then
                                branch
                            else
                                blank
      let mutable i = buff.GetIterAtLine(l - 1)
      let a = new TextChildAnchor()
      buff.InsertChildAnchor(&i, a)
      let image = new Image(pix)
      image.Visible <- true
      codeView.AddChildAtAnchor(image, a)
      if branches.ContainsKey l then
        let from = buff.GetIterAtLineOffset(l - 1, 0)
        let until = buff.GetIterAtLineOffset(l - 1, 1)
        let v, num = branches.[l]
        image.TooltipText <-  String.Format(System.Globalization.CultureInfo.CurrentCulture,
                               GetResourceString "branchesVisited",
                               v,
                               num)
        buff.ApplyTag(if 0 = v then "noBranches"
                      else if v = num then "allBranches"
                           else "someBranches"
                      ,from, until)

 let internal (|Select|_|) (pattern:String) offered =
    if (fst offered) |> String.IsNullOrWhiteSpace |> not &&
       pattern.StartsWith(fst offered, StringComparison.Ordinal)
       then Some offered
       else None

 let private SelectStyle because excluded =
    match (because, excluded) with
                   | Select "author declared (" _ -> Exemption.Declared
                   | Select "tool-generated: " _ -> Exemption.Automatic
                   | Select "static analysis: " _ -> Exemption.StaticAnalysis
                   | (_, true) -> Exemption.Excluded
                   | _ -> 0

 let private CoverageToTag (n:XPathNavigator) =
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

    { visitcount = if visitcount = 0 then fallback else visitcount
      line = Int32.TryParse(line) |> snd
      column = (Int32.TryParse(column) |> snd) + 1
      endline = Int32.TryParse(endline) |> snd
      endcolumn = (Int32.TryParse(endcolumn) |> snd) + 1 }

 let private FilterCoverage (buff:TextBuffer) (n:CodeTag) =
    let lc = buff.LineCount
    n.line > 0 && n.endline > 0 &&
    n.line <= lc &&
    n.endline <= lc

 let private TagByCoverage (buff:TextBuffer) (n:CodeTag) =
    // bound by current line length in case we're looking from stale coverage
    let line = buff.GetIterAtLine(n.line - 1)
    let from = if line.CharsInLine = 0 then line
               else buff.GetIterAtLineOffset(n.line - 1, Math.Min(n.column, line.CharsInLine) - 1)
    let endline = buff.GetIterAtLine(n.endline - 1)
    let until = if endline.CharsInLine = 0 then endline
                else buff.GetIterAtLineOffset(n.endline - 1, Math.Min(n.endcolumn, endline.CharsInLine) - 1)
    let tag = match n.visitcount with
                | 0 -> "notVisited"
                | Exemption.Declared -> "declared"
                | Exemption.Automatic -> "automatic"
                | Exemption.StaticAnalysis -> "static"
                | Exemption.Excluded -> "excluded"
                | _ -> "visited"
    buff.ApplyTag(tag, from, until)

 let private MarkCoverage (root:XPathNavigator) buff filename =
    root.Select("//seqpnt[@document='" + filename + "']")
                    |> Seq.cast<XPathNavigator>
                    |> Seq.map CoverageToTag
                    |> Seq.filter (FilterCoverage buff)
                    |> Seq.iter (TagByCoverage buff)

 let private OnRowActivated (handler:Handler) (activation:RowActivatedArgs) =
        let HitFilter (activated:RowActivatedArgs) (path:TreePath) =
            activated.Path.Compare(path) = 0

        let hits = Mappings.Keys |> Seq.filter (HitFilter activation)

        if not (Seq.isEmpty hits) then
            let m = Mappings.[Seq.head hits]
            if m.HasChildren then
                let child = m.Clone()
                child.MoveToFirstChild() |> ignore
                let filename = child.GetAttribute("document", String.Empty)

                let info = new FileInfo(filename)
                let current = new FileInfo(handler.coverageFiles.Head)
                if (not info.Exists) then
                        MissingSourceThisFileMessage handler.mainWindow current info
                else if (info.LastWriteTimeUtc > current.LastWriteTimeUtc) then
                        OutdatedCoverageThisFileMessage handler.mainWindow current info
                     else
                        let buff = handler.codeView.Buffer
                        buff.Text <- File.ReadAllText(filename)
                        buff.ApplyTag("baseline", buff.StartIter, buff.EndIter)

                        let line = child.GetAttribute("line", String.Empty)
                        let root = m.Clone()
                        root.MoveToRoot()

                        MarkBranches root handler.codeView filename
                        MarkCoverage root buff filename

                        let iter = buff.GetIterAtLine((Int32.TryParse(line) |> snd) - 1)
                        let mark = buff.CreateMark(line, iter, true)
                        handler.codeView.ScrollToMark(mark, 0.0, true, 0.0, 0.3)

 let private PrepareGui () =
   let handler = InitializeHandler ()
   PrepareAboutDialog handler
   PrepareTreeView handler

   readGeometry handler.mainWindow
   readCoverageFiles handler
   populateMenu handler

   handler.codeView.Editable <- false
   InitializeTextBuffer handler.codeView.Buffer

   handler.refreshButton.Sensitive <- false
   handler.refreshButton.Label <- GetResourceString("refreshButton.Label")
   handler.fontButton.Label <- GetResourceString("fontButton.Label")
   handler.showAboutButton.Label <- GetResourceString("showAboutButton.Label")

   // Initialize graphics and begin
   handler.mainWindow.Icon <- new Pixbuf(Assembly.GetExecutingAssembly().GetManifestResourceStream("AltCover.Visualizer.VIcon.ico"))
   handler.aboutVisualizer.Icon <- new Pixbuf(Assembly.GetExecutingAssembly().GetManifestResourceStream("AltCover.Visualizer.VIcon.ico"))
   handler.aboutVisualizer.Logo <- new Pixbuf(Assembly.GetExecutingAssembly().GetManifestResourceStream("AltCover.Visualizer.logo.png"))
   handler.mainWindow.ShowAll()
   handler

 let ParseCommandLine arguments =
   let options = new OptionSet()
   options.Add("-g", "Clear geometry",
                (fun _ -> let k1 = Registry.CurrentUser.CreateSubKey(geometry)
                          k1.Close();
                          save <- false
                          Registry.CurrentUser.DeleteSubKeyTree(geometry)))
          .Add("-r", "Clear recent file list",
                 (fun _ -> let k1 = Registry.CurrentUser.CreateSubKey(recent)
                           k1.Close();
                           Registry.CurrentUser.DeleteSubKeyTree(recent))) |> ignore
   options.Parse(arguments) |> ignore

 [<EntryPoint; STAThread>]
 let internal Main arguments =
   ParseCommandLine arguments
   Application.Init()
   let handler = PrepareGui ()
   handler.mainWindow.DeleteEvent
             |> Event.add (fun args -> if save then saveGeometry handler.mainWindow
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
       |> Seq.mapi (fun n (i:MenuItem) -> i.Activated
                                          |> Event.map (fun _ -> n ))

   // The sum of all these events -- we have explicitly selected a file
   let fileSelection = select
                       |> Seq.fold Event.merge click

   // Update the recent files menu and registry store from memory cache
   // with new most recent file
   let RegDeleteKey (key:RegistryKey) (name:string) =
     key.DeleteValue(name)

   let RegSetKey (key:RegistryKey) (index:int) (name:string) =
        key.SetValue(index.ToString(), name)

   let updateMRU (h:Handler) path add = 
    let casematch = match System.Environment.GetEnvironmentVariable("OS") with
                     | "Windows_NT" -> StringComparison.OrdinalIgnoreCase
                     | _ -> StringComparison.Ordinal
    let files = h.coverageFiles
                |> List.filter (fun n -> not (n.Equals(path, casematch)))
                |> Seq.truncate(9)
                |> Seq.toList

    h.coverageFiles <- (if add then (path :: files) else files)
                       |> Seq.distinctBy (fun n -> match casematch with
                                                   | StringComparison.Ordinal -> n
                                                   | _ -> n.ToUpperInvariant())
                       |> Seq.toList
    populateMenu h
    use fileKey = Registry.CurrentUser.CreateSubKey(recent)
    fileKey.GetValueNames()
    |> Seq.iter (RegDeleteKey fileKey)
    h.coverageFiles
    |> Seq.iteri (RegSetKey fileKey)

   // Now mix in selecting the file currently loaded
   let refresh  = handler.refreshButton.Clicked
                    |> Event.map (fun _ -> 0)

   Event.merge fileSelection refresh
   |> Event.add(fun index ->
        let h = handler
        async {
        let current = FileInfo (if index < 0 then h.justOpened else h.coverageFiles.[index])
        match CoverageFile.LoadCoverageFile current with
        | Left failed -> InvalidCoverageFileMessage h.mainWindow failed
                         InvokeOnGuiThread (fun () -> updateMRU h current.FullName false)
        | Right coverage ->
            // check if coverage is newer that the source files
            let sourceFiles = coverage.Document.CreateNavigator().Select("//seqpnt/@document") |> Seq.cast<XPathNavigator> |> Seq.map (fun x -> x.Value) |> Seq.distinct

            let missing = sourceFiles
                        |> Seq.map (fun f -> new FileInfo(f))
                        |> Seq.filter (fun f -> not f.Exists)
            if not (Seq.isEmpty missing) then
                MissingSourceFileMessage h.mainWindow current

            let newer = sourceFiles
                        |> Seq.map (fun f -> new FileInfo(f))
                        |> Seq.filter (fun f -> f.Exists &&
                                                f.LastWriteTimeUtc > current.LastWriteTimeUtc )
            // warn if not
            if not (Seq.isEmpty newer) then
                OutdatedCoverageFileMessage h.mainWindow current

            let model = new TreeStore( typeof<string>, typeof<Gdk.Pixbuf>,
                                       typeof<string>, typeof<Gdk.Pixbuf>,
                                       typeof<string>, typeof<Gdk.Pixbuf>,
                                       typeof<string>, typeof<Gdk.Pixbuf> )
            Mappings.Clear()

            let ApplyToModel (theModel:TreeStore) (group : XPathNavigator * string) =
                let name = snd group
                let row = theModel.AppendValues(name, AssemblyIcon.Force())
                PopulateAssemblyNode theModel row (fst group)

            let assemblies = coverage.Document.CreateNavigator().Select("//module") |> Seq.cast<XPathNavigator>
            assemblies
            |> Seq.map (fun node -> (node, node.GetAttribute("assemblyIdentity", String.Empty).Split(',') |> Seq.head))
            |> Seq.sortBy (fun nodepair -> snd nodepair)
            |> Seq.iter (ApplyToModel model)

            let UpdateUI (theModel : TreeModel) (info:FileInfo) () =
                // File is good so enable the refresh button
                h.refreshButton.Sensitive <- true
                // Do real UI work here
                h.classStructureTree.Model <- theModel
                updateMRU h info.FullName true
                ////ShowMessage h.mainWindow (sprintf "%s\r\n>%A" info.FullName h.coverageFiles) MessageType.Info

            InvokeOnGuiThread (UpdateUI model current)}
        |> Async.Start )

   handler.fontButton.Clicked |> Event.add (fun x ->
        let executingAssembly = System.Reflection.Assembly.GetExecutingAssembly()
        let resources = new ResourceManager("AltCover.Visualizer.Resource", executingAssembly)
        let format = resources.GetString("SelectFont")
        let selector = new FontSelectionDialog(format)
        selector.SetFontName(readFont()) |> ignore

        if Enum.ToObject(typeof<ResponseType>, selector.Run()) :?> ResponseType = ResponseType.Ok
            then saveFont(selector.FontName)
        selector.Destroy()
     )

   // Tree selection events and such
   handler.classStructureTree.RowActivated |> Event.add (OnRowActivated handler)

   Application.Run()

   0 // needs an int return