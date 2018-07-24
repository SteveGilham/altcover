namespace AltCover.Visualizer

open System
open System.Collections.Generic
open System.Globalization
open System.IO
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
               |> Seq.filter (fun n -> not (String.IsNullOrEmpty(n)))
               |> Seq.filter (fun n -> not (String.IsNullOrEmpty(n.Trim())))
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

 let private InvalidCoverageFileMessage (parent:Window) (x : InvalidFile) =
   let SendMessageToWindow (window:Window) (message:string) () =
     ShowMessage window message MessageType.Error
   let format = GetResourceString("InvalidFile")
   let message = String.Format(System.Globalization.CultureInfo.CurrentCulture,
                               format,
                               x.File.FullName,
                               x.Fault.Message)
   InvokeOnGuiThread(SendMessageToWindow parent message)

 let private OutdatedCoverageFileMessage (parent:Window) (x : FileInfo) =
   let SendMessageToWindow (window:Window) (message:string) () =
     ShowMessage window message MessageType.Warning
   let format = GetResourceString("CoverageOutOfDate")
   let message = String.Format(System.Globalization.CultureInfo.CurrentCulture,
                               format,
                               x.FullName)
   InvokeOnGuiThread(SendMessageToWindow parent message)

 let private OutdatedCoverageThisFileMessage (parent:Window) (c : FileInfo) (s:FileInfo) =
   let SendMessageToWindow (window:Window) (message:string) () =
     ShowMessage window message MessageType.Warning

   let format = GetResourceString("CoverageOutOfDateThisFile")
   let message = String.Format(System.Globalization.CultureInfo.CurrentCulture,
                               format,
                               c.FullName,
                               s.FullName)
   InvokeOnGuiThread(SendMessageToWindow parent message)

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

 let private PrepareAboutDialog (handler:Handler) =
   let ShowUrl (link:string) =
    System.Diagnostics.Process.Start(link) |> ignore

   // The first gets the display right, the second the browser launch
   AboutDialog.SetUrlHook(fun _ link -> ShowUrl link) |> ignore
   LinkButton.SetUriHook(fun _ link -> ShowUrl link) |> ignore

   handler.aboutVisualizer.Parent <- handler.mainWindow
   handler.aboutVisualizer.Version <- System.Diagnostics.FileVersionInfo.GetVersionInfo(
                                      System.Reflection.Assembly.GetExecutingAssembly().Location).FileVersion
   handler.aboutVisualizer.WindowPosition <- WindowPosition.Mouse
   handler.aboutVisualizer.Version <- System.AssemblyVersionInformation.AssemblyFileVersion
   handler.aboutVisualizer.Copyright <- String.Format(System.Globalization.CultureInfo.CurrentCulture,
                                                      handler.aboutVisualizer.Copyright,
                                                      System.AssemblyVersionInformation.AssemblyCopyright)
   handler.aboutVisualizer.License <- String.Format(System.Globalization.CultureInfo.CurrentCulture,
                                                    handler.aboutVisualizer.License,
                                                      System.AssemblyVersionInformation.AssemblyCopyright)

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
       ("excluded", "#87CEEB", "#FFFFFF") ] // Sky Blue on white
        |> Seq.iter (Tag buff)

 let private ParseIntegerAttribute (element:XPathNavigator) (attribute:string) =
   let text = element.GetAttribute(attribute, String.Empty)
   let number = Int32.TryParse(text, NumberStyles.None, CultureInfo.InvariantCulture)
   if (fst number) then
    snd number
   else
     if not <| String.IsNullOrEmpty(text) then System.Diagnostics.Debug.WriteLine("ParseIntegerAttribute : '" + attribute + "' with value '" + text)
     0

 //// // simple sl/el marking
 ////let private MarkBranches (root:XPathNavigator) (buff:TextBuffer) (filename:string) =
 ////   let Decorate (buffer:TextBuffer) (tag:CodeTag) =
 ////       let from = buffer.GetIterAtLineOffset(tag.line - 1, tag.column - 1)
 ////       let until = buffer.GetIterAtLineOffset(tag.endline - 1, tag.endcolumn - 1)
 ////       buffer.ApplyTag("branched", from, until)

 ////   root.Select("//method")
 ////                   |> Seq.cast<XPathNavigator>
 ////                   |> Seq.filter(fun n -> let f = n.Clone()
 ////                                          f.MoveToFirstChild() && filename.Equals(
 ////                                           f.GetAttribute("document", String.Empty),
 ////                                           StringComparison.OrdinalIgnoreCase))

 ////                   |> Seq.collect(fun n -> n.Select("./branch") |> Seq.cast<XPathNavigator>)
 ////                   |> Seq.map (fun n -> let visitcount = ParseIntegerAttribute n "visitcount"
 ////                                        let line = ParseIntegerAttribute n "sl"
 ////                                        let endline = ParseIntegerAttribute n "el"
 ////                                        { visitcount = visitcount
 ////                                          line = line
 ////                                          column = 1
 ////                                          endline = if endline > line then endline else line + 1
 ////                                          endcolumn = 1 })
 ////                   |> Seq.filter (fun n -> n.visitcount = 0 && n.line > 0)
 ////                   |> Seq.iter (Decorate buff)

 let internal (|Select|_|) (pattern:String) offered =
    if (fst offered) |> String.IsNullOrWhiteSpace |> not &&
       pattern.StartsWith(fst offered, StringComparison.Ordinal)
       then Some offered
       else None

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
    let fallback = match (because, excluded) with
                   | Select "author declared (" _ -> Exemption.Declared
                   | Select "tool-generated: " _ -> Exemption.Automatic
                   | Select "static analysis: " _ -> Exemption.StaticAnalysis
                   | (_, true) -> Exemption.Excluded
                   | _ -> 0

    { visitcount = if visitcount = 0 then fallback else visitcount
      line = Int32.TryParse(line) |> snd
      column = Int32.TryParse(column) |> snd
      endline = Int32.TryParse(endline) |> snd
      endcolumn = Int32.TryParse(endcolumn) |> snd }

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
                if (not info.Exists) || (info.LastWriteTimeUtc > current.LastWriteTimeUtc) then
                    OutdatedCoverageThisFileMessage handler.mainWindow current info
                else
                    let buff = handler.codeView.Buffer
                    buff.Text <- File.ReadAllText(filename)
                    buff.ApplyTag("baseline", buff.StartIter, buff.EndIter)

                    let line = child.GetAttribute("line", String.Empty)
                    let root = m.Clone()
                    root.MoveToRoot()

                    ////// simple sl/el marking
                    ////MarkBranches root buff filename

                    let code = root.Select("//seqpnt[@document='" + filename + "']")
                                 |> Seq.cast<XPathNavigator>
                                 |> Seq.map CoverageToTag
                                 |> Seq.filter (FilterCoverage buff)

                    code |> Seq.iter (TagByCoverage buff)

                    let iter = buff.GetIterAtLine((Int32.TryParse(line) |> snd) - 1)
                    let mark = buff.CreateMark(line, iter, true)
                    handler.codeView.ScrollToMark(mark, 0.0, true, 0.0, 0.3)

 [<EntryPoint; STAThread>]
 let internal Main arguments =
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

   Application.Init()
   let handler = InitializeHandler ()
   PrepareAboutDialog handler
   PrepareTreeView handler

   readGeometry handler.mainWindow
   readCoverageFiles handler
   populateMenu handler

   handler.codeView.Editable <- false
   InitializeTextBuffer handler.codeView.Buffer

   handler.refreshButton.Sensitive <- false
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
       |> Seq.map (fun (i:MenuItem) -> let text = (i.Child :?> Label).Text
                                       i.Activated
                                       |> Event.map (fun a -> new FileInfo(text) ))

   // The sum of all these events -- we have explicitly selected a file
   let fileSelection = select
                       |> Seq.fold Event.merge click

   // Update the recent files menu and registry store from memory cache
   // with new most recent file
   let RegDeleteKey (key:RegistryKey) (name:string) =
     key.DeleteValue(name)

   let RegSetKey (key:RegistryKey) (index:int) (name:string) =
        key.SetValue(index.ToString(), name)

   let doMRU =
     let rec loop (h:Handler)
                  (selected:IEvent<FileInfo>)
                  (deleteKey:RegistryKey -> string -> unit)
                  (setKey:RegistryKey -> int -> string -> unit) = // Track this for 4 fields
       async {
                let! current = Async.AwaitEvent selected

                let path = current.FullName
                let files = h.coverageFiles
                            |> List.map (fun n -> new FileInfo(n) |> CoverageFile.LoadCoverageFile)
                            |> List.choose Either.toOption
                            |> List.filter (fun n -> not (n.File.FullName.Equals(path, StringComparison.OrdinalIgnoreCase)))
                            |> Seq.truncate(9)
                            |> Seq.map (fun n -> n.File.FullName)
                            |> Seq.toList

                h.coverageFiles <- (path :: files)
                                   |> Seq.distinct
                                   |> Seq.toList
                use fileKey = Registry.CurrentUser.CreateSubKey(recent)
                fileKey.GetValueNames()
                |> Seq.iter (deleteKey fileKey)
                h.coverageFiles
                |> Seq.iteri (setKey fileKey)

                InvokeOnGuiThread (fun () -> populateMenu h)
                return! loop h selected deleteKey setKey }
     loop handler fileSelection RegDeleteKey RegSetKey

   doMRU |> Async.Start

   // Now mix in selecting the file currently loaded
   let refresh = handler.refreshButton.Clicked
                 |> Event.map (fun x -> new FileInfo(handler.coverageFiles.Head))

   let doUI =
     let rec loop (h:Handler) (loadEvent:IEvent<FileInfo>) =
       async {
        let! current = Async.AwaitEvent loadEvent
        match CoverageFile.LoadCoverageFile current with
        | Left failed -> InvalidCoverageFileMessage h.mainWindow failed
        | Right coverage ->
            // check if coverage is newer that the source files
            let sourceFiles = coverage.Document.CreateNavigator().Select("//seqpnt/@document") |> Seq.cast<XPathNavigator> |> Seq.map (fun x -> x.Value) |> Seq.distinct
            let newer = sourceFiles
                        |> Seq.map (fun f -> new FileInfo(f))
                        |> Seq.filter (fun f -> (not f.Exists) ||
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

            let UpdateUI (theModel : TreeModel) () =
                // File is good so enable the refresh button
                h.refreshButton.Sensitive <- true
                // Do real UI work here
                h.classStructureTree.Model <- theModel

            InvokeOnGuiThread (UpdateUI model)
        return! loop h loadEvent }
     loop handler (Event.merge fileSelection refresh)

   doUI |> Async.Start

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

   // Initialize graphics and begin
   handler.mainWindow.Icon <- new Pixbuf(Assembly.GetExecutingAssembly().GetManifestResourceStream("AltCover.Visualizer.VIcon.ico"))
   handler.aboutVisualizer.Icon <- new Pixbuf(Assembly.GetExecutingAssembly().GetManifestResourceStream("AltCover.Visualizer.VIcon.ico"))
   handler.aboutVisualizer.Logo <- new Pixbuf(Assembly.GetExecutingAssembly().GetManifestResourceStream("AltCover.Visualizer.logo.png"))
   handler.mainWindow.ShowAll()

   Application.Run()

   0 // needs an int return