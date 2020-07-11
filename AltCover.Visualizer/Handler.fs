namespace AltCover.Visualizer

open System
open Gtk
#if !NETCOREAPP2_1
open Glade
#endif

[<Sealed; AutoSerializable(false)>]
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

    [<DefaultValue(true)>]
    val mutable auxModel : TreeStore

    [<
#if NETCOREAPP2_1
      Builder.Object;
#else
      Widget;
#endif
    DefaultValue(true)>]
    val mutable sourceScroller : ScrolledWindow

    [<
#if NETCOREAPP2_1
      Builder.Object;
#else
      Widget;
#endif
    DefaultValue(true)>]
    val mutable lineView : TextView

    [<
#if NETCOREAPP2_1
      Builder.Object;
#else
      Widget;
#endif
    DefaultValue(true)>]
    val mutable codeView : TextView

    [<
#if NETCOREAPP2_1
      Builder.Object;
#else
      Widget;
#endif
    DefaultValue(true)>]
    val mutable viewport1 : Viewport

    [<DefaultValue(true)>]
    val mutable coverageFiles : string list

    [<DefaultValue(true)>]
    val mutable justOpened : string

    [<DefaultValue(true)>]
    val mutable activeRow : int

    static member private showMessage (parent : Window) (message : string) (messageType : AltCover.Visualizer.MessageType) =
      use md =
        new MessageDialog(parent, DialogFlags.Modal ||| DialogFlags.DestroyWithParent,
                          messageType |> int |> enum, ButtonsType.Close, message)
      md.Icon <- parent.Icon
      md.Title <- "AltCover.Visualizer"
      md.Run() |> ignore
#if NETCOREAPP2_1
    // implicit Dispose()
#else
      md.Destroy()
#endif

  // Safe event dispatch => GUI update
    static member InvokeOnGuiThread(action : unit -> unit) =
      Gtk.Application.Invoke(fun (o : obj) (e : EventArgs) -> action())

    member private self.showMessageOnGuiThread (severity : AltCover.Visualizer.MessageType) message =
      let sendMessageToWindow() = Handler.showMessage self.mainWindow message severity
      Handler.InvokeOnGuiThread(sendMessageToWindow)

    interface IVisualizerWindow with
      member self.CoverageFiles
        with get () = self.coverageFiles
        and set(value) = self.coverageFiles <- value
      member self.Title
       with get() = self.mainWindow.Title
       and set(value) = self.mainWindow.Title <- value
      member self.ShowMessageOnGuiThread mtype message =
        self.showMessageOnGuiThread mtype message
  end