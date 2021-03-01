namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open Gtk
#if NET472
open Glade
#endif

[<SuppressMessage("Gendarme.Rules.Design",
                  "DoNotDeclareVirtualMethodsInSealedTypeRule",
                  Justification = "Compiler generated explicit interface methods")>]
[<Sealed; AutoSerializable(false)>]
type internal Handler() =
  class
#if !NET472
    [<Builder.Object; DefaultValue(true)>]
    val mutable toolbar1: Toolbar
#endif

    [<
#if !NET472
      Builder.Object;
#else
      Widget;
#endif
      DefaultValue(true)>]
    val mutable mainWindow: Window

    [<
#if !NET472
      Builder.Object;
#else
      Widget;
#endif
      DefaultValue(true)>]
    val mutable openButton: MenuToolButton

    [<
#if !NET472
      Builder.Object;
#else
      Widget;
#endif
      DefaultValue(true)>]
    val mutable separator1: SeparatorToolItem

    [<
#if !NET472
      Builder.Object;
#else
      Widget;
#endif
      DefaultValue(true)>]
    val mutable exitButton: ToolButton

    [<
#if !NET472
      Builder.Object;
#else
      Widget;
#endif
      DefaultValue(true)>]
    val mutable refreshButton: ToolButton

    [<
#if !NET472
      Builder.Object;
#else
      Widget;
#endif
      DefaultValue(true)>]
    val mutable fontButton: ToolButton

    [<
#if !NET472
      Builder.Object;
#else
      Widget;
#endif
      DefaultValue(true)>]
    val mutable showAboutButton: ToolButton

    [<
#if !NET472
      Builder.Object;
#else
      Widget;
#endif
      DefaultValue(true)>]
    val mutable aboutVisualizer: AboutDialog

    [<
#if !NET472
      Builder.Object;
#else
      Widget;
#endif
      DefaultValue(true)>]
    val mutable fileOpenMenu: Menu

    [<
#if !NET472
      Builder.Object;
#else
      Widget;
#endif
      DefaultValue(true)>]
    val mutable classStructureTree: TreeView

    [<DefaultValue(true)>]
    val mutable auxModel: TreeStore

    [<
#if !NET472
      Builder.Object;
#else
      Widget;
#endif
      DefaultValue(true)>]
    val mutable sourceScroller: ScrolledWindow

    [<
#if !NET472
      Builder.Object;
#else
      Widget;
#endif
      DefaultValue(true)>]
    val mutable lineView: TextView

    [<
#if !NET472
      Builder.Object;
#else
      Widget;
#endif
      DefaultValue(true)>]
    val mutable codeView: TextView

    [<
#if !NET472
      Builder.Object;
#else
      Widget;
#endif
      DefaultValue(true)>]
    val mutable viewport1: Viewport

    [<DefaultValue(true)>]
    val mutable coverageFiles: string list

    [<DefaultValue(true)>]
    val mutable justOpened: string

    [<DefaultValue(true)>]
    val mutable activeRow: int

    static member private ShowMessage
      (parent: Window)
      (message: string)
      (messageType: AltCover.MessageType)
      =
      use md =
        new MessageDialog(
          parent,
          DialogFlags.Modal
          ||| DialogFlags.DestroyWithParent,
          messageType |> int |> enum,
          ButtonsType.Close,
          message
        )

      md.Icon <- parent.Icon
      md.Title <- "AltCover.Visualizer"
      md.Run() |> ignore
#if !NET472
    // implicit Dispose()
#else
      md.Destroy()
#endif

    // Safe event dispatch => GUI update
    static member InvokeOnGuiThread(action: unit -> unit) =
      Gtk.Application.Invoke(fun (o: obj) (e: EventArgs) -> action ())

    member private self.ShowMessageOnGuiThread (severity: AltCover.MessageType) message =
      let sendMessageToWindow () =
        Handler.ShowMessage self.mainWindow message severity

      Handler.InvokeOnGuiThread(sendMessageToWindow)

    interface IVisualizerWindow with
      member self.CoverageFiles
        with get () = self.coverageFiles
        and set (value) = self.coverageFiles <- value

      member self.Title
        with get () = self.mainWindow.Title
        and set (value) = self.mainWindow.Title <- value

      member self.ShowMessageOnGuiThread mtype message =
        self.ShowMessageOnGuiThread mtype message
  end