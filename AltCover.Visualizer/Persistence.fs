namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.IO

open Gtk
#if NET472
open Microsoft.Win32
#endif

module internal Persistence =
  let mutable internal save = true

#if !NET472

  let internal saveSchemaDir =
    Configuration.SaveSchemaDir

  let internal saveFont =
    Configuration.SaveFont

  let internal readFont =
    Configuration.ReadFont

  let internal readSchemaDir =
    Configuration.ReadSchemaDir

  let internal readFolder =
    Configuration.ReadFolder

  let internal saveFolder =
    Configuration.SaveFolder

  let internal saveCoverageFiles =
    Configuration.SaveCoverageFiles

  let internal readCoverageFiles (handler: IVisualizerWindow) =
    Configuration.ReadCoverageFiles(fun files -> handler.CoverageFiles <- files)

  let saveGeometry (w: Window) =
    Configuration.SaveGeometry w.GetPosition w.GetSize

  let readGeometry (w: Window) =
    Configuration.ReadGeometry (fun (width, height) (x, y) ->
      w.DefaultHeight <- height
      w.DefaultWidth <- width

      let display = w.Display

      let monitor =
        { 0 .. display.NMonitors }
        |> Seq.filter (fun i ->
          use monitor = display.GetMonitor(i)
          let bounds = monitor.Geometry

          x >= bounds.Left
          && x <= bounds.Right
          && y >= bounds.Top
          && y <= bounds.Bottom)
        |> Seq.tryHead
        |> Option.defaultValue 0

      use m = display.GetMonitor(monitor)
      let bounds = m.Geometry

      let x' =
        Math.Min(Math.Max(x, bounds.Left), bounds.Right - width)

      let y' =
        Math.Min(Math.Max(y, bounds.Top), bounds.Bottom - height)

      w.Move(x', y'))

  let clearGeometry =
    Configuration.ClearGeometry

#else
  let internal geometry =
    "SOFTWARE\\AltCover\\Visualizer\\Geometry"

  let internal recent =
    "SOFTWARE\\AltCover\\Visualizer\\Recently Opened"

  let internal coveragepath =
    "SOFTWARE\\AltCover\\Visualizer"

  let internal saveFolder (path: string) =
    use key =
      Registry.CurrentUser.CreateSubKey(coveragepath)

    key.SetValue("path", path)

  let internal readFolder () =
    use key =
      Registry.CurrentUser.CreateSubKey(coveragepath)

    key.GetValue("path", System.IO.Directory.GetCurrentDirectory()) :?> string

  let internal saveFont (font: string) =
    use key =
      Registry.CurrentUser.CreateSubKey(coveragepath)

    key.SetValue("font", font)

  let internal readFont () =
    use key =
      Registry.CurrentUser.CreateSubKey(coveragepath)

    key.GetValue("font", "Monospace Normal 10") :?> string

  let internal saveGeometry (w: Window) =
    use key =
      Registry.CurrentUser.CreateSubKey(geometry)

    let (x, y) = w.GetPosition()
    key.SetValue("x", x)
    key.SetValue("y", y)
    let (width, height) = w.GetSize()
    key.SetValue("width", width)
    key.SetValue("height", height)

  let internal readGeometry (w: Window) =
    use key =
      Registry.CurrentUser.CreateSubKey(geometry)

    let width =
      Math.Max(key.GetValue("width", 600) :?> int, 600)

    let height =
      Math.Max(key.GetValue("height", 450) :?> int, 450)

    let screen = w.Screen

    let bounds0 = screen.GetMonitorGeometry(0)

    let x =
      key.GetValue("x", bounds0.Left + ((bounds0.Width - width) / 2)) :?> int

    let y =
      key.GetValue("y", bounds0.Top + ((bounds0.Height - height) / 2)) :?> int

    let monitor =
      { 0 .. screen.NMonitors }
      |> Seq.filter (fun i ->
        let bounds = screen.GetMonitorGeometry(i)

        x >= bounds.Left
        && x <= bounds.Right
        && y >= bounds.Top
        && y <= bounds.Bottom)
      |> Seq.tryHead
      |> Option.defaultValue 0

    let bounds =
      screen.GetMonitorGeometry(monitor)

    let x' =
      Math.Min(Math.Max(x, bounds.Left), bounds.Right - width)

    let y' =
      Math.Min(Math.Max(y, bounds.Top), bounds.Bottom - height)

    w.Move(x', y')
    w.DefaultHeight <- height
    w.DefaultWidth <- width

  [<SuppressMessage("Gendarme.Rules.Exceptions",
                    "InstantiateArgumentExceptionCorrectlyRule",
                    Justification = "Inlined library code")>]
  [<SuppressMessage("Microsoft.Usage",
                    "CA2208:InstantiateArgumentExceptionsCorrectly",
                    Justification = "Ditto, ditto")>]
  let internal readCoverageFiles (handler: IVisualizerWindow) =
    use fileKey =
      Registry.CurrentUser.CreateSubKey(recent)

    let keyToValue (key: RegistryKey) (n: string) = key.GetValue(n, String.Empty)

    let names =
      fileKey.GetValueNames()
      |> Array.filter (fun (s: string) -> s.Length = 1 && Char.IsDigit(s.Chars(0)))
      |> Array.sortBy (Int32.TryParse >> snd)

    let files =
      names
      |> Array.map (keyToValue fileKey)
      |> Seq.cast<string>
      |> Seq.filter (String.IsNullOrWhiteSpace >> not)
      |> Seq.filter File.Exists
      |> Seq.toList

    handler.CoverageFiles <- files

  let saveCoverageFiles files =
    // Update the recent files menu and registry store from memory cache
    // with new most recent file
    let regDeleteKey (key: RegistryKey) (name: string) = key.DeleteValue(name)

    let regSetKey (key: RegistryKey) (index: int) (name: string) =
      key.SetValue(index.ToString(), name)

    use fileKey =
      Registry.CurrentUser.CreateSubKey(recent)

    fileKey.GetValueNames()
    |> Seq.iter (regDeleteKey fileKey)

    files |> Seq.iteri (regSetKey fileKey)

  let clearGeometry () =
    do
      use k1 =
        Registry.CurrentUser.CreateSubKey(geometry)

      ()

    Registry.CurrentUser.DeleteSubKeyTree(geometry)
#endif

[<assembly: SuppressMessage("Microsoft.Performance",
                            "CA1810:InitializeReferenceTypeStaticFieldsInline",
                            Scope = "member",
                            Target = "<StartupCode$AltCover-Visualizer>.$Persistence.#.cctor()",
                            Justification = "Compiler generated")>]
()