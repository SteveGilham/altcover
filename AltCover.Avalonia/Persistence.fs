namespace AltCover

open System

open Avalonia
open Avalonia.Controls

module Persistence =
  let mutable internal save = true

  let internal saveSchemaDir = Configuration.SaveSchemaDir
  let internal saveFont = Configuration.SaveFont
  let internal readFont = Configuration.ReadFont
  let internal readSchemaDir = Configuration.ReadSchemaDir
  let internal readFolder = Configuration.ReadFolder
  let internal saveFolder = Configuration.SaveFolder
  let internal saveCoverageFiles = Configuration.SaveCoverageFiles

  let readCoverageFiles () =
    let mutable l = []
    Configuration.ReadCoverageFiles(fun files -> l <- files)
    l

  let saveGeometry (w: Window) =
    Configuration.SaveGeometry
      (fun () -> w.Position.X, w.Position.Y)
      (fun () -> w.Width, w.Height)

  let readGeometry (w: Window) =
    Configuration.ReadGeometry
      (fun (width, height) (x, y) ->
        w.Height <- float height
        w.Width <- float width

        let monitor =
          w.Screens.All
          |> Seq.filter
               (fun s ->
                 let tl = s.WorkingArea.TopLeft
                 let br = s.WorkingArea.BottomRight
                 x >= tl.X && x <= br.X && y >= tl.Y && y <= br.Y)
          |> Seq.tryHead
          |> Option.defaultValue w.Screens.Primary

        let bounds = monitor.WorkingArea

        let x' =
          Math.Min(Math.Max(x, bounds.TopLeft.X), bounds.BottomRight.X - width)

        let y' =
          Math.Min(Math.Max(y, bounds.TopLeft.Y), bounds.BottomRight.Y - height)

        w.Position <- PixelPoint(x', y'))

  let clearGeometry = Configuration.ClearGeometry