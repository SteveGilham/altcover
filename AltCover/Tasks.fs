namespace AltCover

open System
open Microsoft.Build.Utilities

type ACVPrepare () =
  inherit Task(null)

  member val InPlace = true with get, set

  override self.Execute () = true

type ACVCollect () =
  inherit Task(null)

  member val Executable = String.Empty with get, set

  override self.Execute () = true