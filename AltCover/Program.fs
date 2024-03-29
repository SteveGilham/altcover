﻿namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.Runtime.InteropServices
#if GLOBALTOOL
open System.IO
open System.Reflection
open AltCover.Main
#endif

[<assembly: CLSCompliant(true)>]
[<assembly: ComVisible(false)>]
()

#if DEBUG
[<SuppressMessage("Gendarme.Rules.Performance",
                  "AvoidUninstantiatedInternalClassesRule",
                  Justification = "Like the name says")>]
type internal Marker =
  | DummyValueForReflectiveAccess = 0
#endif

module EntryPoint =
  [<EntryPoint>]
#if GLOBALTOOL
  [<SuppressMessage("Gendarme.Rules.Portability",
                    "DoNotHardcodePathsRule",
                    Justification = "Safe hard-coded in-pacakge relative path")>]
#endif
  let private main arguments =
    CommandLine.toConsole ()

    let result =
#if GLOBALTOOL
      let first =
        arguments
        |> Seq.tryHead
        |> Option.defaultValue String.Empty

      init ()

      match first with
      | Select "TargetsPath" _ ->
        let here =
          Assembly.GetExecutingAssembly().Location

        let targets =
          Path.Combine(
            here |> Path.GetDirectoryName,
            "../../../build/netstandard2.0/altcover.global.targets"
          )
          |> Path.GetFullPath

        targets |> Output.info
        0
      | _ ->
#endif
      AltCover.Main.effectiveMain arguments

    result