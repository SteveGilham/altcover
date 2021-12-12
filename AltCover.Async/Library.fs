namespace AltCover.Recorder

open System.Collections.Generic
open System.Threading

module Instance =
  module I =
    module private CallTrack =

      let attr =
        System.Runtime.Versioning.TargetFrameworkAttribute(".NETFramework,Version=v4.6")

      let value = AsyncLocal<Stack<int>>()

      let instance () =
        match value.Value with
        | null -> value.Value <- Stack<int>()
        | _ -> ()

        value.Value