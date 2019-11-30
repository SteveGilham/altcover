namespace Tests.Visualizer

open System
open System.IO
open System.Reflection

open AltCover.Augment
open NUnit.Framework
open Swensen.Unquote

[<TestFixture>]
type AltCoverTests() =
  class

    // Augment.fs

    [<Test>]
    member self.AugmentNullableDetectNulls() =
      let input = [ "string"; null; "another string" ]
      let nulls = input |> List.map (Option.nullable >> Option.isNone)
      test <@ nulls = [ false; true; false ] @>

    [<Test>]
    member self.AugmentGetOrElseFillsInNone() =
      let input = [ "string"; null; "another string" ]
      let strings = input |> List.map (Option.nullable >> (Option.getOrElse "fallback"))
      test <@ strings =[ "string"; "fallback"; "another string" ] @>

  // GuiCommon.fs
    [<Test>]
    member self.MethodsSortAsExpected() =
      let methods = Assembly.GetExecutingAssembly().GetManifestResourceNames()
                    |> Seq.find (fun n -> n.EndsWith("Methods.txt", StringComparison.Ordinal))
      let sorteds = Assembly.GetExecutingAssembly().GetManifestResourceNames()
                    |> Seq.find (fun n -> n.EndsWith("Sorted.txt", StringComparison.Ordinal))

      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(methods)
      use reader = new StreamReader(stream)
      let names = reader
                  |> Seq.unfold (fun r -> let line = r.ReadLine()
                                          if isNull line then None
                                          else Some (line, r))
                  |> Seq.toArray

      use stream' =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(sorteds)
      use reader' = new StreamReader(stream')
      let sorts = reader'
                  |> Seq.unfold (fun r -> let line = r.ReadLine()
                                          if isNull line then None
                                          else Some (line, r))
                  |> Seq.toArray

      printfn "%A" names

      let keys = names
                 |> Array.map (fun n -> {
                                          AltCover.Visualizer.GuiCommon.MethodKey.m = null
                                          AltCover.Visualizer.GuiCommon.MethodKey.spacename = String.Empty
                                          AltCover.Visualizer.GuiCommon.MethodKey.classname = String.Empty
                                          AltCover.Visualizer.GuiCommon.MethodKey.name = n
                                        })
      Array.sortInPlaceWith AltCover.Visualizer.GuiCommon.MethodNameCompare keys
      let sorted = keys
                   |> Array.map (fun k -> k.name)
      printfn "%A" sorted

      Seq.zip sorts sorted
      |> Seq.iteri (fun i (l,r) -> let k = i.ToString()
                                   test <@ (k + " " + l) = (k + " " + r) @>)

      //test <@ String.Join("; ", sorted) =  String.Join("; ", sorts) @>

  end