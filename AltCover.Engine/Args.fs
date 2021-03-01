#if RUNNER
namespace AltCover
#else
namespace AltCoverFake.DotNet.Testing
#endif

open System
open System.Diagnostics.CodeAnalysis

[<SuppressMessage("Gendarme.Rules.Smells",
                  "RelaxedAvoidCodeDuplicatedInSameClassRule",
                  Justification = "Not worth trying to unify these functions")>]
module internal Args =
  let private item a x =
    if x |> String.IsNullOrWhiteSpace then
      []
    else
      [ a; x ]

  let private optionalItem a x l =
    if x |> String.IsNullOrWhiteSpace
       || l |> List.exists (fun i -> i = x) then
      []
    else
      [ a + ":" + x ]

  let private flag a x = if x then [ a ] else []

  // directly unit tested for empty case
  let internal itemList a x =
    if x |> isNull then
      []
    else
      x |> Seq.collect (fun i -> [ a; i ]) |> Seq.toList

  let internal listItems (args: Abstract.IPrepareOptions) =
    [ ("-i", args.InputDirectories)
      ("-o", args.OutputDirectories)
      ("-y", args.SymbolDirectories)
      ("-d", args.Dependencies)
      ("-k", args.Keys)
      ("-f", args.FileFilter)
      ("-s", args.AssemblyFilter)
      ("-e", args.AssemblyExcludeFilter)
      ("-t", args.TypeFilter)
      ("-m", args.MethodFilter)
      ("-a", args.AttributeFilter)
      ("-p", args.PathFilter)
      ("--attributetoplevel", args.AttributeTopLevel)
      ("--typetoplevel", args.TypeTopLevel)
      ("--methodtoplevel", args.MethodTopLevel)
      ("-c", args.CallContext) ]

  let internal itemLists (args: Abstract.IPrepareOptions) =
    args
    |> listItems
    |> List.collect (fun (a, b) -> itemList a b)

  let internal plainItems (args: Abstract.IPrepareOptions) =
    [ ("--sn", args.StrongNameKey)
      ("--reportFormat", args.ReportFormat)
      ("-r", args.Report) ]

  let internal items (args: Abstract.IPrepareOptions) =
    args
    |> plainItems
    |> List.collect (fun (a, b) -> item a b)

  let internal options (args: Abstract.IPrepareOptions) =
    [ ("--showstatic", args.ShowStatic, [ "-" ]) ]

  let internal optItems (args: Abstract.IPrepareOptions) =
    args
    |> options
    |> List.collect (fun (a, b, c) -> optionalItem a b c)

  let internal flagItems (args: Abstract.IPrepareOptions) =
    [ ("--inplace", args.InPlace)
      ("--save", args.Save)
      ("--zipfile", args.ZipFile)
      ("--methodpoint", args.MethodPoint)
      ("--single", args.SingleVisit)
      ("--linecover", args.LineCover)
      ("--branchcover", args.BranchCover)
      ("--dropReturnCode", (args.ExposeReturnCode |> not))
      ("--sourcelink", args.SourceLink)
      ("--defer", args.Defer)
      ("--localSource", args.LocalSource)
      ("--visibleBranches", args.VisibleBranches)
      ("--showGenerated", args.ShowGenerated) ]

  let internal flags (args: Abstract.IPrepareOptions) =
    args
    |> flagItems
    |> List.collect (fun (a, b) -> flag a b)

  let internal countItems (args: Abstract.IPrepareOptions) =
    [ ("-q", 2 - (int args.Verbosity)) ]

  let internal counts (args: Abstract.IPrepareOptions) =
    args
    |> countItems
    |> List.collect (fun (a, b) -> { 0 .. b } |> Seq.map (fun _ -> a) |> Seq.toList)

  let prepare (args: Abstract.IPrepareOptions) =
    let argsList = args.CommandLine |> Seq.toList

    let trailing =
      if List.isEmpty argsList then
        []
      else
        "--" :: argsList

    let parameters =
      [ itemLists
        items
        optItems
        flags
        counts ]
      |> List.collect (fun f -> f args)

    [ parameters; trailing ] |> List.concat

  let internal buildCollect (args: Abstract.ICollectOptions) =
    let argsList = args.CommandLine |> Seq.toList

    let trailing =
      if List.isEmpty argsList then
        []
      else
        "--" :: argsList

    let exe = args.Executable

    let countItems (args: Abstract.ICollectOptions) = [ ("-q", 2 - (int args.Verbosity)) ]

    let counts (args: Abstract.ICollectOptions) =
      args
      |> countItems
      |> List.collect (fun (a, b) -> { 0 .. b } |> Seq.map (fun _ -> a) |> Seq.toList)

    [ [ "Runner" ]
      item "-r" args.RecorderDirectory
      item "-w" args.WorkingDirectory
      item "-x" exe
      item "-l" args.LcovReport
      item "-t" args.Threshold
      item "-c" args.Cobertura
      item "-o" args.OutputFile
      flag "--collect" (exe |> String.IsNullOrWhiteSpace)
      flag "--dropReturnCode" (args.ExposeReturnCode |> not)
      optionalItem "--summary" args.SummaryFormat []
      counts args
      trailing ]

  let collect (args: Abstract.ICollectOptions) = args |> buildCollect |> List.concat