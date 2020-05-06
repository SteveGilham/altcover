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
    if x |> String.IsNullOrWhiteSpace then [] else [ a; x ]

  let private optionalItem a x l =
    if x
        |> String.IsNullOrWhiteSpace
        || l |> List.exists (fun i -> i = x) then
      []
    else
      [ a + ":" + x ]

  let private flag a x =
    if x then [ a ] else []

  // directly unit tested for empty case
  let internal itemList a x =
    if x |> isNull then
      []
    else
      x
      |> Seq.collect (fun i -> [ a; i ])
      |> Seq.toList

  let internal listItems(args : AltCover.PrepareOptions) =
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
      ("-c", args.CallContext) ]

  let internal itemLists(args : AltCover.PrepareOptions) =
    args
    |> listItems
    |> List.collect (fun (a, b) -> itemList a b)

  let internal plainItems(args : AltCover.PrepareOptions) =
    [ ("--sn", args.StrongNameKey)
      ("--reportFormat", args.ReportFormat)
      ("-x", args.XmlReport) ]

  let internal items(args : AltCover.PrepareOptions) =
    args
    |> plainItems
    |> List.collect (fun (a, b) -> item a b)

  let internal options(args : AltCover.PrepareOptions) =
    [ ("--showstatic", args.ShowStatic, [ "-" ]) ]

  let internal optItems(args : AltCover.PrepareOptions) =
    args
    |> options
    |> List.collect (fun (a, b, c) -> optionalItem a b c)

  let internal flagItems(args : AltCover.PrepareOptions) =
    [ ("--inplace", args.InPlace)
      ("--save", args.Save)
      ("--zipfile", args.ZipFile)
      ("--methodpoint", args.MethodPoint)
      ("--single", args.Single)
      ("--linecover", args.LineCover)
      ("--branchcover", args.BranchCover)
      ("--dropReturnCode", (args.ExposeReturnCode |> not))
      ("--sourcelink", args.SourceLink)
      ("--defer", args.Defer)
      ("--localSource", args.LocalSource)
      ("--visibleBranches", args.VisibleBranches)
      ("--showGenerated", args.ShowGenerated) ]

  let internal flags(args : AltCover.PrepareOptions) =
    args
    |> flagItems
    |> List.collect (fun (a, b) -> flag a b)

  let prepare(args : AltCover.PrepareOptions) =
    let argsList = args.CommandLine |> Seq.toList

    let trailing =
      if List.isEmpty argsList then [] else "--" :: argsList

    let parameters =
      [ itemLists; items; optItems; flags ] |> List.collect (fun f -> f args)

    [ parameters; trailing ] |> List.concat

  let internal buildCollect(args : AltCover.CollectOptions) =
    let argsList = args.CommandLine |> Seq.toList

    let trailing =
      if List.isEmpty argsList then [] else "--" :: argsList

    let exe = args.Executable

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
      optionalItem "--teamcity" args.SummaryFormat []
      trailing ]

  let collect(args : AltCover.CollectOptions) =
    args
    |> buildCollect
    |> List.concat