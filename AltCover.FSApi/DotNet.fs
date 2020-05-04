#if RUNNER
namespace AltCover.FSApi
#else
namespace AltCoverFake.DotNet.Testing
#endif

open System
open System.Diagnostics.CodeAnalysis
open System.Linq

[<RequireQualifiedAccess>]
module DotNet =
  [<NoComparison; SuppressMessage("Microsoft.Design", "CA1034",
                                  Justification = "Idiomatic F#");
                  SuppressMessage("Gendarme.Rules.Smells",
                                  "RelaxedAvoidCodeDuplicatedInSameClassRule",
                                  Justification = "Idiomatic F#");
    AutoSerializable(false)>]
  type CLIOptions =
    | Force of bool
    | FailFast of bool
    | ShowSummary of String
    | Many of CLIOptions seq

    member self.ForceDelete =
      match self with
      | Force b -> b
      | ShowSummary _
      | FailFast _ -> false
      | Many s -> s |> Seq.exists (fun f -> f.ForceDelete)

    member self.Fast =
      match self with
      | FailFast b -> b
      | ShowSummary _
      | Force _ -> false
      | Many s -> s |> Seq.exists (fun f -> f.Fast)

    member self.Summary =
      match self with
      | ShowSummary b -> b
      | FailFast _
      | Force _ -> String.Empty
      | Many s ->
          match s
                |> Seq.map (fun f -> f.Summary)
                |> Seq.filter (String.IsNullOrWhiteSpace >> not)
                |> Seq.tryHead with
          | Some x -> x
          | _ -> String.Empty

  module internal I =
    let private arg name s = (sprintf """/p:AltCover%s="%s" """ name s).Trim()
    let private listArg name (s : String seq) =
      (sprintf """/p:AltCover%s="%s" """ name <| String.Join("|", s)).Trim()

    let private isSet s =
      s
      |> String.IsNullOrWhiteSpace
      |> not

    let private fromList name (s : String seq) = (listArg name s, s.Any())
    let internal fromArg name s = (arg name s, isSet s)
    let internal join(l : string seq) = String.Join(" ", l)

    [<SuppressMessage("Gendarme.Rules.Design.Generic", "AvoidMethodWithUnusedGenericTypeRule",
                       Justification="Compiler Generated")>]
  #if RUNNER
    let internal toPrepareListArgumentList (prepare : AltCover.OptionApi.PrepareOptions) =
  #else
    let internal toPrepareListArgumentList (prepare : OptionApi.PrepareOptions) =
  #endif
      [
        fromList, "SymbolDirectories", prepare.SymbolDirectories
        fromList, "DependencyList", prepare.Dependencies
        fromList, "Keys", prepare.Keys
        fromList, "FileFilter", prepare.FileFilter
        fromList, "AssemblyFilter", prepare.AssemblyFilter
        fromList, "AssemblyExcludeFilter", prepare.AssemblyExcludeFilter
        fromList, "TypeFilter", prepare.TypeFilter
        fromList, "MethodFilter", prepare.MethodFilter
        fromList, "AttributeFilter", prepare.AttributeFilter
        fromList, "PathFilter", prepare.PathFilter
        fromList, "CallContext", prepare.CallContext
      ]

  #if RUNNER
    let internal toPrepareFromArgArgumentList (prepare : AltCover.OptionApi.PrepareOptions) =
  #else
    let internal toPrepareFromArgArgumentList (prepare : OptionApi.PrepareOptions) =
  #endif
      [
        fromArg, "StrongNameKey", prepare.StrongNameKey
        fromArg, "XmlReport", prepare.XmlReport
        fromArg, "ReportFormat", prepare.ReportFormat
        fromArg, "ShowStatic", prepare.ShowStatic
      ]

  #if RUNNER
    let internal toPrepareArgArgumentList (prepare : AltCover.OptionApi.PrepareOptions) =
  #else
    let internal toPrepareArgArgumentList (prepare : OptionApi.PrepareOptions) =
  #endif
      [
        (arg, "ZipFile", "false", prepare.ZipFile)
        (arg, "MethodPoint", "false", prepare.MethodPoint)
        (arg, "Single", "false", prepare.Single)
        (arg, "LineCover", "true", prepare.LineCover)
        (arg, "BranchCover", "true", prepare.BranchCover)
        (arg, "SourceLink", "false", prepare.SourceLink)
        (arg, "LocalSource", "true", prepare.LocalSource)
        (arg, "VisibleBranches", "true", prepare.VisibleBranches)
        (arg, "ShowGenerated", "true", prepare.ShowGenerated)
      ]

  #if RUNNER
    let internal toCollectFromArgArgumentList (collect : AltCover.OptionApi.CollectOptions) =
  #else
    let internal toCollectFromArgArgumentList (collect : OptionApi.CollectOptions) =
  #endif
      [
        fromArg, "LcovReport", collect.LcovReport
        fromArg, "Cobertura", collect.Cobertura
        fromArg, "Threshold", collect.Threshold
        fromArg, "SummaryFormat", collect.SummaryFormat
      ]

    [<SuppressMessage("Gendarme.Rules.Naming", "AvoidRedundancyInMethodNameRule",
                       Justification="Internal implementation detail")>]
    let internal toCLIOptionsFromArgArgumentList (options : CLIOptions) =
      [
        fromArg, "ShowSummary", options.Summary
      ]

    [<SuppressMessage("Gendarme.Rules.Naming", "AvoidRedundancyInMethodNameRule",
                       Justification="Internal implementation detail")>]
    let internal toCLIOptionsArgArgumentList (options : CLIOptions) =
      [
        arg, "Force", "true", options.ForceDelete
        arg, "FailFast", "true", options.Fast
      ]

#if RUNNER
  let ToTestArgumentList (prepare : AltCover.OptionApi.PrepareOptions)
      (collect : AltCover.OptionApi.CollectOptions) (options : CLIOptions) =
#else
  let internal toTestArgumentList (prepare : OptionApi.PrepareOptions)
      (collect : OptionApi.CollectOptions)
      (options : CLIOptions) =
#endif
    [
      [ I.fromArg String.Empty "true" ]
      prepare
      |> I.toPrepareListArgumentList
      |> List.map(fun (f,n,a) -> f n a)
      prepare
      |> I.toPrepareFromArgArgumentList
      |> List.map(fun (f,n,a) -> f n a)
      prepare
      |> I.toPrepareArgArgumentList
      |> List.map(fun (f,n,a,x) -> (f n a,x))

      collect
      |> I.toCollectFromArgArgumentList
      |> List.map(fun (f,n,a) -> f n a)

      options
      |> I.toCLIOptionsFromArgArgumentList
      |> List.map(fun (f,n,a) -> f n a)

      options
      |> I.toCLIOptionsArgArgumentList
      |> List.map(fun (f,n,a,x) -> (f n a,x))
    ]
    |> List.concat
    |> List.filter snd
    |> List.map fst

#if RUNNER
  let ToTestArguments (prepare : AltCover.OptionApi.PrepareOptions)
      (collect : AltCover.OptionApi.CollectOptions) (options : CLIOptions) =
    ToTestArgumentList prepare collect options |> I.join
#else
  let internal toTestArguments (prepare : OptionApi.PrepareOptions)
      (collect : OptionApi.CollectOptions)
      (options : CLIOptions) =
    toTestArgumentList prepare collect options |> I.join
#endif