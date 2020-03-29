#if RUNNER
namespace AltCover
#else
namespace AltCoverFake.DotNet.Testing
#endif

open System
open System.Diagnostics.CodeAnalysis
open System.Linq

[<assembly:CLSCompliant(true)>]
[<assembly:System.Runtime.InteropServices.ComVisible(false)>]
do ()

[<RequireQualifiedAccess>]
module DotNet =
  [<NoComparison; SuppressMessage("Microsoft.Design", "CA1034",
                                  Justification = "Idiomatic F#");
                  SuppressMessage("Gendarme.Rules.Smells",
                                  "RelaxedAvoidCodeDuplicatedInSameClassRule",
                                  Justification = "Idiomatic F#");
    AutoSerializable(false)>]
  type CLIArgs =
    | Force of bool
    | FailFast of bool
    | ShowSummary of String
    | Many of CLIArgs seq

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

#if RUNNER
#else
module internal Internals =
#endif

  let private arg name s = (sprintf """/p:AltCover%s="%s" """ name s).Trim()
  let private listArg name (s : String seq) =
    (sprintf """/p:AltCover%s="%s" """ name <| String.Join("|", s)).Trim()

  let private isSet s =
    s
    |> String.IsNullOrWhiteSpace
    |> not

  let private fromList name (s : String seq) = (listArg name s, s.Any())
  let private fromArg name s = (arg name s, isSet s)
  let private join(l : string seq) = String.Join(" ", l)

#if RUNNER
  let ToTestArgumentList (prepare : AltCover.FSApi.PrepareParameters)
      (collect : AltCover.FSApi.CollectParameters) (force : CLIArgs) =
#else
  let toTestArgumentList (prepare : AltCoverFake.DotNet.Testing.AltCover.PrepareParameters)
      (collect : AltCoverFake.DotNet.Testing.AltCover.CollectParameters)
      (force : DotNet.CLIArgs) =
#endif

    [ fromArg String.Empty "true"
      fromArg "XmlReport" prepare.XmlReport
      (arg "OpenCover" "false", not prepare.OpenCover)
      fromList "FileFilter" prepare.FileFilter
      fromList "PathFilter" prepare.PathFilter
      fromList "AssemblyFilter" prepare.AssemblyFilter
      fromList "AssemblyExcludeFilter" prepare.AssemblyExcludeFilter
      fromList "TypeFilter" prepare.TypeFilter
      fromList "MethodFilter" prepare.MethodFilter
      fromList "AttributeFilter" prepare.AttributeFilter
      (arg "LocalSource" "true", prepare.LocalSource)
      (arg "VisibleBranches" "true", prepare.VisibleBranches)
      fromList "CallContext" prepare.CallContext
      fromList "DependencyList" prepare.Dependencies
      fromArg "LcovReport" collect.LcovReport
      fromArg "Cobertura" collect.Cobertura
      fromArg "Threshold" collect.Threshold
      fromArg "StrongNameKey" prepare.StrongNameKey
      fromList "Keys" prepare.Keys
      (arg "LineCover" "true", prepare.LineCover)
      (arg "BranchCover" "true", prepare.BranchCover)
      (arg "Force" "true", force.ForceDelete)
      (arg "FailFast" "true", force.Fast)
      (fromArg "ShowSummary" force.Summary)
      (fromArg "SummaryFormat" collect.SummaryFormat)
      (fromArg "ShowStatic" prepare.ShowStatic)
      (arg "ShowGenerated" "true", prepare.ShowGenerated) ]
    |> List.filter snd
    |> List.map fst

#if RUNNER
  let ToTestArguments (prepare : AltCover.FSApi.PrepareParameters)
      (collect : AltCover.FSApi.CollectParameters) (force : CLIArgs) =
    ToTestArgumentList prepare collect force |> join
#else
  let toTestArguments (prepare : AltCoverFake.DotNet.Testing.AltCover.PrepareParameters)
      (collect : AltCoverFake.DotNet.Testing.AltCover.CollectParameters)
      (force : DotNet.CLIArgs) =
    toTestArgumentList prepare collect force |> join
#endif