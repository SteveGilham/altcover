#if RUNNER
namespace AltCover
#else
namespace AltCover_Fake.DotNet.Testing
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
                                  Justification = "Idiomatic F#")>]
  type CLIArgs =
    | Force of bool
    | FailFast of bool
    | Many of CLIArgs seq
  with member self.ForceDelete =
        match self with
        | Force b -> b
        | FailFast _ -> false
        | Many s -> s |> Seq.exists(fun f -> f.ForceDelete)
       member self.Fast =
        match self with
        | FailFast b -> b
        | Force _ -> false
        | Many s -> s |> Seq.exists(fun f -> f.Fast)

#if RUNNER
#else
module internal Internals =
#endif

  let private Arg name s = (sprintf """/p:AltCover%s="%s" """ name s).Trim()
  let private ListArg name (s : String seq) =
    (sprintf """/p:AltCover%s="%s" """ name <| String.Join("|", s)).Trim()

  let private IsSet s =
    s
    |> String.IsNullOrWhiteSpace
    |> not

  let private FromList name (s : String seq) = (ListArg name s, s.Any())
  let private FromArg name s = (Arg name s, IsSet s)
  let private Join(l : string list) = String.Join(" ", l)

#if RUNNER
  let ToTestArgumentList
      (prepare : AltCover.FSApi.PrepareParams)
      (collect : AltCover.FSApi.CollectParams)
      (force : CLIArgs) =
#else
  let ToTestArgumentList
      (prepare : AltCover_Fake.DotNet.Testing.AltCover.PrepareParams)
      (collect : AltCover_Fake.DotNet.Testing.AltCover.CollectParams)
      (force : DotNet.CLIArgs) =
#endif

    [ FromArg String.Empty "true"
      FromArg "XmlReport" prepare.XmlReport
      (Arg "OpenCover" "false", not prepare.OpenCover)
      FromList "FileFilter" prepare.FileFilter
      FromList "PathFilter" prepare.PathFilter
      FromList "AssemblyFilter" prepare.AssemblyFilter
      FromList "AssemblyExcludeFilter" prepare.AssemblyExcludeFilter
      FromList "TypeFilter" prepare.TypeFilter
      FromList "MethodFilter" prepare.MethodFilter
      FromList "AttributeFilter" prepare.AttributeFilter
      FromList "CallContext" prepare.CallContext
      FromList "DependencyList" prepare.Dependencies
      FromArg "LcovReport" collect.LcovReport
      FromArg "Cobertura" collect.Cobertura
      FromArg "Threshold" collect.Threshold
      (Arg "LineCover" "true", prepare.LineCover)
      (Arg "BranchCover" "true", prepare.BranchCover)
      (Arg "Force" "true", force.ForceDelete)
      (Arg "FailFast" "true", force.Fast)]
    |> List.filter snd
    |> List.map fst

#if RUNNER
  let ToTestArguments
    (prepare : AltCover.FSApi.PrepareParams)
    (collect : AltCover.FSApi.CollectParams)
    (force : CLIArgs) =
#else
  let ToTestArguments
      (prepare : AltCover_Fake.DotNet.Testing.AltCover.PrepareParams)
      (collect : AltCover_Fake.DotNet.Testing.AltCover.CollectParams)
      (force : DotNet.CLIArgs) =
#endif
    ToTestArgumentList prepare collect force |> Join