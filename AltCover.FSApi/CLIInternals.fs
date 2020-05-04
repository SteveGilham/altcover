#if RUNNER
namespace AltCover.FSApi
#else
namespace AltCoverFake.DotNet.Testing
#endif

open System
open System.Diagnostics.CodeAnalysis
open System.Linq

module internal CLIInternals =
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
  let internal toPrepareListArgumentList (prepare : AltCoverFake.DotNet.Testing.AltCover.PrepareOptions) =
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
  let internal toPrepareFromArgArgumentList (prepare : AltCoverFake.DotNet.Testing.AltCover.PrepareOptions) =
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
  let internal toPrepareArgArgumentList (prepare : AltCoverFake.DotNet.Testing.AltCover.PrepareOptions) =
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
  let internal toCollectFromArgArgumentList (collect : AltCoverFake.DotNet.Testing.AltCover.CollectOptions) =
#endif
    [
      fromArg, "LcovReport", collect.LcovReport
      fromArg, "Cobertura", collect.Cobertura
      fromArg, "Threshold", collect.Threshold
      fromArg, "SummaryFormat", collect.SummaryFormat
    ]

  [<SuppressMessage("Gendarme.Rules.Naming", "AvoidRedundancyInMethodNameRule",
                     Justification="Internal implementation detail")>]
  let internal toCLIOptionsFromArgArgumentList (options : DotNet.CLIOptions) =
    [
      fromArg, "ShowSummary", options.Summary
    ]

  [<SuppressMessage("Gendarme.Rules.Naming", "AvoidRedundancyInMethodNameRule",
                     Justification="Internal implementation detail")>]
  let internal toCLIOptionsArgArgumentList (options : DotNet.CLIOptions) =
    [
      arg, "Force", "true", options.ForceDelete
      arg, "FailFast", "true", options.Fast
    ]