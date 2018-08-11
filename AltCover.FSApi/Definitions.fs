namespace AltCover

open System
open System.Linq

module DotNet =
    let private Arg name s =
      (sprintf """/p:AltCover%s="%s" """ name s).Trim()

    let private ListArg name (s:String array) =
      (sprintf """/p:AltCover%s="%s" """ name <| String.Join("|", s)).Trim()

    let private IsSet s =
        s |> String.IsNullOrWhiteSpace |> not

    let private FromList name (s:String array) =
        (ListArg name s, s.Any())

    let private FromArg name s =
      (Arg name s, IsSet s)

    let Join (l : string list) =
      String.Join(" ", l)

    let ToTestArguments (prepare:PrepareParams) (collect:CollectParams) =
      [ ("/p:AltCover=true", true)
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
      ]
      |> List.filter snd
      |> List.map fst
      |> Join

    let CSToTestArguments (prepare:PrepareParams, collect:CollectParams) =
      ToTestArguments prepare collect