namespace AltCover

open System
open System.Diagnostics.CodeAnalysis

[<ExcludeFromCodeCoverage; NoComparison>]
type CollectParams =
  { RecorderDirectory : String
    WorkingDirectory : String
    Executable : String
    LcovReport : String
    Threshold : String
    Cobertura : String
    OutputFile : String
    CommandLine : String }
  static member Default : CollectParams =
    { RecorderDirectory = String.Empty
      WorkingDirectory = String.Empty
      Executable = String.Empty
      LcovReport = String.Empty
      Threshold = String.Empty
      Cobertura = String.Empty
      OutputFile = String.Empty
      CommandLine = String.Empty }

[<ExcludeFromCodeCoverage; NoComparison>]
type PrepareParams =
  { InputDirectory : String
    OutputDirectory : String
    SymbolDirectories : string seq
    Dependencies : string seq
    Keys : string seq
    StrongNameKey : String
    XmlReport : String
    FileFilter : string seq
    AssemblyFilter : string seq
    AssemblyExcludeFilter : string seq
    TypeFilter : string seq
    MethodFilter : string seq
    AttributeFilter : string seq
    PathFilter : string seq
    CallContext : string seq
    OpenCover : bool
    InPlace : bool
    Save : bool
    Single : bool
    LineCover : bool
    BranchCover : bool
    CommandLine : String }
  static member Default : PrepareParams =
    { InputDirectory = String.Empty
      OutputDirectory = String.Empty
      SymbolDirectories = [||]
      Dependencies = [||]
      Keys = [||]
      StrongNameKey = String.Empty
      XmlReport = String.Empty
      FileFilter = [||]
      AssemblyFilter = [||]
      AssemblyExcludeFilter = [||]
      TypeFilter = [||]
      MethodFilter = [||]
      AttributeFilter = [||]
      PathFilter = [||]
      CallContext = [||]
      OpenCover = true
      InPlace = true
      Save = true
      Single = false
      LineCover = false
      BranchCover = false
      CommandLine = String.Empty }

module Args =
  let internal Item a x =
    if x |> String.IsNullOrWhiteSpace then []
    else [ a; x ]

  let internal ItemList a x =
    if x |> isNull then []
    else
      x
      |> Seq.collect (fun i -> [ a; i ])
      |> Seq.toList

  let internal Flag a x =
    if x then [ a ]
    else []

  let Prepare(args : PrepareParams) =
    [ Item "-i" args.InputDirectory
      Item "-o" args.OutputDirectory
      ItemList "-y" args.SymbolDirectories
      ItemList "-d" args.Dependencies
      ItemList "-k" args.Keys
      Item "--sn" args.StrongNameKey
      Item "-x" args.XmlReport
      ItemList "-f" args.FileFilter
      ItemList "-s" args.AssemblyFilter
      ItemList "-e" args.AssemblyExcludeFilter
      ItemList "-t" args.TypeFilter
      ItemList "-m" args.MethodFilter
      ItemList "-a" args.AttributeFilter
      ItemList "-p" args.PathFilter
      ItemList "-c" args.CallContext
      Flag "--opencover" args.OpenCover
      Flag "--inplace" args.InPlace
      Flag "--save" args.Save
      Flag "--single" args.Single
      Flag "--linecover" args.LineCover
      Flag "--branchcover" args.BranchCover
      Item "--" args.CommandLine ]
    |> List.concat

  let Collect(args : CollectParams) =
    [ [ "Runner" ]
      Item "-r" args.RecorderDirectory
      Item "-w" args.WorkingDirectory
      Item "-x" args.Executable
      Item "-l" args.LcovReport
      Item "-t" args.Threshold
      Item "-c" args.Cobertura
      Item "-o" args.OutputFile
      Flag "--collect" (args.Executable |> String.IsNullOrWhiteSpace)
      Item "--" args.CommandLine ]
    |> List.concat