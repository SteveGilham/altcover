#if RUNNER
namespace AltCover
#else
namespace AltCoverFake.DotNet.Testing
#endif
  [<RequireQualifiedAccess>]
  module Primitive = begin
    [<NoComparisonAttribute ()>]
    type CollectParameters =
      { RecorderDirectory: System.String
        WorkingDirectory: System.String
        Executable: System.String
        LcovReport: System.String
        Threshold: System.String
        Cobertura: System.String
        OutputFile: System.String
        CommandLine: seq<System.String>
        ExposeReturnCode: bool
        SummaryFormat: System.String }
      with
        static member Create : unit -> CollectParameters
      end
    [<NoComparisonAttribute ()>]
    type PrepareParameters =
      { InputDirectories: seq<System.String>
        OutputDirectories: seq<System.String>
        SymbolDirectories: seq<System.String>
        Dependencies: seq<System.String>
        Keys: seq<System.String>
        StrongNameKey: System.String
        XmlReport: System.String
        FileFilter: seq<System.String>
        AssemblyFilter: seq<System.String>
        AssemblyExcludeFilter: seq<System.String>
        TypeFilter: seq<System.String>
        MethodFilter: seq<System.String>
        AttributeFilter: seq<System.String>
        PathFilter: seq<System.String>
        CallContext: seq<System.String>
        ReportFormat: System.String
        InPlace: bool
        Save: bool
        ZipFile: bool
        MethodPoint: bool
        Single: bool
        LineCover: bool
        BranchCover: bool
        CommandLine: seq<System.String>
        ExposeReturnCode: bool
        SourceLink: bool
        Defer: bool
        LocalSource: bool
        VisibleBranches: bool
        ShowStatic: string
        ShowGenerated: bool }
      with
        static member Create : unit -> PrepareParameters
      end
#if RUNNER
    [<NoComparison; NoEquality>]
    type Logging =
      { Info : System.String -> unit
        Warn : System.String -> unit
        Error : System.String -> unit
        Echo : System.String -> unit }
      with
        static member Create : unit -> Logging
      end
#endif
  end