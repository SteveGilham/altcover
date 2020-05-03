#if RUNNER
namespace AltCover
  [<RequireQualifiedAccess>]
  module FSApi = begin
    [<NoComparison>]
    type ValidatedCommandLine =
      { Command: string list
        Errors: seq<string> }
      with
        override ToString : unit -> string
      end
#else
namespace AltCoverFake.DotNet.Testing
  [<RequireQualifiedAccess>]
  module AltCover = begin
#endif
    [<NoComparison>]
    type CollectOptions =
      | Primitive of Primitive.CollectOptions
      | TypeSafe of TypeSafe.CollectOptions
      with
        member Cobertura : System.String
        member CommandLine : seq<string>
        member Executable : System.String
        member ExposeReturnCode : bool
        member LcovReport : System.String
        member OutputFile : System.String
        member RecorderDirectory : System.String
        member SummaryFormat : System.String
        member Threshold : System.String
        member WorkingDirectory : System.String
#if RUNNER
        member Validate : bool -> string []
#endif
      end
    [<NoComparison>]
    type PrepareOptions =
      | Primitive of Primitive.PrepareOptions
      | TypeSafe of TypeSafe.PrepareOptions
      with
        member AssemblyExcludeFilter : System.String list
        member AssemblyFilter : System.String list
        member AttributeFilter : System.String list
        member BranchCover : bool
        member CallContext : System.String list
        member CommandLine : seq<System.String>
        member Defer : bool
        member Dependencies : System.String list
        member ExposeReturnCode : bool
        member FileFilter : System.String list
        member InPlace : bool
        member InputDirectories : System.String list
        member Keys : System.String list
        member LineCover : bool
        member LocalSource : bool
        member MethodFilter : System.String list
        member MethodPoint : bool
        member OutputDirectories : System.String list
        member PathFilter : System.String list
        member ReportFormat : string
        member Save : bool
        member ShowGenerated : bool
        member ShowStatic : string
        member Single : bool
        member SourceLink : bool
        member StrongNameKey : System.String
        member SymbolDirectories : System.String list
        member TypeFilter : System.String list
        member VisibleBranches : bool
        member XmlReport : System.String
        member ZipFile : bool
#if RUNNER
        member Validate : unit -> string []
#endif
    end
#if RUNNER
    type LoggingOptions =
      | Primitive of Primitive.LoggingOptions
      with
        member internal Apply : unit -> unit
        member Echo : (System.String -> unit)
        member Error : (System.String -> unit)
        member Info : (System.String -> unit)
        member Warn : (System.String -> unit)
        static member
          ActionAdapter : action:System.Action<System.String> ->
                            (System.String -> unit)
        static member Create : unit -> LoggingOptions
      end
#endif
  end