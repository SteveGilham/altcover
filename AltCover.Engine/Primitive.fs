#if RUNNER
namespace AltCover
#else
namespace AltCoverFake.DotNet.Testing
#endif

open System
open System.Diagnostics.CodeAnalysis

#if RUNNER
[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage; RequireQualifiedAccess>] // work around coverlet attribute bug
#else
[<RequireQualifiedAccess>]
#endif
module Primitive =

  [<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
  [<SuppressMessage("Gendarme.Rules.Design", "ConsiderAddingInterfaceRule",
    Justification="Not worth the added knock-on complexity in F#")>]
  type CollectOptions =
    { RecorderDirectory : String
      WorkingDirectory : String
      Executable : String
      [<SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
                        Justification="Lcov is a name")>]
      LcovReport : String
      Threshold : String
      [<SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
                        Justification="Cobertura is a name")>]
      Cobertura : String
      OutputFile : String
      CommandLine : String seq
      ExposeReturnCode : bool
      SummaryFormat : String
      Verbosity : System.Diagnostics.TraceLevel }
    static member Create() =
      { RecorderDirectory = String.Empty
        WorkingDirectory = String.Empty
        Executable = String.Empty
        LcovReport = String.Empty
        Threshold = String.Empty
        Cobertura = String.Empty
        OutputFile = String.Empty
        CommandLine = []
        ExposeReturnCode = true
        SummaryFormat = String.Empty
        Verbosity = System.Diagnostics.TraceLevel.Info}

  [<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
  [<SuppressMessage("Gendarme.Rules.Smells", "AvoidLargeClassesRule",
                    Justification="Plenty of options to support")>]
  [<SuppressMessage("Gendarme.Rules.Design", "ConsiderAddingInterfaceRule",
    Justification="Not worth the added knock-on complexity in F#")>]
  type PrepareOptions =
    { InputDirectories : String seq
      OutputDirectories : String seq
      SymbolDirectories : String seq
      Dependencies : String seq
      Keys : String seq
      StrongNameKey : String
      Report : String
      FileFilter : String seq
      AssemblyFilter : String seq
      AssemblyExcludeFilter : String seq
      TypeFilter : String seq
      MethodFilter : String seq
      AttributeFilter : String seq
      PathFilter : String seq
      AttributeTopLevel: String seq
      TypeTopLevel: String seq
      MethodTopLevel: String seq
      CallContext : String seq
      ReportFormat : String
      InPlace : bool
      Save : bool
      ZipFile : bool
      MethodPoint : bool
      SingleVisit : bool
      LineCover : bool
      BranchCover : bool
      CommandLine : String seq
      ExposeReturnCode : bool
      SourceLink : bool
      Defer : bool
      LocalSource : bool
      VisibleBranches : bool
      ShowStatic : string
      ShowGenerated : bool
      Verbosity : System.Diagnostics.TraceLevel }
    static member Create() =
      { InputDirectories = Seq.empty
        OutputDirectories = Seq.empty
        SymbolDirectories = Seq.empty
        Dependencies = Seq.empty
        Keys = Seq.empty
        StrongNameKey = String.Empty
        Report = String.Empty
        FileFilter = Seq.empty
        AssemblyFilter = Seq.empty
        AssemblyExcludeFilter = Seq.empty
        TypeFilter = Seq.empty
        MethodFilter = Seq.empty
        AttributeFilter = Seq.empty
        PathFilter = Seq.empty
        AttributeTopLevel = Seq.empty
        TypeTopLevel = Seq.empty
        MethodTopLevel = Seq.empty
        CallContext = Seq.empty
        ReportFormat = String.Empty
        InPlace = false
        Save = true
        ZipFile = false
        MethodPoint = false
        SingleVisit = false
        LineCover = false
        BranchCover = false
        CommandLine = []
        ExposeReturnCode = true
        SourceLink = false
        Defer = false
        LocalSource = false
        VisibleBranches = false
        ShowStatic = "-"
        ShowGenerated = false
        Verbosity = System.Diagnostics.TraceLevel.Info }

#if RUNNER
  [<ExcludeFromCodeCoverage; NoComparison; NoEquality; AutoSerializable(false)>]
  type LoggingOptions =
    { Info : String -> unit
      Warn : String -> unit
      Failure : String -> unit
      Echo : String -> unit }

    static member Create() : LoggingOptions =
      { Info = ignore
        Warn = ignore
        Failure = ignore
        Echo = ignore }
#endif

#if RUNNER
[<assembly: SuppressMessage("Microsoft.Naming",
  "CA1704:IdentifiersShouldBeSpelledCorrectly", Scope="member",
  Target="AltCover.Primitive+CollectOptions.#.ctor(System.String,System.String,System.String,System.String,System.String,System.String,System.String,System.Collections.Generic.IEnumerable`1<System.String>,System.Boolean,System.String,System.Diagnostics.TraceLevel,System.String)",
  MessageId="cobertura", Justification="Cobertura is a name")>]
[<assembly: SuppressMessage("Microsoft.Naming",
  "CA1704:IdentifiersShouldBeSpelledCorrectly", Scope="member",
  Target="AltCover.Primitive+CollectOptions.#.ctor(System.String,System.String,System.String,System.String,System.String,System.String,System.String,System.Collections.Generic.IEnumerable`1<System.String>,System.Boolean,System.String,System.Diagnostics.TraceLevel,System.String)",
  MessageId="lcov", Justification="LCov is a name")>]
[<assembly: SuppressMessage("Microsoft.Naming",
  "CA1704:IdentifiersShouldBeSpelledCorrectly", Scope="member",
  Target="AltCover.Primitive+CollectOptions.#.ctor(System.String,System.String,System.String,System.String,System.String,System.String,System.String,System.Collections.Generic.IEnumerable`1<System.String>,System.Boolean,System.String,System.Diagnostics.TraceLevel,System.String)",
  MessageId="json", Justification="Json is a name")>]
#else
[<assembly: SuppressMessage("Microsoft.Naming",
  "CA1704:IdentifiersShouldBeSpelledCorrectly", Scope="member",
  Target="AltCoverFake.DotNet.Testing.Primitive+CollectOptions.#.ctor(System.String,System.String,System.String,System.String,System.String,System.String,System.String,System.Collections.Generic.IEnumerable`1<System.String>,System.Boolean,System.String,System.Diagnostics.TraceLevel,System.String)",
  MessageId="cobertura", Justification="Cobertura is a name")>]
[<assembly: SuppressMessage("Microsoft.Naming",
  "CA1704:IdentifiersShouldBeSpelledCorrectly", Scope="member",
  Target="AltCoverFake.DotNet.Testing.Primitive+CollectOptions.#.ctor(System.String,System.String,System.String,System.String,System.String,System.String,System.String,System.Collections.Generic.IEnumerable`1<System.String>,System.Boolean,System.String,System.Diagnostics.TraceLevel,System.String)",
  MessageId="lcov", Justification="LCov is a name")>]
[<assembly: SuppressMessage("Microsoft.Naming",
  "CA1704:IdentifiersShouldBeSpelledCorrectly", Scope="member",
  Target="AltCoverFake.DotNet.Testing.Primitive+CollectOptions.#.ctor(System.String,System.String,System.String,System.String,System.String,System.String,System.String,System.Collections.Generic.IEnumerable`1<System.String>,System.Boolean,System.String,System.Diagnostics.TraceLevel,System.String)",
  MessageId="json", Justification="Json is a name")>]
#endif
()