#if RUNNER
[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage; RequireQualifiedAccess>] // work around coverlet attribute bug
module AltCover.Primitive
#else
[<RequireQualifiedAccess>]
module AltCoverFake.DotNet.Testing.Primitive
#endif

open System
open System.Diagnostics.CodeAnalysis

[<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
type CollectParameters =
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
    SummaryFormat : String }
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
      SummaryFormat = String.Empty }

[<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
[<SuppressMessage("Gendarme.Rules.Smells", "AvoidLargeClassesRule",
                  Justification="Plenty of options to support")>]
type PrepareParameters =
  { InputDirectories : String seq
    OutputDirectories : String seq
    SymbolDirectories : String seq
    Dependencies : String seq
    Keys : String seq
    StrongNameKey : String
    XmlReport : String
    FileFilter : String seq
    AssemblyFilter : String seq
    AssemblyExcludeFilter : String seq
    TypeFilter : String seq
    MethodFilter : String seq
    AttributeFilter : String seq
    PathFilter : String seq
    CallContext : String seq
    OpenCover : bool
    InPlace : bool
    Save : bool
    Single : bool
    LineCover : bool
    BranchCover : bool
    CommandLine : String seq
    ExposeReturnCode : bool
    SourceLink : bool
    Defer : bool
    LocalSource : bool
    VisibleBranches : bool
    ShowStatic : string
    ShowGenerated : bool }
  static member Create() =
    { InputDirectories = Seq.empty
      OutputDirectories = Seq.empty
      SymbolDirectories = Seq.empty
      Dependencies = Seq.empty
      Keys = Seq.empty
      StrongNameKey = String.Empty
      XmlReport = String.Empty
      FileFilter = Seq.empty
      AssemblyFilter = Seq.empty
      AssemblyExcludeFilter = Seq.empty
      TypeFilter = Seq.empty
      MethodFilter = Seq.empty
      AttributeFilter = Seq.empty
      PathFilter = Seq.empty
      CallContext = Seq.empty
      OpenCover = true
      InPlace = true
      Save = true
      Single = false
      LineCover = false
      BranchCover = false
      CommandLine = []
      ExposeReturnCode = true
      SourceLink = false
      Defer = false
      LocalSource = false
      VisibleBranches = false
      ShowStatic = "-"
      ShowGenerated = false }

#if RUNNER
[<ExcludeFromCodeCoverage; NoComparison; NoEquality; AutoSerializable(false)>]
type Logging =
  { Info : String -> unit
    Warn : String -> unit
    Error : String -> unit
    Echo : String -> unit }

  static member Create() : Logging =
    { Info = ignore
      Warn = ignore
      Error = ignore
      Echo = ignore }
#endif

#if RUNNER
[<assembly: SuppressMessage("Microsoft.Naming",
  "CA1704:IdentifiersShouldBeSpelledCorrectly", Scope="member",
  Target="AltCover.Primitive+CollectParameters.#.ctor(System.String,System.String,System.String,System.String,System.String,System.String,System.String,System.Collections.Generic.IEnumerable`1<System.String>,System.Boolean,System.String)",
  MessageId="cobertura", Justification="Cobertura is a name")>]
[<assembly: SuppressMessage("Microsoft.Naming",
  "CA1704:IdentifiersShouldBeSpelledCorrectly", Scope="member",
  Target="AltCover.Primitive+CollectParameters.#.ctor(System.String,System.String,System.String,System.String,System.String,System.String,System.String,System.Collections.Generic.IEnumerable`1<System.String>,System.Boolean,System.String)",
  MessageId="lcov", Justification="LCov is a name")>]
#else
[<assembly: SuppressMessage("Microsoft.Naming",
  "CA1704:IdentifiersShouldBeSpelledCorrectly", Scope="member",
  Target="AltCoverFake.DotNet.Testing.Primitive+CollectParameters.#.ctor(System.String,System.String,System.String,System.String,System.String,System.String,System.String,System.Collections.Generic.IEnumerable`1<System.String>,System.Boolean,System.String)",
  MessageId="cobertura", Justification="Cobertura is a name")>]
[<assembly: SuppressMessage("Microsoft.Naming",
  "CA1704:IdentifiersShouldBeSpelledCorrectly", Scope="member",
  Target="AltCoverFake.DotNet.Testing.Primitive+CollectParameters.#.ctor(System.String,System.String,System.String,System.String,System.String,System.String,System.String,System.Collections.Generic.IEnumerable`1<System.String>,System.Boolean,System.String)",
  MessageId="lcov", Justification="LCov is a name")>]
#endif
()