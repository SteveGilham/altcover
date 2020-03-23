#if RUNNER
[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage; RequireQualifiedAccess>] // work around coverlet attribute bug
module AltCover.TypeSafe
#else
[<RequireQualifiedAccess>]
module AltCoverFake.DotNet.Testing.TypeSafe
#endif

open System
open System.Diagnostics.CodeAnalysis
open System.Globalization
open System.IO
open System.Text.RegularExpressions

// No more primitive obsession!
[<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
type FilePath =
  | Tool of String
  | FilePath of String
  | FInfo of FileInfo
  | NoFile
  member self.AsString() =
    match self with
    | NoFile -> String.Empty
    | FInfo i -> i.FullName
    | FilePath s -> Path.GetFullPath s
    | Tool t -> t

[<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
type DirectoryPath =
  | DirectoryPath of String
  | DInfo of DirectoryInfo
  | NoDirectory
  member self.AsString() =
    match self with
    | NoDirectory -> String.Empty
    | DInfo i -> i.FullName
    | DirectoryPath s -> s

[<ExcludeFromCodeCoverage; NoComparison>]
type CommandArgument =
  | CommandArgument of String
  member self.AsString() =
    match self with
    | CommandArgument s -> s

[<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
type Command =
  | Command of CommandArgument seq
  | NoCommand
  member self.AsStrings() =
    match self with
    | NoCommand -> Seq.empty<String>
    | Command c -> c |> Seq.map (fun a -> a.AsString())

[<ExcludeFromCodeCoverage; NoComparison>]
type Threshold =
  | Threshold of uint8
  | NoThreshold
  member self.AsString() =
    match self with
    | NoThreshold -> String.Empty
    | Threshold t -> t.ToString(CultureInfo.InvariantCulture)

[<ExcludeFromCodeCoverage; NoComparison>]
type Flag =
  | Flag of bool
  | Set
  | Clear
  [<SuppressMessage("Microsoft.Naming", "CA1720:IdentifiersShouldNotContainTypeNames",
    Justification="like 'ToString()'")>]
  member self.AsBool() =
    match self with
    | Set -> true
    | Clear -> false
    | Flag b -> b

[<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
type FilePaths =
  | FilePaths of FilePath seq
  | NoPaths
  member self.AsStrings() =
    match self with
    | NoPaths -> List.empty<String>
    | FilePaths c ->
        c
        |> Seq.map (fun a -> a.AsString())
        |> Seq.toList

[<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
type DirectoryPaths =
  | DirectoryPaths of DirectoryPath seq
  | NoDirectories
  member self.AsStrings() =
    match self with
    | NoDirectories -> List.empty<String>
    | DirectoryPaths c ->
        c
        |> Seq.map (fun a -> a.AsString())
        |> Seq.toList

[<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
type FilterItem =
  | FilterItem of Regex
  | IncludeItem of Regex
  | Raw of String
  member self.AsString() =
    match self with
    | FilterItem r -> r.ToString()
    | IncludeItem r -> "?" + r.ToString()
    | Raw r -> r

[<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
type Filters =
  | Filters of FilterItem seq
  | Unfiltered
  member self.AsStrings() =
    match self with
    | Unfiltered -> List.empty<String>
    | Filters c ->
        c
        |> Seq.map (fun a -> a.AsString())
        |> Seq.toList

[<ExcludeFromCodeCoverage; NoComparison>]
type ContextItem =
  | CallItem of String
  | TimeItem of uint8
  member self.AsString() =
    match self with
    | CallItem c -> c
    | TimeItem t -> t.ToString(CultureInfo.InvariantCulture)

[<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
type Context =
  | Context of ContextItem seq
  | NoContext
  member self.AsStrings() =
    match self with
    | NoContext -> List.empty<String>
    | Context c ->
        c
        |> Seq.map (fun a -> a.AsString())
        |> Seq.toList

[<ExcludeFromCodeCoverage; NoComparison>]
type SummaryFormat =
  | Default
  | [<SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
    Justification="TeamCity notation")>]
    R
  | [<SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
    Justification="TeamCity notation")>]
    B
  | RPlus
  | BPlus
  member self.AsString() =
    match self with
    | Default -> String.Empty
    | B -> "B"
    | R -> "R"
    | BPlus -> "+B"
    | RPlus -> "+R"

[<ExcludeFromCodeCoverage; NoComparison>]
type StaticFormat =
  | Default
  | Show
  | ShowZero
  member self.AsString() =
    match self with
    | Default -> "-"
    | Show -> "+"
    | ShowZero -> "++"

[<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
type CollectParameters =
  { RecorderDirectory : DirectoryPath
    WorkingDirectory : DirectoryPath
    Executable : FilePath
    [<SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
                      Justification="Lcov is a name")>]
    LcovReport : FilePath
    Threshold : Threshold
    [<SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
                      Justification="Cobertura is a name")>]
    Cobertura : FilePath
    OutputFile : FilePath
    CommandLine : Command
    ExposeReturnCode : Flag
    SummaryFormat : SummaryFormat }
  static member Create() =
    { RecorderDirectory = NoDirectory
      WorkingDirectory = NoDirectory
      Executable = NoFile
      LcovReport = NoFile
      Threshold = NoThreshold
      Cobertura = NoFile
      OutputFile = NoFile
      CommandLine = NoCommand
      ExposeReturnCode = Set
      SummaryFormat = SummaryFormat.Default }

[<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
[<SuppressMessage("Gendarme.Rules.Smells", "AvoidLargeClassesRule",
                  Justification="Plenty of options to support")>]
type PrepareParameters =
  { InputDirectories : DirectoryPaths
    OutputDirectories : DirectoryPaths
    SymbolDirectories : DirectoryPaths
    Dependencies : FilePaths
    Keys : FilePaths
    StrongNameKey : FilePath
    XmlReport : FilePath
    FileFilter : Filters
    AssemblyFilter : Filters
    AssemblyExcludeFilter : Filters
    TypeFilter : Filters
    MethodFilter : Filters
    AttributeFilter : Filters
    PathFilter : Filters
    CallContext : Context
    OpenCover : Flag
    InPlace : Flag
    Save : Flag
    Single : Flag
    LineCover : Flag
    BranchCover : Flag
    CommandLine : Command
    ExposeReturnCode : Flag
    SourceLink : Flag
    Defer : Flag
    LocalSource : Flag
    VisibleBranches : Flag
    ShowStatic : StaticFormat
    ShowGenerated : Flag }
  static member Create() =
    { InputDirectories = NoDirectories
      OutputDirectories = NoDirectories
      SymbolDirectories = NoDirectories
      Dependencies = NoPaths
      Keys = NoPaths
      StrongNameKey = NoFile
      XmlReport = NoFile
      FileFilter = Unfiltered
      AssemblyFilter = Unfiltered
      AssemblyExcludeFilter = Unfiltered
      TypeFilter = Unfiltered
      MethodFilter = Unfiltered
      AttributeFilter = Unfiltered
      PathFilter = Unfiltered
      CallContext = NoContext
      OpenCover = Set
      InPlace = Set
      Save = Set
      Single = Clear
      LineCover = Clear
      BranchCover = Clear
      CommandLine = NoCommand
      ExposeReturnCode = Set
      SourceLink = Clear
      Defer = Clear
      LocalSource = Clear
      VisibleBranches = Clear
      ShowStatic = StaticFormat.Default
      ShowGenerated = Clear }

#if RUNNER
[<assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
  Scope="member", Target="AltCover.TypeSafe+CollectParameters.#.ctor(AltCover.TypeSafe+DirectoryPath,AltCover.TypeSafe+DirectoryPath,AltCover.TypeSafe+FilePath,AltCover.TypeSafe+FilePath,AltCover.TypeSafe+Threshold,AltCover.TypeSafe+FilePath,AltCover.TypeSafe+FilePath,AltCover.TypeSafe+Command,AltCover.TypeSafe+Flag,AltCover.TypeSafe+SummaryFormat)",
  MessageId="cobertura", Justification="Cobertura is a name")>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
  Scope="member", Target="AltCover.TypeSafe+CollectParameters.#.ctor(AltCover.TypeSafe+DirectoryPath,AltCover.TypeSafe+DirectoryPath,AltCover.TypeSafe+FilePath,AltCover.TypeSafe+FilePath,AltCover.TypeSafe+Threshold,AltCover.TypeSafe+FilePath,AltCover.TypeSafe+FilePath,AltCover.TypeSafe+Command,AltCover.TypeSafe+Flag,AltCover.TypeSafe+SummaryFormat)",
  MessageId="lcov", Justification="LCov is a name")>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1726:UsePreferredTerms", Scope="type", Target="AltCover.TypeSafe+Flag", MessageId="Flag", Justification="It's a flag, damn it.")>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1726:UsePreferredTerms", Scope="member", Target="AltCover.TypeSafe+Flag.#NewFlag(System.Boolean)", MessageId="Flag", Justification="It's a flag, damn it.")>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1726:UsePreferredTerms", Scope="type", Target="AltCover.TypeSafe+Flag+Flag", MessageId="Flag", Justification="It's a flag, damn it.")>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1726:UsePreferredTerms", Scope="member", Target="AltCover.TypeSafe+Flag+Tags.#Flag", MessageId="Flag", Justification="It's a flag, damn it.")>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", Scope="member", Target="AltCover.TypeSafe+SummaryFormat+Tags.#B", MessageId="B", Justification="TeamCity notation")>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", Scope="member", Target="AltCover.TypeSafe+SummaryFormat+Tags.#R", MessageId="R", Justification="TeamCity notation")>]
#else
[<assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
  Scope="member", Target="AltCoverFake.DotNet.Testing.TypeSafe+CollectParameters.#.ctor(AltCoverFake.DotNet.Testing.TypeSafe+DirectoryPath,AltCoverFake.DotNet.Testing.TypeSafe+DirectoryPath,AltCoverFake.DotNet.Testing.TypeSafe+FilePath,AltCoverFake.DotNet.Testing.TypeSafe+FilePath,AltCoverFake.DotNet.Testing.TypeSafe+Threshold,AltCoverFake.DotNet.Testing.TypeSafe+FilePath,AltCoverFake.DotNet.Testing.TypeSafe+FilePath,AltCoverFake.DotNet.Testing.TypeSafe+Command,AltCoverFake.DotNet.Testing.TypeSafe+Flag,AltCoverFake.DotNet.Testing.TypeSafe+SummaryFormat)",
  MessageId="cobertura", Justification="Cobertura is a name")>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
  Scope="member", Target="AltCoverFake.DotNet.Testing.TypeSafe+CollectParameters.#.ctor(AltCoverFake.DotNet.Testing.TypeSafe+DirectoryPath,AltCoverFake.DotNet.Testing.TypeSafe+DirectoryPath,AltCoverFake.DotNet.Testing.TypeSafe+FilePath,AltCoverFake.DotNet.Testing.TypeSafe+FilePath,AltCoverFake.DotNet.Testing.TypeSafe+Threshold,AltCoverFake.DotNet.Testing.TypeSafe+FilePath,AltCoverFake.DotNet.Testing.TypeSafe+FilePath,AltCoverFake.DotNet.Testing.TypeSafe+Command,AltCoverFake.DotNet.Testing.TypeSafe+Flag,AltCoverFake.DotNet.Testing.TypeSafe+SummaryFormat)",
  MessageId="lcov", Justification="LCov is a name")>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1726:UsePreferredTerms", Scope="type", Target="AltCoverFake.DotNet.Testing.TypeSafe+Flag", MessageId="Flag", Justification="It's a flag, damn it.")>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1726:UsePreferredTerms", Scope="member", Target="AltCoverFake.DotNet.Testing.TypeSafe+Flag.#NewFlag(System.Boolean)", MessageId="Flag", Justification="It's a flag, damn it.")>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1726:UsePreferredTerms", Scope="type", Target="AltCoverFake.DotNet.Testing.TypeSafe+Flag+Flag", MessageId="Flag", Justification="It's a flag, damn it.")>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1726:UsePreferredTerms", Scope="member", Target="AltCoverFake.DotNet.Testing.TypeSafe+Flag+Tags.#Flag", MessageId="Flag", Justification="It's a flag, damn it.")>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", Scope="member", Target="AltCoverFake.DotNet.Testing.TypeSafe+SummaryFormat+Tags.#B", MessageId="B", Justification="TeamCity notation")>]
[<assembly: SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", Scope="member", Target="AltCoverFake.DotNet.Testing.TypeSafe+SummaryFormat+Tags.#R", MessageId="R", Justification="TeamCity notation")>]
#endif
()