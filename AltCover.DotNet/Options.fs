#if RUNNER
namespace AltCover
#else
namespace AltCoverFake.DotNet.Testing
#endif

open System
open System.Diagnostics.CodeAnalysis

[<RequireQualifiedAccess>]
module Options =
  [<AutoSerializable(false)>]
  [<SuppressMessage("Gendarme.Rules.Maintainability",
                    "AvoidLackOfCohesionOfMethodsRule",
                    Justification = "It's a property bag")>]
  type CLI() =
    member val ForceDelete = false with get, set
    member val FailFast = false with get, set
    member val ShowSummary = String.Empty with get, set

    interface DotNet.ICLIOptions with
      member self.ForceDelete = self.ForceDelete
      member self.FailFast = self.FailFast
      member self.ShowSummary = self.ShowSummary

  [<AutoSerializable(false)>]
  [<SuppressMessage("Gendarme.Rules.Maintainability",
                    "AvoidLackOfCohesionOfMethodsRule",
                    Justification = "It's a property bag")>]
  type Collect() =
    member val RecorderDirectory = String.Empty with get, set
    member val WorkingDirectory = String.Empty with get, set
    member val Executable = String.Empty with get, set

    [<SuppressMessage("Microsoft.Naming",
                      "CA1704:IdentifiersShouldBeSpelledCorrectly",
                      Justification = "Lcov is a name")>]
    member val LcovReport = String.Empty with get, set

    member val Threshold = String.Empty with get, set

    [<SuppressMessage("Microsoft.Naming",
                      "CA1704:IdentifiersShouldBeSpelledCorrectly",
                      Justification = "Cobertura is a name")>]
    member val Cobertura = String.Empty with get, set

    member val Packages = Seq.empty<String> with get, set

    member val OutputFile = String.Empty with get, set
    member val CommandLine = Seq.empty<String> with get, set
    member val ExposeReturnCode = true with get, set
    member val SummaryFormat = String.Empty with get, set
    member val Verbosity = System.Diagnostics.TraceLevel.Info with get, set

    interface Abstract.ICollectOptions with
      member self.RecorderDirectory =
        self.RecorderDirectory

      member self.WorkingDirectory =
        self.WorkingDirectory

      member self.Executable = self.Executable

      [<SuppressMessage("Microsoft.Naming",
                        "CA1704:IdentifiersShouldBeSpelledCorrectly",
                        Justification = "Lcov is a name")>]
      member self.LcovReport = self.LcovReport

      member self.Threshold = self.Threshold

      [<SuppressMessage("Microsoft.Naming",
                        "CA1704:IdentifiersShouldBeSpelledCorrectly",
                        Justification = "Cobertura is a name")>]
      member self.Cobertura = self.Cobertura

      member self.Packages = self.Packages

      member self.OutputFile = self.OutputFile
      member self.CommandLine = self.CommandLine

      member self.ExposeReturnCode =
        self.ExposeReturnCode

      member self.SummaryFormat = self.SummaryFormat
      member self.Verbosity = self.Verbosity

  [<AutoSerializable(false)>]
  [<SuppressMessage("Gendarme.Rules.Maintainability",
                    "AvoidLackOfCohesionOfMethodsRule",
                    Justification = "It's a property bag")>]
  [<SuppressMessage("Gendarme.Rules.Smells",
                    "AvoidLargeClassesRule",
                    Justification = "It's a property bag")>]
  type Prepare() =
    member val InputDirectories = Seq.empty<String> with get, set
    member val OutputDirectories = Seq.empty<String> with get, set
    member val SymbolDirectories = Seq.empty<String> with get, set
    member val Dependencies = Seq.empty<String> with get, set
    member val Keys = Seq.empty<String> with get, set
    member val StrongNameKey = String.Empty with get, set
    member val Report = String.Empty with get, set
    member val FileFilter = Seq.empty<String> with get, set
    member val AssemblyFilter = Seq.empty<String> with get, set
    member val AssemblyExcludeFilter = Seq.empty<String> with get, set
    member val TypeFilter = Seq.empty<String> with get, set
    member val MethodFilter = Seq.empty<String> with get, set
    member val AttributeFilter = Seq.empty<String> with get, set
    member val PathFilter = Seq.empty<String> with get, set
    member val AttributeTopLevel = Seq.empty<String> with get, set
    member val TypeTopLevel = Seq.empty<String> with get, set
    member val MethodTopLevel = Seq.empty<String> with get, set
    member val CallContext = Seq.empty<String> with get, set
    member val ReportFormat = String.Empty with get, set
    member val InPlace = false with get, set
    member val Save = true with get, set
    member val ZipFile = false with get, set
    member val MethodPoint = false with get, set
    member val SingleVisit = false with get, set
    member val LineCover = false with get, set
    member val BranchCover = false with get, set
    member val CommandLine = Seq.empty<String> with get, set
    member val ExposeReturnCode = true with get, set
    member val SourceLink = false with get, set
    member val Defer = false with get, set
    member val LocalSource = false with get, set
    member val VisibleBranches = false with get, set
    member val ShowStatic = "-" with get, set
    member val ShowGenerated = false with get, set
    member val Verbosity = System.Diagnostics.TraceLevel.Info with get, set
    member val Trivia = false with get, set
    member val OutputRoot = String.Empty with get, set
    member val Portable = false with get, set

    interface Abstract.IPrepareOptions with
      member self.InputDirectories =
        self.InputDirectories

      member self.OutputDirectories =
        self.OutputDirectories

      member self.SymbolDirectories =
        self.SymbolDirectories

      member self.Dependencies = self.Dependencies
      member self.Keys = self.Keys
      member self.StrongNameKey = self.StrongNameKey
      member self.Report = self.Report
      member self.FileFilter = self.FileFilter

      member self.AssemblyFilter =
        self.AssemblyFilter

      member self.AssemblyExcludeFilter =
        self.AssemblyExcludeFilter

      member self.TypeFilter = self.TypeFilter
      member self.MethodFilter = self.MethodFilter

      member self.AttributeFilter =
        self.AttributeFilter

      member self.PathFilter = self.PathFilter

      member self.AttributeTopLevel =
        self.AttributeTopLevel

      member self.TypeTopLevel = self.TypeTopLevel

      member self.MethodTopLevel =
        self.MethodTopLevel

      member self.CallContext = self.CallContext
      member self.ReportFormat = self.ReportFormat
      member self.InPlace = self.InPlace
      member self.Save = self.Save
      member self.ZipFile = self.ZipFile
      member self.MethodPoint = self.MethodPoint
      member self.SingleVisit = self.SingleVisit
      member self.LineCover = self.LineCover
      member self.BranchCover = self.BranchCover
      member self.CommandLine = self.CommandLine

      member self.ExposeReturnCode =
        self.ExposeReturnCode

      member self.SourceLink = self.SourceLink
      member self.Defer = self.Defer
      member self.LocalSource = self.LocalSource

      member self.VisibleBranches =
        self.VisibleBranches

      member self.ShowStatic = self.ShowStatic
      member self.ShowGenerated = self.ShowGenerated
      member self.Verbosity = self.Verbosity
      member self.Trivia = self.Trivia
      member self.OutputRoot = self.OutputRoot
      member self.Portable = self.Portable

#if RUNNER
  [<AutoSerializable(false)>]
  [<SuppressMessage("Gendarme.Rules.Maintainability",
                    "AvoidLackOfCohesionOfMethodsRule",
                    Justification = "It's a property bag")>]
  type Logging() =
    let wl (w: System.IO.TextWriter) (x: string) = w.WriteLine x
    member val Info = Action<string>(wl Console.Out) with get, set
    member val Warn = Action<string>(wl Console.Out) with get, set
    member val Failure = Action<string>(wl Console.Error) with get, set
    member val Echo = Action<string>(wl Console.Out) with get, set
    member val Verbose = Action<string>(wl Console.Out) with get, set

    interface Abstract.ILoggingOptions with
      member self.Info = self.Info
      member self.Warn = self.Warn
      member self.Failure = self.Failure
      member self.Echo = self.Echo
      member self.Verbose = self.Verbose
#endif