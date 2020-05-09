#if RUNNER
namespace AltCover
#else
namespace AltCoverFake.DotNet.Testing
#endif

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis

module Options =
  let internal toSeq(s : String seq) =
    match s with
    | null -> Seq.empty<string>
    | _ -> s |> Seq.toList |> List.toSeq

  [<AutoSerializable(false)>]
  type CollectOptions() =
    member val RecorderDirectory : String = String.Empty with get, set
    member val WorkingDirectory : String = String.Empty with get, set
    member val Executable : String = String.Empty with get, set
    [<SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
                      Justification="Lcov is a name")>]
    member val LcovReport : String = String.Empty with get, set
    member val Threshold : String = String.Empty with get, set
    [<SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly",
                      Justification="Cobertura is a name")>]
    member val Cobertura : String = String.Empty with get, set
    member val OutputFile : String = String.Empty with get, set
    member val CommandLine : IEnumerable<String> = Seq.empty with get, set
    member val ExposeReturnCode : bool = true with get, set
    member val SummaryFormat : String = String.Empty with get, set

    static member Copy (source:Abstract.ICollectOptions) =
      let result = CollectOptions()
      result.RecorderDirectory <- source.RecorderDirectory
      result.WorkingDirectory <- source.WorkingDirectory
      result.Executable <- source.Executable
      result.LcovReport <- source.LcovReport
      result.Threshold <- source.Threshold
      result.Cobertura <- source.Cobertura
      result.OutputFile <- source.OutputFile
      result.CommandLine <- source.CommandLine |> toSeq
      result.ExposeReturnCode <- source.ExposeReturnCode
      result.SummaryFormat <- source.SummaryFormat
      result

    interface Abstract.ICollectOptions with
      member self.RecorderDirectory = self.RecorderDirectory
      member self.WorkingDirectory = self.WorkingDirectory
      member self.Executable = self.Executable
      member self.LcovReport = self.LcovReport
      member self.Threshold = self.Threshold
      member self.Cobertura = self.Cobertura
      member self.OutputFile = self.OutputFile
      member self.CommandLine = self.CommandLine
      member self.ExposeReturnCode = self.ExposeReturnCode
      member self.SummaryFormat = self.SummaryFormat

  [<SuppressMessage("Gendarme.Rules.Smells", "AvoidLargeClassesRule",
                    Justification="Plenty of options to support")>]
  [<AutoSerializable(false)>]
  type PrepareOptions() =
    member val InputDirectories : IEnumerable<String> = Seq.empty with get, set
    member val OutputDirectories : IEnumerable<String> = Seq.empty with get, set
    member val SymbolDirectories : IEnumerable<String> = Seq.empty with get, set
    member val Dependencies : IEnumerable<String> = Seq.empty with get, set
    member val Keys : IEnumerable<String> = Seq.empty with get, set
    member val StrongNameKey : String = String.Empty with get, set
    member val XmlReport : String = String.Empty with get, set
    member val FileFilter : IEnumerable<String> = Seq.empty with get, set
    member val AssemblyFilter : IEnumerable<String> = Seq.empty with get, set
    member val AssemblyExcludeFilter : IEnumerable<String> = Seq.empty with get, set
    member val TypeFilter : IEnumerable<String> = Seq.empty with get, set
    member val MethodFilter : IEnumerable<String> = Seq.empty with get, set
    member val AttributeFilter : IEnumerable<String> = Seq.empty with get, set
    member val PathFilter : IEnumerable<String> = Seq.empty with get, set
    member val CallContext : IEnumerable<String> = Seq.empty with get, set
    member val ReportFormat : String = String.Empty with get, set
    member val InPlace : bool = true with get, set
    member val Save : bool = true with get, set
    member val ZipFile : bool = false with get, set
    member val MethodPoint : bool = false with get, set
    member val SingleVisit : bool = false with get, set
    member val LineCover : bool = false with get, set
    member val BranchCover : bool = false with get, set
    member val CommandLine : IEnumerable<String> = Seq.empty with get, set
    member val ExposeReturnCode : bool = true with get, set
    member val SourceLink : bool = false with get, set
    member val Defer : bool = false with get, set
    member val LocalSource : bool = false with get, set
    member val VisibleBranches : bool = false with get, set
    member val ShowStatic : string = "-" with get, set
    member val ShowGenerated : bool = false with get, set

    static member Copy (source:Abstract.IPrepareOptions) =
      let result = PrepareOptions()
      result.InputDirectories <- source.InputDirectories
      result.OutputDirectories <- source.OutputDirectories
      result.SymbolDirectories <- source.SymbolDirectories
      result.Dependencies <- source.Dependencies
      result.Keys <- source.Keys
      result.StrongNameKey <- source.StrongNameKey
      result.XmlReport <- source.XmlReport
      result.FileFilter <- source.FileFilter
      result.AssemblyFilter <- source.AssemblyFilter
      result.AssemblyExcludeFilter <- source.AssemblyExcludeFilter
      result.TypeFilter <- source.TypeFilter
      result.MethodFilter <- source.MethodFilter
      result.AttributeFilter <- source.AttributeFilter
      result.PathFilter <- source.PathFilter
      result.CallContext <- source.CallContext
      result.ReportFormat <- source.ReportFormat
      result.InPlace <- source.InPlace
      result.Save <- source.Save
      result.ZipFile <- source.ZipFile
      result.MethodPoint <- source.MethodPoint
      result.SingleVisit <- source.SingleVisit
      result.LineCover <- source.LineCover
      result.BranchCover <- source.BranchCover
      result.CommandLine <- source.CommandLine
      result.ExposeReturnCode <- source.ExposeReturnCode
      result.SourceLink <- source.SourceLink
      result.Defer <- source.Defer
      result.LocalSource <- source.LocalSource
      result.VisibleBranches <- source.VisibleBranches
      result.ShowStatic <- source.ShowStatic
      result.ShowGenerated <- source.ShowGenerated
      result

    interface Abstract.IPrepareOptions with
      member self.InputDirectories = self.InputDirectories |> toSeq
      member self.OutputDirectories = self.OutputDirectories |> toSeq
      member self.SymbolDirectories = self.SymbolDirectories |> toSeq
      member self.Dependencies = self.Dependencies |> toSeq
      member self.Keys = self.Keys |> toSeq
      member self.StrongNameKey = self.StrongNameKey
      member self.XmlReport = self.XmlReport
      member self.FileFilter = self.FileFilter |> toSeq
      member self.AssemblyFilter = self.AssemblyFilter |> toSeq
      member self.AssemblyExcludeFilter = self.AssemblyExcludeFilter |> toSeq
      member self.TypeFilter = self.TypeFilter |> toSeq
      member self.MethodFilter = self.MethodFilter |> toSeq
      member self.AttributeFilter = self.AttributeFilter |> toSeq
      member self.PathFilter = self.PathFilter |> toSeq
      member self.CallContext = self.CallContext |> toSeq
      member self.ReportFormat = self.ReportFormat
      member self.InPlace = self.InPlace
      member self.Save = self.Save
      member self.ZipFile = self.ZipFile
      member self.MethodPoint = self.MethodPoint
      member self.SingleVisit = self.SingleVisit
      member self.LineCover = self.LineCover
      member self.BranchCover = self.BranchCover
      member self.CommandLine = self.CommandLine |> toSeq
      member self.ExposeReturnCode = self.ExposeReturnCode
      member self.SourceLink = self.SourceLink
      member self.Defer = self.Defer
      member self.LocalSource = self.LocalSource
      member self.VisibleBranches = self.VisibleBranches
      member self.ShowStatic = self.ShowStatic
      member self.ShowGenerated = self.ShowGenerated

#if RUNNER
  [<AutoSerializable(false)>]
  type LoggingOptions() =
    member val Info : Action<String> = null with get, set
    member val Warn : Action<String> = null with get, set
    member val Failure : Action<String> = null with get, set
    member val Echo : Action<String> = null with get, set

    //static member Copy (source:Abstract.ILoggingOptions) =
    //  let result = LoggingOptions()
    //  result.Info <- source.Info
    //  result.Warn <- source.Warn
    //  result.Failure <- source.Failure
    //  result.Echo <- source.Echo
    //  result

    interface Abstract.ILoggingOptions with
      member self.Info = self.Info
      member self.Warn = self.Warn
      member self.Failure = self.Failure
      member self.Echo = self.Echo

#endif