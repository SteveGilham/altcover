#if RUNNER
namespace AltCover
#else
namespace AltCoverFake.DotNet.Testing
#endif

open System
open System.Diagnostics.CodeAnalysis
open System.Linq
#if RUNNER

[<assembly: SuppressMessage("Microsoft.Naming",
                            "CA1724:TypeNamesShouldNotMatchNamespaces",
                            Scope = "type",
                            Target = "AltCover.AltCover",
                            Justification = "Design decision")>]
()

#else
open System.Reflection
open AltCoverFake.DotNet.Testing
open Fake.Core
open Fake.DotNet

[<assembly: SuppressMessage("Microsoft.Naming",
                            "CA1724:TypeNamesShouldNotMatchNamespaces",
                            Scope = "type",
                            Target = "AltCoverFake.DotNet.Testing.AltCover",
                            Justification = "Design decision")>]
()

#endif

[<RequireQualifiedAccess>]
module AltCover =
#if RUNNER
  [<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
  type ValidatedCommandLine =
    { Command: string list
      Errors: string seq }
    override self.ToString() =
      let cl =
        String.Join(" ", Seq.concat [ [ "altcover" ]; self.Command ])

      String.Join(
        Environment.NewLine,
        Seq.concat [ [| cl |] |> Array.toSeq; self.Errors ]
      )

  let validateArraySimple a f = a |> Seq.iter (f >> ignore)

  let validateArray a f key = validateArraySimple a (f key)

  let validateOptional f key x =
    if x |> String.IsNullOrWhiteSpace |> not then
      f key x |> ignore

#endif

  let toSeq (s: 'a seq) =
    match s with
    | null -> Seq.empty<'a>
    | _ -> s

  let toList (s: 'a seq) = s |> toSeq |> Seq.toList

  [<ExcludeFromCodeCoverage;
    NoComparison;
    AutoSerializable(false);
    SuppressMessage("Gendarme.Rules.Smells",
                    "RelaxedAvoidCodeDuplicatedInSameClassRule",
                    Justification = "Idiomatic F#")>]
  type CollectOptions =
    | Primitive of Primitive.CollectOptions
    | TypeSafe of TypeSafe.CollectOptions
    | Abstract of Abstract.ICollectOptions

    member self.RecorderDirectory =
      match self with
      | Primitive p -> p.RecorderDirectory
      | Abstract a -> a.RecorderDirectory
      | TypeSafe t -> t.RecorderDirectory.AsString()

    member self.WorkingDirectory =
      match self with
      | Primitive p -> p.WorkingDirectory
      | Abstract a -> a.WorkingDirectory
      | TypeSafe t -> t.WorkingDirectory.AsString()

    member self.Executable =
      match self with
      | Primitive p -> p.Executable
      | Abstract a -> a.Executable
      | TypeSafe t -> t.Executable.AsString()

    [<SuppressMessage("Microsoft.Naming", "CA1704", Justification = "'Lcov' is jargon")>]
    member self.LcovReport =
      match self with
      | Primitive p -> p.LcovReport
      | Abstract a -> a.LcovReport
      | TypeSafe t -> t.LcovReport.AsString()

    member self.Threshold =
      match self with
      | Primitive p -> p.Threshold
      | Abstract a -> a.Threshold
      | TypeSafe t -> t.Threshold.AsString()

    [<SuppressMessage("Microsoft.Naming",
                      "CA1704",
                      Justification = "'Cobertura' is jargon")>]
    member self.Cobertura =
      match self with
      | Primitive p -> p.Cobertura
      | Abstract a -> a.Cobertura
      | TypeSafe t -> t.Cobertura.AsString()

    member self.OutputFile =
      match self with
      | Primitive p -> p.OutputFile
      | Abstract a -> a.OutputFile
      | TypeSafe t -> t.OutputFile.AsString()

    member self.CommandLine =
      match self with
      | Primitive p -> p.CommandLine |> toSeq
      | Abstract a -> a.CommandLine |> toSeq
      | TypeSafe t -> t.CommandLine.AsStrings()

    member self.ExposeReturnCode =
      match self with
      | Primitive p -> p.ExposeReturnCode
      | Abstract a -> a.ExposeReturnCode
      | TypeSafe t -> t.ExposeReturnCode.AsBool()

    member self.SummaryFormat =
      match self with
      | Primitive p -> p.SummaryFormat
      | Abstract a -> a.SummaryFormat
      | TypeSafe t -> t.SummaryFormat.AsString()

    member self.Verbosity =
      match self with
      | Primitive p -> p.Verbosity
      | Abstract a -> a.Verbosity
      | TypeSafe t -> t.Verbosity

    interface Abstract.ICollectOptions with
      member self.RecorderDirectory =
        self.RecorderDirectory

      member self.WorkingDirectory =
        self.WorkingDirectory

      member self.Executable = self.Executable
      member self.LcovReport = self.LcovReport
      member self.Threshold = self.Threshold
      member self.Cobertura = self.Cobertura
      member self.OutputFile = self.OutputFile
      member self.CommandLine = self.CommandLine

      member self.ExposeReturnCode =
        self.ExposeReturnCode

      member self.SummaryFormat = self.SummaryFormat
      member self.Verbosity = self.Verbosity

#if RUNNER
    member self.Validate afterPreparation =
      let saved = CommandLine.error

      let validate f x =
        if x |> String.IsNullOrWhiteSpace |> not then
          f x |> ignore

      let validateOptional f key x = validate (f key) x

      let toOption s =
        if s |> String.IsNullOrWhiteSpace then
          None
        else
          Some s

      try
        let recorder = self.RecorderDirectory

        [ ("--recorderDirectory", recorder)
          ("--workingDirectory", self.WorkingDirectory) ]
        |> List.iter (fun (n, x) -> validateOptional CommandLine.validateDirectory n x)

        [ ("--executable", self.Executable)
          ("--lcovReport", self.LcovReport)
          ("--cobertura", self.Cobertura)
          ("--outputFile", self.OutputFile) ]
        |> List.iter (fun (n, x) -> validateOptional CommandLine.validatePath n x)

        validate Threshold.Validate self.Threshold

        if afterPreparation then
          Runner.requireRecorderTest (recorder |> toOption) () ()

        CommandLine.error |> List.toArray
      finally
        CommandLine.error <- saved
#else
#endif

  [<ExcludeFromCodeCoverage;
    NoComparison;
    AutoSerializable(false);
    SuppressMessage("Gendarme.Rules.Smells",
                    "RelaxedAvoidCodeDuplicatedInSameClassRule",
                    Justification = "Idiomatic F#")>]
  type PrepareOptions =
    | Primitive of Primitive.PrepareOptions
    | TypeSafe of TypeSafe.PrepareOptions
    | Abstract of Abstract.IPrepareOptions

    member self.InputDirectories =
      match self with
      | Primitive p -> p.InputDirectories |> toList
      | Abstract a -> a.InputDirectories |> toList
      | TypeSafe t -> t.InputDirectories.AsStrings()

    member self.OutputDirectories =
      match self with
      | Primitive p -> p.OutputDirectories |> toList
      | Abstract a -> a.OutputDirectories |> toList
      | TypeSafe t -> t.OutputDirectories.AsStrings()

    member self.SymbolDirectories =
      match self with
      | Primitive p -> p.SymbolDirectories |> toList
      | Abstract a -> a.SymbolDirectories |> toList
      | TypeSafe t -> t.SymbolDirectories.AsStrings()

    member self.Dependencies =
      match self with
      | Primitive p -> p.Dependencies |> toList
      | Abstract a -> a.Dependencies |> toList
      | TypeSafe t -> t.Dependencies.AsStrings()

    member self.Keys =
      match self with
      | Primitive p -> p.Keys |> toList
      | Abstract a -> a.Keys |> toList
      | TypeSafe t -> t.Keys.AsStrings()

    member self.StrongNameKey =
      match self with
      | Primitive p -> p.StrongNameKey
      | Abstract a -> a.StrongNameKey
      | TypeSafe t -> t.StrongNameKey.AsString()

    member self.Report =
      match self with
      | Primitive p -> p.Report
      | Abstract a -> a.Report
      | TypeSafe t -> t.Report.AsString()

    member self.FileFilter =
      match self with
      | Primitive p -> p.FileFilter |> toList
      | Abstract a -> a.FileFilter |> toList
      | TypeSafe t -> t.FileFilter.AsStrings()

    member self.AssemblyFilter =
      match self with
      | Primitive p -> p.AssemblyFilter |> toList
      | Abstract a -> a.AssemblyFilter |> toList
      | TypeSafe t -> t.AssemblyFilter.AsStrings()

    member self.AssemblyExcludeFilter =
      match self with
      | Primitive p -> p.AssemblyExcludeFilter |> toList
      | Abstract a -> a.AssemblyExcludeFilter |> toList
      | TypeSafe t -> t.AssemblyExcludeFilter.AsStrings()

    member self.TypeFilter =
      match self with
      | Primitive p -> p.TypeFilter |> toList
      | Abstract a -> a.TypeFilter |> toList
      | TypeSafe t -> t.TypeFilter.AsStrings()

    member self.MethodFilter =
      match self with
      | Primitive p -> p.MethodFilter |> toList
      | Abstract a -> a.MethodFilter |> toList
      | TypeSafe t -> t.MethodFilter.AsStrings()

    member self.AttributeFilter =
      match self with
      | Primitive p -> p.AttributeFilter |> toList
      | Abstract a -> a.AttributeFilter |> toList
      | TypeSafe t -> t.AttributeFilter.AsStrings()

    member self.PathFilter =
      match self with
      | Primitive p -> p.PathFilter |> toList
      | Abstract a -> a.PathFilter |> toList
      | TypeSafe t -> t.PathFilter.AsStrings()

    member self.AttributeTopLevel =
      match self with
      | Primitive p -> p.AttributeTopLevel |> toList
      | Abstract a -> a.AttributeTopLevel |> toList
      | TypeSafe t -> t.AttributeTopLevel.AsStrings()

    member self.TypeTopLevel =
      match self with
      | Primitive p -> p.TypeTopLevel |> toList
      | Abstract a -> a.TypeTopLevel |> toList
      | TypeSafe t -> t.TypeTopLevel.AsStrings()

    member self.MethodTopLevel =
      match self with
      | Primitive p -> p.MethodTopLevel |> toList
      | Abstract a -> a.MethodTopLevel |> toList
      | TypeSafe t -> t.MethodTopLevel.AsStrings()

    member self.CallContext =
      match self with
      | Primitive p -> p.CallContext |> toList
      | Abstract a -> a.CallContext |> toList
      | TypeSafe t -> t.CallContext.AsStrings()

    member self.ReportFormat =
      let simple =
        match self with
        | Primitive p -> p.ReportFormat
        | Abstract a -> a.ReportFormat
        | TypeSafe t -> t.ReportFormat.AsString()

      if String.IsNullOrWhiteSpace simple then
        "OpenCover"
      else
        simple

    member self.InPlace =
      match self with
      | Primitive p -> p.InPlace
      | Abstract a -> a.InPlace
      | TypeSafe t -> t.InPlace.AsBool()

    member self.Save =
      match self with
      | Primitive p -> p.Save
      | Abstract a -> a.Save
      | TypeSafe t -> t.Save.AsBool()

    member self.ZipFile =
      match self with
      | Primitive p -> p.ZipFile
      | Abstract a -> a.ZipFile
      | TypeSafe t -> t.ZipFile.AsBool()

    member self.MethodPoint =
      match self with
      | Primitive p -> p.MethodPoint
      | Abstract a -> a.MethodPoint
      | TypeSafe t -> t.MethodPoint.AsBool()

    member self.All =
      match self with
      | Primitive p -> p.All
      | Abstract a -> a.All
      | TypeSafe t -> t.All.AsBool()

    member self.LineCover =
      match self with
      | Primitive p -> p.LineCover
      | Abstract a -> a.LineCover
      | TypeSafe t -> t.LineCover.AsBool()

    member self.BranchCover =
      match self with
      | Primitive p -> p.BranchCover
      | Abstract a -> a.BranchCover
      | TypeSafe t -> t.BranchCover.AsBool()

    member self.CommandLine =
      match self with
      | Primitive p -> p.CommandLine |> toSeq
      | Abstract a -> a.CommandLine |> toSeq
      | TypeSafe t -> t.CommandLine.AsStrings()

    member self.ExposeReturnCode =
      match self with
      | Primitive p -> p.ExposeReturnCode
      | Abstract a -> a.ExposeReturnCode
      | TypeSafe t -> t.ExposeReturnCode.AsBool()

    member self.SourceLink =
      match self with
      | Primitive p -> p.SourceLink
      | Abstract a -> a.SourceLink
      | TypeSafe t -> t.SourceLink.AsBool()

    member self.Eager =
      match self with
      | Primitive p -> p.Eager
      | Abstract a -> a.Eager
      | TypeSafe t -> t.Eager.AsBool()

    member self.LocalSource =
      match self with
      | Primitive p -> p.LocalSource
      | Abstract a -> a.LocalSource
      | TypeSafe t -> t.LocalSource.AsBool()

    member self.VisibleBranches =
      match self with
      | Primitive p -> p.VisibleBranches
      | Abstract a -> a.VisibleBranches
      | TypeSafe t -> t.VisibleBranches.AsBool()

    member self.ShowStatic =
      match self with
      | Primitive p -> p.ShowStatic
      | Abstract a -> a.ShowStatic
      | TypeSafe t -> t.ShowStatic.AsString()

    member self.ShowGenerated =
      match self with
      | Primitive p -> p.ShowGenerated
      | Abstract a -> a.ShowGenerated
      | TypeSafe t -> t.ShowGenerated.AsBool()

    member self.Verbosity =
      match self with
      | Primitive p -> p.Verbosity
      | Abstract a -> a.Verbosity
      | TypeSafe t -> t.Verbosity

    member self.Trivia =
      match self with
      | Primitive p -> p.Trivia
      | Abstract a -> a.Trivia
      | TypeSafe t -> t.Trivia.AsBool()

    member self.OutputRoot =
      match self with
      | Primitive p -> p.OutputRoot
      | Abstract a -> a.OutputRoot
      | TypeSafe t -> t.OutputRoot.AsString()

    member self.Portable =
      match self with
      | Primitive p -> p.Portable
      | Abstract a -> a.Portable
      | TypeSafe t -> t.Portable.AsBool()

    interface Abstract.IPrepareOptions with
      member self.InputDirectories =
        self.InputDirectories |> toSeq

      member self.OutputDirectories =
        self.OutputDirectories |> toSeq

      member self.SymbolDirectories =
        self.SymbolDirectories |> toSeq

      member self.Dependencies =
        self.Dependencies |> toSeq

      member self.Keys = self.Keys |> toSeq

      member self.StrongNameKey = self.StrongNameKey
      member self.Report = self.Report

      member self.FileFilter =
        self.FileFilter |> toSeq

      member self.AssemblyFilter =
        self.AssemblyFilter |> toSeq

      member self.AssemblyExcludeFilter =
        self.AssemblyExcludeFilter |> toSeq

      member self.TypeFilter =
        self.TypeFilter |> toSeq

      member self.MethodFilter =
        self.MethodFilter |> toSeq

      member self.AttributeFilter =
        self.AttributeFilter |> toSeq

      member self.PathFilter =
        self.PathFilter |> toSeq

      member self.AttributeTopLevel =
        self.AttributeTopLevel |> toSeq

      member self.TypeTopLevel =
        self.TypeTopLevel |> toSeq

      member self.MethodTopLevel =
        self.MethodTopLevel |> toSeq

      member self.CallContext =
        self.CallContext |> toSeq

      member self.ReportFormat = self.ReportFormat
      member self.InPlace = self.InPlace
      member self.Save = self.Save
      member self.ZipFile = self.ZipFile
      member self.MethodPoint = self.MethodPoint
      member self.All = self.All
      member self.LineCover = self.LineCover
      member self.BranchCover = self.BranchCover

      member self.CommandLine =
        self.CommandLine |> toSeq

      member self.ExposeReturnCode =
        self.ExposeReturnCode

      member self.SourceLink = self.SourceLink
      member self.Eager = self.Eager
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

    member private self.Consistent() =
      if self.LineCover && self.BranchCover then
        CommandLine.error <-
          String.Format(
            System.Globalization.CultureInfo.CurrentCulture,
            Output.resources.GetString "Incompatible",
            "--branchcover",
            "--linecover"
          )
          :: CommandLine.error

    member self.Validate() =
      let saved = CommandLine.error

      let validateContext context =
        let select state x =
          let (_, n) =
            Main.validateCallContext state x

          match (state, n) with
          | (true, _)
          | (_, Left(Some _)) -> true
          | _ -> false

        context
        |> toSeq
        |> Seq.fold select false
        |> ignore

      try
        CommandLine.error <- []

        validateArray
          self.InputDirectories
          CommandLine.validateDirectory
          "--inputDirectory"

        validateArray self.OutputDirectories CommandLine.validatePath "--outputDirectory"

        validateOptional
          CommandLine.validateStrongNameKey
          "--strongNameKey"
          self.StrongNameKey

        validateOptional CommandLine.validatePath "--report" self.Report

        validateArray
          self.SymbolDirectories
          CommandLine.validateDirectory
          "--symbolDirectory"

        validateArray self.Dependencies CommandLine.validateAssembly "--dependency"

        validateArray self.Keys CommandLine.validateStrongNameKey "--key"

        [ self.FileFilter
          self.AssemblyFilter
          self.AssemblyExcludeFilter
          self.TypeFilter
          self.MethodFilter
          self.AttributeFilter
          self.PathFilter ]
        |> Seq.iter (fun a -> validateArraySimple a CommandLine.validateRegexes)

        self.Consistent()
        validateContext self.CallContext
        CommandLine.error |> List.toArray
      finally
        CommandLine.error <- saved

  [<ExcludeFromCodeCoverage;
    NoComparison;
    NoEquality;
    AutoSerializable(false);
    SuppressMessage("Gendarme.Rules.Smells",
                    "RelaxedAvoidCodeDuplicatedInSameClassRule",
                    Justification = "Idiomatic F#")>]
  type LoggingOptions =
    | Primitive of Primitive.LoggingOptions
    | Abstract of Abstract.ILoggingOptions

    static member Create() =
      Primitive.LoggingOptions.Create() |> Primitive

    static member Translate(options: Abstract.ILoggingOptions) =
      let tmp: Primitive.LoggingOptions =
        { Failure = options.Failure |> LoggingOptions.ActionAdapter
          Warn = options.Warn |> LoggingOptions.ActionAdapter
          Echo = options.Echo |> LoggingOptions.ActionAdapter
          Info = options.Info |> LoggingOptions.ActionAdapter
          Verbose = options.Verbose |> LoggingOptions.ActionAdapter }

      tmp |> Primitive

    static member ActionAdapter(action: Action<String>) =
      match action with
      | null -> ignore
      | _ -> action.Invoke

    member self.Error =
      match self with
      | Primitive p -> p.Failure
      | Abstract a -> a.Failure |> LoggingOptions.ActionAdapter

    member self.Warn =
      match self with
      | Primitive p -> p.Warn
      | Abstract a -> a.Warn |> LoggingOptions.ActionAdapter

    member self.Echo =
      match self with
      | Primitive p -> p.Echo
      | Abstract a -> a.Echo |> LoggingOptions.ActionAdapter

    member self.Info =
      match self with
      | Primitive p -> p.Info
      | Abstract a -> a.Info |> LoggingOptions.ActionAdapter

    member self.Verbose =
      match self with
      | Primitive p -> p.Verbose
      | Abstract a -> a.Verbose |> LoggingOptions.ActionAdapter
#endif