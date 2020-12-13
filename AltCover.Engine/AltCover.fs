#if RUNNER
namespace AltCover
#else
namespace AltCoverFake.DotNet.Testing
#endif

open System
open System.Diagnostics.CodeAnalysis
open System.Linq
#if RUNNER

[<assembly: SuppressMessage("Microsoft.Naming", "CA1724:TypeNamesShouldNotMatchNamespaces",
  Scope="type", Target="AltCover.AltCover", Justification="Design decision")>]
()

#else
open System.Reflection
open AltCoverFake.DotNet.Testing
open Fake.Core
open Fake.DotNet
#endif

[<RequireQualifiedAccess>]
module AltCover =
#if RUNNER
  [<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
  type ValidatedCommandLine =
    { Command : string list
      Errors : string seq }
    override self.ToString() =
      let cl =
        String.Join
          (" ",
           Seq.concat
             [ [ "altcover" ]
               self.Command ])
      String.Join
        (Environment.NewLine,
         Seq.concat
           [ [| cl |] |> Array.toSeq
             self.Errors ])
#endif

  [<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false);
                    SuppressMessage("Gendarme.Rules.Smells",
                                    "RelaxedAvoidCodeDuplicatedInSameClassRule",
                                    Justification = "Idiomatic F#")>]
  type CollectOptions =
    | Primitive of Primitive.CollectOptions
    | TypeSafe of TypeSafe.CollectOptions
    | Abstract of Abstract.ICollectOptions

    static member private ToSeq(s : String seq) =
      match s with
      | null -> Seq.empty<string>
      | _ -> s

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

    [<SuppressMessage("Microsoft.Naming", "CA1704",
        Justification="'Lcov' is jargon")>]
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

    [<SuppressMessage("Microsoft.Naming", "CA1704",
        Justification="'Cobertura' is jargon")>]
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
      | Primitive p -> p.CommandLine |> CollectOptions.ToSeq
      | Abstract a -> a.CommandLine |> CollectOptions.ToSeq
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
      member self.Verbosity = self.Verbosity

#if RUNNER
    member self.Validate afterPreparation =
      let saved = CommandLine.error

      let validate f x =
        if x
           |> String.IsNullOrWhiteSpace
           |> not
        then f x |> ignore

      let validateOptional f key x = validate (f key) x

      let toOption s =
        if s |> String.IsNullOrWhiteSpace then None else Some s
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
        if afterPreparation then Runner.requireRecorderTest (recorder |> toOption) () ()
        CommandLine.error |> List.toArray
      finally
        CommandLine.error <- saved
#else
#endif

  [<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false);
                    SuppressMessage("Gendarme.Rules.Smells",
                                    "RelaxedAvoidCodeDuplicatedInSameClassRule",
                                    Justification = "Idiomatic F#")>]
  type PrepareOptions =
    | Primitive of Primitive.PrepareOptions
    | TypeSafe of TypeSafe.PrepareOptions
    | Abstract of Abstract.IPrepareOptions

    static member private ToSeq(s : 'a seq) =
      match s with
      | null -> Seq.empty<'a>
      | _ -> s

    static member private ToList(s : 'a seq) =
      s
      |> PrepareOptions.ToSeq
      |> Seq.toList

    member self.InputDirectories =
      match self with
      | Primitive p -> p.InputDirectories |> PrepareOptions.ToList
      | Abstract a -> a.InputDirectories |> PrepareOptions.ToList
      | TypeSafe t -> t.InputDirectories.AsStrings()

    member self.OutputDirectories =
      match self with
      | Primitive p -> p.OutputDirectories |> PrepareOptions.ToList
      | Abstract a -> a.OutputDirectories |> PrepareOptions.ToList
      | TypeSafe t -> t.OutputDirectories.AsStrings()

    member self.SymbolDirectories =
      match self with
      | Primitive p -> p.SymbolDirectories |> PrepareOptions.ToList
      | Abstract a -> a.SymbolDirectories |> PrepareOptions.ToList
      | TypeSafe t -> t.SymbolDirectories.AsStrings()

    member self.Dependencies =
      match self with
      | Primitive p -> p.Dependencies |> PrepareOptions.ToList
      | Abstract a -> a.Dependencies |> PrepareOptions.ToList
      | TypeSafe t -> t.Dependencies.AsStrings()

    member self.Keys =
      match self with
      | Primitive p -> p.Keys |> PrepareOptions.ToList
      | Abstract a -> a.Keys |> PrepareOptions.ToList
      | TypeSafe t -> t.Keys.AsStrings()

    member self.StrongNameKey =
      match self with
      | Primitive p -> p.StrongNameKey
      | Abstract a -> a.StrongNameKey
      | TypeSafe t -> t.StrongNameKey.AsString()

    member self.XmlReport =
      match self with
      | Primitive p -> p.XmlReport
      | Abstract a -> a.XmlReport
      | TypeSafe t -> t.XmlReport.AsString()

    member self.FileFilter =
      match self with
      | Primitive p -> p.FileFilter |> PrepareOptions.ToList
      | Abstract a -> a.FileFilter |> PrepareOptions.ToList
      | TypeSafe t -> t.FileFilter.AsStrings()

    member self.AssemblyFilter =
      match self with
      | Primitive p -> p.AssemblyFilter |> PrepareOptions.ToList
      | Abstract a -> a.AssemblyFilter |> PrepareOptions.ToList
      | TypeSafe t -> t.AssemblyFilter.AsStrings()

    member self.AssemblyExcludeFilter =
      match self with
      | Primitive p -> p.AssemblyExcludeFilter |> PrepareOptions.ToList
      | Abstract a -> a.AssemblyExcludeFilter |> PrepareOptions.ToList
      | TypeSafe t -> t.AssemblyExcludeFilter.AsStrings()

    member self.TypeFilter =
      match self with
      | Primitive p -> p.TypeFilter |> PrepareOptions.ToList
      | Abstract a -> a.TypeFilter |> PrepareOptions.ToList
      | TypeSafe t -> t.TypeFilter.AsStrings()

    member self.MethodFilter =
      match self with
      | Primitive p -> p.MethodFilter |> PrepareOptions.ToList
      | Abstract a -> a.MethodFilter |> PrepareOptions.ToList
      | TypeSafe t -> t.MethodFilter.AsStrings()

    member self.AttributeFilter =
      match self with
      | Primitive p -> p.AttributeFilter |> PrepareOptions.ToList
      | Abstract a -> a.AttributeFilter |> PrepareOptions.ToList
      | TypeSafe t -> t.AttributeFilter.AsStrings()

    member self.PathFilter =
      match self with
      | Primitive p -> p.PathFilter |> PrepareOptions.ToList
      | Abstract a -> a.PathFilter |> PrepareOptions.ToList
      | TypeSafe t -> t.PathFilter.AsStrings()

    member self.AttributeTopLevel =
      match self with
      | Primitive p -> p.AttributeTopLevel |> PrepareOptions.ToList
      | Abstract a -> a.AttributeTopLevel |> PrepareOptions.ToList
      | TypeSafe t -> t.AttributeTopLevel.AsStrings()

    member self.TypeTopLevel =
      match self with
      | Primitive p -> p.TypeTopLevel |> PrepareOptions.ToList
      | Abstract a -> a.TypeTopLevel |> PrepareOptions.ToList
      | TypeSafe t -> t.TypeTopLevel.AsStrings()

    member self.MethodTopLevel =
      match self with
      | Primitive p -> p.MethodTopLevel |> PrepareOptions.ToList
      | Abstract a -> a.MethodTopLevel |> PrepareOptions.ToList
      | TypeSafe t -> t.MethodTopLevel.AsStrings()

    member self.CallContext =
      match self with
      | Primitive p -> p.CallContext |> PrepareOptions.ToList
      | Abstract a -> a.CallContext |> PrepareOptions.ToList
      | TypeSafe t -> t.CallContext.AsStrings()

    member self.ReportFormat =
      let simple =
        match self with
        | Primitive p -> p.ReportFormat
        | Abstract a -> a.ReportFormat
        | TypeSafe t -> t.ReportFormat.AsString()
      if String.IsNullOrWhiteSpace simple
      then "OpenCover"
      else simple

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

    member self.SingleVisit =
      match self with
      | Primitive p -> p.SingleVisit
      | Abstract a -> a.SingleVisit
      | TypeSafe t -> t.SingleVisit.AsBool()

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
      | Primitive p -> p.CommandLine |> PrepareOptions.ToSeq
      | Abstract a -> a.CommandLine |> PrepareOptions.ToSeq
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

    member self.Defer =
      match self with
      | Primitive p -> p.Defer
      | Abstract a -> a.Defer
      | TypeSafe t -> t.Defer.AsBool()

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

    interface Abstract.IPrepareOptions with
      member self.InputDirectories = self.InputDirectories |> PrepareOptions.ToSeq
      member self.OutputDirectories = self.OutputDirectories |> PrepareOptions.ToSeq
      member self.SymbolDirectories = self.SymbolDirectories |> PrepareOptions.ToSeq
      member self.Dependencies = self.Dependencies |> PrepareOptions.ToSeq
      member self.Keys = self.Keys |> PrepareOptions.ToSeq
      member self.StrongNameKey = self.StrongNameKey
      member self.XmlReport = self.XmlReport
      member self.FileFilter = self.FileFilter |> PrepareOptions.ToSeq
      member self.AssemblyFilter = self.AssemblyFilter |> PrepareOptions.ToSeq
      member self.AssemblyExcludeFilter = self.AssemblyExcludeFilter |> PrepareOptions.ToSeq
      member self.TypeFilter = self.TypeFilter |> PrepareOptions.ToSeq
      member self.MethodFilter = self.MethodFilter |> PrepareOptions.ToSeq
      member self.AttributeFilter = self.AttributeFilter |> PrepareOptions.ToSeq
      member self.PathFilter = self.PathFilter |> PrepareOptions.ToSeq
      member self.AttributeTopLevel = self.AttributeTopLevel |> PrepareOptions.ToSeq
      member self.TypeTopLevel = self.TypeTopLevel |> PrepareOptions.ToSeq
      member self.MethodTopLevel = self.MethodTopLevel |> PrepareOptions.ToSeq
      member self.CallContext = self.CallContext |> PrepareOptions.ToSeq
      member self.ReportFormat = self.ReportFormat
      member self.InPlace = self.InPlace
      member self.Save = self.Save
      member self.ZipFile = self.ZipFile
      member self.MethodPoint = self.MethodPoint
      member self.SingleVisit = self.SingleVisit
      member self.LineCover = self.LineCover
      member self.BranchCover = self.BranchCover
      member self.CommandLine = self.CommandLine |> PrepareOptions.ToSeq
      member self.ExposeReturnCode = self.ExposeReturnCode
      member self.SourceLink = self.SourceLink
      member self.Defer = self.Defer
      member self.LocalSource = self.LocalSource
      member self.VisibleBranches = self.VisibleBranches
      member self.ShowStatic = self.ShowStatic
      member self.ShowGenerated = self.ShowGenerated
      member self.Verbosity = self.Verbosity

#if RUNNER
    static member private ValidateArray a f key =
      PrepareOptions.ValidateArraySimple a (f key)

    static member private ValidateArraySimple a f = a |> Seq.iter (fun s -> f s |> ignore)

    static member private ValidateOptional f key x =
      if x
         |> String.IsNullOrWhiteSpace
         |> not
      then f key x |> ignore

    member private self.Consistent() =
      if self.SingleVisit && self.CallContext.Any() then
        CommandLine.error <-
          String.Format
            (System.Globalization.CultureInfo.CurrentCulture,
             CommandLine.resources.GetString "Incompatible", "--single", "--callContext")
          :: CommandLine.error

    member private self.Consistent'() =
      if self.LineCover && self.BranchCover then
        CommandLine.error <-
          String.Format
            (System.Globalization.CultureInfo.CurrentCulture,
             CommandLine.resources.GetString "Incompatible", "--branchcover", "--linecover")
          :: CommandLine.error

    member self.Validate() =
      let saved = CommandLine.error

      let validateContext context =
        let select state x =
          let (_, n) = Main.validateCallContext state x
          match (state, n) with
          | (true, _)
          | (_, Left(Some _)) -> true
          | _ -> false
        context
        |> PrepareOptions.ToSeq
        |> Seq.fold select false
        |> ignore

      try
        CommandLine.error <- []
        PrepareOptions.ValidateArray self.InputDirectories CommandLine.validateDirectory
          "--inputDirectory"
        PrepareOptions.ValidateArray self.OutputDirectories CommandLine.validatePath
          "--outputDirectory"
        PrepareOptions.ValidateOptional CommandLine.validateStrongNameKey "--strongNameKey"
          self.StrongNameKey
        PrepareOptions.ValidateOptional CommandLine.validatePath "--xmlReport"
          self.XmlReport
        PrepareOptions.ValidateArray self.SymbolDirectories CommandLine.validateDirectory
          "--symbolDirectory"
        PrepareOptions.ValidateArray self.Dependencies CommandLine.validateAssembly
          "--dependency"
        PrepareOptions.ValidateArray self.Keys CommandLine.validateStrongNameKey "--key"
        [ self.FileFilter
          self.AssemblyFilter
          self.AssemblyExcludeFilter
          self.TypeFilter
          self.MethodFilter
          self.AttributeFilter
          self.PathFilter ]
        |> Seq.iter
             (fun a -> PrepareOptions.ValidateArraySimple a CommandLine.validateRegexes)
        self.Consistent()
        self.Consistent'()
        validateContext self.CallContext
        CommandLine.error |> List.toArray
      finally
        CommandLine.error <- saved

  [<ExcludeFromCodeCoverage; NoComparison; NoEquality; AutoSerializable(false);
                    SuppressMessage("Gendarme.Rules.Smells",
                                    "RelaxedAvoidCodeDuplicatedInSameClassRule",
                                    Justification = "Idiomatic F#")>]
  type LoggingOptions =
    | Primitive of Primitive.LoggingOptions
    | Abstract of Abstract.ILoggingOptions

    static member Create() = Primitive.LoggingOptions.Create() |> Primitive
    static member Translate (input : Abstract.ILoggingOptions) =
      { Primitive.LoggingOptions.Create() with
          Failure = input.Failure |> LoggingOptions.ActionAdapter
          Warn = input.Warn |> LoggingOptions.ActionAdapter
          Echo = input.Echo |> LoggingOptions.ActionAdapter
          Info = input.Info |> LoggingOptions.ActionAdapter } |> Primitive

    static member ActionAdapter(action : Action<String>) =
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
#endif