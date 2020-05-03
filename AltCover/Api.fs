#if RUNNER
namespace AltCover
#else
namespace AltCoverFake.DotNet.Testing
#endif

open System
open System.Diagnostics.CodeAnalysis
open System.Linq
#if RUNNER
open AltCover
open AltCover.Augment
#else
open System.Reflection
open AltCoverFake.DotNet.Testing
open Fake.Core
open Fake.DotNet
#endif

#if RUNNER
[<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1704",
    Justification="'Api' is OK")>]
[<RequireQualifiedAccess>]
module FSApi =
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
#else
[<RequireQualifiedAccess>]
module AltCover =
#endif

  [<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false);
                    SuppressMessage("Gendarme.Rules.Smells",
                                    "RelaxedAvoidCodeDuplicatedInSameClassRule",
                                    Justification = "Idiomatic F#")>]
  type CollectOptions =
    | Primitive of Primitive.CollectOptions
    | TypeSafe of TypeSafe.CollectOptions

    static member private ToSeq(s : String seq) =
      match s with
      | null -> Seq.empty<string>
      | _ -> s

    member self.RecorderDirectory =
      match self with
      | Primitive p -> p.RecorderDirectory
      | TypeSafe t -> t.RecorderDirectory.AsString()

    member self.WorkingDirectory =
      match self with
      | Primitive p -> p.WorkingDirectory
      | TypeSafe t -> t.WorkingDirectory.AsString()

    member self.Executable =
      match self with
      | Primitive p -> p.Executable
      | TypeSafe t -> t.Executable.AsString()

    [<SuppressMessage("Microsoft.Naming", "CA1704",
        Justification="'Lcov' is jargon")>]
    member self.LcovReport =
      match self with
      | Primitive p -> p.LcovReport
      | TypeSafe t -> t.LcovReport.AsString()

    member self.Threshold =
      match self with
      | Primitive p -> p.Threshold
      | TypeSafe t -> t.Threshold.AsString()

    [<SuppressMessage("Microsoft.Naming", "CA1704",
        Justification="'Cobertura' is jargon")>]
    member self.Cobertura =
      match self with
      | Primitive p -> p.Cobertura
      | TypeSafe t -> t.Cobertura.AsString()

    member self.OutputFile =
      match self with
      | Primitive p -> p.OutputFile
      | TypeSafe t -> t.OutputFile.AsString()

    member self.CommandLine =
      match self with
      | Primitive p -> p.CommandLine |> CollectOptions.ToSeq
      | TypeSafe t -> t.CommandLine.AsStrings()

    member self.ExposeReturnCode =
      match self with
      | Primitive p -> p.ExposeReturnCode
      | TypeSafe t -> t.ExposeReturnCode.AsBool()

    member self.SummaryFormat =
      match self with
      | Primitive p -> p.SummaryFormat
      | TypeSafe t -> t.SummaryFormat.AsString()

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
      | TypeSafe t -> t.InputDirectories.AsStrings()

    member self.OutputDirectories =
      match self with
      | Primitive p -> p.OutputDirectories |> PrepareOptions.ToList
      | TypeSafe t -> t.OutputDirectories.AsStrings()

    member self.SymbolDirectories =
      match self with
      | Primitive p -> p.SymbolDirectories |> PrepareOptions.ToList
      | TypeSafe t -> t.SymbolDirectories.AsStrings()

    member self.Dependencies =
      match self with
      | Primitive p -> p.Dependencies |> PrepareOptions.ToList
      | TypeSafe t -> t.Dependencies.AsStrings()

    member self.Keys =
      match self with
      | Primitive p -> p.Keys |> PrepareOptions.ToList
      | TypeSafe t -> t.Keys.AsStrings()

    member self.StrongNameKey =
      match self with
      | Primitive p -> p.StrongNameKey
      | TypeSafe t -> t.StrongNameKey.AsString()

    member self.XmlReport =
      match self with
      | Primitive p -> p.XmlReport
      | TypeSafe t -> t.XmlReport.AsString()

    member self.FileFilter =
      match self with
      | Primitive p -> p.FileFilter |> PrepareOptions.ToList
      | TypeSafe t -> t.FileFilter.AsStrings()

    member self.AssemblyFilter =
      match self with
      | Primitive p -> p.AssemblyFilter |> PrepareOptions.ToList
      | TypeSafe t -> t.AssemblyFilter.AsStrings()

    member self.AssemblyExcludeFilter =
      match self with
      | Primitive p -> p.AssemblyExcludeFilter |> PrepareOptions.ToList
      | TypeSafe t -> t.AssemblyExcludeFilter.AsStrings()

    member self.TypeFilter =
      match self with
      | Primitive p -> p.TypeFilter |> PrepareOptions.ToList
      | TypeSafe t -> t.TypeFilter.AsStrings()

    member self.MethodFilter =
      match self with
      | Primitive p -> p.MethodFilter |> PrepareOptions.ToList
      | TypeSafe t -> t.MethodFilter.AsStrings()

    member self.AttributeFilter =
      match self with
      | Primitive p -> p.AttributeFilter |> PrepareOptions.ToList
      | TypeSafe t -> t.AttributeFilter.AsStrings()

    member self.PathFilter =
      match self with
      | Primitive p -> p.PathFilter |> PrepareOptions.ToList
      | TypeSafe t -> t.PathFilter.AsStrings()

    member self.CallContext =
      match self with
      | Primitive p -> p.CallContext |> PrepareOptions.ToList
      | TypeSafe t -> t.CallContext.AsStrings()

    member self.ReportFormat =
      let simple =
        match self with
        | Primitive p -> p.ReportFormat
        | TypeSafe t -> t.ReportFormat.AsString()
      if String.IsNullOrWhiteSpace simple
      then "OpenCover"
      else simple

    member self.InPlace =
      match self with
      | Primitive p -> p.InPlace
      | TypeSafe t -> t.InPlace.AsBool()

    member self.Save =
      match self with
      | Primitive p -> p.Save
      | TypeSafe t -> t.Save.AsBool()

    member self.ZipFile =
      match self with
      | Primitive p -> p.ZipFile
      | TypeSafe t -> t.ZipFile.AsBool()

    member self.MethodPoint =
      match self with
      | Primitive p -> p.MethodPoint
      | TypeSafe t -> t.MethodPoint.AsBool()

    member self.Single =
      match self with
      | Primitive p -> p.Single
      | TypeSafe t -> t.Single.AsBool()

    member self.LineCover =
      match self with
      | Primitive p -> p.LineCover
      | TypeSafe t -> t.LineCover.AsBool()

    member self.BranchCover =
      match self with
      | Primitive p -> p.BranchCover
      | TypeSafe t -> t.BranchCover.AsBool()

    member self.CommandLine =
      match self with
      | Primitive p -> p.CommandLine |> PrepareOptions.ToSeq
      | TypeSafe t -> t.CommandLine.AsStrings()

    member self.ExposeReturnCode =
      match self with
      | Primitive p -> p.ExposeReturnCode
      | TypeSafe t -> t.ExposeReturnCode.AsBool()

    member self.SourceLink =
      match self with
      | Primitive p -> p.SourceLink
      | TypeSafe t -> t.SourceLink.AsBool()

    member self.Defer =
      match self with
      | Primitive p -> p.Defer
      | TypeSafe t -> t.Defer.AsBool()

    member self.LocalSource =
      match self with
      | Primitive p -> p.LocalSource
      | TypeSafe t -> t.LocalSource.AsBool()

    member self.VisibleBranches =
      match self with
      | Primitive p -> p.VisibleBranches
      | TypeSafe t -> t.VisibleBranches.AsBool()

    member self.ShowStatic =
      match self with
      | Primitive p -> p.ShowStatic
      | TypeSafe t -> t.ShowStatic.AsString()

    member self.ShowGenerated =
      match self with
      | Primitive p -> p.ShowGenerated
      | TypeSafe t -> t.ShowGenerated.AsBool()

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
      if self.Single && self.CallContext.Any() then
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

    static member Create() = Primitive.LoggingOptions.Create() |> Primitive

    static member ActionAdapter(action : Action<String>) =
      match action with
      | null -> ignore
      | _ -> action.Invoke

    member self.Error =
      match self with
      | Primitive p -> p.Error

    member self.Warn =
      match self with
      | Primitive p -> p.Warn

    member self.Echo =
      match self with
      | Primitive p -> p.Echo

    member self.Info =
      match self with
      | Primitive p -> p.Info
#endif