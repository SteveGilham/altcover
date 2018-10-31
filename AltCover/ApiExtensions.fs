namespace AltCover

open System
open System.Linq
open AltCover
open AltCover.Augment
open System.Diagnostics.CodeAnalysis

module ApiExtensions =
  type AltCover.CollectParams with
    member self.Validate afterPreparation =
      let saved = CommandLine.error

      let validate f x =
        if x
           |> String.IsNullOrWhiteSpace
           |> not
        then f x |> ignore

      let validateOptional f key x = validate (f key) x

      let toOption s =
        if s |> String.IsNullOrWhiteSpace then None
        else Some s
      try
        [ ("--recorderDirectory", self.RecorderDirectory)
          ("--workingDirectory", self.WorkingDirectory) ]
        |> List.iter (fun (n, x) -> validateOptional CommandLine.ValidateDirectory n x)
        [ ("--executable", self.Executable)
          ("--lcovReport", self.LcovReport)
          ("--cobertura", self.Cobertura)
          ("--outputFile", self.OutputFile) ]
        |> List.iter (fun (n, x) -> validateOptional CommandLine.ValidatePath n x)
        validate Runner.ValidateThreshold self.Threshold
        if afterPreparation then
          Runner.RequireRecorderTest (self.RecorderDirectory |> toOption) () ()
        CommandLine.error |> List.toArray
      finally
        CommandLine.error <- saved

  type AltCover.PrepareParams with
    static member private validateArray a f key =
      PrepareParams.validateArraySimple a (f key)

    static member private nonNull a =
      a
      |> isNull
      |> not

    [<SuppressMessage("Microsoft.Usage", "CA2208",
                       Justification = "Some in-lined code must be creating an ArgumentNullException")>]
    static member private validateArraySimple a f =
      if a |> PrepareParams.nonNull then a |> Seq.iter (fun s -> f s |> ignore)

    static member private validateOptional f key x =
      if x
         |> String.IsNullOrWhiteSpace
         |> not
      then f key x |> ignore

    member private self.consistent() =
      if self.Single && self.CallContext |> PrepareParams.nonNull
         && self.CallContext.Any() then
        CommandLine.error <- String.Format
                               (System.Globalization.CultureInfo.CurrentCulture,
                                CommandLine.resources.GetString "Incompatible", "--single",
                                "--callContext") :: CommandLine.error

    member private self.consistent'() =
      if self.LineCover && self.BranchCover then
        CommandLine.error <- String.Format
                               (System.Globalization.CultureInfo.CurrentCulture,
                                CommandLine.resources.GetString "Incompatible",
                                "--branchcover", "--linecover") :: CommandLine.error

    member self.Validate() =
      let saved = CommandLine.error

      let validateContext context =
        if context
           |> isNull
           |> not
        then
          let select state x =
            let (_, n) = Main.ValidateCallContext state x
            match (state, n) with
            | (true, _) | (_, Left(Some _)) -> true
            | _ -> false
          context
          |> Seq.fold select false
          |> ignore
      try
        CommandLine.error <- []
        PrepareParams.validateOptional CommandLine.ValidateDirectory "--inputDirectory"
          self.InputDirectory
        PrepareParams.validateOptional CommandLine.ValidatePath "--outputDirectory"
          self.OutputDirectory
        PrepareParams.validateOptional CommandLine.ValidateStrongNameKey "--strongNameKey"
          self.StrongNameKey
        PrepareParams.validateOptional CommandLine.ValidatePath "--xmlReport"
          self.XmlReport
        PrepareParams.validateArray self.SymbolDirectories CommandLine.ValidateDirectory
          "--symbolDirectory"
        PrepareParams.validateArray self.Dependencies CommandLine.ValidateAssembly
          "--dependency"
        PrepareParams.validateArray self.Keys CommandLine.ValidateStrongNameKey "--key"
        [ self.FileFilter; self.AssemblyFilter; self.AssemblyExcludeFilter;
          self.TypeFilter; self.MethodFilter; self.AttributeFilter; self.PathFilter ]
        |> Seq.iter
             (fun a -> PrepareParams.validateArraySimple a CommandLine.ValidateRegexes)
        self.consistent()
        self.consistent'()
        validateContext self.CallContext
        CommandLine.error |> List.toArray
      finally
        CommandLine.error <- saved