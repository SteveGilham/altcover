namespace AltCover

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.Globalization
open System.IO
open System.Linq
open System.Reflection

open AltCover.Base
open Augment
open Mono.Cecil
open Mono.Options

[<ExcludeFromCodeCoverage>]
type internal AssemblyInfo =
  { Path : string list
    Name : string
    Refs : string list }

module internal Main =
  let init() =
    CommandLine.error <- []
    CommandLine.dropReturnCode := false // ddFlag
    CoverageParameters.defer := None
    CoverageParameters.inputDirectories.Clear()
    CoverageParameters.outputDirectories.Clear()
    ProgramDatabase.symbolFolders.Clear()
    Instrument.ResolutionTable.Clear()

    CoverageParameters.keys.Clear()
    CoverageParameters.defaultStrongNameKey <- None
    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream("AltCover.Recorder.snk")
    use buffer = new MemoryStream()
    stream.CopyTo(buffer)
    let snk = StrongNameKeyData.Make(buffer.ToArray())
    CoverageParameters.Add snk
    CoverageParameters.recorderStrongNameKey <- Some(snk)

    CoverageParameters.reportPath <- None
    CoverageParameters.NameFilters.Clear()
    CoverageParameters.interval <- None
    CoverageParameters.TrackingNames.Clear()
    CoverageParameters.reportFormat <- None
    CoverageParameters.inplace := false // ddFlag
    CoverageParameters.collect := false // ddFlag
    CoverageParameters.local := false // ddFlag
    CoverageParameters.single <- false // more complicated
    CoverageParameters.coverstyle <- CoverStyle.All
    CoverageParameters.sourcelink := false // ddFlag
    CoverageParameters.coalesceBranches := false // ddFlag
    CoverageParameters.staticFilter <- None
    CoverageParameters.showGenerated := false

  let ValidateCallContext predicate x =
    if not (String.IsNullOrWhiteSpace x) then
      let k = x.Trim()
      if Char.IsDigit <| k.Chars(0) then
        if predicate || k.Length > 1 then
          CommandLine.error <-
            String.Format
              (CultureInfo.CurrentCulture,
               CommandLine.resources.GetString
                 ((if predicate then "MultiplesNotAllowed" else "InvalidValue")),
               "--callContext", x)
            :: CommandLine.error
          (false, Left None)
        else
          let (ok, n) = Int32.TryParse(k)
          if ok then
            (ok, Left(Some(pown 10 (7 - n))))
          else
            CommandLine.error <-
              String.Format
                (CultureInfo.CurrentCulture,
                 CommandLine.resources.GetString "InvalidValue", "--callContext", x)
              :: CommandLine.error
            (false, Left None)
      else
        (true, Right k)
    else
      CommandLine.error <-
        String.Format
          (CultureInfo.CurrentCulture, CommandLine.resources.GetString "InvalidValue",
           "--callContext", x) :: CommandLine.error
      (false, Left None)

  let internal declareOptions() =
    let makeFilter filterscope (x : String) =
      x.Replace(char 0, '\\').Replace(char 1, '|')
      |> CommandLine.validateRegexes
      |> Seq.iter (FilterClass.Build filterscope >> CoverageParameters.NameFilters.Add)

    [ ("i|inputDirectory=",
       (fun x ->
         if CommandLine.validateDirectory "--inputDirectory" x then
           let arg = Path.GetFullPath x
           if CoverageParameters.inputDirectories.Contains arg then
             CommandLine.error <-
               String.Format
                 (CultureInfo.CurrentCulture,
                  CommandLine.resources.GetString "DuplicatesNotAllowed", arg,
                  "--inputDirectory") :: CommandLine.error
           else
             CoverageParameters.inputDirectories.Add arg))

      ("o|outputDirectory=",
       (fun x ->
         if CommandLine.validatePath "--outputDirectory" x then
           let arg = Path.GetFullPath x
           if CoverageParameters.outputDirectories.Contains arg then
             CommandLine.error <-
               String.Format
                 (CultureInfo.CurrentCulture,
                  CommandLine.resources.GetString "DuplicatesNotAllowed", arg,
                  "--outputDirectory") :: CommandLine.error
           else
             CommandLine.doPathOperation (fun _ -> CoverageParameters.outputDirectories.Add arg) ()
               false))

      ("y|symbolDirectory=",
       (fun x ->
         if CommandLine.validateDirectory "--symbolDirectory" x then
           ProgramDatabase.symbolFolders.Add x))
      ("d|dependency=",
       (fun x ->
         CommandLine.doPathOperation (fun _ ->
           let path =
             x
             |> Environment.ExpandEnvironmentVariables
             |> Path.GetFullPath

           let name, ok = CommandLine.validateAssembly "--dependency" path
           if ok then
             Instrument.ResolutionTable.[name] <- AssemblyDefinition.ReadAssembly path)
           () false))

      ("k|key=",
       (fun x ->
         let (pair, ok) = CommandLine.validateStrongNameKey "--key" x
         if ok then CoverageParameters.Add pair))
      ("sn|strongNameKey=",
       (fun x ->
         let (pair, ok) = CommandLine.validateStrongNameKey "--strongNameKey" x
         if ok then
           if Option.isSome CoverageParameters.defaultStrongNameKey then
             CommandLine.error <-
               String.Format
                 (CultureInfo.CurrentCulture,
                  CommandLine.resources.GetString "MultiplesNotAllowed", "--strongNameKey")
               :: CommandLine.error
           else
             CoverageParameters.defaultStrongNameKey <- Some pair
             CoverageParameters.Add pair))

      ("x|xmlReport=",
       (fun x ->
         if CommandLine.validatePath "--xmlReport" x then
           if Option.isSome CoverageParameters.reportPath then
             CommandLine.error <-
               String.Format
                 (CultureInfo.CurrentCulture,
                  CommandLine.resources.GetString "MultiplesNotAllowed", "--xmlReport")
               :: CommandLine.error
           else
             CommandLine.doPathOperation
               (fun () -> CoverageParameters.reportPath <- Some(Path.GetFullPath x)) () false))
      ("f|fileFilter=", makeFilter FilterScope.File)
      ("p|pathFilter=", makeFilter FilterScope.Path)
      ("s|assemblyFilter=", makeFilter FilterScope.Assembly)
      ("e|assemblyExcludeFilter=", makeFilter FilterScope.Module)
      ("t|typeFilter=", makeFilter FilterScope.Type)
      ("m|methodFilter=", makeFilter FilterScope.Method)
      ("a|attributeFilter=", makeFilter FilterScope.Attribute)
      (CommandLine.ddFlag "l|localSource" CoverageParameters.local)
      ("c|callContext=",
       (fun x ->
         if CoverageParameters.single then
           CommandLine.error <-
             String.Format
               (CultureInfo.CurrentCulture, CommandLine.resources.GetString "Incompatible",
                "--single", "--callContext") :: CommandLine.error
         else
           let (ok, selection) = ValidateCallContext (Option.isSome CoverageParameters.interval) x
           if ok then
             match selection with
             | Left n -> CoverageParameters.interval <- n
             | Right name -> CoverageParameters.TrackingNames.Add(name)))
      ("opencover",
       (fun _ ->
         if Option.isSome CoverageParameters.reportFormat then
           CommandLine.error <-
             String.Format
               (CultureInfo.CurrentCulture,
                CommandLine.resources.GetString "MultiplesNotAllowed", "--opencover")
             :: CommandLine.error
         else
           CoverageParameters.reportFormat <- Some ReportFormat.OpenCover))
      (CommandLine.ddFlag "inplace" CoverageParameters.inplace)
      (CommandLine.ddFlag "save" CoverageParameters.collect)
      ("single",
       (fun _ ->
         if CoverageParameters.single then
           CommandLine.error <-
             String.Format
               (CultureInfo.CurrentCulture,
                CommandLine.resources.GetString "MultiplesNotAllowed", "--single")
             :: CommandLine.error
         else if Option.isSome CoverageParameters.interval || CoverageParameters.TrackingNames.Any() then
           CommandLine.error <-
             String.Format
               (CultureInfo.CurrentCulture, CommandLine.resources.GetString "Incompatible",
                "--single", "--callContext") :: CommandLine.error
         else
           CoverageParameters.single <- true))
      ("linecover",
       (fun _ ->
         match CoverageParameters.coverstyle with
         | CoverStyle.LineOnly ->
             CommandLine.error <-
               String.Format
                 (CultureInfo.CurrentCulture,
                  CommandLine.resources.GetString "MultiplesNotAllowed", "--linecover")
               :: CommandLine.error
         | CoverStyle.BranchOnly ->
             CommandLine.error <-
               String.Format
                 (CultureInfo.CurrentCulture,
                  CommandLine.resources.GetString "Incompatible", "--linecover",
                  "--branchcover") :: CommandLine.error
         | _ -> CoverageParameters.coverstyle <- CoverStyle.LineOnly))
      ("branchcover",
       (fun _ ->
         match CoverageParameters.coverstyle with
         | CoverStyle.BranchOnly ->
             CommandLine.error <-
               String.Format
                 (CultureInfo.CurrentCulture,
                  CommandLine.resources.GetString "MultiplesNotAllowed", "--branchcover")
               :: CommandLine.error
         | CoverStyle.LineOnly ->
             CommandLine.error <-
               String.Format
                 (CultureInfo.CurrentCulture,
                  CommandLine.resources.GetString "Incompatible", "--branchcover",
                  "--linecover") :: CommandLine.error
         | _ -> CoverageParameters.coverstyle <- CoverStyle.BranchOnly))
      (CommandLine.ddFlag "dropReturnCode" CommandLine.dropReturnCode)
      (CommandLine.ddFlag "sourcelink" CoverageParameters.sourcelink)
      ("defer:",
       (fun x ->
         if !CoverageParameters.defer = None then
           CoverageParameters.defer := if String.IsNullOrWhiteSpace x then
                                         Some true
                                       else
                                         let (|Select|_|) (pattern : String) offered =
                                           if offered
                                              |> String.IsNullOrWhiteSpace
                                              |> not
                                              && pattern.Equals
                                                   (offered, StringComparison.OrdinalIgnoreCase) then
                                             Some offered
                                           else
                                             None
                                         match x with
                                         | Select "-" _ -> Some false
                                         | Select "+" _ -> Some true
                                         | _ -> None
           if !CoverageParameters.defer = None then
             CommandLine.error <-
               String.Format
                 (CultureInfo.CurrentCulture,
                  CommandLine.resources.GetString "InvalidValue", "--defer", x)
               :: CommandLine.error
         else
           CommandLine.error <-
             String.Format
               (CultureInfo.CurrentCulture,
                CommandLine.resources.GetString "MultiplesNotAllowed", "--defer")
             :: CommandLine.error))
      (CommandLine.ddFlag "v|visibleBranches" CoverageParameters.coalesceBranches)
      ("showstatic:",
       (fun x ->
         if CoverageParameters.staticFilter = None then
           CoverageParameters.staticFilter <-
             if String.IsNullOrWhiteSpace x || x = "+" then Some StaticFilter.AsCovered
             else if x = "++" then Some StaticFilter.NoFilter
             else if x = "-" then Some StaticFilter.Hidden
             else None
           if CoverageParameters.staticFilter = None then
             CommandLine.error <-
               String.Format
                 (CultureInfo.CurrentCulture,
                  CommandLine.resources.GetString "InvalidValue", "--showstatic", x)
               :: CommandLine.error
         else
           CommandLine.error <-
             String.Format
               (CultureInfo.CurrentCulture,
                CommandLine.resources.GetString "MultiplesNotAllowed", "--showstatic")
             :: CommandLine.error))
      (CommandLine.ddFlag "showGenerated" CoverageParameters.showGenerated)
      ("?|help|h", (fun x -> CommandLine.help <- x.IsNotNull))

      ("<>",
       (fun x ->
         CommandLine.error <-
           String.Format
             (CultureInfo.CurrentCulture, CommandLine.resources.GetString "InvalidValue",
              "AltCover", x) :: CommandLine.error)) ] // default end stop
    |> List.fold
         (fun (o : OptionSet) (p, a) ->
           o.Add(p, CommandLine.resources.GetString(p), new System.Action<string>(a)))
         (OptionSet())

  let internal processOutputLocation(action : Either<string * OptionSet, string list * OptionSet>) =
    match action with
    | Right(rest, options) ->
        // Check that the directories are distinct
        let fromDirectories = CoverageParameters.InputDirectories()
        let toDirectories = CoverageParameters.OutputDirectories()
        fromDirectories
        |> Seq.iter (fun fromDirectory ->
             if toDirectories.Contains fromDirectory then
               CommandLine.error <-
                 String.Format
                   (CultureInfo.CurrentCulture,
                    CommandLine.resources.GetString "NotInPlace", fromDirectory)
                 :: CommandLine.error)

        CommandLine.doPathOperation (fun () ->
          let found = toDirectories |> Seq.filter Directory.Exists
          if !CoverageParameters.inplace && CommandLine.error |> List.isEmpty && found.Any() then
            found
            |> Seq.iter (fun toDirectory ->
                 CommandLine.error <-
                   String.Format
                     (CultureInfo.CurrentCulture,
                      CommandLine.resources.GetString "SaveExists", toDirectory)
                   :: CommandLine.error)
          if CommandLine.error |> List.isEmpty then
            (Seq.iter CommandLine.ensureDirectory toDirectories)) () false
        if CommandLine.error
           |> List.isEmpty
           |> not then
          Left("UsageError", options)
        else
          Seq.zip toDirectories fromDirectories
          |> Seq.iter (fun (toDirectory, fromDirectory) ->
               if !CoverageParameters.inplace then
                 Output.info
                 <| String.Format
                      (CultureInfo.CurrentCulture,
                       (CommandLine.resources.GetString "savingto"), toDirectory)
                 Output.info
                 <| String.Format
                      (CultureInfo.CurrentCulture,
                       (CommandLine.resources.GetString "instrumentingin"), fromDirectory)
               else
                 Output.info
                 <| String.Format
                      (CultureInfo.CurrentCulture,
                       (CommandLine.resources.GetString "instrumentingfrom"),
                       fromDirectory)
                 Output.info
                 <| String.Format
                      (CultureInfo.CurrentCulture,
                       (CommandLine.resources.GetString "instrumentingto"), toDirectory))
          Right
            (rest, fromDirectories |> Seq.map DirectoryInfo,
             toDirectories |> Seq.map DirectoryInfo,
             CoverageParameters.SourceDirectories() |> Seq.map DirectoryInfo)
    | Left intro -> Left intro

  let internal imageLoadResilient (f : unit -> 'a) (tidy : unit -> 'a) =
    try
      f()
    with
    | :? Mono.Cecil.Cil.SymbolsNotMatchingException
    | :? BadImageFormatException
    | :? ArgumentException
    | :? IOException -> tidy()

  let internal prepareTargetFiles (fromInfos : DirectoryInfo seq)
      (toInfos : DirectoryInfo seq) (sourceInfos : DirectoryInfo seq)
      (targets : string seq) =
    // Copy all the files into the target directory
    let mapping = Dictionary<string, string>()
    Seq.zip sourceInfos targets
    |> Seq.map (fun (x, y) ->
         let f = x.FullName // trim separator
         (Path.Combine(f |> Path.GetDirectoryName, f |> Path.GetFileName), y))
    |> Seq.iter mapping.Add

    Seq.zip fromInfos toInfos
    |> Seq.iter (fun (fromInfo, toInfo) ->
         let files = fromInfo.GetFiles()
         files
         |> Seq.iter (fun info ->
              let fullName = info.FullName
              let filename = info.Name
              let copy = Path.Combine(toInfo.FullName, filename)
              File.Copy(fullName, copy, true)))

    // Track the symbol-bearing assemblies
    let assemblies =
      sourceInfos
      |> Seq.map (fun sourceInfo ->
           sourceInfo.GetFiles()
           |> Seq.fold (fun (accumulator : AssemblyInfo list) info ->
                let fullName = info.FullName
                imageLoadResilient (fun () ->
                  use stream = File.OpenRead(fullName)
                  use def = AssemblyDefinition.ReadAssembly(stream)
                  ProgramDatabase.readSymbols def
                  if def.MainModule.HasSymbols &&
                    (def.IsIncluded).IsInstrumented
                     && (def.MainModule.Attributes &&& ModuleAttributes.ILOnly =
                           ModuleAttributes.ILOnly) then
                    String.Format
                      (CultureInfo.CurrentCulture,
                       (CommandLine.resources.GetString "instrumenting"), fullName)
                    |> Output.info
                    { Path = [ fullName ]
                      Name = def.Name.Name
                      Refs =
                        def.MainModule.AssemblyReferences
                        |> Seq.map (fun r -> r.Name)
                        |> Seq.toList }
                    :: accumulator
                  else
                    accumulator) (fun () -> accumulator)) [])
      |> Seq.toList
      |> Seq.concat
      |> Seq.groupBy (fun a -> a.Name) // assume name is unique
      |> Seq.map (fun (n, agroup) ->
           { (agroup |> Seq.head) with
               Path =
                 agroup
                 |> Seq.map (fun aa -> aa.Path)
                 |> Seq.concat
                 |> Seq.toList })
      |> Seq.toList

    // sort the assemblies into order so that the depended-upon are processed first

    // The set of all names w/o location
    let candidates =
      assemblies
      |> Seq.map (fun a -> a.Name)
      |> Seq.fold (fun (s : Set<string>) n -> Set.add n s) Set.empty<string>

    let simplified =
      assemblies
      |> List.map (fun a ->
           { a with Refs = a.Refs |> List.filter (fun n -> Set.contains n candidates) })

    let rec bundle unassigned unresolved collection n =
      match unassigned with
      | [] -> collection
      | _ ->
          let stage =
            (if n <= 1
             then unassigned
             else unassigned |> List.filter (fun u -> u.Refs |> List.isEmpty))
            |> List.sortBy (fun u -> u.Name)

          let waiting = stage |> List.fold (fun s a -> Set.remove a.Name s) unresolved

          let next =
            unassigned
            |> List.filter (fun u ->
                 u.Refs
                 |> List.isEmpty
                 |> not)
            |> List.map (fun a ->
                 { a with Refs = a.Refs |> List.filter (fun n -> Set.contains n waiting) })
          bundle next waiting (stage :: collection) (n - 1)

    let sorted =
      bundle simplified candidates [] (simplified |> List.length)
      |> List.concat
      |> List.rev
      |> List.map (fun a ->
           let proto = a.Path.Head
           let targets =
             a.Path |> List.map (Path.GetDirectoryName >> (fun d -> mapping.[d]))
           ((proto, targets), a.Name))

    List.unzip sorted

  let internal doInstrumentation arguments =
#if NETCOREAPP2_0
    let dotnetBuild =
      Assembly.GetEntryAssembly() // is null for unit tests
      |> Option.nullable
      |> Option.map (fun a -> Path.GetFileName(a.Location).Equals("MSBuild.dll"))
      |> Option.getOrElse false

#else
    let dotnetBuild = false

#endif
    let check1 =
      declareOptions()
      |> CommandLine.parseCommandLine arguments
      |> CommandLine.processHelpOption
      |> processOutputLocation
    match check1 with
    | Left(intro, options) ->
        CommandLine.HandleBadArguments dotnetBuild arguments
          { Intro = intro
            Options = options
            Options2 = Runner.DeclareOptions() }
        255
    | Right(rest, fromInfo, toInfo, targetInfo) ->
        let report = CoverageParameters.ReportPath()

        let result =
          CommandLine.doPathOperation (fun () ->
            report
            |> Path.GetDirectoryName
            |> CommandLine.ensureDirectory
            let (assemblies, assemblyNames) =
              prepareTargetFiles fromInfo toInfo targetInfo
                (CoverageParameters.InstrumentDirectories())
            Output.info
            <| String.Format
                 (CultureInfo.CurrentCulture,
                  (CommandLine.resources.GetString "reportingto"), report)
            let reporter, document =
              match CoverageParameters.ReportKind() with
              | ReportFormat.OpenCover -> OpenCover.ReportGenerator()
              | _ -> Report.ReportGenerator()

            let visitors =
              [ reporter
                Instrument.instrumentGenerator assemblyNames ]

            Visitor.Visit visitors (assemblies)
            report
            |> Path.GetDirectoryName
            |> Directory.CreateDirectory
            |> ignore
            document.Save(report)
            if !CoverageParameters.collect then Runner.SetRecordToFile report
            CommandLine.processTrailingArguments rest (toInfo |> Seq.head)) 255 true
        CommandLine.ReportErrors "Instrumentation" (dotnetBuild && !CoverageParameters.inplace)
        result

  let internal (|Select|_|) (pattern : String) offered =
    if offered
       |> String.IsNullOrWhiteSpace
       |> not
       && pattern.StartsWith(offered, StringComparison.OrdinalIgnoreCase) then
      Some offered
    else
      None

  let internal main arguments =
    let first =
      arguments
      |> Seq.tryHead
      |> Option.getOrElse String.Empty
    init()
    match first with
    | Select "Runner" _ ->
        Runner.init()
        Runner.DoCoverage arguments (declareOptions())
    | Select "ipmo" _ ->
        Path.Combine
          (Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName,
           "AltCover.PowerShell.dll")
        |> Path.GetFullPath
        |> sprintf "Import-Module %A"
        |> (Output.info)
        0
    | Select "version" _ ->
        Runner.WriteResourceWithFormatItems "AltCover.Version"
          [| AssemblyVersionInformation.AssemblyFileVersion |] false
        0
    | _ -> doInstrumentation arguments

  // mocking point
  [<System.Diagnostics.CodeAnalysis.SuppressMessage(
      "Gendarme.Rules.Performance", "AvoidUncalledPrivateCodeRule",
      Justification = "Unit test accessor")>]
  let mutable internal effectiveMain = main