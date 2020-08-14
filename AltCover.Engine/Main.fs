namespace AltCover

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.Globalization
open System.IO
open System.Linq
open System.Reflection

open Mono.Cecil
open Mono.Options

[<ExcludeFromCodeCoverage; AutoSerializable(false)>]
type internal AssemblyInfo =
  { Path : string list
    Name : string
    Refs : string list }

module internal Main =
  let internal (|Select|_|) (pattern : String) offered =
    if offered
        |> String.IsNullOrWhiteSpace
        |> not
        && pattern.StartsWith(offered, StringComparison.OrdinalIgnoreCase) then
      Some offered
    else
      None

  let internal init() =
    CommandLine.error <- []
    CommandLine.dropReturnCode := false // ddFlag
    CoverageParameters.defer := false // ddflag
    CoverageParameters.theInputDirectories.Clear()
    CoverageParameters.theOutputDirectories.Clear()
    ProgramDatabase.symbolFolders.Clear()
    Instrument.resolutionTable.Clear()

    CoverageParameters.keys.Clear()
    CoverageParameters.defaultStrongNameKey <- None
    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream("AltCover.Recorder.snk")
    use buffer = new MemoryStream()
    stream.CopyTo(buffer)
    let snk = StrongNameKeyData.Make(buffer.ToArray())
    CoverageParameters.add snk
    CoverageParameters.recorderStrongNameKey <- Some(snk)

    CoverageParameters.theReportPath <- None
    CoverageParameters.zipReport := false
    CoverageParameters.methodPoint := false
    CoverageParameters.nameFilters.Clear()
    CoverageParameters.theInterval <- None
    CoverageParameters.trackingNames.Clear()
    CoverageParameters.topLevel.Clear()
    CoverageParameters.theReportFormat <- None
    CoverageParameters.inplace := false // ddFlag
    CoverageParameters.collect := false // ddFlag
    CoverageParameters.local := false // ddFlag
    CoverageParameters.single <- false // more complicated
    CoverageParameters.coverstyle <- CoverStyle.All
    CoverageParameters.sourcelink := false // ddFlag
    CoverageParameters.coalesceBranches := false // ddFlag
    CoverageParameters.staticFilter <- None
    CoverageParameters.showGenerated := false

  let internal validateCallContext predicate x =
    if not (String.IsNullOrWhiteSpace x) then
      let k = x.Trim()
      if Char.IsDigit <| k.Chars(0) then
        if predicate || k.Length > 1 then
          CommandLine.error <-
            CommandLine.Format.Local(
              (if predicate then "MultiplesNotAllowed" else "InvalidValue"),
               "--callContext", x)
            :: CommandLine.error
          (false, Left None)
        else
          let (ok, n) = Int32.TryParse(k)
          if ok then
            (ok, Left(Some(pown 10 (7 - n))))
          else
            CommandLine.error <-
              CommandLine.Format.Local("InvalidValue", "--callContext", x)
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

  [<SuppressMessage("Microsoft.Maintainability", "CA1506:AvoidExcessiveClassCoupling",
    Justification="It's perfectly maintainable.")>]
  module internal I =
    [<SuppressMessage("Microsoft.Maintainability", "CA1506:AvoidExcessiveClassCoupling",
      Justification="It's perfectly maintainable.")>]
    let internal declareOptions() =
      let makeRegex (x : String) =
        x.Replace(char 0, '\\').Replace(char 1, '|')
        |> CommandLine.validateRegexes

      let makeFilter filterscope =
        makeRegex >>
        Seq.map (FilterClass.Build filterscope) >>
        CoverageParameters.nameFilters.AddRange

      let makeTopLevel filterscope =
         makeRegex >>
         (Seq.map (FilterClass.Build filterscope)) >>
         CoverageParameters.topLevel.AddRange

      [ ("i|inputDirectory=",
         (fun x ->
           if CommandLine.validateDirectory "--inputDirectory" x then
             let arg = Path.GetFullPath x
             if CoverageParameters.theInputDirectories.Contains arg then
               CommandLine.error <-
                 CommandLine.Format.Local("DuplicatesNotAllowed", arg,
                    "--inputDirectory") :: CommandLine.error
             else
               CoverageParameters.theInputDirectories.Add arg))

        ("o|outputDirectory=",
         (fun x ->
           if CommandLine.validatePath "--outputDirectory" x then
             let arg = Path.GetFullPath x
             if CoverageParameters.theOutputDirectories.Contains arg then
               CommandLine.error <-
                 CommandLine.Format.Local("DuplicatesNotAllowed", arg,
                    "--outputDirectory") :: CommandLine.error
             else
               CommandLine.doPathOperation (fun _ -> CoverageParameters.theOutputDirectories.Add arg) ()
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
               Instrument.resolutionTable.[name] <- AssemblyDefinition.ReadAssembly path)
             () false))

        ("k|key=",
         (fun x ->
           let (pair, ok) = CommandLine.validateStrongNameKey "--key" x
           if ok then CoverageParameters.add pair))
        ("sn|strongNameKey=",
         (fun x ->
           let (pair, ok) = CommandLine.validateStrongNameKey "--strongNameKey" x
           if ok then
             if Option.isSome CoverageParameters.defaultStrongNameKey then
               CommandLine.error <-
                 CommandLine.Format.Local("MultiplesNotAllowed", "--strongNameKey")
                 :: CommandLine.error
             else
               CoverageParameters.defaultStrongNameKey <- Some pair
               CoverageParameters.add pair))

        ("x|xmlReport=",
         (fun x ->
           if CommandLine.validatePath "--xmlReport" x then
             if Option.isSome CoverageParameters.theReportPath then
               CommandLine.error <-
                 CommandLine.Format.Local("MultiplesNotAllowed", "--xmlReport")
                 :: CommandLine.error
             else
               CommandLine.doPathOperation
                 (fun () -> CoverageParameters.theReportPath <- Some(Path.GetFullPath x)) () false))
        ("f|fileFilter=", makeFilter FilterScope.File)
        ("p|pathFilter=", makeFilter FilterScope.Path)
        ("s|assemblyFilter=", makeFilter FilterScope.Assembly)
        ("e|assemblyExcludeFilter=", makeFilter FilterScope.Module)
        ("t|typeFilter=", makeFilter FilterScope.Type)
        ("m|methodFilter=", makeFilter FilterScope.Method)
        ("a|attributeFilter=", makeFilter FilterScope.Attribute)
        ("attributetoplevel=", makeTopLevel FilterScope.Attribute)
        ("typetoplevel=", makeTopLevel FilterScope.Type)
        ("methodtoplevel=", makeTopLevel FilterScope.Method)
        (CommandLine.ddFlag "l|localSource" CoverageParameters.local)
        ("c|callContext=",
         (fun x ->
           if CoverageParameters.single then
             CommandLine.error <-
               CommandLine.Format.Local("Incompatible",
                  "--single", "--callContext") :: CommandLine.error
           else
             let (ok, selection) = validateCallContext (Option.isSome CoverageParameters.theInterval) x
             if ok then
               match selection with
               | Left n -> CoverageParameters.theInterval <- n
               | Right name -> CoverageParameters.trackingNames.Add(name)))
        ("reportFormat=",
         (fun x ->
           if Option.isSome CoverageParameters.theReportFormat then
             CommandLine.error <-
               CommandLine.Format.Local("MultiplesNotAllowed", "--reportFormat")
               :: CommandLine.error
           else
             CoverageParameters.theReportFormat <- Some (match x with
                                                         | Select "NCover" _ -> ReportFormat.NCover
                                                         | _ ->  ReportFormat.OpenCover)))
        (CommandLine.ddFlag "inplace" CoverageParameters.inplace)
        (CommandLine.ddFlag "save" CoverageParameters.collect)
        (CommandLine.ddFlag "zipfile" CoverageParameters.zipReport)
        (CommandLine.ddFlag "methodpoint" CoverageParameters.methodPoint)
        ("single",
         (fun _ ->
           if CoverageParameters.single then
             CommandLine.error <-
               CommandLine.Format.Local("MultiplesNotAllowed", "--single")
               :: CommandLine.error
           else if Option.isSome CoverageParameters.theInterval || CoverageParameters.trackingNames.Any() then
             CommandLine.error <-
               CommandLine.Format.Local("Incompatible",
                  "--single", "--callContext") :: CommandLine.error
           else
             CoverageParameters.single <- true))
        ("linecover",
         (fun _ ->
           match CoverageParameters.coverstyle with
           | CoverStyle.LineOnly ->
               CommandLine.error <-
                 CommandLine.Format.Local("MultiplesNotAllowed", "--linecover")
                 :: CommandLine.error
           | CoverStyle.BranchOnly ->
               CommandLine.error <-
                 CommandLine.Format.Local("Incompatible", "--linecover",
                    "--branchcover") :: CommandLine.error
           | _ -> CoverageParameters.coverstyle <- CoverStyle.LineOnly))
        ("branchcover",
         (fun _ ->
           match CoverageParameters.coverstyle with
           | CoverStyle.BranchOnly ->
               CommandLine.error <-
                 CommandLine.Format.Local("MultiplesNotAllowed", "--branchcover")
                 :: CommandLine.error
           | CoverStyle.LineOnly ->
               CommandLine.error <-
                 CommandLine.Format.Local("Incompatible", "--branchcover",
                    "--linecover") :: CommandLine.error
           | _ -> CoverageParameters.coverstyle <- CoverStyle.BranchOnly))
        (CommandLine.ddFlag "dropReturnCode" CommandLine.dropReturnCode)
        (CommandLine.ddFlag "sourcelink" CoverageParameters.sourcelink)
        (CommandLine.ddFlag "defer" CoverageParameters.defer)
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
                 CommandLine.Format.Local("InvalidValue", "--showstatic", x)
                 :: CommandLine.error
           else
             CommandLine.error <-
               CommandLine.Format.Local("MultiplesNotAllowed", "--showstatic")
               :: CommandLine.error))
        (CommandLine.ddFlag "showGenerated" CoverageParameters.showGenerated)
        ("?|help|h", (fun x -> CommandLine.help <- x.IsNotNull))

        ("<>",
         (fun x ->
           CommandLine.error <-
             CommandLine.Format.Local("InvalidValue",
                "AltCover", x) :: CommandLine.error)) ] // default end stop
      |> List.fold
           (fun (o : OptionSet) (p, a) ->
             o.Add(p, CommandLine.resources.GetString(p), new System.Action<string>(a)))
           (OptionSet())

    let internal processOutputLocation(action : Either<string * OptionSet, string list * OptionSet>) =
      match action with
      | Right(rest, options) ->
          // Check that the directories are distinct
          let fromDirectories = CoverageParameters.inputDirectories()
          let toDirectories = CoverageParameters.outputDirectories()
          fromDirectories
          |> Seq.iter (fun fromDirectory ->
               if toDirectories.Contains fromDirectory then
                 CommandLine.error <-
                   CommandLine.Format.Local("NotInPlace", fromDirectory)
                   :: CommandLine.error)

          CommandLine.doPathOperation (fun () ->
            let found = toDirectories |> Seq.filter Directory.Exists
            if !CoverageParameters.inplace && CommandLine.error |> List.isEmpty && found.Any() then
              found
              |> Seq.iter (fun toDirectory ->
                   CommandLine.error <-
                     CommandLine.Format.Local("SaveExists", toDirectory)
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
                   <| CommandLine.Format.Local("savingto", toDirectory)
                   Output.info
                   <| CommandLine.Format.Local("instrumentingin", fromDirectory)
                  else
                   Output.info
                   <| CommandLine.Format.Local("instrumentingfrom",
                       fromDirectory)
                   Output.info
                   <| CommandLine.Format.Local("instrumentingto", toDirectory))
            Right
              (rest, fromDirectories |> Seq.map DirectoryInfo,
               toDirectories |> Seq.map DirectoryInfo,
               CoverageParameters.sourceDirectories() |> Seq.map DirectoryInfo)
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
                      CommandLine.Format.Local("instrumenting", fullName)
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

    let internal isMSBuild (assembly : Assembly option) =
      assembly
      |> Option.map (fun a -> Path.GetFileName(a.Location).Equals("MSBuild.dll"))
      |> Option.defaultValue false

    [<SuppressMessage("Gendarme.Rules.BadPractice",
      "GetEntryAssemblyMayReturnNullRule",
      Justification="That is the whole point of the call.")>]
    let internal doInstrumentation arguments =
      let dotnetBuild =
        Assembly.GetEntryAssembly() // is null for unit tests
        |> Option.ofObj
        |> isMSBuild

      let check1 =
        declareOptions()
        |> CommandLine.parseCommandLine arguments
        |> CommandLine.processHelpOption
        |> processOutputLocation
      match check1 with
      | Left(intro, options) ->
          CommandLine.handleBadArguments dotnetBuild arguments
            { Intro = intro
              Options = options
              Options2 = Runner.declareOptions() }
          255
      | Right(rest, fromInfo, toInfo, targetInfo) ->
          let report = CoverageParameters.reportPath()

          let result =
            CommandLine.doPathOperation (fun () ->
              report
              |> Path.GetDirectoryName
              |> CommandLine.ensureDirectory
              let (assemblies, assemblyNames) =
                prepareTargetFiles fromInfo toInfo targetInfo
                  (CoverageParameters.instrumentDirectories())
              Output.info
              <| CommandLine.Format.Local("reportingto", report)
              let reporter, document =
                match CoverageParameters.reportKind() with
                | ReportFormat.OpenCover -> OpenCover.reportGenerator()
                | _ -> Report.reportGenerator()

              let visitors =
                [ reporter
                  Instrument.instrumentGenerator assemblyNames ]

              Visitor.visit visitors assemblies
              Zip.save document report !CoverageParameters.zipReport
              if !CoverageParameters.collect then Runner.setRecordToFile report
              CommandLine.processTrailingArguments rest (toInfo |> Seq.head)) 255 true
          CommandLine.reportErrors "Instrumentation" (dotnetBuild && !CoverageParameters.inplace)
          result

    let internal main arguments =
      let first =
        arguments
        |> Seq.tryHead
        |> Option.defaultValue String.Empty
      init()
      match first with
      | Select "Runner" _ ->
          Runner.init()
          Runner.doCoverage arguments (declareOptions())
      | Select "ImportModule" _ ->
          let here = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
          let parent = here |> Path.GetDirectoryName
          Directory.GetDirectories(parent)
          |> Seq.sort
          |> Seq.collect (fun d -> Directory.GetFiles(d,
                                                      "AltCover.PowerShell.dll",
                                                      SearchOption.TopDirectoryOnly))
          |> Seq.tryHead
          |> Option.map (sprintf "Import-Module %A")
          |> Option.iter Output.info
          0
      | Select "version" _ ->
          CommandLine.writeResourceWithFormatItems "AltCover.Version"
            [| AssemblyVersionInformation.AssemblyFileVersion |] false
          0
      | _ -> doInstrumentation arguments

  // mocking point
  [<System.Diagnostics.CodeAnalysis.SuppressMessage(
      "Gendarme.Rules.Performance", "AvoidUncalledPrivateCodeRule",
      Justification = "Unit test accessor")>]
  let mutable internal effectiveMain = I.main