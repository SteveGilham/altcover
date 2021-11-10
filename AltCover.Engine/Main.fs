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
  { Path: string list
    Name: string
    Refs: string list }

module internal Main =
  let internal (|Select|_|) (pattern: String) offered =
    if
      offered |> String.IsNullOrWhiteSpace |> not
      && pattern.StartsWith
        (
          offered,
          StringComparison.OrdinalIgnoreCase
        )
    then
      Some offered
    else
      None

  let internal init () =
    CommandLine.verbosity <- 0
    CommandLine.error <- []
    CommandLine.dropReturnCode.Value <- false // ddFlag
    CoverageParameters.defer.Value <- false // ddflag
    CoverageParameters.theInputDirectories.Clear()
    CoverageParameters.theOutputDirectories.Clear()
    ProgramDatabase.symbolFolders.Clear()
    Instrument.resolutionTable.Clear()

    CoverageParameters.keys.Clear()
    CoverageParameters.defaultStrongNameKey <- None

    use stream =
      Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("AltCover.Recorder.snk")

    use buffer = new MemoryStream()
    stream.CopyTo(buffer)
    let snk = StrongNameKeyData.Make(buffer.ToArray())
    CoverageParameters.add snk
    CoverageParameters.recorderStrongNameKey <- Some(snk)

    CoverageParameters.theReportPath <- None
    CoverageParameters.zipReport.Value <- false
    CoverageParameters.methodPoint.Value <- false
    CoverageParameters.nameFilters.Clear()
    CoverageParameters.theInterval <- None
    CoverageParameters.trackingNames.Clear()
    CoverageParameters.topLevel.Clear()
    CoverageParameters.theReportFormat <- None
    CoverageParameters.inplace.Value <- false // ddFlag
    CoverageParameters.collect.Value <- false // ddFlag
    CoverageParameters.local.Value <- false // ddFlag
    CoverageParameters.single <- false // more complicated
    CoverageParameters.coverstyle <- CoverStyle.All
    CoverageParameters.sourcelink.Value <- false // ddFlag
    CoverageParameters.coalesceBranches.Value <- false // ddFlag
    CoverageParameters.staticFilter <- None
    CoverageParameters.showGenerated.Value <- false

  let internal validateCallContext predicate x =
    if not (String.IsNullOrWhiteSpace x) then
      let k = x.Trim()

      if Char.IsDigit <| k.Chars(0) then
        if predicate || k.Length > 1 then
          CommandLine.error <-
            CommandLine.Format.Local(
              (if predicate then
                 "MultiplesNotAllowed"
               else
                 "InvalidValue"),
              "--callContext",
              x
            )
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
        String.Format(
          CultureInfo.CurrentCulture,
          CommandLine.resources.GetString "InvalidValue",
          "--callContext",
          x
        )
        :: CommandLine.error

      (false, Left None)

  module internal I =
    let internal declareOptions () =
      let makeRegex (x: String) =
        x.Replace(char 0, '\\').Replace(char 1, '|')
        |> CommandLine.validateRegexes

      let makeFilter filterscope =
        makeRegex
        >> Seq.map (FilterClass.Build filterscope)
        >> CoverageParameters.nameFilters.AddRange

      let makeTopLevel filterscope =
        makeRegex
        >> (Seq.map (FilterClass.Build filterscope))
        >> CoverageParameters.topLevel.AddRange

      [ ("i|inputDirectory=",
         (fun x ->
           if CommandLine.validateDirectory "--inputDirectory" x then
             let arg = canonicalDirectory x

             if CoverageParameters.theInputDirectories.Contains arg then
               CommandLine.error <-
                 CommandLine.Format.Local("DuplicatesNotAllowed", arg, "--inputDirectory")
                 :: CommandLine.error
             else
               CoverageParameters.theInputDirectories.Add arg))

        ("o|outputDirectory=",
         (fun x ->
           if CommandLine.validatePath "--outputDirectory" x then
             let arg = canonicalDirectory x

             if CoverageParameters.theOutputDirectories.Contains arg then
               CommandLine.error <-
                 CommandLine.Format.Local(
                   "DuplicatesNotAllowed",
                   arg,
                   "--outputDirectory"
                 )
                 :: CommandLine.error
             else
               CommandLine.doPathOperation
                 (fun _ -> CoverageParameters.theOutputDirectories.Add arg)
                 ()
                 false))

        ("y|symbolDirectory=",
         (fun x ->
           if CommandLine.validateDirectory "--symbolDirectory" x then
             ProgramDatabase.symbolFolders.Add x))
        ("d|dependency=",
         (fun x ->
           CommandLine.doPathOperation
             (fun _ ->
               let path =
                 x
                 |> Environment.ExpandEnvironmentVariables
                 |> canonicalPath

               let name, ok =
                 CommandLine.validateAssembly "--dependency" path

               if ok then
                 Instrument.resolutionTable.[name] <- AssemblyDefinition.ReadAssembly path)
             ()
             false))

        ("k|key=",
         (fun x ->
           let (pair, ok) =
             CommandLine.validateStrongNameKey "--key" x

           if ok then CoverageParameters.add pair))
        ("sn|strongNameKey=",
         (fun x ->
           let (pair, ok) =
             CommandLine.validateStrongNameKey "--strongNameKey" x

           if ok then
             if Option.isSome CoverageParameters.defaultStrongNameKey then
               CommandLine.error <-
                 CommandLine.Format.Local("MultiplesNotAllowed", "--strongNameKey")
                 :: CommandLine.error
             else
               CoverageParameters.defaultStrongNameKey <- Some pair
               CoverageParameters.add pair))

        ("r|report=",
         (fun x ->
           if CommandLine.validatePath "--report" x then
             if Option.isSome CoverageParameters.theReportPath then
               CommandLine.error <-
                 CommandLine.Format.Local("MultiplesNotAllowed", "--report")
                 :: CommandLine.error
             else
               CommandLine.doPathOperation
                 (fun () -> CoverageParameters.theReportPath <- Some(canonicalPath x))
                 ()
                 false))
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
           let (ok, selection) =
             validateCallContext (Option.isSome CoverageParameters.theInterval) x

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
             CoverageParameters.theReportFormat <-
               Some(
                 match x with
                 | Select "NCover" _ -> ReportFormat.NCover
                 | Select "Json" _ -> ReportFormat.NativeJson
                 | _ -> ReportFormat.OpenCover
               )))
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
                 CommandLine.Format.Local("Incompatible", "--linecover", "--branchcover")
                 :: CommandLine.error
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
                 CommandLine.Format.Local("Incompatible", "--branchcover", "--linecover")
                 :: CommandLine.error
           | _ -> CoverageParameters.coverstyle <- CoverStyle.BranchOnly))
        (CommandLine.ddFlag "dropReturnCode" CommandLine.dropReturnCode)
        (CommandLine.ddFlag "sourcelink" CoverageParameters.sourcelink)
        (CommandLine.ddFlag "defer" CoverageParameters.defer)
        (CommandLine.ddFlag "v|visibleBranches" CoverageParameters.coalesceBranches)
        ("showstatic:",
         (fun x ->
           if CoverageParameters.staticFilter = None then
             CoverageParameters.staticFilter <-
               if String.IsNullOrWhiteSpace x || x = "+" then
                 Some StaticFilter.AsCovered
               else if x = "++" then
                 Some StaticFilter.NoFilter
               else if x = "-" then
                 Some StaticFilter.Hidden
               else
                 None

             if CoverageParameters.staticFilter = None then
               CommandLine.error <-
                 CommandLine.Format.Local("InvalidValue", "--showstatic", x)
                 :: CommandLine.error
           else
             CommandLine.error <-
               CommandLine.Format.Local("MultiplesNotAllowed", "--showstatic")
               :: CommandLine.error))
        (CommandLine.ddFlag "showGenerated" CoverageParameters.showGenerated)
        ("q", (fun _ -> CommandLine.verbosity <- CommandLine.verbosity + 1))
        ("?|help|h", (fun x -> CommandLine.help <- x.IsNotNull))

        ("<>",
         (fun x ->
           CommandLine.error <-
             CommandLine.Format.Local("InvalidValue", "AltCover", x)
             :: CommandLine.error)) ] // default end stop
      |> List.fold
           (fun (o: OptionSet) (p, a) ->
             o.Add(p, CommandLine.resources.GetString(p), new System.Action<string>(a)))
           (OptionSet())

    let private echoDirectories (outputDirectory:string, inputDirectory:string) =
      if CommandLine.verbosity < 1 // implement it early here
      then
        if CoverageParameters.inplace.Value then
          Output.info
          <| CommandLine.Format.Local("savingto", outputDirectory)

          Output.info
          <| CommandLine.Format.Local("instrumentingin", inputDirectory)
        else
          Output.info
          <| CommandLine.Format.Local("instrumentingfrom", inputDirectory)

          Output.info
          <| CommandLine.Format.Local("instrumentingto", outputDirectory)

    let internal processOutputLocation
      (action: Either<string * OptionSet, string list * OptionSet>)
      =
      match action with
      | Right (rest, options) ->
          // Check that the directories are distinct
          let inputDirectories = CoverageParameters.inputDirectories ()
          let outputDirectories = CoverageParameters.outputDirectories ()

          inputDirectories
          |> Seq.iter
               (fun fromDirectory ->
                 if outputDirectories.Contains fromDirectory then
                   CommandLine.error <-
                     CommandLine.Format.Local("NotInPlace", fromDirectory)
                     :: CommandLine.error)

          CommandLine.doPathOperation
            (fun () ->
              let found =
                outputDirectories |> Seq.filter Directory.Exists

              if CoverageParameters.inplace.Value
                 && CommandLine.error |> List.isEmpty
                 && found.Any() then
                found
                |> Seq.iter
                     (fun toDirectory ->
                       CommandLine.error <-
                         CommandLine.Format.Local("SaveExists", toDirectory)
                         :: CommandLine.error)

              if CommandLine.error |> List.isEmpty then
                (Seq.iter CommandLine.ensureDirectory outputDirectories))
            ()
            false

          if CommandLine.error |> List.isEmpty |> not then
            Left("UsageError", options)
          else
            Seq.zip outputDirectories inputDirectories
            |> Seq.iter echoDirectories

            Right(
              rest,
              inputDirectories |> Seq.map DirectoryInfo,
              outputDirectories |> Seq.map DirectoryInfo,
              CoverageParameters.sourceDirectories () // "instrument-from" selection of the above
              |> Seq.map DirectoryInfo
            )
      | Left intro -> Left intro

    let internal imageLoadResilient (f: unit -> 'a) (tidy: unit -> 'a) =
      try
        f ()
      with
      | :? Mono.Cecil.Cil.SymbolsNotMatchingException
      | :? BadImageFormatException
      | :? ArgumentException
      | :? IOException -> tidy ()

    let internal matchType = Maybe (System.Environment.GetEnvironmentVariable("OS") = "Windows_NT")
                                    StringComparison.OrdinalIgnoreCase StringComparison.Ordinal

    let internal isInDirectory (file:string) (dir:string) =
      file.StartsWith(dir, matchType)

    let internal prepareTargetFiles
      (inputInfos: DirectoryInfo seq)
      (outputInfos: DirectoryInfo seq)
      (instrumentFromInfos: DirectoryInfo seq)
      (instrumentToPaths: string seq)
      =
      // Copy all the files into the target directory
      let mapping = Dictionary<string, string>()

      Seq.zip instrumentFromInfos instrumentToPaths
      |> Seq.map
           (fun (x, y) ->
             let f = x.FullName // trim separator
             (Path.Combine(f |> Path.GetDirectoryName, f |> Path.GetFileName), y))
      |> Seq.iter mapping.Add

      Seq.zip inputInfos outputInfos
      |> Seq.iter
           (fun (inputInfo, outputInfo) ->
             // recurse here

             let files = inputInfo.GetFiles("*",SearchOption.AllDirectories)
                         |> Seq.filter (fun i -> outputInfos
                                                 |> Seq.exists(fun o -> isInDirectory i.FullName o.FullName)
                                                 |> not)

             files
             |> Seq.iter
                  (fun info ->
                    let fullName = info.FullName
                    let dirname = info.DirectoryName
                    let reldir =
                      Visitor.I.getRelativeDirectoryPath inputInfo.FullName dirname
                    let copy = Path.Combine(outputInfo.FullName, reldir, info.Name)
                    copy
                    |> Path.GetDirectoryName
                    |> Directory.CreateDirectory
                    |> ignore
                    File.Copy(fullName, copy, true)))

      // Track the symbol-bearing assemblies
      let assemblies =
        instrumentFromInfos
        |> Seq.map
             (fun sourceInfo ->
               sourceInfo.GetFiles()
               |> Seq.fold
                    (fun (accumulator: AssemblyInfo list) info ->
                      let fullName = info.FullName

                      imageLoadResilient
                        (fun () ->
                          use stream = File.OpenRead(fullName)
                          use def = AssemblyDefinition.ReadAssembly(stream)
                          ProgramDatabase.readSymbols def

                          if def.MainModule.HasSymbols
                             && (def.IsIncluded).IsInstrumented
                             && (def.MainModule.Attributes
                                 &&& ModuleAttributes.ILOnly = ModuleAttributes.ILOnly) then
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
                            accumulator)
                        (fun () -> accumulator))
                    [])
        |> Seq.toList
        |> Seq.concat
        |> Seq.groupBy (fun a -> a.Name) // assume name is unique
        |> Seq.map
             (fun (n, agroup) ->
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
        |> Seq.fold (fun (s: Set<string>) n -> Set.add n s) Set.empty<string>

      let simplified =
        assemblies
        |> List.map
             (fun a ->
               { a with
                   Refs =
                     a.Refs
                     |> List.filter (fun n -> Set.contains n candidates) })

      let rec bundle unassigned unresolved collection n =
        match unassigned with
        | [] -> collection
        | _ ->
            let stage =
              (if n <= 1 then
                 unassigned
               else
                 unassigned
                 |> List.filter (fun u -> u.Refs |> List.isEmpty))
              |> List.sortBy (fun u -> u.Name)

            let waiting =
              stage
              |> List.fold (fun s a -> Set.remove a.Name s) unresolved

            let next =
              unassigned
              |> List.filter (fun u -> u.Refs |> List.isEmpty |> not)
              |> List.map
                   (fun a ->
                     { a with
                         Refs =
                           a.Refs
                           |> List.filter (fun n -> Set.contains n waiting) })

            bundle next waiting (stage :: collection) (n - 1)

      let sorted =
        bundle simplified candidates [] (simplified |> List.length)
        |> List.concat
        |> List.rev
        |> List.map
             (fun a ->
               let proto = a.Path.Head

               let targets =
                 a.Path
                 |> List.map (Path.GetDirectoryName >> (fun d -> mapping.[d]))

               ({ AssemblyPath = proto
                  Destinations = targets },
                a.Name))

      List.unzip sorted

    let internal isMSBuild (assembly: Assembly option) =
      assembly
      |> Option.map (fun a -> Path.GetFileName(a.Location).Equals("MSBuild.dll"))
      |> Option.defaultValue false

    let internal selectReportGenerator () =
      match CoverageParameters.reportKind () with
      | ReportFormat.OpenCoverWithTracking
      | ReportFormat.OpenCover -> OpenCover.reportGenerator ()
      | ReportFormat.NativeJsonWithTracking
      | ReportFormat.NativeJson -> NativeJson.reportGenerator ()
      | _ -> Report.reportGenerator ()

    [<SuppressMessage("Gendarme.Rules.BadPractice",
                      "GetEntryAssemblyMayReturnNullRule",
                      Justification = "That is the whole point of the call.")>]
    let internal doInstrumentation arguments =
      let dotnetBuild =
        Assembly.GetEntryAssembly() // is null for unit tests
        |> Option.ofObj
        |> isMSBuild

      let check1 =
        declareOptions ()
        |> CommandLine.parseCommandLine arguments
        |> CommandLine.processHelpOption
        |> processOutputLocation

      match check1 with
      | Left (intro, options) ->
          CommandLine.handleBadArguments
            dotnetBuild
            arguments
            { Intro = intro
              Options = options
              Options2 = Runner.declareOptions () }

          255
      | Right (rest, fromInfo, toInfo, targetInfo) ->
          CommandLine.applyVerbosity ()

          let report = CoverageParameters.reportPath ()

          let result =
            CommandLine.doPathOperation
              (fun () ->
                report
                |> Path.GetDirectoryName
                |> CommandLine.ensureDirectory

                let (assemblies, assemblyNames) =
                  prepareTargetFiles
                    fromInfo
                    toInfo
                    targetInfo
                    (CoverageParameters.instrumentDirectories ())

                Output.info
                <| CommandLine.Format.Local("reportingto", report)

                let reporter, document = selectReportGenerator ()

                let visitors =
                  [ reporter
                    Instrument.instrumentGenerator assemblyNames ]

                Visitor.visit visitors assemblies
                Zip.save document report CoverageParameters.zipReport.Value

                if CoverageParameters.collect.Value then
                  Runner.setRecordToFile report

                CommandLine.processTrailingArguments rest (toInfo |> Seq.head))
              255
              true

          CommandLine.reportErrors
            "Instrumentation"
            (dotnetBuild && CoverageParameters.inplace.Value)

          result

    let internal main arguments =
      let first =
        arguments
        |> Seq.tryHead
        |> Option.defaultValue String.Empty

      init ()

      match first with
      | Select "Runner" _ ->
          Runner.init ()
          Runner.doCoverage arguments (declareOptions ())
      | Select "ImportModule" _ ->
          let here =
            Assembly.GetExecutingAssembly().Location
            |> Path.GetDirectoryName

          [ "../netcoreapp2.0"
            "../netstandard2.0"
            "../any" ]
          |> Seq.map
               (fun d ->
                 canonicalPath(
                   Path.Combine(Path.Combine(here, d), "AltCover.PowerShell.dll")
                 ))
          |> Seq.filter File.Exists
          |> Seq.tryHead
          |> Option.map (sprintf "Import-Module %A")
          |> Option.iter Output.info

          0
      | Select "version" _ ->
          CommandLine.writeResourceWithFormatItems
            "AltCover.Version"
            [| AssemblyVersionInformation.AssemblyFileVersion |]
            false

          0
      | _ -> doInstrumentation arguments

  // mocking point
  [<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Performance",
                                                    "AvoidUncalledPrivateCodeRule",
                                                    Justification = "Unit test accessor")>]
  let mutable internal effectiveMain = I.main