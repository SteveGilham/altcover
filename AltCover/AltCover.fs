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
  { Path: string list
    Name: string
    Refs: string list }

module internal Main =
  let init() =
    CommandLine.error <- []
    CommandLine.dropReturnCode := false
    Visitor.defer := None
    Visitor.inputDirectories.Clear()
    Visitor.outputDirectories.Clear()
    ProgramDatabase.SymbolFolders.Clear()
    Instrument.ResolutionTable.Clear()

    Visitor.keys.Clear()
    Visitor.defaultStrongNameKey <- None
    use stream =
      Assembly.GetExecutingAssembly().GetManifestResourceStream("AltCover.Recorder.snk")
    use buffer = new MemoryStream()
    stream.CopyTo(buffer)
    let snk = StrongNameKeyData.Make(buffer.ToArray())
    Visitor.Add snk
    Visitor.recorderStrongNameKey <- Some(snk)

    Visitor.reportPath <- None
    Visitor.NameFilters.Clear()
    Visitor.interval <- None
    Visitor.TrackingNames.Clear()
    Visitor.reportFormat <- None
    Visitor.inplace := false
    Visitor.collect := false
    Visitor.single <- false
    Visitor.coverstyle <- CoverStyle.All
    Visitor.sourcelink := false

  let ValidateCallContext predicate x =
    if not (String.IsNullOrWhiteSpace x) then
      let k = x.Trim()
      if Char.IsDigit <| k.Chars(0) then
        if predicate || k.Length > 1 then
          CommandLine.error <-
            String.Format
              (CultureInfo.CurrentCulture, CommandLine.resources.GetString "InvalidValue",
               "--callContext", x) :: CommandLine.error
          (false, Left None)
        else
          let (ok, n) = Int32.TryParse(k)
          if ok then (ok, Left(Some(pown 10 (7 - n))))
          else
            CommandLine.error <-
              String.Format
                (CultureInfo.CurrentCulture,
                 CommandLine.resources.GetString "InvalidValue", "--callContext", x)
              :: CommandLine.error
            (false, Left None)
      else (true, Right k)
    else
      CommandLine.error <-
        String.Format
          (CultureInfo.CurrentCulture, CommandLine.resources.GetString "InvalidValue",
           "--callContext", x) :: CommandLine.error
      (false, Left None)

  let internal DeclareOptions() =
    let makeFilter filterclass (x: String) =
      x.Replace('\u0000', '\\').Replace('\u0001','|')
      |> CommandLine.ValidateRegexes
      |> Seq.iter (filterclass >> Visitor.NameFilters.Add)

    [ ("i|inputDirectory=",
       (fun x ->
       if CommandLine.ValidateDirectory "--inputDirectory" x then
         let arg = Path.GetFullPath x
         if Visitor.inputDirectories.Contains arg then
           CommandLine.error <-
             String.Format
               (CultureInfo.CurrentCulture,
                CommandLine.resources.GetString "DuplicatesNotAllowed", arg,
                "--inputDirectory") :: CommandLine.error
         else Visitor.inputDirectories.Add arg))

      ("o|outputDirectory=",
       (fun x ->
       if CommandLine.ValidatePath "--outputDirectory" x then
         let arg = Path.GetFullPath x
         if Visitor.outputDirectories.Contains arg then
           CommandLine.error <-
             String.Format
               (CultureInfo.CurrentCulture,
                CommandLine.resources.GetString "DuplicatesNotAllowed", arg,
                "--outputDirectory") :: CommandLine.error
         else
           CommandLine.doPathOperation (fun _ -> Visitor.outputDirectories.Add arg) ()
             false))

      ("y|symbolDirectory=",
       (fun x ->
       if CommandLine.ValidateDirectory "--symbolDirectory" x then
         ProgramDatabase.SymbolFolders.Add x))
      ("d|dependency=",
       (fun x ->
       CommandLine.doPathOperation
         (fun _ ->
         let path =
           x
           |> Environment.ExpandEnvironmentVariables
           |> Path.GetFullPath

         let name, ok = CommandLine.ValidateAssembly "--dependency" path
         if ok then
           Instrument.ResolutionTable.[name] <- AssemblyDefinition.ReadAssembly path) ()
         false))

      ("k|key=",
       (fun x ->
       let (pair, ok) = CommandLine.ValidateStrongNameKey "--key" x
       if ok then Visitor.Add pair))
      ("sn|strongNameKey=",
       (fun x ->
       let (pair, ok) = CommandLine.ValidateStrongNameKey "--strongNameKey" x
       if ok then
         if Option.isSome Visitor.defaultStrongNameKey then
           CommandLine.error <-
             String.Format
               (CultureInfo.CurrentCulture,
                CommandLine.resources.GetString "MultiplesNotAllowed", "--strongNameKey")
             :: CommandLine.error
         else
           Visitor.defaultStrongNameKey <- Some pair
           Visitor.Add pair))

      ("x|xmlReport=",
       (fun x ->
       if CommandLine.ValidatePath "--xmlReport" x then
         if Option.isSome Visitor.reportPath then
           CommandLine.error <-
             String.Format
               (CultureInfo.CurrentCulture,
                CommandLine.resources.GetString "MultiplesNotAllowed", "--xmlReport")
             :: CommandLine.error
         else
           CommandLine.doPathOperation
             (fun () -> Visitor.reportPath <- Some(Path.GetFullPath x)) () false))
      ("f|fileFilter=", makeFilter FilterClass.File)
      ("p|pathFilter=", makeFilter FilterClass.Path)
      ("s|assemblyFilter=", makeFilter FilterClass.Assembly)
      ("e|assemblyExcludeFilter=", makeFilter FilterClass.Module)
      ("t|typeFilter=", makeFilter FilterClass.Type)
      ("m|methodFilter=", makeFilter FilterClass.Method)
      ("a|attributeFilter=", makeFilter FilterClass.Attribute)
      ("c|callContext=",
       (fun x ->
       if Visitor.single then
         CommandLine.error <-
           String.Format
             (CultureInfo.CurrentCulture, CommandLine.resources.GetString "Incompatible",
              "--single", "--callContext") :: CommandLine.error
       else
         let (ok, selection) = ValidateCallContext (Option.isSome Visitor.interval) x
         if ok then
           match selection with
           | Left n -> Visitor.interval <- n
           | Right name -> Visitor.TrackingNames.Add(name)))
      ("opencover",
       (fun _ ->
       if Option.isSome Visitor.reportFormat then
         CommandLine.error <-
           String.Format
             (CultureInfo.CurrentCulture,
              CommandLine.resources.GetString "MultiplesNotAllowed", "--opencover")
           :: CommandLine.error
       else Visitor.reportFormat <- Some ReportFormat.OpenCover))
      (CommandLine.ddFlag "inplace" Visitor.inplace)
      (CommandLine.ddFlag "save" Visitor.collect)
      ("single",
       (fun _ ->
       if Visitor.single then
         CommandLine.error <-
           String.Format
             (CultureInfo.CurrentCulture,
              CommandLine.resources.GetString "MultiplesNotAllowed", "--single")
           :: CommandLine.error
       else if Option.isSome Visitor.interval || Visitor.TrackingNames.Any() then
         CommandLine.error <-
           String.Format
             (CultureInfo.CurrentCulture, CommandLine.resources.GetString "Incompatible",
              "--single", "--callContext") :: CommandLine.error
       else Visitor.single <- true))
      ("linecover",
       (fun _ ->
       match Visitor.coverstyle with
       | CoverStyle.LineOnly ->
         CommandLine.error <-
           String.Format
             (CultureInfo.CurrentCulture,
              CommandLine.resources.GetString "MultiplesNotAllowed", "--linecover")
           :: CommandLine.error
       | CoverStyle.BranchOnly ->
         CommandLine.error <-
           String.Format
             (CultureInfo.CurrentCulture, CommandLine.resources.GetString "Incompatible",
              "--linecover", "--branchcover") :: CommandLine.error
       | _ -> Visitor.coverstyle <- CoverStyle.LineOnly))
      ("branchcover",
       (fun _ ->
       match Visitor.coverstyle with
       | CoverStyle.BranchOnly ->
         CommandLine.error <-
           String.Format
             (CultureInfo.CurrentCulture,
              CommandLine.resources.GetString "MultiplesNotAllowed", "--branchcover")
           :: CommandLine.error
       | CoverStyle.LineOnly ->
         CommandLine.error <-
           String.Format
             (CultureInfo.CurrentCulture, CommandLine.resources.GetString "Incompatible",
              "--branchcover", "--linecover") :: CommandLine.error
       | _ -> Visitor.coverstyle <- CoverStyle.BranchOnly))
      (CommandLine.ddFlag "dropReturnCode" CommandLine.dropReturnCode)
      (CommandLine.ddFlag "sourcelink" Visitor.sourcelink)
      ("defer:",
       (fun x ->
         if !Visitor.defer = None then
           Visitor.defer := if String.IsNullOrWhiteSpace x then Some true
                            else
                              let (|Select|_|) (pattern: String) offered =
                                if offered
                                   |> String.IsNullOrWhiteSpace
                                   |> not
                                   && pattern.Equals
                                        (offered, StringComparison.OrdinalIgnoreCase)
                                then Some offered
                                else None
                              match x with
                              | Select "-" _ -> Some false
                              | Select "+" _ -> Some true
                              | _ -> None
           if !Visitor.defer = None then
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
      ("?|help|h", (fun x -> CommandLine.help <- not (isNull x)))

      ("<>",
       (fun x ->
       CommandLine.error <-
         String.Format
           (CultureInfo.CurrentCulture, CommandLine.resources.GetString "InvalidValue",
            "AltCover", x) :: CommandLine.error)) ] // default end stop
    |> List.fold
         (fun (o: OptionSet) (p, a) ->
         o.Add(p, CommandLine.resources.GetString(p), new System.Action<string>(a)))
         (OptionSet())

  let internal ProcessOutputLocation(action: Either<string * OptionSet, string list * OptionSet>) =
    match action with
    | Right(rest, options) ->
      // Check that the directories are distinct
      let fromDirectories = Visitor.InputDirectories()
      let toDirectories = Visitor.OutputDirectories()
      fromDirectories
      |> Seq.iter
           (fun fromDirectory ->
           if toDirectories.Contains fromDirectory then
             CommandLine.error <-
               String.Format
                 (CultureInfo.CurrentCulture, CommandLine.resources.GetString "NotInPlace",
                  fromDirectory) :: CommandLine.error)

      CommandLine.doPathOperation
        (fun () ->
        let found = toDirectories |> Seq.filter Directory.Exists
        if !Visitor.inplace && CommandLine.error |> List.isEmpty && found.Any() then
          found
          |> Seq.iter
               (fun toDirectory ->
               CommandLine.error <-
                 String.Format
                   (CultureInfo.CurrentCulture,
                    CommandLine.resources.GetString "SaveExists", toDirectory)
                 :: CommandLine.error)
        if CommandLine.error |> List.isEmpty then
          (Seq.iter CommandLine.ensureDirectory toDirectories)) () false
      if CommandLine.error
         |> List.isEmpty
         |> not
      then Left("UsageError", options)
      else
        Seq.zip toDirectories fromDirectories
        |> Seq.iter
             (fun (toDirectory, fromDirectory) ->
             if !Visitor.inplace then
               Output.Info
               <| String.Format
                    (CultureInfo.CurrentCulture,
                     (CommandLine.resources.GetString "savingto"), toDirectory)
               Output.Info
               <| String.Format
                    (CultureInfo.CurrentCulture,
                     (CommandLine.resources.GetString "instrumentingin"), fromDirectory)
             else
               Output.Info
               <| String.Format
                    (CultureInfo.CurrentCulture,
                     (CommandLine.resources.GetString "instrumentingfrom"), fromDirectory)
               Output.Info
               <| String.Format
                    (CultureInfo.CurrentCulture,
                     (CommandLine.resources.GetString "instrumentingto"), toDirectory))
        Right
          (rest, fromDirectories |> Seq.map DirectoryInfo,
           toDirectories |> Seq.map DirectoryInfo,
           Visitor.SourceDirectories() |> Seq.map DirectoryInfo)
    | Left intro -> Left intro

  let internal ImageLoadResilient (f: unit -> 'a) (tidy: unit -> 'a) =
    try
      f()
    with
    | :? BadImageFormatException
    | :? ArgumentException
    | :? IOException -> tidy()

  let internal PrepareTargetFiles (fromInfos: DirectoryInfo seq)
      (toInfos: DirectoryInfo seq) (sourceInfos: DirectoryInfo seq) (targets: string seq) =
    // Copy all the files into the target directory
    let mapping = Dictionary<string, string>()
    Seq.zip sourceInfos targets
    |> Seq.map (fun (x, y) -> (x.FullName, y))
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
           |> Seq.fold (fun (accumulator: AssemblyInfo list) info ->
                let fullName = info.FullName
                ImageLoadResilient (fun () ->
                  use stream = File.OpenRead(fullName)
                  use def = AssemblyDefinition.ReadAssembly(stream)
                  let assemblyPdb = ProgramDatabase.GetPdbWithFallback def
                  if def
                     |> Visitor.IsIncluded
                     |> Visitor.IsInstrumented
                     && Option.isSome assemblyPdb
                  then
                    String.Format
                      (CultureInfo.CurrentCulture,
                       (CommandLine.resources.GetString "instrumenting"), fullName)
                    |> Output.Info
                    { Path = [ fullName ]
                      Name = def.Name.Name
                      Refs =
                        def.MainModule.AssemblyReferences
                        |> Seq.map (fun r -> r.Name)
                        |> Seq.toList }
                    :: accumulator
                  else accumulator) (fun () -> accumulator)) [])
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
      |> Seq.fold (fun (s: Set<string>) n -> Set.add n s) Set.empty<string>

    let simplified =
      assemblies
      |> List.map
           (fun a ->
           { a with Refs = a.Refs |> List.filter (fun n -> Set.contains n candidates) })

    let rec bundle unassigned unresolved collection n =
      match unassigned with
      | [] -> collection
      | _ ->
        let stage =
          (if n <= 1 then unassigned
           else unassigned |> List.filter (fun u -> u.Refs |> List.isEmpty))
          |> List.sortBy (fun u -> u.Name)

        let waiting = stage |> List.fold (fun s a -> Set.remove a.Name s) unresolved

        let next =
          unassigned
          |> List.filter (fun u ->
               u.Refs
               |> List.isEmpty
               |> not)
          |> List.map
               (fun a ->
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

  let internal DoInstrumentation arguments =
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
      DeclareOptions()
      |> CommandLine.ParseCommandLine arguments
      |> CommandLine.ProcessHelpOption
      |> ProcessOutputLocation
    match check1 with
    | Left(intro, options) ->
      CommandLine.HandleBadArguments dotnetBuild arguments intro options
        (Runner.DeclareOptions())
      255
    | Right(rest, fromInfo, toInfo, targetInfo) ->
      let report = Visitor.ReportPath()

      let result =
        CommandLine.doPathOperation (fun () ->
          report
          |> Path.GetDirectoryName
          |> CommandLine.ensureDirectory
          let (assemblies, assemblyNames) =
            PrepareTargetFiles fromInfo toInfo targetInfo
              (Visitor.InstrumentDirectories())
          Output.Info
          <| String.Format
               (CultureInfo.CurrentCulture,
                (CommandLine.resources.GetString "reportingto"), report)
          let reporter, document =
            match Visitor.ReportKind() with
            | ReportFormat.OpenCover -> OpenCover.ReportGenerator()
            | _ -> Report.ReportGenerator()

          let visitors =
            [ reporter
              Instrument.InstrumentGenerator assemblyNames ]

          Visitor.Visit visitors (assemblies)
          document.Save(report)
          if !Visitor.collect then Runner.SetRecordToFile report
          CommandLine.ProcessTrailingArguments rest (toInfo |> Seq.head)) 255 true
      CommandLine.ReportErrors "Instrumentation" (dotnetBuild && !Visitor.inplace)
      result

  let internal (|Select|_|) (pattern: String) offered =
    if offered
       |> String.IsNullOrWhiteSpace
       |> not
       && pattern.StartsWith(offered, StringComparison.OrdinalIgnoreCase)
    then Some offered
    else None

  let internal Main arguments =
    let first =
      arguments
      |> Seq.tryHead
      |> Option.getOrElse String.Empty
    init()
    match first with
    | Select "Runner" _ ->
      Runner.init()
      Runner.DoCoverage arguments (DeclareOptions())
    | Select "ipmo" _ ->
      Path.Combine
        (Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName,
         "AltCover.PowerShell.dll")
      |> Path.GetFullPath
      |> sprintf "Import-Module %A"
      |> (Output.Info)
      0
    | Select "version" _ ->
      Runner.WriteResourceWithFormatItems "AltCover.Version"
        [| AssemblyVersionInformation.AssemblyFileVersion |] false
      0
    | _ -> DoInstrumentation arguments

  // mocking point
  let mutable internal EffectiveMain = Main