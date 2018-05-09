namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.Globalization
open System.IO
#if NETCOREAPP2_0
#else
open System.Reflection
#endif
open System.Text.RegularExpressions

open AltCover.Base
open Augment
open Mono.Cecil
open Mono.Options

[<ExcludeFromCodeCoverage>]
type AssemblyInfo = {
         Path : string
         Name : string
         Refs : string list
         }

module Main =
  let init () =
    Visitor.inputDirectory <- None
    Visitor.outputDirectory <- None
    ProgramDatabase.SymbolFolders.Clear()
#if NETCOREAPP2_0
#else
    Visitor.keys.Clear()
    Visitor.defaultStrongNameKey <- None
#endif
    Visitor.reportPath <- None
    Visitor.NameFilters.Clear()
    Visitor.interval <- None
    Visitor.TrackingNames.Clear()
    Visitor.reportFormat <- None
    Visitor.inplace <- false
    Visitor.collect <- false

  let internal DeclareOptions () =
    [ ("i|inputDirectory=",
       (fun x -> if not (String.IsNullOrWhiteSpace x) && Directory.Exists x then
                    if Option.isSome Visitor.inputDirectory then
                      CommandLine.error <- String.Format(CultureInfo.CurrentCulture,
                                                         CommandLine.resources.GetString "MultiplesNotAllowed",
                                                         "--inputDirectory") :: CommandLine.error

                    else
                      Visitor.inputDirectory <- Some (Path.GetFullPath x)
                 else CommandLine.error <- String.Format(CultureInfo.CurrentCulture,
                                                         CommandLine.resources.GetString "DirectoryNotFound",
                                                         "--inputDirectory",
                                                         x) :: CommandLine.error))
      ("o|outputDirectory=",
       (fun x -> if not (String.IsNullOrWhiteSpace x) then
                    if Option.isSome Visitor.outputDirectory then
                      CommandLine.error <- String.Format(CultureInfo.CurrentCulture,
                                                         CommandLine.resources.GetString "MultiplesNotAllowed",
                                                         "--outputDirectory") :: CommandLine.error

                    else
                      CommandLine.doPathOperation (fun _ -> Visitor.outputDirectory <- Some (Path.GetFullPath x)) () false
                 else CommandLine.error <- String.Format(CultureInfo.CurrentCulture,
                                                         CommandLine.resources.GetString "DirectoryNotFound",
                                                         "--outputDirectory",
                                                         x) :: CommandLine.error))
      ("y|symbolDirectory=",
       (fun x -> if not (String.IsNullOrWhiteSpace x) && Directory.Exists x then
                    ProgramDatabase.SymbolFolders.Add x
                 else CommandLine.error <- String.Format(CultureInfo.CurrentCulture,
                                                         CommandLine.resources.GetString "DirectoryNotFound",
                                                         "--symbolDirectory",
                                                         x) :: CommandLine.error))
#if NETCOREAPP2_0
#else
      ("k|key=",
       (fun x ->
             if not (String.IsNullOrWhiteSpace x) && File.Exists x then
               CommandLine.doPathOperation (fun () ->
                                          use stream = new System.IO.FileStream(x,
                                                                                System.IO.FileMode.Open,
                                                                                System.IO.FileAccess.Read)
                                          let pair = StrongNameKeyPair(stream)
                                          Visitor.Add pair) () false
             else CommandLine.error <- String.Format(CultureInfo.CurrentCulture,
                                                         CommandLine.resources.GetString "FileNotFound",
                                                         "--key",
                                                         x) :: CommandLine.error ))
      ("sn|strongNameKey=",
       (fun x ->
             if not (String.IsNullOrWhiteSpace x ) && File.Exists x then
               CommandLine.doPathOperation (fun () ->
                                          use stream = new System.IO.FileStream(x,
                                                                                System.IO.FileMode.Open,
                                                                                System.IO.FileAccess.Read)
                                          // printfn "%A %A" x Visitor.defaultStrongNameKey
                                          let pair = StrongNameKeyPair(stream)
                                          if Option.isSome Visitor.defaultStrongNameKey then
                                             CommandLine.error <- String.Format(CultureInfo.CurrentCulture,
                                                                                CommandLine.resources.GetString "MultiplesNotAllowed",
                                                                                "--strongNameKey") :: CommandLine.error

                                          else Visitor.defaultStrongNameKey <- Some pair
                                               Visitor.Add pair) () false
             else CommandLine.error <- String.Format(CultureInfo.CurrentCulture,
                                                         CommandLine.resources.GetString "FileNotFound",
                                                         "--strongNameKey",
                                                         x) :: CommandLine.error ))
#endif
      ("x|xmlReport=",
       (fun x -> if not (String.IsNullOrWhiteSpace x) then
                    if Option.isSome Visitor.reportPath then
                      CommandLine.error <- String.Format(CultureInfo.CurrentCulture,
                                                         CommandLine.resources.GetString "MultiplesNotAllowed",
                                                         "--xmlReport") :: CommandLine.error

                    else
                      CommandLine.doPathOperation (fun () -> Visitor.reportPath <- Some (Path.GetFullPath x)) () false
                 else CommandLine.error <- String.Format(CultureInfo.CurrentCulture,
                                                         CommandLine.resources.GetString "InvalidValue",
                                                         "--xmlReport",
                                                         x) :: CommandLine.error))
      ("f|fileFilter=",
       (fun x -> CommandLine.doPathOperation (fun () ->
                 x.Split([|";"|], StringSplitOptions.RemoveEmptyEntries)
                 |> Seq.iter (Regex >> FilterClass.File >> Visitor.NameFilters.Add))() false))
      ("s|assemblyFilter=",
       (fun x -> CommandLine.doPathOperation (fun () ->
                 x.Split([|";"|], StringSplitOptions.RemoveEmptyEntries)
                 |> Seq.iter (Regex >> FilterClass.Assembly >> Visitor.NameFilters.Add))() false))
      ("e|assemblyExcludeFilter=",
       (fun x -> CommandLine.doPathOperation (fun () ->
                 x.Split([|";"|], StringSplitOptions.RemoveEmptyEntries)
                 |> Seq.iter (Regex >> FilterClass.Module >> Visitor.NameFilters.Add))() false))
      ("t|typeFilter=",
       (fun x -> CommandLine.doPathOperation (fun () ->
                 x.Split([|";"|], StringSplitOptions.RemoveEmptyEntries)
                 |> Seq.iter (Regex >> FilterClass.Type >> Visitor.NameFilters.Add))() false))
      ("m|methodFilter=",
       (fun x -> CommandLine.doPathOperation (fun () ->
                 x.Split([|";"|], StringSplitOptions.RemoveEmptyEntries)
                 |> Seq.iter (Regex >> FilterClass.Method >> Visitor.NameFilters.Add))() false))
      ("a|attributeFilter=",
       (fun x -> CommandLine.doPathOperation (fun () ->
                 x.Split([|";"|], StringSplitOptions.RemoveEmptyEntries)
                 |> Seq.iter (Regex >> FilterClass.Attribute >> Visitor.NameFilters.Add))() false))
      ("c|callContext=",
       (fun x -> if not (String.IsNullOrWhiteSpace x) then
                   let k = x.Trim()
                   if Char.IsDigit <| k.Chars(0) then
                    if Option.isSome Visitor.interval || k.Length > 1 then
                      CommandLine.error <- String.Format(CultureInfo.CurrentCulture,
                                                         CommandLine.resources.GetString "InvalidValue",
                                                         "--callContext",
                                                         x) :: CommandLine.error
                    else
                      let (ok, n) = Int32.TryParse(k)
                      if ok then Visitor.interval <- Some (pown 10 (7 - n))
                      else CommandLine.error <- String.Format(CultureInfo.CurrentCulture,
                                                              CommandLine.resources.GetString "InvalidValue",
                                                              "--callContext",
                                                              x) :: CommandLine.error
                   else
                      Visitor.TrackingNames.Add(k)
                 else CommandLine.error <- String.Format(CultureInfo.CurrentCulture,
                                                         CommandLine.resources.GetString "InvalidValue",
                                                         "--callContext",
                                                         x) :: CommandLine.error))
      ("opencover",
       (fun _ ->  if Option.isSome Visitor.reportFormat then
                      CommandLine.error <- String.Format(CultureInfo.CurrentCulture,
                                                         CommandLine.resources.GetString "MultiplesNotAllowed",
                                                         "--opencover") :: CommandLine.error

                  else
                      Visitor.reportFormat <- Some ReportFormat.OpenCover))
      ("inplace",
       (fun _ ->  if Visitor.inplace then
                      CommandLine.error <- String.Format(CultureInfo.CurrentCulture,
                                                         CommandLine.resources.GetString "MultiplesNotAllowed",
                                                         "--inplace") :: CommandLine.error

                  else
                      Visitor.inplace <- true))
      ("save",
       (fun _ ->  if Visitor.collect then
                      CommandLine.error <- String.Format(CultureInfo.CurrentCulture,
                                                         CommandLine.resources.GetString "MultiplesNotAllowed",
                                                         "--save") :: CommandLine.error

                  else
                      Visitor.collect <- true))
      ("?|help|h", (fun x -> CommandLine.help <- not (isNull x)))
      ("<>", (fun x -> CommandLine.error <- String.Format(CultureInfo.CurrentCulture,
                                                         CommandLine.resources.GetString "InvalidValue",
                                                         "AltCover",
                                                         x) :: CommandLine.error))         ]// default end stop
      |> List.fold (fun (o:OptionSet) (p, a) -> o.Add(p, CommandLine.resources.GetString(p), new System.Action<string>(a))) (OptionSet())

  let internal ProcessOutputLocation (action:(Either<string*OptionSet, string list*OptionSet>)) =
    match action with
    | Right (rest, options) ->
        // Check that the directories are distinct
        let fromDirectory = Visitor.InputDirectory()
        let toDirectory = Visitor.OutputDirectory()
        if fromDirectory = toDirectory then
            CommandLine.error <- CommandLine.resources.GetString "NotInPlace" :: CommandLine.error

        CommandLine.doPathOperation(fun () ->
            if Visitor.inplace &&
               CommandLine.error |> List.isEmpty && toDirectory |> Directory.Exists
            then CommandLine.error <- String.Format(CultureInfo.CurrentCulture,
                                                    CommandLine.resources.GetString "SaveExists",
                                                    toDirectory) :: CommandLine.error

            if CommandLine.error |> List.isEmpty then
              CommandLine.ensureDirectory toDirectory) () false

        if CommandLine.error |> List.isEmpty |> not then
            Left ("UsageError", options)
        else
          if Visitor.inplace then
            Output.Info <| String.Format(CultureInfo.CurrentCulture,
                                        (CommandLine.resources.GetString "savingto"),
                                        toDirectory)
            Output.Info <| String.Format(CultureInfo.CurrentCulture,
                                        (CommandLine.resources.GetString "instrumentingin"),
                                        fromDirectory)
          else
            Output.Info <| String.Format(CultureInfo.CurrentCulture,
                                        (CommandLine.resources.GetString "instrumentingfrom"),
                                        fromDirectory)
            Output.Info <| String.Format(CultureInfo.CurrentCulture,
                                        (CommandLine.resources.GetString "instrumentingto"),
                                        toDirectory)
          Right (rest,
                 DirectoryInfo(fromDirectory),
                 DirectoryInfo(toDirectory),
                 DirectoryInfo(Visitor.SourceDirectory()))
    | Left intro -> Left intro

  let internal ImageLoadResilient (f : unit -> 'a)  (tidy : unit -> 'a) =
    try f ()
    with
    | :? BadImageFormatException
    | :? ArgumentException
    | :? IOException -> tidy()

  let internal PrepareTargetFiles (fromInfo:DirectoryInfo) (toInfo:DirectoryInfo) (sourceInfo:DirectoryInfo) =
    // Copy all the files into the target directory
    let files = fromInfo.GetFiles()
    files
    |> Seq.iter(fun info ->
           let fullName = info.FullName
           let filename = info.Name
           let copy = Path.Combine (toInfo.FullName, filename)
           File.Copy(fullName, copy, true))

    // Track the symbol-bearing assemblies
    let assemblies =
      sourceInfo.GetFiles()
      |> Seq.fold (fun (accumulator : AssemblyInfo list) info ->
           let fullName = info.FullName
           ImageLoadResilient(fun () ->
             use stream = File.OpenRead(fullName)
             use def = AssemblyDefinition.ReadAssembly(stream)
             let assemblyPdb = ProgramDatabase.GetPdbWithFallback def
             if def |> Visitor.IsIncluded |> Visitor.IsInstrumented &&
                Option.isSome assemblyPdb then
                String.Format(CultureInfo.CurrentCulture,
                               (CommandLine.resources.GetString "instrumenting"),
                               fullName) |> Output.Info

                { Path = fullName
                  Name = def.Name.Name
                  Refs = def.MainModule.AssemblyReferences
                         |> Seq.map (fun r -> r.Name)
                         |> Seq.toList} :: accumulator
               else
                  accumulator) (fun () -> accumulator)
        ) []

    // sort the assemblies into order so that the depended-upon are processed first
    let candidates = assemblies
                     |> Seq.map (fun a -> a.Name)
                     |> Seq.fold (fun (s: Set<string>) n -> Set.add n s) Set.empty<string>

    let simplified = assemblies
                     |> List.map (fun a -> { a with Refs = a.Refs
                                                           |> List.filter (fun n -> Set.contains n candidates) })
    let rec bundle unassigned unresolved collection n =
      match unassigned with
      | [] -> collection
      | _ ->
        let stage = (if n <= 1 then unassigned
                     else unassigned |> List.filter (fun u -> u.Refs |> List.isEmpty))
                    |> List.sortBy (fun u -> u.Name)

        let waiting = stage
                      |> List.fold (fun s a -> Set.remove a.Name s) unresolved

        let next = unassigned
                   |> List.filter (fun u -> u.Refs |> List.isEmpty |> not)
                   |> List.map (fun a -> { a with Refs = a.Refs
                                                         |> List.filter (fun n -> Set.contains n waiting) })

        bundle next waiting (stage :: collection) (n-1)

    let sorted = bundle simplified candidates [] (simplified |> List.length)
                 |> List.concat
                 |> List.rev
                 |> List.map (fun a -> (a.Path, a.Name))

    List.unzip sorted

  let internal DoInstrumentation arguments =
    let check1 = DeclareOptions ()
                 |> CommandLine.ParseCommandLine arguments
                 |> CommandLine.ProcessHelpOption
                 |> ProcessOutputLocation
    match check1 with
    | Left (intro, options) ->
        CommandLine.HandleBadArguments arguments intro options (Runner.DeclareOptions())
        255
    | Right (rest, fromInfo, toInfo, targetInfo) ->
        let report = Visitor.ReportPath()
        let result = CommandLine.doPathOperation( fun () ->
            report
            |> Path.GetDirectoryName
            |> CommandLine.ensureDirectory
            let (assemblies, assemblyNames) = PrepareTargetFiles fromInfo toInfo targetInfo
            Output.Info <| String.Format(CultureInfo.CurrentCulture,
                                            (CommandLine.resources.GetString "reportingto"),
                                            report)
            let reporter, document = match Visitor.ReportKind() with
                                     | ReportFormat.OpenCover -> OpenCover.ReportGenerator ()
                                     | _ -> Report.ReportGenerator ()

            let visitors = [ reporter ; Instrument.InstrumentGenerator assemblyNames ]
            Visitor.Visit visitors (assemblies)
            document.Save(report)
            if Visitor.collect then Runner.SetRecordToFile report

            CommandLine.ProcessTrailingArguments rest toInfo) 255 true
        CommandLine.ReportErrors "Instrumentation"
        result

  let internal Main arguments =
    let first = arguments |> Seq.tryHead |> Option.getOrElse String.Empty
    if (first |> String.IsNullOrWhiteSpace |> not) &&
        "Runner".StartsWith(first, StringComparison.OrdinalIgnoreCase)
      then Runner.init()
           Runner.DoCoverage arguments (DeclareOptions())
      else init()
           DoInstrumentation arguments

  // mocking point
  let mutable internal EffectiveMain = Main