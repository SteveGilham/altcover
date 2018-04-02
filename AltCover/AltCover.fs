namespace AltCover

open System
open System.Collections.Generic
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

module Main =

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
                      CommandLine.doPathOperation (fun _ -> Visitor.outputDirectory <- Some (Path.GetFullPath x)) ()
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
                                          Visitor.Add pair) ()
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
                                               Visitor.Add pair) ()
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
                      CommandLine.doPathOperation (fun () -> Visitor.reportPath <- Some (Path.GetFullPath x)) ()
                 else CommandLine.error <- String.Format(CultureInfo.CurrentCulture,
                                                         CommandLine.resources.GetString "InvalidValue",
                                                         "--xmlReport",
                                                         x) :: CommandLine.error))
      ("f|fileFilter=",
       (fun x -> x.Split([|";"|], StringSplitOptions.RemoveEmptyEntries)
                 |> Seq.iter (Regex >> FilterClass.File >> Visitor.NameFilters.Add)))
      ("s|assemblyFilter=",
       (fun x -> x.Split([|";"|], StringSplitOptions.RemoveEmptyEntries)
                 |> Seq.iter (Regex >> FilterClass.Assembly >> Visitor.NameFilters.Add)))
      ("e|assemblyExcludeFilter=",
       (fun x -> x.Split([|";"|], StringSplitOptions.RemoveEmptyEntries)
                 |> Seq.iter (Regex >> FilterClass.Module >> Visitor.NameFilters.Add)))
      ("t|typeFilter=",
       (fun x -> x.Split([|";"|], StringSplitOptions.RemoveEmptyEntries)
                 |> Seq.iter (Regex >> FilterClass.Type >> Visitor.NameFilters.Add)))
      ("m|methodFilter=",
       (fun x -> x.Split([|";"|], StringSplitOptions.RemoveEmptyEntries)
                 |> Seq.iter (Regex >> FilterClass.Method >> Visitor.NameFilters.Add)))
      ("a|attributeFilter=",
       (fun x -> x.Split([|";"|], StringSplitOptions.RemoveEmptyEntries)
                 |> Seq.iter (Regex >> FilterClass.Attribute >> Visitor.NameFilters.Add)))
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
            if CommandLine.error |> List.isEmpty && toDirectory |> Directory.Exists |> not then
              CommandLine.WriteOut <| String.Format(CultureInfo.CurrentCulture,
                                                    (CommandLine.resources.GetString "CreateFolder"),
                                                     toDirectory)
              Directory.CreateDirectory(toDirectory) |> ignore) ()

        if CommandLine.error |> List.isEmpty |> not then
            Left ("UsageError", options)
        else
          if Visitor.inplace then
            CommandLine.WriteOut <| String.Format(CultureInfo.CurrentCulture,
                                        (CommandLine.resources.GetString "savingto"),
                                        toDirectory)
            CommandLine.WriteOut <| String.Format(CultureInfo.CurrentCulture,
                                        (CommandLine.resources.GetString "instrumentingin"),
                                        fromDirectory)
          else
            CommandLine.WriteOut <| String.Format(CultureInfo.CurrentCulture,
                                        (CommandLine.resources.GetString "instrumentingfrom"),
                                        fromDirectory)
            CommandLine.WriteOut <| String.Format(CultureInfo.CurrentCulture,
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
    | :? BadImageFormatException -> tidy()
    | :? IOException -> tidy()

  let internal PrepareTargetFiles (fromInfo:DirectoryInfo) (toInfo:DirectoryInfo) (sourceInfo:DirectoryInfo) =
    // Copy all the files into the target directory
    // Track the symbol-bearing assemblies
    let assemblies =
      fromInfo.GetFiles()
      |> Seq.fold (fun (accumulator : (string*string) list) info ->
           let fullName = info.FullName
           let filename = info.Name
           let copy = Path.Combine (toInfo.FullName, filename)
           File.Copy(fullName, copy, true)
           let source = Path.Combine (sourceInfo.FullName, filename)
           ImageLoadResilient(fun () ->
             let def = AssemblyDefinition.ReadAssembly(fullName)
             let assemblyPdb = ProgramDatabase.GetPdbWithFallback def
             if def |> Visitor.IsIncluded |> Visitor.IsInstrumented &&
                Option.isSome assemblyPdb then
                (source, def.Name.Name) :: accumulator
             else
                accumulator) (fun () -> accumulator)
        ) []

    List.unzip assemblies

  let internal DoInstrumentation arguments =
    let check1 = DeclareOptions ()
                 |> CommandLine.ParseCommandLine arguments
                 |> CommandLine.ProcessHelpOption
                 |> ProcessOutputLocation
    match check1 with
    | Left (intro, options) ->
        String.Join (" ", arguments |> Seq.map (sprintf "%A"))
        |> CommandLine.WriteErr
        CommandLine.error
        |> List.iter CommandLine.WriteErr
        CommandLine.Usage intro options (Runner.DeclareOptions())
        255
    | Right (rest, fromInfo, toInfo, targetInfo) ->
        CommandLine.doPathOperation( fun () ->
        let (assemblies, assemblyNames) = PrepareTargetFiles fromInfo toInfo targetInfo
        CommandLine.WriteOut <| String.Format(CultureInfo.CurrentCulture,
                                         (CommandLine.resources.GetString "reportingto"),
                                         Visitor.ReportPath())
        let reporter, document = match Visitor.ReportKind() with
                                 | ReportFormat.OpenCover -> OpenCover.ReportGenerator ()
                                 | _ -> Report.ReportGenerator ()

        let visitors = [ reporter ; Instrument.InstrumentGenerator assemblyNames ]
        Visitor.Visit visitors (assemblies )
        document.Save(Visitor.ReportPath())

        CommandLine.ProcessTrailingArguments rest toInfo) 255

  [<EntryPoint>]
  let private Main arguments =
    if "Runner".StartsWith(arguments |> Seq.head, StringComparison.OrdinalIgnoreCase)
      then Runner.DoCoverage arguments (DeclareOptions())
      else DoInstrumentation arguments