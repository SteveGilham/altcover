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

open Augment
open Mono.Cecil
open Mono.Options

module Main =

  let internal DeclareOptions () =
    [ ("i|inputDirectory=",
       (fun x -> if not (String.IsNullOrWhiteSpace(x)) && Directory.Exists(x) then
                    if Option.isSome Visitor.inputDirectory then
                      CommandLine.error <- true
                    else
                      Visitor.inputDirectory <- Some (Path.GetFullPath x)
                 else CommandLine.error <- true))
      ("o|outputDirectory=",
       (fun x -> if not (String.IsNullOrWhiteSpace(x)) then
                    if Option.isSome Visitor.outputDirectory then
                      CommandLine.error <- true
                    else
                      CommandLine.doPathOperation (fun _ -> Visitor.outputDirectory <- Some (Path.GetFullPath x))
                 else CommandLine.error <- true))
#if NETCOREAPP2_0
#else
      ("k|key=",
       (fun x ->
             if not (String.IsNullOrWhiteSpace(x)) && File.Exists(x) then
               CommandLine.doPathOperation (fun () -> 
                                          use stream = new System.IO.FileStream(x,
                                                                                System.IO.FileMode.Open,
                                                                                System.IO.FileAccess.Read)
                                          let pair = StrongNameKeyPair(stream)
                                          Visitor.Add pair)
             else CommandLine.error <- true
         ))
      ("sn|strongNameKey=",
       (fun x ->
             if not (String.IsNullOrWhiteSpace(x)) && File.Exists(x) then
               CommandLine.doPathOperation (fun () -> 
                                          use stream = new System.IO.FileStream(x,
                                                                                System.IO.FileMode.Open,
                                                                                System.IO.FileAccess.Read)
                                          // printfn "%A %A" x Visitor.defaultStrongNameKey
                                          let pair = StrongNameKeyPair(stream)
                                          if Option.isSome Visitor.defaultStrongNameKey then CommandLine.error <- true
                                          else Visitor.defaultStrongNameKey <- Some pair
                                               Visitor.Add pair)
             else CommandLine.error <- true  ))
#endif
      ("x|xmlReport=",
       (fun x -> if not (String.IsNullOrWhiteSpace(x)) then
                    if Option.isSome Visitor.reportPath then
                      CommandLine.error <- true
                    else
                      CommandLine.doPathOperation (fun () -> Visitor.reportPath <- Some (Path.GetFullPath x))
                 else CommandLine.error <- true))
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
      ("?|help|h", (fun x -> CommandLine.help <- not (isNull x)))
      ("<>", (fun x -> CommandLine.error <- true))         ]// default end stop
      |> List.fold (fun (o:OptionSet) (p, a) -> o.Add(p, CommandLine.resources.GetString(p), new System.Action<string>(a))) (OptionSet())

  let internal ProcessOutputLocation (action:(Either<string*OptionSet, string list*OptionSet>)) =
    match action with
    | Right (rest, options) ->
        try
           // Check that the directories are distinct
           let fromDirectory = Visitor.InputDirectory()
           let toDirectory = Visitor.OutputDirectory()
           if not (Directory.Exists(toDirectory)) then
              CommandLine.WriteOut <| String.Format(CultureInfo.CurrentCulture,
                       (CommandLine.resources.GetString "CreateFolder"),
                       toDirectory)
              Directory.CreateDirectory(toDirectory) |> ignore
           if fromDirectory = toDirectory then
              CommandLine.WriteErr (CommandLine.resources.GetString "NotInPlace")
              Left ("UsageError", options)
           else
               CommandLine.WriteOut <| String.Format(CultureInfo.CurrentCulture,
                                         (CommandLine.resources.GetString "instrumentingfrom"),
                                         fromDirectory)
               CommandLine.WriteOut <| String.Format(CultureInfo.CurrentCulture,
                                         (CommandLine.resources.GetString "instrumentingto"),
                                         toDirectory)
               Right (rest, DirectoryInfo(fromDirectory), DirectoryInfo(toDirectory))

        with
        |  :? IOException as x -> CommandLine.WriteErr x.Message
                                  Left ("UsageError", options)
    | Left intro -> Left intro

  let internal PrepareTargetFiles (fromInfo:DirectoryInfo) (toInfo:DirectoryInfo) =
    // Copy all the files into the target directory
    // Track the symbol-bearing assemblies
    let assemblies =
      fromInfo.GetFiles()
      |> Seq.fold (fun (accumulator : (string*string) list) info ->
           let fullName = info.FullName
           let target = Path.Combine (toInfo.FullName, info.Name)
           File.Copy(fullName, target, true)
           try
             let def = AssemblyDefinition.ReadAssembly(fullName)
             let assemblyPdb = ProgramDatabase.GetPdbWithFallback def
             if Visitor.IsIncluded def && Option.isSome assemblyPdb then
                (fullName, def.Name.Name) :: accumulator
             else
                accumulator
           with
           | :? BadImageFormatException -> accumulator
           | :? IOException -> accumulator
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
        CommandLine.Usage intro options
    | Right (rest, fromInfo, toInfo) ->
      try
        let (assemblies, assemblyNames) = PrepareTargetFiles fromInfo toInfo
        CommandLine.WriteOut <| String.Format(CultureInfo.CurrentCulture,
                                         (CommandLine.resources.GetString "reportingto"),
                                         Visitor.ReportPath())
        let reporter, document = Report.ReportGenerator ()
        let visitors = [ reporter ; Instrument.InstrumentGenerator assemblyNames ]
        Visitor.Visit visitors (assemblies )
        document.Save(Visitor.ReportPath())

        CommandLine.ProcessTrailingArguments rest toInfo
      with
      | :? IOException as x -> CommandLine.WriteErr x.Message

  [<EntryPoint>]
  let private Main arguments =
    DoInstrumentation arguments
    0