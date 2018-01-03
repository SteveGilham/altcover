namespace AltCover

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Reflection
open System.Resources
open System.Text.RegularExpressions

open Augment
open Mono.Cecil
open Mono.Options

module Main =
  open System.Globalization

  let mutable private help = false
  let mutable private error = false
#if NETCOREAPP2_0
#if ALTCOVER_TEST
  let private resources = ResourceManager("altcover.tests.core.Strings", Assembly.GetExecutingAssembly())
#else
  let private resources = ResourceManager("altcover.core.Strings", Assembly.GetExecutingAssembly())
#endif
#else
  let private resources = ResourceManager("AltCover.Strings", Assembly.GetExecutingAssembly())
#endif

  let internal WriteColoured (writer:TextWriter) colour operation =
       let original = Console.ForegroundColor
       try
         Console.ForegroundColor <- colour
         operation writer
       finally
         Console.ForegroundColor <- original

  let internal Usage (intro:string) (options:OptionSet) =
    WriteColoured Console.Error ConsoleColor.Yellow (fun w ->  w.WriteLine (resources.GetString intro)
                                                               options.WriteOptionDescriptions(w))

  let internal Write (writer:TextWriter) colour data =
    if not(String.IsNullOrEmpty(data)) then
      WriteColoured writer colour (fun w -> w.WriteLine(data))

  let internal WriteErr line =
      Write Console.Error ConsoleColor.Yellow line
  let internal WriteOut line =
      Write Console.Out ConsoleColor.White line

  let internal Launch cmd args toDirectory =
    Directory.SetCurrentDirectory(toDirectory)
    let psi = ProcessStartInfo(cmd,args)
    psi.WorkingDirectory <- toDirectory
    psi.CreateNoWindow <- true
    psi.UseShellExecute <- false
    psi.RedirectStandardError <- true
    psi.RedirectStandardOutput <- true
    use proc = new Process()
    proc.StartInfo <- psi

    proc.ErrorDataReceived.Add(fun e -> WriteErr e.Data)
    proc.OutputDataReceived.Add(fun e -> WriteOut e.Data)
    proc.Start() |> ignore
    proc.BeginErrorReadLine()
    proc.BeginOutputReadLine()
    proc.WaitForExit()

  let internal DeclareOptions () =
    [ ("i|inputDirectory=",
       (fun x -> if not (String.IsNullOrWhiteSpace(x)) && Directory.Exists(x) then
                    if Option.isSome Visitor.inputDirectory then
                      error <- true
                    else
                      Visitor.inputDirectory <- Some (Path.GetFullPath x)
                 else error <- true))
      ("o|outputDirectory=",
       (fun x -> if not (String.IsNullOrWhiteSpace(x)) then
                    if Option.isSome Visitor.outputDirectory then
                      error <- true
                    else
                      try
                        Visitor.outputDirectory <- Some (Path.GetFullPath x)
                      with
                      | :? ArgumentException
                      | :? NotSupportedException
                      | :? PathTooLongException -> error <- true
                      | :? System.Security.SecurityException as s -> WriteErr s.Message
                 else error <- true))
      ("k|key=",
       (fun x ->
             if not (String.IsNullOrWhiteSpace(x)) && File.Exists(x) then
               try
                   use stream = new System.IO.FileStream(x, System.IO.FileMode.Open, System.IO.FileAccess.Read)
                   let pair = StrongNameKeyPair(stream)
                   Visitor.Add pair
               with
               | :? IOException
               | :? ArgumentException
               | :? NotSupportedException -> error <- true
               | :? System.Security.SecurityException as s -> WriteErr s.Message
             else error <- true
         ))
      ("sn|strongNameKey=",
       (fun x ->
             if not (String.IsNullOrWhiteSpace(x)) && File.Exists(x) then
               try
                   use stream = new System.IO.FileStream(x, System.IO.FileMode.Open, System.IO.FileAccess.Read)
                   let pair = StrongNameKeyPair(stream)
                   if Option.isSome Visitor.defaultStrongNameKey then error <- true
                   else Visitor.defaultStrongNameKey <- Some pair
                   Visitor.Add pair
                with
                | :? IOException
                | :? ArgumentException
                | :? NotSupportedException -> error <- true
                | :? System.Security.SecurityException as s -> WriteErr s.Message
             else error <- true  ))
      ("x|xmlReport=",
       (fun x -> if not (String.IsNullOrWhiteSpace(x)) then
                    if Option.isSome Visitor.reportPath then
                      error <- true
                    else
                      try
                        Visitor.reportPath <- Some (Path.GetFullPath x)
                      with
                      | :? ArgumentException
                      | :? NotSupportedException
                      | :? PathTooLongException -> error <- true
                      | :? System.Security.SecurityException as s -> WriteErr s.Message
                 else error <- true))
      ("f|fileFilter=",
       (fun x -> x.Split([|";"|], StringSplitOptions.RemoveEmptyEntries)
                 |> Seq.iter (Regex >> FilterClass.File >> Visitor.NameFilters.Add)))
      ("s|assemblyFilter=",
       (fun x -> x.Split([|";"|], StringSplitOptions.RemoveEmptyEntries)
                 |> Seq.iter (Regex >> FilterClass.Assembly >> Visitor.NameFilters.Add)))
      ("t|typeFilter=",
       (fun x -> x.Split([|";"|], StringSplitOptions.RemoveEmptyEntries)
                 |> Seq.iter (Regex >> FilterClass.Type >> Visitor.NameFilters.Add)))
      ("m|methodFilter=",
       (fun x -> x.Split([|";"|], StringSplitOptions.RemoveEmptyEntries)
                 |> Seq.iter (Regex >> FilterClass.Method >> Visitor.NameFilters.Add)))
      ("a|attributeFilter=",
       (fun x -> x.Split([|";"|], StringSplitOptions.RemoveEmptyEntries)
                 |> Seq.iter (Regex >> FilterClass.Attribute >> Visitor.NameFilters.Add)))
      ("?|help|h", (fun x -> help <- not (isNull x)))
      ("<>", (fun x -> error <- true))         ]// default end stop
      |> List.fold (fun (o:OptionSet) (p, a) -> o.Add(p, resources.GetString(p), new System.Action<string>(a))) (OptionSet())

  let internal ParseCommandLine (arguments:string array) (options:OptionSet) =
      help <- false
      error <- false
      try
          let before = arguments
                       |> Array.takeWhile (fun x -> x <> "--")
          let after = arguments
                      |> Seq.skipWhile (fun x -> x <> "--")
                      |> Seq.skipWhile (fun x -> x = "--")
                      |> Seq.toList
          let parse = options.Parse(before)
          if error || (parse.Count <> 0) then
             Left ("UsageError", options)
          else
             Right (after, options)
       with
       | :? OptionException -> Left ("UsageError", options)

  let internal ProcessHelpOption (parse:(Either<string*OptionSet, string list*OptionSet>)) =
    match parse with
    | Right (_, options) -> if help then Left ("HelpText", options) else parse
    | fail -> fail

  let internal ProcessOutputLocation (action:(Either<string*OptionSet, string list*OptionSet>)) =
    match action with
    | Right (rest, options) ->
        try
           // Check that the directories are distinct
           let fromDirectory = Visitor.InputDirectory()
           let toDirectory = Visitor.OutputDirectory()
           if not (Directory.Exists(toDirectory)) then
              WriteOut <| String.Format(CultureInfo.CurrentCulture,
                       (resources.GetString "CreateFolder"),
                       toDirectory)
              Directory.CreateDirectory(toDirectory) |> ignore
           if fromDirectory = toDirectory then
              WriteErr (resources.GetString "NotInPlace")
              Left ("UsageError", options)
           else
               WriteOut <| String.Format(CultureInfo.CurrentCulture,
                                         (resources.GetString "instrumentingfrom"),
                                         fromDirectory)
               WriteOut <| String.Format(CultureInfo.CurrentCulture,
                                         (resources.GetString "instrumentingto"),
                                         toDirectory)
               Right (rest, DirectoryInfo(fromDirectory), DirectoryInfo(toDirectory))

        with
        |  :? IOException as x -> WriteErr x.Message
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

  let internal ProcessTrailingArguments (rest: string list) (toInfo:DirectoryInfo) =
    // If we have some arguments in rest execute that command line
        match rest |> Seq.toList with
        | [] -> ()
        | cmd::t->
           let args = String.Join(" ", (List.toArray t))
           Launch cmd args toInfo.FullName // Spawn process, echoing asynchronously

  let internal DoInstrumentation arguments =
    let check1 = DeclareOptions ()
                 |> ParseCommandLine arguments
                 |> ProcessHelpOption
                 |> ProcessOutputLocation
    match check1 with
    | Left (intro, options) -> Usage intro options
    | Right (rest, fromInfo, toInfo) ->
      try
        let (assemblies, assemblyNames) = PrepareTargetFiles fromInfo toInfo
        WriteOut <| String.Format(CultureInfo.CurrentCulture,
                                         (resources.GetString "reportingto"),
                                         Visitor.ReportPath())
        let reporter, document = Report.ReportGenerator ()
        let visitors = [ reporter ; Instrument.InstrumentGenerator assemblyNames ]
        Visitor.Visit visitors (assemblies )
        document.Save(Visitor.ReportPath())

        ProcessTrailingArguments rest toInfo
      with
      | :? IOException as x -> WriteErr x.Message

#if ALTCOVER_TEST
#else
  [<EntryPoint>]
  let private Main arguments =
    DoInstrumentation arguments
    0
#endif