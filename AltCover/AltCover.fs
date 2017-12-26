namespace AltCover

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Reflection
open System.Resources

open Augment
open Mono.Cecil
open Mono.Options

module Main =
  open System.Globalization

  let mutable private help = false
  let private resources = ResourceManager("AltCover.Strings", Assembly.GetExecutingAssembly())

  let internal Usage (intro:string) (options:OptionSet) =
    let stderr = Console.Error
    stderr.WriteLine (resources.GetString intro)
    options.WriteOptionDescriptions(stderr);

  let internal Write (writer:TextWriter) colour data =
    if not(String.IsNullOrEmpty(data)) then
       let original = Console.ForegroundColor
       try
         Console.ForegroundColor <- colour
         writer.WriteLine(data)
       finally
         Console.ForegroundColor <- original

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
       (fun x -> Visitor.inputDirectory <- x))
      ("o|outputDirectory=",
       (fun x -> Visitor.outputDirectory <- x))
      ("k|key=",
       (fun x ->
             if not (String.IsNullOrEmpty(x)) && File.Exists(x) then
               try
                   use stream = new System.IO.FileStream(x, System.IO.FileMode.Open, System.IO.FileAccess.Read)
                   let pair = StrongNameKeyPair(stream)
                   Visitor.Add pair
               with
               | :? IOException as io -> WriteErr io.Message
               | :? System.Security.SecurityException as s -> WriteErr s.Message
         ))
      ("sn|strongNameKey=",
       (fun x ->
             if not (String.IsNullOrEmpty(x)) && File.Exists(x) then
               try
                   use stream = new System.IO.FileStream(x, System.IO.FileMode.Open, System.IO.FileAccess.Read)
                   let pair = StrongNameKeyPair(stream)
                   Visitor.defaultStrongNameKey <- Some pair
                   Visitor.Add pair
                with
                | :? IOException as io -> WriteErr io.Message
                | :? System.Security.SecurityException as s -> WriteErr s.Message
                        ))
      ("x|xmlReport=",
       (fun x -> Visitor.reportPath <- Path.Combine(Directory.GetCurrentDirectory(), x)))
      ("f|fileFilter=",
       FilterClass.File >> Visitor.NameFilters.Add)
      ("s|assemblyFilter=",
       FilterClass.Assembly >> Visitor.NameFilters.Add)
      ("t|typeFilter=",
       FilterClass.Type >> Visitor.NameFilters.Add)
      ("m|methodFilter=",
       FilterClass.Method >> Visitor.NameFilters.Add)
      ("a|attributeFilter=",
       FilterClass.Attribute >> Visitor.NameFilters.Add)
      ("?|help|h",
         (fun x -> help <- not (isNull x))) ]
      |> List.fold (fun (o:OptionSet) (p, a) -> o.Add(p, resources.GetString(p), new System.Action<string>(a))) (OptionSet())

  [<EntryPoint>]
  let private Main arguments =
    let options = DeclareOptions ()
    let parse = try
                    Right <| options.Parse(arguments)
                with
                | :? OptionException ->
                    Left "UsageError"
    let action = match parse with
                 | Right _ -> if help then Left "HelpText" else parse
                 | fail -> fail

    let check1 = match action with 
                 | Right rest -> 
                     try 
                        // Check that the directories are distinct
                        let fromDirectory = Path.Combine(Directory.GetCurrentDirectory(), Visitor.inputDirectory)
                        let toDirectory = Path.Combine(Directory.GetCurrentDirectory(), Visitor.outputDirectory)
                        if not (Directory.Exists(toDirectory)) then
                           WriteOut <| String.Format(CultureInfo.CurrentCulture, 
                                    (resources.GetString "CreateFolder"),
                                    toDirectory)
                           Directory.CreateDirectory(toDirectory) |> ignore
                        let fromInfo = DirectoryInfo(fromDirectory)
                        let toInfo = DirectoryInfo(toDirectory)
                        if fromInfo = toInfo then
                           WriteErr (resources.GetString "NotInPlace")
                           Left "UsageError"
                        else Right (rest, fromInfo, toInfo)
                     with
                     | x -> WriteErr x.Message
                            Left "UsageError"
                 | Left intro -> Left intro

    match check1 with
    | Left intro -> Usage intro options
    | Right (rest, fromInfo, toInfo) -> 
      try
        let files = fromInfo.GetFiles()

        // Copy all the files into the target directory
        // Track the symbol-bearing assemblies 
        let assemblies =
          files
          |> Seq.fold (fun (accumulator : (string*string) list) (info:FileInfo) ->
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

        let assemblyNames = 
          assemblies
          |> List.map snd

        // Ensure we always have an absolute file path here.
        Visitor.reportPath <- Path.Combine(Directory.GetCurrentDirectory(), Visitor.reportPath)

        let reporter, document = Report.ReportGenerator ()
        let visitors = [ reporter ; Instrument.InstrumentGenerator assemblyNames ]
        Visitor.Visit visitors (assemblies |> Seq.map fst)
        document.Save(Visitor.reportPath)

        // If we have some arguments in rest execute that command line
        match rest |> Seq.toList with
        | [] -> ()
        | cmd::t->
           let args = String.Join(" ", (List.toArray t))
           Launch cmd args toInfo.FullName // Spawn process, echoing asynchronously
      with
      | x -> WriteErr x.Message

    0                     