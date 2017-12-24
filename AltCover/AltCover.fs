namespace AltCover

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Reflection

open Mono.Cecil
open Mono.Options

module Main =

  let mutable private help = false

  let Usage (intro:string) (options:OptionSet) =
    let stderr = Console.Error
    stderr.WriteLine(intro)
    options.WriteOptionDescriptions(stderr);
    Environment.Exit(1)

  let (!+) (option: string * string * (string->unit)) (options:OptionSet) =
    let prototype, help, action = option
    options.Add(prototype, help, new System.Action<string>(action))

  let Launch cmd args toDirectory =
    Directory.SetCurrentDirectory(toDirectory)
    let psi = ProcessStartInfo(cmd,args)
    psi.WorkingDirectory <- toDirectory
    psi.CreateNoWindow <- true
    psi.UseShellExecute <- false
    psi.RedirectStandardError <- true
    psi.RedirectStandardOutput <- true
    use proc = new Process()
    proc.StartInfo <- psi

    let Write (writer:TextWriter) colour data =
      if not(String.IsNullOrEmpty(data)) then
         let original = Console.ForegroundColor
         try
           Console.ForegroundColor <- colour
           writer.WriteLine(data)
         finally
           Console.ForegroundColor <- original

    let err (e:DataReceivedEventArgs) =
      Write Console.Error ConsoleColor.Yellow e.Data
    let out (e:DataReceivedEventArgs) =
      Write Console.Out ConsoleColor.White e.Data

    proc.ErrorDataReceived.Add(err)
    proc.OutputDataReceived.Add(out)
    proc.Start() |> ignore
    proc.BeginErrorReadLine()
    proc.BeginOutputReadLine()
    proc.WaitForExit()

  [<EntryPoint>]
  let private Main arguments =
    let options = OptionSet()
                    |> !+ (
                        "i|inputDirectory=",
                        "Optional: The folder containing assemblies to instrument (default: current directory)",
                        (fun x -> Visitor.inputDirectory <- x))
                    |> !+ (
                        "o|outputDirectory=",
                        "Optional: The folder to receive the instrumented assemblies and their companions (default: sub-folder '.\\__Instrumented' current directory)",
                        (fun x -> Visitor.outputDirectory <- x))
                    |> !+ (
                        "k|key=",
                        "Optional, multiple: any other strong-name key to use",
                        (fun x ->
                            if not (String.IsNullOrEmpty(x)) && File.Exists(x) then
                              try
                                  use stream = new System.IO.FileStream(x, System.IO.FileMode.Open, System.IO.FileAccess.Read)
                                  let pair = StrongNameKeyPair(stream)
                                  Visitor.Add pair
                              with
                              | :? IOException as io -> Console.WriteLine(io.Message)
                              | :? System.Security.SecurityException as s -> Console.WriteLine(s.Message)
                        ))
                    |> !+ (
                        "sn|strongNameKey=",
                        "Optional: The default strong naming key to apply to instrumented assemblies (default: None)",
                        (fun x ->
                            if not (String.IsNullOrEmpty(x)) && File.Exists(x) then
                              try
                                  use stream = new System.IO.FileStream(x, System.IO.FileMode.Open, System.IO.FileAccess.Read)
                                  let pair = StrongNameKeyPair(stream)
                                  Visitor.defaultStrongNameKey <- Some (pair)
                                  Visitor.Add pair
                              with
                              | :? IOException as io -> Console.WriteLine(io.Message)
                              | :? System.Security.SecurityException as s -> Console.WriteLine(s.Message)
                        ))
                    |> !+ (
                        "x|xmlReport=",
                        "Optional: The output report template file (default: coverage.xml in the current directory)",
                        (fun x -> Visitor.reportPath <- Path.Combine(Directory.GetCurrentDirectory(), x)))
                    |> !+ (
                        "f|fileFilter=",
                        "Optional: file name to exclude from instrumentation (may repeat)",
                        FilterClass.File >> Visitor.NameFilters.Add)
                    |> !+ (
                        "s|assemblyFilter=",
                        "Optional: assembly name to exclude from instrumentation (may repeat)",
                        FilterClass.Assembly >> Visitor.NameFilters.Add)
                    |> !+ (
                        "t|typeFilter=",
                        "Optional: type name to exclude from instrumentation (may repeat)",
                        FilterClass.Type >> Visitor.NameFilters.Add)
                    |> !+ (
                        "m|methodFilter=",
                        "Optional: method name to exclude from instrumentation (may repeat)",
                        FilterClass.Method >> Visitor.NameFilters.Add)
                    |> !+ (
                        "a|attributeFilter=",
                        "Optional: attribute name to exclude from instrumentation (may repeat)",
                        FilterClass.Attribute >> Visitor.NameFilters.Add)
                    |> !+ (
                        "?|help|h",
                         "Prints out the options.",
                          (fun x -> help <- not (isNull x)))

    let rest = try
                    options.Parse(arguments)
               with
                | :? OptionException ->
                  Usage "Error - usage is:" options
                  new List<String>()
    if help then
      let intro = "AltCover [/i[nputDirectory]=VALUE] [/o[utputDirectory]=VALUE] " +
                  "[/sn|strongNameKey=VALUE] [/x[mlReport]=VALUE] [/f[ileFilter]=VALUE] " +
                  "[/s|assemblyFilter=VALUE] [/t|typeFilter=VALUE] [/m|methodFilter=VALUE] " +
                  "[/a|attributeFilter=VALUE] [/?|h[elp]]"
      Usage intro options

    // Check that the directories are distinct
    let fromDirectory = Path.Combine(Directory.GetCurrentDirectory(), Visitor.inputDirectory)
    let toDirectory = Path.Combine(Directory.GetCurrentDirectory(), Visitor.outputDirectory)
    if not (Directory.Exists(toDirectory)) then
      System.Console.WriteLine("Creating folder " + toDirectory);
      Directory.CreateDirectory(toDirectory) |> ignore
    let fromInfo = DirectoryInfo(fromDirectory)
    let toInfo = DirectoryInfo(toDirectory)
    if fromInfo = toInfo then
      Console.WriteLine("From and to directories are identical")

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
       Launch cmd args toDirectory // Spawn process, echoing asynchronously

    0     