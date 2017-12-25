namespace AltCover

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Reflection
open System.Resources

open Mono.Cecil
open Mono.Options

module Main =

  let mutable private help = false
  let private resources = ResourceManager("AltCover.Strings", Assembly.GetExecutingAssembly())

  let Usage (intro:string) (options:OptionSet) =
    let stderr = Console.Error
    stderr.WriteLine(intro)
    options.WriteOptionDescriptions(stderr);
    Environment.Exit(1)

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
    let options = [ ("i|inputDirectory=",
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
                              | :? IOException as io -> Console.WriteLine(io.Message)
                              | :? System.Security.SecurityException as s -> Console.WriteLine(s.Message)
                        ))
                    ("sn|strongNameKey=",
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