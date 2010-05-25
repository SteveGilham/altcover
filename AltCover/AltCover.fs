﻿namespace AltCover

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Reflection

open Mono.Options

module Main =

  let mutable private help = false
  
  let Usage (intro:string) (options:OptionSet) =
    Console.Error.WriteLine(intro)
    options.WriteOptionDescriptions(Console.Error);  
    Environment.Exit(-1)
  
  let (!+) (option: string * string * (string->unit)) (options:OptionSet) =
    let prototype, help, action = option
    options.Add(prototype, help, new System.Action<string>(action))

  let Launch cmd args toDirectory =
    Directory.SetCurrentDirectory(toDirectory)
    let psi = new ProcessStartInfo(cmd,args)
    psi.WorkingDirectory <- toDirectory
    psi.CreateNoWindow <- true
    psi.UseShellExecute <- false
    psi.RedirectStandardError <- true
    psi.RedirectStandardOutput <- true
    let proc = new Process()
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
  let Main arguments =
    let options = new OptionSet()
                    |> !+ (
                        "i|inputDirectory=", 
                        "Optional: The folder containing assemblies to instrument (default: current directory)",
                        (fun x -> Visitor.inputDirectory <- x))
                    |> !+ (
                        "o|outputDirectory=", 
                        "Optional: The folder to receive the instrumented assemblies and their companions (default: sub-folder 'Instrumented' current directory)",
                        (fun x -> Visitor.outputDirectory <- x))
                    |> !+ (
                        "sn|strongNameKey=", 
                        "Optional: The strong naming key to apply to instrumented assemblies (default: None)",
                        (fun x -> 
                            if not (String.IsNullOrEmpty(x)) && File.Exists(x) then
                              try 
                                  use stream = new System.IO.FileStream(x, System.IO.FileMode.Open)
                                  let pair = new StrongNameKeyPair(stream)
                                  Visitor.strongNameKey <- Some (pair)
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
                        (fun x -> Visitor.NameFilters.Add(FilterClass.File(x))))
                    |> !+ (
                        "s|assemblyFilter=", 
                        "Optional: assembly name to exclude from instrumentation (may repeat)",
                        (fun x -> Visitor.NameFilters.Add(FilterClass.Assembly(x))))
                    |> !+ (
                        "t|typeFilter=", 
                        "Optional: type name to exclude from instrumentation (may repeat)",
                        (fun x -> Visitor.NameFilters.Add(FilterClass.Type(x))))
                    |> !+ (
                        "m|methodFilter=", 
                        "Optional: method name to exclude from instrumentation (may repeat)",
                        (fun x -> Visitor.NameFilters.Add(FilterClass.Method(x))))
                    |> !+ (
                        "a|attributeFilter=", 
                        "Optional: attribute name to exclude from instrumentation (may repeat)",
                        (fun x -> Visitor.NameFilters.Add(FilterClass.Attribute(x))))
                    |> !+ (                    
                        "?|help|h",
                         "Prints out the options.",
                          (fun x -> help <- x <> null))
                    |> !+ (                    
                        "<>",
                        String.Empty,
                          (fun x -> ()))                          
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
    let fromInfo = new DirectoryInfo(fromDirectory)
    let toInfo = new DirectoryInfo(toDirectory)
    if fromInfo = toInfo then
      Console.WriteLine("From and to directories are identical")
      
    let files = fromInfo.GetFiles()
    
    // Copy all the files that aren't symbol-bearing assemblies into the target directory
    let assemblies = 
        files 
        |> Seq.fold (fun (accumulator : string list) (info:FileInfo) ->
             let assemblyPdb = ProgramDatabase.PdbPathExists info.FullName
             let target = Path.Combine (toInfo.FullName, info.Name)
             File.Copy(info.FullName, target, true) 
             match assemblyPdb with
             | None -> accumulator
             | _ -> if Visitor.IsIncluded info.FullName then
                        info.FullName :: accumulator
                    else
                        accumulator
          ) []
        
    let assemblyNames = 
        assemblies 
        |> Seq.map (fun path -> Path.GetFileNameWithoutExtension(path))
        |> Seq.toList

    // Ensure we always have an absolute file path here.
    Visitor.reportPath <- Path.Combine(Directory.GetCurrentDirectory(), Visitor.reportPath)

    let reporter, document = Report.ReportGenerator ()
    let visitors = [ reporter ; Instrument.InstrumentGenerator assemblyNames ]
    Visitor.Visit visitors assemblies
    document.Save(Visitor.reportPath)
    
    
    // If we have some arguments in rest execute that command line
    match rest |> Seq.toList with
    | [] -> ()
    | cmd::t-> 
       let args = String.Join(" ", (List.toArray t))
       Launch cmd args toDirectory // Spawn process, echoing asynchronously
    
    0     