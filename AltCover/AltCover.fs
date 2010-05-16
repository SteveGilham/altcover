namespace AltCover

open System
open System.Collections.Generic
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
                        (fun x -> Visitor.reportPath <- x))
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
      
    // TODO -- some more pruning of arguments then useful work
    Console.WriteLine(Visitor.NameFilters.Count)
    
    // If we have some arguments in rest execute that command line
    match rest |> Seq.toList with
    | [] -> ()
    | _::[] -> ()
    | _::cmd::t-> 
       let args = String.Join(" ", (List.toArray t))
       () // TODO -- Spawn process, echoing asynchronously
    
    0     