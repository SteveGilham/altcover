namespace AltCover

open System
open System.Diagnostics
open System.Globalization
open System.IO
open System.Reflection
open System.Resources

open Augment
open Mono.Options

module CommandLine =

  let mutable internal help = false
  let mutable internal error = false

  // Can't hard-code what with .net-core and .net-core tests as well as classic .net
  // all giving this a different namespace
  let private resource = Assembly.GetExecutingAssembly().GetManifestResourceNames()
                         |> Seq.map (fun s -> s.Substring(0, s.Length - 10)) // trim ".resources"
                         |> Seq.find (fun n -> n.EndsWith(".Strings", StringComparison.Ordinal))
  let internal resources = ResourceManager(resource , Assembly.GetExecutingAssembly())

  let internal WriteColoured (writer:TextWriter) colour operation =
       let original = Console.ForegroundColor
       try
         Console.ForegroundColor <- colour
         operation writer
       finally
         Console.ForegroundColor <- original

  let internal Usage (intro:string) (options:OptionSet) (options2:OptionSet) =
    WriteColoured Console.Error ConsoleColor.Yellow (fun w ->  w.WriteLine (resources.GetString intro)
                                                               options.WriteOptionDescriptions(w)
                                                               w.WriteLine (resources.GetString "binder")
                                                               options2.WriteOptionDescriptions(w))

  let internal Write (writer:TextWriter) colour data =
    if not(String.IsNullOrEmpty(data)) then
      WriteColoured writer colour (fun w -> w.WriteLine(data))

  let internal WriteErr line =
      Write Console.Error ConsoleColor.Yellow line
  let internal WriteOut line =
      Write Console.Out ConsoleColor.White line

  let internal Launch (cmd:string) args toDirectory =
    Directory.SetCurrentDirectory(toDirectory)
    let enquoted = "\"" + cmd.Trim([| '"'; '\'' |]) + "\""
    String.Format(CultureInfo.CurrentCulture, resources.GetString "CommandLine", enquoted,args)
    |> WriteOut

    let psi = ProcessStartInfo(enquoted,args)
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
    proc.ExitCode

  let internal doPathOperation (f: unit -> 'a) (defaultValue:'a) =
    let mutable thrown = true
    let mutable result = defaultValue
    try
        result <- f()
        thrown <- false
    with
    | :? ArgumentException as a -> WriteErr a.Message
    | :? NotSupportedException as n -> WriteErr n.Message
    | :? IOException as i -> WriteErr i.Message
    | :? System.Security.SecurityException as s -> WriteErr s.Message
    error <- error || thrown
    result

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

  let internal ProcessTrailingArguments (rest: string list) (toInfo:DirectoryInfo) =
    // If we have some arguments in rest execute that command line
        match rest |> Seq.toList with
        | [] -> 0
        | cmd::t->
           let args = String.Join(" ", (List.toArray t))
           Launch cmd args toInfo.FullName // Spawn process, echoing asynchronously