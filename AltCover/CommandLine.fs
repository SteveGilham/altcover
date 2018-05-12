namespace AltCover

open System
open System.Diagnostics
open System.Globalization
open System.IO
open System.Linq
open System.Reflection
open System.Resources

open Augment
open Mono.Options

[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
module Output =
  let mutable internal Info : (String -> unit) = ignore
  let mutable internal Echo : (String -> unit) = ignore
  let mutable internal Error : (String -> unit) = ignore
  let mutable internal Usage : ((String * obj * obj) -> unit) = ignore

  [<CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2202", Justification="Multiple Close() should be safe")>]
  let LogExceptionToFile path e =
    Directory.CreateDirectory(path |> Path.GetDirectoryName) |> ignore
    use stream = File.Open(path, FileMode.Append, FileAccess.Write)
    use writer = new StreamWriter(stream)

    let rec logException padding ex =
      ex.ToString() |> writer.WriteLine

      ex.GetType().GetProperties()
      |> Seq.filter (fun p -> [ "Message"
                                "StackTrace" ] |> Seq.exists (fun n -> n = p.Name) |> not)
      |> Seq.iter (fun p -> (padding + p.Name + " = ") |> writer.WriteLine
                            match p.GetValue(ex) with
                            | :? Exception as exx ->
                              logException ("  " + padding) exx
                            | v -> v |> sprintf "%A" |> writer.WriteLine)
    logException String.Empty e

module CommandLine =

  let mutable internal help = false
  let mutable internal error :string list = []
  let mutable internal exceptions : Exception list = []

  let internal resources = ResourceManager("AltCover.Strings" , Assembly.GetExecutingAssembly())

  let conditionalOutput condition output =
    if condition()
    then output()

  let ensureDirectory directory =
    conditionalOutput(fun () ->  directory |> Directory.Exists |> not)
        (fun () -> Output.Info <| String.Format(CultureInfo.CurrentCulture,
                                                (resources.GetString "CreateFolder"),
                                                directory)
                   Directory.CreateDirectory(directory) |> ignore)

  let internal WriteColoured (writer:TextWriter) colour operation =
       let original = Console.ForegroundColor
       try
         Console.ForegroundColor <- colour
         operation writer
       finally
         Console.ForegroundColor <- original

  let enquotes = Map.empty |> Map.add "Windows_NT" "\""

  let internal Usage ((intro:string), (o1:obj), (o2:obj)) =
    let options = o1 :?> OptionSet
    let options2 = o2 :?> OptionSet
    WriteColoured Console.Error ConsoleColor.Yellow (fun w ->  if options.Any() || options2.Any() then
                                                                  w.WriteLine (resources.GetString intro)
                                                               if options.Any() then
                                                                 options.WriteOptionDescriptions(w)
                                                               if options.Any() && options2.Any() then
                                                                 w.WriteLine (resources.GetString "binder")
                                                               if options2.Any() then
                                                                 options2.WriteOptionDescriptions(w))

  let internal Write (writer:TextWriter) colour data =
    if not(String.IsNullOrEmpty(data)) then
      WriteColoured writer colour (fun w -> w.WriteLine(data))

  let internal WriteErr line =
      Write Console.Error ConsoleColor.Yellow line
  let internal WriteOut line =
      Write Console.Out ConsoleColor.White line

  let internal Filter line f =
     if line |> String.IsNullOrEmpty |> not
     then f line

  let internal Launch (cmd:string) args toDirectory =
    Directory.SetCurrentDirectory(toDirectory)
    let quote = enquotes
                |> Map.tryFind (System.Environment.GetEnvironmentVariable "OS")
                |> Option.getOrElse String.Empty
    let enquoted = quote + cmd.Trim([| '"'; '\'' |]) + quote
    String.Format(CultureInfo.CurrentCulture, resources.GetString "CommandLine", enquoted, args)
    |> Output.Info

    let psi = ProcessStartInfo(enquoted,args)
    psi.WorkingDirectory <- toDirectory
    psi.CreateNoWindow <- true
    psi.UseShellExecute <- false
    psi.RedirectStandardError <- true
    psi.RedirectStandardOutput <- true
    use proc = new Process()
    proc.StartInfo <- psi

    proc.ErrorDataReceived.Add(fun e -> Output.Error |> Filter e.Data)
    proc.OutputDataReceived.Add(fun e -> Output.Info |> Filter e.Data)
    proc.Start() |> ignore
    proc.BeginErrorReadLine()
    proc.BeginOutputReadLine()
    proc.WaitForExit()
    proc.ExitCode

  let logException store (e : Exception) =
    error <- e.Message :: error
    if store then exceptions <- e :: exceptions

  let internal doPathOperation (f: unit -> 'a) (defaultValue:'a) store =
    let mutable result = defaultValue
    try
        result <- f()
    with
    | :? ArgumentException as a -> a :> Exception |> (logException store)
    | :? NotSupportedException as n -> n :> Exception |> (logException store)
    | :? IOException as i -> i :> Exception |> (logException store)
    | :? System.Security.SecurityException as s -> s :> Exception |> (logException store)
    result

  let internal ParseCommandLine (arguments:string array) (options:OptionSet) =
      help <- false
      error <- []
      try
          let before = arguments
                       |> Array.takeWhile (fun x -> x <> "--")
          let after = arguments
                      |> Seq.skipWhile (fun x -> x <> "--")
                      |> Seq.skipWhile (fun x -> x = "--")
                      |> Seq.toList
          let parse = options.Parse(before)
          if error |> List.isEmpty |> not || (parse.Count <> 0) then
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

  let logExceptionsToFile name =
    let path = Path.Combine (Visitor.OutputDirectory(), name)
    exceptions
    |> List.iter (Output.LogExceptionToFile path)
    if exceptions |> List.isEmpty |> not then
       String.Format (CultureInfo.CurrentCulture, resources.GetString "WrittenTo", path)
       |> Output.Error

  let ReportErrors (tag:string) =
    conditionalOutput(fun () ->  tag |> String.IsNullOrWhiteSpace |> not &&
                                 error |> List.isEmpty |> not)
                     (fun () -> tag |> resources.GetString |> Output.Error)

    error
    |> List.iter Output.Error

    let name = "AltCover-" + DateTime.Now.ToString("yyyy-MM-dd--HH-mm-ss", CultureInfo.InvariantCulture) + ".log"
    logExceptionsToFile name

  let HandleBadArguments arguments intro options1 options =
        String.Join (" ", arguments |> Seq.map (sprintf "%A"))
        |> Output.Echo
        Output.Echo String.Empty
        ReportErrors String.Empty
        Usage (intro, options1, options)