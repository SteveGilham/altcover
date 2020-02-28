namespace AltCover

open System
open System.Diagnostics
open System.Globalization
open System.IO
open System.Linq
open System.Reflection
open System.Resources
open System.Security
open System.Security.Cryptography
open System.Text.RegularExpressions

open Augment
open BlackFox.CommandLine
open Mono.Options

#if NETCOREAPP2_0
#else
[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
module internal Process =
  type System.Diagnostics.Process with
    // Work around observed unreliability of WaitForExit()
    // with an unbounded wait under mono on travis-ci
    member self.WaitForExitCustom() =
      let rec loop() =
        try
          if self.WaitForExit(1000) then
            // allow time for I/O redirection to complete
            System.Threading.Thread.Sleep(1000)
            if self.HasExited then () else loop()
          else
            loop()
        with
        | :? SystemException
        | :? InvalidOperationException
        | :? System.ComponentModel.Win32Exception -> ()
      if System.Environment.GetEnvironmentVariable("OS") = "Windows_NT" && "Mono.Runtime"
                                                                           |> Type.GetType
                                                                           |> isNull then // only rely on .net Framework on Windows
        self.WaitForExit()
      else
        loop()

open Process
#endif

type internal StringSink = Action<String> // delegate of string -> unit

[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage; NoComparison>]
type internal UsageInfo =
  { Intro : String
    Options : OptionSet
    Options2 : OptionSet }

module internal Output =
  let mutable internal Info : String -> unit = ignore
  let mutable internal Warn : String -> unit = ignore
  let mutable internal Echo : String -> unit = ignore
  let mutable internal Error : String -> unit = ignore
  let mutable internal Usage : UsageInfo -> unit = ignore

  let internal WarnOn x =
    if x then Warn else Info

  [<CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2202",
                                 Justification = "Multiple Close() should be safe")>]
  let LogExceptionToFile path e =
    Directory.CreateDirectory(path |> Path.GetDirectoryName) |> ignore
    use stream = File.Open(path, FileMode.Append, FileAccess.Write)
    use writer = new StreamWriter(stream)

    let rec logException padding ex =
      ex.ToString() |> writer.WriteLine
      ex.GetType().GetProperties()
      |> Seq.filter (fun p ->
           [ "Message"; "StackTrace" ]
           |> Seq.exists (fun n -> n = p.Name)
           |> not)
      |> Seq.iter (fun p ->
           (padding + p.Name + " = ") |> writer.WriteLine
           match p.GetValue(ex) with
           | :? Exception as exx -> logException ("  " + padding) exx
           | v ->
               v
               |> sprintf "%A"
               |> writer.WriteLine)
    logException String.Empty e

module internal CommandLine =

  let mutable internal help = false
  let mutable internal error : string list = []
  let mutable internal exceptions : Exception list = []
  let internal dropReturnCode = ref false // ddFlag

  let internal resources =
    ResourceManager("AltCover.Strings", Assembly.GetExecutingAssembly())

  let conditionalOutput condition output =
    if condition() then output()

  let ensureDirectory directory =
    conditionalOutput (fun () ->
      directory
      |> Directory.Exists
      |> not) (fun () ->
      Output.Info
      <| String.Format
           (CultureInfo.CurrentCulture, (resources.GetString "CreateFolder"), directory)
      Directory.CreateDirectory(directory) |> ignore)

  let internal WriteColoured (writer : TextWriter) colour operation =
    let original = Console.ForegroundColor
    try
      Console.ForegroundColor <- colour
      operation writer
    finally
      Console.ForegroundColor <- original

  let enquotes = Map.empty |> Map.add "Windows_NT" "\""

  let internal UsageBase u =
    WriteColoured Console.Error ConsoleColor.Yellow (fun w ->
      if u.Options.Any() || u.Options2.Any() then w.WriteLine(resources.GetString u.Intro)
      if u.Options.Any() then u.Options.WriteOptionDescriptions(w)
      if u.Options.Any() && u.Options2.Any() then
        w.WriteLine(resources.GetString "binder")
      if u.Options2.Any() then
        u.Options2.WriteOptionDescriptions(w)
      else if u.Options.Any() then
        w.WriteLine(resources.GetString "orbinder")
        w.WriteLine(resources.GetString "ipmo")
        w.WriteLine(resources.GetString "orbinder")
        w.WriteLine(resources.GetString "version")
      )

  let internal Write (writer : TextWriter) colour data =
    if not (String.IsNullOrEmpty(data)) then
      WriteColoured writer colour (fun w -> w.WriteLine(data))

  let internal WriteErr line = Write Console.Error ConsoleColor.Yellow line
  let internal WriteOut line = Write Console.Out Console.ForegroundColor line

  let internal Filter line f =
    if line
       |> String.IsNullOrEmpty
       |> not
    then f line

  module private Uncoverlet = // more event handlers
    let AddHandlers (proc : Process) =
      proc.ErrorDataReceived.Add(fun e -> Output.Error |> Filter e.Data)
      proc.OutputDataReceived.Add(fun e -> Output.Info |> Filter e.Data)

  let internal Launch (cmd : string) args toDirectory =
    Directory.SetCurrentDirectory(toDirectory)
    let quote =
      enquotes
      |> Map.tryFind (System.Environment.GetEnvironmentVariable "OS")
      |> Option.getOrElse String.Empty

    let enquoted = quote + cmd.Trim([| '"'; ''' |]) + quote
    String.Format
      (CultureInfo.CurrentCulture, resources.GetString "CommandLine", enquoted, args)
    |> Output.Info

    let psi = ProcessStartInfo(enquoted, args)
    psi.WorkingDirectory <- toDirectory
    psi.CreateNoWindow <- true
    psi.UseShellExecute <- false
    psi.RedirectStandardError <- true
    psi.RedirectStandardOutput <- true
    use proc = new Process()
    proc.StartInfo <- psi

    Uncoverlet.AddHandlers proc
    proc.Start() |> ignore
    proc.BeginErrorReadLine()
    proc.BeginOutputReadLine()
#if NETCOREAPP2_0
    proc.WaitForExit()
#else
    proc.WaitForExitCustom()
#endif
    proc.ExitCode * (!dropReturnCode
                     |> not
                     |> Increment)

  let logException store (e : Exception) =
    error <- e.Message :: error
    if store then exceptions <- e :: exceptions

  let internal doPathOperation (f : unit -> 'a) (defaultValue : 'a) store =
    let mutable result = defaultValue
    try
      result <- f()
    with
    | :? ArgumentException as a -> a :> Exception |> (logException store)
    | :? NotSupportedException as n -> n :> Exception |> (logException store)
    | :? IOException as i -> i :> Exception |> (logException store)
    | :? System.Security.SecurityException as s -> s :> Exception |> (logException store)
    result

  let internal ParseCommandLine (arguments : string array) (options : OptionSet) =
    help <- false
    error <- []
    try
      let before = arguments |> Array.takeWhile (fun x -> x <> "--")

      let after =
        arguments
        |> Seq.skipWhile (fun x -> x <> "--")
        |> Seq.skipWhile (fun x -> x = "--")
        |> Seq.toList

      let parse = options.Parse(before)
      if error
         |> List.isEmpty
         |> not
         || (parse.Count <> 0) then
        Left("UsageError", options)
      else
        Right(after, options)
    with :? OptionException -> Left("UsageError", options)

  let internal ProcessHelpOption(parse : Either<string * OptionSet, string list * OptionSet>) =
    match parse with
    | Right(_, options) ->
        if help then Left("HelpText", options) else parse
    | fail -> fail

  [<System.Diagnostics.CodeAnalysis.SuppressMessage(
    "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
    Justification = "AvoidSpeculativeGenerality too")>]
  let internal ProcessTrailingArguments (rest : string list) (toInfo : DirectoryInfo) =
    // If we have some arguments in rest execute that command line
    match rest |> Seq.toList with
    | [] -> 0
    | cmd :: t ->
        let args =
          t
          |> CmdLine.fromSeq
          |> CmdLine.toString

        let cmd' =
          [ cmd ]
          |> CmdLine.fromSeq
          |> CmdLine.toString

        Launch cmd' args toInfo.FullName // Spawn process, echoing asynchronously

  let logExceptionsToFile name extend =
    let path = Path.Combine(Visitor.OutputDirectories() |> Seq.head, name)
    let path' = Path.Combine(Visitor.InputDirectories() |> Seq.head, name)
    exceptions |> List.iter (Output.LogExceptionToFile path)
    if exceptions
       |> List.isEmpty
       |> not
    then
      let resource =
        if extend then "WrittenToEx" else "WrittenTo"
      String.Format(CultureInfo.CurrentCulture, resources.GetString resource, path, path')
      |> Output.Error

  let ReportErrors (tag : string) extend =
    conditionalOutput (fun () ->
      tag
      |> String.IsNullOrWhiteSpace
      |> not
      && error
         |> List.isEmpty
         |> not) (fun () ->
      tag
      |> resources.GetString
      |> Output.Error)
    error |> List.iter Output.Error
    let name =
      "AltCover-"
      + DateTime.Now.ToString("yyyy-MM-dd--HH-mm-ss", CultureInfo.InvariantCulture)
      + ".log"
    logExceptionsToFile name extend

  let HandleBadArguments extend arguments info =
    String.Join(" ", arguments |> Seq.map (sprintf "%A")) |> Output.Echo
    Output.Echo String.Empty
    ReportErrors String.Empty extend
    Output.Usage info

  let internal ValidateFileSystemEntity exists message key x =
    doPathOperation (fun () ->
      if (not (String.IsNullOrWhiteSpace x)) && x
                                                |> Path.GetFullPath
                                                |> exists then
        true
      else
        error <-
          String.Format(CultureInfo.CurrentCulture, resources.GetString message, key, x)
          :: error
        false) false false

  let internal dnf = "DirectoryNotFound"
  let internal fnf = "FileNotFound"
  let internal iv = "InvalidValue"

  let internal ValidateDirectory dir x =
    ValidateFileSystemEntity Directory.Exists dnf dir x
  let internal ValidateFile file x = ValidateFileSystemEntity File.Exists fnf file x
  let internal ValidatePath path x =
    ValidateFileSystemEntity (fun _ -> true) iv path x

  let internal FindAssemblyName f =
    try
      (AssemblyName.GetAssemblyName f).ToString()
    with
    | :? ArgumentException
    | :? FileNotFoundException
    | :? System.Security.SecurityException
    | :? BadImageFormatException
    | :? FileLoadException -> String.Empty

  let internal ValidateAssembly assembly x =
    if ValidateFile assembly x then
      let name = FindAssemblyName x
      if String.IsNullOrWhiteSpace name then
        error <-
          String.Format
            (CultureInfo.CurrentCulture, resources.GetString "NotAnAssembly", assembly, x)
          :: error
        (String.Empty, false)
      else
        (name, true)
    else
      (String.Empty, false)

  let internal TransformCryptographicException f =
    try
      f()
    with :? CryptographicException as c ->
      (c.Message, c)
      |> SecurityException
      |> raise

  let internal ValidateStrongNameKey key x =
    if ValidateFile key x then
      doPathOperation (fun () ->
        let blob = File.ReadAllBytes x
        TransformCryptographicException (fun () -> (blob |> StrongNameKeyData.Make, true)))
        (StrongNameKeyData.Empty(), false) false
    else
      (StrongNameKeyData.Empty(), false)

  let private stripNulls (x : String) =
    let descape (s : string) = s.Replace(char 0, ';')

    let qRegex (s : String) =
      if s.Substring(0, 1) = "?" then
        { Regex = Regex <| s.Substring(1)
          Sense = Include }
      else
        { Regex = Regex s
          Sense = Exclude }

    let transform array =
      array
      |> Array.map (descape >> qRegex)

    x.Replace(";;", "\u0000").Split([| ";" |], StringSplitOptions.RemoveEmptyEntries)
      |> transform

  let internal ValidateRegexes(x : String) =
    doPathOperation (fun () -> stripNulls x) [||] false

  let internal ddFlag (name : string) flag =
    (name,
     (fun (_:string) ->
       if !flag then
         error <-
           String.Format
             (CultureInfo.CurrentCulture, resources.GetString "MultiplesNotAllowed",
              "--" + (name.Split('|') |> Seq.last)) :: error
       else
         flag := true))