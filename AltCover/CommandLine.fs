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
open System.Diagnostics.CodeAnalysis

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

[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
type internal UsageInfo =
  { Intro : String
    Options : OptionSet
    Options2 : OptionSet }

module internal Output =
  let mutable internal info : String -> unit = ignore
  let mutable internal warn : String -> unit = ignore
  let mutable internal echo : String -> unit = ignore
  let mutable internal error : String -> unit = ignore
  let mutable internal usage : UsageInfo -> unit = ignore

  let internal warnOn x =
    if x then warn else info

  let internal logExceptionToFile path e =
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

  [<SuppressMessage("Gendarme.Rules.Design",
                    "AbstractTypesShouldNotHavePublicConstructorsRule",
                    Justification = "The compiler ignores the 'private ()' declaration")>]
  [<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
  [<AbstractClass; Sealed>] // ~ Static class for methods with params array arguments
  type internal Format private () =
    static member Local(resource, [<ParamArray>] args) =
      String.Format(
        CultureInfo.CurrentCulture,
        resources.GetString resource,
        args)

  module internal I =
    let internal conditionalOutput condition output =
      if condition() then output()

    let internal writeColoured (writer : TextWriter) colour operation =
      let original = Console.ForegroundColor
      try
        Console.ForegroundColor <- colour
        operation writer
      finally
        Console.ForegroundColor <- original

    let internal enquotes = Map.empty |> Map.add "Windows_NT" "\""

    let internal write (writer : TextWriter) colour data =
      if not (String.IsNullOrEmpty(data)) then
        writeColoured writer colour (fun w -> w.WriteLine(data))

    let internal filter line f =
      if line
         |> String.IsNullOrEmpty
         |> not
      then f line

    module private Uncoverlet = // more event handlers
      let addHandlers (proc : Process) =
        proc.ErrorDataReceived.Add(fun e -> Output.error |> filter e.Data)
        proc.OutputDataReceived.Add(fun e -> Output.info |> filter e.Data)

    let internal launch (cmd : string) args toDirectory =
      Directory.SetCurrentDirectory(toDirectory)
      let quote =
        enquotes
        |> Map.tryFind (System.Environment.GetEnvironmentVariable "OS")
        |> Option.getOrElse String.Empty

      let enquoted = quote + cmd.Trim([| '"'; ''' |]) + quote
      Format.Local ("CommandLine", enquoted, args)
      |> Output.info

      let psi = ProcessStartInfo(enquoted, args)
      psi.WorkingDirectory <- toDirectory
      psi.CreateNoWindow <- true
      psi.UseShellExecute <- false
      psi.RedirectStandardError <- true
      psi.RedirectStandardOutput <- true
      use proc = new Process()
      proc.StartInfo <- psi

      Uncoverlet.addHandlers proc
      proc.Start() |> ignore
      proc.BeginErrorReadLine()
      proc.BeginOutputReadLine()
  #if NETCOREAPP2_0
      proc.WaitForExit()
  #else
      proc.WaitForExitCustom()
  #endif
      proc.ExitCode * (!dropReturnCode
                       |> not).ToInt32

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

    let logExceptionsToFile name extend =
      let path = Path.Combine(CoverageParameters.outputDirectories() |> Seq.head, name)
      let path' = Path.Combine(CoverageParameters.inputDirectories() |> Seq.head, name)
      exceptions |> List.iter (Output.logExceptionToFile path)
      if exceptions
         |> List.isEmpty
         |> not
      then
        let resource =
          if extend then "WrittenToEx" else "WrittenTo"
        Format.Local(resource, path, path')
        |> Output.error

    let internal validateFileSystemEntity exists message key x =
      doPathOperation (fun () ->
        if (not (String.IsNullOrWhiteSpace x)) && x
                                                  |> Path.GetFullPath
                                                  |> exists then
          true
        else
          error <-
            Format.Local(message, key, x)
            :: error
          false) false false

    let internal dnf = "DirectoryNotFound"
    let internal fnf = "FileNotFound"
    let internal iv = "InvalidValue"

    let internal validateFile file x = validateFileSystemEntity File.Exists fnf file x
    let internal findAssemblyName f =
      try
        (AssemblyName.GetAssemblyName f).ToString()
      with
      | :? ArgumentException
      | :? FileNotFoundException
      | :? System.Security.SecurityException
      | :? BadImageFormatException
      | :? FileLoadException -> String.Empty

    let internal transformCryptographicException f =
      try
        f()
      with :? CryptographicException as c ->
        (c.Message, c)
        |> SecurityException
        |> raise

    let internal stripNulls (x : String) =
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

  // "Public" interface
  let internal parseCommandLine (arguments : string array) (options : OptionSet) =
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

  let internal processHelpOption(parse : Either<string * OptionSet, string list * OptionSet>) =
    match parse with
    | Right(_, options) ->
        if help then Left("HelpText", options) else parse
    | fail -> fail

  let internal reportErrors (tag : string) extend =
    I.conditionalOutput (fun () ->
      tag
      |> String.IsNullOrWhiteSpace
      |> not
      && error
          |> List.isEmpty
          |> not) (fun () ->
      tag
      |> resources.GetString
      |> Output.error)
    error |> List.iter Output.error
    let name =
      "AltCover-"
      + DateTime.Now.ToString("yyyy-MM-dd--HH-mm-ss", CultureInfo.InvariantCulture)
      + ".log"
    I.logExceptionsToFile name extend

  let internal handleBadArguments extend arguments info =
    String.Join(" ", arguments |> Seq.map (sprintf "%A")) |> Output.echo
    Output.echo String.Empty
    reportErrors String.Empty extend
    Output.usage info

  let internal ddFlag (name : string) flag =
    (name,
     (fun (_:string) ->
       if !flag then
         error <-
           Format.Local("MultiplesNotAllowed",
              "--" + (name.Split('|') |> Seq.last)) :: error
        else
          flag := true))

  [<System.Diagnostics.CodeAnalysis.SuppressMessage(
    "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
    Justification = "AvoidSpeculativeGenerality too")>]
  let internal processTrailingArguments (rest : string list) (toInfo : DirectoryInfo) =
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

        I.launch cmd' args toInfo.FullName // Spawn process, echoing asynchronously

  let internal validateAssembly assembly x =
    if I.validateFile assembly x then
      let name = I.findAssemblyName x
      if String.IsNullOrWhiteSpace name then
        error <-
          Format.Local("NotAnAssembly", assembly, x)
          :: error
        (String.Empty, false)
      else
        (name, true)
    else
      (String.Empty, false)

  let internal validateRegexes(x : String) =
    I.doPathOperation (fun () -> I.stripNulls x) [||] false

  let internal validateStrongNameKey key x =
    if I.validateFile key x then
      I.doPathOperation (fun () ->
        let blob = File.ReadAllBytes x
        I.transformCryptographicException (fun () -> (blob |> StrongNameKeyData.Make, true)))
        (StrongNameKeyData.Empty(), false) false
    else
      (StrongNameKeyData.Empty(), false)

  [<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Design.Generic",
    "AvoidMethodWithUnusedGenericTypeRule",
    Justification="Delegation = first class functions")>]
  let internal doPathOperation = I.doPathOperation
  let internal findAssemblyName = I.findAssemblyName

  let internal validateDirectory dir x =
    I.validateFileSystemEntity Directory.Exists I.dnf dir x

  let internal ensureDirectory directory =
    I.conditionalOutput (fun () ->
      directory
      |> Directory.Exists
      |> not) (fun () ->
      Output.info
      <| Format.Local("CreateFolder", directory)
      Directory.CreateDirectory(directory) |> ignore)

  let internal validatePath path x =
    I.validateFileSystemEntity (fun _ -> true) I.iv path x

  let internal writeErr line = I.write Console.Error ConsoleColor.Yellow line
  let internal writeOut line = I.write Console.Out Console.ForegroundColor line
  let internal usageBase u =
    I.writeColoured Console.Error ConsoleColor.Yellow (fun w ->
      if u.Options.Any() || u.Options2.Any() then w.WriteLine(resources.GetString u.Intro)
      if u.Options.Any() then u.Options.WriteOptionDescriptions(w)
      if u.Options.Any() && u.Options2.Any() then
        w.WriteLine(resources.GetString "binder")
      if u.Options2.Any() then
        u.Options2.WriteOptionDescriptions(w)
      else if u.Options.Any() then
        w.WriteLine(resources.GetString "orbinder")
        w.WriteLine(resources.GetString "ImportModule")
        w.WriteLine(resources.GetString "orbinder")
        w.WriteLine(resources.GetString "version")
        )

  let internal writeResource = resources.GetString >> Output.info
  let internal writeResourceWithFormatItems s x warn =
    Format.Local(s, x)
    |> (Output.warnOn warn)
  let internal writeErrorResourceWithFormatItems s x =
    Format.Local(s, x)
    |> Output.error