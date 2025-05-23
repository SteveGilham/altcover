﻿namespace AltCover

open System
open System.Diagnostics
open System.Globalization
open System.IO
open System.IO.Compression
open System.Linq
open System.Reflection
open System.Resources
open System.Security
open System.Security.Cryptography
open System.Text.RegularExpressions

open BlackFox.CommandLine
open Mono.Options
open System.Diagnostics.CodeAnalysis
open AltCover.Shared

[<ExcludeFromCodeCoverage>]
module internal Process =
  type System.Diagnostics.Process with
    // Work around observed unreliability of WaitForExit()
    // with an unbounded wait under mono on travis-ci
    member self.WaitForExitCustom() =
      // [<TailCall>]
      let rec loop () =
        try
          if self.WaitForExit(1000) then
            // allow time for I/O redirection to complete
            System.Threading.Thread.Sleep(1000)
            if self.HasExited then () else loop ()
          else
            loop ()
        with
        | :? SystemException
        | :? InvalidOperationException
        | :? System.ComponentModel.Win32Exception -> ()

      if "Mono.Runtime" |> Type.GetType |> isNull then
        self.WaitForExit()
      else
        loop ()

open Process

module internal Zip =
  let internal save (document: Stream -> unit) (report: string) (zip: bool) =
    if zip then
      use file = File.OpenWrite(report + ".zip")
      file.SetLength 0L

      use archive =
        new ZipArchive(file, ZipArchiveMode.Create)

      let entry =
        report |> Path.GetFileName |> archive.CreateEntry

      use sink = entry.Open()
      document sink
    else
      use file = File.OpenWrite report
      file.SetLength 0L
      document file

  [<SuppressMessage("Gendarme.Rules.Correctness",
                    "EnsureLocalDisposalRule",
                    Justification =
                      "This rule does not check to see whether this method opens the stream for a consumer to use and dispose")>]
  [<SuppressMessage("Microsoft.Reliability",
                    "CA2000:Dispose objects before losing scope",
                    Justification = "ditto, ditto.")>]
  let internal openUpdate (report: string) (zipped: bool) =
    if zipped then
      let zip =
        ZipFile.Open(report + ".zip", ZipArchiveMode.Update)

      let entry =
        report |> Path.GetFileName |> zip.GetEntry

      let stream =
        if entry.IsNotNull then
          entry.Open()
        else
          new MemoryStream() :> Stream

      (zip, stream)
    else
      let stream =
        new FileStream(
          report,
          FileMode.Open,
          FileAccess.ReadWrite,
          FileShare.None,
          4096,
          FileOptions.SequentialScan
        )

      (null, stream :> Stream)

  let internal openUpdateReport format (report: string) (zipped: bool) =
    let container, stream =
      openUpdate report zipped

    use _ = container
    use _ = stream
    DocumentType.LoadReportStream format stream

type internal StringSink = Action<String>

module internal CommandLine =

  let mutable internal verbosity = 0
  let mutable internal help = false
  let mutable internal error: string list = []

  let mutable internal exceptions: Exception list =
    []

  let internal dropReturnCode = ref false // ddFlag

  [<SuppressMessage("Gendarme.Rules.Design",
                    "AbstractTypesShouldNotHavePublicConstructorsRule",
                    Justification = "The compiler ignores the 'private ()' declaration")>]
  [<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
  [<AbstractClass; Sealed>] // ~ Static class for methods with params array arguments
  type internal Format private () =
    static member Local(resource, [<ParamArray>] args) =
      String.Format(CultureInfo.CurrentCulture, Output.resources.GetString resource, args)

  module internal I =
    let internal conditionalOutput condition output =
      if condition () then
        output ()

    let internal writeColoured (writer: TextWriter) colour operation =
      let original = Console.ForegroundColor

      try
        Console.ForegroundColor <- colour
        operation writer
      finally
        Console.ForegroundColor <- original

    let internal enquotes =
      Map.empty |> Map.add "Windows_NT" "\""

    let internal write (writer: TextWriter) colour data =
      if not (String.IsNullOrEmpty(data)) then
        writeColoured writer colour _.WriteLine(data)

    let internal filter line f =
      if line |> String.IsNullOrEmpty |> not then
        f line

    module private Uncoverlet =
      let addHandlers (proc: Process) =
        proc.ErrorDataReceived.Add(fun e -> Output.error |> filter e.Data)
        proc.OutputDataReceived.Add(fun e -> Output.info |> filter e.Data)

    let internal launch (cmd: string) args toDirectory =
      Directory.SetCurrentDirectory(toDirectory)

      let quote =
        enquotes
        |> Map.tryFind (System.Environment.GetEnvironmentVariable "OS")
        |> Option.defaultValue String.Empty

      let enquoted =
        quote + cmd.Trim([| '"'; ''' |]) + quote

      Format.Local("CommandLine", enquoted, args)
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
      proc.WaitForExitCustom()

      proc.ExitCode
      * (dropReturnCode.Value |> not).ToInt32

    let logException store (e: Exception) =
      error <- e.Message :: error

      if store then
        exceptions <- e :: exceptions

    let internal doPathOperation (f: unit -> 'a) (defaultValue: 'a) store =
      PathOperation.DoPathOperation f (fun x ->
        x |> (logException store)
        defaultValue)

    let private handledRetryFault (x: exn) =
      (x :? IOException)
      || (x :? System.Security.SecurityException)
      || (x :? UnauthorizedAccessException)

    [<SuppressMessage("Gendarme.Rules.Smells",
                      "AvoidLongParameterListsRule",
                      Justification = "Long enough but no longer")>]
    //Stopped working in 9.0.200 [<TailCall>]
    let rec internal doRetry action log limit (rest: int) depth f =
      try
        action f
      with x when handledRetryFault x ->
        if depth < limit then
          Threading.Thread.Sleep(rest)
          doRetry action log limit rest (depth + 1) f
        else
          x.ToString() |> log

    let logExceptionsToFile name extend =
      let path =
        Path.Combine(
          CoverageParameters.outputDirectories ()
          |> Seq.head,
          name
        )

      let path' =
        Path.Combine(CoverageParameters.inputDirectories () |> Seq.head, name)

      exceptions
      |> List.iter (Output.logExceptionToFile path)

      if exceptions |> List.isEmpty |> not then
        let resource =
          if extend then
            "WrittenToEx"
          else
            "WrittenTo"

        Format.Local(resource, path, path')
        |> Output.error

    let internal validateFileSystemEntity exists message key x =
      doPathOperation
        (fun () ->
          if
            (not (String.IsNullOrWhiteSpace x))
            && x |> canonicalPath |> exists
          then
            true
          else
            error <- Format.Local(message, key, x) :: error
            false)
        false
        false

    let internal dnf = "DirectoryNotFound"
    let internal fnf = "FileNotFound"
    let internal iv = "InvalidValue"

    let internal validateFile file x =
      validateFileSystemEntity File.Exists fnf file x

    type SecurityException with
      [<SuppressMessage("Gendarme.Rules.Design.Generic",
                        "AvoidMethodWithUnusedGenericTypeRule",
                        Justification = "Matches clause type")>]
      static member Throw<'T>(e: exn) : 'T =
        (e.Message, e) |> SecurityException |> raise

    let internal transformCryptographicException f =
      try
        f ()
      with :? CryptographicException as c ->
        c |> SecurityException.Throw

    [<SuppressMessage("Microsoft.Globalization",
                      "CA1307:SpecifyStringComparison",
                      Justification = "No suitable overload in netstandard2.0/net472")>]
    let internal stripNulls (x: String) =
      let descape (s: string) = s.Replace(char 0, ';')

      let qRegex (s: String) =
        if s.Substring(0, 1) == "?" then
          { Regex = Regex <| s.Substring(1)
            Sense = Include }
        else
          { Regex = Regex s; Sense = Exclude }

      let transform array = array |> Array.map (descape >> qRegex)

      x.Replace(";;", "\u0000").Split([| ";" |], StringSplitOptions.RemoveEmptyEntries)
      |> transform

  // "Public" interface
  let internal parseCommandLine (arguments: string array) (options: OptionSet) =
    help <- false
    error <- []

    try
      let before =
        arguments |> Array.takeWhile (fun x -> x != "--")

      let after =
        arguments
        |> Seq.skipWhile (fun x -> x != "--")
        |> Seq.skipWhile (fun x -> x == "--")
        |> Seq.toList

      let parse = options.Parse(before)

      if error |> List.isEmpty |> not || (parse.Count <> 0) then
        Left("UsageError", options)
      else
        Right(after, options)
    with :? OptionException ->
      Left("UsageError", options)

  let internal processHelpOption
    (parse: Either<string * OptionSet, string list * OptionSet>)
    =
    match parse with
    | Right(_, options) ->
      if help then
        Left("HelpText", options)
      else
        parse
    | fail -> fail

  let internal applyVerbosity () =
    if verbosity >= 0 then
      Output.verbose <- ignore

    if verbosity >= 1 then
      Output.info <- ignore
      Output.echo <- ignore

    if verbosity >= 2 then
      Output.warn <- ignore

    if verbosity >= 3 then
      Output.error <- ignore
      Output.usage <- ignore

  let internal reportErrors (tag: string) extend =
    I.conditionalOutput
      (fun () ->
        tag |> String.IsNullOrWhiteSpace |> not
        && error |> List.isEmpty |> not)
      (fun () -> tag |> Output.resources.GetString |> Output.error)

    error |> List.iter Output.error

    let name =
      "AltCover-"
      + DateTime.Now.ToString("yyyy-MM-dd--HH-mm-ss", CultureInfo.InvariantCulture)
      + ".log"

    I.logExceptionsToFile name extend

  let internal handleBadArguments extend arguments info =
    String.Join(" ", arguments |> Seq.map (sprintf "%A"))
    |> Output.echo

    Output.echo String.Empty
    reportErrors String.Empty extend
    Output.usage info

  let internal ddFlag (name: string) (flag: bool ref) =
    (name,
     (fun (_: string) ->
       if flag.Value then
         error <-
           Format.Local("MultiplesNotAllowed", "--" + (name.Split('|') |> Seq.last))
           :: error
       else
         flag.Value <- true))

  [<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Maintainability",
                                                    "AvoidUnnecessarySpecializationRule",
                                                    Justification =
                                                      "AvoidSpeculativeGenerality too")>]
  let internal processTrailingArguments (rest: string list) (toInfo: DirectoryInfo) =
    // If we have some arguments in rest execute that command line
    match rest |> Seq.toList with
    | [] -> 0
    | cmd :: t ->
      let args =
        t |> CmdLine.fromSeq |> CmdLine.toString

      let cmd' =
        [ cmd ] |> CmdLine.fromSeq |> CmdLine.toString

      I.launch cmd' args toInfo.FullName // Spawn process, echoing asynchronously

  let internal validateAssembly assembly x =
    if I.validateFile assembly x then
      let name =
        AssemblyConstants.findAssemblyName x

      if String.IsNullOrWhiteSpace name then
        error <-
          Format.Local("NotAnAssembly", assembly, x)
          :: error

        (String.Empty, false)
      else
        (name, true)
    else
      (String.Empty, false)

  let internal validateRegexes (x: String) =
    I.doPathOperation (fun () -> I.stripNulls x) [||] false

  let internal validateStrongNameKey key x =
    if I.validateFile key x then
      I.doPathOperation
        (fun () ->
          let blob = File.ReadAllBytes x

          I.transformCryptographicException (fun () ->
            (blob |> StrongNameKeyData.Make, true)))
        (StrongNameKeyData.Empty(), false)
        false
    else
      (StrongNameKeyData.Empty(), false)

  [<System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Design.Generic",
                                                    "AvoidMethodWithUnusedGenericTypeRule",
                                                    Justification =
                                                      "Delegation = first class functions")>]
  let internal doPathOperation =
    I.doPathOperation

  let internal validateDirectory dir x =
    I.validateFileSystemEntity Directory.Exists I.dnf dir x

  let internal ensureDirectory directory =
    I.conditionalOutput (fun () -> directory |> Directory.Exists |> not) (fun () ->
      if
        verbosity < 1 // implement it early here
      then
        Output.info
        <| Format.Local("CreateFolder", directory)

      Directory.CreateDirectory(directory) |> ignore)

  let internal validatePath path x =
    I.validateFileSystemEntity (fun _ -> true) I.iv path x

  let internal writeErr line =
    I.write Console.Error ConsoleColor.Yellow line

  let internal writeOut line =
    I.write Console.Out Console.ForegroundColor line

  let internal usageBase u =
    I.writeColoured Console.Error ConsoleColor.Yellow (fun w ->
      w.WriteLine(Output.resources.GetString u.Intro)
      u.Options.WriteOptionDescriptions(w)

      if u.Options.Any() && u.Options2.Any() then
        w.WriteLine(Output.resources.GetString "orbinder")

      if u.Options2.Any() then
        w.WriteLine("  Runner")
        u.Options2.WriteOptionDescriptions(w)

      w.WriteLine(Output.resources.GetString "orbinder")
      w.WriteLine(Output.resources.GetString "ImportModule")
      w.WriteLine(Output.resources.GetString "orbinder")
      w.WriteLine(Output.resources.GetString "Version")
      w.WriteLine(Output.resources.GetString "orglobal")
      w.WriteLine(Output.resources.GetString "TargetsPath"))

  let internal writeResource =
    Output.resources.GetString >> Output.info

  let internal writeResourceWithFormatItems s x warn =
    Format.Local(s, x) |> (Output.warnOn warn)

  let internal writeErrorResourceWithFormatItems s x = Format.Local(s, x) |> Output.error

  [<SuppressMessage("Gendarme.Rules.Performance",
                    "AvoidUncalledPrivateCodeRule",
                    Justification = "An internal API")>]
  let internal toConsole () =
    Output.error <- writeErr
    Output.usage <- usageBase
    Output.echo <- writeErr
    Output.info <- writeOut
    Output.verbose <- writeOut
    Output.warn <- writeOut

[<assembly: SuppressMessage("Gendarme.Rules.Exceptions",
                            "InstantiateArgumentExceptionCorrectlyRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "AltCover.CommandLine/I/transform@294::Invoke(System.String[])",
                            Justification = "Inlined library code")>]
()