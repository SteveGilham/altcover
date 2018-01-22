namespace AltCover

open System
open System.Diagnostics
open System.IO
open System.Reflection
open System.Resources

open Mono.Options

module CommandLine =

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

  let internal Usage (intro:string) (options:OptionSet) =
    WriteColoured Console.Error ConsoleColor.Yellow (fun w ->  w.WriteLine (resources.GetString intro)
                                                               options.WriteOptionDescriptions(w))

  let internal Write (writer:TextWriter) colour data =
    if not(String.IsNullOrEmpty(data)) then
      WriteColoured writer colour (fun w -> w.WriteLine(data))

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