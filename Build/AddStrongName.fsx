// script relative paths to the Mono.Cecil and Mono.Options assemblies
#I "../packages/Mono.Cecil.0.10.1/lib/net40"
#I "../packages/Mono.Options.Signed.0.2.3/lib/net45"
#r "Mono.Cecil"
#r "Mono.Cecil.Pdb" // Have to put the .dll on if Mono.Cecil.Pdb is also present
#r "Mono.Options"

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open Microsoft.FSharp.Text
open Mono.Cecil
open Mono.Cecil.Pdb
open Mono.Options

// Command line argument parsing preamble ---------------------------------
let (!+) (option : string * string * (string -> unit)) (options : OptionSet) =
  let prototype, help, action = option
  options.Add(prototype, help, new System.Action<string>(action))

let Usage (intro : string) (options : OptionSet) =
  Console.Error.WriteLine(intro)
  options.WriteOptionDescriptions(Console.Error)
  Environment.Exit(1)

let assemblyName = ref ""
let keyName = ref ""
let cor32plus = ref false

let options =
  new OptionSet()
  |> !+("k|key=", "The strong naming key to apply", (fun s -> keyName := s))
  |> !+("a|assembly=", "The assembly to process", (fun s -> assemblyName := s))
  |> !+("c|cor32", "Do what CorFlags  /32BIT+ /Force does.",
        (fun x -> cor32plus := x <> null))

let rest =
  try
    options.Parse(fsi.CommandLineArgs)
  with :? OptionException ->
    Usage "Error - usage is:" options
    new List<String>()

// The meat of the script starts here ---------------------------------
// load files
let stream = new FileStream(!keyName, FileMode.Open, FileAccess.Read)
let key = new StrongNameKeyPair(stream)
let definition = AssemblyDefinition.ReadAssembly(!assemblyName)

// Do what CorFlags /32BIT+ /Force does if required
if !cor32plus then
  definition.MainModule.Attributes <-
    ModuleAttributes.Required32Bit ||| definition.MainModule.Attributes
// The headline section : strong-naming ---------------------------------
// (Re-)apply the strong name
definition.Name.HasPublicKey <- true
definition.Name.PublicKey <- key.PublicKey

let pkey = new WriterParameters()

pkey.WriteSymbols <- true
pkey.SymbolWriterProvider <- new PdbWriterProvider()
pkey.StrongNameKeyPair <- key

let file = Path.GetFileName(!assemblyName)
// Write the assembly
definition.Write(file, pkey)