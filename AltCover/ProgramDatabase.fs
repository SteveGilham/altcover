namespace AltCover

open System
open System.Collections
open System.Collections.Generic
open System.IO
open System.Reflection

open AltCover.Monads
open AltCover.Augment

open Mono.Cecil
open Mono.Cecil.Cil
open Mono.Cecil.Pdb
         
type AssemblyModel = {
         Assembly : AssemblyDefinition;
         Symbols : ISymbolReader }    

module ProgramDatabase =
  // We no longer have to violate Cecil encapsulation to get the PDB path!
  let GetPdbFromImage (assembly:AssemblyDefinition) =
    let mutable header = [| 0uy |]
    if assembly.MainModule.HasDebugHeader then
      assembly.MainModule.GetDebugHeader(&header) |> ignore
      let name = header // header is followed by UTF-8 nul terminated string
                      |> Seq.skip 0x18  // size of the debug header
                      |> Seq.takeWhile (fun x -> x <> byte 0)
                      |> Seq.toArray
      System.Text.Encoding.UTF8.GetString(name)
      |> Option.select (fun (s:String) -> s.Length > 0)
      |> Option.bind (Option.select File.Exists)
    else
      None
    
  let GetPdbWithFallback (assembly:AssemblyDefinition) =
    match GetPdbFromImage assembly with
    | None -> let fallback = Path.ChangeExtension(assembly.MainModule.FullyQualifiedName, ".pdb")
              if File.Exists(fallback) then Some fallback else None
    | pdbpath -> pdbpath


  // Ensure that we read symbols from the .pdb path we discovered.
  // Cecil currently only does the Path.ChangeExtension(path, ".pdb") fallback if left to its own devices
  // Will fail  with InvalidOperationException if there is a malformed file with the expected name
  let ReadSymbols (assembly:AssemblyDefinition) =
    GetPdbWithFallback assembly
    |> Option.iter (fun pdbpath -> 
                        let provider = new PdbReaderProvider()
                        let reader = provider.GetSymbolReader(assembly.MainModule, pdbpath)
                        assembly.MainModule.ReadSymbols(reader))