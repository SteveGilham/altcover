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
  // Violate Cecil encapsulation to get the PDB path
  let GetPdbFromImage (assembly:AssemblyDefinition) =
    let m = assembly.MainModule
    let imageField = typeof<ModuleDefinition>.GetField("Image", BindingFlags.Instance ||| BindingFlags.NonPublic)
    let image = imageField.GetValue(m)
    let getDebugHeaderInfo = image.GetType().GetMethod("GetDebugHeader")
    let ba = [| 0y |]
    let oa = [| ba :> Object |]
    try 
        getDebugHeaderInfo.Invoke(image, oa) |> ignore
        let SizeOfDebugInfo = 0x18
        let header = oa.[0] :?> byte array
        let name = header
                    |> Seq.skip SizeOfDebugInfo
                    |> Seq.takeWhile (fun x -> x <> byte 0)
                    |> Seq.map (fun x -> char x)
                    |> Seq.toArray
        let name' = new String(name)
        Option.select (fun (s:String) -> s.Length > 0) name'
    with 
        | :? TargetInvocationException -> None

    |> Option.bind (Option.select File.Exists)
    
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