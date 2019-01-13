namespace AltCover

open System
open System.Collections.Generic
open System.IO

open Mono.Cecil
open Mono.Cecil.Cil
open Mono.Cecil.Mdb
open Mono.Cecil.Pdb

module internal ProgramDatabase =
  let internal SymbolFolders = List<String>()

  // We no longer have to violate Cecil encapsulation to get the PDB path
  // but we do to get the embedded PDB info
  let internal getEmbed =
    (typeof<Mono.Cecil.AssemblyDefinition>.Assembly.GetTypes()
     |> Seq.filter (fun m -> m.FullName = "Mono.Cecil.Mixin")
     |> Seq.head)
      .GetMethod("GetEmbeddedPortablePdbEntry")

  let internal GetEmbeddedPortablePdbEntry(assembly : AssemblyDefinition) =
    getEmbed.Invoke(null, [| assembly.MainModule.GetDebugHeader() :> obj |]) :?> ImageDebugHeaderEntry

  let GetPdbFromImage(assembly : AssemblyDefinition) =
    Some assembly.MainModule
    |> Option.filter (fun x -> x.HasDebugHeader)
    |> Option.map (fun x -> x.GetDebugHeader())
    |> Option.filter (fun x -> x.HasEntries)
    |> Option.bind (fun x -> x.Entries |> Seq.tryFind (fun t -> true))
    |> Option.map (fun x -> x.Data)
    |> Option.filter (fun x -> x.Length > 0x18)
    |> Option.map (fun x ->
         x
         |> Seq.skip 0x18 // size of the debug header
         |> Seq.takeWhile (fun x -> x <> byte 0)
         |> Seq.toArray
         |> System.Text.Encoding.UTF8.GetString)
    |> Option.filter (fun s -> s.Length > 0)
    |> Option.filter (fun s ->
         File.Exists s || (s = (assembly.Name.Name + ".pdb") && (assembly
                                                                 |> GetEmbeddedPortablePdbEntry
                                                                 |> isNull
                                                                 |> not)))

  let GetSymbolsByFolder fileName folderName =
    let name = Path.Combine(folderName, fileName)
    let fallback = Path.ChangeExtension(name, ".pdb")
    if File.Exists(fallback) then Some fallback
    else
      let fallback2 = name + ".mdb"
      // Note -- the assembly path, not the mdb path, because GetSymbolReader wants the assembly path for Mono
      if File.Exists(fallback2) then Some name
      else None

  let GetPdbWithFallback(assembly : AssemblyDefinition) =
    match GetPdbFromImage assembly with
    | None ->
      let foldername = Path.GetDirectoryName assembly.MainModule.FileName
      let filename = Path.GetFileName assembly.MainModule.FileName
      foldername :: (Seq.toList SymbolFolders)
      |> Seq.map (GetSymbolsByFolder filename)
      |> Seq.choose id
      |> Seq.tryFind (fun _ -> true)
    | pdbpath -> pdbpath

  // Ensure that we read symbols from the .pdb path we discovered.
  // Cecil currently only does the Path.ChangeExtension(path, ".pdb") fallback if left to its own devices
  // Will fail  with InvalidOperationException if there is a malformed file with the expected name
  let ReadSymbols(assembly : AssemblyDefinition) =
    GetPdbWithFallback assembly
    |> Option.iter (fun pdbpath ->
         let provider : ISymbolReaderProvider =
           if pdbpath.EndsWith(".pdb", StringComparison.OrdinalIgnoreCase) then
             PdbReaderProvider() :> ISymbolReaderProvider
           else MdbReaderProvider() :> ISymbolReaderProvider

         let reader = provider.GetSymbolReader(assembly.MainModule, pdbpath)
         assembly.MainModule.ReadSymbols(reader))