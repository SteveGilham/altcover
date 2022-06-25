namespace AltCover

open System
open System.Collections.Generic
open System.IO

open Mono.Cecil
open Mono.Cecil.Cil
open Mono.Cecil.Mdb
open Mono.Cecil.Pdb
open Mono.Cecil.Rocks

open AltCover.Shared

[<RequireQualifiedAccess>]
module internal ProgramDatabase =
  // "Public" "field"
  let internal symbolFolders = List<String>()

  // Implementation details
  module internal I =

    // We no longer have to violate Cecil encapsulation to get the PDB path
    // but we do to get the embedded PDB info
    let internal getEmbed =
      (typeof<Mono.Cecil.AssemblyDefinition>.Assembly.GetTypes ()
       |> Seq.filter (fun m -> m.FullName == "Mono.Cecil.Mixin")
       |> Seq.head)
        .GetMethod("GetEmbeddedPortablePdbEntry")

    let internal getEmbeddedPortablePdbEntry (assembly: AssemblyDefinition) =
      getEmbed.Invoke(null, [| assembly.MainModule.GetDebugHeader() :> obj |])
      :?> ImageDebugHeaderEntry

    let internal getSymbolsByFolder fileName folderName =
      let name =
        Path.Combine(folderName, fileName)

      let fallback =
        Path.ChangeExtension(name, ".pdb")

      if File.Exists(fallback) then
        Some fallback
      else
        let fallback2 = name + ".mdb"
        // Note -- the assembly path, not the mdb path, because GetSymbolReader wants the assembly path for Mono
        if File.Exists(fallback2) then
          Some name
        else
          None

    let lead (bytes: byte array) =
      (int bytes[0])
      ||| ((int bytes[1]) <<< 8)
      ||| ((int bytes[2]) <<< 16)
      ||| ((int bytes[3]) <<< 24)

    let getAssemblyTokens (assembly: AssemblyDefinition) =
      let m = assembly.MainModule

      let t =
        m.GetDebugHeader().Entries
        |> Seq.filter (fun e ->
          e.Data.Length > 0x18
          && e.Directory.Type = ImageDebugType.CodeView)
        |> Seq.map (fun x -> x.Data)
        |> Seq.filter (fun x -> lead x = 1396986706)
        |> Seq.map (fun x -> x |> Array.skip 4 |> Array.take 16 |> System.Guid)
        |> Seq.toList

      (m.Mvid, t)

    let checkMdb (b: BinaryReader) =

      let magic = b.ReadInt64()
      let major = b.ReadInt32()
      let minor = b.ReadInt32()

      magic = 5037318119232611860L
      && major = 50
      && minor = 0

    let checkPdb (b: BinaryReader) =
      let start = b.ReadInt32()
      if start = 0x424a5342 // portable format
      then
        let major = b.ReadInt16()
        let minor = b.ReadInt16()
        let reserved = b.ReadInt32()
        let versionSize = b.ReadInt32()
        let version = b.ReadBytes(versionSize)
        let flags = b.ReadInt16()
        let streams = b.ReadInt16() |> int // # of stream headers
        let headers =
          {1..streams}
          |> Seq.map (fun _ -> let offset = b.ReadInt32()
                               let size = b.ReadInt32()
                               let name = Seq.initInfinite id
                                          |> Seq.map (fun _ -> b.ReadInt32())
                                          |> Seq.takeWhile (fun x -> x > 0xffffff)
                                          |> Seq.toArray
                               (offset, size, name))
          |> Seq.toArray

        // don't expect any other #Pdb??? streams
        let guid = headers |> Seq.tryFind (fun (_, _, x) -> x = [| 0x62645023 |] )
        let ok = guid.IsSome

        if ok
        then
          let (o, _, _ ) = guid.Value
          b.BaseStream.Seek(int64 o, SeekOrigin.Begin) |> ignore

        ok &&
        reserved = 0 &&
        (int flags) = 0

      else
        // windows native
        // new PdbFileHeader()
        let magic = b.ReadBytes(28)
        let pageSize = b.ReadInt32()
        let freePageMap = b.ReadInt32()
        let pagesUsed = b.ReadInt32()
        let directorySize  = b.ReadInt32()
        let zero  = b.ReadInt32()

        let ok =
                  zero = 0 &&
                  start = 0x7263694d &&
                  magic = [| 111uy; 115uy; 111uy; 102uy; 116uy; 32uy;
                              67uy; 47uy; 67uy; 43uy; 43uy; 32uy; 77uy; 83uy; 70uy; 32uy;
                              55uy; 46uy; 48uy; 48uy; 13uy; 10uy; 26uy; 68uy; 83uy; 0uy;
                              0uy; 0uy |]

        printfn "native ok %A" ok

        let directoryPages = ((directorySize + pageSize - 1) / pageSize * 4 + pageSize - 1) / pageSize
        printfn "pageSize %A directorySize %A" pageSize directorySize
        let directoryRoot =
          {1..directoryPages}
          |> Seq.map (fun _ -> b.ReadInt32())
          |> Seq.toArray

        directoryRoot
        |> Seq.iter (printfn "directoryRoot %A")

        let pages = (directorySize + pageSize - 1)/pageSize
        let pagesPerPage = pageSize / 4
        let mutable pagesToGo = pages
        let streamData =
          directoryRoot
          |> Seq.map (fun page -> b.BaseStream.Seek((int64)(page * pageSize), SeekOrigin.Begin) |> ignore
                                  let pagesInThisPage = Math.Min (pagesToGo, pagesPerPage)
                                  pagesToGo <- pagesToGo - pagesPerPage
                                  {1..pagesInThisPage}
                                  |> Seq.map (fun _ -> b.ReadInt32())
                                  |> Seq.toArray)
          |> Seq.toArray
          |> Array.concat

        streamData
        |> Seq.iter (printfn "streamData %A")

        //MsfDirectory dir = new MsfDirectory(reader, head, bits);
        // => DataStream type

        ////DbiModuleInfo[] modules = null;
        ////Dictionary<string, PdbSource> sourceCache = new Dictionary<string, PdbSource>();
        //dir.streams[1].Read(reader, bits);
        //Dictionary<string, int> nameIndex = LoadNameIndex(bits, out pdbInfo.Age, out pdbInfo.Guid);
        // Guid starts 12 bytes into the name index

        ok

    let symbolMatch tokens (path: String) =
      use s =
        (if (Path.GetExtension path) <> ".pdb" then
           path + ".mdb"
         else
           path)
        |> File.OpenRead

      Abstraction.DoPathOperation
        (fun () ->
          use b =
            new BinaryReader(s, System.Text.Encoding.UTF8, true)
          let (ok, guids) =
            if (Path.GetExtension path) <> ".pdb" then
              (checkMdb b, [ fst tokens ])
            else
              (checkPdb b, snd tokens)

          let buffer = b.ReadBytes(16)
          let g = Guid buffer

          sprintf "Symbol file %s GUID = %A" path g
          |> Output.verbose

          ok
          && guids |> Seq.exists (fun t -> t = g))
        (fun _ -> false)

  // "Public" API
  let internal getPdbFromImage (assembly: AssemblyDefinition) =
    let tokens = I.getAssemblyTokens assembly

    (tokens,
      Some assembly.MainModule
      |> Option.filter (fun x -> x.HasDebugHeader)
      |> Option.map (fun x -> x.GetDebugHeader())
      |> Option.filter (fun x -> x.HasEntries)
      |> Option.bind (fun x ->
        x.Entries
        |> Seq.filter (fun e -> e.Data.Length > 0x18 &&
                                e.Directory.Type = ImageDebugType.CodeView)
        |> Seq.map (fun x -> x.Data)
        //|> Seq.filter (fun x -> x.Length > 0x18)
        |> Seq.map (fun x ->
          let g = x |> Array.skip 4 |> Array.take 16 |> System.Guid
          sprintf "Assembly symbol GUID = %A mvid = %A" g assembly.MainModule.Mvid
          |> Output.verbose
          let s = x
                  |> Seq.skip 0x18 // size of the debug header
                  |> Seq.takeWhile (fun x -> x <> byte 0)
                  |> Seq.toArray
                  |> System.Text.Encoding.UTF8.GetString
          s, g)
        |> Seq.filter (fun (s, g) -> s.Length > 0)
        |> Seq.filter (fun (s, g) ->
          // printfn "Path to check %A for %A" s assembly
          ([
            Path.IsPathRooted
            File.Exists
            I.symbolMatch tokens
           ]
           |> List.forall(fun f -> f s))
          || (s == (assembly.Name.Name + ".pdb")
              && (assembly |> I.getEmbeddedPortablePdbEntry)
                .IsNotNull))
        |> Seq.map fst
        |> Seq.tryHead))
  let internal getPdbWithFallback (assembly: AssemblyDefinition) =
    let path = assembly.MainModule.FileName

    match getPdbFromImage assembly with
    | (tokens, None) when path |> String.IsNullOrWhiteSpace |> not -> // i.e. assemblies read from disk only
      let foldername = Path.GetDirectoryName path
      let filename = Path.GetFileName path

      let folder =
        foldername :: (Seq.toList symbolFolders)
        |> Seq.map (I.getSymbolsByFolder filename)
        |> Seq.choose id
        |> Seq.tryFind (I.symbolMatch tokens)

      sprintf
        "Assembly %s symbols from folder '%A'"
        path
        (Option.defaultValue String.Empty folder)
      |> Output.verbose

      folder
    | (_, pdbpath) ->
      sprintf
        "Assembly %s symbols from image '%s'"
        path
        (Option.defaultValue String.Empty pdbpath)
      |> Output.verbose

      pdbpath

  // Ensure that we read symbols from the .pdb path we discovered.
  // Cecil currently only does the Path.ChangeExtension(path, ".pdb") fallback if left to its own devices
  // Will fail  with InvalidOperationException if there is a malformed file with the expected name
  let internal readSymbols (assembly: AssemblyDefinition) =
    getPdbWithFallback assembly
    |> Option.iter (fun pdbpath ->
      let provider: ISymbolReaderProvider =
        if pdbpath.EndsWith(".pdb", StringComparison.OrdinalIgnoreCase) then
          PdbReaderProvider() :> ISymbolReaderProvider
        else
          MdbReaderProvider() :> ISymbolReaderProvider

      let reader =
        provider.GetSymbolReader(assembly.MainModule, pdbpath)

      assembly.MainModule.ReadSymbols(reader))

  // reflective short-cuts don't work.
  // maybe move this somewhere else, now?
  let internal getModuleDocuments (``module``: ModuleDefinition) =
    ``module``.GetAllTypes()
    |> Seq.collect (fun t -> t.GetMethods())
    |> Seq.collect (fun m -> m.DebugInformation.SequencePoints)
    |> Seq.map (fun s -> s.Document)
    |> Seq.distinctBy (fun d -> d.Url)
    |> Seq.toList