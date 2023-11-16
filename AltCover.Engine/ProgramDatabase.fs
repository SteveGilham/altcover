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
open System.Reflection

[<RequireQualifiedAccess>]
module internal ProgramDatabase =
  // "Public" "field"
  let internal symbolFolders = List<String>()

  // Implementation details
  module internal I =

    // We no longer have to violate Cecil encapsulation to get the PDB path
    // but we do to get the embedded PDB info
    let internal getEmbed =
      (typeof<Mono.Cecil.AssemblyDefinition>.Assembly
        .GetTypes()
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

    let extractGuid =
      (Array.skip 4) >> (Array.take 16) >> System.Guid

    let getAssemblyTokens (assembly: AssemblyDefinition) =
      let m = assembly.MainModule

      let t =
        m.GetDebugHeader().Entries
        |> Seq.filter (fun e ->
          e.Data.Length > 0x18
          && e.Directory.Type = ImageDebugType.CodeView)
        |> Seq.map _.Data
        |> Seq.filter (fun x -> lead x = 1396986706)
        |> Seq.map extractGuid
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
      let stream = b.BaseStream
      let start = b.ReadInt32()

      if
        start = 0x424a5342 // portable format
      then
        let major = b.ReadInt16()
        let minor = b.ReadInt16()
        let reserved = b.ReadInt32()
        let versionSize = b.ReadInt32()
        let version = b.ReadBytes(versionSize)
        let flags = b.ReadInt16()
        let streams = b.ReadInt16() |> int // # of stream headers

        let headers =
          { 1..streams }
          |> Seq.map (fun _ ->
            let offset = b.ReadInt32()
            let size = b.ReadInt32()

            let name =
              Seq.initInfinite id
              |> Seq.map (fun _ -> b.ReadInt32())
              |> Seq.takeWhile (fun x -> x > 0xffffff)
              |> Seq.toArray

            (offset, size, name))
          |> Seq.toArray

        // don't expect any other #Pdb??? streams
        let guid =
          headers
          |> Seq.tryFind (fun (_, _, x) -> x = [| 0x62645023 |])

        let ok = guid.IsSome

        if ok then
          let (o, _, _) = guid.Value
          stream.Seek(int64 o, SeekOrigin.Begin) |> ignore

        ok && reserved = 0 && (int flags) = 0

      else
        // windows native
        // subset internal static PdbInfo LoadFunctions(Stream read)
        let binding =
          BindingFlags.NonPublic ||| BindingFlags.Instance

        stream.Seek((int64) 0, SeekOrigin.Begin) |> ignore

        let nreader =
          typeof<Mono.Cecil.Pdb.NativePdbReader>.Assembly

        let construct name parameters = // memoize??
          let classtype = nreader.GetType name // line for static analysis

          let constructor =
            classtype.GetConstructor(
              binding,
              null,
              parameters |> Array.map _.GetType(),
              [||]
            )

          let instance = constructor.Invoke parameters
          (classtype, instance)

        let bitaccess = nreader.GetType()

        let makeba =
          bitaccess.GetConstructor(binding, null, [| typeof<int> |], [||])

        let _, bits =
          construct "Microsoft.Cci.Pdb.BitAccess" [| 65536 :> obj |]

        let pdbheader, head =
          construct "Microsoft.Cci.Pdb.PdbFileHeader" [| stream; bits |]

        let field (t: Type) name instance =
          t.GetField(name, binding).GetValue(instance)

        let pageSize =
          (field pdbheader "pageSize" head) :?> int

        let _, reader =
          construct "Microsoft.Cci.Pdb.PdbReader" [| stream; pageSize |]

        let msfdirectory, dir =
          construct "Microsoft.Cci.Pdb.MsfDirectory" [| reader; head; bits |]

        let datastream =
          (field msfdirectory "streams" dir) :?> System.Collections.IEnumerable
          |> Seq.cast
          |> Seq.skip 1
          |> Seq.head

        let page0 =
          (field (datastream.GetType()) "pages" datastream) :?> int array
          |> Seq.head

        // Position stream 12 bytes into the page
        stream.Seek((int64) (page0 * pageSize + 12), SeekOrigin.Begin)
        |> ignore

        true // or would have thrown

    let symbolMatch tokens (path: String) =
      PathOperation.DoPathOperation
        (fun () ->
          let isNotPdb =
            (Path.GetExtension path) != ".pdb"

          use s =
            (if isNotPdb then path + ".mdb" else path)
            |> File.OpenRead

          use b =
            new BinaryReader(s, System.Text.Encoding.UTF8, true)

          let (ok, guids) =
            if isNotPdb then
              (checkMdb b, [ fst tokens ])
            else
              (checkPdb b, snd tokens)

          let buffer = b.ReadBytes(16)
          let g = Guid buffer

          sprintf "Symbol file %s GUID = %A" path g
          |> Output.verbose

          ok && guids |> Seq.exists (fun t -> t = g))
        (fun _ -> false)

  // "Public" API
  let internal getPdbFromImage (assembly: AssemblyDefinition) =
    let tokens = I.getAssemblyTokens assembly

    (tokens,
     Some assembly.MainModule
     |> Option.filter _.HasDebugHeader
     |> Option.map _.GetDebugHeader()
     |> Option.filter _.HasEntries
     |> Option.bind (fun x ->
       x.Entries
       |> Seq.filter (fun e ->
         e.Data.Length > 0x18
         && e.Directory.Type = ImageDebugType.CodeView)
       |> Seq.map (fun entry ->
         let x = entry.Data
         let g = x |> I.extractGuid

         sprintf "Assembly symbol GUID = %A mvid = %A" g assembly.MainModule.Mvid
         |> Output.verbose

         let s =
           x
           |> Seq.skip 0x18 // size of the debug header
           |> Seq.takeWhile (fun x -> x <> byte 0)
           |> Seq.toArray
           |> System.Text.Encoding.UTF8.GetString

         s, g)
       |> Seq.filter (fun (s, g) -> s.Length > 0)
       |> Seq.filter (fun (s, g) ->
         // printfn "Path to check %A for %A" s assembly
         ([ Path.IsPathRooted
            File.Exists
            I.symbolMatch tokens ]
          |> List.forall (fun f -> f s))
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
        |> Seq.choose (I.getSymbolsByFolder filename)
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
    |> Seq.collect _.GetMethods()
    |> Seq.collect _.DebugInformation.SequencePoints
    |> Seq.map _.Document
    |> Seq.distinctBy _.Url
    |> Seq.toList

[<assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Gendarme.Rules.Exceptions",
                                                            "InstantiateArgumentExceptionCorrectlyRule",
                                                            Scope = "member",
                                                            Target =
                                                              "AltCover.ProgramDatabase/I/construct@143::Invoke(System.String,System.Object[])",
                                                            Justification =
                                                              "Compiler generated")>]
()