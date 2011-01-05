namespace AltCover

open System
open System.Collections
open System.Collections.Generic
open System.IO
open System.Reflection

open Mono.Cecil
open Mono.Cecil.Cil
open Mono.Cecil.Pdb

/// <summary>
/// Source code segment location.
/// </summary>
type CodeSegment = {
         Line : UInt32;
         Column : UInt16;
         EndLine : UInt32;
         EndColumn : UInt16;
         Document : string }
         
type AssemblyModel = {
         Assembly : AssemblyDefinition;
         Symbols : IDictionary }    

module ProgramDatabase =
  open Mono.Cecil.Cil

  let SizeOfSection = 0x28L

  let Seek (reader: BinaryReader) offset mark =
    reader.BaseStream.Seek(offset, mark) |> ignore

  let SignatureCheck reader =    
    Seek reader 0L SeekOrigin.Begin
    if reader.ReadByte() <> byte 'M' then None
    elif reader.ReadByte() <> byte 'Z' then None
    else Some reader.BaseStream.Position
    
  let CommonObjectFileFormatHeader reader (two:int64) =    
    let PESignatureOffsetLoc = 0x3CL
    // Find the address of the PE Header
    Seek reader PESignatureOffsetLoc SeekOrigin.Begin
    let PESignatureOffset = int64 <| reader.ReadByte()
    Seek reader PESignatureOffset SeekOrigin.Begin
    // Start of PE Signature
    if reader.ReadByte() <> byte 'P' then None
    elif reader.ReadByte() <> byte 'E' then None
    elif reader.ReadByte() <> 0uy then None
    elif reader.ReadByte() <> 0uy then None
    elif reader.ReadUInt16() <> 0x014cus then None
    else
      // Start of CommonObjectFileFormat Header
      Some ( reader.BaseStream.Position - 2L )
      

  let Unpick reader headerOffset =
    let SizeOfCommonObjectFileFormatHeader = 0x14
    // CommonObjectFileFormat: 0x02 contains 2 bytes that indicate the NumberOfSections
    // CommonObjectFileFormat: 0x10 contains 2 bytes that indicate the SizeOfOptionalHeader
    Seek reader (headerOffset +  int64 0x02) SeekOrigin.Begin
    let NumberOfSections = reader.ReadUInt16()
    Seek reader (headerOffset + int64 0x10) SeekOrigin.Begin
    let SizeOfOptionalHeader = reader.ReadUInt16()

    // Start of PE Header
    let PEHeaderStart = headerOffset + int64 SizeOfCommonObjectFileFormatHeader
    let StartOfSections = PEHeaderStart + int64 SizeOfOptionalHeader
    Seek reader PEHeaderStart SeekOrigin.Begin

    let PEFormat = int <| reader.ReadInt16()
    Seek reader (PEHeaderStart + 92L) SeekOrigin.Begin
    let dataDirectoryCount = int <| reader.ReadInt32()
    
    // Check for the existance of a non-null CLI header.
    // If found, this is an assembly of some kind, otherwise
    // it's a native PE file of one kind or another.
    Seek reader (PEHeaderStart + 208L) SeekOrigin.Begin
    let rvaCLIHeader = int <| reader.ReadInt32()  // Partition II, 24.2.3.3, CLI Header (rva)
    
    // PEHeader fields vary based upon the PE_Format setting
    match (PEFormat, dataDirectoryCount, rvaCLIHeader) with
    | _, _, 0 -> None
    | 0x010B, 0x10, _ -> Some(PEHeaderStart + int64 0x90, StartOfSections, int NumberOfSections)
    | 0x020B, 0x10, _  -> Some(PEHeaderStart + int64 0xA0, StartOfSections, int NumberOfSections)
    | _ -> None

  let GetRelativeVirtualAddress reader debugDirectoryOffset =
     Seek reader debugDirectoryOffset SeekOrigin.Begin
     let DebugRelativeVirtualAddress = reader.ReadUInt32()
     DebugRelativeVirtualAddress
  
  let GetVirtualAddressForRelativeVirtualAddress reader triplet =
     // Find a section where:
     // VirtualAddress <= StartRelativeVirtualAddress <= VirtualAddress+VirtualSize
     let RelativeVirtualAddress, StartOfSections, NumberOfSections = triplet
     
     [0 .. (NumberOfSections - 1)] 
     |> List.choose (fun i -> 
       let j = int64 i
       Seek reader (StartOfSections + j * SizeOfSection) SeekOrigin.Begin
       Seek reader (int64 0x08) SeekOrigin.Current
       let VirtualSize = reader.ReadUInt32()
       let VirtualAddress = reader.ReadUInt32()
       if (VirtualAddress <= RelativeVirtualAddress && RelativeVirtualAddress <= VirtualAddress + VirtualSize) then Some VirtualAddress
       else None
     )
     |> List.tryPick (fun x -> Some x)


  let GetPdbOffset reader triplet =
    let SizeOfDebugDirectory = 0x1CL
    let SizeOfDebugInfo = 0x18L
    let debugDirectoryOffset, StartOfSections, NumberOfSections = triplet
    let DebugRelativeVirtualAddress = GetRelativeVirtualAddress reader debugDirectoryOffset
    match GetVirtualAddressForRelativeVirtualAddress reader (DebugRelativeVirtualAddress, StartOfSections, NumberOfSections) with
    | None -> None
    | Some va -> 
      let DebugInfoFilePointer = (int64 DebugRelativeVirtualAddress) - (int64 va) + StartOfSections + SizeOfSection * (int64 NumberOfSections) + 0x10L
      Some (DebugInfoFilePointer + SizeOfDebugDirectory + SizeOfDebugInfo)

  let GetPdbNameFromOffset reader offset =
    Seek reader offset SeekOrigin.Begin
    let name =  reader 
                |> Seq.unfold (fun stream -> Some(stream.ReadByte(), stream))
                |> Seq.takeWhile (fun x -> x > byte 0)
                |> Seq.map (fun x -> char x)
                |> Seq.toArray             
    Some (new String(name))
      
  let PdbPath path =
    use raw = new FileStream(path, FileMode.Open, FileAccess.Read)
    use reader = new BinaryReader(raw)
    reader 
    |> SignatureCheck  
    |> Option.bind (CommonObjectFileFormatHeader reader)
    |> Option.bind (Unpick reader)
    |> Option.bind (GetPdbOffset reader)
    |> Option.bind (GetPdbNameFromOffset reader)

  let PdbPathExists path =
    match PdbPath path with
    | Some x when File.Exists(x) -> Some x
    | _ -> None
    
  let LoadSymbols (m:ModuleDefinition) path =
    let innate = PdbPath path
    let pdbpath = match innate with
                  | Some x when File.Exists(x) -> x
                  | _ -> Path.ChangeExtension(path, ".pdb")
                  
    if File.Exists(pdbpath) then 
      use stream = File.OpenRead(pdbpath)
      let reader = PdbReaderProvider().GetSymbolReader(m, stream)
      let header = Array.create<byte> 24 0uy
      let dir = ImageDebugDirectory()
      if reader.ProcessDebugHeader(dir, header) then
            // reflective hack.  Don't worry, a proper refactoring 
            // is ahead of us, but that's several replays down the line
            let field = typeof<PdbReader>.GetField("functions", BindingFlags.Instance ||| BindingFlags.NonPublic)
            field.GetValue(reader) :?> IDictionary
      else Hashtable() :> IDictionary
    else Hashtable() :> IDictionary                  

  let LoadAssembly (path:string) =
    let assembly = AssemblyDefinition.ReadAssembly(path)
    { Assembly = assembly;
      Symbols = LoadSymbols assembly.MainModule path }
      
  let GetCodeSegmentsForMethod (assembly : AssemblyModel) (methodDef : MethodDefinition) =
    let segments = new Dictionary<UInt32, CodeSegment>()
    let token = methodDef.MetadataToken.ToUInt32()
    // reflective hack.  Don't worry, a proper refactoring 
    // is ahead of us, but that's several replays down the line
    let value = assembly.Symbols.[token]
    if value <> null then
      let lines' = value.GetType().GetField("lines", BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public)
      let lines = lines'.GetValue(value) :?> obj[]
      seq { for x in lines do 
              let lines'' = x.GetType().GetField("lines", BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public)
              let xlines = lines''.GetValue(x) :?> obj[]
              if xlines <> null then 
                  for y in xlines do
                    let lineBegin' = y.GetType().GetField("lineBegin", BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public)
                    let yLineBegin = lineBegin'.GetValue(y) :?> UInt32
                    if yLineBegin <> 0xfeefeeu then
                        let file' = x.GetType().GetField("file", BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public)
                        let file = file'.GetValue(x)
                        let name' = file.GetType().GetField("name", BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public)
                        yield (name'.GetValue(file) :?> string, y) }
      |> Seq.iter (fun pair -> 
         let z = snd pair
         let offset' = z.GetType().GetField("offset", BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public)
         let offset = offset'.GetValue(z) :?> UInt32
         let lineBegin' = z.GetType().GetField("lineBegin", BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public)
         let lineBegin = lineBegin'.GetValue(z) :?> UInt32
         let lineEnd' = z.GetType().GetField("lineEnd", BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public)
         let lineEnd = lineEnd'.GetValue(z) :?> UInt32
         let colBegin' = z.GetType().GetField("colBegin", BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public)
         let colBegin = colBegin'.GetValue(z) :?> UInt16
         let colEnd' = z.GetType().GetField("colEnd", BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public)
         let colEnd = colEnd'.GetValue(z) :?> UInt16

         segments.Add( offset, 
            {         
            Line = lineBegin;
            Column = colBegin;
            EndLine = lineEnd;
            EndColumn = colEnd;
            Document = fst pair }))
    segments