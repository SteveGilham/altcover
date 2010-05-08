namespace AltCover

open System
open System.Collections.Generic
open System.IO

open Microsoft.Cci.Pdb
open Mono.Cecil

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
         Symbols : Dictionary<UInt32, PdbFunction> }    

module ProgramDatabase =
  let (>?>) (arg : option<'TAny>) ( operation : 'TAny -> option<'TAnother> ) =
    match arg with 
    | None -> None
    | Some x -> operation x

  let SizeOfSection = 0x28L

  let Seek (reader: BinaryReader) offset mark =
    reader.BaseStream.Seek(offset, mark) |> ignore
    
  let CommonObjectFileFormatHeader reader =    
    let PESignatureOffsetLoc = 0x3CL
    // Find the address of the PE Header
    Seek reader PESignatureOffsetLoc SeekOrigin.Begin
    let PESignatureOffset = int64 <| reader.ReadByte()
    Seek reader PESignatureOffset SeekOrigin.Begin
    // Start of PE Signature
    if reader.ReadByte() <> byte 'P' then None
    elif reader.ReadByte() <> byte 'E' then None
    elif reader.ReadByte() <> byte 0 then None
    elif reader.ReadByte() <> byte 0 then None
    else
      // Start of CommonObjectFileFormat Header
      Some reader.BaseStream.Position
      

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
    
    // PEHeader fields vary based upon the PE_Format setting
    match PEFormat with
    | 0x010B -> Some(PEHeaderStart + int64 0x90, StartOfSections, int NumberOfSections)
    | 0x020B -> Some(PEHeaderStart + int64 0xA0, StartOfSections, int NumberOfSections)
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
    |> CommonObjectFileFormatHeader
    >?> Unpick reader
    >?> GetPdbOffset reader
    >?> GetPdbNameFromOffset reader

  let LoadSymbols path =
    let innate = PdbPath path
    let pdbpath = match innate with
                  | Some x when File.Exists(x) -> x
                  | _ -> Path.ChangeExtension(path, ".pdb")
                  
    let symbols = new Dictionary<UInt32, PdbFunction>()
    if File.Exists(pdbpath) then 
      use stream = File.OpenRead(pdbpath)
      PdbFile.LoadFunctions(stream, true)
                 |> Seq.iter (fun func -> symbols.Add(func.Token, func))
                      
    symbols

  let LoadAssembly (path:string) =
    { Assembly = AssemblyDefinition.ReadAssembly(path);
      Symbols = LoadSymbols path }
      
  let GetCodeSegmentsForMethod (assembly : AssemblyModel) (methodDef : MethodDefinition) =
    let segments = new Dictionary<UInt32, CodeSegment>()
    let token = methodDef.MetadataToken.ToUInt32()
    let symbols = assembly.Symbols
    if symbols.ContainsKey(token) && symbols.[token] <> null then
      let value = symbols.[token]
      seq { for x in value.Lines do 
              for y in x.Lines do
                if y.lineBegin <> 0xfeefeeu then
                   yield (x.FileName, y) }
      |> Seq.iter (fun pair -> 
         let z = snd pair
         segments.Add( z.offset, 
            {         
            Line = z.lineBegin;
            Column = z.colBegin;
            EndLine = z.lineEnd;
            EndColumn = z.colEnd;
            Document = fst pair }))
    segments