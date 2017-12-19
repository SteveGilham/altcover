// Based upon C# code by Sergiy Sakharov (sakharov@gmail.com)
// http://code.google.com/p/dot-net-coverage/source/browse/trunk/Coverage/Instrument/CounterAssemblyBuilder.cs
// http://code.google.com/p/dot-net-coverage/source/browse/trunk/Coverage/Instrument/InstrumentorVisitor.cs

namespace AltCover

open System
open System.Collections.Generic
open System.IO
open System.Reflection

open AltCover.Augment
open Mono.Cecil
open Mono.Cecil.Cil
open Mono.Cecil.Rocks

/// <summary>
/// Module to handle instrumentation visitor
/// </summary>
module Instrument =

  /// <summary>
  /// State object passed from visit to visit
  /// </summary>
  type private Context = { InstrumentedAssemblies : string list;
                           RenameTable : Dictionary<String, String>;
                           ModuleId : Guid;
                           RecordingAssembly : AssemblyDefinition;
                           RecordingMethod : MethodDefinition;
                           RecordingMethodRef : MethodReference;
                           MethodBody : MethodBody;
                           MethodWorker :ILProcessor }

  /// <summary>
  /// Higher-order function that returns a visitor
  /// </summary>
  /// <param name="assemblies">List of assembly paths to visit</param>
  /// <returns>Stateful visitor function</returns>
  let internal InstrumentGenerator (assemblies : string list) =
    let initialState = {
             InstrumentedAssemblies = assemblies;
             RenameTable = null;
             ModuleId = Guid.Empty;
             RecordingAssembly = null;
             RecordingMethod = null;
             RecordingMethodRef = null;
             MethodBody = null;
             MethodWorker = null }

    /// <summary>
    /// Workround for not being able to take typeof<SomeModule> even across
    /// assembly boundaries -- start with a pure type then iterate to the module
    /// </summary>
    /// <returns>A representation of the type used to record all coverage visits.</returns>
    let RecorderInstanceType () =
      let trace  = typeof<AltCover.Recorder.Tracer>
      trace.Assembly.GetExportedTypes()
                            |> Seq.find (fun (t:Type) -> t.Name.Contains("Instance"))

    /// <summary>
    /// Applies a new key to an assembly name
    /// </summary>
    /// <param name="assemblyName">The name to update</param>
    /// <param name="key">The possibly empty key to use</param>
    let UpdateStrongNaming (assemblyName:AssemblyNameDefinition) (key:StrongNameKeyPair option) =
      match key with
      | None -> assemblyName.HasPublicKey <- false
                assemblyName.PublicKey <- null
                assemblyName.PublicKeyToken <- null
      | Some key' -> assemblyName.HasPublicKey <- true
                     assemblyName.PublicKey <- key'.PublicKey // sets token implicitly

    /// <summary>
    /// Create the new assembly that will record visits, based on the prototype.
    /// </summary>
    /// <returns>A representation of the assembly used to record all coverage visits.</returns>
    let DefineRecordingAssembly () =
      let recorder = typeof<AltCover.Recorder.Tracer>
      let definition = AssemblyDefinition.ReadAssembly(recorder.Assembly.Location)
      ProgramDatabase.ReadSymbols definition
      definition.Name.Name <- definition.Name.Name + ".g"
      use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream("AltCover.Recorder.snk")
      use buffer = new MemoryStream()
      stream.CopyTo(buffer)
      let pair = StrongNameKeyPair(buffer.ToArray())
      UpdateStrongNaming definition.Name (Some pair)

      // set the coverage file path
      let other = RecorderInstanceType()
      let token = other.GetMethod("get_ReportFile").MetadataToken
      let pathGetterDef = definition.MainModule.LookupToken(token) :?> MethodDefinition

      let body = pathGetterDef.Body
      let worker = body.GetILProcessor();
      let head = body.Instructions.[0]
      worker.InsertBefore(head, worker.Create(OpCodes.Ldstr, Visitor.reportPath));
      worker.InsertBefore(head, worker.Create(OpCodes.Ret));

      definition

    /// <summary>
    /// Locate the method that must be called to register a code point for coverage visit.
    /// </summary>
    /// <param name="assembly">The assembly containing the recorder method</param>
    /// <returns>A representation of the method to call to signal a coverage visit.</returns>
    let RecordingMethod (recordingAssembly : AssemblyDefinition) =
      let other = RecorderInstanceType()
      let token = other.GetMethod("Visit").MetadataToken
      recordingAssembly.MainModule.LookupToken(token) :?> MethodDefinition

    /// <summary>
    /// Locate the key, if any, which was used to name this assembly.
    /// </summary>
    /// <param name="name">The name of the assembly</param>
    /// <returns>A key, if we have a match.</returns>
    let KnownKey (name:AssemblyNameDefinition) =
        if not name.HasPublicKey then
          None
        else
          let index = KeyStore.ArrayToIndex name.PublicKey
          match Visitor.keys.TryGetValue(index) with
          | (false, _ ) -> None
          | (_, record) -> Some record.Pair

    /// <summary>
    /// Locate the key, if any, which was used to name this assembly.
    /// </summary>
    /// <param name="name">The name of the assembly</param>
    /// <returns>A key, if we have a match.</returns>
    let KnownToken (name:AssemblyNameReference) =
        let pktoken = name.PublicKeyToken
        if isNull pktoken then
          None
        else
          let index = KeyStore.TokenAsULong pktoken
          match Visitor.keys.TryGetValue(index) with
          | (false, _ ) -> None
          | (_, record) -> Some record

    /// <summary>
    /// Determine new names for input strongnamed assemblies; if we have a key and
    /// the assembly was already strongnamed then give it the new key token, otherwise
    /// set that there is no strongname.
    /// </summary>
    /// <param name="assembly">The assembly object being operated upon</param>
    /// <param name="path">The names of all assemblies of interest</param>
    /// <returns>Map from input to output names</returns>
    let UpdateStrongReferences (assembly : AssemblyDefinition) (assemblies : string list) =
      let effectiveKey = if assembly.Name.HasPublicKey then Visitor.defaultStrongNameKey else None
      UpdateStrongNaming assembly.Name effectiveKey

      // TODO -- is this still lookup table of any use??
      let assemblyReferenceSubstitutions = new Dictionary<String, String>()

      let interestingReferences =  assembly.MainModule.AssemblyReferences
                                   |> Seq.cast<AssemblyNameReference>
                                   |> Seq.filter (fun x -> assemblies |> List.exists (fun y -> y.Equals(x.Name)))

      interestingReferences
      |> Seq.iter (fun r -> let original = r.ToString()
                            let token = KnownToken r
                            let effectiveKey = match token with
                                               | None -> Visitor.defaultStrongNameKey
                                                         |> Option.map KeyStore.KeyToRecord
                                               | key -> key

                            match effectiveKey with
                            | None -> r.HasPublicKey <- false
                                      r.PublicKeyToken <- null
                                      r.PublicKey <- null
                            | Some key -> r.HasPublicKey <- true
                                          r.PublicKey <- key.Pair.PublicKey // implicitly sets token

                            let updated = r.ToString()
                            if  not <| updated.Equals(original, StringComparison.Ordinal) then
                              assemblyReferenceSubstitutions.[original] <- updated
                    )

      assemblyReferenceSubstitutions

    /// <summary>
    /// Commit an instrumented assembly to disk
    /// </summary>
    /// <param name="assembly">The instrumented assembly object</param>
    /// <param name="path">The full path of the output file</param>
    /// <remark>Can raise "System.Security.Cryptography.CryptographicException: Keyset does not exist" at random
    /// when asked to strongname.  This writes a new .pdb/.mdb alongside the instrumented assembly</remark>
    let WriteAssembly (assembly:AssemblyDefinition) (path:string) =
      let pkey = Mono.Cecil.WriterParameters()
      pkey.WriteSymbols <- true
      pkey.SymbolWriterProvider <- match Path.GetExtension (Option.getOrElse String.Empty (ProgramDatabase.GetPdbWithFallback assembly)) with
                                   | ".mdb" -> Mono.Cecil.Mdb.MdbWriterProvider() :> ISymbolWriterProvider
                                   | _ -> Mono.Cecil.Pdb.PdbWriterProvider() :> ISymbolWriterProvider
      KnownKey assembly.Name
      |> Option.iter (fun key -> pkey.StrongNameKeyPair <- key)
      assembly.Write(path, pkey)

    /// <summary>
    /// Adjust the IL for exception handling
    /// </summary>
    /// <param name="handler">The exception handler</param>
    /// <param name="oldBoundary">The uninstrumented location</param>
    /// <param name="newBoundary">Where it has moved to</param>
    let SubstituteExceptionBoundary (handler:ExceptionHandler) (oldBoundary:Instruction)( newBoundary : Instruction ) =
      if handler.FilterStart = oldBoundary then handler.FilterStart <- newBoundary
      if handler.HandlerEnd = oldBoundary then handler.HandlerEnd <- newBoundary
      if handler.HandlerStart = oldBoundary then handler.HandlerStart <- newBoundary
      if handler.TryEnd = oldBoundary then handler.TryEnd <- newBoundary
      if handler.TryStart = oldBoundary then handler.TryStart <- newBoundary

    /// <summary>
    /// Adjust the IL to substitute an opcode
    /// </summary>
    /// <param name="instruction">Instruction being processed</param>
    /// <param name="oldOperand">Type we are looking for</param>
    /// <param name="newOperand">Type to replace it with</param>
    let SubstituteInstructionOperand(instruction:Instruction) (oldOperand:Instruction) (newOperand:Instruction) =
      // Performance reasons - only 3 types of operators have operands of Instruction types
      // instruction.Operand getter - is rather slow to execute it for every operator
      match instruction.OpCode.OperandType with
      | OperandType.InlineBrTarget
      | OperandType.ShortInlineBrTarget
      | OperandType.InlineSwitch ->
        if instruction.Operand = (oldOperand :> Object) then
           instruction.Operand <- newOperand
        // At this point instruction.Operand will be either Operand != oldOperand
        // or instruction.Operand will be of type Instruction[]
        // (in other words - it will be a switch operator's operand)
        else if instruction.OpCode.OperandType = OperandType.InlineSwitch then
           let operands = instruction.Operand :?> Instruction array
           for i in operands
                           |> Seq.mapi (fun i x -> (i,x))
                           |> Seq.filter (fun (i,x) -> x = oldOperand)
                           |> Seq.map (fun (i,x) -> i)
               do
                Array.blit [|newOperand|] 0 operands i 1
        else ()
      | _ -> ()

    /// <summary>
    /// Perform visitor operations
    /// </summary>
    /// <param name="state">Contextual information for the visit</param>
    /// <param name="node">The node being visited</param>
    /// <returns>Updated state</returns>
    let InstrumentationVisitor (state : Context) (node:Node) =
       match node with
       | Start _ -> { state with RecordingAssembly = DefineRecordingAssembly() }
       | Assembly (assembly, included) ->
            let updates = UpdateStrongReferences assembly state.InstrumentedAssemblies
            if included then
               assembly.MainModule.AssemblyReferences.Add(state.RecordingAssembly.Name)
            { state with RenameTable = updates } // TODO use this (attribute mappings IIRC)
       | Module (m, included) -> //of ModuleDefinition * bool
           let restate = match included with
                         | true ->
                           let recordingMethod = match state.RecordingMethod with
                                                 | null -> RecordingMethod state.RecordingAssembly
                                                 | _ -> state.RecordingMethod

                           { state with
                                 RecordingMethodRef = m.Import(recordingMethod);
                                 RecordingMethod = recordingMethod }
                         | _ -> state
           { restate with ModuleId = m.Mvid }

       | Type _ -> //of TypeDefinition * bool * AssemblyModel
           state
       | Method (m,  included) -> //of MethodDefinition * bool
           match included with
           | true ->
             let body = m.Body
             { state with
                MethodBody = body;
                MethodWorker = body.GetILProcessor() }
           | _ -> state

       | MethodPoint (instruction, _, point, included) -> //of Instruction * CodeSegment * int * bool
         if included && (not(isNull instruction.SequencePoint)) &&
                        (Visitor.IsIncluded instruction.SequencePoint.Document.Url) then
              let counterMethodCall = state.MethodWorker.Create(OpCodes.Call, state.RecordingMethodRef);
              let instrLoadModuleId = state.MethodWorker.Create(OpCodes.Ldstr, state.ModuleId.ToString())
              let instrLoadPointId = state.MethodWorker.Create(OpCodes.Ldc_I4, point);

              state.MethodWorker.InsertBefore(instruction, instrLoadModuleId);
              state.MethodWorker.InsertAfter(instrLoadModuleId, instrLoadPointId);
              state.MethodWorker.InsertAfter(instrLoadPointId, counterMethodCall);

              // Change references in operands from "instruction" to first counter invocation instruction (instrLoadModuleId)
              state.MethodBody.Instructions
              |> Seq.iter (fun x -> SubstituteInstructionOperand x instruction instrLoadModuleId)

              state.MethodBody.ExceptionHandlers
              |> Seq.iter (fun x -> SubstituteExceptionBoundary x instruction instrLoadModuleId)

         state
       | AfterMethod included -> //of MethodDefinition * bool
           if included then
              let body = state.MethodBody
              // changes conditional (br.s, brtrue.s ...) operators to corresponding "long" ones (br, brtrue)
              body.SimplifyMacros()
              // changes "long" conditional operators to their short representation where possible
              body.OptimizeMacros()
           state

       | AfterModule -> state
       | AfterAssembly assembly ->
           let path = Path.Combine(Visitor.outputDirectory, FileInfo(assembly.MainModule.Name).Name)
           WriteAssembly assembly path
           state
       | Finish -> let counterAssemblyFile = Path.Combine(Visitor.outputDirectory, state.RecordingAssembly.Name.Name + ".dll")
                   WriteAssembly (state.RecordingAssembly) counterAssemblyFile
                   state

    Visitor.EncloseState InstrumentationVisitor initialState