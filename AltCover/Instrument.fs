// Based upon C# code by Sergiy Sakharov (sakharov@gmail.com)
// http://code.google.com/p/dot-net-coverage/source/browse/trunk/Coverage/Instrument/CounterAssemblyBuilder.cs
// http://code.google.com/p/dot-net-coverage/source/browse/trunk/Coverage/Instrument/InstrumentorVisitor.cs

namespace AltCover

open System
open System.Collections.Generic
open System.IO

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
    /// Create the new assembly that will record visits, based on the prototype.
    /// </summary>
    /// <returns>A representation of the assembly used to record all coverage visits.</returns>
    let DefineRecordingAssembly () =
      let recorder = typeof<AltCover.Recorder.Tracer>
      let definition = AssemblyDefinition.ReadAssembly(recorder.Assembly.Location)
      definition.Name.Name <- definition.Name.Name + ".g"
      match Visitor.strongNameKey with
      | None -> definition.Name.HasPublicKey <- false
                definition.Name.PublicKey <- null
                definition.Name.PublicKeyToken <- null
      | Some key -> definition.Name.HasPublicKey <- true
                    definition.Name.PublicKey <- key.PublicKey
                    
      // set the coverage file path  
      let other = RecorderInstanceType()
      let token = other.GetMethod("get_ReportFile").MetadataToken
      let pathGetterDef = definition.MainModule.LookupToken(token) :?> MethodDefinition

      let worker = pathGetterDef.Body.GetILProcessor();
      worker.InsertBefore(pathGetterDef.Body.Instructions.[0], worker.Create(OpCodes.Ret));
      worker.InsertBefore(pathGetterDef.Body.Instructions.[0], worker.Create(OpCodes.Ldstr, Visitor.reportPath));
                        
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
    /// Determine new names for input strongnamed assemblies; if we have a key and
    /// the assembly was already strongnamed then give it the new key token, otherwise
    /// set that there is no strongname.
    /// </summary>
    /// <param name="assembly">The assembly object being operated upon</param>
    /// <param name="path">The names of all assemblies of interest</param>
    /// <returns>Map from input to output names</returns>
    let UpdateStrongReferences (assembly : AssemblyDefinition) (assemblies : string list) =
      let assemblyReferenceSubstitutions = new Dictionary<String, String>()
      let effectiveKey = if assembly.Name.HasPublicKey then Visitor.strongNameKey else None
      match effectiveKey with
      | None -> assembly.Name.HasPublicKey <- false
                assembly.Name.PublicKey <- null
                assembly.Name.PublicKeyToken <- null
      | Some key -> assembly.Name.HasPublicKey <- true
                    assembly.Name.PublicKey <- key.PublicKey
                
      assembly.MainModule.AssemblyReferences                  
      |> Seq.cast<AssemblyNameReference>
      |> Seq.filter (fun x -> assemblies 
                              |> List.exists (fun y -> y.Equals(x.Name)))
      |> Seq.iter (fun x ->
        let original = x.ToString()
        // HasPublicKey may not be set even if PublicKeyToken is!
        let theKey = if (x.HasPublicKey || x.PublicKeyToken <> null) then
                        Visitor.strongNameKey
                     else
                        None
        match theKey with
        | None -> x.HasPublicKey <- false
                  x.PublicKey <- null
                  x.PublicKeyToken <- null
        | Some key -> x.HasPublicKey <- true
                      x.PublicKey <- key.PublicKey
               
        let updated = x.ToString() 
        if  updated <> original then 
          assemblyReferenceSubstitutions.[original] <- x.ToString()              
       )     
      assemblyReferenceSubstitutions
    
    /// <summary>
    /// Commit an instrumented assembly to disk
    /// </summary>
    /// <param name="assembly">The instrumented assembly object</param>
    /// <param name="path">The full path of the output file</param>
    /// <remark>Can raise "System.Security.Cryptography.CryptographicException: Keyset does not exist" at random
    /// when asked to strongname.</remark>
    let WriteAssembly (assembly:AssemblyDefinition) (path:string) =
      match (Visitor.strongNameKey, assembly.Name.HasPublicKey) with
      | (Some key, true) -> let pkey = new Mono.Cecil.WriterParameters()
                            pkey.StrongNameKeyPair <- key
                            assembly.Write(path, pkey)
      | _ -> assembly.Write(path)
 
                     
    /// <summary>
    /// Adjust the IL for exception handling
    /// </summary>
    /// <param name="handler">The exception handler</param>
    /// <param name="oldBoundary">The uninstrumented location</param>
    /// <param name="newBoundary">Where it has moved to</param>
    let SubstituteExceptionBoundary (handler:ExceptionHandler) (oldBoundary:Instruction)( newBoundary : Instruction ) =
      //if handler.FilterEnd = oldBoundary then handler.FilterEnd <- newBoundary // ?? Obsolete
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
           operands
           |> Seq.iteri (fun i x -> if x = oldOperand then operands.[i] <- newOperand)
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
            { state with RenameTable = updates }
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
             { state with 
                MethodBody = m.Body;
                MethodWorker = m.Body.GetILProcessor() }
           | _ -> state
         
       | MethodPoint (instruction, _, point, included) -> //of Instruction * CodeSegment * int * bool
         if included then
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
              // changes conditional (br.s, brtrue.s ...) operators to corresponding "long" ones (br, brtrue)
              state.MethodBody.SimplifyMacros()
              // changes "long" conditional operators to their short representation where possible
              state.MethodBody.OptimizeMacros()
           state

       | AfterModule -> state
       | AfterAssembly assembly ->
           let name = new FileInfo(assembly.MainModule.Name)
           let path = Path.Combine(Visitor.outputDirectory, name.Name)
           WriteAssembly assembly path
           state
       | Finish -> let counterAssemblyFile = Path.Combine(Visitor.outputDirectory, state.RecordingAssembly.Name.Name + ".dll")
                   WriteAssembly (state.RecordingAssembly) counterAssemblyFile
                   state

    Visitor.EncloseState InstrumentationVisitor initialState 
