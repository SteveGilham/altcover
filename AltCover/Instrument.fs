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

module Instrument =
  type private Context = { InstrumentedAssemblies : string list;
                           RenameTable : Dictionary<String, String>;
                           ModuleId : int;
                           RecordingAssembly : AssemblyDefinition;
                           RecordingMethod : MethodDefinition;
                           RecordingMethodRef : MethodReference;
                           MethodBody : MethodBody;
                           MethodWorker :ILProcessor }

  let internal InstrumentGenerator (assemblies : string list) =
    let initialState = {
             InstrumentedAssemblies = assemblies;
             RenameTable = null;
             ModuleId = 0;
             RecordingAssembly = null;
             RecordingMethod = null;
             RecordingMethodRef = null;
             MethodBody = null;
             MethodWorker = null }

    let RecorderInstanceType () =
      let trace  = typeof<AltCover.Recorder.Tracer>
      trace.Assembly.GetExportedTypes()
                            |> Seq.find (fun (t:Type) -> t.Name.Contains("Instance"))

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
                  
    let RecordingMethod (recordingAssembly : AssemblyDefinition) =
      let other = RecorderInstanceType()
      let token = other.GetMethod("Visit").MetadataToken
      recordingAssembly.MainModule.LookupToken(token) :?> MethodDefinition 
    
    let UpdateStrongReferences (assembly : AssemblyDefinition) (assemblies : string list) =
      let assemblyReferenceSubstitutions = new Dictionary<String, String>()
      let effectiveKey = if assembly.Name.HasPublicKey then None else Visitor.strongNameKey
      match effectiveKey with
      | None -> assembly.Name.HasPublicKey <- false
                assembly.Name.PublicKey <- null
                assembly.Name.PublicKeyToken <- null
      | Some key -> assembly.Name.HasPublicKey <- true
                    assembly.Name.PublicKey <- key.PublicKey
                
      assembly.MainModule.AssemblyReferences                  
      |> Seq.cast<AssemblyNameReference>
      |> Seq.filter (fun x -> assemblies 
                              |> List.forall (fun y -> not <| y.Equals(x.Name)))
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
    
    (*// Simply ported from source -- probably doesn't work with the current Mono.Cecil
    // TODO -- fix this
    let SubstituteAttributeParameterScopeReferences (updates:Dictionary<String,String>) (values:System.Collections.IList)  =
      values 
      |> Seq.cast
      |> Seq.iteri (fun i x ->
         let parameter = ref (x.ToString())
         match !parameter with
         | null -> ()
         | _ -> updates.Keys 
                |> Seq.cast<String>
                |> Seq.filter (!parameter).Contains
                |> Seq.iter (fun o ->
                  parameter := (!parameter).Replace(o, updates.[o]))
         )
    
    let SubstituteAttributeScopeReferences updates attributes =
      attributes
      |> Seq.cast<CustomAttribute>
      |> Seq.iter ignore (*fun x -> 
                       SubstituteAttributeParameterScopeReferences updates x.ConstructorArguments
                       SubstituteAttributeParameterScopeReferences updates x.Properties*)
    *)
    
    let WriteAssembly (assembly:AssemblyDefinition) (path:string) =
      match Visitor.strongNameKey with
      | None -> assembly.Write(path)
      | Some key -> let pkey = new Mono.Cecil.WriterParameters()
                    pkey.StrongNameKeyPair <- key
                    assembly.Write(path, pkey)
                     
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

    let InstrumentationVisitor (state : Context) (node:Node) = 
       match node with
       | Start _ -> { state with RecordingAssembly = DefineRecordingAssembly() }
       | Assembly (model, included) ->
           let updates = UpdateStrongReferences model.Assembly state.InstrumentedAssemblies
           ////SubstituteAttributeScopeReferences updates model.Assembly.CustomAttributes
           if included then 
               model.Assembly.MainModule.AssemblyReferences.Add(state.RecordingAssembly.Name)
           { state with RenameTable = updates }
       | Module (m, id, _, included) -> //of ModuleDefinition * int * AssemblyModel * bool
           let restate = match included with
                         | true -> 
                           let recordingMethod = match state.RecordingMethod with
                                                 | null -> RecordingMethod state.RecordingAssembly
                                                 | _ -> state.RecordingMethod
                         
                           { state with 
                                 RecordingMethodRef = m.Import(recordingMethod); 
                                 RecordingMethod = recordingMethod }
                         | _ -> state
           ////SubstituteAttributeScopeReferences state.RenameTable m.CustomAttributes
           { restate with ModuleId = id + 1}
         
       | Type ( _(*typedef*),_,_) -> //of TypeDefinition * bool * AssemblyModel
           ////SubstituteAttributeScopeReferences state.RenameTable typedef.CustomAttributes
           state 
       | Method (m,  included,_) -> //of MethodDefinition * bool * AssemblyModel
           ////SubstituteAttributeScopeReferences state.RenameTable m.CustomAttributes
           match included with
           | true ->
             { state with 
                MethodBody = m.Body;
                MethodWorker = m.Body.GetILProcessor() }
           | _ -> state
         
       | MethodPoint (instruction, _, point, included) -> //of Instruction * CodeSegment * int * bool
         if included then
              let counterMethodCall = state.MethodWorker.Create(OpCodes.Call, state.RecordingMethodRef);
              let instrLoadModuleId = state.MethodWorker.Create(OpCodes.Ldc_I4, state.ModuleId);
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
