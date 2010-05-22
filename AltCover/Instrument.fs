// Based upon C# code by Sergiy Sakharov (sakharov@gmail.com)
// http://code.google.com/p/dot-net-coverage/source/browse/trunk/Coverage/Instrument/CounterAssemblyBuilder.cs
// http://code.google.com/p/dot-net-coverage/source/browse/trunk/Coverage/Instrument/InstrumentorVisitor.cs

namespace AltCover

open System
open System.Collections.Generic
open System.IO

open Mono.Cecil

module Instrument =
  type internal Context = {
         InstrumentedAssemblies : string list;
         RenameTable : Dictionary<String, String>;
         RecordingAssembly : AssemblyDefinition;
         RecordingMethod : MethodDefinition;
         RecordingMethodRef : MethodReference }

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
    definition
                  
  let RecordingMethod (recordingAssembly : AssemblyDefinition) =
    let trace  = typeof<AltCover.Recorder.Tracer>
    let other = trace.Assembly.GetExportedTypes()
                           |> Seq.find (fun (t:Type) -> t.Name.Contains("Instance"))
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
      match x.ToString() with
      | value when value = original -> ()
      | _ -> assemblyReferenceSubstitutions.[original] <- x.ToString()             
     )     
    assemblyReferenceSubstitutions
    
  // Simply ported from source -- probably doesn't work with the current Mono.Cecil
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
    |> Seq.iter (fun x -> 
                     SubstituteAttributeParameterScopeReferences updates x.ConstructorArguments
                     SubstituteAttributeParameterScopeReferences updates x.Properties)
                     
  let WriteAssembly (assembly:AssemblyDefinition) (path:string) =
    match Visitor.strongNameKey with
    | None -> assembly.Write(path)
    | Some key -> let pkey = new Mono.Cecil.WriterParameters()
                  pkey.StrongNameKeyPair <- key
                  assembly.Write(path, pkey)
                     
  let internal InstrumentationVisitor (state : Context) (node:Node) = 
     match node with
     | Start _ -> state
     | Assembly (model, included) ->
         let updates = UpdateStrongReferences model.Assembly state.InstrumentedAssemblies
         SubstituteAttributeScopeReferences updates model.Assembly.CustomAttributes
         if included then 
             model.Assembly.MainModule.AssemblyReferences.Add(state.RecordingAssembly.Name)
         { state with RenameTable = updates }
     | Module (m, i, am, included) -> //of ModuleDefinition * int * AssemblyModel * bool
         let restate = match included with
                       | true -> 
                         let recordingMethod = match state.RecordingMethod with
                                               | null -> RecordingMethod state.RecordingAssembly
                                               | _ -> state.RecordingMethod
                       
                         { state with 
                               RecordingMethodRef = m.Import(recordingMethod); 
                               RecordingMethod = recordingMethod }
                       | _ -> state
         SubstituteAttributeScopeReferences state.RenameTable m.CustomAttributes
         restate

     | Type _ -> state //of TypeDefinition * bool * AssemblyModel
     | Method _ -> state //of MethodDefinition * bool * AssemblyModel
     | MethodPoint _ -> state //of Instruction * CodeSegment * int * bool
     | AfterMethod _ -> state //of MethodDefinition
     | AfterModule -> state
     | AfterAssembly assembly ->
         if not (Directory.Exists(Visitor.outputDirectory)) then
                System.Console.WriteLine("Creating folder " + Visitor.outputDirectory);
                Directory.CreateDirectory(Visitor.outputDirectory) |> ignore

         let name = new FileInfo(assembly.Name.Name)
         let path = Path.Combine(Visitor.outputDirectory, name.Name)
         WriteAssembly assembly path
         state
     | Finish -> let counterAssemblyFile = Path.Combine(Visitor.outputDirectory, state.RecordingAssembly.Name.Name + ".dll")
                 WriteAssembly (state.RecordingAssembly) counterAssemblyFile
                 state