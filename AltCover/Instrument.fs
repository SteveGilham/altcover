// Based upon C# code by Sergiy Sakharov (sakharov@gmail.com)
// http://code.google.com/p/dot-net-coverage/source/browse/trunk/Coverage/Instrument/CounterAssemblyBuilder.cs
// http://code.google.com/p/dot-net-coverage/source/browse/trunk/Coverage/Instrument/InstrumentorVisitor.cs

namespace AltCover

open System
open System.Collections.Generic
open System.IO

open Mono.Cecil

module Instrument =
  let private IntrumentedAssemblies = List<string>()
  let private recorder = ref<AssemblyDefinition> (null)

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
                  
  let GetRecordingAssembly () =
    match !recorder with
    | null -> recorder := DefineRecordingAssembly()
              !recorder
    | _ -> !recorder
                  
  let UpdateStrongReferences (assembly : AssemblyDefinition) =
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
    |> Seq.filter (fun x -> not (IntrumentedAssemblies.Contains(x.Name)))
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
      assemblyReferenceSubstitutions.[original] <- x.ToString()              
     )     
    assemblyReferenceSubstitutions
    
  // Simply ported from source -- probably doesn't work with the current Mono.Cecil
  // TODO -- fix this
  let SubstituteAttributeParameterScopeReferences (updates:Dictionary<String,String>) values =
    values 
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
                     
  let internal InstrumentationVisitor (node:Node) = 
     match node with
     | Start _ -> ()
     | Assembly (model, included) ->  //of AssemblyModel * bool
         let updates = UpdateStrongReferences model.Assembly
         SubstituteAttributeScopeReferences updates model.Assembly.CustomAttributes
         if included then 
             model.Assembly.MainModule.AssemblyReferences.Add(GetRecordingAssembly().Name)
     | Module (m, i, am, b) -> () //of ModuleDefinition * int * AssemblyModel * bool
     | Type (t,b,m) -> () //of TypeDefinition * bool * AssemblyModel
     | Method (m,b,a) -> () //of MethodDefinition * bool * AssemblyModel
     | MethodPoint (instruction, segment, i, b) -> () //of Instruction * CodeSegment * int * bool
     | AfterMethod m -> () //of MethodDefinition
     | AfterModule -> ()
     | AfterAssembly a ->  //of AssemblyDefinition
         if not (Directory.Exists(Visitor.outputDirectory)) then
                System.Console.WriteLine("Creating folder " + Visitor.outputDirectory);
                Directory.CreateDirectory(Visitor.outputDirectory) |> ignore

         let name = new FileInfo(a.Name.Name)
         let path = Path.Combine(Visitor.outputDirectory, name.Name)
         WriteAssembly a path
     | Finish -> let counterAssemblyFile = Path.Combine(Visitor.outputDirectory, GetRecordingAssembly().Name.Name + ".dll")
                 WriteAssembly (GetRecordingAssembly()) counterAssemblyFile