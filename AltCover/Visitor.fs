namespace AltCover

// Functional Visitor pattern

open Mono.Cecil
open Mono.Cecil.Cil

type internal Node = 
     | Assembly of AssemblyDefinition * bool
     | AfterAssembly of AssemblyDefinition
     | Module of ModuleDefinition * Node // TODO -- can we do any better?
     | Type of TypeDefinition
     | Method of MethodDefinition
     | MethodPoint of Instruction * CodeSegment
     | Finish
     
module Visitor =

  // TODO
  let IsIncluded _ =
    true
    
  let internal apply (visitors : seq<Node -> unit>) (node : Node) =
    visitors |> 
    Seq.iter (fun v -> v node)
    
  let internal GetModules a =
    match a with
    | Assembly(a, b) -> a.Modules |> Seq.cast
    | _ -> Seq.empty<ModuleDefinition>
    
  // TODO
  let internal VisitModule (visitors : seq<Node -> unit>) m =
    ()

  let internal Visit (visitors : seq<Node -> unit>) (assemblies : seq<string>) =
    let outerVisit = assemblies
                     |> Seq.filter IsIncluded
                     |> Seq.map (fun x -> AssemblyDefinition.ReadAssembly(x))
                     |> Seq.map (fun x -> Assembly(x, IsIncluded x))
                     
    outerVisit                     
    |> Seq.iter (apply visitors)
                     
    seq { for x in outerVisit do
           for y in GetModules x do
             yield Module(y, x) } 
    |> Seq.iter (VisitModule visitors)                               
                     
    outerVisit
    |> Seq.iter (fun x -> 
                 match x with 
                 | Assembly(a, b) -> apply visitors (AfterAssembly(a))
                 | _ -> ())                     
                     
    apply visitors Finish