// Based upon C# code by Sergiy Sakharov (sakharov@gmail.com)
// http://code.google.com/p/dot-net-coverage/source/browse/trunk/Coverage.Counter/Coverage.Counter.csproj

namespace AltCover

// Functional Visitor pattern

open System
open System.Collections.Generic
open System.Linq
open System.Reflection

open Mono.Cecil
open Mono.Cecil.Cil
open Mono.Cecil.Rocks

type internal Node = 
     | Start of seq<string>
     | Assembly of AssemblyModel * bool
     | Module of ModuleDefinition * int * AssemblyModel * bool
     | Type of TypeDefinition * bool * AssemblyModel
     | Method of MethodDefinition * bool * AssemblyModel
     | MethodPoint of Instruction * CodeSegment * int * bool
     | AfterMethod of bool
     | AfterModule
     | AfterAssembly of AssemblyDefinition
     | Finish

type Fix<'T> = delegate of 'T -> Fix<'T>
     
module Visitor =

  let internal NameFilters = new List<FilterClass>();

  let mutable internal inputDirectory = "."
  let mutable internal outputDirectory = ".\\__Instrumented"
  let mutable internal reportPath = ".\\coverage.xml"
  let mutable internal strongNameKey : option<StrongNameKeyPair> = None

  let IsIncluded (nameProvider:Object) =
    not (NameFilters |> Seq.exists (Filter.Match nameProvider))  
    
  let ToSeq node =
    seq {yield node} 
    
  let rec PopInstructions (key:UInt32) (list : list<Instruction>) =
    match list with
    | [] -> ([], None)
    | h::[] when h.Offset = int key -> ([], Some h)
    | h::t when h.Offset > int key || h.Offset = 0 -> PopInstructions key t
    | h::t when h.Offset = int key -> (t, Some h)
    | _ -> (list, None)
    
  let internal After node =
    match node with
    | Start _ -> ToSeq Finish
    | Assembly (a,_) -> AfterAssembly a.Assembly |> ToSeq
    | Module _ -> AfterModule |> ToSeq
    | Method (_,included,_) -> AfterMethod included |> ToSeq
    | _ -> Seq.empty<Node> 
    
  let mutable private PointNumber : int = 0
  let mutable private ModuleNumber : int = 0

  let rec internal Deeper node =
    let defaultReturn = Seq.empty<Node>  // To move Nest inside the code
    let Nest node =
      Seq.concat [ ToSeq node ; Deeper node ; After node ]

    match node with 
    | Start paths -> paths
                     |> Seq.filter IsIncluded
                     |> Seq.map (fun x -> ProgramDatabase.LoadAssembly(x))
                     |> Seq.map (fun x -> Assembly(x, IsIncluded x.Assembly))  
                     |> Seq.map (fun x -> BuildSequence x)
                     |> Seq.concat

    | Assembly (a, b) -> a.Assembly.Modules 
                         |> Seq.cast
                         |> Seq.map (fun x -> 
                                     let i = ModuleNumber + 1
                                     ModuleNumber <- i
                                     Module (x, i, a, b))

                         |> Seq.map (fun x -> BuildSequence x)
                         |> Seq.concat

                         
    | Module (x, _, a, included) -> PointNumber <- 0
                                    x.GetAllTypes() 
                                    |> Seq.cast  
                                    |> Seq.map (fun t -> Type (t, included && IsIncluded t, a))
                                    |> Seq.map (fun x -> BuildSequence x)
                                    |> Seq.concat
                             
    | Type (t, included, a) -> t.Methods
                               |> Seq.cast
                               |> Seq.filter (fun (m : MethodDefinition) -> not m.IsAbstract 
                                                                            && not m.IsRuntime
                                                                            && not m.IsPInvokeImpl)
                               |> Seq.map (fun m -> Method (m, included && IsIncluded m, a))
                               |> Seq.map (fun x -> BuildSequence x)
                               |> Seq.concat
      
    | Method (m, _, a) -> 
            let segments = ProgramDatabase.GetCodeSegmentsForMethod a m
            let instructions = m.Body.Instructions
                               |> Seq.cast
                               |> Seq.toList
                               |> List.rev
            
            segments.OrderByDescending(fun p -> p.Key)
            |> Seq.cast
            |> Seq.scan (fun state (p:KeyValuePair<UInt32, CodeSegment>) -> 
                            let _, _, list = state
                            let newlist, inst = PopInstructions p.Key list
                            (Some p, inst, newlist))
                            (None, None, instructions)
            |> Seq.filter (fun x ->
                            let a,b,_ = x
                            a <> None && b <> None)
            |> Seq.map (fun x ->
                         let i = PointNumber
                         PointNumber <- i + 1
                         match x with 
                         | Some a, Some b, _ -> MethodPoint (b, a.Value, i, true) // TODO
                         | _ -> failwith "unexpected None value")

    | _ -> Seq.empty<Node>                    
  
  and internal BuildSequence node =
    Seq.concat [ ToSeq node ; Deeper node ; After node ]
  
  let internal invoke (node : Node) (visitor:Fix<Node>)  =
    visitor.Invoke(node)

  let internal apply (visitors : list<Fix<Node>>) (node : Node) =
    visitors |> 
    List.map (invoke node) 

  let internal Visit (visitors : list<Fix<Node>>) (assemblies : seq<string>) =
    PointNumber <- 0
    ModuleNumber <- 0
    Start assemblies
    |> BuildSequence
    |> Seq.fold apply visitors
    |> ignore

  let EncloseState (visitor : 'State -> 'T -> 'State) (current : 'State) =
    let bootstrap = current // fudge fun-hoisting bug in analysis
    let rec stateful l = new Fix<'T> (
                           fun (node:'T) -> 
                           let next = visitor l node
                           stateful next)
    stateful bootstrap