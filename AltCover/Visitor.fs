﻿// Based upon C# code by Sergiy Sakharov (sakharov@gmail.com)
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
     | Assembly of AssemblyDefinition * bool
     | Module of ModuleDefinition * bool
     | Type of TypeDefinition * bool
     | Method of MethodDefinition * bool
     | MethodPoint of Instruction * SequencePoint * int * bool
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
    
  let internal After node =
    match node with
    | Start _ -> ToSeq Finish
    | Assembly (a,_) -> AfterAssembly a |> ToSeq
    | Module _ -> AfterModule |> ToSeq
    | Method (_, included) -> AfterMethod included |> ToSeq
    | _ -> Seq.empty<Node> 
    
  let mutable private PointNumber : int = 0

  let rec internal Deeper node =
    let defaultReturn = Seq.empty<Node>  // To move Nest inside the code
    let Nest node =
      Seq.concat [ ToSeq node ; Deeper node ; After node ]

    match node with 
    | Start paths -> paths
                     |> Seq.filter IsIncluded
                     |> Seq.map (fun x -> AssemblyDefinition.ReadAssembly(x))
                     |> Seq.map (fun x -> let included = IsIncluded x
                                          if included then ProgramDatabase.ReadSymbols(x)
                                          Assembly(x, included))
                     |> Seq.map (fun x -> BuildSequence x)
                     |> Seq.concat

    | Assembly (a, included) ->  a.Modules 
                                 |> Seq.cast
                                 |> Seq.map (fun x -> Module (x, included))
                                 |> Seq.map (fun x -> BuildSequence x)
                                 |> Seq.concat                         

    | Module (x, included) ->    PointNumber <- 0
                                 x.GetAllTypes() 
                                 |> Seq.cast  
                                 |> Seq.map (fun t -> Type (t, included && IsIncluded t))
                                 |> Seq.map (fun x -> BuildSequence x)
                                 |> Seq.concat
                             
    | Type (t, included) ->    t.Methods
                               |> Seq.cast
                               |> Seq.filter (fun (m : MethodDefinition) -> not m.IsAbstract 
                                                                            && not m.IsRuntime
                                                                            && not m.IsPInvokeImpl)
                               |> Seq.map (fun m -> Method (m, included && IsIncluded m))
                               |> Seq.map (fun x -> BuildSequence x)
                               |> Seq.concat
      
    | Method (m, _) -> 
            let segments = new Dictionary<int, SequencePoint>()
            let instructions = m.Body.Instructions
                               |> Seq.cast
                               |> Seq.filter (fun (x:Instruction) -> x.SequencePoint <> null && x.SequencePoint.StartLine <> 0xfeefee)
                               |> Seq.toList

            let number = instructions.Count();
            let point = PointNumber
            PointNumber <- point + number
            
            instructions.OrderByDescending(fun (x:Instruction) -> x.Offset)
            |> Seq.mapi (fun i x -> MethodPoint (x, x.SequencePoint, i+point, true))

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
    Start assemblies
    |> BuildSequence
    |> Seq.fold apply visitors
    |> ignore

  let EncloseState (visitor : 'State -> 'T -> 'State) (current : 'State) =
    let rec stateful l = new Fix<'T> (
                           fun (node:'T) -> 
                           let next = visitor l node
                           stateful next)
    stateful current