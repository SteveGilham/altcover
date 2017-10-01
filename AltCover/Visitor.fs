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
     | Assembly of AssemblyDefinition * bool
     | Module of ModuleDefinition * bool
     | Type of TypeDefinition * bool
     | Method of MethodDefinition * bool
     | MethodPoint of Instruction * SequencePoint * int * bool
     | AfterMethod of bool
     | AfterModule
     | AfterAssembly of AssemblyDefinition
     | Finish

type KeyRecord = {
         Pair : StrongNameKeyPair;
         Token : byte[] }

module KeyStore =
    let private hash = new System.Security.Cryptography.SHA1CryptoServiceProvider()

    let internal TokenOfArray (key:byte array) =
        hash.ComputeHash(key)
            |> Array.rev
            |> Array.take 8

    let internal TokenOfKey (key:StrongNameKeyPair) =
        TokenOfArray key.PublicKey

    let internal TokenAsULong (token:byte array) =
      BitConverter.ToUInt64(token, 0)

    let internal KeyToIndex (key:StrongNameKeyPair) =
      key
      |> TokenOfKey
      |> TokenAsULong

    let internal ArrayToIndex (key:byte array) =
      key
      |> TokenOfArray
      |> TokenAsULong

    let internal KeyToRecord (key:StrongNameKeyPair) =
      { Pair = key
        Token = TokenOfKey key }

type Fix<'T> = delegate of 'T -> Fix<'T>

module Visitor =

  let internal NameFilters = new List<FilterClass>();

  let mutable internal inputDirectory = "."
  let mutable internal outputDirectory = ".\\__Instrumented"
  let mutable internal reportPath = ".\\coverage.xml"
  let mutable internal defaultStrongNameKey : option<StrongNameKeyPair> = None
  let internal keys = new Dictionary<UInt64, KeyRecord>()

  let inline isNotNull x = not (isNull x)

  let internal Add (key:StrongNameKeyPair) =
    let index = KeyStore.KeyToIndex key
    keys.[index] <- KeyStore.KeyToRecord key

  let IsIncluded (nameProvider:Object) =
    not (NameFilters |> Seq.exists (Filter.Match nameProvider))

  let ToSeq node =
    List.toSeq [ node ]

  let internal After node =
    match node with
    | Start _ -> ToSeq Finish
    | Assembly (a,_) -> AfterAssembly a |> ToSeq
    | Module _ -> AfterModule |> ToSeq
    | Method (_, included) -> AfterMethod included |> ToSeq
    | _ -> Seq.empty<Node>

  let mutable private PointNumber : int = 0

  let significant (m : MethodDefinition) = 
    [Filter.IsFSharpInternal
     Filter.IsCSharpAutoProperty] 
    |> Seq.map (fun f -> f m)
    |> Seq.filter id
    |> Seq.isEmpty

  let rec internal Deeper node =
    // The pattern here is map x |> map y |> map x |> concat => collect (x >> y >> z)
    match node with
    | Start paths -> paths
                     |> Seq.filter IsIncluded
                     |> Seq.collect ((fun x -> AssemblyDefinition.ReadAssembly(x)) >>
                                     (fun x -> let included = IsIncluded x
                                               if included then ProgramDatabase.ReadSymbols(x)
                                               Assembly(x, included)) >> BuildSequence)

    | Assembly (a, included) ->  a.Modules
                                 |> Seq.cast
                                 |> Seq.collect ((fun x -> Module (x, included)) >> BuildSequence)

    | Module (x, included) ->    PointNumber <- 0
                                 x.GetAllTypes()
                                 |> Seq.cast
                                 |> Seq.collect ((fun t -> Type (t, included && IsIncluded t)) >> BuildSequence)

    | Type (t, included) ->    t.Methods
                               |> Seq.cast
                               |> Seq.filter (fun (m : MethodDefinition) -> not m.IsAbstract
                                                                            && not m.IsRuntime
                                                                            && not m.IsPInvokeImpl
                                                                            && significant m)
                               |> Seq.collect ((fun m -> Method (m, included && IsIncluded m)) >> BuildSequence)

    | Method (m, _) ->
            let instructions = m.Body.Instructions
                               |> Seq.cast
                               |> Seq.distinctBy(fun (x:Instruction) -> x.Offset)
                               |> Seq.filter (fun (x:Instruction) -> isNotNull x.SequencePoint && x.SequencePoint.StartLine <> 0xfeefee)
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