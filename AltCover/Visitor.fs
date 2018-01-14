// Based upon C# code by Sergiy Sakharov (sakharov@gmail.com)
// http://code.google.com/p/dot-net-coverage/source/browse/trunk/Coverage.Counter/Coverage.Counter.csproj

namespace AltCover

// Functional Visitor pattern

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Linq
open System.Reflection
open System.Text.RegularExpressions

open AltCover.Augment
open Mono.Cecil
open Mono.Cecil.Cil
open Mono.Cecil.Rocks

[<ExcludeFromCodeCoverage>]
type internal Node =
     | Start of seq<string>
     | Assembly of AssemblyDefinition * ISymbolReader option * bool
     | Module of ModuleDefinition * ISymbolReader option * bool
     | Type of TypeDefinition * ISymbolReader option  * bool
     | Method of MethodDefinition * MethodDebugInformation option * bool
     | MethodPoint of Instruction * SequencePoint * int * bool
     | AfterMethod of bool
     | AfterModule
     | AfterAssembly of AssemblyDefinition
     | Finish

[<ExcludeFromCodeCoverage>]
type KeyRecord = {
         Pair : StrongNameKeyPair;
         Token : byte list }

module KeyStore =
    let private hash = new System.Security.Cryptography.SHA1CryptoServiceProvider()

    let private publicKeyOfKey (key:StrongNameKeyPair) =
#if NETCOREAPP2_0
      [||]
#else
      key.PublicKey
#endif

    let internal TokenOfArray (key:byte array) =
        hash.ComputeHash(key)
            |> Array.rev
            |> Array.take 8

    let internal TokenOfKey (key:StrongNameKeyPair) =
        key |> publicKeyOfKey |> TokenOfArray |> Array.toList

    let internal TokenAsULong (token:byte array) =
      BitConverter.ToUInt64(token, 0)

    let internal KeyToIndex (key:StrongNameKeyPair) =
      key
      |> TokenOfKey
      |> List.toArray
      |> TokenAsULong

    let internal ArrayToIndex (key:byte array) =
      key
      |> TokenOfArray
      |> TokenAsULong

    let internal KeyToRecord (key:StrongNameKeyPair) =
      { Pair = key
        Token = TokenOfKey key }

[<ExcludeFromCodeCoverage>]
type Fix<'T> = delegate of 'T -> Fix<'T>

module Visitor =

  let internal NameFilters = new List<FilterClass>();
  let private specialCaseFilters = [ @"^CompareTo\$cont\@\d+\-?\d$" |> Regex |> FilterClass.Method ]

  let mutable internal inputDirectory : Option<string> = None
  let private defaultInputDirectory = "."
  let InputDirectory () = Path.GetFullPath (Option.getOrElse defaultInputDirectory inputDirectory)

  let mutable internal outputDirectory : Option<string> = None
  let private defaultOutputDirectory = "__Instrumented"
  let OutputDirectory () = Path.GetFullPath (Option.getOrElse defaultOutputDirectory outputDirectory)

  let mutable internal reportPath : Option<string> = None
  let defaultReportPath = "coverage.xml"
  let ReportPath () = Path.GetFullPath (Option.getOrElse defaultReportPath reportPath)

  let mutable internal defaultStrongNameKey : option<StrongNameKeyPair> = None
  let internal keys = new Dictionary<UInt64, KeyRecord>()

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
    | Assembly (a,_,_) -> AfterAssembly a |> ToSeq
    | Module _ -> AfterModule |> ToSeq
    | Method (_,_,included) -> AfterMethod included |> ToSeq
    | _ -> Seq.empty<Node>

  let mutable private PointNumber : int = 0

  let significant (m : MethodDefinition) =
    [Filter.IsFSharpInternal
     Filter.IsCSharpAutoProperty
     (fun m -> specialCaseFilters
               |> Seq.exists (Filter.Match m))
     ]
    |> Seq.exists (fun f -> f m)
    |> not

  let rec internal Deeper node =
    // The pattern here is map x |> map y |> map x |> concat => collect (x >> y >> z)
    match node with
    | Start paths -> paths
                     |> Seq.collect (AssemblyDefinition.ReadAssembly >>
                                     (fun x -> let included = IsIncluded x
                                               let reader = if included then ProgramDatabase.ReadSymbols(x)
                                                            else None
                                               Assembly(x, reader, included)) >> BuildSequence)

    | Assembly (a, reader, included) ->  a.Modules
                                         |> Seq.cast
                                         |> Seq.collect ((fun x -> Module (x, reader, included)) >> BuildSequence)

    | Module (x, reader, included) ->    PointNumber <- 0
                                         x.GetAllTypes()
                                         |> Seq.cast
                                         |> Seq.collect ((fun t -> Type (t, reader, included && IsIncluded t)) >> BuildSequence)

    | Type (t, reader, included) ->    t.Methods
                                       |> Seq.cast
                                       |> Seq.filter (fun (m : MethodDefinition) -> not m.IsAbstract
                                                                                    && not m.IsRuntime
                                                                                    && not m.IsPInvokeImpl
                                                                                    && significant m)
                                       |> Seq.collect ((fun m -> let dbg = reader
                                                                           |> Option.map (fun r -> r.Read m)
                                                                           |> Option.bind Option.nullable
                                                                 Method (m, dbg, included && IsIncluded m)) >> BuildSequence)

    | Method (m, dbg, included) ->
            let instructions = m.Body.Instructions
                               |> Seq.cast
                               |> Seq.distinctBy(fun (x:Instruction) -> x.Offset)
                               |> Seq.filter (fun (x:Instruction) -> match dbg with
                                                                     | None -> false
                                                                     | Some d ->
                                                                                 if d.HasSequencePoints then
                                                                                   let s = d.GetSequencePoint x
                                                                                   (not << isNull) s && s.StartLine <> 0xfeefee
                                                                                 else false)
                               |> Seq.toList

            let number = instructions.Length
            let point = PointNumber
            PointNumber <- point + number

            instructions.OrderByDescending(fun (x:Instruction) -> x.Offset)
            |> Seq.mapi (fun i x -> let s = (Option.get dbg).GetSequencePoint(x)
                                    MethodPoint (x, s, i+point, included && (IsIncluded s.Document.Url)))

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