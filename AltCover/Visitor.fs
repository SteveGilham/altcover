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
open AltCover.Base
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
     | MethodPoint of Instruction * SequencePoint option * int * bool
     | AfterMethod of bool
     | AfterType
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

    let internal HashFile sPath =
      use stream = File.OpenRead sPath
      stream |>hash.ComputeHash |> BitConverter.ToString

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

  let mutable internal reportFormat : Option<ReportFormat> = None
  let defaultReportFormat = ReportFormat.NCover
  let ReportFormat () = (Option.getOrElse defaultReportFormat reportFormat)

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
    | Type _ -> AfterType |> ToSeq
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

  let private StartVisit (paths:seq<string>) buildSequence =
        paths
        |> Seq.collect (AssemblyDefinition.ReadAssembly >>
                        (fun x -> let included = IsIncluded x
                                  let reader = if included then ProgramDatabase.ReadSymbols(x)
                                                            else None
                                  Assembly(x, reader, included)) >> buildSequence)

  let private VisitAssembly (a:AssemblyDefinition) reader included buildSequence =
        a.Modules
        |> Seq.cast
        |> Seq.collect ((fun x -> Module (x, reader, included && IsIncluded x)) >> buildSequence)

  let private VisitModule (x:ModuleDefinition) reader included buildSequence =
        PointNumber <- 0
        [x]
        |> Seq.takeWhile (fun _ -> included)
        |> Seq.collect(fun x -> x.GetAllTypes() |> Seq.cast)
        |> Seq.collect ((fun t -> Type (t, reader, included && IsIncluded t)) >> buildSequence)

  let private VisitType (t:TypeDefinition) (reader:ISymbolReader option) included buildSequence =
        t.Methods
        |> Seq.cast
        |> Seq.filter (fun (m : MethodDefinition) -> not m.IsAbstract
                                                    && not m.IsRuntime
                                                    && not m.IsPInvokeImpl
                                                    && significant m)
        |> Seq.collect ((fun m -> let dbg = reader
                                            |> Option.map (fun r -> r.Read m)
                                            |> Option.bind Option.nullable
                                  Method (m, dbg, included && IsIncluded m)) >> buildSequence)

  let private VisitMethod (m:MethodDefinition) (dbg:MethodDebugInformation option) included =
            let rawInstructions = m.Body.Instructions
            let instructions = rawInstructions
                               |> Seq.cast
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

            if included && instructions |> Seq.isEmpty && rawInstructions |> Seq.isEmpty |> not then
                rawInstructions
                |> Seq.take 1
                |> Seq.map (fun i -> MethodPoint (i, None, m.MetadataToken.ToInt32(), included))
            else
                instructions.OrderByDescending(fun (x:Instruction) -> x.Offset)
                |> Seq.mapi (fun i x -> let s = (Option.get dbg).GetSequencePoint(x)
                                        MethodPoint (x, Some s, i+point, included && (IsIncluded s.Document.Url)))

  let rec internal Deeper node =
    // The pattern here is map x |> map y |> map x |> concat => collect (x >> y >> z)
    match node with
    | Start paths -> StartVisit paths  BuildSequence
    | Assembly (a, reader, included) ->  VisitAssembly a reader included BuildSequence
    | Module (x, reader, included) ->  VisitModule x  reader included BuildSequence
    | Type (t, reader, included) -> VisitType t reader  included BuildSequence
    | Method (m, dbg, included) -> VisitMethod m dbg included
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