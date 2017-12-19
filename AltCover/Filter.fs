namespace AltCover

open System
open System.Diagnostics
open System.IO
open System.Runtime.CompilerServices

open Mono.Cecil
open AltCover.Augment

type internal FilterClass =
  | File of string
  | Assembly of string
  | Type of string
  | Method of string
  | Attribute of string

module Filter =

  let internal Match (nameProvider:Object) (filter:FilterClass) =
    match filter with
    | File name -> match nameProvider with
                   | :? string as fileName -> Path.GetFileName(fileName).Contains(name)
                   | _ -> false
    | Assembly name -> match nameProvider with
                       | :? AssemblyDefinition as assembly -> assembly.Name.Name.Contains(name)
                       | _ -> false
    | Type name -> match nameProvider with
                   | :? TypeDefinition as typeDef -> typeDef.FullName.Contains(name)
                   | _ -> false
    | Method name -> match nameProvider with
                     | :? MethodDefinition as methodDef -> methodDef.Name.Contains(name)
                     | _ -> false
    | Attribute name -> match nameProvider with
                        | :? ICustomAttributeProvider as attributeProvider ->
                            attributeProvider.HasCustomAttributes &&
                                 attributeProvider.CustomAttributes
                                 |> Seq.cast<CustomAttribute>
                                 |> Seq.exists (fun attr ->
                                    attr.Constructor.DeclaringType.FullName.Contains(name))
                        | _ -> false

  let internal IsCSharpAutoProperty (m:MethodDefinition) =
      (m.IsSetter || m.IsGetter) && m.HasCustomAttributes &&
        m.CustomAttributes
        |> Seq.exists (fun x -> x.AttributeType.FullName = typeof<CompilerGeneratedAttribute>.FullName)

  let internal IsFSharpInternal (m:MethodDefinition) =

    // Discriminated Union/Sum/Algebraic data types are implemented as
    // subtypes nested in the base type
    // Algebraic types have debug proxies nested in the base type which are not attributed at the type level
    let baseType = Option.nullable m.DeclaringType.DeclaringType
                  |> Option.filter (fun t -> t.HasCustomAttributes)
                  |> Option.map (fun t -> t.CustomAttributes :> seq<CustomAttribute>)
                  |> Option.filter (Seq.isEmpty >> not)
                  |> Option.getOrElse Seq.empty<CustomAttribute>

    let thisType = Some m.DeclaringType
                  |> Option.filter (fun t -> t.HasCustomAttributes)
                  |> Option.map (fun t -> t.CustomAttributes :> seq<CustomAttribute>)
                  |> Option.filter (Seq.isEmpty >> not)
                  |> Option.getOrElse Seq.empty<CustomAttribute>

    // Use string literals since Mono doesn't return a Type
    let mappings = Seq.concat [baseType; thisType]
                   |> Seq.filter (fun x -> x.AttributeType.FullName = "Microsoft.FSharp.Core.CompilationMappingAttribute")
                   |> Seq.exists (fun x -> let arg1 = x.ConstructorArguments |> Seq.head
                                           match (arg1.Value :?> SourceConstructFlags) &&& SourceConstructFlags.KindMask with
                                           | SourceConstructFlags.SumType
                                           | SourceConstructFlags.RecordType -> true
                                           | _ -> false)

    // record type has getters marked as field
    let fieldGetter = match m.IsGetter with
                      | false -> false
                      | _ -> let owner = m.DeclaringType.Properties
                                         |> Seq.filter (fun x -> x.GetMethod = m)
                                         |> Seq.head
                             if owner.HasCustomAttributes then
                                owner.CustomAttributes
                                |> Seq.filter (fun x -> x.AttributeType.FullName = "Microsoft.FSharp.Core.CompilationMappingAttribute")
                                |> Seq.exists (fun x -> let arg1 = x.ConstructorArguments |> Seq.head
                                                        (arg1.Value :?> SourceConstructFlags) &&& SourceConstructFlags.KindMask = SourceConstructFlags.Field)
                             else false

    mappings &&
      (fieldGetter || m.IsConstructor || 
         (m.HasCustomAttributes && m.CustomAttributes
                                   |> Seq.exists (fun x -> let fullName = x.AttributeType.FullName
                                                           fullName = typeof<CompilerGeneratedAttribute>.FullName ||
                                                           fullName = typeof<DebuggerNonUserCodeAttribute>.FullName ||
                                                           fullName = typeof<CompilationMappingAttribute>.FullName)))