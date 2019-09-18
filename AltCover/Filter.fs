namespace AltCover

open System
open System.Diagnostics
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Runtime.CompilerServices
open System.Text.RegularExpressions

open Mono.Cecil
open Mono.Cecil.Cil
open AltCover.Augment

[<ExcludeFromCodeCoverage; NoComparison>]
type internal FilterSense =
  | Exclude
  | Include

[<ExcludeFromCodeCoverage; NoComparison>]
type internal FilterClass =
  | File of (Regex * FilterSense)
  | Assembly of (Regex * FilterSense)
  | Module of (Regex * FilterSense)
  | Type of (Regex * FilterSense)
  | Method of (Regex * FilterSense)
  | Attribute of (Regex * FilterSense)
  | Path of (Regex * FilterSense)
  member this.Apply
    with get () =
      match this with
      | File (_, Exclude)
      | Assembly (_, Exclude)
      | Module (_, Exclude)
      | Type (_, Exclude)
      | Method (_, Exclude)
      | Attribute (_, Exclude)
      | Path (_, Exclude) -> id
      | _ -> not

module internal Filter =

  let rec private MatchAttribute (name : Regex) f (nameProvider : Object) =
    (match nameProvider with
     | :? MethodDefinition as m ->
       if m.IsGetter || m.IsSetter then
         let owner =
           m.DeclaringType.Properties
           |> Seq.filter (fun x -> x.GetMethod = m || x.SetMethod = m)
           |> Seq.head
         MatchAttribute name f owner
       else false
     | _ -> false)
    || (match nameProvider with
        | :? ICustomAttributeProvider as attributeProvider ->
          attributeProvider.HasCustomAttributes
          && attributeProvider.CustomAttributes
             |> Seq.cast<CustomAttribute>
             |> Seq.exists (fun attr -> name.IsMatch attr.AttributeType.FullName)
             |> f
        | _ -> false)

  let MatchItem<'a> (name : Regex) f (nameProvider : Object) (toName : 'a -> string) =
    match nameProvider with
    | :? 'a as item ->
      item
      |> toName
      |> name.IsMatch
      |> f
    | _ -> false

  let internal Match (nameProvider : Object) (filter : FilterClass) =
    let f = filter.Apply
    match filter with
    | File (name, _) -> MatchItem<string> name f nameProvider Path.GetFileName
    | Assembly (name, _) ->
      MatchItem<AssemblyDefinition> name f nameProvider (fun assembly -> assembly.Name.Name)
    | Module (name, _) ->
      MatchItem<ModuleDefinition> name f nameProvider
        (fun ``module`` -> ``module``.Assembly.Name.Name)
    | Type (name, _) ->
      MatchItem<TypeDefinition> name f nameProvider (fun typeDef -> typeDef.FullName)
    | Method (name, _) ->
      MatchItem<MethodDefinition> name f nameProvider (fun methodDef -> methodDef.Name)
    | Attribute (name, _) -> MatchAttribute name f nameProvider
    | Path (name, _) -> MatchItem<string> name f nameProvider Path.GetFullPath

  let internal IsCSharpAutoProperty(m : MethodDefinition) =
    (m.IsSetter || m.IsGetter) && m.HasCustomAttributes
    && m.CustomAttributes
       |> Seq.exists
            (fun x ->
            x.AttributeType.FullName = typeof<CompilerGeneratedAttribute>.FullName)

  let internal IsFSharpInternalAlgebraic(m : MethodDefinition) =
    // Discriminated Union/Sum/Algebraic data types are implemented as
    // subtypes nested in the base type
    // Algebraic types have debug proxies nested in the base type which are not attributed at the type level
    let baseType =
      Option.nullable m.DeclaringType.DeclaringType
      |> Option.filter (fun t -> t.HasCustomAttributes)
      |> Option.map (fun t -> t.CustomAttributes :> seq<CustomAttribute>)
      |> Option.filter (Seq.isEmpty >> not)
      |> Option.getOrElse Seq.empty<CustomAttribute>

    let thisType =
      Some m.DeclaringType
      |> Option.filter (fun t -> t.HasCustomAttributes)
      |> Option.map (fun t -> t.CustomAttributes :> seq<CustomAttribute>)
      |> Option.filter (Seq.isEmpty >> not)
      |> Option.getOrElse Seq.empty<CustomAttribute>

    // Use string literals since Mono doesn't return a Type
    let mappings =
      Seq.concat [ baseType; thisType ]
      |> Seq.filter
           (fun x ->
           x.AttributeType.FullName = "Microsoft.FSharp.Core.CompilationMappingAttribute")
      |> Seq.exists (fun x ->
           let arg1 =
             Enum.ToObject(typeof<SourceConstructFlags>,
                           x.GetBlob()
                           |> Seq.skip 2
                           |> Seq.head) // (x.ConstructorArguments |> Seq.head).Value
           match (arg1 :?> SourceConstructFlags) &&& SourceConstructFlags.KindMask with
           | SourceConstructFlags.SumType | SourceConstructFlags.RecordType -> true
           | _ -> false)

    // record type has getters marked as field
    let fieldGetter =
      match m.IsGetter with
      | false -> false
      | _ ->
        let owner =
          m.DeclaringType.Properties
          |> Seq.filter (fun x -> x.GetMethod = m)
          |> Seq.head
        if owner.HasCustomAttributes then
          owner.CustomAttributes
          |> Seq.filter
               (fun x ->
               x.AttributeType.FullName = "Microsoft.FSharp.Core.CompilationMappingAttribute")
          |> Seq.exists
               (fun x ->
               let arg1 =
                 Enum.ToObject(typeof<SourceConstructFlags>,
                               x.GetBlob()
                               |> Seq.skip 2
                               |> Seq.head) // (x.ConstructorArguments |> Seq.head).Value
               (arg1 :?> SourceConstructFlags) &&& SourceConstructFlags.KindMask = SourceConstructFlags.Field)
        else false

    mappings
    && (fieldGetter || m.IsConstructor
        || (m.HasCustomAttributes
            && m.CustomAttributes
               |> Seq.exists
                    (fun x ->
                    let fullName = x.AttributeType.FullName
                    fullName = typeof<CompilerGeneratedAttribute>.FullName
                    || fullName = typeof<DebuggerNonUserCodeAttribute>.FullName
                    || fullName = typeof<CompilationMappingAttribute>.FullName)))

  let internal IsFSharpAutoProperty(m : MethodDefinition) =
    let body = m.Body.Instructions
    if m.IsSetter then
      body
      |> Seq.tryFind (fun i -> i.OpCode = OpCodes.Stfld)
      |> Option.map
           (fun i ->
           let f = i.Operand :?> FieldReference
           (f.DeclaringType.FullName = m.DeclaringType.FullName)
           && m.Name.Replace("set_", String.Empty) + "@" = f.Name)
      |> Option.getOrElse false
    else if m.IsGetter then
      body
      |> Seq.tryFind (fun i -> i.OpCode = OpCodes.Ldfld)
      |> Option.map
           (fun i ->
           let f = i.Operand :?> FieldReference
           (f.DeclaringType.FullName = m.DeclaringType.FullName)
           && m.Name.Replace("get_", String.Empty) + "@" = f.Name)
      |> Option.getOrElse false
    else false

  let internal IsFSharpInternal(m : MethodDefinition) =
    IsFSharpAutoProperty m || IsFSharpInternalAlgebraic m