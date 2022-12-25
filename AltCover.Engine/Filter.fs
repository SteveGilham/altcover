namespace AltCover

open System
open System.Diagnostics
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Runtime.CompilerServices
open System.Text.RegularExpressions

open Mono.Cecil
open Mono.Cecil.Cil

open AltCover.Shared

[<ExcludeFromCodeCoverage; NoComparison>]
type internal FilterSense =
  | Exclude
  | Include

[<ExcludeFromCodeCoverage; NoComparison>]
type internal FilterScope =
  | File
  | Assembly
  | Module
  | Type
  | Method
  | Attribute
  | Path

[<ExcludeFromCodeCoverage; NoComparison>]
type internal StaticFilter =
  | NoFilter
  | AsCovered
  | Hidden

[<ExcludeFromCodeCoverage;
  NoComparison;
  SuppressMessage("Gendarme.Rules.Naming",
                  "UseCorrectSuffixRule",
                  Justification = "ex is part of Regex");
  AutoSerializable(false)>]
type internal FilterRegex =
  { Regex: Regex
    Sense: FilterSense }
  static member Exclude(s: Regex) = { Regex = s; Sense = Exclude }

[<ExcludeFromCodeCoverage; NoComparison; AutoSerializable(false)>]
type internal FilterClass =
  { Scope: FilterScope
    Regex: Regex
    Sense: FilterSense }

  static member internal Build scope (regex: FilterRegex) =
    { Scope = scope
      Regex = regex.Regex
      Sense = regex.Sense }

  member internal this.Apply =
    if this.Sense = Exclude then id else not

[<RequireQualifiedAccess>]
module internal Filter =

  // Implementation details
  module private I =

    let refStructMagic =
      [ 0x01uy
        0x00uy
        0x52uy
        0x54uy
        0x79uy
        0x70uy
        0x65uy
        0x73uy
        0x20uy
        0x77uy
        0x69uy
        0x74uy
        0x68uy
        0x20uy
        0x65uy
        0x6duy
        0x62uy
        0x65uy
        0x64uy
        0x64uy
        0x65uy
        0x64uy
        0x20uy
        0x72uy
        0x65uy
        0x66uy
        0x65uy
        0x72uy
        0x65uy
        0x6euy
        0x63uy
        0x65uy
        0x73uy
        0x20uy
        0x61uy
        0x72uy
        0x65uy
        0x20uy
        0x6euy
        0x6fuy
        0x74uy
        0x20uy
        0x73uy
        0x75uy
        0x70uy
        0x70uy
        0x6fuy
        0x72uy
        0x74uy
        0x65uy
        0x64uy
        0x20uy
        0x69uy
        0x6euy
        0x20uy
        0x74uy
        0x68uy
        0x69uy
        0x73uy
        0x20uy
        0x76uy
        0x65uy
        0x72uy
        0x73uy
        0x69uy
        0x6fuy
        0x6euy
        0x20uy
        0x6fuy
        0x66uy
        0x20uy
        0x79uy
        0x6fuy
        0x75uy
        0x72uy
        0x20uy
        0x63uy
        0x6fuy
        0x6duy
        0x70uy
        0x69uy
        0x6cuy
        0x65uy
        0x72uy
        0x2euy
        0x01uy
        0x00uy
        0x00uy ]

    let rec internal matchAttribute (name: Regex) f (nameProvider: Object) =
      (match nameProvider with
       | :? MethodDefinition as m ->
         if m.IsGetter || m.IsSetter then
           let owner =
             m.DeclaringType.Properties
             |> Seq.filter (fun x -> x.GetMethod = m || x.SetMethod = m)
             |> Seq.head

           matchAttribute name f owner
         else
           false
       | _ -> false)
      || (match nameProvider with
          | :? ICustomAttributeProvider as attributeProvider ->
            attributeProvider.HasCustomAttributes
            && attributeProvider.CustomAttributes
               |> Seq.cast<CustomAttribute>
               |> Seq.filter (fun a ->
                 match nameProvider with
                 | :? TypeDefinition as t ->
                   if
                     t.IsValueType
                     && a.AttributeType.FullName
                        == "System.ObsoleteAttribute"
                   then
                     let blob = a.GetBlob()

                     (blob.Length <> 88)
                     || (Seq.zip blob refStructMagic
                         |> Seq.exists (fun (l, r) -> l <> r))
                   else
                     true
                 | _ -> true)
               |> Seq.exists (fun attr -> name.IsMatch attr.AttributeType.FullName)
               |> f
          | _ -> false)

    let internal matchItem<'a>
      (name: Regex)
      f
      (nameProvider: Object)
      (toName: 'a -> string)
      =
      match nameProvider with
      | :? 'a as item -> item |> toName |> name.IsMatch |> f
      | _ -> false

    let internal isFSharpInternalAlgebraic (m: MethodDefinition) =
      // Discriminated Union/Sum/Algebraic data types are implemented as
      // subtypes nested in the base type
      // Algebraic types have debug proxies nested in the base type which are not attributed at the type level
      let baseType =
        Option.ofObj m.DeclaringType.DeclaringType
        |> Option.filter (fun t -> t.HasCustomAttributes)
        |> Option.map (fun t -> t.CustomAttributes :> seq<CustomAttribute>)
        |> Option.filter (Seq.isEmpty >> not)
        |> Option.defaultValue Seq.empty<CustomAttribute>

      let thisType =
        Some m.DeclaringType
        |> Option.filter (fun t -> t.HasCustomAttributes)
        |> Option.map (fun t -> t.CustomAttributes :> seq<CustomAttribute>)
        |> Option.filter (Seq.isEmpty >> not)
        |> Option.defaultValue Seq.empty<CustomAttribute>

      // Use string literals since Mono doesn't return a Type
      let mappings =
        Seq.concat [ baseType; thisType ]
        |> Seq.filter (fun x ->
          x.AttributeType.FullName
          == "Microsoft.FSharp.Core.CompilationMappingAttribute")
        |> Seq.exists (fun x ->
          let arg1 =
            Enum.ToObject(
              typeof<SourceConstructFlags>,
              x.GetBlob() |> Seq.skip 2 |> Seq.head
            ) // (x.ConstructorArguments |> Seq.head).Value

          match
            (arg1 :?> SourceConstructFlags)
            &&& SourceConstructFlags.KindMask
          with
          | SourceConstructFlags.SumType
          | SourceConstructFlags.RecordType -> true
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
            |> Seq.filter (fun x ->
              x.AttributeType.FullName
              == "Microsoft.FSharp.Core.CompilationMappingAttribute")
            |> Seq.exists (fun x ->
              let arg1 =
                Enum.ToObject(
                  typeof<SourceConstructFlags>,
                  x.GetBlob() |> Seq.skip 2 |> Seq.head
                ) // (x.ConstructorArguments |> Seq.head).Value

              (arg1 :?> SourceConstructFlags)
              &&& SourceConstructFlags.KindMask = SourceConstructFlags.Field)
          else
            false

      mappings
      && (fieldGetter
          || m.IsConstructor
          || (m.HasCustomAttributes
              && m.CustomAttributes
                 |> Seq.exists (fun x ->
                   let fullName = x.AttributeType.FullName

                   fullName
                   == typeof<CompilerGeneratedAttribute>.FullName
                   || fullName
                      == typeof<DebuggerNonUserCodeAttribute>.FullName
                   || fullName
                      == typeof<CompilationMappingAttribute>.FullName)))

    let internal isFSharpAutoProperty (m: MethodDefinition) =
      let body = m.Body.Instructions

      if m.IsSetter then
        body
        |> Seq.tryFind (fun i -> i.OpCode = OpCodes.Stfld)
        |> Option.map (fun i ->
          let f = i.Operand :?> FieldReference

          (f.DeclaringType.FullName
           == m.DeclaringType.FullName)
          && m.Name.Replace("set_", String.Empty) + "@"
             == f.Name)
        |> Option.defaultValue false
      else if m.IsGetter then
        body
        |> Seq.tryFind (fun i -> i.OpCode = OpCodes.Ldfld)
        |> Option.map (fun i ->
          let f = i.Operand :?> FieldReference

          (f.DeclaringType.FullName
           == m.DeclaringType.FullName)
          && m.Name.Replace("get_", String.Empty) + "@"
             == f.Name)
        |> Option.defaultValue false
      else
        false

  // "Public" API
  let internal ``match`` (nameProvider: Object) (filter: FilterClass) =
    let f = filter.Apply

    match filter.Scope with
    | File -> I.matchItem<string> filter.Regex f nameProvider Path.GetFileName
    | Assembly ->
      I.matchItem<AssemblyDefinition> filter.Regex f nameProvider (fun assembly ->
        assembly.Name.Name)
    | Module ->
      I.matchItem<ModuleDefinition> filter.Regex f nameProvider (fun ``module`` ->
        ``module``.Assembly.Name.Name)
    | Type ->
      I.matchItem<TypeDefinition> filter.Regex f nameProvider (fun typeDef ->
        typeDef.FullName)
    | Method ->
      I.matchItem<MethodDefinition>
        filter.Regex
        f
        nameProvider
        // Interfaces w/implementation have no base type
        (fun methodDef ->
          let decltype =
            methodDef.DeclaringType.BaseType
            |> Option.ofObj
            |> Option.map (fun x -> x.Name)
            |> Option.defaultValue String.Empty

          let name = methodDef.Name

          if
            decltype.StartsWith("FSharpFunc", StringComparison.Ordinal)
            || decltype == "FSharpTypeFunc"
          then
            methodDef.DeclaringType.Name + "." + name
          else
            name)
    | Attribute -> I.matchAttribute filter.Regex f nameProvider
    | Path -> I.matchItem<string> filter.Regex f nameProvider canonicalPath

  let internal isFSharpInternal (m: MethodDefinition) =
    I.isFSharpAutoProperty m
    || I.isFSharpInternalAlgebraic m

  let internal isCSharpAutoProperty (m: MethodDefinition) =
    (m.IsSetter || m.IsGetter)
    && m.HasCustomAttributes
    && m.CustomAttributes
       |> Seq.exists (fun x ->
         x.AttributeType.FullName
         == typeof<CompilerGeneratedAttribute>.FullName)

[<assembly: SuppressMessage("Gendarme.Rules.Exceptions",
                            "InstantiateArgumentExceptionCorrectlyRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "AltCover.Filter/I/Pipe #2 stage #2 at line 175@175::Invoke(Mono.Cecil.CustomAttribute)",
                            Justification = "Inlined from library code")>]
[<assembly: SuppressMessage("Gendarme.Rules.Globalization",
                            "PreferStringComparisonOverrideRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "AltCover.Filter/I/Pipe #1 stage #2 at line 292@292::Invoke(Mono.Cecil.Cil.Instruction)",
                            Justification =
                              "System.String System.String::Replace(System.String,System.String,System.StringComparison) Not available at netstandard2.0")>]
[<assembly: SuppressMessage("Gendarme.Rules.Globalization",
                            "PreferStringComparisonOverrideRule",
                            Scope = "member", // MethodDefinition
                            Target =
                              "AltCover.Filter/I/Pipe #2 stage #2 at line 303@303::Invoke(Mono.Cecil.Cil.Instruction)",
                            Justification =
                              "System.String System.String::Replace(System.String,System.String,System.StringComparison) Not available at netstandard2.0")>]
()