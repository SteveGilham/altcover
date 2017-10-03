namespace AltCover

open System

open Mono.Cecil

module Naming =

    let inline isNotNull x = not (isNull x)

    let TypeName (def : TypeDefinition) =
        let name = def.Name
        if String.IsNullOrWhiteSpace name then String.Empty else name

    let TypeRefName (def : TypeReference) =
        let name = def.Name
        if String.IsNullOrWhiteSpace name then String.Empty else name

    let rec FullTypeName (def : TypeDefinition) =
        let deft = def.DeclaringType
        if isNotNull deft then (FullTypeName deft) + "+" + (TypeName def)
        else let ns = def.Namespace
             if String.IsNullOrWhiteSpace ns then TypeName def else ns + "." + TypeName def

    let rec FullTypeRefName (def : TypeReference) =
        let deft = def.DeclaringType
        if isNotNull deft then (FullTypeRefName deft) + "+" + (TypeRefName def)
        else let ns = def.Namespace
             if String.IsNullOrWhiteSpace ns then TypeRefName def else ns + "." + TypeRefName def

    let MethodName (def : MethodDefinition) =
        let name = def.Name
        if String.IsNullOrWhiteSpace name then String.Empty else name

    let FullMethodName (def : MethodDefinition) =
        let parameters = String.Join(",", def.Parameters
                                        |> Seq.filter isNotNull
                                        |> Seq.map (fun p -> p.ParameterType)
                                        |> Seq.filter isNotNull
                                        |> Seq.map FullTypeRefName)
        let generic = def.HasGenericParameters
        let tparameters = if generic then
                                String.Join(",", def.GenericParameters
                                        |> Seq.filter isNotNull
                                        |> Seq.map FullTypeRefName)
                          else String.Empty

        let return' = FullTypeRefName def.ReturnType

        String.Join(String.Empty, [return'
                                   " "
                                   FullTypeName def.DeclaringType
                                   "."
                                   (if def.IsConstructor && (not def.IsStatic) then "#ctor" else MethodName def)
                                   (if generic then "<" else String.Empty)
                                   tparameters
                                   (if generic then ">" else String.Empty)
                                   "("
                                   parameters
                                   ")"])