namespace AltCover

open System
open System.Text

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
        let builder = StringBuilder()
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

        builder.Append(FullTypeRefName def.ReturnType)
               .Append(" ")
               .Append(FullTypeName def.DeclaringType)
               .Append(".")
               .Append(if def.IsConstructor && (not def.IsStatic) then "#ctor" else MethodName def)
               .Append(if generic then "<" else String.Empty)
               .Append(tparameters)
               .Append(if generic then ">" else String.Empty)
               .Append("(")
               .Append(parameters)
               .Append(")")
               .ToString()