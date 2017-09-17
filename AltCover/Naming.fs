namespace AltCover

open System
open System.Text

open Mono.Cecil

module Naming =

    let inline isNotNull x = not (isNull x)

    let TypeName (def : TypeDefinition) =
        if String.IsNullOrWhiteSpace def.Name then String.Empty else def.Name

    let TypeRefName (def : TypeReference) =
        if String.IsNullOrWhiteSpace def.Name then String.Empty else def.Name

    let rec FullTypeName (def : TypeDefinition) =
        if isNotNull def.DeclaringType then (FullTypeName def.DeclaringType) + "+" + (TypeName def)
        else if String.IsNullOrWhiteSpace def.Namespace then TypeName def else def.Namespace + "." + TypeName def

    let rec FullTypeRefName (def : TypeReference) =
        if isNotNull def.DeclaringType then (FullTypeRefName def.DeclaringType) + "+" + (TypeRefName def)
        else if String.IsNullOrWhiteSpace def.Namespace then TypeRefName def else def.Namespace + "." + TypeRefName def

    let MethodName (def : MethodDefinition) =
        if String.IsNullOrWhiteSpace def.Name then String.Empty else def.Name

    let FullMethodName (def : MethodDefinition) =
        let builder = StringBuilder()
        let parameters = String.Join(",", def.Parameters
                                        |> Seq.filter isNotNull
                                        |> Seq.map (fun p -> p.ParameterType)
                                        |> Seq.filter isNotNull
                                        |> Seq.map FullTypeRefName)

        let tparameters = if def.HasGenericParameters then
                                String.Join(",", def.GenericParameters
                                        |> Seq.filter isNotNull
                                        |> Seq.map FullTypeRefName)
                          else String.Empty

        builder.Append(FullTypeName def.DeclaringType)
               .Append(".")
               .Append(if def.IsConstructor then "#ctor" else MethodName def)
               .Append(if def.HasGenericParameters then "<" else String.Empty)
               .Append(tparameters)
               .Append(if def.HasGenericParameters then ">" else String.Empty)
               .Append("(")
               .Append(parameters)
               .Append(")")
               .ToString()