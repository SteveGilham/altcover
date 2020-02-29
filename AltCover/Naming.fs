namespace AltCover

open System

open Augment
open Mono.Cecil

[<RequireQualifiedAccess>]
module internal Naming =
  module internal I =
    let internal emptyIfIsNullOrWhiteSpace name =
      if String.IsNullOrWhiteSpace name then String.Empty else name

    let internal suffixIfNotIsNullOrWhiteSpace name suffix =
      if String.IsNullOrWhiteSpace name then String.Empty else (name + suffix)

    [<System.Diagnostics.CodeAnalysis.SuppressMessage(
      "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
      Justification = "AvoidSpeculativeGenerality too")>]
    let internal TypeName(def : TypeDefinition) = emptyIfIsNullOrWhiteSpace def.Name
    [<System.Diagnostics.CodeAnalysis.SuppressMessage(
      "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
      Justification = "AvoidSpeculativeGenerality too")>]
    let internal TypeRefName(def : TypeReference) = emptyIfIsNullOrWhiteSpace def.Name

    let rec internal FullTypeRefName(def : TypeReference) =
      let deft = def.DeclaringType
      if deft.IsNotNull
      then (FullTypeRefName deft) + "+" + (TypeRefName def)
      else (suffixIfNotIsNullOrWhiteSpace def.Namespace ".") + TypeRefName def

    let internal MethodName(def : MethodDefinition) =
      if def.IsConstructor && (not def.IsStatic) then
        "#ctor"
      else
        emptyIfIsNullOrWhiteSpace def.Name

  // "Public" interface
  let rec internal FullTypeName(def : TypeDefinition) =
    let deft = def.DeclaringType
    if deft.IsNotNull
    then (FullTypeName deft) + "+" + (I.TypeName def)
    else (I.suffixIfNotIsNullOrWhiteSpace def.Namespace ".") + I.TypeName def

  let internal FullMethodName(def : MethodDefinition) =
    let parameters =
      String.Join
        (",",
          def.Parameters
          |> Seq.filter (fun x -> x.IsNotNull)
          |> Seq.map (fun p -> p.ParameterType)
          |> Seq.filter (fun x -> x.IsNotNull)
          |> Seq.map I.FullTypeRefName)

    let generic = def.HasGenericParameters

    let tparameters =
      if generic then
        String.Join
          (",",
            def.GenericParameters
            |> Seq.filter (fun x -> x.IsNotNull)
            |> Seq.map I.FullTypeRefName)
      else
        String.Empty

    let return' = I.FullTypeRefName def.ReturnType
    String.Join
      (String.Empty,
        [ return'
          " "
          FullTypeName def.DeclaringType
          "."
          I.MethodName def
          (if generic then "<" else String.Empty)
          tparameters
          (if generic then ">" else String.Empty)
          "("
          parameters
          ")" ])