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
    let internal typeName(def : TypeDefinition) = emptyIfIsNullOrWhiteSpace def.Name
    [<System.Diagnostics.CodeAnalysis.SuppressMessage(
      "Gendarme.Rules.Maintainability", "AvoidUnnecessarySpecializationRule",
      Justification = "AvoidSpeculativeGenerality too")>]
    let internal typeRefName(def : TypeReference) = emptyIfIsNullOrWhiteSpace def.Name

    let rec internal fullTypeRefName(def : TypeReference) =
      let deft = def.DeclaringType
      if deft.IsNotNull
      then (fullTypeRefName deft) + "+" + (typeRefName def)
      else (suffixIfNotIsNullOrWhiteSpace def.Namespace ".") + typeRefName def

    let internal methodName(def : MethodDefinition) =
      if def.IsConstructor && (not def.IsStatic) then
        "#ctor"
      else
        emptyIfIsNullOrWhiteSpace def.Name

  // "Public" interface
  let rec internal fullTypeName(def : TypeDefinition) =
    let deft = def.DeclaringType
    if deft.IsNotNull
    then (fullTypeName deft) + "+" + (I.typeName def)
    else (I.suffixIfNotIsNullOrWhiteSpace def.Namespace ".") + I.typeName def

  let internal fullMethodName(def : MethodDefinition) =
    let parameters =
      String.Join
        (",",
          def.Parameters
          |> Seq.filter (fun x -> x.IsNotNull)
          |> Seq.map (fun p -> p.ParameterType)
          |> Seq.filter (fun x -> x.IsNotNull)
          |> Seq.map I.fullTypeRefName)

    let generic = def.HasGenericParameters

    let tparameters =
      if generic then
        String.Join
          (",",
            def.GenericParameters
            |> Seq.filter (fun x -> x.IsNotNull)
            |> Seq.map I.fullTypeRefName)
      else
        String.Empty

    let return' = I.fullTypeRefName def.ReturnType
    String.Join
      (String.Empty,
        [ return'
          " "
          fullTypeName def.DeclaringType
          "."
          I.methodName def
          (if generic then "<" else String.Empty)
          tparameters
          (if generic then ">" else String.Empty)
          "("
          parameters
          ")" ])