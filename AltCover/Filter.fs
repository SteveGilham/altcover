namespace AltCover

open System
open System.IO

open Mono.Cecil

type internal FilterClass =
  | File of string
  | Assembly of string
  | Type of string
  | Method of string
  | Attribute of string

module Filter =

  let internal Match (nameProvider:Object) (filter:FilterClass) =
    match nameProvider with
    | :? string as fileName ->
         match filter with
         | File name -> Path.GetFileName(fileName).Contains(name)
         | _ -> false
    | :? AssemblyDefinition as assembly ->
         match filter with
         | Assembly name -> assembly.Name.Name.Contains(name)
         | _ -> false
    | :? TypeDefinition as typeDef ->
         match filter with
         | Type name -> typeDef.FullName.Contains(name)
         | _ -> false
    | :? MethodDefinition as methodDef ->
         match filter with
         | Method name -> methodDef.Name.Contains(name)
         | _ -> false
    | :? ICustomAttributeProvider as attributeProvider ->
         match filter with
         | Attribute name -> attributeProvider.HasCustomAttributes &&
                             attributeProvider.CustomAttributes
                             |> Seq.cast<CustomAttribute>
                             |> Seq.exists (fun attr ->
                                attr.Constructor.DeclaringType.FullName.Contains(name))
         | _ -> false
    | _ -> false