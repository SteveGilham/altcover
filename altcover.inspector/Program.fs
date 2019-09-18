// Learn more about F# at http://fsharp.org

open System
open System.IO

open Mono.Cecil
open Mono.Cecil.Cil
open Mono.Cecil.Rocks

let getFileExists f =
  try
    let full = Path.GetFullPath f
    if File.Exists full
    then Some full
    else
      eprintfn "File %s does not exist" full
      None
  with
  | x -> eprintfn "Treating %s as a file caused %A" f x
         None

let loadInCecil (path:string) =
  try
    path |> AssemblyDefinition.ReadAssembly |> Some
  with
  | x -> eprintfn "Treating %s as an assembly caused %A" path x
         None

let checkCalls (m:MethodDefinition) =
  let il = m.Body.Instructions
  il
  |> Seq.filter (fun i -> i.OpCode = OpCodes.Call)
  |> Seq.filter (fun i -> let op = i.Operand :?> MethodReference
                          op.Name = "Visit" && op.DeclaringType.FullName = "AltCover.Recorder.Instance")
  |> Seq.iter (fun v -> let p = v.Previous
                        if p.OpCode.ToString().StartsWith("ldc.i4", StringComparison.OrdinalIgnoreCase) |> not
                        then eprintfn "Suspicious call in %s - visit number = %A" m.FullName p

                        let p2 = p.Previous
                        if p2.OpCode.ToString() <> "ldstr"
                        then eprintfn "Suspicious call in %s - module id = %A" m.FullName p2

                        if p2.Operand |> string |> String.IsNullOrWhiteSpace
                        then eprintfn "Suspicious call in %s - module id = %A" m.FullName p2

                        il
                        |> Seq.iter (fun o -> match o.OpCode.OperandType with
                                              | OperandType.InlineBrTarget
                                              | OperandType.ShortInlineBrTarget ->
                                                  let target = o.Operand :?> Instruction
                                                  if target.Offset = v.Offset
                                                  then eprintfn "Suspicious jump in %s from %A to %A" m.FullName o v
                                                  if target.Offset = p.Offset
                                                  then eprintfn "Suspicious jump in %s from %A to %A" m.FullName o p
                                              | OperandType.InlineSwitch ->
                                                let targets = o.Operand :?> Instruction[]
                                                targets
                                                |> Seq.iter (fun target -> if target.Offset = v.Offset
                                                                           then eprintfn "Suspicious jump in %s from %A to %A" m.FullName o v
                                                                           if target.Offset = p.Offset
                                                                           then eprintfn "Suspicious jump in %s from %A to %A" m.FullName o p)
                                              | _ -> ()
                        )
   )

let inspect (def:AssemblyDefinition)  =
  def.MainModule.GetAllTypes()
  |> Seq.collect (fun t -> t.Methods)
  |> Seq.filter (fun m -> m.HasBody)
  |> Seq.iter checkCalls
  printfn "Scan completed for %s" def.FullName

[<EntryPoint>]
let main argv =
    argv
    |> Seq.map getFileExists
    |> Seq.choose id
    |> Seq.map loadInCecil
    |> Seq.choose id
    |> Seq.iter inspect

    0 // return an integer exit code