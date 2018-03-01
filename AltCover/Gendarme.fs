namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Xml.Linq

open Mono.Cecil
open Mono.Cecil.Cil

module Gendarme =

  // OpenCover uses Gendarme to compute Cyclomatic Complexity values.  Reimplement that algorithm here
  let mask = [  0xFFFF6C3FCUL
                0x1B0300000000FFE0UL
                0x400100FFF800UL
                0xDE0UL ]

  let FindFirstUnconditionalBranchTarget (ins:Cil.Instruction) =
     Seq.unfold (fun (state:Cil.Instruction) -> if isNull state then None else Some (state, state.Next)) ins
     |> Seq.tryFind (fun i -> i.OpCode.FlowControl = FlowControl.Branch)
     |> Option.map (fun i -> i.Operand :?> Cil.Instruction)

  let AccumulateSwitchTargets (ins:Cil.Instruction) (targets:System.Collections.Generic.HashSet<Cil.Instruction>) =
    let cases = ins.Operand :?> Cil.Instruction[]
    cases |> Seq.iter(fun target -> if target <> ins.Next then target |> targets.Add |> ignore)

    // add 'default' branch (if one exists)
    let next = ins.Next
    if next.OpCode.FlowControl = FlowControl.Branch then
      let operand = next.Operand :?> Cil.Instruction
      match cases |> Seq.head |> FindFirstUnconditionalBranchTarget with
      | Some unc when unc = operand -> ()
      | _ -> operand |> targets.Add |> ignore

  let ``detect ternary pattern`` code =
    let index = int code
    if mask |> Seq.skip (index >>> 6) |> Seq.head &&& (1UL <<< (index &&& 63)) = 0UL
    then 0
    else 1

  let SwitchCyclomaticComplexity (instructions:Cil.Instruction seq) =
    let targets = System.Collections.Generic.HashSet<Cil.Instruction>()
    let fast = instructions
               |> Seq.fold (fun c i ->
                                match i.OpCode.FlowControl with
                                | FlowControl.Branch ->
                                    let previous = i.Previous
                                    c + if previous |> isNull |> not then
                                          do if previous.OpCode.FlowControl = FlowControl.Cond_Branch then
                                              match previous.Operand with
                                              | :? (Cil.Instruction array) -> ()
                                              | :? Cil.Instruction as branch ->
                                                 if targets.Contains branch
                                                 then i |> targets.Add |> ignore
                                              | _ -> ()
                                          ``detect ternary pattern`` previous.OpCode.Code
                                        else 0
                                | FlowControl.Cond_Branch ->
                                    if i.OpCode = OpCodes.Switch then
                                      AccumulateSwitchTargets i targets
                                      c
                                    else
                                      let branch = i.Operand :?> Cil.Instruction
                                      let previous = branch.Previous
                                      c + if previous |> isNull |> not &&
                                             previous.Previous.OpCode.Code <> OpCodes.Switch.Code &&
                                             branch |> targets.Contains |> not
                                          then 1
                                          else 0
                                | _ -> c ) 1
    fast + targets.Count

  let CyclomaticComplexity (m:MethodDefinition) =
    if m.HasBody then
        let instructions = m.Body.Instructions
                           |> Seq.cast<Cil.Instruction>
        match instructions |> Seq.tryFind (fun i -> i.OpCode = OpCodes.Switch) with
        | None ->
           instructions
            |> Seq.fold (fun c i -> match i.OpCode.FlowControl with
                                    | FlowControl.Cond_Branch ->
                                      c + 1
                                    | FlowControl.Branch ->
                                      let previous = i.Previous
                                      c + if previous |> isNull |> not then
                                             ``detect ternary pattern`` previous.OpCode.Code
                                          else 0
                                    | _ -> c ) 1
        | _ -> SwitchCyclomaticComplexity instructions
    else 1