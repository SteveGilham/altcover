namespace AltCover

open Mono.Cecil
open Mono.Cecil.Cil

open AltCover.Augment

module internal Gendarme =

  // OpenCover uses Gendarme to compute Cyclomatic Complexity values.  Reimplement that algorithm here
  let mask = [  0xFFFF6C3FCUL
                0x1B0300000000FFE0UL
                0x400100FFF800UL
                0xDE0UL ]

  let FindFirstUnconditionalBranchTarget(ins : Cil.Instruction) =
    Seq.unfold (fun (state : Cil.Instruction) ->
      if isNull state then None
      else Some(state, state.Next)) ins
    |> Seq.tryFind (fun i -> i.OpCode.FlowControl = FlowControl.Branch)
    |> Option.map (fun i -> i.Operand :?> Cil.Instruction)

  let AccumulateSwitchTargets (ins : Cil.Instruction)
      (targets : System.Collections.Generic.HashSet<Cil.Instruction>) =
    let cases = ins.Operand :?> Cil.Instruction []
    cases
    |> Seq.iter (fun target ->
         if target <> ins.Next then
           target
           |> targets.Add
           |> ignore)
    // add 'default' branch (if one exists)
    let next = ins.Next
    if next.OpCode.FlowControl = FlowControl.Branch then
      let operand = next.Operand :?> Cil.Instruction
      match cases
            |> Seq.head
            |> FindFirstUnconditionalBranchTarget with
      | Some unc when unc = operand -> ()
      | _ ->
        operand
        |> targets.Add
        |> ignore

  let ``detect ternary pattern`` (code : Code option) =
    let index = int (Option.getOrElse Code.Nop code)
    mask
    |> Seq.skip (index >>> 6)
    |> Seq.head
    &&& (1UL <<< (index &&& 63))
    <> 0UL
    |> Augment.Increment

  let SwitchCyclomaticComplexity(instructions : Cil.Instruction seq) =
    let targets = System.Collections.Generic.HashSet<Cil.Instruction>()

    let fast =
      instructions
      |> Seq.fold (fun c i ->
           match i.OpCode.FlowControl with
           | FlowControl.Branch ->
             c + (Option.nullable i.Previous
                  |> Option.map (fun (previous : Instruction) ->
                       do if previous.OpCode.FlowControl = FlowControl.Cond_Branch then
                            match previous.Operand with
                            | :? Cil.Instruction as branch ->
                              if targets.Contains branch then
                                i
                                |> targets.Add
                                |> ignore
                            | _ -> ()
                       previous.OpCode.Code)
                  |> ``detect ternary pattern``)
           | FlowControl.Cond_Branch ->
             if i.OpCode = OpCodes.Switch then
               AccumulateSwitchTargets i targets
               c
             else
               let branch = i.Operand :?> Cil.Instruction
               c + (Option.nullable branch.Previous
                    |> Option.filter (fun (previous : Instruction) ->
                         previous.Previous.OpCode.Code <> OpCodes.Switch.Code && branch
                                                                                 |> targets.Contains
                                                                                 |> not)
                    |> Option.map (fun _ -> 1)
                    |> Option.getOrElse 0)
           | _ -> c) 1
    fast + targets.Count

  let CyclomaticComplexity(m : MethodDefinition) =
    if m.HasBody then
      let instructions = m.Body.Instructions |> Seq.cast<Cil.Instruction>
      match instructions |> Seq.tryFind (fun i -> i.OpCode = OpCodes.Switch) with
      | None ->
        instructions
        |> Seq.fold (fun c i ->
             match i.OpCode.FlowControl with
             | FlowControl.Cond_Branch -> c + 1
             | FlowControl.Branch ->
               c + (Option.nullable i.Previous
                    |> Option.map (fun (previous : Instruction) -> previous.OpCode.Code)
                    |> ``detect ternary pattern``)
             | _ -> c) 1
      | _ -> SwitchCyclomaticComplexity instructions
    else 1