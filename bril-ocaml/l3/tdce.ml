open! Core

module Global = struct
  let process instrs =
    List.fold_right instrs ~init:([], String.Set.empty) ~f:(fun instr (instrs, used) ->
        let args = instr |> Bril.Instr.args |> String.Set.of_list in
        match Bril.Instr.dest instr with
        | Some (name, _) when not (Set.mem used name) -> (instrs, used)
        | _ -> (instr :: instrs, Set.union used args))
    |> fst
end

module Local = struct
  let process instrs =
    List.fold_right instrs ~init:([], String.Set.empty) ~f:(fun instr (instrs, reassigned) ->
        let args = instr |> Bril.Instr.args |> String.Set.of_list in
        match Bril.Instr.dest instr with
        | Some (name, _) ->
          if Set.mem reassigned name then (instrs, reassigned)
          else (instr :: instrs, Set.diff (Set.add reassigned name) args)
        | None -> (instr :: instrs, Set.diff reassigned args))
    |> fst
end
