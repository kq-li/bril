open! Core

let process block =
  List.fold_right block ~init:([], String.Set.empty) ~f:(fun instr (block, used) ->
      let args = String.Set.of_list (Bril.Instr.args instr) in
      match Bril.Instr.dest instr with
      | Some (name, _) ->
        if Set.mem used name then (instr :: block, Set.remove used name |> Set.union args)
        else (block, used)
      | None -> (instr :: block, Set.union used args))
  |> fst
