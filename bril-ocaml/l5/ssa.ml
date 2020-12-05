open! Core

module Map = struct
  include Map

  let replace_exn t key ~f = set t ~key ~data:(find_exn t key |> f)
end

module Dest = struct
  include Bril.Dest
  include Comparable.Make_plain (Bril.Dest)
end

let validate ({ blocks; _ } as func : Bril.Func.t) =
  {
    func with
    blocks =
      Map.mapi blocks ~f:(fun ~key:block ~data:instrs ->
          match instrs with
          | Label _ :: _ -> instrs
          | _ -> Label ("__" ^ block) :: instrs);
  }

let insert ({ blocks; _ } as func : Bril.Func.t) =
  let def_map =
    Map.fold blocks ~init:Dest.Map.empty ~f:(fun ~key:block ~data:instrs def_map ->
        List.fold instrs ~init:def_map ~f:(fun def_map instr ->
            match Bril.Instr.dest instr with
            | None -> def_map
            | Some dest -> Map.add_multi def_map ~key:dest ~data:block))
  in
  let frontier = Dominance.Lists.frontier func in
  let rec insert blocks def_map phi_map =
    let (new_blocks, new_def_map, new_phi_map, changed) =
      Map.fold def_map ~init:(blocks, def_map, phi_map, false) ~f:(fun ~key:dest ~data:defs acc ->
          List.fold defs ~init:acc ~f:(fun acc def_block ->
              Map.find_exn frontier def_block
              |> List.fold ~init:acc ~f:(fun (blocks, def_map, phi_map, changed) frontier_block ->
                     if
                       Map.find phi_map frontier_block
                       |> Option.value_map ~default:false ~f:(Fn.flip Set.mem dest)
                     then (blocks, def_map, phi_map, changed)
                     else
                       let phi = Bril.Instr.Phi (dest, []) in
                       ( Map.replace_exn blocks frontier_block ~f:(function
                             | Bril.Instr.Label label :: instrs -> Label label :: phi :: instrs
                             | instrs -> phi :: instrs),
                         Map.add_multi def_map ~key:dest ~data:frontier_block,
                         Map.update phi_map frontier_block ~f:(function
                             | None -> Dest.Set.singleton dest
                             | Some phis -> Set.add phis dest),
                         true ))))
    in
    if changed then insert new_blocks new_def_map new_phi_map else new_blocks
  in
  let blocks = insert blocks def_map String.Map.empty in
  { func with blocks }

let rename ({ args; blocks; succs; order; _ } as func : Bril.Func.t) =
  let rec rename name_map (phis, counts, blocks) block =
    let instrs = Map.find_exn blocks block in
    let (name_map, phis, counts, instrs) =
      List.fold
        instrs
        ~init:(name_map, phis, counts, [])
        ~f:(fun (name_map, phis, counts, instrs) instr ->
          let new_args =
            Bril.Instr.args instr
            |> List.map ~f:(fun arg -> Map.find_exn name_map arg |> List.hd_exn)
          in
          let make_new_name name =
            sprintf "%s.%d" name (Map.find counts name |> Option.value ~default:0)
          in
          let (new_dest, name_map, new_phis, counts) =
            Bril.Instr.dest instr
            |> Option.value_map ~default:(None, name_map, phis, counts) ~f:(fun (name, bril_type) ->
                   let new_name = make_new_name name in
                   ( Some (new_name, bril_type),
                     Map.add_multi name_map ~key:name ~data:new_name,
                     ( match instr with
                     | Phi _ -> Map.set phis ~key:new_name ~data:name
                     | _ -> phis ),
                     Map.update counts name ~f:(Option.value_map ~default:1 ~f:(Int.( + ) 1)) ))
          in
          let new_instr = instr |> Bril.Instr.set_args new_args |> Bril.Instr.set_dest new_dest in
          (name_map, new_phis, counts, new_instr :: instrs))
    in
    let instrs = List.rev instrs in
    let blocks = Map.set blocks ~key:block ~data:instrs in
    (* print_s
     *   [%message
     *     block
     *       (name_map : string list String.Map.t)
     *       (phis : string String.Map.t)
     *       (counts : int String.Map.t)
     *       (blocks : Bril.Instr.t list String.Map.t)]; *)
    let label () =
      match instrs with
      | Label label :: _ -> label
      | _ -> failwith "found block with no label"
    in
    let blocks =
      Map.find_exn succs block
      |> List.fold ~init:blocks ~f:(fun blocks succ ->
             Map.replace_exn blocks succ ~f:(fun instrs ->
                 List.map instrs ~f:(function
                     | Phi (((name, _) as dest), params) as instr ->
                       let name = Map.find phis name |> Option.value ~default:name in
                       ( match Map.find name_map name with
                       | Some (name :: _) -> Bril.Instr.Phi (dest, (label (), name) :: params)
                       | _ -> instr )
                     | instr -> instr)))
    in
    Dominance.Lists.tree func
    |> snd
    |> Fn.flip Map.find_exn block
    |> List.fold ~init:(phis, counts, blocks) ~f:(rename name_map)
  in
  let name_map = List.map args ~f:(fun (name, _) -> (name, [ name ])) |> String.Map.of_alist_exn in
  let (_, _, blocks) =
    rename name_map (String.Map.empty, String.Map.empty, blocks) (List.hd_exn order)
  in
  { func with blocks }

let to_ssa func = func |> validate |> insert |> rename

let from_ssa ({ blocks; _ } as func : Bril.Func.t) =
  let (blocks, to_add) =
    Map.fold
      blocks
      ~init:(blocks, String.Map.empty)
      ~f:(fun ~key:block ~data:instrs (blocks, to_add) ->
        let (instrs, to_add) =
          List.fold instrs ~init:([], to_add) ~f:(fun (instrs, to_add) -> function
            | Phi (dest, params) ->
              ( instrs,
                List.fold params ~init:to_add ~f:(fun to_add (label, arg) ->
                    Map.add_multi
                      to_add
                      ~key:label
                      ~data:(Bril.Instr.Unary (dest, Bril.Op.Unary.Id, arg))) )
            | instr -> (instr :: instrs, to_add))
        in
        (Map.set blocks ~key:block ~data:(List.rev instrs), to_add))
  in
  let blocks =
    Map.merge blocks to_add ~f:(fun ~key:_ -> function
      | `Left instrs
      | `Right instrs ->
        Some instrs
      | `Both (instrs, instrs_to_add) ->
        Some
          ( match List.rev instrs with
          | ((Jmp _ | Br _ | Ret _) as instr) :: instrs ->
            List.rev instrs @ instrs_to_add @ [ instr ]
          | _ -> instrs @ instrs_to_add ))
  in
  { func with blocks }
