open! Core

let nat_loops (func : Bril.Func.t) =
  let nat_loop first last =
    let rec aux loop cur =
      if Set.mem loop cur then loop
      else Map.find_exn func.preds cur |> List.fold ~init:(Set.add loop cur) ~f:aux
    in
    aux (String.Set.singleton first) last
  in
  Bril.Func.Dominance.Sets.back_edges func
  |> Map.fold ~init:String.Map.empty ~f:(fun ~key:last ~data:firsts loops ->
         Set.fold firsts ~init:loops ~f:(fun loops first ->
             let loop = nat_loop first last in
             Map.update loops first ~f:(function
                 | None -> loop
                 | Some loop2 -> Set.union loop loop2)))

module Annot_instr = struct
  module T = struct
    type t = string * int * Bril.Instr.t [@@deriving compare, sexp_of]
  end

  include T
  include Comparable.Make_plain (T)
end

let licm func =
  let loops = nat_loops func in
  let dominated = Bril.Func.Dominance.Sets.dominated func in
  let defs_by_blocks =
    Reaching_definitions.analyze func
    |> fst
    |> Map.map
         ~f:
           (Map.map
              ~f:
                (Set.fold ~init:Annot_instr.Set.empty ~f:(fun defs -> function
                   | Reaching_definitions.Def.Arg _ -> defs
                   | Instr (b, i, instr) -> Set.add defs (b, i, instr))))
  in
  let rec aux inv loop =
    let new_inv =
      Set.fold loop ~init:inv ~f:(fun inv b ->
          let defs_by_var = Map.find_exn defs_by_blocks b in
          Map.find_exn func.blocks b
          |> List.foldi ~init:(inv, defs_by_var) ~f:(fun i (inv, defs_by_var) instr ->
                 let arg_inv var =
                   match Map.find_exn defs_by_var var |> Set.to_list with
                   | [ def ] when Set.mem inv def -> true
                   | defs -> List.for_all defs ~f:(Set.mem inv)
                 in
                 let new_inv = Set.add inv (b, i, instr) in
                 let inv =
                   match instr with
                   | Const _ -> new_inv
                   | Binary (_, _, arg1, arg2) when arg_inv arg1 && arg_inv arg2 -> new_inv
                   | Unary (_, _, arg) when arg_inv arg -> new_inv
                   | _ -> inv
                 in
                 let defs_by_var =
                   Bril.Instr.dest instr
                   |> Option.value_map ~default:defs_by_var ~f:(fun (var, _) ->
                          Map.update defs_by_var var ~f:(function
                              | None -> Annot_instr.Set.singleton (b, i, instr)
                              | Some defs -> Set.add defs (b, i, instr)))
                 in
                 (inv, defs_by_var))
          |> fst)
    in
    if Annot_instr.Set.equal inv new_inv then inv else aux new_inv loop
  in
  let invs_by_loop = Map.map loops ~f:(aux Annot_instr.Set.empty) in
  let (_, live_out_by_block) = Live_variables.analyze func in
  let to_move_by_block_by_header =
    Map.mapi invs_by_loop ~f:(fun ~key:header ~data:inv ->
        let (defs_by_var, uses_by_var) =
          Map.find_exn loops header
          |> Set.fold ~init:(String.Map.empty, String.Map.empty) ~f:(fun acc b ->
                 Map.find_exn func.blocks b
                 |> List.foldi ~init:acc ~f:(fun i (defs_by_var, uses_by_var) instr ->
                        let update =
                          Map.update ~f:(function
                              | None -> Annot_instr.Set.singleton (b, i, instr)
                              | Some annot_instrs -> Set.add annot_instrs (b, i, instr))
                        in
                        ( Bril.Instr.dest instr
                          |> Option.map ~f:fst
                          |> Option.fold ~init:defs_by_var ~f:update,
                          Bril.Instr.args instr |> List.fold ~init:uses_by_var ~f:update )))
        in
        let loop = Map.find_exn loops header in
        let exits =
          Set.filter loop ~f:(fun b ->
              Map.find_exn func.succs b |> List.exists ~f:(Fn.non (Set.mem loop)))
        in
        (* print_s
         *   [%message
         *     (defs_by_var : Annot_instr.Set.t String.Map.t)
         *       (uses_by_var : Annot_instr.Set.t String.Map.t)
         *       (exits : String.Set.t)
         *       (live_out_by_block : String.Set.t String.Map.t)]; *)
        Set.filter inv ~f:(fun (b, i, instr) ->
            let domed = Map.find_exn dominated b in
            match Bril.Instr.dest instr with
            | None -> false
            | Some (var, _) ->
              Map.find_exn defs_by_var var |> Set.length = 1
              && Map.find uses_by_var var
                 |> Option.value ~default:Annot_instr.Set.empty
                 |> Set.for_all ~f:(fun (use_b, use_i, _) ->
                        Set.mem domed use_b || (String.equal use_b b && i < use_i))
              && Set.for_all exits ~f:(fun exit_b ->
                     Set.mem domed exit_b
                     || Map.find_exn live_out_by_block exit_b |> Fn.non (Fn.flip Set.mem var)))
        |> Set.fold ~init:String.Map.empty ~f:(fun to_move_by_block (b, i, instr) ->
               Map.update to_move_by_block b ~f:(function
                   | None -> Annot_instr.Set.singleton (b, i, instr)
                   | Some annot_instrs -> Set.add annot_instrs (b, i, instr))))
  in
  Map.fold
    to_move_by_block_by_header
    ~init:func
    ~f:(fun ~key:header ~data:to_move_by_block ({ blocks; order; succs; preds; _ } as func) ->
      let (instrs, blocks) =
        Map.fold
          to_move_by_block
          ~init:([], blocks)
          ~f:(fun ~key:block ~data:to_move (instrs, blocks) ->
            ( instrs @ (Set.to_list to_move |> List.map ~f:Tuple.T3.get3),
              Map.update blocks block ~f:(function
                  | None -> []
                  | Some instrs ->
                    List.filteri instrs ~f:(fun i instr -> not (Set.mem to_move (block, i, instr))))
            ))
      in
      let preheader = "pre_" ^ header in
      let blocks = Map.set blocks ~key:preheader ~data:(Bril.Instr.Label preheader :: instrs) in
      let order =
        match instrs with
        | [] -> order
        | _ ->
          List.fold_right order ~init:[] ~f:(fun block order ->
              if String.equal block header then preheader :: header :: order else block :: order)
      in
      let succs =
        Map.set succs ~key:preheader ~data:[ header ]
        |> Map.map
             ~f:(List.map ~f:(fun block -> if String.equal header block then preheader else block))
      in
      let preds =
        Map.set preds ~key:preheader ~data:(Map.find_exn preds header)
        |> Map.set ~key:header ~data:[ preheader ]
      in
      { func with blocks; order; succs; preds })

let () =
  In_channel.input_all In_channel.stdin
  |> Yojson.Basic.from_string
  |> Bril.from_json
  |> List.map ~f:licm
  |> Bril.to_json
  |> Yojson.Basic.pretty_to_string
  |> print_endline
