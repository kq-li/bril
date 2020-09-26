open! Core

type dir =
  | Forward
  | Backward

module Dataflow = struct
  module type M_intf = sig
    type t [@@deriving compare, sexp_of]

    val dir : dir
    val top : t
    val meet : t -> t -> t
    val init : Bril.Func.t -> t
    val transfer : t -> Bril.Instr.t list -> t
  end

  module Make (M : M_intf) = struct
    module M = struct
      include M
      include Comparable.Make_plain (M)
    end

    let analyze ({ blocks; order; preds; succs; _ } : Bril.Func.t) =
      let (preds, succs) =
        match M.dir with
        | Forward -> (preds, succs)
        | Backward -> (succs, preds)
      in
      let rec aux worklist ins outs =
        match Fqueue.dequeue worklist with
        | None ->
          ( match M.dir with
          | Forward -> (ins, outs)
          | Backward -> (outs, ins) )
        | Some (b, worklist) ->
          let in_b =
            Map.find_exn preds b
            |> List.fold ~init:(Map.find_exn ins b) ~f:(fun in_b pred ->
                   M.meet in_b (Map.find_exn outs pred))
          in
          let out_b = M.transfer in_b (Map.find_exn blocks b) in
          let worklist =
            if M.equal out_b (Map.find_exn outs b) then worklist
            else Map.find_exn succs b |> List.fold ~init:worklist ~f:Fqueue.enqueue
          in
          aux worklist (Map.set ins ~key:b ~data:in_b) (Map.set outs ~key:b ~data:out_b)
      in
      let empty = List.map order ~f:(fun b -> (b, M.top)) |> String.Map.of_alist_exn in
      aux (Fqueue.of_list order) empty empty
  end
end

module Defined_vars = Dataflow.Make (struct
  type t = String.Set.t [@@deriving compare, sexp_of]

  let dir = Forward
  let top = String.Set.empty
  let meet = Set.union
  let init (func : Bril.Func.t) = List.map func.args ~f:fst |> String.Set.of_list

  let transfer in_b block =
    List.fold block ~init:in_b ~f:(fun defs instr ->
        match Bril.Instr.dest instr with
        | None -> defs
        | Some (name, _) -> Set.add defs name)
end)

module Live_vars = Dataflow.Make (struct
  type t = String.Set.t [@@deriving compare, sexp_of]

  let dir = Backward
  let top = String.Set.empty
  let meet = Set.union
  let init _ = top

  let transfer in_b block =
    List.fold_right block ~init:in_b ~f:(fun instr live ->
        let gen = Bril.Instr.args instr |> String.Set.of_list in
        let kill =
          match Bril.Instr.dest instr with
          | None -> String.Set.empty
          | Some (name, _) -> String.Set.singleton name
        in
        Set.union gen (Set.diff live kill))
end)

let () =
  let string_of_set = Fn.compose (String.concat ~sep:", ") Set.to_list in
  In_channel.input_all In_channel.stdin
  |> Yojson.Basic.from_string
  |> Bril.from_json
  |> List.iter ~f:(fun func ->
         let (ins, outs) = Live_vars.analyze func in
         List.iter func.order ~f:(fun b ->
             printf
               "%s\n  in: {%s}\n  out: {%s}\n"
               b
               (Map.find_exn ins b |> string_of_set)
               (Map.find_exn outs b |> string_of_set)))
