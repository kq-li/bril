open! Core

type dir =
  | Forward
  | Backward

module type M_intf = sig
  type t [@@deriving compare, sexp_of]

  val analysis : string
  val dir : dir
  val top : t
  val meet : t -> t -> t
  val init : Bril.Func.t -> t
  val transfer : t -> Bril.Instr.t list -> t
  val to_string : t -> string

  include Comparable.S_plain with type t := t
end

module type S = sig
  type t

  val analyze : Bril.Func.t -> t String.Map.t * t String.Map.t
  val print : Bril.Func.t -> unit
end

module Make (M : M_intf) : S with type t := M.t = struct
  include M

  let analyze ({ blocks; order; preds; succs; _ } as func : Bril.Func.t) =
    let (preds, succs) =
      match dir with
      | Forward -> (preds, succs)
      | Backward -> (succs, preds)
    in
    let rec aux worklist ins outs =
      match Fqueue.dequeue worklist with
      | None ->
        ( match dir with
        | Forward -> (ins, outs)
        | Backward -> (outs, ins) )
      | Some (b, worklist) ->
        let in_b =
          String.Map.find_exn preds b
          |> List.fold ~init:(String.Map.find_exn ins b) ~f:(fun in_b pred ->
                 M.meet in_b (String.Map.find_exn outs pred))
        in
        let out_b = transfer in_b (String.Map.find_exn blocks b) in
        let worklist =
          if equal out_b (String.Map.find_exn outs b) then worklist
          else String.Map.find_exn succs b |> List.fold ~init:worklist ~f:Fqueue.enqueue
        in
        aux worklist (String.Map.set ins ~key:b ~data:in_b) (String.Map.set outs ~key:b ~data:out_b)
    in
    let empty = List.map order ~f:(fun b -> (b, M.top)) |> String.Map.of_alist_exn in
    let start = List.hd_exn order in
    let (ins, outs) =
      match dir with
      | Forward -> (String.Map.set empty ~key:start ~data:(init func), empty)
      | Backward -> (empty, String.Map.set empty ~key:start ~data:(init func))
    in
    aux (Fqueue.of_list order) ins outs

  let print func =
    let (ins, outs) = analyze func in
    let string_of_map map =
      List.map func.order ~f:(fun name ->
          sprintf "\t%s -> %s" name (String.Map.find_exn map name |> M.to_string))
      |> String.concat ~sep:"\n"
    in
    printf
      "analyzing %s...\nins:\n%s\nouts:\n%s\n"
      analysis
      (string_of_map ins)
      (string_of_map outs)
end
