open! Core

module Maybe_const = struct
  module T = struct
    type t =
      | Const of Bril.Const.t
      | Dynamic
    [@@deriving compare, equal, sexp_of]

    let merge t1 t2 =
      match (t1, t2) with
      | (Const c1, Const c2) when Bril.Const.equal c1 c2 -> Const c1
      | _ -> Dynamic

    let fold_binary binop t1 t2 =
      match (t1, t2) with
      | (Const c1, Const c2) -> Const (Bril.Op.Binary.fold binop c1 c2)
      | _ -> Dynamic

    let fold_unary unop t =
      match t with
      | Const c -> Const (Bril.Op.Unary.fold unop c)
      | _ -> Dynamic

    let to_string = function
      | Const const -> Bril.Const.to_string const
      | Dynamic -> "?"
  end

  include T
  include Comparable.Make_plain (T)
end

include Dataflow.Make (struct
  module M = struct
    type t = Maybe_const.t String.Map.t [@@deriving compare, sexp_of]

    let analysis = "constant propagation"
    let dir = Dataflow.Forward
    let top = String.Map.empty

    let meet =
      Map.merge ~f:(fun ~key:_ -> function
        | `Left v
        | `Right v ->
          Some v
        | `Both (v1, v2) -> Some (Maybe_const.merge v1 v2))

    let init _ = top

    let transfer in_b block =
      List.fold block ~init:in_b ~f:(fun values instr ->
          let data =
            match (instr, Bril.Instr.dest instr) with
            | (Const ((name, _), const), _) -> Some (name, Maybe_const.Const const)
            | (Binary ((name, _), op, arg1, arg2), _) ->
              Some
                ( name,
                  Maybe_const.fold_binary op (Map.find_exn values arg1) (Map.find_exn values arg2)
                )
            | (Unary ((name, _), op, arg), _) ->
              Some (name, Maybe_const.fold_unary op (Map.find_exn values arg))
            | (_, Some (name, _)) -> Some (name, Maybe_const.Dynamic)
            | _ -> None
          in
          Option.value_map data ~default:values ~f:(fun (name, data) ->
              Map.set values ~key:name ~data))

    let to_string t =
      Map.to_alist t
      |> List.map ~f:(fun (name, maybe_const) ->
             sprintf "%s: %s" name (Maybe_const.to_string maybe_const))
      |> String.concat ~sep:", "
      |> sprintf "{%s}"
  end

  include M
  include Comparable.Make_plain (M)
end)
