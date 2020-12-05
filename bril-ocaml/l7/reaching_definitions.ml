open! Core

module Def = struct
  module T = struct
    type t =
      | Arg of string
      | Instr of string * int * Bril.Instr.t
    [@@deriving compare, sexp_of]

    let to_string = function
      | Arg arg -> sprintf "arg %s" arg
      | Instr (block, index, instr) ->
        sprintf "def %s, %s #%d" (Bril.Instr.to_string instr) block index
  end

  include T
  include Comparable.Make_plain (T)
end

include Dataflow.Make (struct
  module M = struct
    type t = Def.Set.t String.Map.t [@@deriving compare, sexp_of]

    let analysis = "reaching definitions"
    let dir = Dataflow.Forward
    let top = String.Map.empty

    let meet =
      Map.merge ~f:(fun ~key:_ -> function
        | `Left defs
        | `Right defs ->
          Some defs
        | `Both (defs1, defs2) -> Some (Set.union defs1 defs2))

    let init (func : Bril.Func.t) =
      List.map func.args ~f:(fun (name, _) -> (name, Def.Set.singleton (Def.Arg name)))
      |> String.Map.of_alist_exn

    let transfer in_b b block =
      List.foldi block ~init:in_b ~f:(fun i vars instr ->
          match Bril.Instr.dest instr with
          | None -> vars
          | Some (name, _) ->
            Map.set vars ~key:name ~data:(Def.Set.singleton (Def.Instr (b, i, instr))))

    let to_string t =
      Map.data t
      |> List.concat_map ~f:Set.to_list
      |> List.map ~f:Def.to_string
      |> String.concat ~sep:", "
      |> sprintf "{%s}"
  end

  include M
  include Comparable.Make_plain (M)
end)
