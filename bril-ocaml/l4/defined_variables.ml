open! Core

include Dataflow.Make (struct
  module M = struct
    type t = String.Set.t [@@deriving compare, sexp_of]

    let analysis = "defined variables"
    let dir = Dataflow.Forward
    let top = String.Set.empty
    let meet = Set.union
    let init (func : Bril.Func.t) = List.map func.args ~f:fst |> String.Set.of_list

    let transfer in_b block =
      List.fold block ~init:in_b ~f:(fun defs instr ->
          match Bril.Instr.dest instr with
          | None -> defs
          | Some (name, _) -> Set.add defs name)

    let to_string t = t |> Set.to_list |> String.concat ~sep:", " |> sprintf "{%s}"
  end

  include M
  include Comparable.Make_plain (M)
end)
