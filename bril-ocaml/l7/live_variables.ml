open! Core

include Dataflow.Make (struct
  module M = struct
    type t = String.Set.t [@@deriving compare, sexp_of]

    let analysis = "live variables"
    let dir = Dataflow.Backward
    let top = String.Set.empty
    let meet = Set.union
    let init _ = top

    let transfer in_b _ block =
      List.fold_right block ~init:in_b ~f:(fun instr live ->
          let gen = Bril.Instr.args instr |> String.Set.of_list in
          let kill =
            match Bril.Instr.dest instr with
            | None -> String.Set.empty
            | Some (name, _) -> String.Set.singleton name
          in
          Set.union gen (Set.diff live kill))

    let to_string t = t |> Set.to_list |> String.concat ~sep:", " |> sprintf "{%s}"
  end

  include M
  include Comparable.Make_plain (M)
end)
