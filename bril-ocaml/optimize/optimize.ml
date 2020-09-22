open! Core

[@@@ocamlformat "disable"]

let local_optimizations = [
  Lvn.process;
  Tdce.Local.process;
]

let global_optimizations = [
  Tdce.Global.process;
]

[@@@ocamlformat "enable"]

let () =
  In_channel.input_all In_channel.stdin
  |> Yojson.Basic.from_string
  |> Bril.from_json
  |> List.map ~f:(fun func ->
         {
           func with
           blocks =
             Map.map func.blocks ~f:(fun block ->
                 List.fold local_optimizations ~init:block ~f:(Fn.flip ( @@ )));
         })
  |> List.map ~f:(fun func ->
         Bril.Func.set_instrs
           func
           (List.fold global_optimizations ~init:(Bril.Func.instrs func) ~f:(Fn.flip ( @@ ))))
  |> Bril.to_json
  |> Yojson.Basic.pretty_to_string
  |> print_endline
