open! Core

[@@@ocamlformat "disable"]

let optimizations = [
  Lvn.process;
  Tdce.process;
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
                 List.fold optimizations ~init:block ~f:(Fn.flip ( @@ )));
         })
  |> Bril.to_json
  |> Yojson.Basic.pretty_to_string
  |> print_endline
