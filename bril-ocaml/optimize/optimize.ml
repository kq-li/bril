open! Core

let () =
  In_channel.input_all In_channel.stdin
  |> Yojson.Basic.from_string
  |> Bril.from_json
  |> List.map ~f:(fun func ->
         {
           func with
           blocks = Map.map func.blocks ~f:(fun block -> block |> Lvn.process |> Tdce.process);
         })
  |> Bril.to_json
  |> Yojson.Basic.pretty_to_string
  |> print_endline
