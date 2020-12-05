open! Core

let () =
  Command.basic
    ~summary:"convert to/from SSA Bril"
    (let%map_open.Command action = anon ("action" %: string) in
     fun () ->
       In_channel.input_all In_channel.stdin
       |> Yojson.Basic.from_string
       |> Bril.from_json
       |> List.map ~f:Ssa.to_ssa
       |> ( match String.lowercase action with
          | "ssa" -> Fn.id
          | "roundtrip" -> List.map ~f:Ssa.from_ssa
          | _ -> failwith "action must be ssa|roundtrip" )
       |> Bril.to_json
       |> Yojson.Basic.pretty_to_string
       |> print_endline)
  |> Command.run
