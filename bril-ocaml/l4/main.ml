open! Core

[@@@ocamlformat "disable"]

let analyses =
  String.Map.of_alist_exn
    [
      ("defined", Defined_vars.print);
      ("live", Live_vars.print);
      ("reaching", Reaching_defs.print);
    ]

let default = "defined"

[@@@ocamlformat "enable"]

let () =
  Command.basic
    ~summary:"perform a dataflow analysis"
    (let%map_open.Command analysis = anon (maybe_with_default default ("analysis" %: string)) in
     fun () ->
       In_channel.input_all In_channel.stdin
       |> Yojson.Basic.from_string
       |> Bril.from_json
       |> List.iter ~f:(Map.find_exn analyses analysis))
  |> Command.run
